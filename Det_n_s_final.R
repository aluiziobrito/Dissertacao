# 1 -    ==== CARREGAR OS ATRIBUTOS ESPECTRAIS ==== 

# Definir o diretório onde os arquivos estão localizados
dir_path <- "Insira aqui o seu diretório"

# Listar todos os arquivos .tif na pasta
files_list <- list.files(path = dir_path, pattern = "\\.tif$", full.names = TRUE)

# Carregar os arquivos 
rasters_list <- lapply(files_list, raster)

# Nomear cada elemento da lista com o nome do arquivo (sem a extensão)
names(rasters_list) <- tools::file_path_sans_ext(basename(files_list))
print(rasters_list)

# Criar o stack com os rasters da lista
  # Para detalhes sobre os atributos, consulte o Mark Down Clouds_Shadows_Mask.md
img <- stack(rasters_list[["Wv"]],    
             rasters_list[["WI"]],     
             rasters_list[["HOT"]],    
             rasters_list[["SWIR_SCD"]], 
             rasters_list[["NIR-SCD"]], 
             rasters_list[["Cloud Index"]], 
             rasters_list[["Cirrus"]]) 

plot(img)

# 2 -    ==== ETAPA DE AMOSTRAGEM ==== 

# Carregar os shapefiles e padronizar o CRS para todos
amostras <- list.files("Insira aqui o diretório das suas amostras", 
                       pattern = ".shp", full.names = TRUE) %>%
  lapply(st_read) %>%
  lapply(function(x) st_transform(x, st_crs(4326))) # O CRS WGS84 está como exemplo, ajuste conforme necessário

# Combinar os shapefiles em um único objeto 'sf'
amostras_f <- do.call(rbind, amostras)

# Extrair valores dos rasters para os pontos das amostras
atributos_amostras <- raster::extract(img, amostras_f)

# Converter para dataframe e adicionar a coluna 'Classe'
atributos_df <- as.data.frame(atributos_amostras)
dados_amostras <- st_drop_geometry(amostras_f)

# Atualizar a coluna 'id' para a classe desejada
dados_amostras$id <- ifelse(dados_amostras$id == 3, 1, dados_amostras$id)

# Unir os dados das classes com os atributos extraídos
amostras_atributos <- data.frame(Classe = dados_amostras$id, atributos_df)

# Separar as amostras em treino e teste (Neste caso, 70% para treino, ajuste conforme necessário.)
index <- createDataPartition(amostras_atributos$Classe, p = 0.7, list = FALSE)
treino_amostras <- amostras_atributos[index, ]
teste_amostras <- amostras_atributos[-index, ]

print(amostras_atributos)

------------------------------------------------------------
  #   3 -    ==== MODELO R-PART ==== 

treinar_e_avaliar <- function(dados) {
  set.seed(100)  
  num_folds <- 10 # Ajuste conforme necessário, valores entre 5 e 10 são indicados
  folds <- createFolds(dados$Classe, k = num_folds, list = FALSE)  # Criação de cada um dos folds
  
  metricas_df <- data.frame(Acuracia = numeric(num_folds),
                            Sensibilidade = numeric(num_folds),
                            Especificidade = numeric(num_folds),
                            Precisao = numeric(num_folds),
                            F1_Pontuacao = numeric(num_folds))
  
  for (i in 1:num_folds) {
    indice_treino <- which(folds != i)
    indice_teste <- which(folds == i)
    
    # Dados de treino e teste
    dados_treino <- dados[indice_treino, ]
    dados_teste <- dados[indice_teste, ]
    
    # Treinar o modelo # Ajuste os hiperparâmetros conforme necessário, consulte o Mark Down Clouds_Shadows_Mask.md.
    filtrpart <- rpart(
      Classe ~ ., 
      data = dados_treino,
      method = "class",
      control = rpart.control(
        minsplit = 10,
        minbucket = round(10 / 3),
        cp = 0.001,
        maxcompete = 4, 
        maxsurrogate = 1,
        usesurrogate = 2, 
        xval = 0,  
        surrogatestyle = 0, 
        maxdepth = 20
      )
    )
  
    # Avaliar
    classe_predita <- predict(filtrpart, dados_teste, type = "class")
    
    # Converter para a mesma referência
    dados_teste$Classe <- factor(dados_teste$Classe, levels = levels(classe_predita))
    MatrizConfusao <- confusionMatrix(data = classe_predita, reference = dados_teste$Classe)
    
    # Armazenar métricas
    metricas_df[i, "Acuracia"] <- MatrizConfusao$overall['Accuracy']
    metricas_df[i, "Sensibilidade"] <- MatrizConfusao$byClass['Sensitivity']
    metricas_df[i, "Especificidade"] <- MatrizConfusao$byClass['Specificity']
    metricas_df[i, "Precisao"] <- MatrizConfusao$byClass['Precision']
    metricas_df[i, "F1_Pontuacao"] <- MatrizConfusao$byClass['F1']
    
    # Imprimir as métricas de cada fold
    cat("Fold:", i, "\n")
    cat("Acurácia:", metricas_df[i, "Acuracia"], "\n")
    cat("Sensibilidade:", metricas_df[i, "Sensibilidade"], "\n")
    cat("Especificidade:", metricas_df[i, "Especificidade"], "\n")
    cat("Precisão:", metricas_df[i, "Precisao"], "\n")
    cat("F1 Pontuação:", metricas_df[i, "F1_Pontuacao"], "\n")
    cat("----------------------\n")
    
    # plotar um por um
    readline(prompt = "Pressione Enter no console para continuar para o próximo fold :) ...")
  }
  
  # Gerar os nomes das métricas em português
  metricas_df_long <- melt(as.data.frame(metricas_df))
  colnames(metricas_df_long) <- c("Métrica", "Valor")
  
  # Substituir os nomes das métricas para português, ajuste para inglês ou o idioma que achar melhor.
  metricas_df_long$Métrica <- factor(metricas_df_long$Métrica, 
                                     levels = c("Acuracia", "Sensibilidade", "Especificidade", "Precisao", "F1_Pontuacao"),
                                     labels = c("Acurácia", "Sensibilidade", "Especificidade", "Precisão", "F1"))
  
  # Gerando gráficos 
  for (metrica in unique(metricas_df_long$Métrica)) {
    grafico_individual <- ggplot(subset(metricas_df_long, Métrica == metrica), aes(x = Métrica, y = Valor)) + 
      geom_boxplot(fill = "lightblue", color = "darkblue", outlier.color = "red", outlier.shape = 16) + 
      stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "darkred", fill = "darkred") +
      labs(title = paste("Desempenho -", metrica),
           x = "Métrica",
           y = "Valores") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 12, face = "bold", color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold")
      )
    
    # Exibir gráfico
    print(grafico_individual)
    
    # Exibir gráficos
    readline(prompt = "Pressione Enter no console para ver o próximo gráfico :) ...")
  }
  
  # Gerando Intervalo de Confiança
  intervalos_conf <- data.frame(
    Métricas = colnames(metricas_df),
    Média = colMeans(metricas_df),
    IC_Inf = apply(metricas_df, 2, function(x) mean(x) - qt(0.975, df = num_folds - 1) * sd(x) / sqrt(num_folds)),
    IC_Sup = apply(metricas_df, 2, function(x) mean(x) + qt(0.975, df = num_folds - 1) * sd(x) / sqrt(num_folds))
  )
  
  # Gráfico de intervalos de confiança
  ggplot(intervalos_conf, aes(x = Métricas, y = Média)) + 
    geom_point() +
    geom_errorbar(aes(ymin = IC_Inf, ymax = IC_Sup), width = 0.2) +
    labs(title = "Intervalos de Confiança das Métricas de Desempenho",
         x = "Métricas",
         y = "Valores") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # ângulo dos textos no eixo x
}
# Chamando a função
treinar_e_avaliar(amostras_atributos)

------------------------------------------------------------
  #   4 -    ==== PREDIÇÃO ==== 

filtrpart <- rpart(
  Classe ~ ., 
  data = treino_amostras,
  method = "class",
  control = rpart.control(
    minsplit = 10,
    minbucket = round(10 / 3),
    cp = 0.001,
    maxcompete = 4, 
    maxsurrogate = 1,
    usesurrogate = 2, 
    xval = 0,  
    surrogatestyle = 0, 
    maxdepth = 20
  )
)

# Verificar se o modelo foi treinado certo e se a imagem está em raster
if (!is.null(filtrpart) && inherits(img, "Raster")) {
  
  # Prever
  rpart.pred <- raster::predict(img, filtrpart, progress = "text", type = "class", na.rm = TRUE)
  
  # Plotar o resultado =
  plot(rpart.pred, main = "Predição do Modelo rpart", col = viridis(2))  #
  
  # Nomear o arquivo a ser salvo...
  output_file <- "Class_rpart_Todos_atbA2if.tif"
  
  # Se o arquivo existir, perguntar se deseja salvar por cima
  if (file.exists(output_file)) {
    message("O arquivo já existe. Deseja salvar por cima? (s/n)")
    resposta <- readline()
    if (tolower(resposta) != "s") {
      message("O arquivo não foi salvo.")
    } else {
      writeRaster(rpart.pred, filename = output_file, overwrite = TRUE)
      message("Arquivo salvo com o nome: ", output_file)
    }
  } else {
    writeRaster(rpart.pred, filename = output_file, overwrite = TRUE)
    message("Arquivo salvo com o nome: ", output_file)
  }
  
} else {
  message("Erro: O modelo ou a imagem não estão no formato esperado.")
}

rpart.pred_values <- raster::values(rpart.pred)


#   5 -  ==== AVALIAÇÃO/TESTE ==== 

# Função para calcular o MCC
calcular_mcc <- function(vp, vn, fp, fn) {
  numerador <- (vp * vn) - (fp * fn)
  denominador <- sqrt((vp + fp) * (vp + fn) * (vn + fp) * (vn + fn))
  
  if (denominador == 0) {
    return(0)
  }
  
  mcc <- numerador / denominador
  return(mcc)
}
# Avaliando o modelo com amostras aleatórias

avaliar_modelo <- function(caminho_amostras, img, rpart_pred) {
  # Carregar as amostras aleatórias
  amostras_aleatorias <- st_read(caminho_amostras)  
  
  # Verificar se a coluna "Classe" existe na camada
  if (!"Classe" %in% colnames(amostras_aleatorias)) {
    stop("A coluna 'Classe' não existe.")
  }
  
  # Transformar CRS das amostras
  amostras_aleatorias <- st_transform(amostras_aleatorias, crs(rpart_pred))
  
  # Extrair os valores do raster preditos pelo rpart
  valores_preditos <- raster::extract(rpart_pred, amostras_aleatorias)
  
  # Obter os valores da amostra
  valores_reais <- amostras_aleatorias$Classe
  
  # Calcular as métricas de desempenho
  # Atenção aos números usado para as classes e altere se necessário, neste caso, 1 representa nuvens/sombras e 2 representa outras coberturas
  vp <- sum(valores_preditos == 1 & valores_reais == 1, na.rm = TRUE)
  vn <- sum(valores_preditos == 2 & valores_reais == 2, na.rm = TRUE)
  fp <- sum(valores_preditos == 1 & valores_reais == 2, na.rm = TRUE)
  fn <- sum(valores_preditos == 2 & valores_reais == 1, na.rm = TRUE)
  
  # Acurácia Global
  ag <- (vp + vn) / (vp + vn + fp + fn)
  
  # Sensibilidade
  sensibilidade <- ifelse((vp + fn) == 0, 0, vp / (vp + fn))
  
  # Precisão
  precisao <- ifelse((vp + fp) == 0, 0, vp / (vp + fp))
  
  # F1-Score
  f1 <- ifelse((precisao + sensibilidade) == 0, 0, 2 * (precisao * sensibilidade) / (precisao + sensibilidade))
  
  # Especificidade
  especificidade <- ifelse((vn + fp) == 0, 0, vn / (vn + fp))
  
  # Matthew Correlation Coefficient
  mcc <- calcular_mcc(vp, vn, fp, fn)
  
  resultados <- list(
    Acuracia = ag,
    Sensibilidade = sensibilidade,
    Precisao = precisao,
    F1 = f1,
    Especificidade = especificidade,
    Falsos_Positivos = fp,
    Falsos_Negativos = fn,
    MCC = mcc
  )
  
  return(resultados)
}
resultados <- avaliar_modelo("Insira aqui o local das suas amostras de teste", img, rpart.pred)
print(resultados)

