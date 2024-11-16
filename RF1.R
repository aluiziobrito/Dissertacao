# ============================================================
# 1 - CARREGAR BIBLIOTECAS NECESSÁRIAS
# ============================================================
# Lista de pacotes necessários
neededPackages <- c(
  "raster", "stats", "sf", "ggplot2", "sp", "dplyr", "tidyr", "ROCR",
  "reshape2", "randomForest", "caret", "caTools", "geobr", "prettymapr", 
  "tidyselect", "Boruta", "corrplot", "terra", "cluster", "parallel", "shiny"
) 
pkgTest <- function(package) { 
  if (!require(package, character.only = TRUE)) { 
    install.packages(package, dependencies = TRUE) 
    library(package, character.only = TRUE) 
  }
}
sapply(neededPackages, pkgTest)

# ============================================================
# 2 - DEFINIR E VERIFICAR ATRIBUTOS 
# ============================================================

# Definir caminhos dos arquivos de entrada
caminho_mascara_nuvens <- "D:\\Dissertação\\R\\RF1\\Máscara\\mascara.tif"  # Caminho para a máscara de nuvens
caminho_rasters <- "D:/Dissertação/R/RF1/Atributos"  # Diretório com os arquivos de atributos
raster_referencia <- "D:\\Dissertação\\R\\RF1\\Atributos\\NDVI.tif"  # para reamostragem 

# Listar todos os arquivos .tif no diretório especificado
arquivos_entrada <- list.files(
  path = caminho_rasters,       # Caminho do diretório
  pattern = "\\.tif$",          # Procurar apenas arquivos com extensão .tif
  full.names = TRUE             # Retornar caminhos completos
)

# Verificar número de arquivos encontrados
num_arquivos <- length(arquivos_entrada)  # Contar o número de arquivos listados

if (num_arquivos == 0) {
 
  stop("Nenhum arquivo .tif encontrado no diretório especificado.")
} else {
  cat("Número de arquivos .tif encontrados:", num_arquivos, "\n")
  
# ============================================================
# 3 - AJUSTE DOS ATRIBUTOS
# ============================================================
  
adjust_raster <- function(raster_path, reference) {
    cat("Processando..:", raster_path, "\n")
    
    # Carregar o raster e a máscara
    r <- rast(raster_path)
    
    # Verificar e ajustar o CRS do raster
    cat("  Verificando CRS do raster\n")
    if (!compareCRS(r, reference)) {
      cat("  Projeções diferentes. Reprojetando raster...\n")
      r <- project(r, crs(reference))
    }
    
    # Reamostrar o raster para corresponder à resolução e extensão do raster de referência
    cat("  Reamostrando raster.\n")
    r <- resample(r, reference, method = "bilinear") # Para dados contínuos
    
    # Ajustar a extensão do raster para corresponder ao raster de referência
    cat("  Verificando extensão do raster\n")
    if (ext(r) != ext(reference)) {
      cat("  Extensões diferentes. Recortando raster...\n")
      r <- crop(r, ext(reference))
    }
    
    return(r)
  }

# Carregar o raster de referência 
raster_referencia <- rast(raster_referencia)
plot(raster_referencia)

# Carregar a máscara de nuvens
mascara_nuvens <- rast(caminho_mascara_nuvens)
plot(mascara_nuvens)

# Ajustar a máscara de nuvens para combinar com o raster de referência
ajustar_mascara_nuvens <- function(mascara_nuvens, raster_referencia) {
  cat("Ajustando a máscara de nuvens...\n")
  
  # Reprojetar a máscara de nuvens para combinar com o CRS do raster de referência, se necessário
  if (!compareCRS(mascara_nuvens, raster_referencia)) {
    cat("  Reprojetando máscara de nuvens...\n")
    mascara_nuvens <- project(mascara_nuvens, crs(raster_referencia))
  }
  
  # Reamostrar a máscara de nuvens para combinar com a resolução do raster de referência
  cat("  Reamostrando máscara de nuvens...\n")
  mascara_nuvens <- resample(mascara_nuvens, raster_referencia, method = "near")
  
  # Cortar a máscara de nuvens para combinar com a extensão do raster de referência
  cat("  Cortando máscara de nuvens...\n")
  mascara_nuvens <- crop(mascara_nuvens, ext(raster_referencia))
  
  return(mascara_nuvens)
}

# Ajustar a máscara de nuvens
mascara_nuvens_ajustada <- ajustar_mascara_nuvens(mascara_nuvens, raster_referencia)
plot(mascara_nuvens_ajustada)

# Chama Função para ajustar o raster
ajustar_raster <- function(caminho_raster, referencia, mascara) {
  cat("Processando:", basename(caminho_raster), "\n")
  r <- rast(caminho_raster)
  
  # Verificar e ajustar o CRS
  if (!compareCRS(r, referencia)) {
    cat("  CRS diferente. Reprojetando...\n")
    r <- project(r, crs(referencia))
  }
  
  # Reamostrar para combinar com a resolução de referência
  cat("  Reamostrando...\n")
  r <- resample(r, referencia, method = "bilinear")
  
  # Cortar para combinar com a extensão de referência
  cat("  Cortando...\n")
  r <- crop(r, ext(referencia))
  
  # Verificar a extensão da máscara
  if (ext(mascara) != ext(r)) {
    cat("  A extensão da máscara não corresponde à do raster. Ajustando máscara...\n")
    mascara <- crop(mascara, ext(r))
  }
  
  # Aplicar a máscara de nuvens
  cat("  Aplicando máscara de nuvens...\n")
  r <- mask(r, mascara, maskvalue = 1)
  
  # Verificar se o raster é válido
  if (!inherits(r, "SpatRaster")) {
    cat("  Erro: Não é um objeto SpatRaster válido.\n")
    return(NULL)
  }
  
  return(r)
}

# Ajustar todos os rasters para a mesma resolução, extensão e projeção
rasters_ajustados <- list()
for (i in seq_along(arquivos_entrada)) {
  cat("Processando raster", i, "de", length(arquivos_entrada), "\n")
  try({
    raster_ajustado <- ajustar_raster(arquivos_entrada[i], raster_referencia, mascara_nuvens_ajustada)
    if (!is.null(raster_ajustado)) {
      rasters_ajustados[[length(rasters_ajustados) + 1]] <- raster_ajustado
    } else {
      cat("  Raster não ajustado corretamente.\n")
    }
  }, silent = FALSE)
}

# Verificar se algum raster foi ajustado
if (length(rasters_ajustados) == 0) {
  stop("Nenhum raster foi ajustado corretamente.")
}

# Verificar o tipo dos objetos na lista
print(sapply(rasters_ajustados, class))


# Empilhar os rasters ajustados
stack <- rast(rasters_ajustados)

Atributos <- stack # aqui redefini o stack para o nome "Atributos"
plot(Atributos) 
print(Atributos)
summary(Atributos)
# ============================================================
#   4 -    ==== AMOSTRAGEM ==== 
# ============================================================
# Duas classes separadas em dois arquivos shapefile na mesma pasta

amostras = list.files("D:\\Dissertação\\R\\RF1\\Amostragem\\Amostras", pattern = ".shp", full.names = T)
# juntar os .shp em uma lista
amostras = lapply(amostras, st_read) 
# Converter o CRS do segundo conjunto de dados para o CRS do primeiro
amostras[[2]] <- st_transform(amostras[[2]], st_crs(amostras[[1]])) 
# Combina os arquivos shp em um único arquivo
amostras_f = rbind(amostras[[1]], amostras[[2]]) 

# Extrair valores dos raster para as amostras
atributos_amostras = raster::extract(Atributos, amostras_f)

# Acessar apenas os dados em amostras_f
dados_amostras <- st_drop_geometry(amostras_f)

# Unir a informacao da classe da amostra com os valores dos atributos
amostras_atributos <- data.frame(Classe = dados_amostras$id, atributos_amostras)
amostras_atributos <- amostras_atributos %>% select(-ID)
amostras_atributos <- na.omit(amostras_atributos)

# Analisar distribuição das amostras 

dados_melt1 <- melt(amostras_atributos, id.vars = "Classe")

# Criar o boxplot
boxplot <- ggplot(dados_melt1, aes(x = variable, y = value, fill = factor(Classe))) +
  geom_boxplot(outlier.size = 0.9) +
  scale_fill_manual(values = c("#b00003","#63ffed"), labels = c("Não Inundado", "Inundado")) +
  labs(x = "Variável", y = "Valor", fill = "Classe") +  # Ajuste dos rótulos dos eixos e legenda
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(family = "Arial"),
        axis.title = element_text(face = "bold", size = 10),
        axis.text.x = element_text(colour = "white", size = 8),
        axis.text.y = element_text(colour = "black", size = 10),
        axis.line = element_line(size = 1, colour = "black")) +
  facet_wrap(~variable, scales = "free") +
  theme(plot.margin = unit(c(1,1,1,1), "lines"))

print(boxplot)


ggsave(plot = boxplot, filename = "D:\\Dissertação\\Resultados\boxplot_RF1", device = 'png', 
       width = 11.7, height = 8.3, units = 'in', dpi = 600)


# ============================================================
# 5 - MODELO RANDOM FOREST
# ============================================================

amostras_atributos$Classe <- as.factor(amostras_atributos$Classe)

# Rodando o Boruta para seleção de variáveis
set.seed(123)
boruta_result <- Boruta(Classe ~ ., data = amostras_atributos, doTrace = 2)

# Obter as variáveis realmente importantes
importantes <- getSelectedAttributes(boruta_result, withTentative = FALSE)
print(importantes)

# Subconjunto dos dados com apenas as variáveis selecionadas pelo Boruta
amostras_atributos_boruta <- amostras_atributos[, c(importantes, "Classe")]

boruta_importancia <- attStats(boruta_result)

# Visualizando o ranking
print(boruta_importancia)

# Classificar as variáveis com base na importância (em ordem decrescente)
boruta_importancia <- boruta_importancia[order(boruta_importancia$meanImp, decreasing = TRUE), ]

# Visualizando as variáveis ordenadas
print(boruta_importancia)

# Criar o gráfico da importância dos atributos

ggplot(boruta_importancia, aes(x = reorder(rownames(boruta_importancia), meanImp), y = meanImp)) +
  geom_bar(stat = "identity", fill = "#69b3a2", color = "black") +  
  coord_flip() + 
  labs(title = "Importância Média dos Atributos - Boruta",
       x = "Atributos",
       y = "Importância Média") +
  theme_minimal(base_size = 15) +  
  theme(
    panel.background = element_rect(fill = "lightgrey"),  
    plot.background = element_rect(fill = "lightgrey"),   
    panel.grid.major = element_line(color = "white"),      
    panel.grid.minor = element_line(color = "white"),      
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"), 
    axis.title = element_text(size = 14)                    
  )


# Matriz de correlação entre as variáveis preditoras
correlacao <- cor(amostras_atributos %>% select(-Classe))
corrplot::corrplot(correlacao, method = "circle")


# AJUSTE DO MODELO COM INTERFACE

# Interface do usuário
ui <- fluidPage(
  titlePanel("Ajuste do Random Forest"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("ntree", 
                  "Número de Árvores (ntree):", 
                  min = 100, 
                  max = 2000, 
                  value = 800, 
                  step = 50),
      
      sliderInput("mtry", 
                  "Número de Variáveis por Nó (mtry):", 
                  min = 1, 
                  max = length(variaveis_importantes), 
                  value = floor(sqrt(length(variaveis_importantes))), 
                  step = 1),
      
      actionButton("train", "Treinar Modelo"),
      actionButton("saveModel", "Salvar Modelo")  # Novo botão para salvar o modelo
    ),
    
    mainPanel(
      plotOutput("oobPlot"),
      verbatimTextOutput("modelSummary")
    )
  )
)


server <- function(input, output, session) {
  
  # Variável reativa para armazenar o modelo
  modelo_rf <- reactiveVal(NULL)
  
  # Treinar o modelo quando o botão for clicado
  observeEvent(input$train, {
    set.seed(3)
    
    # Subconjunto de dados com as variáveis selecionadas pelo Boruta
    amostras_atributos_boruta <- amostras_atributos[, c(variaveis_importantes, "Classe")]
    
    # Treinando o modelo Random Forest
    modelo <- randomForest(Classe ~ ., 
                           data = amostras_atributos_boruta, 
                           ntree = input$ntree, 
                           mtry = input$mtry, 
                           importance = TRUE)
    assign("modelo_rf", modelo, envir = .GlobalEnv)
    
    modelo_rf(modelo)
  })
  
  # Mostrar as métricas do modelo
  output$modelSummary <- renderPrint({
    req(modelo_rf())  # Garante que o modelo já foi treinado
    print(modelo_rf())
  })
  
  # Plotar o erro OOB
  output$oobPlot <- renderPlot({
    req(modelo_rf())  # Garante que o modelo já foi treinado
    
    oob_error <- as.data.frame(modelo_rf()$err.rate) %>% 
      mutate(ntree = as.numeric(row.names(.)))
    
    ggplot(oob_error, aes(x = ntree, y = OOB)) +
      geom_line(color = "darkblue", size = 1) +    
      geom_point(color = "red", size = 1) +      
      theme_bw(base_size = 14) +                    
      labs(
        title = "Erro OOB em Relação ao Número de Árvores",  
        x = "Número de Árvores",                          
        y = "Erro OOB"                                    
      ) +
      theme(
        axis.title.x = element_text(size = 14, face = "bold"),    
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  
        panel.grid.major = element_line(size = 0.5, color = "gray80"),     
        panel.grid.minor = element_blank(),                              
        panel.border = element_rect(color = "black", fill = NA)           
      )
  })
  

  observeEvent(input$saveModel, {
    req(modelo_rf())  
    
    # Salvar o modelo 
    saveRDS(modelo_rf(), file = "D:/Dissertação/Resultados/Modelos_RF/Modelo_RF_final.rds")
    
    # Exibir uma mensagem de sucesso
    showModal(modalDialog(
      title = "Sucesso",
      "Modelo salvo com sucesso!",
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

# Rodar o aplicativo Shiny
shinyApp(ui = ui, server = server)

# ============================================================
# 6 - PREDICT - Processo de Classificação
# ============================================================

# Classifica com base no modelo rf
rf.class <- raster::predict(Atributos, modelo_rf, progress = "text", type = "response")
plot(rf.class)

# Escrever o objeto raster em um arquivo
writeRaster(rf.class, "D:\\Dissertação\\R\\RF1\\Classificações\\Class8.tif", overwrite = TRUE)

# ============================================================
# 7 - Entropia de Shannon
# ============================================================

# Definição dos caminhos onde serão salvos os arquivos
Probabilidade <- "D:\\Dissertação\\R\\RF2\\Incerteza\\Prob_teste.tif"
Entropia_Caminho = "D:\\Dissertação\\R\\RF2\\Incerteza\\Entropia_teste.tif"
Amostras_Entropia = "D:\\Dissertação\\R\\RF2\\Incerteza\\Amostras.teste.shp"

#  Extrai a informação de probabilidade da classificação
prob_RF <- raster::predict(Atributos, modelo_rf, type = "prob") 
print(prob_RF)
plot(prob_RF)
writeRaster(prob_RF, filename = Probabilidade, overwrite = TRUE)

# Calculando a entropia
entropy <- - ((prob_RF$`X1` * log2(prob_RF$`X1`)) + (prob_RF$`X0` * log2(prob_RF$`X0`)))
entropy[is.na(entropy)] = 0.000 # Evita log2(0)
plot(entropy)
print(entropy)

# Aplicar a máscara de nuvens
entropy <- mask(entropy, mascara_nuvens_ajustada, maskvalue = 1)
plot(entropy)
Salva o raster de entropia
writeRaster(entropy, filename = Entropia_Caminho, overwrite = TRUE)
