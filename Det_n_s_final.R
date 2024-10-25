library(rpart)
library(rpart.plot)

# 1 - Carregar bibliotecas

neededPackages = c("raster", "stats", "sf", "ggplot2", "sp", "dplyr", "tidyr", "sp", "sf","ROCR",
                   "reshape2", "randomForest", "caret", "caTools", "geobr", "prettymapr", "tidyselect" )
pkgTest = function(x) { if (x %in% rownames(installed.packages()) == FALSE) { install.packages(x, dependencies= TRUE) }; library(x, character.only = TRUE) }
for (package in neededPackages) { pkgTest(package) }

# Ler pasta com o atributo

WVCDI <- "D:\\Dissertação\\Nuvens e Sombras\\Índices_Correto\\WV_CDI.tif"
WI<- "D:\\Dissertação\\Nuvens e Sombras\\Índices_Correto\\WI.tif"
HOT <- "D:\\Dissertação\\Nuvens e Sombras\\Índices_Correto\\HOT.tif"
SWIR_SCD <- "D:\\Dissertação\\Nuvens e Sombras\\Índices_Correto\\SWIR_SCD.tif"
NIR_SCD <- "D:\\Dissertação\\Nuvens e Sombras\\Índices_Correto\\HOT_N32.tif"
CI <- "D:\\Dissertação\\Nuvens e Sombras\\Índices_Correto\\Cloud Index.tif"
Cirrus <- "D:\\Dissertação\\Nuvens e Sombras\\Índices_Correto\\Cirrus.tif"
NIR_SCD16 <- "D:\\Dissertação\\Nuvens e Sombras\\Índices\\HOT_W.tif"


# Criar stack com os atributos
img <- stack(WVCDI, WI, HOT, SWIR_SCD, NIR_SCD, CI, Cirrus)
# Ler como DataFrame

img.df <- data.frame(values(img))

#   3 -    ==== AMOSTRAGEM ==== 

# um shp para cada classe na mesma pasta

amostras = list.files("D:\\Dissertação\\Nuvens e Sombras\\Árvore de decisão\\Amostras_Final", pattern = ".shp", full.names = T)
amostras = lapply(amostras, st_read) #junta os .shp em uma lista
amostras[[2]] <- st_transform(amostras[[2]], st_crs(amostras[[1]])) # Converter o CRS do segundo conjunto de dados para o CRS do primeiro
amostras_f = rbind(amostras[[1]], amostras[[2]]) #combina em um ?nico vetor
amostras_f <- do.call(rbind, amostras)
amostras_f <- st_as_sf(amostras_f)
amostras_f <- as(st_as_sf(amostras_f), "Spatial")
#Extrair valores dos raster para as amostras

atributos_amostras <- raster::extract(img, amostras_f)
class(atributos_amostras)

# Extrair atributos como um dataframe
atributos_df <- as.data.frame(atributos_amostras)

# Verificar a classe do dataframe
class(atributos_df)
# Acessar apenas os dados em amostras_f
dados_amostras <- st_drop_geometry(amostras_f)


dados_amostras$id <- ifelse(dados_amostras$id == 3, 1, dados_amostras$id)


# Unir a informacao da classe da amostra com os valores dos atributos

amostras_atributos <- data.frame(Classe = dados_amostras$id, atributos_df)

print(amostras_atributos)

index <- createDataPartition(amostras_atributos$Classe, p = 0.7, list = FALSE)

# Separando as amostras de treino e teste
treino.amostras <- amostras_atributos[index, ]
teste.amostras <- amostras_atributos[-index, ]

------------------------------------------------------------
  #   3 -    ==== MODELO R-PART ==== 

# Modelo

set.seed(125)
filtrpart <- rpart(
  treino.amostras$Classe ~ ., 
  data = treino.amostras,
  method = "class",
  control = rpart.control(
    minsplit = 1,
    minbucket = round(1/3),
    cp = 0.010,
    maxcompete = 4, 
    maxsurrogate = 10,
    usesurrogate = 2, 
    xval = 10,
    surrogatestyle = 0, 
    maxdepth = 30
  )
)


print(filtrpart)
plot(filtrpart)
# Plotar a árvore de decisão 
rpart.plot(
  filtrpart,
  box.palette = c("lightblue", "orange"),  # Cores para os nós
  fallen.leaves = TRUE,  # nós terminais alinhadas à esquerda
  type = 2,  
  branch = 1,  # Formato dos ramos
  branch.lty = 1,  # linha dos ramos
  branch.lwd = 4,  # Largura da linha dos ramos
  uniform = TRUE,  # Espaçamento uniforme entre os nós
  main = "Detecção de Nuvens/Sombras",  # Título do gráfico
  sub = " ",  # Subtítulo do gráfico
  shadow.col = "white",  # Cor do texto (letras)
  cex = 0.7  # tamanho do texto
)

print(filtrpart)
------------------------------------------------------------
  
  #   4 -    ==== PREDIÇÃO ==== 

rpart.pred <- raster::predict(img, filtrpart, progress = "text", type = "class")

plot(rpart.pred)
writeRaster(rpart.pred, "Class_rpart_Todos_atbA2if", overwrite = TRUE)

#   5 -  ==== AVALIAÇÃO ==== 

predictclass <- raster::predict(filtrpart, teste.amostras, progress = "text", type = "class")

#levels(predictclass)
#levels(teste.amostras$Classe)

# Converter para mesma referência
teste.amostras$Classe <- factor(teste.amostras$Classe, levels = levels(predictclass))


MatrizConf.RF <- confusionMatrix(data = predictclass, reference = teste.amostras$Classe)
print(MatrizConf.RF)


precisão <- MatrizConf.RF$byClass["Precision"]
sensibilidade <- MatrizConf.RF$byClass["Recall"]
especificidade <- MatrizConf.RF$byClass["Specificity"] #vi no github mas não sei a potencialidade

print(precisão)
print(sensibilidade)
print(especificidade)

f1_score <- MatrizConf.RF$byClass["F1"]
print(f1_score)
-------------------------------------------------------------------------------
  #  6 -  ==== Validação-Cruzada K-Fold ====

treinar_e_avaliar <- function(indice_treino, indice_teste, dados) {
  
  # Dados de treino e teste
  dados_treino <- dados[indice_treino, ]
  dados_teste <- dados[indice_teste, ]
  
  # Treinar o modelo (mesmas métricas selecionadas no modelo)
  filtrpart <- rpart(
    Classe ~ ., 
    data = dados_treino,
    method = "class",
    control = rpart.control(
      minsplit = 10,
      minbucket = round (10/3),
      cp = 0.001,
      maxcompete = 4, 
      maxsurrogate = 1,
      usesurrogate = 2, 
      xval = 10,
      surrogatestyle = 0, 
      maxdepth = 20
    )
  )
  
  # Avaliar 
  classe_predita <- raster::predict(filtrpart, dados_teste, progress = "text", type = "class")
  # Converter para a mesma referência
  dados_teste$Classe <- factor(dados_teste$Classe, levels = levels(classe_predita))
  MatrizConfusao <- confusionMatrix(data = classe_predita, reference = dados_teste$Classe)
  
  library(reshape2)
  
  # Gerar os nomes das métricas em português
  metricas_df_long <- reshape2::melt(as.data.frame(metricas_df))
  colnames(metricas_df_long) <- c("Métrica", "Valor")
  
  # Substituir os nomes das métricas para português
  metricas_df_long$Métrica <- factor(metricas_df_long$Métrica, 
                                     levels = c("Acuracia", "Sensibilidade", "Especificidade", "Precisao", "F1_Pontuacao"),
                                     labels = c("Acurácia", "Sensibilidade", "Especificidade", "Precisão", "F1"))
  # Gerar gráficos individuais para cada métrica
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
  }
}

# Gerar Intervalo de Confiança
intervalos_conf <- data.frame(
  Métricas = colnames(metricas_df),
  Média = colMeans(metricas_df),
  IC_Inf = apply(metricas_df, 2, function(x) mean(x) - qt(0.975, df = num_folds - 1) * sd(x) / sqrt(num_folds)),
  IC_Sup = apply(metricas_df, 2, function(x) mean(x) + qt(0.975, df = num_folds - 1) * sd(x) / sqrt(num_folds))
)

ggplot(intervalos_conf, aes(x = Métricas, y = Média)) + 
  geom_point() +
  geom_errorbar(aes(ymin = IC_Inf, ymax = IC_Sup), width = 0.2) +
  labs(title = "Intervalos de Confiança das Métricas de Desempenho",
       x = "Métricas",
       y = "Valores") +
  theme_minimal()

#gráfico
ggplot(intervalos_conf, aes(x = Métricas, y = Média)) + 
  geom_point() +
  geom_errorbar(aes(ymin = IC_Inf, ymax = IC_Sup), width = 0.2) +
  labs(title = "Intervalos de Confiança das Métricas de Desempenho",
       x = "Métricas",
       y = "Valores") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  #  ângulo dos textos no eixo x
  
# ======= 7 - Plotar com o partykit e prever de novo ======

# Carregar bibliotecas

install.packages("partykit")
library(partykit)

# Converter para uma estrutura de árvore compatível com o partykit
filtrpart_partykit <- as.party(filtrpart)

# Plotar a árvore de decisão com todas as informações
plot(filtrpart_partykit)
print(filtrpart_partykit)




rpart.pred <- raster::predict(img, filtrpart, progress = "text", type = "class")

plot(rpart.pred)
writeRaster(rpart.pred, "Class_rpart_Todos_atbA2MEDIA.tif", overwrite = TRUE)



## ==========  MCC ====

# Função para calcular o MCC
calcular_mcc <- function(vp, vn, fp, fn) {
  # Calcula o numerador e denominador
  numerador <- (vp * vn) - (fp * fn)
  denominador <- sqrt((vp + fp) * (vp + fn) * (vn + fp) * (vn + fn))
  
  # Verifica se o denominador é zero para evitar divisão por zero
  if (denominador == 0) {
    return(0)
  }
  
  # Calcula o MCC
  mcc <- numerador / denominador
  return(mcc)
}

vp <- 292
vn <- 92
fp <- 12
fn <- 104

# Calcula e imprime o MCC
mcc_resultado <- calcular_mcc(vp, vn, fp, fn)
cat("O valor do MCC é:", mcc_resultado, "\n")


MCC A2 - 0.7503242
MCC A3 - 0.738615 
MCC SCL - 0.5171092 

# Dados
modelos <- c("Árvore 2", "Árvore 3", "SCL")
mcc <- c(0.7503242, 0.738615, 0.5171092)
falsos_positivos <- c(16, 17, 12)  
falsos_negativos <- c(27, 28, 104)  

# Criar um data frame para o MCC
df_mcc <- data.frame(Modelo = modelos, MCC = mcc)

# Criar um data frame para FP e FN
df_fp_fn <- data.frame(Modelo = modelos, Falsos_Positivos = falsos_positivos, Falsos_Negativos = falsos_negativos)

# Gráfico para MCC
p1 <- ggplot(df_mcc, aes(x = Modelo, y = MCC, group = 1)) +
  geom_line(color = "#FFA500", size = 1) +  # Cor laranja
  geom_point(color = "#FFA500", size = 4) +
  labs(title = "Comparação do MCC entre modelos", x = "Modelo", y = "MCC") +
  ylim(0, 1) +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5))

# Gráfico para FPs e FNs com a legenda renomeada
p2 <- ggplot(df_fp_fn, aes(x = Modelo)) +
  geom_line(aes(y = Falsos_Positivos, color = "Falsos Positivos"), size = 1) +
  geom_point(aes(y = Falsos_Positivos, color = "Falsos Positivos"), size = 4) +
  geom_line(aes(y = Falsos_Negativos, color = "Falsos Negativos"), size = 1) +
  geom_point(aes(y = Falsos_Negativos, color = "Falsos Negativos"), size = 4) +
  labs(title = "Comparação dos Falsos Positivos e Falsos Negativos", x = "Modelo", y = "Quantidade") +
  scale_color_manual(name = "Métricas", values = c("Falsos Positivos" = "#4682B4", "Falsos Negativos" = "#B22222")) +  # Cores azul e vermelho
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5))

# Combinar os gráficos
grid.arrange(p1, p2, ncol = 2)
