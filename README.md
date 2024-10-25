# DETECÇÃO DE ÁREAS INUNDADAS COM COBERTURA DE NUVENS EM SISTEMAS ÓPTICOS: INTEGRAÇÃO COM O MODELO DIGITAL DE ELEVAÇÃO

Este projeto é parte do trabalho de dissertação e inclui um conjunto de códigos em linguagem R. Os códigos abordam diferentes etapas do processo e foram desenvolvidos para serem rapidamente e facilmente replicáveis. O projeto propõe um método para a detecção de áreas inundadas em imagens parcialmente cobertas por nuvens, explorando o potencial de atributos hidrológicos e morfométricos extraídos de um Modelo Digital de Elevação (MDE) para a identificação de áreas inundáveis, complementando o mapeamento de água superficial em imagens ópticas.

O conjunto de códigos inclui:

I - O arquivo "Det_n_s_final.R", que contém o código para a detecção e criação da máscara de nuvens e sombras utilizando o classificador por árvores de decisão Rpart.

II - O arquivo "RF1.R", que inclui todas as etapas (parametrização, amostragem, classificação e avaliação) do mapeamento das áreas inundadas utilizando o classificador Random Forest.

III - O arquivo "RF2.R", que inclui todas as etapas (parametrização, amostragem, classificação e avaliação) do mapeamento das áreas inundáveis utilizando o classificador Random Forest.

Qualquer referência ao código e ao trabalho pode ser feita em:
MAIA, A. B. Detecção de Áreas Inundadas Com Cobertura de Nuvem em Sistemas Ópticos: Integração com o Modelo Digital de Elevação. Dissertação (Mestrado em Sensoriamento Remoto) - Instituto Nacional de Pesquisas Espaciais (INPE), São José dos Campos, 2025.

Sinta-se à vontade para contribuir! Se você tiver sugestões, não hesite em abrir uma issue ou um pull request.


Para mais informação e discussões, contate:
Aluizio Brito Maia aluizio.maia@inpe.br
Camilo Daleles Rennó camilo.renno@inpe.br


## Instalação 
Para instalar os pacotes necessários, execute:

```R
# Lista de pacotes necessários
neededPackages = c("raster", "stats", "sf", "ggplot2", "sp", "dplyr", "tidyr", "ROCR",
                   "reshape2", "randomForest", "caret", "caTools", "geobr", "prettymapr", 
                   "tidyselect", "rpart", "rpart.plot", "partykit")

# Função para verificar se o pacote está instalado. Se não estiver, ele será instalado e carregado.
pkgTest = function(x) {
  if (!x %in% rownames(installed.packages())) { 
    install.packages(x, dependencies = TRUE) 
  }
  library(x, character.only = TRUE)
}
for (package in neededPackages) {
  pkgTest(package)
}
```
# DETECTION OF FLOODED AREAS UNDER CLOUD COVER IN OPTICAL SYSTEMS: INTEGRATION WITH DIGITAL ELEVATION MODEL 

This project is part of the dissertation work and includes a set of codes in R language. The codes address different stages of the process and have been developed to be quickly and easily replicable. The project proposes a method for detecting flooded areas in images partially covered by clouds, exploring the potential of hydrological and morphometric attributes extracted from a Digital Elevation Model (DEM) for identifying flood-prone areas, complementing the surface water mapping in optical images.

The set of codes includes:

I - The file **"Det_n_s_final.R"**, which contains the code for detecting and creating the cloud and shadow mask using the Rpart decision tree classifier.

II - The file **"RF1.R"**, which includes all stages (parameterization, sampling, classification, and evaluation) of mapping flooded areas using the Random Forest classifier.

III - The file **"RF2.R"**, which includes all stages (parameterization, sampling, classification, and evaluation) of mapping flood-prone areas using the Random Forest classifier.

Any reference to the code and the work can be made in:
**MAIA, A. B.** Detection of Flooded Areas Under Cloud Cover in Optical Systems: Integration with Digital Elevation Model. Dissertation (Master’s in Remote Sensing) - National Institute for Space Research (INPE), São José dos Campos, 2025.

Feel free to contribute! If you have suggestions, don’t hesitate to open an issue or a pull request.

For more information and discussions, contact:
Aluizio Brito Maia aluizio.maia@inpe.br  
Camilo Daleles Rennó camilo.renno@inpe.br  

## Installation 
To install the required packages, run:

```R
# List of required packages
neededPackages = c("raster", "stats", "sf", "ggplot2", "sp", "dplyr", "tidyr", "ROCR",
                   "reshape2", "randomForest", "caret", "caTools", "geobr", "prettymapr", 
                   "tidyselect", "rpart", "rpart.plot", "partykit")

# Function to check if the package is installed. If not, it will be installed and loaded.
pkgTest = function(x) {
  if (!x %in% rownames(installed.packages())) { 
    install.packages(x, dependencies = TRUE) 
  }
  library(x, character.only = TRUE)
}
for (package in neededPackages) {
  pkgTest(package)
}



