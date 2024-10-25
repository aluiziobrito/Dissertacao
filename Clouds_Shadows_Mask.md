# Gerando a Máscara de Nuvens e Sombras para Imagens de Eventos de Inundação

A cobertura de nuvens é uma das principais dificuldades na obtenção de imagens de sensoriamento remoto. Existem diversos métodos para criar máscaras de nuvens e sombras. O método proposto neste trabalho tem como objetivo criar uma máscara de nuvens e sombras especificamente para eventos de inundação, visando um processamento simples que equilibre a detecção de nuvens e sombras com a presença de falsos positivos, especialmente aqueles originados de água superficial.

Defina, em um estudo prévio, os atributos selecionados para a criação da sua máscara de nuvens e sombras. Sugere-se realizar limiarizações individuais dos atributos para analisar a capacidade de separabilidade de cada um e seus respectivos potenciais.

No estudo em questão, os atributos foram selecionados em um processo simples de limiarização utilizando Sistemas de Informação Geográfica (SIGs). Neste caso, foram usadas bandas do MSI/Sentinel-2, e os atributos devem ser adaptados de acordo com as bandas disponíveis no satélite utilizado.

# Atributos para imagens MSI/Sentinel-2

O stack gerado no código  utiliza 7 atributos que foram pré-selecionados, sendo eles:

* WV-CDI (Water Vapour Cloud Detection Index) - Consultar Maia (2025)
* WI (Whiteness Index) -  Consultar Gómez-Chova et al. (2007) 
* HOT (Haze Optimized Transformation) - Consultar Zhang et al. (2002)
* SWIR-SCD (Short-Wave Infrared Shadow and Clouds Detection Index) - Consultar Maia (2025)
* NIR-SCD (Near Infrared Shadow and Clouds Detection Index) - Consultar Maia (2025)
* Cloud Index - Consultar Zhai et al. (2018)
* Cirrus - Banda 10 do MSI/Sentinel-2

Os atributos foram gerados em um SIG. É importante mencionar que as bandas de 60 m devem ser reamostradas. Um exemplo de um processo simples de reamostragem pode ser feito pela linha abaixo:

```r
target_resolution <- 10  # Resolução a ser definida
B9 <- resample(B9, B1, method="bilinear")

```

Como alternativa à geração dos atributos por um SIG, o código abaixo calcula cada um dos atributos (partindo do pressuposto de que as bandas estão reamostradas). 

```r
# Carregando as bandas do MSI/Sentinel-2 que serão utilizadas 

B2 <- rast("caminho/para/B2.tif")
B3 <- rast("caminho/para/B3.tif")
B4 <- rast("caminho/para/B4.tif")
B8 <- rast("caminho/para/B8.tif")
B8A <- rast("caminho/para/B8A.tif")
B9 <- rast("caminho/para/B9.tif")
B11 <- rast("caminho/para/B11.tif")

# Calculando os atributos espectrais
HOT <- B2 - (0.45 * B4) - 0.08
M <- (0.25 * B2) + (0.375 * B3) + (0.375 * B4)
WI <- ( (B2 - M) / M ) + ( (B3 - M) / M ) + ( (B4 - M) / M )
Cloud Index <- (B8 + (2 * B11) / (B2 + B3 + B4)
WV <- B2 - (0.45 * B4) - (0.32 * B9)
SWIR_SCD <- B2 - (0.45 * B4) - (0.32 * B11)
NIR-SCD <- B2− (0,45 * B4) − (0,16* NIR)

# Salvar os resultados em arquivos raster
writeRaster(HOT, "caminho", overwrite=TRUE)
writeRaster(WI, "caminho", overwrite=TRUE)
writeRaster(Cloud_Index, "caminho", overwrite=TRUE)
writeRaster(WV, "caminho", overwrite=TRUE)
writeRaster(SWIR_SCD, "caminho", overwrite=TRUE)

```

# Atributos para imagens de sensores RGB-NIR (CBERS-4, CBERS-4A, Amazonia-1, etc)

```r

# Carregando as bandas que serão utilizadas, no exemplo, as bandas do MUX/CBERS-4A



B15 <- rast("caminho/para/B2.tif")
B3 <- rast("caminho/para/B3.tif")
B4 <- rast("caminho/para/B4.tif")
B8 <- rast("caminho/para/B8.tif")
B8A <- rast("caminho/para/B8A.tif")
B9 <- rast("caminho/para/B9.tif")
B11 <- rast("caminho/para/B11.tif")

```



# REFERÊNCIAS 
GóMEZ-CHOVA, L. et al. Cloud-screening algorithm for envisat/meris multispectral images. IEEE Transactions on Geoscience and Remote Sensing, v. 45, n. 12, p. 4105–4118, 2007. 8

ZHAI, H.; ZHANG, H.; ZHANG, L.; LI, P. Cloud/shadow detection based on spectral indices for multi/hyperspectral optical remote sensing imagery. ISPRS Journal of Photogrammetry and Remote Sensing, v. 144, p. 235–253, 2018.

ZHANG, Y.; HANG, Y.; GUINDON, B.; CIHLAR, J. An image transform to characterize and compensate for spatial variations in thin cloud contamination of landsat images. Remote Sensing of Environment, v. 82, n. 2-3, p. 173–187, 2002. 7, 8, 21




