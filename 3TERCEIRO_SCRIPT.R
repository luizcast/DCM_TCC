### DATASET INFO #####
#TOTAL FILES: 165371
#TOTAL SIZE: 89.9GB

##### Transformar imagem dos arquivos DICOM em JPEG (reestruturar dados de imagem para JPEG) #####
#no terminal: find ~/Desktop/USP/TCC/MATERIAL/DATA-SET/* -name '*.dcm' -exec dcmdjpeg {} {} \;
#toolkit "DCMTK" - https://dicom.offis.de/dcmtk.php.en
#O R não realiza leitura direta das imagens DICOM (ainda(?))

##### SORT RENAME por SERIE NUMBER #####
#no R: ttt <- c("/home/luiz/Desktop/DATA-SET/")
#as.data.frame(ttt) -> va
#sortDicom(path =va[1,], depth = 1000, forceStack = TRUE)


##### Elimina Arquivos fora de padrão e maiores que o padrão com tamanho 538,0kb #####
#no terminal: find -size +540k -exec rm -r {} {} \; find -size -100k -exec rm -r {} {} \;


##### PACKAGES #####
## Principais DICOM Packages utilizados :
#"oro.dicom", "divest", "tidyverse", "data.table" 

## Como referência para outros projetos
#"RNifti", "espadon"

pacotes <- c("oro.dicom", "tidyverse", "plotly", "readr","imager", "raster", 
             "radtools", "divest", "data.table", "kableExtra", "lmtest", "caret",
             "pROC")
 
if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}



###_OBTENDO A BASE DE TRABALHO_####

#Import List

oro.dicom::readDICOM("~/Desktop/DATA-SET_TESTE/AAAAA/", verbose = TRUE) -> ID
ID_hdr <- ID$hdr
map(ID_hdr,
    ~dplyr::filter(.x, grepl("SeriesNumber|InstanceNumber|SliceLocation|ReconstructionDiameter|DataCollectionDiameter|ExposureTime| SpaceBetweenSlices|TableHeight|Exposure", name))) -> 
  ID_MAIOR
ID_MAIOR[1] %>% kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F, 
                font_size = 18)

##Isolando variaveis

#SerieNumber
map(ID_hdr, ~filter(.x, grepl("SeriesNumber", name))) -> ID_SeriesNumber
map(ID_SeriesNumber, ~dplyr::select(.x, "value")) -> ID_SeriesNumber
rbindlist(ID_SeriesNumber, idcol = "name") -> ID_SeriesNumber

#InstanceNumber
map(ID_hdr, ~filter(.x, grepl("InstanceNumber", name))) -> ID_InstanceNumber
map(ID_InstanceNumber, ~dplyr::select(.x, "value")) -> ID_InstanceNumber
rbindlist(ID_InstanceNumber, idcol = "name") -> ID_InstanceNumber

#SLiceLocation
map(ID_hdr, ~filter(.x, grepl("SliceLocation", name))) -> ID_SliceLocation
map(ID_SliceLocation, ~dplyr::select(.x, "value")) -> ID_SliceLocation
rbindlist(ID_SliceLocation, idcol = "name") -> ID_SliceLocation

#ReconstructionDiameter
map(ID_hdr, ~filter(.x, grepl("ReconstructionDiameter", name))) -> ID_ReconstructionDiameter
map(ID_ReconstructionDiameter, ~dplyr::select(.x, "value")) -> ID_ReconstructionDiameter
rbindlist(ID_ReconstructionDiameter, idcol = "name") -> ID_ReconstructionDiameter

#DataCollectionDiameter
map(ID_hdr, ~filter(.x, grepl("DataCollectionDiameter", name))) -> ID_DataCollectionDiameter
map(ID_DataCollectionDiameter, ~dplyr::select(.x, "value")) -> ID_DataCollectionDiameter
rbindlist(ID_DataCollectionDiameter, idcol = "name") -> ID_DataCollectionDiameter

#ExposureTime e ExposureTimeInms

map(ID_hdr, ~filter(.x, grepl("ExposureTime", name))) -> ID_ExposureTime
map(ID_hdr, ~filter(.x, grepl("ExposureTimeInms", name))) -> ID_ExposureTimeInms
map(ID_ExposureTime, ~filter(.x, grepl("IS", code))) -> ID_ExposureTime
map(ID_ExposureTime, ~dplyr::select(.x, "value")) -> ID_ExposureTime
map(ID_ExposureTimeInms, ~dplyr::select(.x, "value")) -> ID_ExposureTimeInms

rbindlist(ID_ExposureTime, idcol = "name") -> ID_ExposureTime
rbindlist(ID_ExposureTimeInms, idcol = "name") -> ID_ExposureTimeInms

#TableHeight
map(ID_hdr, ~filter(.x, grepl("TableHeight", name))) -> ID_TableHeight
map(ID_TableHeight, ~dplyr::select(.x, "value")) -> ID_TableHeight
rbindlist(ID_TableHeight, idcol = "name") -> ID_TableHeight

ID_SeriesNumber[ID_InstanceNumber, on = .(name)] -> key1
key1[ID_SliceLocation, on = .(name)] -> key2
key2[ID_ReconstructionDiameter, on = .(name)] -> key3
key3[ID_DataCollectionDiameter, on = .(name)] -> key4
key4[ID_ExposureTime, on = .(name)] -> key5
key5[ID_ExposureTimeInms, on = .(name)] -> key6
key6[ID_TableHeight, on = .(name)] -> key7


novas_variaveis <- as_tibble(key7)

nomes_keys <- c("FilePath", 
                "SeriesNumber", 
                "InstanceNumber", 
                "SliceLocation", 
                "ReconstructionDiameter", 
                "DataCollectionDiameter",
                "ExposureTime",
                "ExposureTimeInms",
                "TableHeight")

names(novas_variaveis) <- nomes_keys
glimpse(novas_variaveis)

transform(novas_variaveis,
          SeriesNumber = as.numeric(SeriesNumber),
          InstanceNumber = as.numeric(InstanceNumber),
          SliceLocation = as.numeric(SliceLocation),
          ReconstructionDiameter = as.numeric(ReconstructionDiameter),
          DataCollectionDiameter = as.numeric(DataCollectionDiameter),
          ExposureTime = as.numeric(ExposureTime),
          ExposureTimeInms = as.numeric(ExposureTimeInms),
          TableHeight = as.numeric(TableHeight)
) -> novas_variaveis

glimpse(novas_variaveis)

#write_csv(novas_variaveis, "base_para_anotacao.csv")

#Aqui foram inseridas as anotaçãos sobre a posição  das imagens de interesse 
# na sequencia de fotos. 
# feita no dia 25.07 pelo Dr. Mario

#### BASE PRINCIPAL ####

BASE_PRINCIPAL <- read_csv("BASE_PRINCIPAL.csv")
BASE_PRINCIPAL$ExposureTimeInms <- gsub(BASE_PRINCIPAL$ExposureTimeInms, pattern = ",", replacement = ".")
BASE_PRINCIPAL$ExposureTimeInms <- as.numeric(BASE_PRINCIPAL$ExposureTimeInms)

BASE_PRINCIPAL %>% group_by(Paciente) %>% count() %>% inner_join(BASE_PRINCIPAL, by = "Paciente") %>% 
    ungroup() %>% dplyr::relocate( Paciente, n, .after = FilePath) -> PRINCIPAL
glimpse(PRINCIPAL)
head(PRINCIPAL)


#### Primeiros Insights ####

PRINCIPAL %>% transform(Paciente = as.factor(Paciente),
                        TF1 = as.factor(TF1),
                        TF2 = as.factor(TF2)) -> PRINCIPAL_FACTOR

#Variãvel InstanceNumber

PRINCIPAL_FACTOR %>%  ggplot(aes(InstanceNumber, ..count..)) + 
  geom_density(aes(fill = TF1))


#Variável SliceLocation

PRINCIPAL_FACTOR %>%  ggplot(aes(InstanceNumber, SliceLocation)) + 
  geom_point(aes(color = TF1))

PRINCIPAL_FACTOR %>%  ggplot(aes(SliceLocation, ..count..)) + 
  geom_density(aes(fill = TF1))

#Variavel Paciente n eixo Y

PRINCIPAL_FACTOR %>% ggplot(aes(x = InstanceNumber, y = Paciente, color = TF1)) +
  geom_line(size = 4)

PRINCIPAL_FACTOR %>% ggplot(aes(x = InstanceNumber, y = Paciente, color = TF2)) +
  geom_line(size = 4)

PRINCIPAL_FACTOR %>% ggplot(aes(x = SliceLocation, y = Paciente, color = TF1)) +
  geom_line(size = 6)


#### MODELOS #####
#MODELO TF1

MODELO_TF1 <- glm(formula = TF1 ~ . -Paciente -FilePath -TF2,
                        family = "binomial",
                        data = PRINCIPAL)
summary(MODELO_TF1)
step(MODELO_TF1) -> MODELO_TF1_STEP
summary(MODELO_TF1_STEP)

confusionMatrix(table(predict(MODELO_TF1_STEP, type = "response") >= 0.35, PRINCIPAL$TF1 == 1)[2:1,2:1])


data.frame(Sensitividade = confusionMatrix(table(predict(MODELO_TF1_STEP,
                                                         type = "response") >= 0.35,
                                                 PRINCIPAL$TF1 == 1)[2:1, 2:1])[["byClass"]][["Sensitivity"]],
           Especificidade = confusionMatrix(table(predict(MODELO_TF1_STEP,
                                                          type = "response") >= 0.35,
                                                  PRINCIPAL$TF1 == 1)[2:1, 2:1])[["byClass"]][["Specificity"]],
           Acurácia = confusionMatrix(table(predict(MODELO_TF1_STEP,
                                                    type = "response") >= 0.35,
                                            PRINCIPAL$TF1 == 1)[2:1, 2:1])[["overall"]][["Accuracy"]]) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F, 
                font_size = 27)


ROC1 <- roc(response = PRINCIPAL$TF1, 
            predictor = MODELO_TF1_STEP$fitted.values)

ggplotly(
  ggroc(ROC1, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC1$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((ROC1$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)



#### COMPARANDO MODELOS ####
logLik(MODELO_TF1_STEP)
lrtest(MODELO_TF1,MODELO_TF1_STEP)


####### Criando base de teste para aplicar o modelo #######


oro.dicom::readDICOM("~/Desktop/DATA-SET_TESTE/BBBBBB/", verbose = TRUE) -> base_teste

ID_hdr <- base_teste$hdr
map(ID_hdr,
    ~dplyr::filter(.x, grepl("SeriesNumber|InstanceNumber|SliceLocation|ReconstructionDiameter|DataCollectionDiameter|ExposureTime| SpaceBetweenSlices|TableHeight|Exposure", name))) -> 
  ID_MAIOR

head(ID_MAIOR)

##Isolando variaveis

#SerieNumber
map(ID_hdr, ~filter(.x, grepl("SeriesNumber", name))) -> ID_SeriesNumber
map(ID_SeriesNumber, ~dplyr::select(.x, "value")) -> ID_SeriesNumber
rbindlist(ID_SeriesNumber, idcol = "name") -> ID_SeriesNumber

#InstanceNumber
map(ID_hdr, ~filter(.x, grepl("InstanceNumber", name))) -> ID_InstanceNumber
map(ID_InstanceNumber, ~dplyr::select(.x, "value")) -> ID_InstanceNumber
rbindlist(ID_InstanceNumber, idcol = "name") -> ID_InstanceNumber

#SLiceLocation
map(ID_hdr, ~filter(.x, grepl("SliceLocation", name))) -> ID_SliceLocation
map(ID_SliceLocation, ~dplyr::select(.x, "value")) -> ID_SliceLocation
rbindlist(ID_SliceLocation, idcol = "name") -> ID_SliceLocation

#ReconstructionDiameter
map(ID_hdr, ~filter(.x, grepl("ReconstructionDiameter", name))) -> ID_ReconstructionDiameter
map(ID_ReconstructionDiameter, ~dplyr::select(.x, "value")) -> ID_ReconstructionDiameter
rbindlist(ID_ReconstructionDiameter, idcol = "name") -> ID_ReconstructionDiameter

#DataCollectionDiameter
map(ID_hdr, ~filter(.x, grepl("DataCollectionDiameter", name))) -> ID_DataCollectionDiameter
map(ID_DataCollectionDiameter, ~dplyr::select(.x, "value")) -> ID_DataCollectionDiameter
rbindlist(ID_DataCollectionDiameter, idcol = "name") -> ID_DataCollectionDiameter

#ExposureTime e ExposureTimeInms

map(ID_hdr, ~filter(.x, grepl("ExposureTime", name))) -> ID_ExposureTime
map(ID_hdr, ~filter(.x, grepl("ExposureTimeInms", name))) -> ID_ExposureTimeInms
map(ID_ExposureTime, ~filter(.x, grepl("IS", code))) -> ID_ExposureTime
map(ID_ExposureTime, ~dplyr::select(.x, "value")) -> ID_ExposureTime
map(ID_ExposureTimeInms, ~dplyr::select(.x, "value")) -> ID_ExposureTimeInms

rbindlist(ID_ExposureTime, idcol = "name") -> ID_ExposureTime
rbindlist(ID_ExposureTimeInms, idcol = "name") -> ID_ExposureTimeInms

#TableHeight
map(ID_hdr, ~filter(.x, grepl("TableHeight", name))) -> ID_TableHeight
map(ID_TableHeight, ~dplyr::select(.x, "value")) -> ID_TableHeight
rbindlist(ID_TableHeight, idcol = "name") -> ID_TableHeight

ID_SeriesNumber[ID_InstanceNumber, on = .(name)] -> key1
key1[ID_SliceLocation, on = .(name)] -> key2
key2[ID_ReconstructionDiameter, on = .(name)] -> key3
key3[ID_DataCollectionDiameter, on = .(name)] -> key4
key4[ID_ExposureTime, on = .(name)] -> key5
key5[ID_ExposureTimeInms, on = .(name)] -> key6
key6[ID_TableHeight, on = .(name)] -> key7


novas_variaveis <- as_tibble(key7)

nomes_keys <- c("FilePath", 
                "SeriesNumber", 
                "InstanceNumber", 
                "SliceLocation", 
                "ReconstructionDiameter", 
                "DataCollectionDiameter",
                "ExposureTime",
                "ExposureTimeInms",
                "TableHeight")

names(novas_variaveis) <- nomes_keys
glimpse(novas_variaveis)

transform(novas_variaveis,
          SeriesNumber = as.numeric(SeriesNumber),
          InstanceNumber = as.numeric(InstanceNumber),
          SliceLocation = as.numeric(SliceLocation),
          ReconstructionDiameter = as.numeric(ReconstructionDiameter),
          DataCollectionDiameter = as.numeric(DataCollectionDiameter),
          ExposureTime = as.numeric(ExposureTime),
          ExposureTimeInms = as.numeric(ExposureTimeInms),
          TableHeight = as.numeric(TableHeight)
) %>% as_tibble() -> novas_variaveis 


predict(object = MODELO_BINOMIAL_STEP2, 
        newdata = novas_variaveis,
        type = "response")


##########PLOT INTERESSANTE DO DATACAMP

ggplot(akl_daily, aes(x = max_temp, y = month, height = ..density..)) +
  geom_density_ridges(stat = "density")
