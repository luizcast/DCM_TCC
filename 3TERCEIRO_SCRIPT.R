### DATASET INFO #####
#TOTAL FILES: 165371
#TOTAL SIZE: 89.9GB


##### PACKAGES #####
## Principais DICOM Packages utilizados :
#"oro.dicom", "divest" 

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

PRINCIPAL_FACTOR %>% ggplot(aes(x = InstanceNumber, y = Paciente, color = TF2)) +
  geom_line(size = 4)

PRINCIPAL_FACTOR %>% ggplot(aes(x = SliceLocation, y = Paciente, color = TF1)) +
  geom_line(size = 6)


#### MODELOS #####
#MODELO 1 calculo de TF2 com TF1
MODELO_BINOMIAL <- glm(formula = TF2 ~ . -Paciente -FilePath -n,
                       family = "binomial",
                       data = PRINCIPAL)
summary(MODELO_BINOMIAL)
step(MODELO_BINOMIAL) -> MODELO_BINOMIAL_STEP


summary(MODELO_BINOMIAL_STEP)
logLik(MODELO_BINOMIAL_STEP)
lrtest(MODELO_BINOMIAL_STEP)

confusionMatrix(table(predict(MODELO_BINOMIAL_STEP, type = "response") >= 0.5, PRINCIPAL$TF2 == 1)[2:1,2:1])


data.frame(Sensitividade = confusionMatrix(table(predict(MODELO_BINOMIAL_STEP,
                                                         type = "response") >= 0.5,
                                                 PRINCIPAL$TF2 == 1)[2:1, 2:1])[["byClass"]][["Sensitivity"]],
           Especificidade = confusionMatrix(table(predict(MODELO_BINOMIAL_STEP,
                                                          type = "response") >= 0.5,
                                                  PRINCIPAL$TF2 == 1)[2:1, 2:1])[["byClass"]][["Specificity"]],
           Acurácia = confusionMatrix(table(predict(MODELO_BINOMIAL_STEP,
                                                    type = "response") >= 0.5,
                                            PRINCIPAL$TF2 == 1)[2:1, 2:1])[["overall"]][["Accuracy"]]) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F, 
                font_size = 27)


ROC <- roc(response = PRINCIPAL$TF2, 
           predictor = MODELO_BINOMIAL_STEP$fitted.values)

ggplotly(
  ggroc(ROC, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((ROC$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)


#MODELO 2 calciulo de TF2 com "n", "TF1"
MODELO_BINOMIAL2 <- glm(formula = TF2 ~ . -Paciente -FilePath,
                       family = "binomial",
                       data = PRINCIPAL)
summary(MODELO_BINOMIAL2)
step(MODELO_BINOMIAL2) -> MODELO_BINOMIAL_STEP2


summary(MODELO_BINOMIAL_STEP2)
logLik(MODELO_BINOMIAL_STEP2)
lrtest(MODELO_BINOMIAL_STEP2)

confusionMatrix(table(predict(MODELO_BINOMIAL_STEP2, type = "response") >= 0.5, PRINCIPAL$TF2 == 1)[2:1,2:1])


data.frame(Sensitividade = confusionMatrix(table(predict(MODELO_BINOMIAL_STEP2,
                                                         type = "response") >= 0.5,
                                                 PRINCIPAL$TF2 == 1)[2:1, 2:1])[["byClass"]][["Sensitivity"]],
           Especificidade = confusionMatrix(table(predict(MODELO_BINOMIAL_STEP2,
                                                          type = "response") >= 0.5,
                                                  PRINCIPAL$TF2 == 1)[2:1, 2:1])[["byClass"]][["Specificity"]],
           Acurácia = confusionMatrix(table(predict(MODELO_BINOMIAL_STEP2,
                                                    type = "response") >= 0.5,
                                            PRINCIPAL$TF2 == 1)[2:1, 2:1])[["overall"]][["Accuracy"]]) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F, 
                font_size = 27)


ROC2 <- roc(response = PRINCIPAL$TF2, 
           predictor = MODELO_BINOMIAL_STEP2$fitted.values)

ggplotly(
  ggroc(ROC2, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC2$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((ROC2$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

#MODELO 3 com "n" sem "TF1"

MODELO_BINOMIAL3 <- glm(formula = TF2 ~ . -Paciente -FilePath -TF1,
                       family = "binomial",
                       data = PRINCIPAL)
summary(MODELO_BINOMIAL3)
step(MODELO_BINOMIAL3) -> MODELO_BINOMIAL_STEP3

confusionMatrix(table(predict(MODELO_BINOMIAL_STEP3, type = "response") >= 0.5, PRINCIPAL$TF2 == 1)[2:1,2:1])


data.frame(Sensitividade = confusionMatrix(table(predict(MODELO_BINOMIAL_STEP3,
                                                         type = "response") >= 0.5,
                                                 PRINCIPAL$TF2 == 1)[2:1, 2:1])[["byClass"]][["Sensitivity"]],
           Especificidade = confusionMatrix(table(predict(MODELO_BINOMIAL_STEP3,
                                                          type = "response") >= 0.5,
                                                  PRINCIPAL$TF2 == 1)[2:1, 2:1])[["byClass"]][["Specificity"]],
           Acurácia = confusionMatrix(table(predict(MODELO_BINOMIAL_STEP3,
                                                    type = "response") >= 0.5,
                                            PRINCIPAL$TF2 == 1)[2:1, 2:1])[["overall"]][["Accuracy"]]) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F, 
                font_size = 27)


ROC3 <- roc(response = PRINCIPAL$TF2, 
            predictor = MODELO_BINOMIAL_STEP3$fitted.values)

ggplotly(
  ggroc(ROC3, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC3$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((ROC3$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

#Modelo 4 calculo de TF1

MODELO_BINOMIAL4 <- glm(formula = TF1 ~ . -Paciente -FilePath -TF2,
                        family = "binomial",
                        data = PRINCIPAL)
summary(MODELO_BINOMIAL4)
step(MODELO_BINOMIAL4) -> MODELO_BINOMIAL_STEP4

confusionMatrix(table(predict(MODELO_BINOMIAL_STEP4, type = "response") >= 0.5, PRINCIPAL$TF2 == 1)[2:1,2:1])


data.frame(Sensitividade = confusionMatrix(table(predict(MODELO_BINOMIAL_STEP4,
                                                         type = "response") >= 0.5,
                                                 PRINCIPAL$TF2 == 1)[2:1, 2:1])[["byClass"]][["Sensitivity"]],
           Especificidade = confusionMatrix(table(predict(MODELO_BINOMIAL_STEP4,
                                                          type = "response") >= 0.5,
                                                  PRINCIPAL$TF2 == 1)[2:1, 2:1])[["byClass"]][["Specificity"]],
           Acurácia = confusionMatrix(table(predict(MODELO_BINOMIAL_STEP4,
                                                    type = "response") >= 0.5,
                                            PRINCIPAL$TF2 == 1)[2:1, 2:1])[["overall"]][["Accuracy"]]) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F, 
                font_size = 27)


ROC4 <- roc(response = PRINCIPAL$TF2, 
            predictor = MODELO_BINOMIAL_STEP4$fitted.values)

ggplotly(
  ggroc(ROC4, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC4$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((ROC4$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

#Modelo 5 calculo de TF1 sem paciente17
PRINCIPAL %>% group_by(Paciente) %>% filter(Paciente == "PACIENTE17") -> PACIENTE17

anti_join(PRINCIPAL, PACIENTE17, by = "Paciente") -> SEM_17


MODELO_BINOMIAL5 <- glm(formula = TF1 ~ . -Paciente -FilePath -TF2,
                        family = "binomial",
                        data = SEM_17)
summary(MODELO_BINOMIAL5)
step(MODELO_BINOMIAL5) -> MODELO_BINOMIAL_STEP5

confusionMatrix(table(predict(MODELO_BINOMIAL_STEP5, type = "response") >= 0.5, SEM_17$TF2 == 1)[2:1,2:1])


data.frame(Sensitividade = confusionMatrix(table(predict(MODELO_BINOMIAL_STEP4,
                                                         type = "response") >= 0.5,
                                                 PRINCIPAL$TF2 == 1)[2:1, 2:1])[["byClass"]][["Sensitivity"]],
           Especificidade = confusionMatrix(table(predict(MODELO_BINOMIAL_STEP4,
                                                          type = "response") >= 0.5,
                                                  PRINCIPAL$TF2 == 1)[2:1, 2:1])[["byClass"]][["Specificity"]],
           Acurácia = confusionMatrix(table(predict(MODELO_BINOMIAL_STEP4,
                                                    type = "response") >= 0.5,
                                            PRINCIPAL$TF2 == 1)[2:1, 2:1])[["overall"]][["Accuracy"]]) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F, 
                font_size = 27)


ROC4 <- roc(response = PRINCIPAL$TF2, 
            predictor = MODELO_BINOMIAL_STEP4$fitted.values)

ggplotly(
  ggroc(ROC4, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC4$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((ROC4$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)



#### COMPARANDO MODELOS ####
logLik(MODELO_BINOMIAL_STEP)
logLik(MODELO_BINOMIAL_STEP2)
logLik(MODELO_BINOMIAL_STEP3)
logLik(MODELO_BINOMIAL_STEP4)
lrtest(MODELO_BINOMIAL_STEP, MODELO_BINOMIAL_STEP2, MODELO_BINOMIAL_STEP3)



my_plot <-
  data.frame(MODELO1 = logLik(MODELO_BINOMIAL_STEP),
             MODELO2 = logLik(MODELO_BINOMIAL_STEP2),
             MODELO3 = logLik(MODELO_BINOMIAL_STEP3)) %>% 
 data.table::melt() 
names_data_modelo <- c("MODELO", "LOGLIK")
names(my_plot) <- names_data_modelo

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
