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



##### WRANGLING DATASET WITH DATA.TABLE AND DYPLR PACKAGES #####

##### Join Data.Table #####

oro.dicom::readDICOM("~/Desktop/DATA-SET_TESTE/AAAAA/", verbose = TRUE) -> ID
ID_hdr <- ID$hdr

map(ID_hdr, ~dplyr::filter(.x, grepl("SeriesNumber|InstanceNumber|SliceLocation", name))) -> 
  ID_CORE

#SerieNumber
map(ID_CORE, ~filter(.x, grepl("SeriesNumber", name))) -> ID_SeriesNumber
map(ID_SeriesNumber, ~dplyr::select(.x, "value")) -> ID_SeriesNumber

#InstanceNumber
map(ID_CORE, ~filter(.x, grepl("InstanceNumber", name))) -> ID_InstanceNumber
map(ID_InstanceNumber, ~dplyr::select(.x, "value")) -> ID_InstanceNumber

#SLiceLocation
map(ID_CORE, ~filter(.x, grepl("SliceLocation", name))) -> ID_SliceLocation
map(ID_SliceLocation, ~dplyr::select(.x, "value")) -> ID_SliceLocation

#Data.table
rbindlist(ID_SeriesNumber, idcol = "name") -> SeriesNumber
rbindlist(ID_InstanceNumber, idcol = "name") -> InstanceNumber
rbindlist(ID_SliceLocation, idcol = "name") -> SliceLocation

SeriesNumber[InstanceNumber, on = .(name)] -> k1
k1[SliceLocation, on = .(name)] -> base_key
names(base_key)
nomes_base_key <- c("FilePath", "SerieNumber", "InstanceNumber", "SliceLocation")
names(base_key) <- nomes_base_key
transform(base_key,
          SerieNumber = as.numeric(SerieNumber),
          InstanceNumber = as.numeric(InstanceNumber),
          SliceLocation = as.numeric(SliceLocation)) -> base_key

str(base_key)
base_key %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)


base_key %>% write_csv(file = "table_para_anotacao.csv")

######## TESTANDO O IMPORT DOS DICOM PELA TIBBLE ################
## SUCESS!!!!!! ##
##importing dicom from tibble
#oro.dicom::readDICOM(final$Paciente, verbose = TRUE) -> dicom
oro.dicom::readDICOMFile(base_key$FilePath[1]) -> paciente1
paciente1$hdr -> hdr_paciente1
glimpse(hdr_paciente1)

##aprendendo sobre a base
hdr_paciente1 %>% filter(group == "0010")

##observando import the single file 
oro.dicom::readDICOMFile("/home/luiz/Desktop/testando/AA/1.2.840.113704.1.111.3616.1440707529.12439.dcm") -> 
  sub
hdrsbu <- sub$hdr
sub$hdr %>% filter(grepl("UID", code))


####### JOIN TABLE FIRST OBSERVING ESTUDY ######
#Aqui foi inserida a tabela com a identificação da posição do figado 
#em 20 pacientes, feita no dia 25.07 pelo Dr. Mario.
#Essa tabela foi chamada de first_join.csv


read_csv("first_join.csv", 
         col_names = FALSE, 
         col_types = ("cnncc")) %>% tibble::as_tibble() -> first_join
str(first_join)
class(first_join)
nomes_first_join <- c("FilePath", "Serie", "Instance", "Slice", "TF")
names(first_join) <- nomes_first_join


dplyr::left_join(final, first_join, by = "FilePath")  %>% as_tibble() -> BASE_JOIN_1
str(BASE_JOIN_1)

BASE_JOIN_1 <- BASE_JOIN_1[,c(1:4,8)]
names(BASE_JOIN_1) <- nomes_first_join

edit(BASE_JOIN_1) %>% tibble::as_tibble() -> BASE_JOIN_1

BASE_JOIN_1
write.csv(BASE_JOIN_1, "BASE2.csv")



### BASE3
#A BASE3 foi criada atravez da inserção dos valores no excel

read_csv("BASE3.csv", col_names = c("FilePath", "SeriesNumber", "InstanceNumber", "SliceLocation", "TF", "Paciente"),
         col_types = "cnnnff") %>% as_tibble() -> BASE3

glimpse(BASE3)
str(BASE3)

###### PRIMEIRO INSIGHT #####

BASE3 %>% filter(TF == 1)

BASE3 %>% filter(TF == 1) %>% filter(Instance >= 195)
BASE3 %>% filter(TF == 1) %>% filter(Instance <= 10)

BASE3 %>% filter(Instance <= 200) -> BASE3_FILTER1
glimpse(BASE3_FILTER1)

#Histograma
BASE3_FILTER1 %>%  ggplot(aes(TF, fill = TF)) + geom_histogram(stat = "count")

BASE3_FILTER1 %>% ggplot(aes(Instance, fill =  TF)) + geom_histogram(stat = "count")

#Variãvel Instance


BASE3_FILTER1 %>%  ggplot(aes(Instance, ..count..)) + 
  geom_density(aes(fill = TF))


#Variável Slice

BASE3_FILTER1 %>%  ggplot(aes(Instance, Slice)) + 
  geom_point(aes(color = TF))

BASE3_FILTER1 %>%  ggplot(aes(Slice, ..count..)) + 
  geom_density(aes(fill = TF))

#Variavel Paciente n eixo Y

BASE3_FILTER1 %>% ggplot(aes(x = Instance, y = Paciente, color = TF)) +
  geom_line(size = 6)

BASE3_FILTER1 %>% ggplot(aes(x = Slice, y = Paciente, color = TF)) +
  geom_line(size = 6)

BASE3_FILTER1 %>% group_by(Paciente) %>% filter(Paciente == "PACIENTE12")

BASE3_FILTER1 %>% filter(Slice <= 0) %>% group_by(Paciente) %>% summary()

##CONSTRUINDO BASE X de CONTAGEM DE IMAGENS POR PACIENTES
by_paciente <- BASE3_FILTER1 %>% group_by(Paciente)
by_paciente %>% tally()  -> baseX

#write_csv(baseX, "basex.csv")

## DIVIDINDO EM 3 GRANDES GRUPOS QUE FORAM OBSERVADOS

baseX %>% filter(n >= 170) -> grupo1
baseX %>% filter(n <= 170) -> grupo2

left_join(grupo1, BASE3_FILTER1) -> BASE_GRUPO1
left_join(grupo2, BASE3_FILTER1) -> BASE_GRUPO2

BASE_GRUPO1 %>%  ggplot(aes(x = Instance, y = Paciente, color = TF)) +
  geom_line(size = 6)

BASE_GRUPO2 %>% ggplot(aes(x = Instance, y = Paciente, color = TF)) +
  geom_line(size = 6)

BASE_GRUPO2 %>% filter(Paciente == "PACIENTE12") -> paciente12

anti_join(BASE_GRUPO2, paciente12) -> BASE_GRUPO2_SEM_REVERT


## PRIMEIRO MODELO GRUPO 2 SEM INVERT (n < 170)

MODELO_poisson <- glm(formula = TF ~ Instance + Slice,
                      family = "binomial",
                    data = BASE_GRUPO2_SEM_REVERT)

predict(object = MODELO_poisson, 
        data.frame(Instance = 1, Slice = 498), 
        type = "response")


summary(MODELO_poisson)
logLik(MODELO_poisson)
lrtest(MODELO_poisson) 

BASE_GRUPO2_SEM_REVERT$phat <- MODELO_poisson$fitted.values

ID_hdr[300]
       
BASE_GRUPO2_SEM_REVERT %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

##MATRIZ DE CONFUSÃO
confusionMatrix(table(predict(MODELO_poisson, type = "response") >= 0.5,
                      BASE_GRUPO2_SEM_REVERT$TF == 1)[2:1, 2:1])


data.frame(Sensitividade = confusionMatrix(table(predict(MODELO_poisson,
                                                         type = "response") >= 0.5,
                                                 BASE_GRUPO2_SEM_REVERT$TF == 1)[2:1, 2:1])[["byClass"]][["Sensitivity"]],
           Especificidade = confusionMatrix(table(predict(MODELO_poisson,
                                                          type = "response") >= 0.5,
                                                  BASE_GRUPO2_SEM_REVERT$TF == 1)[2:1, 2:1])[["byClass"]][["Specificity"]],
           Acurácia = confusionMatrix(table(predict(MODELO_poisson,
                                                    type = "response") >= 0.5,
                                            BASE_GRUPO2_SEM_REVERT$TF == 1)[2:1, 2:1])[["overall"]][["Accuracy"]]) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F, 
                font_size = 27)


ROC <- roc(response = BASE_GRUPO2_SEM_REVERT$TF, 
           predictor = MODELO_poisson$fitted.values)

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

##### Aqui foi observado que o modelo pode melhorar o desempenho #####
### da Acurácia, da Especificidade e da Sensitividade se outras variãveis de interesse 
### forem indentificadas. 


#### Incluindo novas variáveis na base para gerar modelo #####

oro.dicom::readDICOM("~/Desktop/DATA-SET_TESTE/AAAAA/", verbose = TRUE) -> ID
ID_hdr <- ID$hdr
map(ID_hdr,
    ~dplyr::filter(.x, grepl("SeriesNumber|InstanceNumber|SliceLocation|ReconstructionDiameter|DataCollectionDiameter|ExposureTime| SpaceBetweenSlices|TableHeight|Exposure", name))) -> 
  ID_MAIOR


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

write_csv(novas_variaveis, "base_para_anotacao.csv")



novas_variaveis[is.na(novas_variaveis$SeriesNumber), ] %>% glimpse()

right_join(BASE3, novas_variaveis, by="FilePath") -> GRANDE_JOIN 
GRANDE_JOIN %>% filter(SeriesNumber.x != SeriesNumber.y)

glimpse(GRANDE_JOIN)
GRANDE_JOIN %>% dplyr::select(1,2,3,4,5,6,10,11,12,13,14) -> BASE4
glimpse(BASE4)
summary(BASE3)
summary(GRANDE_JOIN)
class(BASE4)
names(BASE4)


names(BASE4) <- c("FilePath", 
                "SeriesNumber", 
                "InstanceNumber", 
                "SliceLocation",
                "TF",
                "Paciente",
                "ReconstructionDiameter", 
                "DataCollectionDiameter",
                "ExposureTime",
                "ExposureTimeInms",
                "TableHeight"
                )


BASE_PRECISA <- read_delim("BASE_PRECISA.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)


anti_join(novas_variaveis, BASE_PRECISA, by = "FilePath") -> complemento
write_csv2(complemento, "Complemento.csv")
glimpse(BASE_PRECISA)
glimpse(Complemento_completo)

BASE_PRECISA %>%  dplyr::select(-Paciente, -n) -> BASE_X
BASE_PRECISA$TF <-as.factor(BASE_X$TF)
BASE_PRECISA$TF2 <-as.factor(BASE_X$TF2)
glimpse(BASE_X)

bind_rows(BASE_X, Complemento_completo) -> UNIAO
glimpse(UNIAO)
as_tibble(UNIAO) -> UNIAO

UNIAO[is.na(UNIAO$TF), ]


summary(UNIAO)
UNIAO %>% dplyr::select(-ExposureTime, -ExposureTimeInms, )


#write_csv(UNIAO, "BASE_PRINCIPAL.csv")




BASE_PRINCIPAL$TF1 <- as.factor(BASE_PRINCIPAL$TF1)
BASE_PRINCIPAL$TF2 <- as.factor(BASE_PRINCIPAL$TF2)
BASE_PRINCIPAL$Paciente <- as.factor(BASE_PRINCIPAL$Paciente)

glimpse(BASE_PRINCIPAL)


BASE_PRINCIPAL %>% group_by(Paciente) %>% count() %>% inner_join(BASE_PRINCIPAL, by = "Paciente") %>% 
  ungroup() %>% dplyr::relocate( Paciente, n, .after = FilePath) -> PRINCIPAL
glimpse(PRINCIPAL)



#modelo 1
MODELO_BINOMIAL <- glm(formula = TF2 ~ . -Paciente -FilePath,
                      family = "binomial",
                      data = PRINCIPAL)
summary(MODELO_BINOMIAL)
step(MODELO_BINOMIAL) -> MODELO_BINOMIAL_STEP


summary(MODELO_BINOMIAL_STEP)
logLik(MODELO_BINOMIAL_STEP)
lrtest(MODELO_BINOMIAL_STEP)


BASE4$Fit <- MODELO_BINOMIAL_STEP$fitted.values
 


write_csv2(BASE4, "BASE_PRECISA.csv")


read_delim("BASE_ANOTADA.csv",
           delim = ";", escape_double = FALSE, trim_ws = TRUE, 
           col_types = ("cnnfnnnnnnfnf")) %>% 
  dplyr::relocate( TF2, .after = TF) %>% 
  as_tibble() -> BASE4_ANOTADA

##MATRIZ DE CONFUSÃO


confusionMatrix(table(predict(MODELO_BINOMIAL_STEP, type = "terms") >= 0.5, BASE4$TF == 1)[2:1,2:1])
count(BASE4)
count(MODELO_BINOMIAL_STEP$fitted.values)
MODELO_BINOMIAL_STEP
  data.frame(Sensitividade = confusionMatrix(table(predict(NOVO_MODELO_poisson,
                                                         type = "response") >= 0.5,
                                                 BASE4_SEM_NAs$TF == 1)[2:1, 2:1])[["byClass"]][["Sensitivity"]],
           Especificidade = confusionMatrix(table(predict(NOVO_MODELO_poisson,
                                                          type = "response") >= 0.5,
                                                  BASE4_SEM_NAs$TF == 1)[2:1, 2:1])[["byClass"]][["Specificity"]],
           Acurácia = confusionMatrix(table(predict(NOVO_MODELO_poisson,
                                                    type = "response") >= 0.5,
                                            BASE4_SEM_NAs$TF == 1)[2:1, 2:1])[["overall"]][["Accuracy"]]) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F, 
                font_size = 27)


ROC <- roc(response = BASE4_SEM_NAs$TF, 
           predictor = NOVO_MODELO_poisson$fitted.values)

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
#aqui serâ necessario criar um dataset para tentar predizer a posição das imagens de interesse

##MATRIZ DE CONFUSÃO
confusionMatrix(table(predict(STEP_CONTAGEM, type = "response") >= 0.5,
                      BASE6$TF == 1)[2:1, 2:1])

ROC <- roc(response = BASE6$TF, 
           predictor = STEP_CONTAGEM$fitted.values)

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





summary(BASE6)


lrtest(STEP_CONTAGEM, NOVO_MODELO_poisson, NOVO_MODELO_com_contagem )

#Multinivel
#Estimação do modelo com Interceptos Aleatórios
modelo_intercept_hlm2 <- lme(fixed = TF ~ SliceLocation,
                             random = ~ Paciente | ExposureTimeInms,
                             data = BASE6,
                             method = "REML")

BASE6

