#### INICIO ####
#Existe um facínio em ver a máquina acertar. Quanto mais ela acerta, mais dúvidas aparecem sobre o a realidade que nos cerca. 


### DATASET INFO #####
#TOTAL FILES: 165371
#TOTAL SIZE: 89.9GB
#FILE FORMAT: Tomographic images and Metadata of Medical Exams in DICOM format
#FILE EXTENSION: ".dcm"

# https://www.dicomstandard.org/ai

###PACKAGES####
   
#oro.dicom() - https://cran.r-project.org/web/packages/oro.dicom/index.html
#dcmtk() - https://dicom.offis.de/dcmtk.php.en
#divest() - 
#tidyverse() - https://www.tidyverse.org/

func_init <-function(){pacotes <- c("keras", "tensorflow", "tidyverse", "oro.dicom","dcmtk", "readxl", "tinytex", "divest")
if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
}
func_init()
tf_config()
tf_gpu_configured()


###PATH####
path = "/home/luiz/Desktop/DATA-SET_TESTE/"

#### 1.Transformar as imagem dos arquivos DICOM em JPEG (reestruturar dados de imagem para JPEG) #####
#O R NÃO realiza leitura direta das imagens DICOM 
#É necessário CONVERTER AS IMAGENS PARA O FORMATO JPEG(ainda(?))
#usar o pacote "DCMTK" no terminal: 
# sudo apt install dcmtk
# find ~/Desktop/USP/TCC/MATERIAL/DATA-SET/* -name '*.dcm' -exec dcmdjpeg {} {} \;


#### 2.TRATANDO BANCO DE IMAGENS DICOM ####
#renomeando os arquivos com o sortDICOM
#library(divest)
#sortDicom(path = path, labelFormat = "%i_%s_%r", forceStack = FALSE)

#### 3.ANONIMIZANDO A BASE #####

#library(dcmtk)
#list.files(path) %>% as_tibble() -> anon_table
#for (obs in anon_table) {paste(file.path(path),obs,sep='')} %>% as_tibble()-> anon_table
#for (value in anon_table$value){dcm_anon(value)}

#### Preparando Dataset annot_clean ####
#Esta base foi anotada pelos médicos da UNIFESP.
read_excel("~/Desktop/USP/TCC/projeto_TCC/annot_clean.xlsx") %>% as_tibble() -> annot_clean
tail(annot_clean, 20)
glimpse(annot_clean)
gsub(pattern = "Não se aplica", replacement = "0", annot_clean$`Densidade não habitual (sim=1, não=0)`) -> annot_clean$`Densidade não habitual (sim=1, não=0)`
gsub(pattern = "Não se aplica", replacement = "NA", annot_clean$`Corte INICIAL vesícula`) -> annot_clean$`Corte INICIAL vesícula`
gsub(pattern = "Não se aplica", replacement = "NA", annot_clean$`Corte FINAL vesícula`) -> annot_clean$`Corte FINAL vesícula`

annot_clean$`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` = as.factor(annot_clean$`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`)
annot_clean$`Corte FINAL vesícula` = as.numeric(annot_clean$`Corte FINAL vesícula`)
annot_clean$`Corte INICIAL vesícula` = as.numeric(annot_clean$`Corte INICIAL vesícula`)
annot_clean$`Densidade não habitual (sim=1, não=0)` = as.numeric(annot_clean$`Densidade não habitual (sim=1, não=0)`)
glimpse(annot_clean)
table(annot_clean$`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`)
annot_clean


##### Criando Dataset que vai gerar as imagens #####
list.files(path) %>%  as_tibble() -> array_arquivos_dicom_tratados
separate(array_arquivos_dicom_tratados, col = value, sep = "_", into = c("PatientID","SerieNumber","InstanceNumber")) -> index_dicom
index_dicom$InstanceNumber %>% str_sub(end=-5) -> index_dicom$InstanceNumber
bind_cols(array_arquivos_dicom_tratados,index_dicom) -> index_dicom
  index_dicom
names(index_dicom) <- c("FileName","PatientID","SerieNumber","InstanceNumber")

          index_dicom$PatientID = as.numeric(index_dicom$PatientID)
          index_dicom$SerieNumber = as.numeric(index_dicom$SerieNumber)
          index_dicom$InstanceNumber = as.numeric(index_dicom$InstanceNumber) 

#join com annot_clean
index_dicom %>% group_by(PatientID) %>% 
  left_join(annot_clean, by = c("PatientID" = "Patient ID")) %>% 
  ungroup() -> index_dicom
index_dicom$FilePath <- paste(path, index_dicom$FileName, sep='')
glimpse(index_dicom)

index_dicom %>% dplyr::select(1:9) -> base_tratada
base_tratada %>% dplyr::select(-5) -> base_tratada
base_tratada$FilePath <- paste(path, base_tratada$FileName, sep='')
glimpse(base_tratada)
base_tratada %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`%in% NA) -> base_NAs
base_tratada %>% anti_join(base_NAs) -> base_tratada

#### GERANDO TABELAS #####
#TABELA FREQUENCIA
annot_clean %>% ggplot(aes(x = `Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`)) + geom_bar(aes(fill = `Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`)) + ylim(0, 105) + geom_text(stat = 'count', aes(label = ..count..), vjust=-0.5) + ggtitle("annot_clean") + ylab("contagem") + xlab(label ='') + theme(legend.position = "none")

#plot FREQUENCIA
base_tratada %>% ggplot(aes(x = `Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`)) + geom_bar(aes(fill = `Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`)) + ylim(0, 80000) + geom_text(stat = 'count', aes(label = ..count..), vjust=-0.5) + ggtitle("base_tratada") + ylab("contagem") + theme(legend.position = "none")

base_tratada %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`%in% c("C","S")) %>% nrow()

#n de Imagens por SerieNumber
base_tratada %>% group_by(PatientID) %>% count(SerieNumber) %>% ungroup(PatientID)

#TABELA SEM VESICULA COM CLIPE
base_tratada %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` == "C") %>% nrow()

#TABELA SEM VESICULA SEM CLIPE
base_tratada %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` == "S") %>% nrow()

#TABELA COM VESICULA HIPOESTENDIDA
base_tratada %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` == "H") %>% nrow()

#TABELA COM VESICULA NORMAL
base_vesicula_normal <- base_tratada %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` == "V")
base_vesicula_normal

#TABELA COM AS IMAGENS DA VESICULA
base_vesicula_normal %>% mutate(posicao_interesse = ifelse(c(InstanceNumber >= `Corte INICIAL vesícula` & 
                                                               InstanceNumber <= `Corte FINAL vesícula`), 1, 0)) -> base_frames_vesicula_normal
base_frames_vesicula_normal %>% filter(posicao_interesse == 1) -> base_frames_vesicula_normal
glimpse(base_frames_vesicula_normal)

#QUAL SERA A SERIE? CORRESPONDENTE A IMAGEM ANOTADA?#
base_vesicula_normal %>% group_by(PatientID) %>% count(SerieNumber) %>% ungroup() -> tabela_n_por_serie
base_vesicula_normal %>% group_by(PatientID) %>% count(SerieNumber) %>% ungroup() %>% count(PatientID) 

#### AMOSTRA VESICULA NORMAL ####

base_frames_vesicula_normal %>% unite("ID_SERIE", PatientID:SerieNumber) -> ID_SERIE
base_frames_vesicula_normal[!duplicated(ID_SERIE$ID_SERIE),] -> ID_UNICO
ID_UNICO$MEDIA <- c(ID_UNICO$`Corte INICIAL vesícula` + ID_UNICO$`Corte FINAL vesícula`) / 2
ID_UNICO$MEDIA <- as.integer(ID_UNICO$MEDIA)
ID_UNICO %>% unite("PatientID","PatientID","SerieNumber","MEDIA", sep = "_") %>% select(PatientID) -> gerador
ID_UNICO %>% select(PatientID,SerieNumber,MEDIA) -> PatienIDSerieNumberMEDIA
bind_cols(gerador, PatienIDSerieNumberMEDIA) -> geradorPatienIDSerieNumberMEDIA
geradorPatienIDSerieNumberMEDIA$FINAL <- paste("0000",geradorPatienIDSerieNumberMEDIA$PatientID...1,".dcm", sep = '')
str_length(geradorPatienIDSerieNumberMEDIA$FINAL)
geradorPatienIDSerieNumberMEDIA %>% mutate(FINAL = ifelse(str_length(FINAL) >= 20, str_sub(FINAL,start=2L), FINAL)) -> geradorPatienIDSerieNumberMEDIA
geradorPatienIDSerieNumberMEDIA %>% left_join(base_frames_vesicula_normal, by=c("FINAL"="FileName")) %>% select(FilePath) -> vesicula_select
str_length(geradorPatienIDSerieNumberMEDIA$FINAL)
geradorPatienIDSerieNumberMEDIA

##### AMOSTRA VESICULA HIPODISTENDIDA #####

base_tratada %>% 
  filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` == "H") %>% 
  mutate(posicao_interesse = ifelse(c(InstanceNumber >= `Corte INICIAL vesícula` & 
                                        InstanceNumber <= `Corte FINAL vesícula`), 1, 0)) %>% 
  filter(posicao_interesse == 1)  %>% 
  mutate(MEDIA = c(`Corte INICIAL vesícula`+`Corte FINAL vesícula`)/2) %>% 
  filter(InstanceNumber == MEDIA) %>% 
  select(-posicao_interesse, -MEDIA) %>% 
  .[!duplicated(.$PatientID),] -> imgs_hipo


#### CRIANDO MODELO GLM #####

base_tratada %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` %in% c('H','V')) %>% 
  filter(str_length(FileName) <= 21)-> tabela_GLM
tabela_GLM %>% mutate(posicao_interesse = ifelse(c(InstanceNumber >= `Corte INICIAL vesícula` & 
                                      InstanceNumber <= `Corte FINAL vesícula`), 1,0))  -> tabela_GLM
tabela_GLM$PatientID = as.factor(tabela_GLM$PatientID)
table(tabela_GLM$`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`)

#modelo
glm(posicao_interesse~InstanceNumber+SerieNumber, family = binomial, data = tabela_GLM) -> modelo_glm
summary(modelo_glm)
step(modelo_glm) -> modelo_fit
summary(modelo_fit)
tabela_GLM$fit <- modelo_fit$fitted.values

tabela_GLM %>% filter(tabela_GLM$posicao_interesse == 1) %>% summary()

#### AMOSTRA CLIPE (aplicando o modelo GLM) ####

summary(modelo_fit)
tabela_GLM$fit %>% summary()

predict(object = modelo_fit, 
        data.frame(base_clipe), 
        type = "response") -> base_clipe$fit
tabela_GLM$fit %>% summary()
ggplot(tabela_GLM, aes(fit)) + geom_histogram(bins = 10)
base_clipe$fit %>% summary()
ggplot(base_clipe, aes(fit)) + geom_histogram(bins = 10)
base_clipe %>% filter(fit >= 0.025 & fit <= 0.21)

#achando a média
c(as.integer(mean(base_vesicula_normal$`Corte INICIAL vesícula`)))-10 -> ENTRADA
c(as.integer(mean(base_frames_vesicula_normal$`Corte FINAL vesícula`)))+10 -> SAIDA
base_clipe %>% filter(fit >= 0.025 & fit <= 0.21) %>% filter(InstanceNumber >= ENTRADA & InstanceNumber <= SAIDA) -> clipe_select
clipe_select$PatientID <- as.factor(clipe_select$PatientID)
glimpse(clipe_select)
clipe_select %>% count(PatientID) %>% glimpse()


#### GERANDO BANCO DE IMAGENS #####
#função para indexar a img na tabela
temp_dcm_export <- function(arg1, arg2, arg3){dcmj2pnm(arg1, opt = arg2, outfile = arg3, "--use-window 1")}


#PRONTO! SÓ GERAR!

#gerando para saber qual serie
amostra_vesicula %>% group_by(FilePath) %>% 
    mutate(imagePath = temp_dcm_export(FilePath, "--write-jpeg", make.names(paste(str_sub(FilePath, start = 35L, end = -5L), "png", sep = "."),unique = TRUE))) %>% ungroup() -> bomba_com_img
clipe_select %>% group_by(FilePath) %>% 
  mutate(imagePath = temp_dcm_export(FilePath, "write-jpeg", make.names(paste(str_sub(FilePath, start = 35L, end = -5L), "png", sep = "."),unique = TRUE))) %>% ungroup() -> clipe_select_outs

list.files("~/Desktop/IMAGENS_KERAS/VESICULA/") %>%  as_tibble() -> vesic
separate(vesic, col = value, sep = "_", into = c("FileName","SerieNumber","InstanceNumber")) -> vesic
vesic$InstanceNumber %>% str_sub(end=-5) -> vesic$InstanceNumber


imgs_vesicula_norm %>% group_by(FilePath) %>% 
  mutate(imagePath = temp_dcm_export(FilePath, "--write-jpeg", make.names(paste(str_sub(FilePath, start = 35L, end = -5L), "png", sep = "."),unique = TRUE))) %>% ungroup()

temp_dcm_export(base_tratada$FilePath, '--write-jpeg', make.names(paste(str_sub(FilePath, start = 35L, end = -5L), "png", sep = "."),unique = TRUE))
  

amostra_vesicula %>% mutate(id = str_sub(FilePath, start = 35L, end = -5L)) %>% 
  separate(col = id, sep = "_", into = c("FileName","SerieNumber","InstanceNumber")) %>% right_join(vesic) -> tabela_imgs_vesicula
tabela_imgs_vesicula
<<<<<<< HEAD
=======

>>>>>>> cda627d3ce7d4a7920a971f59a26c7a783a8240e



base_vesicula_normal %>% filter(PatientID == 3870806 & SerieNumber == 2) %>% filter(InstanceNumber >= 50 & InstanceNumber <= 75) %>% group_by(FilePath) %>% 
  mutate(imagePath = temp_dcm_export(FilePath, "--write-jpeg", make.names(paste(str_sub(FilePath, start = 35L, end = -5L), "png", sep = "."),unique = TRUE))) %>% ungroup() -> prova_imgs_out




#####

list.files("~/prova/") %>% as_tibble() %>% separate(col = value, sep = "_", into = c("PatientID","SerieNumber","InstanceNumber")) -> ID_utilizados
ID_utilizados$InstanceNumber %>% str_sub(end=-5) -> ID_utilizados$InstanceNumber
ID_utilizados$PatientID %>% str_sub(start=2) -> ID_utilizados$PatientID
ID_utilizados
bind_cols(list.files("~/prova/"), ID_utilizados) -> ID_utilizados
c("FilePath", "PatientID", "SerieNumber", "InstanceNumber") -> names(ID_utilizados)

ID_utilizados$PatientID <- as.factor(ID_utilizados$PatientID)
ID_utilizados$SerieNumber <- as.numeric(ID_utilizados$SerieNumber)
ID_utilizados$InstanceNumber <- as.numeric(ID_utilizados$InstanceNumber)
glimpse(ID_utilizados)
glimpse(tabela_GLM)
tabela_GLM %>% anti_join(ID_utilizados, by = "PatientID") -> anti_ID
glimpse(anti_ID) %>% group_by(PatientID) %>% count(SerieNumber)
summary(anti_ID$PatientID)

anti_ID$FilePath <- paste(path, anti_ID$FileName, sep='')
anti_ID %>% group_by(FilePath) %>% 
  mutate(imagePath = temp_dcm_export(FilePath, "--write-jpeg", make.names(paste(str_sub(FilePath, start = 35L, end = -5L), "png", sep = "."),unique = TRUE))) %>% ungroup() -> imgs_out_anti

sum(table(base_tratada$`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`)) %>% kable(format = "simple")
table(annot_clean$`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`)
summary(base_tratada)

base_tratada %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` %in% NA) -> base_NAs
base_NAs$PatientID = as.factor(base_NAs$PatientID)
base_NAs$PatientID %>% summary()

base_tratada %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` %in% NA) %>% head() %>% group_by(FilePath) %>% 
  mutate(imagePath = temp_dcm_export(FilePath, "--write-jpeg", make.names(paste(str_sub(FilePath, start = 35L, end = -5L), "png", sep = "."),unique = TRUE))) %>% ungroup() -> NASSSS



###### TRATANDO BASE AMOSTRA #####

path3 = "/home/luiz/amostra/"
list.files(path = path3) %>% as_tibble() -> array_imgs_amostra
list.files(path = path3) %>% as_tibble() %>% 
  separate(col = value, sep = "_", into = c("PatientID","SerieNumber","InstanceNumber")) -> imgs_amostra

imgs_amostra$InstanceNumber %>% str_sub(end=-5) -> imgs_amostra$InstanceNumber
imgs_amostra$PatientID %>% str_sub(start=2) -> imgs_amostra$PatientID

imgs_amostra$PatientID = as.numeric(imgs_amostra$PatientID)
imgs_amostra$SerieNumber = as.numeric(imgs_amostra$SerieNumber)
imgs_amostra$InstanceNumber = as.numeric(imgs_amostra$InstanceNumber)

base_tratada %>% group_by(PatientID) %>% right_join(imgs_amostra) -> base_imgs_amostra
summary(base_imgs_amostra$`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`)

base_tratada %>% anti_join(base_arquivos_amostra, by = "PatientID") %>% group_by(PatientID) -> base_arquivos_fora_da_amostra
table(base_arquivos_fora_da_amostra$`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`)
summary(base_arquivos_fora_da_amostra)

paste("/home/luiz/yolov7/DICOM_VESICULA_YOLO/train/vesicula/X0000695874_4_43.txt")


path4 = "~/Desktop/USP/TCC/projeto_TCC/IMGs/resultado/"
list.files(path = path4) %>% as_tibble() -> array_imgs_resultados
list.files(path = path4) %>% as_tibble() %>% 
  separate(col = value, sep = "_", into = c("PatientID","SerieNumber","InstanceNumber")) -> imgs_resultados
imgs_resultados$InstanceNumber %>% str_sub(end=-5) -> imgs_resultados$InstanceNumber
imgs_resultados$PatientID %>% str_sub(start=2) -> imgs_resultados$PatientID
imgs_resultados$PatientID = as.numeric(imgs_resultados$PatientID)
imgs_resultados$SerieNumber = as.numeric(imgs_resultados$SerieNumber)
imgs_resultados$InstanceNumber = as.numeric(imgs_resultados$InstanceNumber)
  base_tratada %>% group_by(PatientID) %>% right_join(imgs_resultados) -> base_imgs_resultados
base_imgs_resultados %>% dplyr::select(-9) -> base_imgs_resultados
#write_csv(base_imgs_resultados, "Resultados.csv")


library(readxl)
Resultados_amostra_teste_paraTCC <- read_excel("Resultados_amostra_teste_paraTCC.xlsx")
Resultados_amostra_teste_paraTCC$Acuracia = as.numeric(Resultados_amostra_teste_paraTCC$Acuracia)
summary(Resultados_amostra_teste_paraTCC$Acuracia) 

library(fs) 
(dir_ls(path = "~/yolov7/EXEMPLO_AMOSTRA/train/", recurse = TRUE))
nrow(base_arquivos_amostra)

cat(nrow(filter(base_tratada,`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` %in% c("H","V"))), "imagens de", paste(base_tratada %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` %in% c("H","V")) %>% count(PatientID) %>% nrow()), "pacientes que TEM vesícula")
cat(nrow(filter(base_tratada,`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` %in% c("S","C"))), "imagens de", paste(base_tratada %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` %in% c("S","C")) %>% count(PatientID) %>% nrow()), "pacientes que NÃO TEM vesícula")
cat(nrow(filter(base_tratada,`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` %in% c("V"))), "Imagens Vesícula Normal")
cat(nrow(filter(base_tratada,`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` %in% c("H"))), "Imagens Vesícula Hipodistendida")
cat(nrow(filter(base_tratada,`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` %in% c("C"))), "Imagens Clipe Cirúrgico")
cat(nrow(filter(base_tratada,`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` %in% c("S"))), "Imagens sem Vesícula/Clipe")
cat("Total de",nrow(base_tratada),"Imagens DICOM")
c("Imagens Vesícula Normal", "Imagens Vesícula Hipodistendida", "Imagens Clipe Cirúrgico", "Imagens sem Vesícula/Clipe", "Total") %>% as_tibble() -> x
x$Imagens = as.numeric()
x$Imagens[1] <- nrow(filter(base_tratada,`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` %in% c("V")))
x$Imagens[2] <- nrow(filter(base_tratada,`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` %in% c("H")))
x$Imagens[3] <- nrow(filter(base_tratada,`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` %in% c("C")))
x$Imagens[4] <- nrow(filter(base_tratada,`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` %in% c("S")))
x$Imagens[5] <- nrow(base_tratada)
names(x) <- c("CATEGORIA DICOM", "Imagens") 
x
<<<<<<< HEAD



#####

base_tratada %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`%in% c("V", "H")) %>%  mutate(posicao_interesse = ifelse(c(InstanceNumber >= `Corte INICIAL vesícula` & 
                                      InstanceNumber <= `Corte FINAL vesícula`), 1,0)) %>% 
  ggplot(aes(InstanceNumber)) + stat_count() + facet_wrap(posicao_interesse~.)



base_NAs %>% filter(SerieNumber==2) %>% group_by(FilePath) %>% 
  mutate(imagePath = temp_dcm_export(FilePath, "--write-jpeg", make.names(paste(str_sub(FilePath, start = 35L, end = -5L), "png", sep = "."),unique = TRUE))) %>% ungroup()


=======
>>>>>>> cda627d3ce7d4a7920a971f59a26c7a783a8240e
