### DATASET INFO #####
#TOTAL FILES: 165371
#TOTAL SIZE: 89.9GB
#FILE FORMAT: DICOM Medical Exams of Tomographic images and its Metadata in DICOM format
# FILE EXTENSION: ".dcm"

###PACKAGES####
func_init <-function(){pacotes <- c("keras", "tensorflow", "tidyverse", "oro.dicom","dcmtk", "readxl")
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

###PATH####
path = "/home/luiz/Desktop/DATA-SET_TESTE/"

#### 1.Transformar as imagem dos arquivos DICOM em JPEG (reestruturar dados de imagem para JPEG) #####
#no terminal: find ~/Desktop/USP/TCC/MATERIAL/DATA-SET/* -name '*.dcm' -exec dcmdjpeg {} {} \;
#toolkit "DCMTK" - https://dicom.offis.de/dcmtk.php.en
#O R não realiza leitura direta das imagens DICOM é necessário converte-las para JPEG(ainda(?))

#### 2.TRATANDO BANCO DE IMAGENS DICOM ####
#renomeando os arquivos com o sortDICOM
#sortDicom(path = path, labelFormat = "%i_%s_%r", forceStack = FALSE)

#### 3.ANONIMIZANDO A BASE #####
#for (i in full_tratada$FilePath){dcm_anon(i)}

#### Preparando Dataset annot_clean ####
#Esta base foi anotada pelos médicos da UNIFESP.
read_excel("~/Desktop/USP/TCC/MATERIAL/ZIPS/notebooks/annot_clean.xlsx") %>% as_tibble() -> annot_clean
tail(annot_clean)
glimpse(annot_clean)
gsub(pattern = "Não se aplica", replacement = "NA", annot_clean$`Densidade não habitual (sim=1, não=0)`) -> annot_clean$`Densidade não habitual (sim=1, não=0)`
gsub(pattern = "Não se aplica", replacement = "NA", annot_clean$`Corte INICIAL vesícula`) -> annot_clean$`Corte INICIAL vesícula`
gsub(pattern = "Não se aplica", replacement = "NA", annot_clean$`Corte FINAL vesícula`) -> annot_clean$`Corte FINAL vesícula`

annot_clean$`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` <- as.factor(annot_clean$`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`)
annot_clean$`Corte FINAL vesícula` <- as.numeric(annot_clean$`Corte FINAL vesícula`)
annot_clean$`Corte INICIAL vesícula` <- as.numeric(annot_clean$`Corte INICIAL vesícula`)
annot_clean$`Densidade não habitual (sim=1, não=0)` <- as.numeric(annot_clean$`Densidade não habitual (sim=1, não=0)`)

annot_clean[170:180,]
glimpse(annot_clean)

table(annot_clean$`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`)
annot_clean
###### (observacao) Uma funçao para enteragir com a arquivo DICOM carregado na memória #####
#desta forma é impossível trabalhar com grandes volumes de arquivos. Porém, segmentando
#a imagem em blocos menores do que 14Gb é possivel manipula-las.

prepare_dicom <- function(path_dicom) {
  
  oro.dicom::readDICOM(path_dicom, verbose = TRUE) -> ID
  ID_hdr <- ID$hdr
  
  map(ID_hdr, ~dplyr::filter(.x, name == "PatientID")) -> ID
  map(ID, ~dplyr::select(.x, "value")) -> ID
  rbindlist(ID, idcol = "FilePath") -> ID
  
  map(ID_hdr, ~dplyr::filter(.x, name == "SeriesNumber")) -> SN
  map(SN, ~dplyr::select(.x, "value")) -> SN
  rbindlist(SN, idcol = "FilePath") -> SN
  
  map(ID_hdr, ~dplyr::filter(.x, name == "InstanceNumber")) -> IN
  map(IN, ~dplyr::select(.x, "value")) -> IN
  rbindlist(IN, idcol = "FilePath") -> IN
  
  left_join(ID, SN, by="FilePath") %>% left_join(y=IN, by="FilePath") -> base
  names(base) <- c("FilePath", "PatientID", "SeriesNumber","InstanceNumber")
  as.numeric(base$SeriesNumber) -> base$SeriesNumber
  as.numeric(base$InstanceNumber) -> base$InstanceNumber
  as.numeric(base$PatientID) -> base$PatientID
  base <- as_tibble(base)
}
prepare_dicom("~/Desktop/DATA-SET_TESTE/AAAAA/") -> amostra
glimpse(amostra)


##### Criando Dataset que vai gerar as imagens #####
list.files(path) %>%  as_tibble() -> array_arquivos_dicom_tratados
separate(array_arquivos_dicom_tratados, col = value, sep = "_", into = c("PatientID","SerieNumber","InstanceNumber")) -> index_dicom
index_dicom$InstanceNumber %>% str_sub(end=-5) -> index_dicom$InstanceNumber
bind_cols(array_arquivos_dicom_tratados,index_dicom) -> index_dicom
  index_dicom
names(index_dicom) <- c("FileName","PatientID","SerieNumber","InstanceNumber")

          index_dicom$PatientID <- as.numeric(index_dicom$PatientID)
          index_dicom$SerieNumber = as.numeric(index_dicom$SerieNumber)
          index_dicom$InstanceNumber = as.numeric(index_dicom$InstanceNumber) 

#join com annot_clean
index_dicom %>% group_by(PatientID) %>% 
  left_join(annot_clean, by = c("PatientID" = "Patient ID")) %>% 
  ungroup() -> index_dicom
glimpse(index_dicom)


#### GERANDO TABELAS #####
index_dicom %>% dplyr::select(1:9) -> base_tratada
base_tratada %>% dplyr::select(-5) -> base_tratada
base_tratada$FilePath <- paste(path, base_tratada$FileName, sep='')
glimpse(base_tratada)
#write_csv(full, "full.csv")
#write_csv(full_tratada, "full_tratada.csv")

#TABELAS FREQUENCIA
table(annot_clean$`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`)

base_tratada %>% group_by(PatientID) %>% count(SerieNumber) %>% ungroup(PatientID)-> frequencia_por_serie
frequencia_por_serie

#TABELA SEM VESICULA COM CLIPE : full_clipe
base_clipe <- base_tratada %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` == "C")
glimpse(base_clipe)
#TABELA SEM VESICULA SEM CLIPE : full_sem_clipe
base_sem_clipe <- base_tratada %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` == "S")
glimpse(base_sem_clipe)
#TABELA COM VESICULA HIPOESTENDIDA : full_vesicula_hipodistendida
base_vesicula_hipodistendida <- base_tratada %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` == "H")
glimpse(base_vesicula_hipodistendida)
#TABELA COM VESICULA NORMAL : full_vesicula_normal
base_vesicula_normal <- base_tratada %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` == "V")
base_vesicula_normal
#TABELA COM AS IMAGENS DA VESICULA : frames_vesicula_normal
base_vesicula_normal %>% mutate(posicao_interesse = ifelse(c(InstanceNumber >= `Corte INICIAL vesícula` & 
                                                               InstanceNumber <= `Corte FINAL vesícula`), 1, 2)) -> base_frames_vesicula_normal
base_frames_vesicula_normal %>% filter(posicao_interesse == 1) -> base_frames_vesicula_normal
glimpse(base_frames_vesicula_normal)
#write_csv(frames_vesicula_normal, "frames_vesicula.csv")

#QUAL SERA A SERIE? CORRESPONDENTE A IMAGEM ANOTADA?#
base_vesicula_normal %>% group_by(PatientID) %>% count(SerieNumber) %>% ungroup() -> tabela_n_por_serie
tabela_n_por_serie
tabela_n_por_serie %>% count(PatientID)

#RETIRANDO DA TABELA A "IMAGENS NO MEIO" DE CASA SERIE
base_frames_vesicula_normal %>% unite("ID_SERIE", PatientID:SerieNumber) -> ID_SERIE
base_frames_vesicula_normal[!duplicated(ID_SERIE$ID_SERIE),] -> NanaZinha
NanaZinha$MEDIA <- c(NanaZinha$`Corte INICIAL vesícula`+NanaZinha$`Corte FINAL vesícula`)/2
NanaZinha$MEDIA <- as.integer(NanaZinha$MEDIA)
NanaZinha %>% unite("PatientID","PatientID","SerieNumber","MEDIA", sep = "_") %>% select(PatientID) -> gerador
NanaZinha %>% select(PatientID,SerieNumber,MEDIA) -> PatienIDSerieNumberMEDIA
bind_cols(gerador, PatienIDSerieNumberMEDIA) -> geradorPatienIDSerieNumberMEDIA
geradorPatienIDSerieNumberMEDIA$FINAL <- paste("0000",geradorPatienIDSerieNumberMEDIA$PatientID...1,".dcm", sep = '')
str_length(geradorPatienIDSerieNumberMEDIA$FINAL)
geradorPatienIDSerieNumberMEDIA %>% mutate(FINAL = ifelse(str_length(FINAL) >= 20, str_sub(FINAL,start=2L), FINAL)) -> geradorPatienIDSerieNumberMEDIA
geradorPatienIDSerieNumberMEDIA %>% left_join(base_frames_vesicula_normal, by=c("FINAL"="FileName")) %>% select(FilePath) -> bomba
str_length(geradorPatienIDSerieNumberMEDIA$FINAL)
geradorPatienIDSerieNumberMEDIA


base_tratada %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` %in% c('H','V')) %>% select(1:6) %>% 
  filter(str_length(FileName) <= 21)-> tabela_GLM
tabela_GLM %>% left_join(tabela_n_por_serie) -> tabela_GLM
tabela_GLM %>% mutate(posicao_interesse = ifelse(c(InstanceNumber >= `Corte INICIAL vesícula` & 
                                      InstanceNumber <= `Corte FINAL vesícula`), 1,0)) %>% select(-5,-6) -> tabela_GLM


sum(is.na(tabela_GLM$n))
tabela_GLM %>% filter(is.na(n)) -> naSolving
tabela_GLM %>% filter(!is.na(n)) -> Solving

naSolving %>% group_by(PatientID) %>% count(SerieNumber) %>% ungroup() -> table_NAS
names(table_NAS) <- c('PatientID','SerieNumber', 'total')
glimpse(table_NAS)
naSolving %>% left_join(table_NAS) %>% select(-5) -> NA_REMOVED
names(NA_REMOVED) <- c('FileName','PatientID','SerieNumber','InstanceNumber','posicao_interesse','n')
bind_rows(Solving,NA_REMOVED) -> tabela_GLM

#modelo
glm(posicao_interesse~InstanceNumber+n+SerieNumber, family = binomial, data = tabela_GLM) -> modelo_glm
summary(modelo_glm)
step(modelo_glm) -> modelo_fit
summary(modelo_fit)
tabela_GLM$fit <- modelo_fit$fitted.values
tabela_GLM %>% filter(tabela_GLM$posicao_interesse == 1) %>% summary()
tabela_GLM$PatientID <- as.factor(tabela_GLM$PatientID)


#aplicando a tabela CLIPE

summary(modelo_fit)
predict(object = modelo_fit, 
        data.frame(base_clipe), 
        type = "response") -> base_clipe$fit
tabela_GLM$fit %>% summary()
ggplot(tabela_GLM, aes(fit)) + geom_histogram(bins = 10)
base_clipe$fit %>% summary()
ggplot(base_clipe, aes(fit)) + geom_histogram(bins = 20)
base_clipe %>% filter(fit >= 0.025 & fit <= 0.21)
#achando a média
c(as.integer(mean(base_vesicula_normal$`Corte INICIAL vesícula`)))-3 -> ENTRADA
c(as.integer(mean(base_frames_vesicula_normal$`Corte FINAL vesícula`)))+4 -> SAIDA
base_clipe %>% filter(fit >= 0.025 & fit <= 0.21) %>% filter(InstanceNumber >= ENTRADA & InstanceNumber <= SAIDA) -> clipe_select


#### GERANDO BANCO DE IMAGENS #####
#funcaozinha para indexar a img temporária na tabela
temp_dcm_export <- function(arg1, arg2, arg3){dcmj2pnm(arg1, opt = arg2, outfile = arg3, "--use-window 1")}


#PRONTO! SÓ GERAR!

#gerando para saber qual serie
bomba %>% group_by(FilePath) %>% 
    mutate(imagePath = temp_dcm_export(FilePath, "--write-jpeg", make.names(paste(str_sub(FilePath, start = 35L, end = -5L), "png", sep = "."),unique = TRUE))) %>% ungroup() -> bomba_com_img
clipe_select %>% group_by(FilePath) %>% 
  mutate(imagePath = temp_dcm_export(FilePath, "write-jpeg", make.names(paste(str_sub(FilePath, start = 35L, end = -5L), "png", sep = "."),unique = TRUE))) %>% ungroup() -> clipe_select_outs

list.files("~/Desktop/IMAGENS_KERAS/VESICULA/") %>%  as_tibble() -> vesic
separate(vesic, col = value, sep = "_", into = c("FileName","SerieNumber","InstanceNumber")) -> vesic
vesic$InstanceNumber %>% str_sub(end=-5) -> vesic$InstanceNumber

vesic 

bomba %>% mutate(id = str_sub(FilePath, start = 35L, end = -5L)) %>% 
  separate(col = id, sep = "_", into = c("FileName","SerieNumber","InstanceNumber")) %>% right_join(vesic) -> tabela_imgs_vesicula
tabela_imgs_vesicula


imgs %>% group_by(FilePath) %>% 
  mutate(imagePath = temp_dcm_export(FilePath, "--write-jpeg", make.names(paste(str_sub(FilePath, start = 42L, end = -5L), "png", sep = "."),unique = TRUE))) -> imgs_com_path


base_vesicula_normal %>% filter(PatientID == 3870806 & SerieNumber == 2) %>% filter(InstanceNumber >= 50 & InstanceNumber <= 75) %>% group_by(FilePath) %>% 
  mutate(imagePath = temp_dcm_export(FilePath, "--write-jpeg", make.names(paste(str_sub(FilePath, start = 35L, end = -5L), "png", sep = "."),unique = TRUE))) %>% ungroup() -> prova_imgs_out
