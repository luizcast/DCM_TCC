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
gsub(pattern = "Não se aplica", replacement = "NA", annot_clean$`Densidade não habitual (sim=1, não=0)`) -> annot_clean$`Densidade não habitual (sim=1, não=0)`
gsub(pattern = "Não se aplica", replacement = "NA", annot_clean$`Corte INICIAL vesícula`) -> annot_clean$`Corte INICIAL vesícula`
gsub(pattern = "Não se aplica", replacement = "NA", annot_clean$`Corte FINAL vesícula`) -> annot_clean$`Corte FINAL vesícula`

annot_clean$`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` <- as.factor(annot_clean$`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`)
annot_clean$`Corte FINAL vesícula` <- as.numeric(annot_clean$`Corte FINAL vesícula`)
annot_clean$`Corte INICIAL vesícula` <- as.numeric(annot_clean$`Corte INICIAL vesícula`)
annot_clean$`Densidade não habitual (sim=1, não=0)` <- as.numeric(annot_clean$`Densidade não habitual (sim=1, não=0)`)
glimpse(annot_clean)

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
list.files(path) %>%  as_tibble() -> tiblinha
separate(tiblinha, col = value, sep = "_", into = c("PatientID","SerieNumber","InstanceNumber")) -> new
new$InstanceNumber %>% str_sub(end=-5) -> new$InstanceNumber
bind_cols(tiblinha,new) -> nw
  nw
names(nw) <- c("FileName","PatientID","SerieNumber","InstanceNumber")

          nw$PatientID <- as.numeric(nw$PatientID)
          nw$SerieNumber = as.numeric(nw$SerieNumber)
          nw$InstanceNumber = as.numeric(nw$InstanceNumber) 

#join com annot_clean
nw %>% group_by(PatientID) %>% 
  left_join(annot_clean, by = c("PatientID" = "Patient ID")) %>% 
  ungroup() -> full


#### GERANDO TABELAS #####
full %>% dplyr::select(1:9) -> full_tratada
full_tratada %>% dplyr::select(-5) -> full_tratada
full_tratada$FilePath <- paste(file.path(path), full_tratada$FileName, sep='')
glimpse(full_tratada)
#write_csv(full, "full.csv")
#write_csv(full_tratada, "full_tratada.csv")

#TABELAS FREQUENCIA
full_tratada %>% count(c("PatientID","SerieNumber"))
full_tratada %>% group_by(PatientID) %>% count(SerieNumber) %>% ungroup(PatientID)-> frequencia_por_serie
frequencia_por_serie
table(frequencia_por_serie$n)
#TABELA SEM VESICULA COM CLIPE : full_clipe
full_clipe <- full_tratada %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` == "C")
glimpse(full_clipe)
#TABELA SEM VESICULA SEM CLIPE : full_sem_clipe
full_sem_clipe <- full_tratada %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` == "S")
glimpse(full_sem_clipe)
#TABELA COM VESICULA HIPOESTENDIDA : full_vesicula_hipodistendida
full_vesicula_hipodistendida <- full %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` == "H")
glimpse(full_hipodistendida)
#TABELA COM VESICULA NORMAL : full_vesicula_normal
full_vesicula_normal <- full %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` == "V")
full_vesicula_normal
#TABELA COM AS IMAGENS DA VESICULA : frames_vesicula_normal
full_vesicula_normal %>% mutate(posicao_interesse = ifelse(c(InstanceNumber >= `Corte INICIAL vesícula` & 
                                                               InstanceNumber <= `Corte FINAL vesícula`), 1, 2)) -> full_vesicula_normal
full_vesicula_normal %>% filter(posicao_interesse == 1) -> frames_vesicula_normal
glimpse(frames_vesicula_normal)
#write_csv(frames_vesicula_normal, "frames_vesicula.csv")

#QUAL SERA A SERIE?
frames_vesicula_normal %>% group_by(PatientID) %>% count(SerieNumber) %>% ungroup() -> tabela_para_descobrir_serie
tabela_para_descobrir_serie %>% count(PatientID)
tabela_para_descobrir_serie

#RETIRANDO DA TABELA A "IMAGENS NO MEIO" DE CASA SERIE
frames_vesicula_normal %>% unite("ID_SERIE", PatientID:SerieNumber) -> ID_SERIE
frames_vesicula_normal[!duplicated(ID_SERIE$ID_SERIE),] -> NanaZinha
NanaZinha$MEDIA <- c(NanaZinha$`Corte INICIAL vesícula`+NanaZinha$`Corte FINAL vesícula`)/2
NanaZinha$MEDIA <- as.integer(NanaZinha$MEDIA)
NanaZinha %>% unite("PatientID","PatientID","SerieNumber","MEDIA", sep = "_") %>% select(PatientID) -> gerador
NanaZinha %>% select(PatientID,SerieNumber,MEDIA) -> PatienIDSerieNumberMEDIA
bind_cols(gerador, PatienIDSerieNumberMEDIA) -> geradorPatienIDSerieNumberMEDIA
geradorPatienIDSerieNumberMEDIA$FINAL <- paste("0000",geradorPatienIDSerieNumberMEDIA$PatientID...1,".dcm", sep = '')
str_length(geradorPatienIDSerieNumberMEDIA$FINAL)
geradorPatienIDSerieNumberMEDIA %>% mutate(FINAL = ifelse(str_length(FINAL) >= 20, str_sub(FINAL,start=2L), FINAL)) -> geradorPatienIDSerieNumberMEDIA
geradorPatienIDSerieNumberMEDIA %>% left_join(frames_vesicula_normal, by=c("FINAL"="FileName")) %>% select(FilePath) -> bomba
str_length(geradorPatienIDSerieNumberMEDIA$FINAL)
geradorPatienIDSerieNumberMEDIA



NanaZinha %>% select(-FileName, -InstanceNumber) %>% relocate(MEDIA,.after = SerieNumber) -> para_join
names(para_join) -> nomes
names(para_join$SerieNumber)
NanaZinha %>% left_join(frames_vesicula_normal)


#### GERANDO BANCO DE IMAGENS #####
#funcaozinha para indexar a img temporária
temp_dcm_export <- function(arg1, arg2){dcmj2pnm(arg1, opt = arg2)}

#PRONTO! SÓ GERAR!
head(frames_vesicula_normal) %>% group_by(FilePath) %>% mutate(ImagePath = temp_dcm_export(FilePath, "--write-jpeg")) %>% ungroup()
#gerando para saber qual serie
bomba %>% group_by(FilePath) %>% mutate(imagePath = temp_dcm_export(FilePath, "--write-jpeg")) -> bomba_com_img



table(annot_clean$`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`)



