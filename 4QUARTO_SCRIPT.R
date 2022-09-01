pacotes <- c("oro.dicom", "tidyverse", "plotly", "readr","imager", "raster", 
             "radtools", "divest", "data.table", "kableExtra", "lmtest", "caret",
             "pROC", "dcmtk", "readr", "readxl")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}



### Preparando Base annot_clean

annot_clean <- read_excel("~/Desktop/USP/TCC/MATERIAL/ZIPS/notebooks/annot_clean.xlsx")
View(annot_clean)

annot_clean <- as_tibble(annot_clean)
glimpse(annot_clean)

annot_clean$`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` <- as.factor(annot_clean$`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`)


gsub(pattern = "Não se aplica", replacement = "NA", annot_clean$`Densidade não habitual (sim=1, não=0)`) -> annot_clean$`Densidade não habitual (sim=1, não=0)`
gsub(pattern = "Não se aplica", replacement = "NA", annot_clean$`Corte INICIAL vesícula`) -> annot_clean$`Corte INICIAL vesícula`
gsub(pattern = "Não se aplica", replacement = "NA", annot_clean$`Corte FINAL vesícula`) -> annot_clean$`Corte FINAL vesícula`

annot_clean$`Corte FINAL vesícula` <- as.numeric(annot_clean$`Corte FINAL vesícula`)
annot_clean$`Corte INICIAL vesícula` <- as.numeric(annot_clean$`Corte INICIAL vesícula`)
annot_clean$`Densidade não habitual (sim=1, não=0)` <- as.numeric(annot_clean$`Densidade não habitual (sim=1, não=0)`)

glimpse(annot_clean)


#######################

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

glimpse(annot_clean)

names(annot_clean)
names(um)

IOP <- extractHeader(umz$hdr, "PatientID", numeric=TRUE)


#readDICOM(basinha_path$basinha_path[1]) -> listinha

#JOIN TABLES

left_join(um, annot_clean, by=c("PatientID" ="Patient ID")) -> joinzinho
tail(joinzinho) -> mini_joinzinho
glimpse(joinzinho)

##### EDA #####

levels(annot_clean$`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`)

annot_clean %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` %in% c("C","S")) -> base_sem_vesicula
annot_clean %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` %in% c("H","V")) -> base_com_vesicula


base_com_vesicula %>% dplyr::select(2:6) -> 

joinzinho %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`== "V") 


#FUNCAO PARA CRIAR AS IMAGENS DA REDE CONVULACIONAL

lfpc <- function(arg1, arg2) {
  dcmj2pnm(arg1, opt = arg2, outfile = )
}

joinzinho %>% group_by(FilePath) %>% mutate(ImagePath = lfpc(FilePath, "--write-raw-pnm")) -> com_img

com_img %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`%in% c("C","S")) 


annot_clean %>% group_by(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`) %>% count() %>% as_tibble()-> tabela_contagem
tabela_contagem

annot_clean %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` %in% c("H","V")) -> tabela_vesicula
tabela_vesicula

#########

ggplot(annot_clean, aes(x=`Corte INICIAL vesícula`)) +
  geom_bar(width = 1 )

ggplot(annot_clean, aes(x=`Corte FINAL vesícula`)) +
  geom_histogram()

annot_clean %>% filter(`Corte INICIAL vesícula` <= 70) %>% 
ggplot(., aes(x=`Corte INICIAL vesícula`)) +
  geom_bar(width = 0.8)



annot_clean$`Corte INICIAL vesícula` <-
annot_clean %>% group_by(`Corte INICIAL vesícula`) %>% count(arrange()) %>% ggplot(aes(x=`Corte INICIAL vesícula`,y=n)) + geom_point() 



patientDir <- as_tibble(list.dirs(path = "~/Desktop/DATA-SET_TESTE/", full.names = TRUE, recursive = FALSE ))
glimpse(basinha_path)


base


library(XML)
xmlToDataFrame('~/Desktop/USP/TCC/DATA-SET-DR-IGOR/1.2.840.113704.1.111.10024.1500721597.1/studyXml.xml') -> aa

str(bb)


for (dcm in length(amostra$FilePath)) {
  prepare_dicom(dcm)
}

sortDicom("~/Desktop/AAAAA/1" )

ttt <- c("/home/luiz/Desktop/AAAAA/")
as.data.frame(ttt) -> va
sortDicom(path =va[1,], depth = 1000, labelFormat = "%i_%s_%r", forceStack = FALSE)

prepare_dicom('~/Desktop/AAAAA/1/') -> a
a

basinha_path %>% 
  mutate(org = print(divest::sortdicom(path = basinha_path$value,depth = 1000, labelFormat = "%i_%s_%r", forceStack = FALSE))) -> basics
