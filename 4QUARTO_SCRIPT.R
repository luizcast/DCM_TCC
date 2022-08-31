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


annot_clean <- read_excel("~/Desktop/USP/TCC/MATERIAL/ZIPS/notebooks/annot_clean.xlsx")
View(annot_clean)

annot_clean <- as_tibble(annot_clean)
glimpse(annot_clean)

annot_clean$`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` <- as.factor(annot_clean$`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`)

levels(annot_clean$`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`)

annot_clean %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` == "C")

table(annot_clean$`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`, annot_clean$`Densidade não habitual (sim=1, não=0)`)

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
names(base) <- c("FilePath", "Patient ID", "SeriesNumber","InstanceNumber")
as.numeric(base$SeriesNumber) -> base$SeriesNumber
as.numeric(base$InstanceNumber) -> base$InstanceNumber
as.numeric(base$PatientID) -> base$PatientID
base <- as_tibble(base)
}


prepare_dicom("~/Desktop/DATA-SET_TESTE/AAAAA/") -> um
um$`Patient ID` <- as.numeric(um$`Patient ID`)
glimpse(um)
names(annot_clean)
names(um)

#readDICOM(basinha_path$basinha_path[1]) -> listinha

#JOIN TABLES
left_join(um, annot_clean, by="Patient ID") -> joinzinho
tail(joinzinho) -> mini_joinzinho
glimpse(joinzinho)

#FUNCAO PARA CRIAR AS IMAGENS DA REDE CONVULACIONAL 
lfpc <- function(arg1, arg2) {
  dcmj2pnm(arg1, opt = arg2)
}

joinzinho %>% group_by(FilePath) %>% mutate(ImagePath = lfpc(FilePath, "--write-raw-pnm")) -> com_img

com_img %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`%in% c("C","S")) 


annot_clean %>% group_by(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`) %>% count() %>% as_tibble()-> tabela_contagem
tabela_contagem

annot_clean %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` %in% c("H","V")) -> tabela_vesicula
tabela_vesicula

#########

ggplot(annot_clean, aes(x=`Corte INICIAL vesícula`)) +
  geom_bar(width = 2)

ggplot(annot_clean, aes(x=`Corte FINAL vesícula`)) +
  geom_histogram()

annot_clean %>% filter(`Corte INICIAL vesícula` <= 70) %>% 
ggplot(., aes(x=`Corte INICIAL vesícula`)) +
  geom_bar(width = 0.8)



annot_clean$`Corte INICIAL vesícula` <-
annot_clean %>% group_by(`Corte INICIAL vesícula`) %>% count(arrange()) %>% ggplot(aes(x=`Corte INICIAL vesícula`,y=n)) + geom_point() 


patientDir <- sort(list.dirs(path = "~/Desktop/USP/TCC/MATERIAL/DATA-SET/", full.names = TRUE, recursive = FALSE))
patientDir 

basinha_path <- as.vector(patientDir)
glimpse(basinha_path)
basinha_path <- as.array(basinha_path)
as.data.frame(basinha_path) -> basinha_path
glimpse(basinha_path)




