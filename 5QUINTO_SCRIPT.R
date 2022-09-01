#### CRIANDO BANCO DE IMAGENS PNM ####

path = "/home/luiz/Desktop/DATA-SET_TESTE/"
#sortDicom(path = path, labelFormat = "%i_%s_%r", forceStack = FALSE)



#### Preparando Base annot_clean ####

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

##### PREPARANDO BASINHA QUE VAI GERAR AS IMAGENS : BASE_nw #####

list.files(path) %>%  as_tibble() -> tiblinha
separate_rows(tiblinha, value, sep = "_") -> new
separate(tiblinha, col = value, sep = "_", into = c("I","L","C")) -> new
new$C %>% str_sub(end=-5) -> new$C
bind_cols(tiblinha,new) -> nw
  nw
names(nw) <- c("arq_name","PatientID","SerieNumber","InstanceNumber")

          nw$PatientID <- as.numeric(nw$PatientID)
          nw$SerieNumber = as.numeric(nw$SerieNumber)
          nw$InstanceNumber = as.numeric(nw$InstanceNumber) 

nw %>% group_by(PatientID) %>% 
  left_join(annot_clean, by = c("PatientID" = "Patient ID")) %>% 
  ungroup() -> full

full %>% dplyr::select(1:9) -> full_tratada
full_tratada %>% dplyr::select(-5) -> full_tratada
glimpse(full_tratada)

full_tratada %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` == "C") -> ful_clipe

levels(annot_clean$`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)`)

annot_clean %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` %in% c("C","S")) -> base_sem_vesicula
annot_clean %>% filter(`Hipodistendida/Vesícula normal/Clipe/sem clipe (HVSC)` %in% c("H","V")) -> base_com_vesicula
