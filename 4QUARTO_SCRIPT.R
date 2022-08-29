library(readxl)
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



#######################


luiz <- function(path_dicom) {
  
oro.dicom::readDICOM(path_dicom, verbose = TRUE) -> ID
ID_hdr <- ID$hdr
map(ID_hdr, ~dplyr::filter(.x, name == "PatientID")) -> ID_MAIOR
map(ID_MAIOR, ~dplyr::select(.x, "value")) -> ID_MAIOR
rbindlist(ID_MAIOR, idcol = "SystemFilePath") -> ID_MAIOR

}

luiz("~/Desktop/DATA-SET_TESTE/T20150818162802_NWILSON_ROBERTO_PEREIRA_S2/") -> teste
