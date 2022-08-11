
### DATASET INFO #####
#TOTAL FILES: 165371
#TOTAL SIZE: 89.9GB


##### PACKAGES #####
#"RNifti"

pacotes <- c("oro.dicom", "tidyverse", "purrr","imager", "raster", "radtools", 
             "divest", "RNifti", "data.table", "knitr")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
 

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


#### ANONIMIZANDO A BASE #####
#no terminal: dcmodify -v -ea "(0010,0010)" *.dcm


##### Import DICOM File #####
dicom_teste_1_meta <- readDICOMFile("~/Desktop/DATA-SET_TESTE/AAAAA/T20140208000021_NDEBORA_LOURENCO_DE_LIMA_S5/1.2.840.113704.1.111.5464.1391825094.34544.dcm")
dicom_teste_1_meta
class(dicom_teste_1_meta)
dicom_teste_1_meta[["hdr"]]
dicom_teste_1_meta[["img"]]
str(dicom_teste_1_meta)


##### Tratando o DATA_FRAME do Arquivo Dicom #####
dicom_teste_1_database <- dicom_teste_1_meta$hdr
class(dicom_teste_1_database)
names(dicom_teste_1_database)
dicom_teste_1_database <- transform(dicom_teste_1_database,
                        group = as.factor(group),
                        element = as.numeric(element),
                        length = as.numeric(length)) %>%  as_tibble()
dicom_teste_1_database
str(dicom_teste_1_database)
view(dicom_teste_1_database)

#Anonimizando a BASE
dicom_teste_1_database[50,6]

dicom_teste_1_database[50, 6] <-  gsub(x = dicom_teste_1_database[50, 6],
                      pattern = "",
                      replacement = NA)

dicom_teste_1_database[50,6]


##### Tratando a IMAGEM do Arquivo DICOM #####
dicom_teste_1_img <- dicom_teste_1_meta$img
print(dicom_teste_1_img)
image(dicom_teste_1_img, col = grey(0:64/64))

image(t(dicom_teste_1_meta[[2]]), col = grey(0:64/64), axes = FALSE,
      xlab = "", ylab = "")

#Raster
rasterjpeg <- raster(dicom_teste_1_img)
plot(rasterjpeg, col = grey(0:64/64), axes = TRUE, legend = TRUE, asp=1 )
class(rasterjpeg)
raster::image(rasterjpeg, col = grey(0:64/64), asp=1, axes = FALSE, legend = FALSE, xlab = NA, ylab = NA)
rasterjpeg

###### Vãrios Arquivos em uma list() ########
dicom_patient1 <- readDICOM("data-files/patient1_jpeg_transformed_img/", verbose = TRUE)
dicom_patient1_hdr <- dicom_patient1$hdr
dicom_patient1_img <- dicom_patient1$img
str(dicom_patient1_img)

###### extractHeader em Ação ######
extractHeader(dicom_patient1$hdr, "PatientsName", numeric = FALSE)


###### COMPARANDO DATATABLES #######
dicom_patient1_hdr_1 <- dicom_patient1$hdr$`data-files/patient1_jpeg_transformed_img//1.2.840.113704.1.111.28952.1503356202.44143.dcm`
dicom_patient1_hdr_2 <- dicom_patient1$hdr$`data-files/patient1_jpeg_transformed_img//1.2.840.113704.1.111.28952.1503356285.44256.dcm`
dicom_patient1_hdr_X <- dicom_patient1$hdr$`data-files/patient1_jpeg_transformed_img//1.2.840.113704.1.111.28952.1503356293.44319.dcm`
dicom_patient1_hdr_2 %>% anti_join(dicom_patient1_hdr_X)

####### Observando Headers dos arquivos ######
hdr_paciente <- dicom_patient1$hdr$`data-files/patient1_jpeg_transformed_img//1.2.840.113704.1.111.28952.1503356202.44143.dcm`
hdr_paciente <- transform(hdr_paciente,
                          group = as.factor(group),
                          element = as.numeric(element),
                          length = as.numeric(length)) %>%  as_tibble()
hdr_paciente
str(hdr_paciente) 
view(hdr_paciente)

###### Filtrando para entender relação entre as posições das fotos ######################################### 
dicom_patient1_hdr$`data-files/patient1_jpeg_transformed_img//1.2.840.113704.1.111.28952.1503356202.44143.dcm` %>% 
      filter( grepl( "Name|Number", name ) ) 

dicom_patient1_hdr$`data-files/patient1_jpeg_transformed_img//1.2.840.113704.1.111.28952.1503356285.44256.dcm` %>% 
  filter( grepl( "SeriesNumber|InstanceNumber|SliceLocation", name ) )

dicom_patient1_hdr$`data-files/patient1_jpeg_transformed_img//1.2.840.113704.1.111.28952.1503356285.44257.dcm` %>% 
  filter( grepl( "SeriesNumber|InstanceNumber|SliceLocation", name ) )

dicom_patient1_hdr$`data-files/patient1_jpeg_transformed_img//1.2.840.113704.1.111.28952.1503356285.44258.dcm` %>% 
  filter( grepl( "SeriesNumber|InstanceNumber|SliceLocation", name ) )

dicom_patient1_hdr$`data-files/patient1_jpeg_transformed_img//1.2.840.113704.1.111.28952.1503356285.44259.dcm` %>% 
  filter( grepl( "SeriesNumber|InstanceNumber|SliceLocation", name ) )

dicom_patient1_hdr$`data-files/patient1_jpeg_transformed_img//1.2.840.113704.1.111.28952.1503356343.44821.dcm` %>% 
  filter( grepl( "SeriesNumber|InstanceNumber|SliceLocation", name ) ) 

dicom_patient1_hdr$`data-files/patient1_jpeg_transformed_img//1.2.840.113704.7.1.1.38264.1503356428.583.dcm` %>% 
  filter( grepl("SeriesNumber|InstanceNumber|SliceLocation", name))

dicom_patient1_hdr$`data-files/patient1_jpeg_transformed_img//1.2.840.113704.7.1.1.1600.1503356243.1.dcm` %>% 
  filter( grepl("SeriesNumber|InstanceNumber|SliceLocation", name))

dicom_patient1_hdr$`data-files/patient1_jpeg_transformed_img//1.2.840.113704.1.111.28952.1503356343.44821.dcm` %>% 
  filter( grepl( "SeriesNumber|InstanceNumber|SliceLocation", name))



###### Criando Dataframe com numero de arquivos por pasta #####
img_dir_list %>% 
  map(length) %>% as.data.frame() %>% t() %>% as.data.frame -> imgs_por_pacientes
rename(imgs_por_pacientes, ARQUIVOS_POR_PASTA = V1) -> imgs_finais
imgs_finais
str(imgs_finais)


##### Mudando o Nome do Arquivo na pasta pela Série e Instância #####
map(dicom_patient1_hdr, ~filter(.x, grepl("SeriesNumber|InstanceNumber", name))) -> numbers
class(numbers)
numbers$`data-files/patient1_jpeg_transformed_img//1.2.840.113704.1.111.28952.1503356202.44143.dcm`
view(numbers)
str(numbers)
head(numbers, 20)
map(numbers, ~dplyr::select(.x, "value")) -> numbers_filtrados
view(numbers_filtrados) %>% base::t() %>% as.data.frame() -> numbers_filtrados_transposta
numbers_filtrados_transposta[,1:2] -> nomes.finais 
class(nomes.finais)
nomes.finais <- rename(nomes.finais, SerieNumber = V1,
                       InstanceNumber = V2)
names(nomes.finais)
nomes.finais
nomes.finais$SerieNumber %>% t() -> SeriesNumber
nomes.finais$InstanceNumber %>% t() -> InstanceNumber

names(dicom_patient1_img) <- print(paste(SeriesNumber, InstanceNumber, sep = "_")) 
names(dicom_patient1_img)
tail(dicom_patient1_img, 15)

##### Join Data.Table #####

  oro.dicom::readDICOM("~/Desktop/DATA-SET_TESTE/T20140208000021_NDEBORA_LOURENCO_DE_LIMA_S7/", verbose = TRUE) -> ID
  ID_hdr <- ID$hdr
  str(ID_hdr)
  
  map(ID_hdr, ~dplyr::filter(.x, grepl("SeriesNumber|InstanceNumber|SliceLocation", name))) -> ID_CORE
  
  
  
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
  k1[SliceLocation, on = .(name)] -> final
  names(final)
  nomes_tables <- c("FilePath", "Serie", "Instance", "Slice")
  names(final) <- nomes_tables
  
  final <- as_tibble(final)
  final$Serie <- as.numeric(final$Serie)
  final$Instance <- as.numeric(final$Instance)
  final$Slice <- as.numeric((final$Slice))
  final
#final %>% write_csv(file = "PacienteX.csv")


  
#oro.dicom::readDICOM(final$Paciente, verbose = TRUE) -> dicom
#dicom[[220]] %>% RNifti::asNifti()

##### #####
