readDICOM()

dir("~/Desktop/USP/TCC/MATERIAL/DATA-SET/1.2.840.113704.1.111.10024.1500721597.1/") -> files_list_paciente1
files_list_paciente1
lapply(files_list_paciente1, readDICOMFile())

readDICOMFile
  
file.exists(files_list_paciente1)

readDICOMfiletered(i) {
  dicomRAW <- (readDICOMFile(files_list_paciente1[[1]]))
  return(dicom)
}


  print(files_list_paciente1)
  
##########################################################

img_dir_list_tibble <- as_tibble(files_list_paciente1)
  
readDICOM(path = img_dir_list_tibble[1,1])

img_dir_tibble <- as_vector(img_dir_list_tibble)
img_dir_tibble
str(img_dir_tibble)


readDICOMFile()

as.list(patient1) -> list_patient1$value
as_tibble(img_dir_list[["Paciente_1"]]) -> patient1

lapply(img_dir_list[["Paciente_1"]],readDICOMFile()) -> dicom
print(patient1)



#######################################################

fil <- tempfile()
  ## Write an ASCII version of the 'base' function mean() to our temp file, ..
dput(base::mean, fil)
## ... read it back into 'bar' and confirm it is the same
bar <- dget(fil)
stopifnot(all.equal(bar, base::mean, check.environment = FALSE))

## Create a function with comments
baz <- function(x) {
  # Subtract from one
  1-x
}
## and display it
dput(baz)
## and now display the saved source
dput(baz, control = "useSource")

## Numeric values:
xx <- pi^(1:3)
dput(xx)
dput(xx, control = "digits17")
dput(xx, control = "hexNumeric")
dput(xx, fil); dget(fil) - xx # slight rounding on all platforms
dput(xx, fil, control = "digits17")
dget(fil) - xx # slight rounding on some platforms
dput(xx, fil, control = "hexNumeric"); dget(fil) - xx
unlink(fil)

xn <- setNames(xx, paste0("pi^",1:3))
dput(xn) # nicer, now "niceNames" being part of default 'control'
dput(xn, control = "S_compat") # no names
## explicitly asking for output as in R < 3.5.0:
dput(xn, control = c("keepNA", "keepInteger", "showAttributes"))

#########################


names(numbers, paste0("  ",1:3))

########################
asd <- readDICOM("~/Desktop/USP/TCC/MATERIAL/DATA-SET/1.2.840.113704.1.111.12136.1500207016.1/",
                 verbose = TRUE)

#########################
readDICOMFile(apllying)
apllying <- img_dir_list$Paciente_1
map() 

extractHeader(dicom_patient1_hdr, "SeriesNumber") -> headerzinho

header2matrix(dicom_patient1_hdr, "SeriesNUmber")
img_dir_list %>% t() %>% as.data.frame() -> transposta

transposta$Paciente_27 %>% as.data.frame() -> transposta_pacienteX
map(transposta_pacienteX, readDICOMFile(transposta_pacienteX$Paciente_27)) -> aaa
map(img_dir_list, ~filter(.x, grepl("SeriesNumber|InstanceNumber", name))) -> numbers
unlist(img_dir_list$Paciente_3) -> vetorzeira
class(vetorzeira)
readDICOM(path = img_dir_list$Paciente_5)
readDICOMFile(path.expand(vetorzeira))
e

class(img_dir_list$paciente_2[1])
readDICOM("~/Desktop/USP/TCC/MATERIAL/DATA-SET/1.2.840.113704.1.111.10184.1496621024.1/", recursive = TRUE) -> testinho

path = img_dir_list$paciente_2[1]
readDICOM(img_dir_list$paciente_2, recursive = TRUE)
file.exists(path)
file.exists(path = "~/Desktop/USP/TCC/MATERIAL/DATA-SET/1.2.840.113704.1.111.10064.1475365678.1/1.2.840.113704.1.111.1892.1475365860.111494.dcm")
read

readDICOMFile("~/Downloads/1.2.840.113704.1.111.3216.1302961431.63767.dcm") -> testinho
testinho$hdr -> hdr_testinho
hdr_testinho <- as.data.frame(hdr_testinho)
extractHeader(testinho$hdr, c("SeriesNumber", "InstanceNumber"))
testinho$img %>%  raster() -> img_testinho
image(img_testinho)
plot(img_testinho, col = grey(0:64/64), axes = TRUE, legend = TRUE, asp=1 )

library(raster)

dicom_patient1$img$`data-files/patient1_jpeg_transformed_img//1.2.840.113704.1.111.28952.1503356285.44260.dcm` -> raster1
dicom_patient1$img$`data-files/patient1_jpeg_transformed_img//1.2.840.113704.1.111.28952.1503356285.44261.dcm` -> raster2
dicom_patient1$img$`data-files/patient1_jpeg_transformed_img//1.2.840.113704.1.111.28952.1503356286.44262.dcm` -> raster3
raster1 <- raster(raster1)
raster2 <- raster(raster2)
raster3 <- raster(raster3)

rm <- raster::merge(raster1, raster2, raster3)
rm1 <- raster::merge(raster1, raster2)

plot(rm1)
plot(rm$layer)
rm$layer
rm$layer
rm1


readDICOM(img_dir_list[1], recursive = TRUE)
img_dir_list[1] %>% t() -> imgSS
as_vector(img_dir_list[1]) -> imgSS
str(imgSS)
readDICOM(imgSS)
print(img_dir_list[[1]][[2]]) %>% unlist()
img_dir_list[1] %>% unlist() -> listinha
as_vector(listinha) -> listinha
str(listinha)
as.vector(img_dir_list[1]) %>% unlist()-> listinha
print(listinha)
install.packages("EBImages")

class(printinho)
readDICOMFile("printinho")



if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("EBImage")
library("EBImage")
library(BiocManager)
install_github("https://git.bioconductor.org/packages/EBImage")
install("fftw3")

  browseVignettes("EBImage")
installgit()
readDICOMFile(img_dir_list[[1]][[2]])

##################


path <- "~/Desktop/USP/TCC/MATERIAL/DATA-SET/"

dir(path) -> files_list

cat( "first sub dir. file check : ", files_list [[1]],"\n")

cat("sub dir. class : ", class(files_list) )

# job def : build images dir. list of train set images
img_dir_list <-list()

# counter
cnt = 0

# build images dir. list  
for (file in seq(length(files_list)) ) {
  # sub dir1 files : file1
  dir(paste(path,files_list[[file]], sep ="/")) -> file1
  
  #sub dir.1 path : path1
  paste(path,files_list[[file]], file1,sep ="/") -> path1
  
  # single file image(s)
  paste(path1,dir(path1),sep ="/")  -> img
  
  # build images files list
  img -> img_dir_list[[file]]
  
  # incr. counter
  cnt = cnt + 1
}
# job check
n_files <-  length(files_list)
ifelse(cnt == n_files, "all files good"," not good man")

# dir check
# first and last file dir 
img_dir_list[1]
img_dir_list <- img_dir_list[-104]


# rename images directories files
names(img_dir_list) <- paste("File",1:n_files,sep="_")

#img dir. list filename check
# head
head(names(img_dir_list),3)

# tail
tail(names(img_dir_list),3)


#################
# Read  50 files from images list dir
# create empty list and prealloc.
images <- vector("list",50)

# downsample img_dir_list
set.seed(1234)
sample(length(img_dir_list),50,  replace = FALSE) -> n_sample_indx

# red file display, if any need to check red files 
red_file =""
cnt = 0
# read in n_sample files with for loop
for (ind in  n_sample_indx) {
  cnt = cnt + 1
  
  # get image name
  red_file =""
  red_file <- names(img_dir_list)[ind]
  
  # create file name 
  paste0("File_",ind) -> File
  
  # create image list
  hdr <- list()
  img <- list()
  im <- list(hdr,img)
  #im[[1]] <- hdr
  #im[[2]] <- img
  
  # read images
  try(
    readDICOM(
      strsplit(unlist(img_dir_list),"File_%d")[[ind]]
    ) -> im
    #Path)-> images [ind]
    
  )
  # assign image to images list
  images[[cnt]]<- im
  
  # set image names
  names(images)[cnt] <- File
  
}

#print(red_file)
print(cnt)

# check job
ifelse(cnt == length(n_sample_indx),"all images red😉","not all images red🤔")
##############################


fname <- system.file(file.path("dcm", "Abdo.dcm"), package="oro.dicom")
R> abdo <- readDICOMFile(fname)
R> names(abdo)
unlist(lapply(img_dir_list, readDICOMFile))
readDICOMFile(as.file(unlist(img_dir_list$File_1)))
file
  fname <- system.file("~/Desktop/USP/TCC/R-ROOT/data-files/patient2_BIG_jpeg_img/", package="oro.dicom")
  data(dicom.dic)
  big <- readDICOM(fname)
  
  
readDICOM("~/Desktop/USP/TCC/MATERIAL/DATA-SET/1.2.840.113704.1.111.10024.1500721597.1/", verbose = TRUE, boffset = 128) -> testinho
readDICOM
readDICOMFile                                                                                                      

pixeldata = TRUE
readDICOM("~/Desktop/USP/TCC/MATERIAL/DATA-SET/1.2.840.113704.1.111.10024.1500721597.1/") -> aaa

readDICOM("/home/luiz/Desktop/USP/TCC/MATERIAL/DATA-SET/1.2.840.113704.1.111.620.1501513704.1/1/", recursive = TRUE, verbose = TRUE) -> dividido

append(dividido, dividido2) -> listinhooo

map(divido ~dplyr::filter(.x, grepl("SeriesNumber|InstanceNumber", name))) -> dividido_
class(numbers)
numbers$`data-files/patient1_jpeg_transformed_img//1.2.840.113704.1.111.28952.1503356202.44143.dcm`
view(numbers)
str(numbers)
head(numbers, 20)
map(numbers, ~dplyr::select(.x, "value")) -> numbers_filtrados
view(numbers_filtrados) %>% t() %>% as.data.frame() -> numbers_filtrados_transposta
numbers_filtrados_transposta[,1:2] -> nomes.finais 
class(nomes.finais)
nomes.finais <- rename(nomes.finais, SerieNumber = V1,
                       InstanceNumber = V2)
names(nomes.finais)
nomes.finais
nomes.finais$SerieNumber %>% t() -> SeriesNumber
nomes.finais$InstanceNumber %>% t() -> InstanceNumber

names(numbers) <- print(paste(SeriesNumber, InstanceNumber, sep = "_")) 
names(numbers)

readDICOMFile("data-files/patient1_jpeg_transformed_img/1.2.840.113704.1.111.28952.1503356202.44143.dcm") 







install.packages("radtools")
git_install()
library(devtools)
githubinstall("radtools")
install_github("pamelarussell/radtools")
library(radtools)
read_dicom(img_dir_list)
radtools::dicom_header_tag(dicom_patient1, )
radtools::num_slices()
?radtools
num_slices(dicom_teste_1_meta$img)
dicom_teste_1_meta$img -> qqqq
num_slices(qqqq)

divest::scanDicom("~/Desktop/DATA-SET-DR-IGOR/1.2.840.113704.1.111.10024.1500721597.1/", verbosity = TRUE) -> aaaaaaaaaaabb
data.frame(aaaaaaaaaaabb) %>% filter(seriesNumber == 2)
divest::sortDicom("~/Desktop/DATA-SET-DR-IGOR/1.2.840.113704.1.111.10024.1500721597.1/") -> aaaaaaaaaaabb

paciente1 <- readDICOMFile("~/Desktop/remake/1.2.840.113704.1.111.1240.1479854699.1/")
divest::readDicom("~/Desktop/USP/TCC/MATERIAL/DATA-SET/1.2.840.113704.1.111.10024.1500721597.1/", verbosity = TRUE) -> aabb
scanDicom("~/Desktop/DATA-SET-DR-IGOR/1.2.840.113704.1.111.1460.1503204031.1/", verbosity = TRUE) -> bb

.readDI
install.packages("RNifti")
library(RNifti)
RNifti::lyr(aabb[1])
aabb
RNifti::analyzeHeader(aabb[20])
niftiHeader(aabb)
view(aabb[2])
image(aabb)
dcm2nii
readDicom()c
RNifti::writeAnalyze()
RNifti::niftiHeader(aabb)


RNifti::defaultInfoPanel(point = 2, data = aabb, labels = SeriesNumber)


fname <- system.file("data-files/patient2_BIG_jpeg_img/", package="oro.dicom")
data(dicom.dic)
hkLuiz <- readDICOM(fname)
R> unlist(lapply(hk40, length))
0


sliceloc.col <- which(dicom_patient1$hdr[[1]]$name == "SliceLocation")
sliceLocation <- as.numeric(hk40.info[, sliceloc.col])
R> head(sliceLocation)

readDicom("~/Desktop/DATA-SET-DR-IGOR/1.2.840.113704.1.111.10184.1496621024.1/", verbosity = TRUE, forceStack = TRUE, labelFormat = "%s_%r") -> aaBBBBB
aaBBBBB[1]

scanDicom("~/Desktop/DATA-SET-DR-IGOR/1.2.840.113704.1.111.4976.1499729456.1//", verbosity = TRUE, forceStack = TRUE, labelFormat = "%s_%r") -> aaQQ
aaQQ %>% filter(seriesNumber == 3) -> aaQQQ
  sortDicom(path = aaQQQ$rootPath, forceStack = FALSE, nested = TRUE) -> aa222222
read_DICOM(aa222222)
aaQQQ

divest::readDicom("~/Desktop/DATA-SET-DR-IGOR/1.2.840.113704.1.111.10184.1496621024.1/", verbosity = TRUE, forceStack = TRUE, labelFormat = "%s_%r") -> aaWW

aaWW[  4]

as.array(aaBBBBB[[2]]) -> array11
  RNifti::channels(array11, raw = TRUE)
RNifti::defaultInfoPanel(array11)
array

oro.dicom::readDICOM("~/Desktop/DATA-SET/T20150829092545_NMARIA_LOURDES_DIMAS_S6/") -> asasdasd



oro.dicom::readDICOM("~/Desktop/DATA-SET_TESTE/T20140208000021_NDEBORA_LOURENCO_DE_LIMA_S5/", verbose = TRUE) -> ID
ID_hdr <- ID$hdr
map(ID_hdr, ~filter(.x, grepl("SeriesNumber|InstanceNumber|SliceLocation", name))) -> ID_CORE

map(ID_CORE, ~filter(.x, grepl("SeriesNumber", name))) -> ID_SeriesNumber
map(ID_SeriesNumber, ~dplyr::select(.x, "value")) -> ID_SeriesNumber

map(ID_CORE, ~filter(.x, grepl("InstanceNumber", name))) -> ID_InstanceNumber
map(ID_InstanceNumber, ~dplyr::select(.x, "value")) -> ID_InstanceNumber

map(ID_CORE, ~filter(.x, grepl("SliceLocation", name))) -> ID_SliceLocation
map(ID_SliceLocation, ~dplyr::select(.x, "value")) -> ID_SliceLocation

rbindlist(ID_SeriesNumber, idcol = "name") -> SeriesNumber
rbindlist(ID_InstanceNumber, idcol = "name") -> InstanceNumber
rbindlist(ID_SliceLocation, idcol = "name") -> SliceLocation

SeriesNumber[InstanceNumber, on = .(name)] -> k1
k1[SliceLocation, on = .(name)] -> final
names(final)
nomes_tables <- c("Paciente", "Serie", "Instance", "Slice")
names(final) <- nomes_tables

oro.dicom::readDICOM("/home/luiz/Desktop/DATA-SET_TESTE/AMOSTRA_TREINO/T20150916211316_NVANUSA_DA_SILVA_ARAUJO_S3/") -> anonimizado



anonimizado$hdr$`/home/luiz/Desktop/DATA-SET_TESTE/AMOSTRA_TREINO/T20150916211316_NVANUSA_DA_SILVA_ARAUJO_S3//1.2.840.113704.1.111.3372.1442449133.13679.dcm` %>% 
  as.data.frame() -> anoniminha
anoniminha[83,6] -> SLICELOCATION_1
anoniminha[75,6] -> SERIE_NUMBER
anoniminha[77,6] -> INSTANCE_NUMBER
anoniminha[c(75,77,83),c(3,6)] -> VALORES_ID
print(paste(SERIE_NUMBER, INSTANCE_NUMBER, SLICELOCATION_1, sep = "_"))
pull(VALORES_ID, var = "value")


anoniminha[c(75,77,83),c(3,6)] %>% dplyr::pull("value") -> vectorzinho
print(vectorzinho)
anonimizado$img -> anoniminha_img
anonimizado$hdr -> anoniminha_hdr

names(anoniminha_img)

map(anoniminha_hdr, ~dplyr::filter(.x, grepl("SeriesNumber|InstanceNumber|SliceLocation", name))) -> numbers
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

numbers$`/home/luiz/Desktop/DATA-SET_TESTE/AMOSTRA_TREINO/T20150916211316_NVANUSA_DA_SILVA_ARAUJO_S3//1.2.840.113704.1.111.3372.1442449132.13662.dcm`

args(sortDicom)

oro.dicom::readDICOMFile("~/Desktop/testando/T20150929204320_NIVONE_BRASIL_MOREIRA_DE_SOUZA_S3/1.2.840.113704.1.111.3092.1443570916.21306.dcm") -> anonimizadeo

anonimizadeo$hdr -> hdr_ano
hdr_ano %>% filter(group == "0010") %>% filter(grepl("Patient", name))
hdr_ano %>% filter(group == "0010") %>% filter(element == "0010")


#transform(hdr_paciente1,
#          group = as.numeric(as.character(group)),
#          element = as.numeric(element),
#          length = as.numeric(length)) %>%  as_tibble() -> paciente1_tbl
#
#paciente1_tbl %>% 
#  kable() %>%
#  kable_styling(bootstrap_options = "striped", 
#                full_width = T, 
#                font_size = 12)
#
#glimpse(paciente1_tbl)
#str(paciente1_tbl)


write_csv(paciente1, )

hdr_paciente1 %>% write_csv(file = "PacienteZero.csv")

########## ESPADON PACKAGE ##########
library(espadon)


dicom.raw.data.loader("~/Desktop/DATA-SET_TESTE/T20140208000021_NDEBORA_LOURENCO_DE_LIMA_S5/1.2.840.113704.1.111.5464.1391825094.34544.dcm") -> loader
dicom.raw.data.anonymizer(paciente1) -> ano_loader
xlsx.from.dcm(ano_loader, ano.xlsx)

espadon::dicom.tag.dictionary("~/Desktop/DATA-SET_TESTE/T20140208000021_NDEBORA_LOURENCO_DE_LIMA_S5/1.2.840.113704.1.111.5464.1391825094.34544.dcm") -> listi$tag
dicom.raw.data.loader()
espadon::dicom.raw.data.anonymizer() -> ano1
args(dicom.raw.data.anonymizer)
espadon::load.obj.from.dicom(final$FilePath[1]) -> XXxxXX
XXxxX
dicom.raw.data.anonymizer(XXxxXX[0010,0010])
espadon::dicom.browser(XXxxXX)
dicom.raw




scanDicom(path ="~/Desktop/1.2.840.113704.1.111.1532.1503312054.1/", verbosity = 3, forceStack = TRUE) -> novo_divest

readDicom(novo_divest$rootPath[1]) -> tt

image(tt)

remotes::install_github("muschellij2/dcmtk")
library(dcmtk)
dcmtk_filename(version = )

install.packages("dcmtk")
dcmj2pnm()
  dcmj2pnm(path1)
PRINCIPAL[1,1]
path1 = "~/Desktop/DATA-SET_TESTE/AAAAA/T20140208000021_NDEBORA_LOURENCO_DE_LIMA_S7/1.2.840.113704.1.111.5464.1391825219.35564.dcm"

PRINCIPAL$VETOR[1]
PRINCIPAL$FilePath[1]


dcmj2pnm("~/Desktop/DATA-SET_TESTE/AAAAA/T20140208000021_NDEBORA_LOURENCO_DE_LIMA_S7/1.2.840.113704.1.111.5464.1391825219.35566.dcm", outfile = "~/Desktop/USP/TCC/projeto_TCC/123.png") -> dcm 

dcmj2pnm(PRINCIPAL$FilePath[1]) -> dcmtk1
lfpc <- function(arg1) {
  arg1 <- dcmj2pnm(arg1, outfile = "~/Desktop/USP/TCC/projeto_TCC/*.png") 

  }

for (i in PRINCIPAL$FilePath) {
  dcmdjpeg(i)
} -> assq


lfpc(PRINCIPAL$FilePath[1]) -> lfpc1


readDICOMFile("~/Desktop/DATA-SET_TESTE/AAAAA/T20150818162802_NWILSON_ROBERTO_PEREIRA_S4/1.2.840.113704.1.111.7736.1439926520.10885.dcm") -> azao
graphics::image(t(azao$img))
raster::image(azao$img)
as_image(azao$img)


prob()

samp1le()
