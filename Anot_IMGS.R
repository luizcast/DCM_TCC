library(jsonlite)
fromJSON("IMGS_ANNOT_POLIGON/X0000695874_4_43.json")
fromJSON("IMGS_ANNOT_POLIGON/X0000695874_4_43.json") -> aa
str(aa)
aa$shapes
aa %>% spread()

minify(aa)

#devtools::install_github("bnosac/image", subdir = "image.darknet", build_vignettes = TRUE)

library(image.darknet)

#If required, Set new working directory where the final predictions imaged with bounding box will be saved

#setwd(paste0(getwd(),"/projects/"))

image.darknet::image_darknet_model()



#Define Model - here it is Tiny Yolo
yolo_luiz <- image_darknet_model(type = 'detect', 
                                     model = "tiny-yolo-voc.cfg", 
                                     weights = system.file(package="image.darknet", "models", "tiny-yolo-voc.weights"), 
                                     labels = system.file(package="image.darknet", "include", "darknet", "data", "voc.names"))

system.file(package="image.darknet", "include", "darknet", "cfg", "tiny.cfg")

image
#Image Detection
x <- image_darknet_detect(file = "tinyyolo_in_R/google-car.png", 
                          object = yolo_tiny_voc,
                          threshold = 0.19)



##
## Define the detection model (YOLO) 
## (structure of the deep learning model + the learned weights + the labels)
##

f <- system.file(package="image.darknet", "include", "darknet", "data", "voc.names")
labels <- readLines(f)

yolo_tiny_voc <- image_darknet_model(type = 'detect', 
                                     model = "tiny-yolo-voc.cfg", 
                                     weights = system.file(package="image.darknet", "models", "tiny-yolo-voc.weights"), 
                                     labels = labels)
yolo_tiny_voc
