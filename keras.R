####keras
pacotes <- c("keras", "tensorflow", "tidyverse")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

path = "~/Desktop/IMAGENS_KERAS/"
BATH_SIZE = 50

image_dataset_from_directory('~/Desktop/DATA-SET_TESTE/') -> sera

data_generator <- keras::image_data_generator(rescale = 1./255, validation_split = 0.30)

train_generator <- flow_images_from(directory = path, 
                           generator = data_generator, 
                           shuffle = TRUE, 
                           seed = 100, 
                           class_mode = 'categorical',
                           batch_size = BATH_SIZE,
                           subset = "training")

validation_generator <- flow_images_from_directory(directory = path,
                           generator = data_generator,
                           shuffle = TRUE,
                           seed = 100,
                           class_mode = 'categorical',
                           batch_size = BATH_SIZE,
                           subset="validation")



# BATH 80TREINO / 10TESTE / 10VALIDACAO
#### Construçao do modelo ####

keras_model_sequential() %>% 
  #camada convolucional
  layer_conv_2d(filters = 64, kernel_size = 2, activation='relu', input_shape = c(28,28,1)) %>%  
  #camada de maxpooling para downsampling
  layer_max_pooling_2d(pool_size = 2) %>% 
  # camada dropout
  layer_dropout(rate = 0.3) %>% 
  #segunda camada
  layer_conv_2d(filters = 128, kernel_size = 2, activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = 2) %>% 
  layer_dropout(rate=0.3) %>%  
  #reshape no output transformando em array
  layer_flatten() %>% 
  #camada densa full-connected
  layer_dense(units = 256, activation = 'relu') %>%  
  layer_dropout(rate = 0.3) %>% 
  #camada densa de saida
  layer_dense(units = 64, activation = 'relu') -> keras_modelo

summary(keras_modelo)


#### Compilação: Como a rede irá arender #####

keras_modelo %>% compile(loss = 'binary_crossentroy',
                         optimizer = 'adam',
                         metrics = 'accuracy')


keras::fit_generator(callbacks = )
    
