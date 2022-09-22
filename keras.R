####keras
pacotes <- c("keras", "tensorflow", "tidyverse", "recogito", "shiny")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

tf_gpu_configured()

path = "~/Desktop/USP/TCC/projeto_TCC/IMGs/"
BATH_SIZE = 20


image_dataset_from_directory('~/Desktop/USP/TCC/projeto_TCC/IMGs/') -> data_from_directory

data_generator <- keras::image_data_generator(rescale = 1./255, validation_split = 0.20)


train_generator <- flow_images_from_directory(directory = path, 
                           generator = data_generator, 
                           shuffle = TRUE, 
                           seed = 1986, 
                           class_mode = 'categorical',
                           batch_size = BATH_SIZE,
                           subset = "training",
                           target_size = c(224,224))

validation_generator <- flow_images_from_directory(directory = path,
                           generator = data_generator,
                           shuffle = TRUE,
                           seed = 1986,
                           class_mode = 'categorical',
                           batch_size = BATH_SIZE,
                           subset= "validation",
                           target_size = c(224,224))



# BATH 80TREINO / 10TESTE / 10VALIDACAO


#### Construçao do modelo ####

keras_model_sequential() %>% 
  #camada convolucional
  layer_conv_2d(filters = 64, kernel_size = 2, activation='relu', input_shape = c(224,224,3)) %>%  
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


keras::compile(keras_modelo, loss = 'binary_crossentropy',
                         optimizer = 'adam',
                         metrics = 'accuracy', run_eagerly = TRUE)



checkpoint <- keras::callback_model_checkpoint(path, 
                                               monitor = 'val_loss',
                                               verbose=1,
                                               mode='min',
                                               save_best_only = TRUE,
                                               save_weights_only = TRUE)
  
early_stop <- keras::callback_early_stopping(monitor = 'val_loss',
                                             min_delta = 0.001,
                                             patience=5,
                                             mode='min',
                                             verbose = 1)

cb <- list(checkpoint,early_stop)


serialize_model(keras_modelo, include_optimizer = TRUE)


fit_generator(keras_modelo, generator = train_generator,
              steps_per_epoch = train_generator$samples / BATH_SIZE,
              validation_data = validation_generator,
              validation_steps = validation_generator$samples / BATH_SIZE,
              epochs = 50,
              callbacks = cb)




#### model2 ####

keras_model_sequential() %>%
  layer_conv_2d(filter = 32, kernel_size=c(3,3), activation = 'relu', input_shape = c(224,224,3)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_dropout(rate = 0.3) %>%
  layer_conv_2d(filter = 64, kernel_size=c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_dropout(rate = 0.3) %>%
  layer_flatten() %>%
  layer_dense(units = 256, activation = 'relu') %>%  
  layer_dropout(rate = 0.3) %>% 
  #camada densa de saida
  layer_dense(units = 64, activation = 'sigmoid') -> modelo_2

summary(modelo_2)


keras::compile(modelo_2, loss = 'binary_crossentropy',
               optimizer = 'adam',
               metrics = 'accuracy', run_eagerly = TRUE)



fit_generator(modelo_2, generator = train_generator,
              steps_per_epoch = train_generator$samples / BATH_SIZE,
              validation_data = validation_generator,
              validation_steps = validation_generator$samples / BATH_SIZE,
              epochs = 50,
              callbacks = cb)

