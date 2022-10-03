library(keras)

deepData <- trainingData
deepNrow <- trainingNrow
lookBack <- 6#21

years <- 6#21
# Vectorization
x <- array(0, dim = c(deepNrow, years, 81))
#y_signBit <- array(0, dim = c(deepNrow, 1))
y_exponentBias <- array(0, dim = c(deepNrow, 11))


for(i in 1:deepNrow) {
  x[i,1,] <- c(c(unlist(financialStatment[deepData[i], c(3:14,16:82)]), unlist(financialStatment[deepData[i],c('freeCashFlow')])) / financialStatment[deepData[i], 15], floor(log(financialStatment[deepData[i], 15], 10)))
  x[i,2,] <- c(unlist(financialStatment[deepData[i], paste0(c(3:14,16:82, 'pastFCF'),'_1')]) / financialStatment[deepData[i], '15_1'], floor(log(financialStatment[deepData[i], '15_1'], 10)))
  x[i,3,] <- c(unlist(financialStatment[deepData[i], paste0(c(3:14,16:82, 'pastFCF'),'_2')]) / financialStatment[deepData[i], '15_2'], floor(log(financialStatment[deepData[i], '15_2'], 10)))
  x[i,4,] <- c(unlist(financialStatment[deepData[i], paste0(c(3:14,16:82, 'pastFCF'),'_3')]) / financialStatment[deepData[i], '15_3'], floor(log(financialStatment[deepData[i], '15_3'], 10)))
  x[i,5,] <- c(unlist(financialStatment[deepData[i], paste0(c(3:14,16:82, 'pastFCF'),'_4')]) / financialStatment[deepData[i], '15_4'], floor(log(financialStatment[deepData[i], '15_4'], 10)))
  x[i,6,] <- c(unlist(financialStatment[deepData[i], paste0(c(3:14,16:82, 'pastFCF'),'_5')]) / financialStatment[deepData[i], '15_5'], floor(log(financialStatment[deepData[i], '15_5'], 10)))
  
  if(sum(financialStatment[deepData[i], c('freeCashFlow','pastFCF_1','pastFCF_2','pastFCF_3','pastFCF_4','pastFCF_5')]) < 0) {
    y_exponentBias[i, 11] <- 1
  } else {
    medianFCF <- median(unlist(financialStatment[deepData[i], c('freeCashFlow','pastFCF_1', 'pastFCF_2', 'pastFCF_3', 'pastFCF_4', 'pastFCF_5')]))
    countExponent <- floor(log(abs(medianFCF), 10))
    y_exponentBias[i, countExponent] <- 1
  }
  
}


readFS <- keras_model_sequential()
readFS %>%
  layer_lstm(units = 81, input_shape = c(years, 81), return_sequences = TRUE) %>% 
  layer_dropout(0.1) %>%
  layer_lstm(units = 81) %>% 
  layer_dropout(0.1) %>%
  layer_dense(81, activation = 'sigmoid', kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  layer_dropout(0.1)


readFSInput <- layer_input(shape = list(years, 81))
readModel <- readFS(readFSInput)


#outputSign <- readModel%>%
#              layer_dense(81) %>%
#              layer_dropout(0.1) %>%
#              layer_dense(1, activation = "sigmoid") %>%
#              layer_dropout(0.1)


outputFraction <- readModel %>% 
                  layer_dense(81) %>%
                  layer_dropout(0.1) %>%
                  layer_dense(11, activation = "softmax")


Model_1 <- keras_model(list(readFSInput), list(outputFraction))#


#adam <- optimizer_adam(lr = 0.0001)
Model_1 %>% compile(
  loss = list("categorical_crossentropy"),#
  optimizer = "rmsprop"
)
summary(Model_1)


Model_1 %>% fit(
  list(x), list(y_exponentBias), 
  batch_size = 120,
  epochs = 600
)
