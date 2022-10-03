start_time <- Sys.time()

library(keras)

deepData <- trainingData
deepNrow <- trainingNrow
lookBack <- 6#21

years <- 6#21
# Vectorization
x_1 <- array(0, dim = c(deepNrow * lookBack, years, 81))
y_fraction <- array(0, dim = c(deepNrow * lookBack, years))
y_signBit_RL <- array(0, dim = c(deepNrow * lookBack, 1))
y_index <- array(0, dim = c(deepNrow * lookBack, 1))

for(i in 1:deepNrow) {
  medianFCF <- median(unlist(financialStatment[deepData[i], c('freeCashFlow','pastFCF_1', 'pastFCF_2', 'pastFCF_3', 'pastFCF_4', 'pastFCF_5')]))
  countExponent <- floor(log(abs(medianFCF), 10))
  
  for(j in 1:lookBack) {
    
    x_1[(i-1)*lookBack+j,1,] <- c(c(unlist(financialStatment[deepData[i], c(3:14,16:82)]), unlist(financialStatment[deepData[i],c('freeCashFlow')])) / financialStatment[deepData[i], 15], floor(log(financialStatment[deepData[i], 15], 10)))
    x_1[(i-1)*lookBack+j,2,] <- c(unlist(financialStatment[deepData[i], paste0(c(3:14,16:82, 'pastFCF'),'_1')])  / financialStatment[deepData[i], '15_1'], floor(log(financialStatment[deepData[i], '15_1'], 10)))
    x_1[(i-1)*lookBack+j,3,] <- c(unlist(financialStatment[deepData[i], paste0(c(3:14,16:82, 'pastFCF'),'_2')]) / financialStatment[deepData[i], '15_2'], floor(log(financialStatment[deepData[i], '15_2'], 10)))
    x_1[(i-1)*lookBack+j,4,] <- c(unlist(financialStatment[deepData[i], paste0(c(3:14,16:82, 'pastFCF'),'_3')]) / financialStatment[deepData[i], '15_3'], floor(log(financialStatment[deepData[i], '15_3'], 10)))
    x_1[(i-1)*lookBack+j,5,] <- c(unlist(financialStatment[deepData[i], paste0(c(3:14,16:82, 'pastFCF'),'_4')]) / financialStatment[deepData[i], '15_4'], floor(log(financialStatment[deepData[i], '15_4'], 10)))
    x_1[(i-1)*lookBack+j,6,] <- c(unlist(financialStatment[deepData[i], paste0(c(3:14,16:82, 'pastFCF'),'_5')]) / financialStatment[deepData[i], '15_5'], floor(log(financialStatment[deepData[i], '15_5'], 10)))
    
    y_fraction[(i-1)*lookBack+j,] <- unlist(abs(financialStatment[deepData[i], c('pastFCF_5', 'pastFCF_4', 'pastFCF_3', 'pastFCF_2', 'pastFCF_1', 'freeCashFlow')]) / (10^countExponent))
    
    if(sum(financialStatment[deepData[i], c('freeCashFlow','pastFCF_1','pastFCF_2','pastFCF_3','pastFCF_4','pastFCF_5')]) < 0) {
      y_signBit_RL[(i-1)*lookBack+j, 1] <- 1
    } else {
      y_signBit_RL[(i-1)*lookBack+j, 1] <- 0
    }
    
    y_index[(i-1)*lookBack+j, 1] <- deepData[i]
  }
}



LevyLayer_1 <- R6::R6Class(
  "KerasLayer_2",
  inherit = KerasLayer,
  public = list(
    output_dim = NULL,
    kernel = NULL,
    initialize = function(output_dim) {
      self$output_dim <- output_dim
    },
    build = function(input_shape) {
      self$kernel <- self$add_weight(
        name = 'kernel', 
        shape = list(input_shape[[2]], 1L),
        initializer = initializer_random_normal(mean = 0.2, stddev = 0.1),
        trainable = TRUE
      )
    },
    call = function(x, mask = NULL) {
      #x[1]: p, x[2]: tick, x[3]: c, x[4]: i
      k_relu(k_dot(x, self$kernel)+1e-5) + 1e-5#(exp(1.5) * 3 ^ (-1.5) * sqrt(2 * pi))
      
    },
    compute_output_shape = function(input_shape) {
      list(input_shape[[1]], self$output_dim)
    })
)

# Create layer wrapper function
layer_levy_1 <- function(object, output_dim, name = NULL, trainable = TRUE) {
  create_layer(LevyLayer_1, object, list(
    output_dim = as.integer(output_dim),
    name = name,
    trainable = trainable
  ))
}

LevyLayer_2 <- R6::R6Class(
  "KerasLayer_1",
  inherit = KerasLayer,
  public = list(
    output_dim = NULL,
    kernel = NULL,
    initialize = function(output_dim) {
      self$output_dim <- output_dim
    },
    build = function(input_shape) {
      self$kernel <- self$add_weight(
        name = 'kernel', 
        shape = list(input_shape[[2]], 1L),
        initializer = initializer_random_normal(),
        trainable = TRUE
      )
    },
    call = function(x, mask = NULL) {
      #x[1]: p, x[2]: tick, x[3]: c, x[4]: i
      k_sigmoid(k_dot(x, self$kernel))
      
    },
    compute_output_shape = function(input_shape) {
      list(input_shape[[1]], self$output_dim)
    })
)

# Create layer wrapper function
layer_levy_2 <- function(object, output_dim, name = NULL, trainable = TRUE) {
  create_layer(LevyLayer_2, object, list(
    output_dim = as.integer(output_dim),
    name = name,
    trainable = trainable
  ))
}


LevyLayer_3 <- R6::R6Class(
  "KerasLayer_3",
  inherit = KerasLayer,
  public = list(
    output_dim = NULL,
    kernel = NULL,
    initialize = function(output_dim) {
      self$output_dim <- output_dim
    },
    build = function(input_shape) {
      self$kernel <- self$add_weight(
        name = 'kernel', 
        shape = list(input_shape[[2]], 1L),
        initializer = initializer_random_normal(),
        trainable = TRUE
      )
    },
    call = function(x, mask = NULL) {
      #x[1]: p, x[2]: tick, x[3]: c, x[4]: i
      #p <- (-15 + sqrt(225+40*c^2)) / 2 *c^2
      x_1 <- k_sigmoid(k_dot(x, self$kernel))# * (x_3 * (2 / 3)) + (x_2 * 7)
      return(x_1)
    },
    compute_output_shape = function(input_shape) {
      list(input_shape[[1]], self$output_dim)
    })
)

# Create layer wrapper function
layer_levy_3 <- function(object, output_dim, name = NULL, trainable = TRUE) {
  create_layer(LevyLayer_3, object, list(
    output_dim = as.integer(output_dim),
    name = name,
    trainable = trainable
  ))
}





readFS_2 <- keras_model_sequential()
readFS_2 %>%
  layer_lstm(units = 81, input_shape = c(years, 81), return_sequences = TRUE) %>%
  layer_dropout(0.1) %>%
  layer_lstm(units = 81) %>%
  layer_dropout(0.1) %>%
  layer_dense(81, activation = 'sigmoid', kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  layer_dropout(0.1)

readFSInput_2 <- layer_input(shape = list(years, 81))
readModel_2 <- readFS_2(readFSInput_2)

output_c <- readModel_2%>%
  layer_levy_1(output_dim = 1) %>%
  layer_dropout(0.1)

output_tick <- readModel_2%>%
  layer_levy_3(output_dim = 1) %>%
  layer_dropout(0.1)

pivot <- readModel_2%>%
  layer_levy_2(output_dim = 1) %>%
  layer_dropout(0.1)

discountedRate <- readModel_2%>%
  layer_levy_2(output_dim = 1) %>%
  layer_dropout(0.1)
#multiply_pivot <- layer_multiply(c(pivot, output_c), trainable = FALSE)

#tick_7 <- layer_lambda(output_tick, function(x){
#  x*7
#})

#output_pivot <- layer_add(c(multiply_pivot, tick_7))


Model_2 <- keras_model(list(readFSInput_2), list(output_c, pivot, output_tick, discountedRate))#

levyLoss <- function(y_true, y_pred) {
  #clip_y_pred <- k_clip(y_pred, 1e-1, 0.999)
  vectorA <- y_true - (1/2)
  vectorB <- y_pred - (1/2)
  signOfAB <- k_sign(vectorA * vectorB) 
  positiveError <- k_abs(y_pred - y_true)
  positiveError <- k_clip(positiveError, 0, 0.999)
  errors <- ((1 - signOfAB)/2) * (-k_log(1-positiveError)) + ((1 + signOfAB)/2) * (k_square(positiveError)) 
  
  k_mean(errors)
}

logScale <- function(y_true, y_pred) {
  y_pred <- k_clip(y_pred, 1e-10, 1)
  errors <- k_abs((k_log(y_pred) - k_log(y_true)))
  #error <- k_abs( k_sin((positiveError-1)/(10*pi)) / k_cos(positiveError-1)/(10*pi)) # / y_true
  
  k_mean(errors)
}

logEntropy <- function(y_true, y_pred) {
  y_pred <- k_clip(y_pred, 1e-10, 1)
  errors <- -k_log(1-k_abs(y_pred-y_true))
  #error <- k_abs( k_sin((positiveError-1)/(10*pi)) / k_cos(positiveError-1)/(10*pi)) # / y_true
  
  k_mean(errors)
}

#temp <- predict(Model_2, list(x_1, x_3)); summary(temp)
Model_2 %>% compile(
  loss = list(logScale, logEntropy, logScale, logScale),#logScale),
  optimizer = "adam"
)
summary(Model_2)


#utility for Demo
dLevy_c <- function(x, pivot, tangent) {
  realPivot <- pivot * (2/3) * x
  #print(paste0('x: ', x,'realPivot: ', realPivot))
  a <- (sqrt(x/(2*pi)) * exp(-x/(2*realPivot)) * (-1.5*realPivot^(-2.5)+0.5*x*realPivot^(-3.5)))
  #b <- tangent
  #-(a*b) / abs(a*b) * (1/(abs(a)-abs(b) + 1e-5))
  return(a)
}
dLevy_p <- function(x, C_, tangent) {
  cx <- x * (2/3) * C_
  vectorA <- (sqrt(C_/(2*pi)) * exp(-C_/(2*cx)) * (-1.5*cx^(-2.5)+0.5*C_*cx^(-3.5)))
  vectorB <- tangent
  #return((vectorA * vectorB) / (abs(vectorA) * abs(vectorB)))
  abs(vectorA - vectorB)
}

realValueLevy <- function(i, C_, pivot, tick) {
  realPivot <- pivot * (2/3) * C_
  position <- max((realPivot) + (tick * i), 1e-5)
  return(sqrt(C_/(2*pi))  /  (position^(3/2)  * exp(C_ / (position * 2)))/ tick )
}

demo_c <- function(fractions, C_, pivot, tick) {
  min_c <- exp(1.5) * 3 ^ (-1.5) * sqrt(2 * pi)
  tangents <- diff(fractions)
  meanTan <- mean(tangents, na.rm = TRUE)
  
  tanLevy <- dLevy_c(C_, pivot, meanTan)
  
  C_r <- C_
  
  if(tanLevy * meanTan > 0 && abs(tanLevy) - abs(meanTan) > 0) {
    C_r <- C_r + runif(1) / 20
  } else {
    C_r <- C_r - runif(1) / 20
  }
  
  if(C_r < 1e-6) {
    C_r <- 1e-6
  }
  
  return(C_r)
}

demo_pivot <- function(fractions, C_, pivot, tick) {
  tangents <- diff(fractions)
  meanTan <- mean(tangents, na.rm = TRUE)
  #print(paste0('fractions1: ', fractions[1], ' fractions2: ', fractions[2],
  #             ' fractions3: ', fractions[3], ' fractions4: ', fractions[4],
  #             ' fractions5: ', fractions[5], ' fractions6: ', fractions[6], 
  #             ' C_: ', C_, ' pivot: ', pivot, ' tick: ', tick, ' meanTan: ', meanTan))
  
  root_1 <- optimize(dLevy_p, interval=c(0, 0.2), tangent=meanTan, C_=C_, maximum=FALSE)
  root_2 <- optimize(dLevy_p, interval=c(0.2, 0.4), tangent=meanTan, C_=C_, maximum=FALSE)
  root_3 <- optimize(dLevy_p, interval=c(0.4, 0.6), tangent=meanTan, C_=C_, maximum=FALSE)
  root_4 <- optimize(dLevy_p, interval=c(0.6, 0.8), tangent=meanTan, C_=C_, maximum=FALSE)
  root_5 <- optimize(dLevy_p, interval=c(0.8, 1), tangent=meanTan, C_=C_, maximum=FALSE)
  #root <- uniroot(dLevy_p, interval = c(0, (2*C_)/3), tol = 0.0001, tangent=meanTan, C_=C_)
  minimal <- which.min(c(root_1$objective, root_2$objective, root_3$objective, root_4$objective, root_5$objective))
  
  minPostion <- switch(minimal,
                       root_1$minimum,
                       root_2$minimum,
                       root_3$minimum,
                       root_4$minimum,
                       root_5$minimum)
  
  shoulBe <- minPostion
  if((minPostion-0.5) * (pivot-0.5) < 0){
    if(pivot < 0.5) {
      shoulBe <- 0.99
    } else {
      shoulBe <- 0.01
    }
  }
  
  return(shoulBe)
  
  
}

demo_tick <- function(fractions, C_, pivot, tick) {
  sumOfTick <- 1e-5
  for(i in 0:(length(fractions)-1)) {
    sumOfTick <- sumOfTick + (realValueLevy(-i, C_, pivot, 1) / fractions[i+1])
  }
  #print(paste0('sumOfTick,:',sumOfTick, '\n'))
  return(min(sumOfTick / length(fractions), 1))
}


demo_dRate <- function(fractions, C_, pivot, tick, dRate, index, financialStatment, expon) {
  lookRorward <- 1000
  #print(financialStatment[index, 'backTest'])
  if(is.null(financialStatment[index, 'backTest']) ||is.na(financialStatment[index, 'backTest']) || which.max(expon) == 11) {
    return(dRate)
  }
  
  cumulatived <- 1
  sumDCF <- 0
  
  fract <- realValueLevy(1, C_, pivot, tick)
  freeCashFlowOfJ <- fract * (10^(which.max(expon)))
  cumulatived <- cumulatived * dRate
  sumDCF <- sumDCF + (freeCashFlowOfJ * cumulatived)
  for(j in 2:lookRorward) {
    fract <- realValueLevy(j, C_, pivot, tick)
    freeCashFlowOfJ <- fract * (10^(which.max(expon)))
      
    cumulatived <- cumulatived * dRate
      
    sumDCF <- sumDCF + freeCashFlowOfJ * cumulatived
  }
  tempValue <- (sumDCF + financialStatment[index, 3] + financialStatment[index, 12] + (financialStatment[deepData[i], 11] - financialStatment[deepData[i], 16]) - financialStatment[index, 19]) / (financialStatment[index, 20] / 10)
  
  shoulBe <- dRate
  if(tempValue > financialStatment[index, 'backTest']) {
    shoulBe <- shoulBe  - abs(tempValue / financialStatment[index, 'backTest']) / 100
  } else if(tempValue < financialStatment[index, 'backTest']) {
    shoulBe <- shoulBe  + abs(financialStatment[index, 'backTest'] / tempValue) / 100
  }
  
  if(shoulBe < 0.0001) {
    shoulBe <- 0.0001
  } else if (shoulBe > 1) {
    shoulBe <- 1
  }
  
  return(shoulBe)
}

distri_c_g <- c(); distri_c_r <- c()
distri_pivot_g <- c(); distri_pivot_r <- c()
distri_tick_g <- c(); distri_tick_r <- c()
distri_dRate_g <- c(); distri_dRate_r <- c()

y_c <- array(0, dim = c(nrow(y_fraction), 1))
y_pivot <- array(0, dim = c(nrow(y_fraction), 1))
y_tick <- array(0, dim = c(nrow(y_fraction), 1))
y_dRate <- array(0, dim = c(nrow(y_fraction), 1))

reserving <- array(600, dim = c(10))
expanding <- 1:600
for(i in expanding) {#
  print(paste0('current: ', i))
  C_pivot_tick <- predict(Model_2, list(x_1))
  expon <- predict(Model_1, list(x_1))
  
  y_c[] <- 0
  y_pivot[] <- 0
  y_tick[] <- 0
  y_dRate[] <- 0
  
  for(j in 1:nrow(y_fraction)) {
    y_c[j] <- demo_c(y_fraction[j,], C_pivot_tick[[1]][j], C_pivot_tick[[2]][j], C_pivot_tick[[3]][j])
    
    y_pivot[j] <- demo_pivot(y_fraction[j,], C_pivot_tick[[1]][j], C_pivot_tick[[2]][j], C_pivot_tick[[3]][j])
    
    y_tick[j] <- demo_tick(y_fraction[j,], C_pivot_tick[[1]][j], C_pivot_tick[[2]][j], C_pivot_tick[[3]][j])
    
    y_dRate[j] <- demo_dRate(y_fraction[j,], C_pivot_tick[[1]][j], C_pivot_tick[[2]][j], C_pivot_tick[[3]][j], C_pivot_tick[[4]][j], y_index[j], financialStatment, expon[j,])
  }
  
  replay <- y_signBit_RL == 0 #(1:nrow(y_fraction))#,(nrow(y_fraction)/2)
  print(summary(C_pivot_tick[[1]][replay]))
  print(summary(y_c[replay]))
  print(sum(y_c[replay]-C_pivot_tick[[1]][replay]))
  distri_c_g <- rbind(distri_c_g, summary(C_pivot_tick[[1]][replay]))
  distri_c_r <- rbind(distri_c_r, summary(y_c[replay]))
  
  print(summary(C_pivot_tick[[2]][replay]))
  print(summary(y_pivot[replay]))
  print(sum(y_pivot[replay]-C_pivot_tick[[2]][replay]))
  distri_pivot_g <- rbind(distri_pivot_g, summary(C_pivot_tick[[2]][replay]))
  distri_pivot_r <- rbind(distri_pivot_r, summary(y_pivot[replay]))
  
  print(summary(C_pivot_tick[[3]][replay]))
  print(summary(y_tick[replay]))
  print(sum(y_tick[replay]-C_pivot_tick[[3]][replay]))
  distri_tick_g <- rbind(distri_tick_g, summary(C_pivot_tick[[3]][replay]))
  distri_tick_r <- rbind(distri_tick_r, summary(y_tick[replay]))
  
  print(summary(C_pivot_tick[[4]][replay]))
  print(summary(y_dRate[replay]))
  print(sum(y_dRate[replay]-C_pivot_tick[[4]][replay]))
  distri_dRate_g <- rbind(distri_dRate_g, summary(C_pivot_tick[[4]][replay]))
  distri_dRate_r <- rbind(distri_dRate_r, summary(y_dRate[replay]))
  
  #x_1[is.na(x_1)] <- 0
  Model_2 %>% fit(
    list(x_1[replay,,]), list(y_c[replay], y_pivot[replay], y_tick[replay], y_dRate[replay]), #y_signBit, y_exponentBias
    batch_size = 480,
    epochs = 40
  )
  
  #errors <- sapply(replay, function(x, index, c_, pivot, tick, y_fraction) {
  #  error <- 0
  #  for( i in 0:(length(y_fraction)-1)) {
  #    error <- error+ abs(realValueLevy(-i, c_[x], pivot[x], tick[x]) - y_fraction[i+1])
  #  }
  #  return(error)
  #}, index=index,c_=C_pivot_tick[[1]], pivot = C_pivot_tick[[2]], tick =C_pivot_tick[[3]], y_fraction=y_fraction)
  
  #print(paste0('errors: ',mean(errors)))
}

#summary tangent
#a <- apply(y_fraction, 1, diff)
#b <- apply(a, 2, diff)
#summary(b)
plotDistribution <- function(distri_g, distri_r, ylabel) {
  minValue = min(distri_g[,2], distri_r[,2])
  maxValue = max(distri_g[,5], distri_r[,5])
  
  #plot(distri_g[,1], type="l", xlab = "epoch", ylab =ylabel, ylim = c(minValue, maxValue), col="darkred")
  #par(new=TRUE)
  plot(distri_g[,2], type="l", xlab = "epoch", ylab =ylabel, ylim = c(minValue, maxValue), col="darkblue")
  par(new=TRUE)
  plot(distri_g[,3], type="l", xlab = "epoch", ylab =ylabel, ylim = c(minValue, maxValue), col="orange")
  par(new=TRUE)
  #plot(distri_g[,4], type="l", xlab = "epoch", ylab =ylabel, ylim = c(minValue, maxValue), col="darkgreen")
  #par(new=TRUE)
  plot(distri_g[,5], type="l", xlab = "epoch", ylab =ylabel, ylim = c(minValue, maxValue), col="blue")
  par(new=TRUE)
  #plot(distri_g[,6], type="l", xlab = "epoch", ylab =ylabel, ylim = c(minValue, maxValue), col="red")
  #par(new=TRUE)
  
  #plot(distri_r[,1], type="l", xlab = "epoch", ylab =ylabel, ylim = c(minValue, maxValue), col="darkred", lty=2)
  #par(new=TRUE)
  plot(distri_r[,2], type="l", xlab = "epoch", ylab =ylabel, ylim = c(minValue, maxValue), col="darkblue", lty=2)
  par(new=TRUE)
  plot(distri_r[,3], type="l", xlab = "epoch", ylab =ylabel, ylim = c(minValue, maxValue), col="orange", lty=4)
  par(new=TRUE)
  #plot(distri_r[,4], type="l", xlab = "epoch", ylab =ylabel, ylim = c(minValue, maxValue), col="darkgreen", lty=2)
  #par(new=TRUE)
  plot(distri_r[,5], type="l", xlab = "epoch", ylab =ylabel, ylim = c(minValue, maxValue), col="blue", lty=2)
  #par(new=TRUE)
  #plot(distri_r[,6], type="l", xlab = "epoch", ylab =ylabel, ylim = c(minValue, maxValue), col="red", lty=2)
}

plotDistribution(distri_c_g, distri_c_r, 'c')
plotDistribution(distri_pivot_g, distri_pivot_r, 'pivot')
plotDistribution(distri_tick_g, distri_tick_r, 'tick')
plotDistribution(distri_dRate_g, distri_dRate_r, 'rate')










if(is.null(financialStatment$value)) {
  value <- array(NA, dim = c(nrow(financialStatment), 1))
  colnameTemp <- c(colnames(financialStatment), 'value')
  financialStatment <- cbind(financialStatment, value)
} else {
  financialStatment[,'value'] <- NA
}

if(is.null(financialStatment$sign)) {
  sign <- array(NA, dim = c(nrow(financialStatment), 1))
  colnameTemp <- c(colnames(financialStatment), 'sign')
  financialStatment <- cbind(financialStatment, sign)
} else {
  financialStatment[,'sign'] <- NA
}

reverse745 <- function(fractPa, expon, j) {
  fract <- realValueLevy(j, fractPa[[1]][1], fractPa[[2]][1], fractPa[[3]][1])
  
  #print(paste0(' sign:  ', sign, ', fract_1:', fractPa[1], ', fract_2:', fractPa[2], 
  #             ', fract_3:', fractPa[3], ', fract:', fract, ' expon:', which.max(expon)))
  
  fract * (10^(which.max(expon)+0))
}

deepData <- which(notNaData & 1)#testingData##which(notNaData & financialStatment[,1] == '1314')
deepNrow <- sum(notNaData & 1)#testingNrow##sum(notNaData & financialStatment[,1] == '1314')
lookRorward <- 1000

x_1_test <- array(0, dim = c(1, years, 81))
for(i in 1:deepNrow) {#deepNrow
  #print(paste0(' progress:  ', i,'/', deepNrow))
  
  x_1_test[1,1,] <- c(c(unlist(financialStatment[deepData[i], c(3:14,16:82)]), unlist(financialStatment[deepData[i],c('freeCashFlow')])) / financialStatment[deepData[i], 15], floor(log(financialStatment[deepData[i], 15], 10)))
  x_1_test[1,2,] <- c(unlist(financialStatment[deepData[i], paste0(c(3:14,16:82, 'pastFCF'),'_1')]) / financialStatment[deepData[i], '15_1'], floor(log(financialStatment[deepData[i], '15_1'], 10)))
  x_1_test[1,3,] <- c(unlist(financialStatment[deepData[i], paste0(c(3:14,16:82, 'pastFCF'),'_2')]) / financialStatment[deepData[i], '15_2'], floor(log(financialStatment[deepData[i], '15_2'], 10)))
  x_1_test[1,4,] <- c(unlist(financialStatment[deepData[i], paste0(c(3:14,16:82, 'pastFCF'),'_3')]) / financialStatment[deepData[i], '15_3'], floor(log(financialStatment[deepData[i], '15_3'], 10)))
  x_1_test[1,5,] <- c(unlist(financialStatment[deepData[i], paste0(c(3:14,16:82, 'pastFCF'),'_4')]) / financialStatment[deepData[i], '15_4'], floor(log(financialStatment[deepData[i], '15_4'], 10)))
  x_1_test[1,6,] <- c(unlist(financialStatment[deepData[i], paste0(c(3:14,16:82, 'pastFCF'),'_5')]) / financialStatment[deepData[i], '15_5'], floor(log(financialStatment[deepData[i], '15_5'], 10)))
  
  
  #equityCost <- (interestRate[financialStatment[deepData[i], 2] == interestRate[, 1], 2][[1]] / 100 + 1)^(1/4)-1
  #debtCost <- financialStatment[deepData[i], 52] / financialStatment[deepData[i], 19]
  #debtProporation <- financialStatment[deepData[i], 19] / financialStatment[deepData[i], 15]
  
  sumDCF <- 0
  cumulatived <- 1
  
  sign_Exponent <- predict(Model_1, list(x_1_test))
  c_pivot_tick <- predict(Model_2, list(x_1_test))
  #print(paste0('0',' freeCashFlow   :  ', financialStatment[deepData[i], 'freeCashFlow']))
  
  if(which.max(sign_Exponent) == 11) {
    financialStatment[deepData[i], 'sign'] <- 1
    next
  }
  financialStatment[deepData[i], 'sign'] <- 0
  
  previousFCF <- reverse745(c_pivot_tick, sign_Exponent, 1)
  freeCashFlowOfJ <- previousFCF
  cumulatived <- cumulatived * c_pivot_tick[[4]][1]#(1 + (debtCost * debtProporation + ((1-debtProporation)*max(equityCost, (freeCashFlowOfJ-previousFCF)/previousFCF))))
  sumDCF <- sumDCF + (freeCashFlowOfJ * cumulatived)
  #print(paste0('freeCashFlow: ', financialStatment[deepData[i], 'freeCashFlow']))
  #print(paste0(1 ,' freeCashFlowOfJ:  ', freeCashFlowOfJ, ', NP:', freeCashFlowOfJ / (cumulatived ^ (1/4)), ' discounted rate:', (cumulatived ^ (1/4)), ', sumDCF:', sumDCF, ', FreeCashFloe:', financialStatment[deepData[i], 'freeCashFlow']))
  #cat(allCasgFlow[[1]][1], allCasgFlow[[2]][1], allCasgFlow[[3]][1,], '\n')
  for(j in 2:lookRorward) {
    freeCashFlowOfJ <- reverse745(c_pivot_tick, sign_Exponent, j)
    #cat(allCasgFlow[[1]][j], allCasgFlow[[2]][j], allCasgFlow[[3]][j,], '\n')
    cumulatived <- cumulatived * c_pivot_tick[[4]][1]#(1 + (debtCost * debtProporation + ((1-debtProporation)*max(equityCost, (freeCashFlowOfJ-previousFCF)/previousFCF)))) # 
    
    sumDCF <- sumDCF + (freeCashFlowOfJ * cumulatived)
    previousFCF <- freeCashFlowOfJ
    #print(paste0(j ,' freeCashFlowOfJ:  ', freeCashFlowOfJ, ', NP:', freeCashFlowOfJ / (cumulatived ^ (1/4)), ' discounted rate:', (cumulatived ^ (1/4)), ', sumDCF:', sumDCF))
  }
  financialStatment[deepData[i], 'value'] <- (sumDCF + financialStatment[deepData[i], 3] + financialStatment[deepData[i], 12] + (financialStatment[deepData[i], 11] - financialStatment[deepData[i], 16]) - financialStatment[deepData[i], 19]) / (financialStatment[deepData[i], 20] / 10) #
  cat(financialStatment[deepData[i], 2], ',', financialStatment[deepData[i], 1], 'sign: ',  financialStatment[deepData[i], 'sign'], ', value: ', financialStatment[deepData[i], 'value'], '\n')
}



if(is.null(financialStatment$premium)) {
  premium <- array(0, dim = c(nrow(financialStatment), 1))
  colnameTemp <- c(colnames(financialStatment), 'premium')
  financialStatment <- cbind(financialStatment, premium)
} else {
  financialStatment[,'premium'] <- 0
}


financialStatment[, 'premium'] <- (financialStatment[, 'value'] - financialStatment[, 'backTest']) / abs(financialStatment[, 'backTest'])

financialStatment[is.na(financialStatment[, 'premium']), 'premium'] <- 0
underEstimate <- financialStatment[, 'premium'] > 0
financialStatment[financialStatment[, 2] == '2018/03' & underEstimate, c('¤½¥q','premium', 'value', 'backTest')]

write.csv(financialStatment, file = 'result_Guided_Policy_Search.csv')



end_time <- Sys.time()
end_time - start_time

