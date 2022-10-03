
years <- 6
training <- financialStatment[, 2] < '2004/01'
testing <- financialStatment[, 2] >= '2004/01'
notNaData <- (!is.na(financialStatment[,'futureFCF']) & !is.null(financialStatment[,'futureFCF'])) &
              (financialStatment[,'freeCashFlow'] != 0 & !is.na(financialStatment[,'freeCashFlow']) & !is.null(financialStatment[,'freeCashFlow'])) &
              (financialStatment[,'pastFCF_1'] != 0 & !is.na(financialStatment[,'pastFCF_1']) & !is.null(financialStatment[,'pastFCF_1'])) &
              (financialStatment[,'pastFCF_2'] != 0 & !is.na(financialStatment[,'pastFCF_2']) & !is.null(financialStatment[,'pastFCF_2'])) &
              (financialStatment[,'pastFCF_3'] != 0 & !is.na(financialStatment[,'pastFCF_3']) & !is.null(financialStatment[,'pastFCF_3'])) &
              (financialStatment[,'pastFCF_4'] != 0 & !is.na(financialStatment[,'pastFCF_4']) & !is.null(financialStatment[,'pastFCF_4'])) & 
              (financialStatment[,'pastFCF_5'] != 0 & !is.na(financialStatment[,'pastFCF_5']) & !is.null(financialStatment[,'pastFCF_5'])) & 
              (financialStatment[,15] != 0 & !is.na(financialStatment[,15]) & !is.null(financialStatment[,15])) &
              (financialStatment[,'15_1'] != 0 & !is.na(financialStatment[,'15_1']) & !is.null(financialStatment[,'15_1'])) &
              (financialStatment[,'15_2'] != 0 & !is.na(financialStatment[,'15_2']) & !is.null(financialStatment[,'15_2'])) & 
              (financialStatment[,'15_3'] != 0 & !is.na(financialStatment[,'15_3']) & !is.null(financialStatment[,'15_3'])) &
              (financialStatment[,'15_4'] != 0 & !is.na(financialStatment[,'15_4']) & !is.null(financialStatment[,'15_4'])) &
              (financialStatment[,'15_5'] != 0 & !is.na(financialStatment[,'15_5']) & !is.null(financialStatment[,'15_5'])) 

trainingData <- which(notNaData & training)
testingData <- which(notNaData & testing)
trainingNrow <- sum(notNaData & training)
testingNrow <- sum(notNaData & testing)

