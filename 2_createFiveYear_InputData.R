
financialStatment[is.na(financialStatment)] <- 0
#financialStatment[,83] <-  financialStatment[,83] - financialStatment[,79]# - financialStatment[,'investment']#financialStatment[,43]-financialStatment[,27]-financialStatment[,28]-financialStatment[,30]-financialStatment[,31]+financialStatment[,36]-financialStatment[,44]-financialStatment[,'investment']
colnameTemp <- c(colnames(financialStatment)[1:(ncol(financialStatment)-1)], 'freeCashFlow')
colnames(financialStatment) <- colnameTemp

cashEquivalent <- array(0, dim = c(nrow(financialStatment), 6))
colnameTemp <- c(colnames(financialStatment), 
                 'futureFCF', 'pastFCF_1', 'pastFCF_2', 'pastFCF_3', 'pastFCF_4', 'pastFCF_5')
financialStatment <- cbind(financialStatment, cashEquivalent)
colnames(financialStatment) <- colnameTemp
rm(cashEquivalent)

financialStatment <- financialStatment[order(financialStatment[,2], decreasing = TRUE),]

uniqueCompany <- unique(financialStatment[,1])
for(symbol in uniqueCompany) {
  FCF <- financialStatment[financialStatment[,1] == symbol, 'freeCashFlow']
  if(length(FCF) > 6) {
    FCF <- filter(FCF/6, rep(1, 6))
    FCF[is.na(FCF)] <- 0
    financialStatment[financialStatment[,1] == symbol, 'futureFCF'] <- c(0, FCF[1:(length(FCF)-1)])
    financialStatment[financialStatment[,1] == symbol, 'pastFCF_1'] <- c(FCF[2:length(FCF)], 0)
    financialStatment[financialStatment[,1] == symbol, 'pastFCF_2'] <- c(FCF[3:length(FCF)], 0, 0)
    financialStatment[financialStatment[,1] == symbol, 'pastFCF_3'] <- c(FCF[4:length(FCF)], 0, 0, 0)
    financialStatment[financialStatment[,1] == symbol, 'pastFCF_4'] <- c(FCF[5:length(FCF)], 0, 0, 0, 0)
    financialStatment[financialStatment[,1] == symbol, 'pastFCF_5'] <- c(FCF[6:length(FCF)], 0, 0, 0, 0, 0)
  }
}

getFiveYearsDelay <- function(columnName, finalcialStat) {
  
  past5 <- array(0, dim = c(nrow(finalcialStat), 6))
  colnameTemp <- c(colnames(finalcialStat), 
                   paste0(columnName, '_1'), paste0(columnName, '_2'), paste0(columnName, '_3'), paste0(columnName, '_4'), paste0(columnName, '_5'), 
                   paste0(columnName, '_6'))
  finalcialStat <- cbind(finalcialStat, past5)
  colnames(finalcialStat) <- colnameTemp
  
  finalcialStat <- finalcialStat[order(finalcialStat[,2], decreasing = TRUE),]
  uniqueCompany <- unique(finalcialStat[,1])
  
  for(symbol in uniqueCompany) {
    pastData <-finalcialStat[finalcialStat[,1] == symbol, columnName]
    if(length(pastData) > 6) {
      finalcialStat[finalcialStat[,1] == symbol, paste0(columnName, '_1')] <- c(pastData[2:length(pastData)], 0)
      finalcialStat[finalcialStat[,1] == symbol, paste0(columnName, '_2')] <- c(pastData[3:length(pastData)], 0, 0)
      finalcialStat[finalcialStat[,1] == symbol, paste0(columnName, '_3')] <- c(pastData[4:length(pastData)], 0, 0, 0)
      finalcialStat[finalcialStat[,1] == symbol, paste0(columnName, '_4')] <- c(pastData[5:length(pastData)], 0, 0, 0, 0)
      finalcialStat[finalcialStat[,1] == symbol, paste0(columnName, '_5')] <- c(pastData[6:length(pastData)], 0, 0, 0, 0, 0)
      finalcialStat[finalcialStat[,1] == symbol, paste0(columnName, '_6')] <- c(pastData[7:length(pastData)], 0, 0, 0, 0, 0, 0)
    }
  }
  return(finalcialStat)
}

for(i in c(3:82)) {
  cat('Making data', i, '\n')
  financialStatment <- getFiveYearsDelay(i, financialStatment)
}
