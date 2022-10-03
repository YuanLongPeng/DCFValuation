
backTest <- NULL

if(is.null(financialStatment$backTest)) {
  backTest <- array(NA, dim = c(nrow(financialStatment), 1))
  colnameTemp <- c(colnames(financialStatment), 'backTest')
  financialStatment <- cbind(financialStatment, backTest)
}

distinMonth <- sort(unique(sapply(financialStatment[,2], substr, 1, 7)))

allSubDate <- sapply(priceData[,2], function(x){
  substr(x, 1, 7)
})
#distinAllSubDate <- sort(unique(allSubDate))

for(i in 1:nrow(financialStatment)) {#testingData
  realQuarter <- distinMonth[which(financialStatment[i, 2] == distinMonth) + 1]
  matchData <- financialStatment[i, 1] == priceData[,1] & allSubDate == realQuarter
  if(sum(matchData, na.rm = TRUE) > 0) {
    price <- priceData[matchData, 3]
    financialStatment[i, 'backTest'] <- head(price, 1)
  }
  print(paste0(i, ', Month: ',financialStatment[i, 2],', price: ', financialStatment[i, 'backTest']))
}
