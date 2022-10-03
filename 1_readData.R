library(readxl)

financialStatment_2015 <- read_excel("path/to/financialStatment_2015.xlsx", 
                                     col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric"), na = "0")

financialStatment_2010 <- read_excel("path/to/financialStatment_2010.xlsx", 
                                     col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric"), na = "0")

financialStatment_2005 <- read_excel("path/to/financialStatment_2005.xlsx", 
                                     col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric"), na = "0")

financialStatment_2000 <- read_excel("path/to/financialStatment_2000.xlsx", 
                                     col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric"), na = "0")

financialStatment_1995 <- read_excel("path/to/financialStatment_1995.xlsx", 
                                     col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric"), na = "0")

financialStatment_1990 <- read_excel("path/to/financialStatment_1990.xlsx", 
                                     col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric"), na = "0")

financialStatment_1985 <- read_excel("path/to/financialStatment_1985.xlsx", 
                                     col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric","numeric", "numeric"), na = "0")

financialStatment <- NULL
financialStatment <- rbind(financialStatment, financialStatment_2015)
financialStatment <- rbind(financialStatment, financialStatment_2010)
financialStatment <- rbind(financialStatment, financialStatment_2005)
financialStatment <- rbind(financialStatment, financialStatment_2000)
financialStatment <- rbind(financialStatment, financialStatment_1995)
financialStatment <- rbind(financialStatment, financialStatment_1990)
financialStatment <- rbind(financialStatment, financialStatment_1985)

rm(financialStatment_2015)
rm(financialStatment_2010)
rm(financialStatment_2005)
rm(financialStatment_2000)
rm(financialStatment_1995)
rm(financialStatment_1990)
rm(financialStatment_1985)


financialStatment[,1] <- sapply(unlist(financialStatment[,1]), function(symbol) {
  return(unlist(strsplit(unlist(symbol), " "))[1])
})


interestRate <- read_excel("path/to/interestRate.xlsx", 
                           col_types = c("text", "blank", "blank", 
                                         "blank", "blank", "blank", "blank", 
                                         "blank", "blank", "blank", "numeric", 
                                         "blank", "blank", "blank", "blank", 
                                         "blank", "blank", "blank", "blank", 
                                         "blank", "blank"), na = "0")


X2002 <- read_excel("path/to/2002.xlsx", 
                           col_types = c("text", "text", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric"))

X2003 <- read_excel("path/to/2003.xlsx", 
                    col_types = c("text", "text", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"))

X2004 <- read_excel("path/to/2004.xlsx", 
                    col_types = c("text", "text", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"))

X2005 <- read_excel("path/to/2005.xlsx", 
                    col_types = c("text", "text", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"))

X2006 <- read_excel("path/to/2006.xlsx", 
                    col_types = c("text", "text", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"))

X2007 <- read_excel("path/to/2007.xlsx", 
                    col_types = c("text", "text", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"))

X2008 <- read_excel("path/to/2008.xlsx", 
                    col_types = c("text", "text", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"))

X2009 <- read_excel("path/to/2009.xlsx", 
                    col_types = c("text", "text", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"))

X2010 <- read_excel("path/to/2010.xlsx", 
                    col_types = c("text", "text", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"))

X2011 <- read_excel("path/to/2011.xlsx", 
                    col_types = c("text", "text", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"))

X2012 <- read_excel("path/to/2012.xlsx", 
                    col_types = c("text", "text", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"))

X2013 <- read_excel("path/to/2013.xlsx", 
                    col_types = c("text", "text", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"))

X2014 <- read_excel("path/to/2014.xlsx", 
                    col_types = c("text", "text", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"))

X2015 <- read_excel("path/to/2015.xlsx", 
                    col_types = c("text", "text", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"))

X2016 <- read_excel("path/to/2016.xlsx", 
                    col_types = c("text", "text", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"))

X2017 <- read_excel("path/to/2017.xlsx", 
                    col_types = c("text", "text", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"))

X2018 <- read_excel("path/to/2018.xlsx", 
                    col_types = c("text", "text", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"))

priceData <- NULL
priceData <-rbind(priceData, X2002)
priceData <-rbind(priceData, X2003)
priceData <-rbind(priceData, X2004)
priceData <-rbind(priceData, X2005)
priceData <-rbind(priceData, X2006)
priceData <-rbind(priceData, X2007)
priceData <-rbind(priceData, X2008)
priceData <-rbind(priceData, X2009)
priceData <-rbind(priceData, X2010)
priceData <-rbind(priceData, X2011)
priceData <-rbind(priceData, X2012)
priceData <-rbind(priceData, X2013)
priceData <-rbind(priceData, X2014)
priceData <-rbind(priceData, X2015)
priceData <-rbind(priceData, X2016)
priceData <-rbind(priceData, X2017)
priceData <-rbind(priceData, X2018)

priceData[,1] <- sapply(unlist(priceData[,1]), function(symbol) {
  return(unlist(strsplit(unlist(symbol), " "))[1])
})

rm(X2002)
rm(X2003)
rm(X2004)
rm(X2005)
rm(X2006)
rm(X2007)
rm(X2008)
rm(X2009)
rm(X2010)
rm(X2011)
rm(X2012)
rm(X2013)
rm(X2014)
rm(X2015)
rm(X2016)
rm(X2017)
rm(X2018)

