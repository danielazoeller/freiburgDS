setwd("~/Documents/MDS/Projekte/CORD")

library(readr)
Input_A <- read.csv("~/Documents/MDS/Projekte/CORD/Input_data_blockA.csv", header=TRUE, sep=";")


names(Input_A)[1] <- c("id")
Input_A$id <- seq(1,length(Input_A$id))
Input_A$Alter <- ceiling( as.double( as.Date( Sys.time() ) - as.Date(Input_A$AngabeGeburtsdatum, "%d.%m.%Y") ) / 365.25 )
Input_A$D1 <- as.factor(Input_A$AngabeDiag1)
Input_A$Sex <- as.factor(Input_A$AngabeGeschlecht)

Input_A_result <- Input_A[,c('id','AngabeDiag1','D1', 'TextDiagnose1',
                          'Alter',
                          'AngabeGeschlecht', 'Sex')]

set.seed(10)
resA <- split(Input_A_result, c(sample(rep(1:2, (nrow(Input_A)/2)-0.5)),1))

#Server 101
upload_opal(resA[[2]], "administrator", "datashield_test&", "http://192.168.56.101:8080", "CORD4", "BlockA")

#Server 100
upload_opal(resA[[1]], "administrator", "datashield_test&", "http://192.168.56.100:8080", "CORD4", "BlockA")

save(resA, file="DatenBlockA.RData")
