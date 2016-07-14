# Tests the code on first 13 stocks

stockVector = list()
BestPrediction = vector()

stockVector[[1]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/ADMP.csv", sep=",", header=TRUE)
stockVector[[2]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/AMS.csv", sep=",", header=TRUE)
stockVector[[3]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/CGI.csv", sep=",", header=TRUE)
stockVector[[4]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/CLWT.csv", sep=",", header=TRUE)
stockVector[[5]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/DAC.csv", sep=",", header=TRUE)
stockVector[[6]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/DGLY.csv", sep=",", header=TRUE)
stockVector[[7]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/DRD.csv", sep=",", header=TRUE)
stockVector[[8]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/ECTE.csv", sep=",", header=TRUE)
stockVector[[9]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/EVRI.csv", sep=",", header=TRUE)
stockVector[[10]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/HGG.csv", sep=",", header=TRUE)
stockVector[[11]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/HMNY.csv", sep=",", header=TRUE)
stockVector[[12]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/ISNS.csv", sep=",", header=TRUE)
stockVector[[13]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/KONA.csv", sep=",", header=TRUE)
stockVector[[14]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/REN.csv", sep=",", header=TRUE)
stockVector[[15]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/TCCO.csv", sep=",", header=TRUE)
stockVector[[16]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/RAD.csv", sep=",", header=TRUE)
stockVector[[17]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/XPLR.csv", sep=",", header=TRUE)
stockVector[[18]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/IIN.csv", sep=",", header=TRUE)
stockVector[[19]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/CO.csv", sep=",", header=TRUE)
stockVector[[20]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/AMCN.csv", sep=",", header=TRUE)
stockVector[[21]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/OZM.csv", sep=",", header=TRUE)
stockVector[[22]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/AEG.csv", sep=",", header=TRUE)
stockVector[[23]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/TI.csv", sep=",", header=TRUE)
stockVector[[24]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/KVHI.csv", sep=",", header=TRUE)
stockVector[[25]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/JRJC.csv", sep=",", header=TRUE)
stockVector[[26]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/PZN.csv", sep=",", header=TRUE)
stockVector[[27]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/NVDQ.csv", sep=",", header=TRUE)
stockVector[[28]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/SPCB.csv", sep=",", header=TRUE)
stockVector[[29]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/FCEL.csv", sep=",", header=TRUE)
stockVector[[30]] = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/GASS.csv", sep=",", header=TRUE)



# Populate the Best Error Vector
  for(i in 1:length(stockVector))
{
    cat("\n*** predicting stock number: ",i) ;
   BestPrediction[i] = findBestPrediction(stockVector[[i]])
   
}
cat("\n\n!!! winning models:", BestPrediction);
cat("\n!!! most frequent model:", names(which.max(table(BestPrediction))))

events <- data.frame(type = factor(BestPrediction))
table(events$type)
