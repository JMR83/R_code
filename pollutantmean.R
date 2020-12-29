pollutantmean <- function(directory, pollutant, id = 1:332){
        setwd("C:/Users/Julian.Reber/Desktop/Julian/R/Programming Assignment Week 2/specdata")
        directory = getwd()
        observations = list.files(path = directory, pattern = ".csv")
                for (i in 1:length(observations))   {assign(observations[[i]], i)
                } 
        pollution_total = lapply(observations[id], read.csv, header = TRUE)
        pollutant_evaluation= do.call(rbind, pollution_total)
        round(mean(pollutant_evaluation[,pollutant], na.rm = T),6)
}


##      pollutantmean("specdata", "sulfate", 1:10)
##      [1] 4.064128
##      pollutantmean("specdata", "nitrate", 70:72)
##      [1] 1.706047
##      pollutantmean("specdata", "nitrate", 23)
##      [1] 1.280833
