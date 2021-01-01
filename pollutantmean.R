pollutantmean <- function(directory, pollutant, id = 1:332) {
        
        observations = list.files(path = directory, pattern = ".csv")
        
        for (i in seq(length(observations))) {assign(observations[[i]], i)
        } 
        pollution_total = lapply(X = observations[id], 
                                 FUN = read.csv, header = TRUE)
        
        pollutant_evaluation = do.call(rbind, pollution_total)
        round(mean(pollutant_evaluation[ ,pollutant], na.rm = TRUE),6)
}

dir <- file.path("C:", "Users", "julia", "OneDrive", "Julian", "R", 
                 "Programming Assignment Week 2", "specdata")

##      pollutantmean(dir, "sulfate", 1:10)
##      [1] 4.064128
##      pollutantmean(dir, "nitrate", 70:72)
##      [1] 1.706047
##      pollutantmean(dir, "nitrate", 23)
##      [1] 1.280833