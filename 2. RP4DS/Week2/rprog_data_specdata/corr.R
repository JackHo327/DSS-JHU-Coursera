corr <- function(directory, threshold = 0) {
      ## 'directory' is a character vector of length 1 indicating
      ## the location of the CSV files
      setwd(paste("E:/Coursera/JHU_Data Science/RP4DS/Week2/rprog_data_specdata",directory,sep="/"))
      
      
      
      file_list <- list.files()
      
      for(file in file_list) {
            
            if(!exists('dataset')) {
                  dataset <- read.csv(file)
            }
            
            if(exists('dataset')) {
                  temp_dataset <- read.csv(file)
                  dataset <- rbind(dataset, temp_dataset)
                  rm(temp_dataset)
            }
      }
     
      ## 'threshold' is a numeric vector of length 1 indicating the
      ## number of completely observed observations (on all
      ## variables) required to compute the correlation between
      ## nitrate and sulfate; the default is 0
      
      ## Return a numeric vector of correlations
      ## NOTE: Do not round the result!
}