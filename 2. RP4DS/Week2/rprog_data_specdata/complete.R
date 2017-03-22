complete <- function(directory, id) {
      ## 'directory' is a character vector of length 1 indicating
      ## the location of the CSV files
      setwd("E:/Coursera/JHU_Data Science/RP4DS/Week2/rprog_data_specdata")
      ## 'id' is an integer vector indicating the monitor ID numbers
      ## to be used
      file(directory)
      dataset <- read.csv(file)
      print(dataset)
      # file_list <- list.files()
      
#       for(file in file_list) {
#             
#             if(!exists('dataset')) {
#                   dataset <- read.csv(file)
#                   print(dataset)
#             }
            
#             if(exists('dataset')) {
#                   temp_dataset <- read.csv(file)
#                   dataset <- rbind(dataset, temp_dataset)
#                   rm(temp_dataset)
#             }
      # }
      # dataset <- na.omit(
            # dataset[dataset[["ID"]] %in% id, ]
      # )
      
      ## Return a data frame of the form:
      ## id nobs
      ## 1  117
      ## 2  1041
      ## ...
      # data.frame(id=id,nobs=nrow(dataset))
      ## where 'id' is the monitor ID number and 'nobs' is the
      ## number of complete cases
}