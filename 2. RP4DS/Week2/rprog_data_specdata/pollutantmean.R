pollutantmean <- function(directory, pollutant, ids) {
      
      ### 1. Setting directory as wd
      
      setwd("E:/Coursera/JHU_Data Science/RP4DS/Week2/rprog_data_specdata/specdata")
      
      
      
      ##dataset<-read.csv(c(paste("E:/Coursera/JHU_Data Science/RP4DS/Week2/rprog_data_specdata",directory,sep="/"),c("/",paste(id,"csv",sep="."))))
   
      #dataset<-read.csv(paste(paste("E:/Coursera/JHU_Data Science/RP4DS/Week2/rprog_data_specdata",directory,sep="/"),paste(id,"csv",sep="."),sep="/"))
      #file_list <- list.files()
      file_list <- list.files()
      # print(file_list)
      
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
      
#       ###3. Define rows based on ID Vector 
#       
      # print(dataset[["ID"]])
#       dataset <- na.omit(
#             dataset[dataset[["ID"]] %in% ids, ]
#       )
       
#       
#       #dataset
#       ### 4. Calculate means in combination with round() and for the given pollutant  
      round(colMeans(dataset[pollutant],na.rm=TRUE),digits=4)
      
}

