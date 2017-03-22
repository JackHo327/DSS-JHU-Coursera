setwd("E:/Coursera/JHU_Data Science/RP4DS/Week4/PA3")

## The function returns a 2-column data frame containing the hospital in each state that has the ranking specified in num.

rankall1 <- function(outcome = "heart failure", num = 1) {
      ## Read outcome data
      
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      statedata <- as.vector(unique(data$State))
      statedatavec <- as.vector(statedata[order(statedata)])
      otm <- c("heart attack", "heart failure", "pneumonia")
      ## check if outcome is valid
      if(all(is.element(outcome,otm))){
            ## For each state, find the hospital of the given rank
            for(statename in statedatavec){
                  
                  specstate <- subset(data,data$State == statename)
                  
                        if(outcome == otm[2]){
                              if(!exists('outputlist')){
                              specstatedata_na <- cbind(specstate[,2],specstate[,7],specstate[,17])
                              specstatedata_naomit <- subset(specstatedata_na,specstatedata_na[,3] != "Not Available")
                              specstatedatanew <- data.frame(hospital.Name = specstatedata_naomit[,1],Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure = as.numeric(specstatedata_naomit[,3]))
                              seq <- order(specstatedatanew[,2],specstatedatanew[,1])
                              hosnamelist <- specstatedatanew[seq, ]
                              outputlist  <- cbind(hosnamelist[num,1:2],statename)  
                              }
                        if(exists('outputlist')){
                              specstatedata_na <- cbind(specstate[,2],specstate[,7],specstate[,17])
                              specstatedata_naomit <- subset(specstatedata_na,specstatedata_na[,3] != "Not Available")
                              specstatedatanew <- data.frame(hospital.Name = specstatedata_naomit[,1],Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure = as.numeric(specstatedata_naomit[,3]))
                              seq <- order(specstatedatanew[,2],specstatedatanew[,1])
                              hosnamelist <- specstatedatanew[seq, ]
                              hosname1 <- cbind(hosnamelist[num,1:2],statename)
                              outputlist <- rbind(outputlist,hosname1)
                              rm(hosname1)
                        }
                  }  
            } 
            fram <- data.frame(hospital = outputlist[,1],state = outputlist[,3])
      }
      
      fram
      
      
      ## Return a data frame with the hospital names and the (abbreviated) state name
}