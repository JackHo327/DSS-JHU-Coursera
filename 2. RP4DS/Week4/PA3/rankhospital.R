setwd("E:/Coursera/JHU_Data Science/RP4DS/Week4/PA3")


rankhospital <- function(state, outcome, num = "best") {
      ## Read outcome data
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      ## Check that state and outcome are valid
      statedata <- as.vector(unique(data$State))
      
      otm <- c("heart attack", "heart failure", "pneumonia")
      if(all(is.element(state,statedata)) == FALSE & all(is.element(outcome,otm))){
            ## stop stops execution of the current expression and executes an error action.
            stop("invalid state")
            
      }
      else if(all(is.element(outcome,otm)) == FALSE & all(is.element(state,statedata))){
            
            stop("invalid outcome")
            
      }
      else if(all(is.element(state,statedata)) & all(is.element(outcome,otm))){
            ##extract the specific state 
            
            specstate <- subset(data,data$State == state)
            
            ## "heart attack"
            if(outcome == otm[1]){
                  specstatedata_na <- cbind(specstate[,2],specstate[,11])
                  specstatedata_naomit <- subset(specstatedata_na,specstatedata_na[,2] != "Not Available")
                  specstatedatanew <- data.frame(Hospital.Name = specstatedata_naomit[,1],Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure = as.numeric(specstatedata_naomit[,2]))
                  seq <- order(specstatedatanew[,2],specstatedatanew[,1])
                  hosnamelist <- specstatedatanew[seq, ]
                  
                  if(num == "best"){
                        num <- 1
                  }
                  else if(num == "worst"){
                        num <- length(specstatedata_naomit[,2])
                  }
                  hosname <- hosnamelist[num,1]
                  if(as.numeric(num) > length(specstatedata_naomit[,2])){
                        hosname <- c(NA)
                  }
            }
            
            ##"heart failure"
            if(outcome == otm[2]){
                  specstatedata_na <- cbind(specstate[,2],specstate[,17])
                  specstatedata_naomit <- subset(specstatedata_na,specstatedata_na[,2] != "Not Available")
                  specstatedatanew <- data.frame(Hospital.Name = specstatedata_naomit[,1],Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure = as.numeric(specstatedata_naomit[,2]))
                  seq <- order(specstatedatanew[,2],specstatedatanew[,1])
                  hosnamelist <- specstatedatanew[seq, ]
                  
                  if(num == "best"){
                        num <- 1
                  }
                  else if(num == "worst"){
                        num <- length(specstatedata_naomit[,2])
                  }
                  hosname <- hosnamelist[num,1]
                  if(as.numeric(num) > length(specstatedata_naomit[,2])){
                        hosname <- c(NA)
                  }
            }
            
            ##"pneumonia"
            if(outcome == otm[3]){
                  specstatedata_na <- cbind(specstate[,2],specstate[,23])
                  specstatedata_naomit <- subset(specstatedata_na,specstatedata_na[,2] != "Not Available")
                  specstatedatanew <- data.frame(Hospital.Name = specstatedata_naomit[,1],Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure = as.numeric(specstatedata_naomit[,2]))
                  seq <- order(specstatedatanew[,2],specstatedatanew[,1])
                  hosnamelist <- specstatedatanew[seq, ]
                  
                  if(num == "best"){
                        num <- 1
                  }
                  else if(num == "worst"){
                        num <- length(specstatedata_naomit[,2])
                  }
                  hosname <- hosnamelist[num,1]
                  if(as.numeric(num) > length(specstatedata_naomit[,2])){
                        hosname <- c(NA)
                  }
            } 
      }
      ## Return hospital name in that state with lowest 30-day death rate
      print(as.character(hosname))
}