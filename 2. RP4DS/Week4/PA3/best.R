
setwd("E:/Coursera/JHU_Data Science/RP4DS/Week4/PA3")
# outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#Function best:
#The function reads the outcome-of-care-measures.csv file and returns a character vector with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome in that state.

best <- function(state, outcome) {
      ## Read outcome data
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      ## Check that state and outcome are valid
      statedata <- as.vector(unique(data$State))
      
      otm <- c("heart attack", "heart failure", "pneumonia")
      if(all(is.element(state,statedata)) == FALSE & all(is.element(outcome,otm))){
            
            stop("invalid state")
      
            }
      else if(all(is.element(outcome,otm)) == FALSE & all(is.element(state,statedata))){
            stop("invalid outcome")
      
            }
      else if(all(is.element(state,statedata)) & all(is.element(outcome,otm))){
            #extract the specific state 
            
            specstate <- subset(data,data$State == state)
            
            if(outcome == otm[1]){
                  
                  heartattack_na <- as.numeric(specstate[,11])
                  
                  heartattack_naomit <- na.omit(heartattack_na)
                  min <- min(heartattack_naomit)
                  htatk_loc <- which(heartattack_na == min)
                  locationvec <- specstate[htatk_loc, ]
                  hosname <- locationvec[,2]
                  # print(hosname)
            }
            if(outcome == otm[2]){
                  
                  heartattack_na <- as.numeric(specstate[,17])
                  heartattack_naomit <- na.omit(heartattack_na)
                  min <- min(heartattack_naomit)
                  htatk_loc <- which(heartattack_na == min)
                  locationvec <- specstate[htatk_loc, ]
                  hosname <- locationvec[,2]
                  # print(hosname)
            }
            if(outcome == otm[3]){
                  
                  heartattack_na <- as.numeric(specstate[,23])
                  heartattack_naomit <- na.omit(heartattack_na)
                  min <- min(heartattack_naomit)
                  htatk_loc <- which(heartattack_na == min)
                  locationvec <- specstate[htatk_loc, ]
                  hosname <- locationvec[,2]
                  
            }
            
            
            
      }
      ## Return hospital name in that state with lowest 30-day death
      ## rate
      print(hosname)
      
}