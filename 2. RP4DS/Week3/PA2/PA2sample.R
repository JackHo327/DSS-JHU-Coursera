#preliminary work
getwd()
#dir.create("E:/Coursera/JHU_Data Science/RP4DS/Week3/PA2")
setwd("E:/Coursera/JHU_Data Science/RP4DS/Week3/PA2")

#sample how to caching the Mean of a Vector:    
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4. get the value of the mean

# x= numeric(empty) ----- a container, which is used to hold numbers
makeVector <- function(x = numeric()) {
      #NULL ref:http://www.douban.com/note/283113438/
      #m<-NULL, means M is "unknown", and NULL is not a placeholder as NA|NAN.
      m <- NULL
      set <- function(y) {
            #<<- : normally be used in function, assign a value to a objectunder the environment.
            x <<- y
            m <<- NULL
      }
      #if a function is a very simple one (does not contain a complex structure), it could be write like "function(args) ....(code)"
      get <- function() x
      setmean <- function(mean) m <<- mean
      getmean <- function() m
      list(set = set, get = get,
           setmean = setmean,
           getmean = getmean)
}

cachemean <- function(x, ...) {
      m <- x$getmean()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- mean(data, ...)
      x$setmean(m)
      m
}