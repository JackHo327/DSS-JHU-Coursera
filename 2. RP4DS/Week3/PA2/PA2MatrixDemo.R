## preliminary work
## getwd()
## dir.create("E:/Coursera/JHU_Data Science/RP4DS/Week3/PA2")
## setwd("E:/Coursera/JHU_Data Science/RP4DS/Week3/ProgrammingAssignment2")
## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object that can cache its inverse.
## e.g. you could set a matrix(runif(25),5,5) or some else (just keep it as a square matrix)----  t<-makeCacheMatrix(matrix(runif(25),5,5))
## 
## (x= matrix(empty) ----- a container, which is used to hold a matrix.)
makeCacheMatrix <- function(x = matrix()) {
      
      ## m<-NULL, means M is "unknown", and NULL is not a placeholder as NA|NAN.
      m<-NULL
      set<-function(y){
            x<<-y
            m<-NULL
      }
      get<-function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## e.g. since you have assgin the function makeCacheMatrix() to t, and t has been equal to makeCacheMatrix(matrix(runif(25),5,5)), now you can just stimulate the codes: cacheSolve(t), then you will see the inverse. And if you call cacheSolve(t) again, since there is a preliminary "if" check, there will be a message like "Dude! you just got the cached matrix!!" come out and then the cached matrix will also be displayed.
## enjoy your coding :)
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse ()
      if(!is.null(m)) {
            message("Dude! you just got the cached matrix!!")
            return(m)
      }
      data <- x$get()
      ## call function "slove()" to calculate the inverse value of the matrix.
      m <- solve(data, ...)
      x$setinverse(m)
      m
      
}
