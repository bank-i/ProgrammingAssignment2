## R Programming assignment 2: write two functions
## 1. "makeCacheMatrix" - a function that creates a special matrix object that can cache its inverse.
## 2. "cacheSolve" - a function that computes the inverse of a special matrix returned by function 1. If the inverse has already been calculated (and the matrix has not changed), then cachesolve should retrieve the inverse from the cache.
# I. Bank Sept 18th, 2014

## This function creates a special matrix and saves it ('m")

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get <-function()x
        setinversematrix<-function(solve) m<<-solve
        getinversematrix<-function() m
        list (set=set, get=get,setinversematrix=setinversematrix,getinversematrix=setinversematrix)
}      
        




## cacheSolve function searches for cached special matrix "m", and returns inversed 

cacheSolve <- function(x, ...) {
        m <- x$getinversematrix()
        if(!is.null(m)) {
                solve(m)
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinversematrix(m)
        m
}

