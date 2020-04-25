## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {## define the argument with the deault mode of matrix
    inv<-NULL ## set inv as NULL first, inv will hold value of matrix inverse
    set<-function(y){ ## create the set function to assign new value of matrix in parent environment
        x<<-y
        inv<-NULL ## inv will be rest to NULL if there is a new matrix
    }
    get<-function()x ## create the get function which returns the value of matrix argument
    setinverse<-function(inverse)inv<<-inverse ## assign value of inv in parent environment
    getinverse<-function()inv  ## get the value of inv where it is called
    list(set=set, 
         get=get,
         setinverse=setinverse,
         getinverse=getinverse)   ## used to refer to the function with operator$
}


## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<-x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data,...)
    x$setinverse(inv)
    inv 
}
