<<<<<<< HEAD
## R Progamming assignment 2 DJMcClellan1966

## sources:
## assignment examples
##google search
##https://class.coursera.org/rprog-009/forum/thread?thread_id=164

## this function creates a special "matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {         ##defines makeCacheMatrix function
    mCM <- Null                                     ## assigning Null value to variable
    set <- function(y) {
        x <<- y                                     ## looks in global environment for y and when finds set to value of x
        mCM <<- NULL                                ## and null set to variable assigned to different
    }                                               ## value in memory.
    
    get <- function() x                             ## call x function
    setinverse <- function(inverse) mCM <<- inverse     ##searched for inverse and sets to inverse of mCM
    getinverse<- function() mCM                         ##retrieves value of cache mCM
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}



## cacheSolve receieves a variable set as matrix defined by makeCacheMatrix

cacheSolve <- function(x, ...) {                   ## calls makeCacheMatrix
    cS <- x$getinverse()                            ## checks environment to find cS
    if(!is.null(cS)) {                              ## checks to see if cS is null
        message("getting cached data")              ## if cS is not null returns message
        return(cS)
    }
    data <- x$get()                                 ##calls original x matrix from makeCacheMatrix
    cS <- solve(data)                               ## solve inverts x matrix
    x$setinverse(cS)
    inv                                                 ## evaluates cS to check if value is not Null
}
=======
>>>>>>> FETCH_HEAD

