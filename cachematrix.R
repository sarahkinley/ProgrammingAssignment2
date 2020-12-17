## Sarah Kinley

## Create a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        verse <- NULL ## Set the value of the vector
        set <- function(y) {
                x <<- y
                verse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) verse <<- inverse
        getinverse <- function() verse
        list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
}

## Compute the inverse of the special 'matrix' returned my makeCacheMatrix

cacheSolve <- function(x, ...) {
        verse <- x$getinverse()
        if(!is.null(verse)){
                message("getting cached data")
                return(verse)
        }
        data <-x$get()
        verse <- solve(data) %% data
        x$setinverse(verse)
        verse
}
