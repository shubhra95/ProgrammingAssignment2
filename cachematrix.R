## A special "matrix" object is created below which stores a matrix and caches its inverse.
## All this is achieved by this makeCacheMatrix function.

makeCacheMatrix <- function(x = matrix()) 
{inv <- NULL
 set <- function(y) 
     {x <<- y
      inv <<- NULL}

          get <- function() x
          setInverse <- function(inverse) inv <<- inverse
          getInverse <- function() inv
          list(set = set,get = get,setInverse = setInverse,
               getInverse = getInverse)
}


##The cacheSolve function below computes  inverse of the special "matrix" created by the function defined before i.e. 
## makeCacheMatrix . If the inverse was already  calculated and there is no chang in the matrix, 
## then it  retrieves the inverse from the cache and displays a message "getting cached data"

cacheSolve <- function(x, ...) 

{## This returns a matrix that is the inverse of matrix 'x'
inv <- x$getInverse()
 if (!is.null(inv)) 
    { message("getting cached data")
      return(inv)}

        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

#USAGE EXAMPLE
# > x <- cbind(c(2, 2), c(3, 2))
# > me<- makeCacheMatrix(x)
# > me$get()
#       [,1] [,2]
# [1,]    2    3
# [2,]    2    2
# > cacheSolve(me)
#       [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0
# > cacheSolve(me)
# getting cached data
#       [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0
# > me$getInverse()
#       [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0
