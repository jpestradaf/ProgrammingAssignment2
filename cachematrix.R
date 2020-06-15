## Functions to avoid re-calculating the inverse of a matrix
## when it has been already calculated...

## The next function creates functions to:
# -Set a matrix   -Get the value of a matrix
# -Set the inverse value of a matrix  -Get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
      m_inv <- NULL
      set <- function(y){
            x <<- y
            m_inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) m_inv <<- inverse
      getInverse <- function() m_inv 
      list(set = set, get = get, 
           setInverse = setInverse, 
           getInverse = getInverse)
}


## Returns the inverse, either by getting it from cache or by calculating it

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m_inv <- x$getInverse()
      if(!is.null(m_inv)){
            message("getting cached data")
            return(m_inv)
      }
      mat <- x$get()
      m_inv <- solve(mat,...)
      x$setInverse(m_inv)
      m_inv
}
