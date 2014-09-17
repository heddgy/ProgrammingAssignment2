## creating some magic matrix able to store cached inverse version

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      ## quick setting new matrix via $set() function
      ## cached inverse nullified accordingly
      set <- function(y) {
            x <<- y
            m <<- NULL
            message("cache data cleaned")
      }
      ## return our matrix via $get() function
      get <- function() x
      ## setting and returning cached inverse matricies
      setInverse <- function(solve) m <<- solve
      getInverse <- function() m
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## creating function for quick solve in case of already cached inverse matrix

cacheSolve <- function(x, ...) {
      ## if inverse already cached, return it w/o additional calculations
      m <- x$getInverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      ## if not, when do all additional calculations and store inverse in cache
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)
      message("cache data created")
      m
}
