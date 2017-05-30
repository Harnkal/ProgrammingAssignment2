## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## With these function it is possible to cache the inverse of a matrix, and 
## retrive its value when needed.

## 'makeCacheMatrix' creates a special "matrix" object that can cache its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL # define the inverse matrix as empty
      
      # 'set.matrix' caches the matrix that must be solved
      set.matrix <- function(y) {
            x <<- y # cache the matrix given as argument
            inv <<- NULL #erase the currently cached inverse matrix
      }
      
      # 'get.matrix' gets the matrix currently cached
      get.matrix <- function() {
            x 
      }
      
      # 'set.inverse' caches the inverse matrix
      set.inverse <- function(inverse) {
            inv <<- inverse 
      }
      
      # 'get.inverse' gets the inverse matrix currently cached
      get.inverse <- function() {
            inv
      }
      
      # compiles the functions in a list
      list(set.matrix = set.matrix, get.matrix = get.matrix, 
           set.inverse = set.inverse, get.inverse = get.inverse)
}


## 'cacheSolve' computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then the 'cacheSolve' should retrieve the inverse from the 
## cache.

# '...' removed. It makes the function more reliable as through the '...'
# argument, the function can be used to solve the matrix for other matrices of
# the same dimenssion and the user may be lead to use this function for this
# end. This may look useful, however, if the user tries to solve the function
# for another function and there already is a solved matrix stored in the
# cache, the function will return the cached matrix, missleading the user into
# thinking that the result is the solution he was looking for. Wihtout the
# '...' this function can only be used to solve the matrix for an identity
# matrix wich will result in the inverse matrix. For more information see the
# documentation for solve().

cacheSolve <- function(x) {
      inv <- x$get.inverse() # retrieve the currently cached inverse matrix
      
      # Test if the inverse matrix has already been calculated
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv) #return the cached version if it is not NULL
      }
      
      data <- x$get.matrix() #retrieves the matrix created by 'makeCacheMatrix'
      inv <- solve(data) #solves the matrix for identity (returns the inverse)
      x$set.inverse(inv) #cache solution
      inv #return solution
}
