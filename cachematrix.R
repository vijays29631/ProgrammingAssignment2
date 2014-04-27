## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix is a  fucntion that accepts a matrix as input and
# returns a list of length 4, whose individual elements are the 4 fns:
# set, get, setinverse and getinverse. This list is then used by the cacheSolve 
# methdod to return the inverse of the matrix, inverse is computed for a new 
# matrix, while the cached result is returned for a matrix whose inverse was 
# already calculated

## Write a short comment describing this function
#  makeCacheMatrix takes a matrix and returns a list of functions, set,
#  get, setinverse and getinverse. 
#  set sets the matrix value x in the parent global environment to what is passed
#  locally in the set method, also sets the parent gloabl varibale 
#  m(representing the inverse) to null
#  get - gets the matrix from the parent global environment
#  setinverse sets the inverse to m and getinverse returns the inverse m

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
         x <<- y
         m <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse) 
}


## Write a short comment describing this function
## takes the list returned by makeCacheMatrix as an input, calls the 
## getinverse() method to check if the inverse m, was previously computed, 
## if so, returns the already computed value, if not, calls the get() method to
## get the value of the new matirx set in the parent global variable x, the 
## return value is then passed to the solve() method to find the inverse and
## store in the variable m. setinverse() is called on m, which sets the 
## parent gloabl variable m (representing the inverse). This value of m from 
## the parnet environment is finally returned as the required matrix 
## inverse m
  
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
   
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached inverse data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
