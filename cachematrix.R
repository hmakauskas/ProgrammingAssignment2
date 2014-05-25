## Lexical Scoping: the values of free variables are searched for in the environment in which the function was de???ned

# The following function create the cached variables to be used to inverse a square invertible matrix
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL # initialize a NULL variable
  set <- function(y) {
    # Here we're using the <<- super assignment operator in this subfunction: 
    # this allows the subfunction to alter the x and m variables in the parent environment, 
    # the makeCacheMatrix() function itself
    x <<- y 
    m <<- NULL 
  }
  get <- function() x # The get() subfunction grabs whatever matrix might be stored in x and returns it.
  setcache <- function(cache) m <<- cache # it takes a matrix passed into it and stores it in m, the cache.
  getcache <- function() m # Return cached matrix.
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)

}

## The following function calculates the inverse (solve) of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse matrix from the cache
## and skips the computation. Otherwise, it calculates the inverse using solve of the data and sets the value of the solve in the cache via the setcache function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getcache() # Gets the matrix from the cache
  if(!is.null(m)) { # it check to see if the returned cache has anything in it. If so, we print the "getting cached data" message and return the cached matrix.
    message("getting cached data")
    return(m)
  }
  
  # If there cache is empty #  
  data <- x$get() # It uses the subfunction 'get()' to get a matrix and place it in a local variable data.
  m <- solve(data, ...) # Here it gets the inverse matrix using the solve function and put it into a local variable.
  x$setcache(m) # It stores the inverse matrix into the cache.
  m # Here it prints the variable.
}
