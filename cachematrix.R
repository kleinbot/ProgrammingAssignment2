## This pair of functions caches the inverse of a matrix.

## The makeCacheMatrix function creates a special "matrix", which 
## is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv_matx <- NULL
        
        set <- function(a_mtx_to_cache) {
                x <<- a_mtx_to_cache
                inv_matx <<- NULL
        }
        
        get <- function() x
        
        set_caching_of_inv_matrix <- function(inverse_matrix) {
                inv_matx <<- inverse_matrix
        }
        
        get_inv_matrix <- function() inv_matx
        
        list(set = set, get = get,
             set_caching_of_inv_matrix = set_caching_of_inv_matrix,
             get_inv_matrix = get_inv_matrix)
}


## The cacheSolve function calculates the inverse of the special "matrix" 
## created with the above function. 
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value
## of the inverse in the cache via the set_caching_of_inv_matrix function.

cacheSolve <- function(x, ...) {
        
        inv_matrix <- x$get_inv_matrix()
        
        if (!is.null(inv_matrix)) {
                message("getting cached matrix")
                return(inv_matrix) 
        }
        
        original_matrix <- x$get()
        inv_matrix <- solve(original_matrix,...)
        x$set_caching_of_inv_matrix(inv_matrix)
        inv_matrix
}
