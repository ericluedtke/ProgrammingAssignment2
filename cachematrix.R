## This pair of functions takes a matrix, caches it, then uses the cached matrix
## to compute the inverse of the matrix. Please not it requires a square matrix.

## This first function takes a matrix and creates a list of functions that
## can be used to set and then retrieve a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## Creates object m as a null (empty) object
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## Creates the set function within makeCacheMatrix, assigns value y to x
        ## and continues with m as NULL
        get <- function() x
        ## Creates the get function, which allows for retrieval of x
        set_matrix_inverse <- function(matrix_inverse) m <<- matrix_inverse
        ## Sets the matrix inverse, assigns the inverse to m in parent dir
        get_matrix_inverse <- function() m
        ## Gets the matrix inverse by pulling up m
        list(set = set, get = get,
             set_matrix_inverse = set_matrix_inverse,
             get_matrix_inverse = get_matrix_inverse)
}

## This second function first checks to see if a cached version of the inverse
## matrix exists. If it does, it prints it. If not, it calculates it.

cacheSolve <- function(x, ...) {
        ## Creates the function cacheSolve with matrix argument x
        m <- x$get_matrix_inverse()
        ## Assigns function get_matrix_inverse from makeCacheMatrix to m
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Runs check to see if there is a cached value. If not...
        data <- x$get()
        ## Assigns the get function to data
        m <- solve(data, ...)
        ## Assigns the inverse matrix of the data to m
        x$set_matrix_inverse(m)
        ## Sets the mean to this new inverse matrix
        m
        ## Prints a matrix that is the inverse of 'x'
}
