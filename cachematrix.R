## These functions take matrices and store or retrieve them from cache, as well
## as generating an inversion of any cached matrix and storing/retrieving that

## The "makeCacheMatrix" function is itself comprised of four functions:
## 2 of these functions can set, or store in cache, a matrix or its inverse;
## 2 of these functions can retrieve from cache the matrix or its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    set <- function(y) {
        x <<- y
        inv_matrix <<- NULL
    }
    get <- function() x
    set_inversion <- function(inversion) inv_matrix <- inversion
    get_inversion <- function() inv_matrix
    list(
        set = set,
        get = get,
        set_inversion = set_inversion,
        get_inversion = get_inversion
        )
}


## cacheSolve can both retrieve a cached inversion of a matrix as created by
## makeCacheMatrix, or it can generate (and store to cache) an inversion of
## a cached matrix if no cached inversion currently exists

cacheSolve <- function(x, ...) {
    inv_matrix <- x$get_inversion()
    if(!is.null(inv_matrix)) {
        message("Retrieving cached data...")
        return(inv_matrix)
    }
    cache_matrix <- x$get()
    inv_matrix <- solve(cache_matrix, ...)
    x$set_inversion(inv_matrix)
    inv_matrix
}
