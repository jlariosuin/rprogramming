makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Función calcula la inversa de la "matriz"

cacheSolve <- function(x, ...) {
        ## Matriz inversa de 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("Obtener datos caché")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
