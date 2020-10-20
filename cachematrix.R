
## Acontinuacion se muestra unas funciones que permiten-
##almacenar una matriz , la inversa de la matriz en el cache.

## La funcion crea un objeto llamado "makeCachematrix" que puede almacenar la matriz y su inversa.
## x matrix
## inver con valor nulo
## cambiar referencias a "inverse"
makeCacheMatrix <- function(x = matrix()) {
  
  inver <- NULL
  set <- function(k) {
    x <<- k
    inver <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## objeto "cachesolve". la matriz creada por "makeCachematrix"  como ya se calcula su inversa
## se guarda en el cache. solucion de cache debera recuperar la inversa.

cacheSolve <- function(x, ...) {
  
  
  inver <- x$getInverse()
  if (!is.null(inver)) {
    message("cached data")
    return(inver)
  }
  mat <- x$get()
  inver <- solve(mat, ...)
  x$setInverse(inver)
  inver
  
}

## Ejemplos
##matrix_example<-makeCacheMatrix(matrix(9:12, 2, 2,byrow = TRUE))
## matrix_example$get()
## matrix_example$getInverse()
## cacheSolve(matrix_example)
