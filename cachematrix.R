## Función que almacena la inversa de una matriz

## Crea una matriz especial que puede almacenar en su cache la inversa

makeCacheMatrix <- function(x = matrix()) {
    
    ##Inicia la propiedad inversa i
    i <- NULL
    
    ## Establece la matriz set
    set <- function(matrix){
        m <<- matrix
        i <<- NULL
    }
    
    ## Obtiene la matriz
    get <- function(matrix){
        ## La devuelve
        m
    }

    ## Establece la inversa de la matriz
    setInverse <- function(inverse){
        i <<- inverse
    }
    
    ## Obtiene la inversa de la matriz
    getInverse <- function(){
        ## Devuelve la propiedad de la inversa
        i
    }
    
    ## Devuelve una lista de los métodos
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## Calcula la inversa de la matriz especial
## Si la inversa ya ha sido calculada y la matriz no ha cambiado
## recupera la matriz que tiene en caché

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    ## solo devuelve la inversa si ya está establecida
    if( !is.null(m)){
        message("Obteniendo datos en caché")
        return(m)
    }
    
    ## Obtener la matriz de nuestro objeto
    data <- x$get()
    
    ## Calcular la inversa utilizando la multiplicación de la matroz
    m <- solve(data)%*% data
    
    ## Establece la inversa en el objeto
    x$setInverse(m)
    
    ## Devuelve la matriz
    m
}
