## Two functions that cache the inverx of a matrix

##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(A = matrix()){
       
        A_inverse <- NULL
        
        set <- function (B) {
                A <<- B
                A_inverse <<- NULL
        }
        
        get <- function () A
        
        setinverse <- function (inverse) A_inverse <<- inverse
        
        getinverse <- function () A_inverse
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse ) 
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache



cacheSolve <- function (A, ...){
        
        #Check if there is already exist an inverse
        A_inverse <- A$getinverse()
        
        if (!is.null(A_inverse)) {
                message("There is data in the cache")
                return(A_inverse)
                
        }
        matrice <- A$get()
        if (nrow(matrice) == ncol(matrice)) {
                A_inverse <- solve(matrice, ...)
                A$setinverse(A_inverse)
                A_inverse
                
        } else {
                
                message("It is not a square matrix, please try again")
                
        }
}
