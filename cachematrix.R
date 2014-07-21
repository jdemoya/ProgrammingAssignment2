## Put comments here that give an overall description of what your
## functions do
##This functions take a matrix and calculates its inverse and stores in cache memory and later if you call it again 
## it checks if the inverse is stored in cache if not then calculates its inverse and stores in cached memory.
## if matrix is changed the inverse is recalculated and stored in cache memory
## use: m <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2)) ##set a matrix
## m$get()                                                       ##Returns the original matrix
## cacheSolve(m)                                                 ##Computes, caches, and returns    matrix inverse
## m$getinv()                                                    ##Returns matrix inverse
## cacheSolve(m)                                                 ##Returns cached matrix inverse using previously computed in the previous step
## m$set(matrix(c(0,1,8,45), nrow=2, ncol=2))                    ##Modify existing matrix
## cacheSolve(m)                                                 ##Computes, caches, and returns    matrix inverse of the new matrix
## m$get()    						         ##Returns new matrix
## m$getinv()							 ##Returns matrix inverse of new matrix

## Write a short comment describing this function
## This function assign a matrix or get the value of a matrix that is loaded in cache or change the value of that matrix in memory
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
          x <<- y
          m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


## Write a short comment describing this function
##This funtion calculate de inverse of a matrix and if the inverse is cached only get it from memory and skip the process for calculating it
##but if the inverse is not stored in cache memory the function calculates it and the inverse matrix is stored in cache memory
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m

}
