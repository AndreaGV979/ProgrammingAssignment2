## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#this Function makeCacheMatrix use a matrix as an input, set the value of the matrix,
#get the value of the matrix, calculate the inverse Matrix with the function inverse.

makeCacheMatrix <- function(x = matrix()) { #Here we define the function that is going to use a matriz that we want to inverse
  inversematrix<-NULL #Create an objecto which is empty
  #Now we set a value to a Matrix 
  set <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  get <- function() x      #get the value of the matrix
  setinverse <- function(inverse) inversematrix <<- inverse #set the value of a matrix that has to be ivertible
  getinverse <- function() inversematrix #get the value of the matrix
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse) #we create a listi in which we are going to have te elements to ge inverse matrix
  
  
}


## Write a short comment describing this function
## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
# In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
# and set the invertible  matrix by using the solve function.
# In case inverse matrix from makeCacheMatrix((matrix) has some value in it (always works
#after running the code 1st time), it returns a message  "Getting Cached Invertible Matrix" 
#and the cached object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("obteniendo datos")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
#Here im going to give ypu an example to demostrate the program works
B <- matrix(c(6,8,3,5),2,2) #This is the matrix that I choose (invertible of course) DIMENSIONS 2*2
B1 <- makeCacheMatrix(B) #B1 is going to be teh inverse 
cacheSolve(B1)
cacheSolve(B1)