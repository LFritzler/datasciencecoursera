#function creates a  special "matrix" object
#that can cache its inverse


makeCacheMatrix  <- function(myMatrix = matrix())
{
     myInverseMatrix <- NULL
     
     setMatrix <- function(dataIn)
     {
        myMatrix <<- dataIn
        myInverseMatrix <<- NULL
     
    }#function
     
   getMatrix  <-  function() myMatrix
   setInverse <-  function(invData) myInverseMatrix <<- invData
   getInverse <-  function () myInverseMatrix
   
   list( setMatrix =  setMatrix,
         getMatrix =  getMatrix,
         getInverse = getInverse,
         setInverse=  setInverse
       )

}# end makeCacheMatrix


#funciton compute inverse of the special "matrix" returned 
#by makeCacheMatrix. If the inverse has been already calculated
#(and matrix unchanged) the cacheSolve should get the inverse.gaussian()
#from the cache




cacheSolve <- function(myMatrix, ...)
{
    myInverseMatrix <- myMatrix$getInverse()
    
    if(!is.null(myInverseMatrix)){
       
        message("Inverse already completed:  getting the cached data.")
        
        return(myInverseMatrix) 
         
    }#if
    
    data <-  myMatrix$getMatrix()
    
    #solve() returns inverse of a matrix
    myInverseMatrixNew <- solve(data,...)
    myMatrix$setInverse(myInverseMatrixNew)

    
}# end cacheSolve