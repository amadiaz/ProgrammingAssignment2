makeCacheMatrix <- function(inputmatrix = matrix())
{
    ## This function, makeCacheMatrix, creates a special "matrix" object that can cache its inverse. The argument (input) to this function is inputmatrix, which is a matrix. First, we define a new variable, inversematrix, as NULL.
    inversematrix <- NULL
    
    set <- function(somedata)  
    {
        ## Next, 'set' is a new function being created with new variable
        # somedata as its argument. Inside this new function, the input 
        # variable somedata is being assigned as the value of the matrix 
        # inputmatrix, which exists in a different environment: the function 
        # that contains this function. Also, a NULL value is being assigned to 
        # the variable inversematrix which exists in a different environment: 
        # the function that contains this function (this is necessary because 
        # function 'set' changes the data by assigning somedata as the value of 
        # inputmatrix, so the old matrix is no longer valid).
        
        inputmatrix <<- somedata
        inversematrix <<- NULL
    }
    
    get <- function()
    {
        ## Then, we make another new function 'get' that returns inputmatrix.
        return(inputmatrix)
    }
    set.inversematrix <- function(blarg) 
    {
        ## Now, we make set.inversematrix, a new function that takes argument 
        # "blarg" and returns inversematrix, which has been defined in the next 
        # higher environment (the makeCacheMatrix function), and is now being 
        # assigned the value of "blarg."
        inversematrix <<- blarg
    }
    
    get.inversematrix <- function() 
    {
        ## Then, get.inversematrix is a new function that returns the value of 
        # inversematrix.
        return(inversematrix) 
    } 
    
    ## This last step creates a list of the newly-created functions that will 
    # be returned by the makeCacheMatrix function.
    list(set = set, get = get,
         set.inversematrix = set.inversematrix,
         get.inversematrix = get.inversematrix)
}


cacheSolve <- function(matrixobj, ...) 
{
    ## This second function, cacheSolve, computes the inverse of the special 
    # "matrix" object returned by makeCacheMatrix above (assuming that is the 
    # input to cacheSolve). If the inverse has already been calculated (and the 
    # matrix has not changed), then cacheSolve should retrieve the inverse from 
    # the cache. 
    ## First, we look in the input to this function, which is the output from 
    # makeCacheMatrix, to run the get.inversematrix function and name its 
    # output 'inversematrix.' Then, we check to see if inversematrix has a NULL 
    # value: if not, I want to return the value of inversematrix and exit the 
    # function. 
    inversematrix <- matrixobj$get.inversematrix()
    if(!is.null(inversematrix))
    {
        message("getting cached data") 
        return(inversematrix) 
    }
    
    ## However, if that 'if' condition is not met, because inversematrix has a 
    # NULL value, I want to run the following lines of code in order to compute 
    # and return the inverse of the input matrix. 
    inputmatrix <- matrixobj$get()
    inversematrix <- solve(inputmatrix, ...)
    matrixobj$set.inversematrix(inversematrix)
    inversematrix
}
