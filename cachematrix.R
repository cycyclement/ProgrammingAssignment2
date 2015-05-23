#Basically, I re-used the exact same codin lines as in the assignment explanations but instead of working
#with a vector I used a matrix and instead of applying the mean I applied the inverse of this specific matrix


#The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse of the matrix
#get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix())
{
        invmat<- NULL
        set <- function(y)
                {
                x <<- y
                invmat <<- NULL
                }
        get <- function() x
        setinverse <- function(inverse) invmat <<- inverse
        getinverse <- function() invmat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#The following function calculates the inverse of the special "matrix"
#created with the above function. However, it first checks to see if the inverse
#has already been calculated. If so, it gets the inverse from the cache and
#skips the computation. Otherwise, it calculates the inverse of the data and
#sets the value of the inverse in the cache via the setinverse## function.

cacheSolve <- function(x, ...)
{
        invmat <- x$getinverse()
        if(!is.null(invmat))
        {
                message("getting cached data")
                return(invmat)
        }
        data <- x$get()
        invmat <- solve(data)
        x$setinverse(invmat)
        invmat
        ## Return a matrix that is the inverse of 'x'
}
