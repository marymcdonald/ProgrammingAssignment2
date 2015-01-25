## Creates a pair of functions that cache the inverse of
## a matrix.

## makeCacheMatrix creates a special matrix object: 
##it is a list containing a function to
##set the value of the matrix, get the value of the matrix
##set the value of the inverse, and then get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
          x<<-y
          m<<-NULL
        }
        
        get<-function() x
        setinverse<-function(solve)
          m<<-solve
        getinverse<-function() m
        list(set=set, get=get, 
             setinverse=setinverse
             getinverse=getinverse)

}


## cacheSolve computes the inverse of the matrix object returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix hasn't changed)
## cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
      m<-x$getinverse()
      if(!is.null(m)){
        message("getting cached data")
        return (m)
      }
      data<-x$get()
      m<-solve(data,...)
      x$setinverse(m)
      m
}

##I thought at first that the assignment was to write an algorithm calculating the inverse
##of a matrix. After I'd written it, I read over the assignment and saw that I was 
##wrong. Anyway, if you want to see my inverse algorithm, it's here:
##https://github.com/marymcdonald/inversematrix/blob/master/inversematrixalg
## It ends up being more accurate than solve, but I didn't try it out with massive matrices
## so it is most likely slower...
