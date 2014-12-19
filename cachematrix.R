makeCacheMatrix <- function(x = matrix()) {
        m<-NULL 
        # m its inverse matrix and it's reset to NULL every 
        # time makeCacheMatrix is called
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        # this function set the value of the original matrix
        
        get<-function() x
        # this function returns the value of the original matrix
        
        setmatrix<-function(solve) m<<- solve
        # this is called by cacheSolve() during the first cacheSolve()
        # access and it will store the value using superassignment
  
        getmatrix<-function() m
        # this will return the cached value to cacheSolve() on
  
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
        
        # is a list of the internal functions ('methods') so a calling function
        # knows how to access those methods.     
}

cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
                # if m was already cached
                # send message to the console 
                # and return   
        }
        matrix<-x$get()
        
        m<-solve(matrix, ...)
        # if m was NULL then we have to solve
        x$setmatrix(m)
        # store the calculated inverse of a matrix value in x 
        m
        # return the matrix to the code that called this function
}
