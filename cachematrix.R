cacheSolve <- function(x, ...) {
        
        
        inv = x$getinv()
        
        ## is there a cached version
        if (!is.null(inv)){
                # go get the cache version
                message("go get a cup of tea I going for cached data")
                return(inv)
        }
        
        ## calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        ## used the invs function to set the inverse
        x$setinv(inv)
        
        return(inv)
}
makeCacheMatrix <- function(x = matrix()) {
        ## This should take a matrix and produce the inverse of it
        ## Following the following stages
        ## Stage 1) setup a matrix
        ## Stage 2) get the input matrix and put it into the newly setup matrix
        ## Stage 3) compute the inverse
        ## Stage 4) put the inverse into the matrix
        
        
        ##Stage 1)
        matInverse = NULL
        set = function(y) {
                ## "<<-" Gives object value in a different environment
                x<<-y
                matInverse <<-NULL
        }
        
        ##Stage2)
        get = function() x
        
        ##Stage3)
        set = function(inverse) matInverse
        
        ##Stage4) 
        setmatinverse = function(inverse) matInverse <<- inverse
        getmatinverse = function() matInverse
        list(set=set, get=get, setmatinverse=setmatinverse, getmatinverse=getmatinverse)
}
