# the environment of the function is remembered by functions "set", "get" & "getinverse"
# the environment will be persistent (not removed by gc) as long there is any reference to the functions
# this is the mechanism for manipulating and resetting variables of the environment
makeCacheMatrix<-function(x=matrix()){
      xInv<-NULL
      set<-function(y){
            x<<-y
            xInv<<-NULL   # the old inverse is not valid for the new matrix
      }
      get<-function() x
      # Well-behaving R-functions are quiet until problem occurs
      # All kind of messages can be perceived as warnings and they can disturb the workflow
      getinverse<-function(verbose = FALSE)
      {
            if(is.null(xInv))
            {  
                  xInv <<- solve(x)  # evaluate if it's for first time, otherwise solve the inverse
            } else {
                  if(verbose) message("getting cached data")
            }
            return(xInv)
      }
      return(list(set=set,get=get, getinverse = getinverse)) ##The object provided to cacheSolve
}
cacheSolve<-function(x, ...) return(x$getinverse(...))
##Will return the inverse of the matrix (gotten from list)
##If a message is desired when receiving cached data, verbose=TRUE (or TRUE) can be entered as 2nd argument
