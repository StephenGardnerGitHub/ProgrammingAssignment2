###############################################################################
## The following two functions allow matrix inversion to be cached since it
## is such an expensive operation to perform. 
## The programs work together by exploiting R's lexical scoping. 
##
## The first function (makeCacheMatrix) allows an object to be 
## created that has operations defined on it that support caching. 
##
## The second function uses the first function/object to implement a 
## cache for the value of the matrix inversion.
##
###############################################################################

## makeCacheMatrix) allows an object containing a matrix  
## to be created that has operations defined on it 
## that support caching. 

makeCacheMatrix <- function(x = matrix()) {
	invMat<-NULL

	# allow setting the matrix
	set<-function(y){
		# We have to set a new value for x by reaching up 
		# with the <<- operator.
		x<<-y

		# And, of course, null out the cached inversion of x
		invMat<-NULL
		
	}

	# Simple: just return the matrix but it's best to 
	#    always use curly braces 
	get<-function(){x} 

	# Get the value of the inverted matrix. Note that it will be
	# null unless it has been set.
	getInv<-function() {invMat}
 
	# Set the value of the inverse matrix.
	setInv<-function(invrs){
		# reach up with <<- and use the lexical scope to set the correct
		# invMat (defined in the lexical scope of the functions we have
		# created
		invMat<<-invrs
	}

	# Now return the object which is really just a list of functions
	# that have a common environment for each instance. This
	# environment will contain the matrix and the inverse if set.
	list(set=set,get=get,getInv=getInv,setInv=setInv)
}


## cacheSolve uses the first function/object to implement a 
## cache for the value of the matrix inversion.

cacheSolve <- function(x, ...) {

	# get the potentially cached inverse of the matrix stored in x
	invrs<-x$getInv()

	# if it has been cached then it will not be null
	if(!is.null(invrs)) {
		message("Getting cached inversion")
		return(invrs)
	}

	# Okay, it was null, now we gotta do the work of inverting it.
	# get the matrix out and use solve to invert it
	mtrx<-x$get()
	invrs<-solve(mtrx)

	# Now set the value of the inverse so it will be cached for the future.
	x$setInv(invrs)

	# and,of course, return the invers
	invrs

	
}
