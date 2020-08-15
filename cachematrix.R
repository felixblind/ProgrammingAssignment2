## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix is a set/get method for a matrix and its inverse

## Write a short comment describing this function
## makeCacheMatrix builds takes a matrix and gives back a method to
## store (set), get a matrix and set and get its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv_matrix <- NULL
	set <- function(new_matrix) {
		x <<- new_matrix
		inv_matrix <<- NULL
	}
	# get_ does not mask the build in function get
	get_ <- function() x
	set_inverse <- function(new_inv_matrix) inv_matrix <<- new_inv_matrix
	get_inverse <- function() inv_matrix
	list(set=set, get_=get_, set_inverse=set_inverse, get_inverse=get_inverse)
}


## Write a short comment describing this function
## cacheSolve uses the get method to get_inverse method to get the
## inverse. If some is stored we just give that out without computation
## If not we check if the matrix is square and therefore has an inverse.
## If not we throw an exception. If the matrix is invertible we compute
## the inverse and store / set it in the clojure of makeCacheMatrix.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv_matrix <- x$get_inverse()
	if (!is.null(inv_matrix)) {
		message("getting inverse matrix")
		return(inv_matrix)
		}
	# matrix_ does not mask built in matrix function
	matrix_ <- x$get_()
	mdims <- dim(matrix_)
	print(mdims)
	if (mdims[1] == mdims[2]) {
		inv_matrix <- solve(matrix_)
	}
	else {
		stop("The matrix needs to be square.")
	}
	x$set_inverse(inv_matrix)
	inv_matrix
}
