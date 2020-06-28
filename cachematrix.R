# This function creates a special matrix that caches the inverse of that matrix

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# This function calculates the inverse of the matrix created in the function above
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}

#test: 2x2 matrix
my_matrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))   # create matrix
my_matrix$get()
#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4

my_matrix$getInverse()   # calling inverse of my_matrix
# NULL                   # NULL because my_matrix isn't cached

cacheSolve(my_matrix)    # caching my_matrix
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

cacheSolve(my_matrix)    # calling cached my-matrix
# getting cached data
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

my_matrix$getInverse()   # calling inverse of my_matrix
#       [,1] [,2]        # cached my_matrix is re-called
# [1,]   -2  1.5
# [2,]    1 -0.5



# testing2: doesn't work for any square matrix above 2:2 (and I cannot figure why)
my_matrix <- makeCacheMatrix(matrix(1:25, nrow=5, ncol=5))
my_matrix$get()
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    1    6   11   16   21
# [2,]    2    7   12   17   22
# [3,]    3    8   13   18   23
# [4,]    4    9   14   19   24
# [5,]    5   10   15   20   25

my_matrix$getInverse()
# NULL

cacheSolve(my_matrix)
#  Error in solve.default(data, ...) : 
# Lapack routine dgesv: system is exactly singular: U[3,3] = 0 
