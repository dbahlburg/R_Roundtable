# create 3 matrices
A = matrix(c(1:9),3,3)
B = matrix(c(11:19),3,3)
C = matrix(c(4:12),3,3)

# use apply() for matrices (arrays)



# create list object including the three matrices
mylist <- list(A,B,C)

# use lapply() for list objects


# use sapply() to extract the first element from each list object
Z <- sapply(mylist,"[", 1,1 )

# create vector with repeated elements in Z 
Z <- rep(Z,c(3,1,2))


# use mapply() to apply a function to multiple list or vector arguments


