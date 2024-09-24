a <- c(1,2,3,4,5,6)

X <- c(20,10,40,15)
sort(X)

Y <- c("B", "A", "D")
sort(Y, decreasing = TRUE)

X[c(1,4)]
Y[3]

cells <- c(1:9)
rnames <- c("R1", "R2", "R3")
cnames <- c("C1", "C2", "C3")
mymatrix <- matrix(cells, nrow=3, ncol=3, byrow=TRUE, dimnames=list(rnames, cnames))

mymatrix[1 ,2]
