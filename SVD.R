A = as.matrix(data.frame(c(4,7,-1,8), c(-5,-2,4,2), c(-1,3,-3,6)))
A
SVD <- svd(A)
SVD

variance.explained = prop.table(svd(A)$d^2)
variance.explained

# We can reconstruct A using:
SVD$u %*% diag(SVD$d) %*% t(SVD$v)
# %*% is matrix multiplication

# Or we can reconstruct ~ 75% A using:
(SVD$u[,1] * SVD$d[1]) %*% t(SVD$v[,1])
# Compare this with the original matrix A
A

# If we take two singular vectors, we can reconstruct ~95% of A
SVD$u[,1:2] %*% diag(SVD$d[1:2]) %*% t(SVD$v[,1:2])
