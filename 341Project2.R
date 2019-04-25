values <- c(-242,444,-288,642,702,278,-492,304,-742,-802,209,-352,248,-511,-559,-110,232,-112,362,370,-84,132,-96,192,220)

A <- matrix(values, nrow = 5, byrow=FALSE)


xk = matrix(c(1,1,1,1,1))
for (i in 1:50) {
  xk = (A %*% xk / max(abs(A %*% xk)))
  if (i == 50) {
    dom <- xk
    print(dom)
  }
}

A %*% dom
dom
lambda = 40
(A %*% dom) == (lambda * dom)

(1/40)*dom

B <- c(40,0,0,0,0,0,40,0,0,0,0,0,40,0,0,0,0,0,40,0,0,0,0,0,40)
Bmat <- matrix(B, nrow=5, byrow=FALSE)
yk = matrix(c(1,1,1,1,1))
for (i in 1:50) {
  yk = (Bmat %*% xk / max(abs(Bmat %*% xk)))
  if (i == 50) {
    dom2 <- yk
    print(dom2)
  }
}

Bmat %*% dom2

solve(A)
