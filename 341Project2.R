# MA 341
# Project 2
#Author: Adam Turner

### Prompt 1
#### Part A, Part B

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

Bmat <- solve(A)
yk = matrix(c(1,1,1,1,1))
for (i in 1:50) {
  yk = (Bmat %*% yk) / (max(abs(Bmat %*% yk)))
  if (i == 50) {
    dom2 <- yk
    print(dom2)
  }
}

Bmat %*% dom2

dom2 * (1/40)

# Prompt 1 Part C
###Unused Matrices
MB = matrix(c(1, -10/pi, 1, 10/pi), nrow=2)
MC = matrix(c(1, -20/pi, 1, 20/pi), nrow=2)

### Forward Eulers method
MF = matrix(c(1, pi/10, -pi/10, 1), nrow=2)
deltat = 0.05

feulerx <- c()
feulery <- c()
for (i in 1:20){
  xka = MF %*% matrix(c(cos(2*pi*deltat*i), sin(2*pi*deltat*i)))
  feulerx <- c(feulerx, xka[1,1])
  feulery <- c(feulery, xka[2,1])
}

feulerdata <- data.frame(feulerx, feulery)

### Backward Euler Method
MBB = matrix(c(100/(100+pi^2), -10*pi/(100+pi^2), 10*pi/(100+pi^2), 100/(100+pi^2)), nrow=2)
deltat = 0.05

beulerx <- c()
beulery <- c()
for (i in 1:20){
  xkb = MBB %*% matrix(c(cos(2*pi*deltat*i), sin(2*pi*deltat*i)))
  beulerx <- c(feulerx, xkb[1,1])
  beulery <- c(feulery, xkb[2,1])
}

beulerdata <- data.frame(beulerx, beulery)

### Crank-Nicolson Method
MT = matrix(c(400/(400+pi^2), 20*pi/(400+pi^2), -20*pi/(400+pi^2), 400/(400+pi^2)), nrow=2)
deltat = 0.05

ceulerx <- c()
ceulery <- c()
for (i in 1:20){
  xkc = MT %*% matrix(c(cos(2*pi*deltat*i), sin(2*pi*deltat*i)))
  ceulerx <- c(ceulerx, xkc[1,1])
  ceulery <- c(ceulery, xkc[2,1])
}

ceulerdata <- data.frame(ceulerx, ceulery)

#Plots
library(ggplot2)
  #Forward Eulers Method
ggplot(feulerdata, aes(feulerdata$feulerx, feulerdata$feulery)) +
  geom_path(color="green", size=3) +
  labs(x= "t", y="x(t)", title="Forward Eulers Method")

  #Backward Eulers Method
ggplot(beulerdata, aes(beulerdata$beulerx, beulerdata$beulery)) +
  geom_path(color="red", size=3) +
  labs(x= "t", y="x(t)", title="Backward Eulers Method")

#Crank-Nicolson Method
ggplot(ceulerdata, aes(ceulerdata$ceulerx, ceulerdata$ceulery)) +
  geom_path(color="blue", size=3) +
  labs(x= "t", y="x(t)", title="Backward Eulers Method")
