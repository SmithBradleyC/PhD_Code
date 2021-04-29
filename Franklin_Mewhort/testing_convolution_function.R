
# tests of the convolution function in r
x<-1:5
y<-1:5

x<-rnorm(100)
y<-rnorm(100)

# randy's functions
test1<-Convolve(x,y)
test2<-Deconvolve(x,test1)
Cosine(y,test2)

# need the rev(y) in the y parameter to get the same convolution from TODAM
test3<-normalize(convolve(x = x,y = rev(y),conj = F,type = "circular"))
# rev() around the whole deconvolution, memory as x and vector as y
test4<-rev(normalize(convolve(y = (x), x = (test3), conj = T,type = "circular")))
Cosine(y,test4)

# did the convolution happen the same?
all(round(test1,5)==round(test3,5))
# did the deconvolution happen the same?
all(round(test2,5)==round(test4,5))

# hand calculations for 1 2 3 4 5
1*5+2*1+3*2+4*3+5*4
1*4+2*5+3*1+4*2+5*3
1*3+2*4+3*5+4*1+5*2
1*2+2*3+3*4+4*5+5*1
1*1+2*2+3*3+4*4+5*5



# Modulo function for circular convolution
mod <- function (a, b) {
  return(a %% b)
}

# One way circular convolution with end unit normalization
Convolve <- function (x, y) {
  n <- length(x)
  z <- array(0, dim=n)
  y <- rev(y)
  for (i in 0:(n-1)) {
    for (j in 0:(n-1)) {
      z[i+1] <- z[i+1] + x[mod(j,n)+1] * y[mod((i-j),n)+1]
    }
  }
  z <- z/sqrt(sum(z^2))
  return(z)
}

# One way circular deconvolution with end unit normalization
Deconvolve <- function (x, m) {
  n <- length(x)
  z <- array(0, dim=n)
  for (i in 0:(n-1)) {
    for (j in 0:(n-1)) {
      z[i+1] <- z[i+1] + x[mod(j,n)+1] * m[mod((i+j),n)+1]
    }
  }
  z <- z/sqrt(sum(z^2))
  z <- rev(z)
  return(z)
}

# Standard cosine function
Cosine <- function (x, y) {
  z <- sum(normalize(x) * normalize(y))
  return(z)
}