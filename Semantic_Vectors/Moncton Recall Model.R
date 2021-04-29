# Moncton Convolution-based Recall Model

# Load relevant libraries
library(readxl)
library(LSAfun)

# Clear the work environment
rm(list = ls())

# Load the LSA semantic vectors 
# load("LSA.rda"); Semantic_vectors <- LSA
# load("TASA.rda"); Semantic_vectors <- TASA
load("BEAGLE.rda"); Semantic_vectors <- BEAGLE

# Vector normalize
Normalized_semantic_vectors <- t(apply(X=Semantic_vectors, MARGIN=1, FUN=normalize))

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

# Compute a cosine matrix (quick matrix form)
Cosine_matrix <- function (Words, Vectors) {
  x <- apply(X=Vectors[Words,], MARGIN=1, FUN=normalize)
  z <- t(x) %*% x
  return(z)
}

# Run a memory trial
Serial_recall <- function (Words, Vectors, Alpha, Gamma, Kriterion) {
  M <- Vectors[Words[1],]
  for (i in 2:length(Words)) {

    Item <- normalize(Vectors[Words[i], ])
    Association <- as.vector(Convolve(Vectors[Words[i-1], ], Vectors[Words[i], ]))
    M <- Alpha * M +
         Gamma * (1 - Cosine(M, Item)) * Item +
         (1 - Gamma) * (1 - Cosine(M, Association)) * Association
    
    # E <- Vectors[Words[i], ] + as.vector(Convolve(Vectors[Words[i-1], ], Vectors[Words[i], ]))
    # M <- (1 / sum((M + E)^2)) * (M + E)
         
    # Item <- Vectors[Words[i], ]
    # Association <- as.vector(Convolve(Vectors[Words[i-1], ], Vectors[Words[i], ]))
    # M <- Alpha * M +
    #      (1 - Cosine(Item, M)) * Item +
    #      (1 - Cosine(Association, M)) * Association
    
    # Classical encoding formula
    # M <- Alpha * M +
    #      Gamma * Vectors[Words[i], ] +
    #      (1 - Gamma) * as.vector(Convolve(Vectors[Words[i-1], ], Vectors[Words[i], ]))
    
    # Classical encoding formula
    # M <- Alpha * M + 
    #      (Gamma/2) * Vectors[Words[i-1], ] + 
    #      (Gamma/2) * Vectors[Words[i], ] + 
    #      (1 - Gamma) * as.vector(Convolve(Vectors[Words[i-1], ], Vectors[Words[i], ])) 
  
  }
  Recall <- matrix(0, length(Words))
  Recall[1] <- Words[1]
  for (i in 2:length(Words)) {
    Recall[i] <- rownames(Vectors)[[which.max(Normalized_semantic_vectors %*% normalize(Deconvolve(Vectors[Recall[i-1], ], M)))]]
    M <- M - (Gamma * Vectors[Recall[i],] + (1 - Gamma) * Convolve(Vectors[Words[i-1],], Vectors[Recall[i],]))
  }
  print(cbind(Words, Recall))
}

# Simulation
Words <- c("start", "brief", "avenue", "deputy", "banana", "bottle", "barn")
Words <- c("start", "forces", "spare", "birth", "rope", "creative", "gal")
Words <- c("start", "divine", "sin", "heaven", "faith", "soul", "evil")
Words <- c("start", "apple", "orange", "banana", "peach", "cherry", "coconut")
Words <- c("start", "apple", "sin", "street", "school", "running", "birth")
Words <- c("start", rownames(Semantic_vectors)[sample(1:nrow(Semantic_vectors),size = 6)])
Words <- c("start", "psychology", "soccer", "biology", "football", "science", "baseball")

Cos_list<-c()
for(i in 1:length(Words)){
  for(j in 1:length(Words)){
    Cos<-LSAfun::Cosine(Words[i],Words[j], tvectors = Semantic_vectors)
    if(Cos == 1){}else{Cos_list<-c(Cos_list,Cos)}
  }
}
mean(Cos_list)

Serial_recall(Words=Words, Vectors=Semantic_vectors, Alpha=0.95, Gamma=0.5, Kriterion=1)

AB <- as.vector(Convolve(Vectors[Words[1],], Vectors[Words[2],]))
BC <- as.vector(Convolve(Vectors[Words[2],], Vectors[Words[3],]))
CD <- as.vector(Convolve(Vectors[Words[3],], Vectors[Words[4],]))
DE <- as.vector(Convolve(Vectors[Words[4],], Vectors[Words[5],]))
EF <- as.vector(Convolve(Vectors[Words[5],], Vectors[Words[6],]))
FG <- as.vector(Convolve(Vectors[Words[6],], Vectors[Words[7],]))
A <- as.vector(Vectors[Words[2],])
B <- as.vector(Vectors[Words[3],])
C <- as.vector(Vectors[Words[4],])
D <- as.vector(Vectors[Words[5],])
E <- as.vector(Vectors[Words[6],])
F <- as.vector(Vectors[Words[7],])

Noise <- .00
M <- matrix(runif(ncol(Vectors), 0, 1/sqrt(ncol(Vectors))), ncol(Vectors))
M <- Alpha * M + (1 - Cosine(M,AB)) * AB + (1 - Cosine(M,A)) * A + runif(ncol(Vectors), -Noise, +Noise)
M <- Alpha * M + (1 - Cosine(M,BC)) * BC + (1 - Cosine(M,B)) * B + rnorm(ncol(Vectors), -Noise, +Noise)
M <- Alpha * M + (1 - Cosine(M,CD)) * CD + (1 - Cosine(M,C)) * C + rnorm(ncol(Vectors), -Noise, +Noise)
M <- Alpha * M + (1 - Cosine(M,DE)) * DE + (1 - Cosine(M,D)) * D + rnorm(ncol(Vectors), -Noise, +Noise)
M <- Alpha * M + (1 - Cosine(M,EF)) * EF + (1 - Cosine(M,E)) * E + rnorm(ncol(Vectors), -Noise, +Noise)
M <- Alpha * M + (1 - Cosine(M,FG)) * FG + (1 - Cosine(M,F)) * F + rnorm(ncol(Vectors), -Noise, +Noise)
for (i in 1:(length(Words)-1)) {
  print(paste(Words[i], rownames(Vectors)[[which.max(Normalized_semantic_vectors %*% normalize(Deconvolve(Vectors[Words[i], ], M)))]]))
}


# # VAD lists
# Words <- c("start", "brief", "avenue", "deputy", "banana", "bottle", "barn")
# Words <- c("start", "forces", "spare", "birth", "rope", "creative", "gal")
# Words <- c("start", "dress", "intelligent", "sing", "mercy", "anniversary", "protected")
# 
# # Related lists
# Words <- c("start", "patient", "surgery", "ward", "medication", "medical", "clinic")
# Words <- c("start", "divine", "sin", "heaven", "faith", "soul", "evil")
# 
# # Random lists
# Words <- c("start", "muscle", "spider", "path", "hostage", "tent", "bottom")

