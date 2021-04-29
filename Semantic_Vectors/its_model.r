# problems: Forgettting rate is spelled wrong

##############################################################
# An Instance Theory of Semantic Memory
# Jamieson et al. (2018, Computational Brain and Behaviour)
##############################################################

# Clear the work environment
rm(list = ls())

start_time<-Sys.time()

# Name the corpus
Corpus <- "tasaDocsPara.txt"

# Set parameters
N_dimensions <- 2000
Tau <- 3
Forgetting_rate <- 0

# Load useful libraries (tm and LSAfun need to be installed)
library(tm)
library(LSAfun)

# Get unique words in the corpus
Unique_words <- unique(scan(file=Corpus, what="char", sep=""))

# Construct the environmental word vectors from a Gaussian with mean = 0 and sd = 1/sqrt(n)
Words <- matrix(rnorm(length(Unique_words)*N_dimensions, 0, 1/sqrt(N_dimensions)),
                length(Unique_words),
                N_dimensions)
rownames(Words) <- Unique_words

start_time<-Sys.time()

# Get all exemplar-documents in the corpus
Sentences <- scan(file=Corpus, what="char", sep="\n")

# cut out 90% of the sentences
Sentences<-Sentences[1:round(length(Sentences)*.1)]

# create a progress bar
pb<-txtProgressBar(min = 0, max = length(Sentences), style = 3)
# record time that loop started
#start_time2<-Sys.time()
# Construct exemplar-document record of the corpus
Memory <- matrix(0, length(Sentences), N_dimensions)
for (i in 1:length(Sentences)) {
  # print average time per iteration
  #print((Sys.time()-start_time2)/i)
  setTxtProgressBar(pb,i)
  Tmp <- strsplit(Sentences[i], " ")[[1]]
  if (length(Tmp) > 0) {
    for (j in 1:length(Tmp)) {
      Memory[i,] <- Memory[i,] + Words[Tmp[j],]
    }
  }
}
close(pb)
rownames(Memory) <- Sentences

# Vector cosine
Cosine <- function (x, y) {
  z <- 0
  if (sum(abs(x)) != 0 & sum(abs(y)) != 0) z <- sum(x*y) / sqrt(sum(x*x) * sum(y*y))
  return(z)
} 

# Get cosine similarity table for all rows in a matrix of semantic vectors
Cosine_table <- function (M) {
  # create a progress bar
  pb<-txtProgressBar(min = 0, max = nrow(M), style = 3)
  Tmp <- matrix(0, nrow(M), nrow(M))
  for (i in 1:nrow(M)) {
    setTxtProgressBar(pb,i)
    for (j in 1:nrow(M)) {
      if (sum(abs(M[i,])) != 0 & sum(abs(M[j,])) != 0) 
        Tmp[i,j] <- sum(M[i,]*M[j,]) / sqrt(sum(M[i,]*M[i,]) * sum(M[j,]*M[j,]))
    }
  }
  close(pb)
  rownames(Tmp) <- rownames(M)
  colnames(Tmp) <- rownames(M)
  return(Tmp)
}

# Introduce forgetting to memory (Forgetting_rate = 1 - L in the original MINERVA 2 model)
p <- matrix(runif(nrow(Memory)*ncol(Memory), 0, 1), nrow(Memory), ncol(Memory))
Memory <- Memory * ifelse(p < Forgetting_rate, 0, 1)
rm(p)

# Get semantic vectors (P = List of probe words, W = Words matrix, M = Memory matrix, Tau = Retrieval gradient)
Semantic_subset <- function (P, W, M, Tau) {
  pb<-txtProgressBar(min = 0, max = length(P), style = 3)
  S <- matrix(0, length(P), ncol(M))
  for (i in 1:length(P)) {
    setTxtProgressBar(pb, i)
    Probe <- unlist(strsplit(P[i], split="/"))
    if (all(Probe %in% rownames(W)) == TRUE) {
      for (j in 1:nrow(M)) {
        A <- 1.0
        for (k in 1:length(Probe)) {
          A <- A * Cosine(W[Probe[k],], M[j,])**Tau
        }
        S[i,] <- S[i,] + A * M[j,]
      }
    }
  }
  close(pb)
  rownames(S) <- P
  S <- S[abs(rowSums(S)) != 0,]
  return(S)
}

# Words from Jones & Mewhort (2007, Figure 3) to demonstrate taxonomic clustering
JonesMewhort_Figure3 <- c("financial","savings","finance","pay","invested",
                          "loaned","borrow","lend","invest","investments",
                          "bank","spend","save","astronomy","physics",
                          "chemistry","psychology","biology","scientific",
                          "mathematics","technology","scientists","science",
                          "research","sports","team","teams","football",
                          "coach","sport","players","baseball","soccer",
                          "tennis","basketball")

# Get semantic space
Probe_list <- JonesMewhort_Figure3
Semantic_space <- Semantic_subset(P = Probe_list,
                                  W = Words,
                                  M = Memory,
                                  Tau = 3)

# Plot the results as a clustered correlation plot
corrplot::corrplot(Cosine_table(Semantic_space),
                   method="circle",
                   order="hclust",
                   tl.col="black",
                   tl.cex=.4,
                   title="",
                   cl.cex=.5)

# Plot the results as a 2d MDS solution
plot_wordlist(rownames(Semantic_space),
              tvectors=Semantic_space,
              method="MDS",
              dims=2)

end_time<-Sys.time()
print(end_time-start_time)