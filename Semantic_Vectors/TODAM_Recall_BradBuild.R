# Load relevant libraries
#library(readxl)
library(LSAfun)

# Clear the work environment
rm(list = ls())

# Load the semantic vectors 
load("BEAGLE.rda"); Semantic_vectors <- BEAGLE

# Normalize the semantic vectors
Semantic_vectors <- t(apply(X=Semantic_vectors, MARGIN=1, FUN=normalize))

# Run a memory trial
Serial_recall <- function (Words, Vectors, Alpha, Gamma, Kriterion) {
  # Initiate memory
  M <- Vectors[Words[1],]
  
  # go through the word list
  for (i in 2:length(Words)) {
    
    # identify the vector for the word in question
    #Item <- normalize(Vectors[Words[i], ]) # needed if vectors are not already normalized
    Item <- Vectors[Words[i],]
    # identify the association between the word and the previous word
    Association <- normalize(convolve(x = Vectors[Words[i-1], ], y = rev(Vectors[Words[i], ]), conj = F, type = "circular"))
    
    # Classic encoding formula
    M <- (Alpha * M +
         Gamma * Item +
         (1 - Gamma) * Association)
  } #end of word list
  
  
  # start recall test
  # make a recall list
  Recall <- matrix(0, length(Words))
  
  # put the start cue as the first word
  Recall[1] <- Words[1]
  
  # for all the experiment words
  for (i in 2:length(Words)) {
    
    # find the word in the corpus with the most similarity to the deconvolution of memory with the previously recalled word
    Recall[i] <- rownames(Vectors)[[which.max(Vectors %*% rev(normalize(convolve(y = (Vectors[Recall[i-1], ]), x = M, conj = T, type = "circular"))))]]
    # update memory by removing the recalled word and its association with the previous word (and normalize memory)
    M <- (M - (Gamma * Vectors[Recall[i],] + (1 - Gamma) * normalize(convolve(x = Vectors[Recall[i-1],], y = rev(Vectors[Recall[i],]), conj = F,type = "circular"))))
  }
  # print the words presented and recalled
  print(cbind(Words, Recall))
}


alpha<-1
gamma<-0.05
kriterion<-1


#Words <- c("start", "short", "avenue", "deputy", "banana", "bottle", "barn")
#Words <- c("start", "apple", "sin", "street", "school", "running", "birth", "end")
Words <- c("start", "psychology", "soccer", "biology", "football", "science", "baseball", "end")
Words<-c("start","table","pizza","chair","sandwich","couch","beer","end")
Serial_recall(Words=Words, Vectors=Semantic_vectors, Alpha=alpha, Gamma=gamma, Kriterion=kriterion)






# this<-normalize(convolve(x=Semantic_vectors["apple",],y = rev(Semantic_vectors["table",]),conj = F,type = "circular"))
# that<-Convolve(Semantic_vectors["apple",],Semantic_vectors["table",])
# all(round(this,12)==round(that,12))
