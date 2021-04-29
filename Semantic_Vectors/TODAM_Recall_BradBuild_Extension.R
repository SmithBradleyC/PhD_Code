# Load relevant libraries
#library(readxl)
library(LSAfun)

# Clear the work environment
rm(list = ls())

# load in DRM lists
source(file = "createDRMwordlist.R")

# Load the semantic vectors 
#load("BEAGLE.rda"); Semantic_vectors <- BEAGLE

load("BEAGLE_20newsCorpus.rda"); Semantic_vectors <- BEAGLE

# make orthanormal start and end flags
startflag<-rnorm(n = ncol(Semantic_vectors),mean = 0,sd = (1/ncol(Semantic_vectors))^2)
endflag<-rnorm(n = ncol(Semantic_vectors),mean = 0,sd = (1/ncol(Semantic_vectors))^2)
Semantic_vectors<-rbind(Semantic_vectors,startflag,endflag)

# Normalize the semantic vectors
Semantic_vectors <- t(apply(X=Semantic_vectors, MARGIN=1, FUN=normalize))

# Run a memory trial
Serial_recall <- function (Words, Vectors, Alpha, Gamma, Kriterion, EchoThresh) {
  
  # start timer 
  starttime<-Sys.time()

  # Initiate memory
  # put in some random noise to begin
  # M <- rnorm(n = ncol(Vectors),mean = 0,sd = (1/ncol(Vectors))^2)
  # put in the startflag
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
            (1 - Gamma) * Association) #+ rnorm(n = ncol(Vectors),mean = 0,sd = (1/ncol(Vectors))^2)
  } #end of word list
  
  
  # start recall test
  # make a recall list
  Recall <- matrix(0, length(Words))
  
  # put the start cue as the first word
  Recall[1] <- Words[1]
  
  # for all the experiment words
  # for (i in 2:length(Words)) { # original
  
  # make matrix to store similarity results
  Similarity_Store<-matrix(nrow = nrow(Vectors), ncol = 0)
  
  # record similarity of all words to the memory vector
  Initial_Similarity<-Vectors %*% M
  
  # set j at 2 for iteration
  j<-2
  for (i in 2:(length(Words)*2)){

    same<-F
    # get the echo
    Echo<-rev(normalize(convolve(y = (Vectors[Recall[j-1], ]), x = M, conj = T, type = "circular")))
    # similarity between echo and all words in the corpus
    Corpus_Similarity_Values<-Vectors %*% Echo
    # update similarity store
    Similarity_Store<-cbind(Similarity_Store,Corpus_Similarity_Values)
    # pick the most similar item to recall
    Recall[j] <- rownames(Vectors)[[which.max(Corpus_Similarity_Values)]]
    # if they recalled the same word as before (or begin) then flag it
    #if(Recall[j] == Recall[j-1]){same<-T}
    if(Recall[j]%in%Recall[1:(j-1)]){same<-T}
    # update memory by removing the recalled word and its association with the previous word (and normalize memory)
    M <- (M - (Gamma * Vectors[Recall[j],] + (1 - Gamma) * normalize(convolve(x = Vectors[Recall[j-1],], y = rev(Vectors[Recall[j],]), conj = F,type = "circular"))))
    # I HAVE CHANGED THE GAMMA COEFFICIENTS
    #M <- (M - ((Gamma)*2 * Vectors[Recall[j],] + (1-Gamma) * normalize(convolve(x = Vectors[Recall[j-1],], y = rev(Vectors[Recall[j],]), conj = F,type = "circular"))))
    # if you got to the end then break
    if(Recall[j]=="end"){break}
    # if the echo is weak then break
    if(Vectors[Recall[j],]%*%Echo<EchoThresh){break}
    # only update j if they recalled a new word
    if(same==F){j<-j+1}
  }
  # print the words presented and recalled
  to_print<-cbind(Words, Recall)
  print(to_print)
  return(list(words=rownames(Vectors),similarities=Similarity_Store, initialsimilarity=Initial_Similarity, results=to_print,time_taken=Sys.time()-starttime))
}



alpha<-1
gamma<-0.2
kriterion<-1
echothresh<-.15




#Words <- c("startflag", "short", "avenue", "deputy", "banana", "bottle", "barn","endflag") #"random"
#Words <- c("startflag", "apple", "sin", "street", "school", "running", "birth", "endflag") #"random"
#Words <- c("startflag", "psychology", "soccer", "biology", "football", "science", "baseball", "endflag") #subject and sport
#Words<-c("startflag","table","pizza","chair","sandwich","couch","beer","endflag") #furniture and food
#Words<-c("startflag",rownames(Semantic_vectors)[sample(x=1:nrow(Semantic_vectors),size = 10,replace = F)],"endflag") # random list of words
#Words<-c("startflag","fear","temper","hatred","fury","happy","enrage","emotion","rage","hate","mean","ire","mad","wrath","calm","fight","endflag") #drm for anger
#Words<-c("startflag","sill","shade","screen","ledge","sash","door","view","house","glass","shutter","frame","breeze","curtain","pane","open","endflag") #drm for window
#Words<-c("startflag","rest","bed","nap","peace","drowsy","blanket","doze","tired","awake","snooze","yawn","slumber","snore","wake","dream","endflag") #drm for sleep
#Words<-c("startflag","jazz","horn","concert","orchestra","rhythm","sing","piano","band","note","instrument","art","sound","symphony","radio","melody","endflag") #drm for music
#Words<-c("startflag","fly","insect","animal","ugly","tarantula","poison","bug","bite","feelers","creepy","web","arachnid","small","fright","crawl","endflag") # drm for spider

## to see what words in the DRM lists are not in the semantic vectors
#DRMwordlists_Stadler1999[which(!tolower((DRMwordlists_Stadler1999))%in%rownames(Semantic_vectors))]

# identify the rows/drm words that have a word in the list that is not in the semantic vectors (problem if last word is invalid)
words_to_remove<-unique(which(!tolower((DRMwordlists_Stadler1999))%in%rownames(Semantic_vectors))%%nrow(DRMwordlists_Stadler1999))
DRMwordlists_Stadler1999<-DRMwordlists_Stadler1999[-c(words_to_remove),]
Stadler1999_results<-Stadler1999_results[-c(words_to_remove),]

drmword<-"black"
Words<-c('startflag',DRMwordlists_Stadler1999[toupper(drmword),], 'endflag')

# # for running a list n_sub times
# n_sub<-10
# 
# full_results<-matrix(0,nrow = 17,ncol = n_sub)
# time_taken_total<-c(rep(0,n_sub))
# for(i in 1:n_sub){
#   results<-Serial_recall(Words=Words, Vectors=Semantic_vectors, Alpha=alpha, Gamma=gamma, Kriterion=kriterion, EchoThresh = echothresh)
#   full_results[,i]<-results$results[,2]
#   time_taken_total[i]<-results$time_taken
# }

# for running a list just once
results<-Serial_recall(Words=Words, Vectors=Semantic_vectors, Alpha=alpha, Gamma=gamma, Kriterion=kriterion, EchoThresh = echothresh)

# for running the whole list of words
num_recalled<-c(rep(0,nrow(DRMwordlists_Stadler1999)))
crit_recalled<-c(rep(0,nrow(DRMwordlists_Stadler1999)))
for(i in 1:nrow(DRMwordlists_Stadler1999)){
  drmword<-rownames(DRMwordlists_Stadler1999)[i]
  Words<-c('startflag',DRMwordlists_Stadler1999[toupper(drmword),], 'endflag')
  results<-Serial_recall(Words=Words, Vectors=Semantic_vectors, Alpha=alpha, Gamma=gamma, Kriterion=kriterion, EchoThresh = echothresh)
  subtract<-0
  if("startflag"%in%results$results[,2]){subtract<-subtract+1}
  if("endflag"%in%results$results[,2]){subtract<-subtract+1}
  num_recalled[i]<-sum(results$results[,2]%in%results$results[,1])-subtract
  if(tolower(drmword)%in%results$results[,2]){crit_recalled[i]<-1}
}

# put results into a simple to read matrix
sim_results<-matrix(c(num_recalled/15*100,crit_recalled),ncol = 2,byrow = F)
rownames(sim_results)<-rownames(DRMwordlists_Stadler1999)
colnames(sim_results)<-c("Proportion Recalled","Crit Word Recalled")
# see if results correlate with actual results
cor.test(num_recalled,Stadler1999_results[,16])


# prints the words that are not in the first 1000 most similar words to the initial memory (after encoding)
results$results[!results$results[,2]%in%names(results$initialsimilarity[order(results$initialsimilarity[,1],decreasing = TRUE)[1:1000],1]),2]

drm_word<-tolower(drmword)
par(mfrow = c(3,3))
for(i in 1:ncol(results$similarities)){
  barplot(results$similarities[c(drm_word,Words),i],col = c(1,rep(2,length(Words))),las=2,ylim = c(0,.5))
}


#ploting similarities
top_n<-5
par(mfrow = c(3,3))
for(i in 1:ncol(results$similarities)){
  barplot(results$similarities[c(drm_word,names(tail(sort((results$similarities[,i])),top_n))),i],col = c(1,rep(2,top_n)),las=2,ylim=c(0,.5))
}


# this<-normalize(convolve(x=Semantic_vectors["apple",],y = rev(Semantic_vectors["table",]),conj = F,type = "circular"))
# that<-Convolve(Semantic_vectors["apple",],Semantic_vectors["table",])
# all(round(this,12)==round(that,12))

