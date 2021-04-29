##########################################
# Get BEAGLE context vectors
##########################################

# Clear the work environment
rm(list = ls())

# Parameters
n <- 1000

# Assign corpus
Corpus <- "final_text.txt"

# Download a local copy of the corpus 
#download.file("https://www.dropbox.com/s/x8laqkn2jk05xyp/tasasentdocs.txt?dl=0", "tasadocspara.txt")

# Get unique words in the corpus
Unique_words <- unique(scan(file=Corpus, what="char", sep=""))
w <- length(Unique_words)

# Get all sentences in the corpus
Conversations <- scan(file=Corpus, what="char", sep="\n")

# Construct the environmental word vectors  
Words <- matrix(rnorm(w*n, 0, 1/sqrt(n)), w, n)
rownames(Words) <- Unique_words

# Construct BEAGLE semantic vectors (excluding own word)
Start <- Sys.time()
pb<-txtProgressBar(min = 0,max = length(Conversations),initial = 0,style = 3)
BEAGLE <- matrix(0, nrow(Words), ncol(Words))
rownames(BEAGLE) <- rownames(Words)
for (i in 1:length(Conversations)) {
  s <- strsplit(Conversations[i], split=" ")[[1]]
  if (length(s) > 1) {
    word_indices <- match(s, rownames(Words))
    v <- colSums(Words[word_indices,])
    for (j in 1:length(s)) {
      BEAGLE[s[j],] <- BEAGLE[s[j],] + v - Words[s[j],]
    }
  }
  setTxtProgressBar(pb = pb,value = i)
}
close(pb)
End <- Sys.time()
print(End-Start)

# The rest of this program just illustrates some ways of looking at semantic relationships

# Cosine similarity
Cosine <- function (x, y) {
  C <- sum(x*y) / sqrt(sum(x*x) * sum(y*y))
  return(C)
}

# Probe list from Jones and Mewhort (2007, Figure 3)
Word_list <- c("financial","savings","finance","pay","invested",
               "loaned","borrow","lend","invest","investments",
               "bank","spend","save","astronomy","physics",
               "chemistry","psychology","biology","scientific",
               "mathematics","technology","scientists","science",
               "research","sports","team","teams","football",
               "coach","sport","players","baseball","soccer",
               "tennis","basketball")

# Set graphing parameters (two rows by 1 column)
par(mfrow=c(2,1))

# Plot results as a correlation plot with k clusters marked 
corrplot:: corrplot(corr=multicos(x=Word_list, tvectors=BEAGLE), 
                    order="hclust", 
                    tl.col="black", 
                    tl.cex=1)
corrplot:: corrRect.hclust(corr=multicos(x=Word_list, tvectors=BEAGLE), 
                           k=3)

# Plot results as a 2d MDS solution
LSAfun:: plot_wordlist(Word_list, 
                       tvectors=BEAGLE, 
                       method="MDS", 
                       dims=2)

# Save the vectors to an .rda file: load("BEAGLE.rda")
save(BEAGLE, file="BEAGLE_20newsCorpus.rda")
