num_words<-200

ntest<-2
ord_save<-rep(0,20)
for(z in 1:ntest){
# for consistency
#set.seed(4321)
# set up random words with orthonormal vectors
words<-matrix(data = rnorm(n = 2048*num_words,
                           mean = 0,
                           #sd = 1/((2048)^(1/2))),
                           sd = sqrt(1/2048)),
              nrow = num_words,
              ncol = 2048)


combs<-combn(num_words,2) # get all possible pairs (10 seconds ish?)

memory<-colSums(words*0.001) # create memory with all words summed in memory and weighted

associations<-rep(0,ncol(words)) # create new memory system to hold associations (temporary)

start<-Sys.time() # just to time how long the process takes

pb<-txtProgressBar(min = 0,max = ncol(combs)/1,style = 3) # make a progress bar

for(i in 1:(ncol(combs)/1)){ # for all pairs (takes an hour ish?)
  setTxtProgressBar(pb = pb,value = i) # update the progress bar
  associations<-associations+((0.001)*(convolve(x = words[combs[1,i],], # convolve the pair or words
                                                y = words[combs[2,i],], 
                                                conj = F, # non-conjegate convolution
                                                type = "circular"))) # circular
}
close(pb) # close the progress bar

time_taken<-Sys.time()-start # how long did the convolutions take?

initial_memory<-memory+associations # make the initial memory by summing the two vectors together

start_item<-rnorm(n=ncol(words), mean=0, sd = 1/((2048)^(1/2))) # make a "start of list" item
end_item<-rnorm(n=ncol(words), mean=0, sd = 1/((2048)^(1/2))) # make an "end of list" item


###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################


List_Length<-20 # how long is the study list
brad<-Sys.time()


#save<-c()
#ord_save<-rep(0,20)
#test<-4
#ntest<-30
#for(z in 1:test){
#for(y in 1:test){
#for(z in 1:ntest){
  
exp_memory<-initial_memory # set the experimental memory so that I don't have to redo the simulation every time
exp_memory<-matrix(exp_memory,nrow = 1) # confirm that the memory is treated as a matrix

# set up strength of items being added for the list of length below
lambda<-.99/1 # variable changes association strengths
#lambda<-.95/z # variable changes association strengths
omega<-0.9/1 # variable changes item strengths
#omega<-0.9/y # variable changes item strengths
beta<-0.35 # variable to set minimum item weight
y0<-0.65 #variable to set themaximum value of any studied item
association_strengths<-rep(0,List_Length+1) # make list of strengths for association
item_strengths<-rep(0,List_Length+1) # make list of strengths for items

for (i in 1:(List_Length+1)){ # calculate the weights for each words being studied
  association_strengths[i]<-1*(lambda^i) # association weights
  item_strengths[(List_Length+2-i)]<-beta+y0*(omega^(i-1)) # item weights
}

#association_strengths # print to check
item_strengths # print to check
#item_strengths[20]<-1 #testing why it isn't working

# update memory with study list
#set.seed(1234) # for consistency if desired
study_list<-sample(nrow(words),List_Length,replace = F) # sample a random set of words
#study_list<-1:List_Length # take the first words (it shouldn't matter really and make graphing easier)
#study_list<-sample(study_list) # randomize the order of the list

count<-1 # used for iterating
for (i in study_list){ # for all words in the study list
  if(count<-1){ # if this is the first trial
    exp_memory<-exp_memory+(start_item*(item_strengths[1]))*1 # put the start item in memory
    exp_memory<-exp_memory+(words[i,]*(item_strengths[count]))*1 # update memory with first list item
    exp_memory<-exp_memory+convolve(x = start_item,y = words[i,],conj = F,type = "circular")*association_strengths[count] # convolve the start item with the first study item and add to memroy
  }else{ # for every other trial after the first
    exp_memory<-exp_memory+(words[i,]*(item_strengths[count]))*1 # update memory with item
    exp_memory<-exp_memory+convolve(x = words[i,], # update memory with convolution with prior item
                                    y = words[study_list[count-1],], 
                                    conj = F,
                                    type = "circular")*association_strengths[count]
  }
  if(count == List_Length){ # if this is the last trial
    count<-count+1 # up the count 1 (will be list length+1)
    exp_memory<-exp_memory+convolve(x = end_item,y = words[i,],conj = F,type = "circular")*1#association_strengths[count] # convolve end item with last word
    exp_memory<-exp_memory+end_item*item_strengths[count] # add last item into memory
  }
  count<-count+1 # update the count on every trial
}


library(DescTools) # for the Closest function
recall_list<-c() # list of words recalled
scaling<-0.063 # scaling value used by paper
target_value <- 1
margin_of_error<-0.5# margin of error used by paper
count<-0 # count for iterations
cont<-T # Do we stop recall
cont2<-T # Do we use the start item as a probe
#par(mfrow = c(3,3))
probe<-(end_item) # set probe to end item initially

calc_mom_stren<-function(mem,probe,vocab){
  echo<-convolve(x = probe,y = mem,conj = T,type = "circular") # circular correlation
  sims<-echo%*%t(vocab) # calculate the similarity 
  curr_strength<-mem%*%t(vocab) # calculate current strength
  mom_stren<-sims+curr_strength # calculate the momentary strength
  return(mom_stren) # return values
}

while(cont){
  mom_strength<-calc_mom_stren(exp_memory,probe,words) # get the momentary strength
  choice<-Closest(x = mom_strength,a = target_value,which = T) # choose the word with a momentary strength closest to the target value
  if(mom_strength[choice] < target_value-margin_of_error | mom_strength[choice] > target_value+margin_of_error){ # if outside of the target range
    probe<-start_item # set the probe to the starting item
    if(cont2==F){cont1<-F} # if this is the second time we get here then stop the trial
    print("here") # for testing
    cont2<-F # set check to false
  }else{ # if there is a value close to 1
    recall_list<-c(recall_list,choice) # update the recall list
    exp_memory<-exp_memory+words[choice,]*c((1-(words[choice,]%*%t(exp_memory)))*scaling) #update memory with the word (with a scaling factor)
    associate<-convolve(x = words[choice,],y = probe,conj = F,type = "circular") # get the association (convolution) of the word with the probe
    exp_memory<-exp_memory+associate*c(1-(associate%*%t(exp_memory))*scaling) # update memory with the association (with scaling factor)
    probe<-words[choice,] # make the recalled word the new probe
  }
  if(count==40){ # for testing, makes the model stop eventually
    cont<-F
  }
  count<-count+1 # update the count for testing
}

ord_save<-ord_save+(study_list%in%recall_list)
#save<-c(save,sum(study_list%in%recall_list))
#}
#}
}
plot(ord_save/ntest)
lines(ord_save/ntest)

#save
Sys.time()-brad