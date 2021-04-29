
# brad1<-c(.534,-0.875,0.473)
# brad2<-c(-1.125,-0.279,-1.121)
# brad3<-round(brad1%*%t(brad2),3)
# sum(brad3[1,1],brad3[2,3],brad3[3,2])
# sum(brad3[1,2],brad3[2,1],brad3[3,3])
# sum(brad3[1,3],brad3[2,2],brad3[3,1])
# con<-convolve(x=brad1,y=brad2,conj = F,type = 'circular')
# 
# brad4<-c(0.534,-0.875,0.473)
# brad5<-round((con)%*%t(brad4),3)
# sum(brad5[1,2],brad5[2,3],brad5[3,1])
# sum(brad5[1,3],brad5[2,1],brad5[3,2])
# sum(brad5[1,1],brad5[2,2],brad5[3,3])
# convolve(x=con,y = brad4,conj = T, type = "circular")

set.seed(1234)
# for random vectors
words<-matrix(data = rnorm(n = 2048*2000,
                           mean = 0,
                           sd = 1/((2048)^(1/2))),
              nrow = 2000,
              ncol = 2048)


combs<-combn(2000,2) # 10 seconds ish?

memory<-colSums(words*0.001)

associations<-rep(0,ncol(words))

start<-Sys.time()
for(i in 1:(ncol(combs)/1)){ # an hour ish?
#for(i in 1:10000){
  associations<-associations+((0.001)*(convolve(x = words[combs[1,i],], 
           y = words[combs[2,i],], 
           conj = F,
           type = "circular")))
}
time_taken<-Sys.time()-start

initial_memory<-memory+associations


start_item<-rnorm(n=ncol(words), mean=0, sd = 1/((2048)^(1/2)))
end_item<-rnorm(n=ncol(words), mean=0, sd = 1/((2048)^(1/2)))

# so I don't have to redo initial memory
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
List_Length<-20

sub_data<-matrix(ncol=List_Length, nrow = 0)
start<-Sys.time()
nsub<-100
for(sub in 1:nsub){



exp_memory<-initial_memory
exp_memory<-matrix(exp_memory,nrow = 1)


# set up strength of items being added for the list of length below
lambda<-.95/1
omega<-0.9/1
association_strengths<-rep(0,List_Length)
item_strengths<-rep(0,List_Length)

for (i in 1:(List_Length+1)){
  association_strengths[i]<-1*(lambda^i)
  item_strengths[(List_Length+2-i)]<-0.35+.65*(omega^(i-1))
}

association_strengths
#item_strengths

# update memory with study list
#set.seed(1234)
study_list<-sample(nrow(words),List_Length,replace = F)
#study_list<-1:List_Length
#study_list<-sample(study_list)
count<-1
for (i in study_list){
  exp_memory<-exp_memory+(words[i,]*(item_strengths[count]))*1

  if(count<-1){
    exp_memory<-exp_memory+convolve(x = start_item,y = words[i,],conj = F,type = "circular")*association_strengths[count]
  }else{
    exp_memory<-exp_memory+convolve(x = words[i,], 
                                  y = words[study_list[count-1],], 
                                  conj = F,
                                  type = "circular")*association_strengths[count]
  }
  if(count == List_Length){
    count<-count+1
    exp_memory<-exp_memory+convolve(x = end_item,y = words[i,],conj = F,type = "circular")*association_strengths[count]
    exp_memory<-exp_memory+end_item*item_strengths[count]
    }
  count<-count+1
}

# report/recall
library(DescTools)
recall_list<-c(2000) #makes no sense but making it work for now
scaling<-0.063
margin_of_error<-0.3
count<-0
cont<-T
cont2<-T
par(mfrow = c(3,3))
probe<-(end_item)
if(nrow(exp_memory)>2){exp_memory<-t(exp_memory)}
while(cont){
  #print(exp_memory)
  #print(probe)
  echo<-convolve(x = probe,y = exp_memory,conj = T,type = "circular")

  sim<-(echo)%*%t(words)
  momentary_strength<-sim+((exp_memory)%*%t(words))
  recall<-Closest(x = momentary_strength[-recall_list],a = 1,which = 1)
  #print(momentary_strength[-recall_list][recall])
  this_one<-which(momentary_strength == momentary_strength[-recall_list][recall])
  #print(this_one)
  #barplot(momentary_strength[1:50],ylim = c(-0.5,2),col = c(rep(1,this_one-1),2,rep(1,100)))
  if(momentary_strength[-recall_list][recall] < (1-margin_of_error) | momentary_strength[-recall_list][recall] > (1+margin_of_error)){
    if(cont2==F){
      #print("quit search")
      cont<-F
      break
    }else{
      probe<-start_item
      cont2<-F
      next
    }
  }else{
    
    recall_list<-c(recall_list,this_one)
    exp_memory<-exp_memory+(words[this_one,]*c((1-(exp_memory%*%(words[this_one,])))*scaling))
    exp_memory<-exp_memory+(convolve(x = probe,y = words[this_one,],conj = F,type = "circular")*
                              c((1-(exp_memory%*%(convolve(x = probe,y = words[this_one,],conj = F,type = "circular"))))*scaling)) 
    # gives a negative weight to add the association back in?
    
    # exp_memory<-exp_memory+
    #   1*
    #   ((words[this_one,]*
    #       (1-(exp_memory%*%(words[this_one,])))*scaling)+
    #      (convolve(x = probe, y=words[this_one,],conj = F,type = "circular")*
    #         (1-((convolve(x = probe,y = words[this_one,],conj = F,type = "circular")%*%t(exp_memory)))*scaling)))
  }
  if(nrow(exp_memory)>2){exp_memory<-t(exp_memory)}
  count<-count+1
  if(count==List_Length*2){cont<-F}
}


#sum(study_list%in%recall_list)

sub_data<-rbind(sub_data,c(study_list%in%recall_list))

}


#rowSums(sub_data)
par(mfrow=c(1,1))
plot(colSums(sub_data)/nsub)
lines(colSums(sub_data)/nsub)
Sys.time()-start












# for BEAGLE vectors
load("C:/Users/brads/Desktop/Projects/Mem_as_Hologram/BEAGLE.rda")

set.seed(1234)
sem_words<-sample(x = nrow(BEAGLE),size = 2000,replace = F)
sem_words_names<-rownames(BEAGLE)[sem_words]
sem_words<-matrix(data = BEAGLE[sem_words,],
              nrow = 2000)
rownames(sem_words)<-sem_words_names


combs<-combn(2000,2) # 10 seconds ish?

sem_memory<-colSums(sem_words*0.001)

sem_associations<-rep(0,ncol(sem_words))

sem_start<-Sys.time()
for(i in 1:(ncol(combs)/1)){
  #for(i in 1:10000){
  sem_associations<-sem_associations+((0.001)*(convolve(x = sem_words[combs[1,i],], 
                                               y = sem_words[combs[2,i],], 
                                               conj = F,
                                               type = "circular")))
}
sem_time_taken<-Sys.time()-sem_start

sem_initial_memory<-sem_memory+sem_associations



# T1<-c()





