#set.seed(1)

start<-Sys.time()
num<-100

save<-c(0,0,0)

for(i in 1:num){
words<-matrix(data = rnorm(n = 200*100,
                           mean = 0,
                           sd = sqrt(1/200)),
                           #sd = 1/((2048)^(1/2))), # all equivelent to 8 decimals
                           #sd = (1/2048)^(1/2)),
                           #sd = sqrt(1/2048)),
              nrow = 100,
              ncol = 200)


i_memory<-colSums(words*0.01) # create memory with all words summed in memory and weighted

par(mfrow = c(2,2))


memory<-i_memory
hold1<-memory%*%t(words)
plot(hold1[1,], ylim = c(-.5,.5),main = "All Items Added at Weight 0.01")



memory<-i_memory+words[1,]*.35
hold2<-memory%*%t(words)
plot(hold2[1,], ylim = c(-.5,.5),main = "Item 1 Added Again at value 0.35")
d2<-hold2-hold1


memory<-i_memory+words[2,]*.55
hold3<-memory%*%t(words)
plot(hold3[1,], ylim = c(-.5,.5),main = "Item 2 Added Again at value 0.55")
d3<-hold3-hold1


memory<-i_memory+words[3,]*.8
hold4<-memory%*%t(words)
plot(hold4[1,], ylim = c(-.5,.5),main = "Item 3 added as value 0.80")
d4<-hold4-hold1


par(mfrow = c(1,3))
simTo1<-words[1,]%*%t(words[c(-1),])
simTo2<-words[2,]%*%t(words[c(-2),])
simTo3<-words[3,]%*%t(words[c(-3),])

plot(x=simTo1,y = d2[,c(-1)], ylim = c(-.2,.2),xlim = c(-.3,.3))
save[1]<-save[1]+lm(d2[,c(-1)]~simTo1[1,])$coefficients[2]
plot(x=simTo2,y = d3[,c(-1)], ylim = c(-.2,.2),xlim = c(-.3,.3))
save[2]<-save[2]+lm(d3[,c(-1)]~simTo2[1,])$coefficients[2]
plot(x=simTo3,y = d4[,c(-1)], ylim = c(-.2,.2),xlim = c(-.3,.3))
save[3]<-save[3]+lm(d4[,c(-1)]~simTo3[1,])$coefficients[2]
}

save/num

Sys.time()-start