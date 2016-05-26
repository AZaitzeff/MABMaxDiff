CC = "" 

#folder <- "/Users/ericmsch/Dropbox/Sawtooth/Bandit Code Files/"
#folder <- "C:/Users/ericmsch/Dropbox/Sawtooth/Bandit Code Files/"
folder <- paste0(CC,tools:::file_path_as_absolute("."),"/")

data1<-read.csv( file=paste(folder,"HB_120items.csv",sep="") )

utilsall<-as.matrix(data1[,-1:-2])
#dim(utilsall)
#utilsall<-matrix(seq(-2,2,length.out=nitems),ncol=nitems,byrow=T)


#utilsall
nitems<-40
#timestamp()

set.seed(pi)
utilsall2<-matrix(NA,nrow(utilsall),nitems)
subset<-sample(1:120,nitems)
#subset
#dim(utilsall2)
#utilsall2[,1:nitems]<-utilsall[,subset]

#for(i in 121:nitems)
#{
#  utilsall2[,i]<-sample(runif(1,0,.8)*utilsall[,sample(1:ncol(utilsall),1)]+runif(1,0,.8)*utilsall[,sample(1:ncol(utilsall),1)])
#}

#c(54,37,55,72,113,105,106,49,63,73)[10:1]
#top10trueindex<-tail(order(colMeans(utilsall)),30)[30:1]
#top10trueindex2<-tail(order(colMeans(utilsall2)),10)[10:1]
#top10trueindex
#top10trueindex2

write.csv(cbind(data1[,1:2],utilsall2),paste(folder,"HB_40items.csv",sep=""))

#top10trueindex<-tail(order(colMeans(utilsall)),10)