
# rev 2016-04-09 ES

#MAC:
folder <- "/Users/azaitzeff/Documents/MultiArmedBandits"
#PC:
#folder <- "C:/Users/ericmsch/Dropbox/Sawtooth/Bandit Code Files/"

data1<-read.csv( file=paste(folder,"HB_120items.csv",sep="") )
#data1<-rbind(c(1,1000,seq(-2,2,length.out=120)))
MDDesign<-read.csv( file=paste(folder,"30itemMD_MD30_Design.csv",sep="") ) 
#MDDesign<-read.csv( file=paste(folder,"MD_MD1_Design.csv",sep="") ) 


### ========================
### Save true utilities 
### ========================

niters = 100
nrespondents = 1020
nitems=120
nitemsperset=5
nquestions=18
nitemsperbot=30

nperupdate = 20
nupdates = nrespondents/nperupdate
respseq = seq(nperupdate,nrespondents,by=nperupdate)



A<-rbind(diag(nitems-1),-1)
utilsall<-as.matrix(data1[,-1:-2])
dim(utilsall)
# Testing for parameter recovery: use evenly spread utilities
#utilsall<-matrix(seq(2,-2,length.out=nitems),ncol=nitems,byrow=T)

pdf( paste(folder,"trueutils.pdf",sep=""),width=5 )
	plot(1:nitems,sort(colMeans(utilsall) ),pch=1,frame= FALSE, 
		axes=F,	xlab="items",ylab="utility on logit scale")
	axis(1,at=c(1,seq(20,120,by=20)),labels=c(1,seq(20,120,by=20)) )
	axis(2)
		points(1 + nitems - 10:1, sort(colMeans(utilsall))[1 + nitems - 10:1], col="blue" )
		points(1 + nitems - 3:1, sort(colMeans(utilsall))[1 + nitems - 3:1], col="red" )
dev.off()

pdf( paste(folder,"trueutilsall.pdf",sep=""),width=5 )
	boxplot( utilsall , pch=".", whisklty=1, frame= FALSE, boxwex=.5,
		axes=F, xlab="items",ylab="utility on logit scale") 
	axis(1,at=c(1,seq(20,120,by=20)),labels=c(1,seq(20,120,by=20)) )
	axis(2)
dev.off()

top10trueindex<-tail(order(colMeans(utilsall)),10)[10:1]
top3trueindex <-tail(order(colMeans(utilsall)), 3)[ 3:1]



### ================== 
## Hit rate comparison
### ==================


## Constants common across all methods

niters = 100
nrespondents = 1020
nitems=120
nitemsperset=5
nquestions=18
nitemsperbot=30

nperupdate = 20
nupdates = nrespondents/nperupdate
respseq = seq(nperupdate,nrespondents,by=nperupdate)

	

hrhist_comp_by20 <- NULL
#paste(" (TS - 120 items - 30 items per resp - split ", nreg, " TS ", nbig, " TSx", TSgamma, ")",sep="")

filesall <- c( 
"(fixed_sparse - 120 items - 30 items per resp - misinf 0 )",
"(TS - 120 items - 30 items per resp - split 30 TS 0 TSx10)",
"(TS - 120 items - 30 items per resp - split 20 TS 10 TSx10)",
"(TS - 120 items - 30 items per resp - split 20 TS 10 TSx20)",
"(TS - 120 items - 30 items per resp - split 15 TS 15 TSx10)",
"(TS - 120 items - 30 items per resp - split 15 TS 15 TSx20)",
"(fixed_sparse - 120 items - 30 items per resp - misinf 50 )",
"(TS - 120 items - 30 items per resp - split none - misinformed)",
"(TS - 120 items - 30 items per resp - split 20 TS 10 TSx10 - misinf 50 )",
"(TS - 120 items - 30 items per resp - split 20 TS 10 TSx20 - misinf 50 )"
)
	
for (filesuffix_in in filesall){ # loop over all hitrate files

	if (filesuffix_in == "(fixed_sparse - 120 items - 30 items per resp - misinf 0 )") {TSeps = NA; TSgamma = NA; misinformed=""; method="FixedSparse"; method_misinformed=paste(method,misinformed)}
	if (filesuffix_in == "(TS - 120 items - 30 items per resp - split 30 TS 0 TSx10)")  {TSeps = 0; TSgamma = 1; misinformed=""; method="edTS (e=0,d=1)"; method_misinformed=paste(method,misinformed)}
	if (filesuffix_in == "(TS - 120 items - 30 items per resp - split 20 TS 10 TSx10)") {TSeps = 1/3; TSgamma = 10; misinformed=""; method="edTS (e=10/30,d=10)"; method_misinformed=paste(method,misinformed)}
	if (filesuffix_in == "(TS - 120 items - 30 items per resp - split 20 TS 10 TSx20)") {TSeps = 1/3; TSgamma = 20; misinformed=""; method="edTS (e=10/30,d=20)"; method_misinformed=paste(method,misinformed)}
	if (filesuffix_in == "(TS - 120 items - 30 items per resp - split 15 TS 15 TSx10)") {TSeps = 1/2; TSgamma = 10; misinformed=""; method="edTS (e=15/30,d=10)"; method_misinformed=paste(method,misinformed)}
	if (filesuffix_in == "(TS - 120 items - 30 items per resp - split 15 TS 15 TSx20)") {TSeps = 1/2; TSgamma = 20; misinformed=""; method="edTS (e=15/30,d=20)"; method_misinformed=paste(method,misinformed)}
	if (filesuffix_in == "(fixed_sparse - 120 items - 30 items per resp - misinf 50 )") {TSeps = NA; TSgamma = NA; misinformed="misinform_50"; method="FixedSparse"; method_misinformed=paste(method,misinformed)}
	if (filesuffix_in == "(TS - 120 items - 30 items per resp - split none - misinformed)") {TSeps = 0; TSgamma = 1; misinformed="misinform_50"; method="edTS (e=0,d=1)"; method_misinformed=paste(method,misinformed)}
	if (filesuffix_in == "(TS - 120 items - 30 items per resp - split 20 TS 10 TSx10 - misinf 50 )") {TSeps = 1/3; TSgamma = 10; misinformed="misinform_50"; method="edTS (e=10/30,d=10)"; method_misinformed=paste(method,misinformed)}
	if (filesuffix_in == "(TS - 120 items - 30 items per resp - split 20 TS 10 TSx20 - misinf 50 )") {TSeps = 1/3; TSgamma = 20; misinformed="misinform_50"; method="edTS (e=10/30,d=20)"; method_misinformed=paste(method,misinformed)}
	
	# TSeps; TSgamma; Misinformed; Method
	nbig = round(30*TSeps)
	nreg = 30-nbig
	
	print(filesuffix_in)
	hr_in <- read.csv( paste(folder,"DataDump/hrhist ",filesuffix_in,".csv",sep="") )
	hr_cur <- hr_in[-1,] # the first line below headers is always empty
	hr_out <- data.frame(
		method = method, misinformed = misinformed, method_misinformed=method_misinformed,
		nbig=nbig, eps = round(TSeps,2), gamma = TSgamma,
		respondent = respseq,
		hr_top3 = tapply(hr_cur$hr_top3,INDEX=hr_cur$respondent,FUN=mean),
		hr_top10 = tapply(hr_cur$hr_top10,INDEX=hr_cur$respondent,FUN=mean)
	)
	hrhist_comp_by20 <- rbind(hrhist_comp_by20,hr_out)

} # end loop over all hitrate files

hrhist_comp_by20$method <- factor(hrhist_comp_by20$method)
hrhist_comp_by20$misinformed <- factor(hrhist_comp_by20$misinformed)
hrhist_comp_by20$nbig <- factor(hrhist_comp_by20$nbig)
hrhist_comp_by20$eps <- factor(hrhist_comp_by20$eps)
hrhist_comp_by20$gamma <- factor(hrhist_comp_by20$gamma)

write.csv( hrhist_comp_by20, file=paste(folder,"hrhist_comp_120itemTSandFixed_11methods_by20.csv",sep="") )

by100 <- hrhist_comp_by20$respondent %in% seq(20,1020,by=100)
hrhist_comp <- hrhist_comp_by20[by100,] 

write.csv( hrhist_comp, file=paste(folder,"hrhist_comp_120itemTSandFixed_11methods.csv",sep="") )


# Hit rate over time plots
pdf(paste(folder,"hr10_edTSandFixed_comp.pdf",sep=""))
 p <- ggplot(data=hrhist_comp, aes(x=respondent,y=hr_top10) ) + theme_bw()
 p + geom_line(aes(col=method), subset(hrhist_comp,misinformed == "")  )
 p + geom_line(aes(col=method)) + facet_grid(facets=~misinformed) 
 
 ggplot(subset(df,fruit == "apple"),aes(x = year,y = qty,colour = fruit)) + 
    geom_point() + 
    scale_colour_discrete(drop = FALSE)
dev.off()

pdf(paste(folder,"hr3_edTSedTSandFixed_comp.pdf",sep=""))
 q <- ggplot(data=hrhist_comp, aes(x=respondent,y=hr_top3) ) + theme_bw()
 q + geom_line(aes(col=method)) + facet_grid(facets=~misinformed)
dev.off()



# Hit rate tables for sparse, TS, eps-diff TS early/late

hrsummary <- data.frame(method_misinformed=levels(hrhist_comp_by20$method_misinformed))

when = 260 
hrsub <- hrhist_comp_by20[hrhist_comp_by20$respondent %in% when,]
tapply( hrsub$hr_top3, FUN=mean, INDEX=hrsub$method_misinformed )
hrsummary$top3_resp260 <- tapply( hrsub$hr_top3, FUN=mean, INDEX=hrsub$method_misinformed )

when = 1020 
hrsub <- hrhist_comp_by20[hrhist_comp_by20$respondent %in% when,]
tapply( hrsub$hr_top3, FUN=mean, INDEX=hrsub$method_misinformed )
hrsummary$top3_resp1020 <- tapply( hrsub$hr_top3, FUN=mean, INDEX=hrsub$method_misinformed )

when = 260 
hrsub <- hrhist_comp_by20[hrhist_comp_by20$respondent %in% when,]
tapply( hrsub$hr_top10, FUN=mean, INDEX=hrsub$method_misinformed )
hrsummary$top10_resp260 <- tapply( hrsub$hr_top10, FUN=mean, INDEX=hrsub$method_misinformed )

when = 1020 
hrsub <- hrhist_comp_by20[hrhist_comp_by20$respondent %in% when,]
tapply( hrsub$hr_top10, FUN=mean, INDEX=hrsub$method_misinformed )
hrsummary$top10_resp1020 <- tapply( hrsub$hr_top10, FUN=mean, INDEX=hrsub$method_misinformed )

hrsummary



### ====================================
## Counting how often items appear: which items each respondent sees
### ====================================

res$ex$respbyitem 
# respbyitem is same as itemlistperbot

# Given itemlistperbot

 # itemlistperbot <- data.frame(
 #    respondent = rep(1:nbot,each=nitemsperbot),
 #    item = c(t(botitemlist)),
 #    itemrank = order(order(colMeans(utilsall),decreasing=T))[c(t(botitemlist))],
 #    itemutil = colMeans(utilsall)[ c(t(botitemlist)) ]
 #    )
 #  itemcount <- rbind(itemcount, c( table(botitemlist))  )

  plot(itemlistperbot$respondent,itemlistperbot$itemrank,pch='.',xlab="Respondents",ylab="Items (1 = True Best)")
  points( rep(1021,30) , 1:30,  pch=".",col="red") 

#png(paste(folder,"DataDump/",timestampstr," - ",nbot," Resp Run Progression Plot",filesuffix,".png",sep=""), width=8, height=8, units="in", res=600)
 png(file=paste(folder,"itembyresp_ex_fixedvadaptive.png",sep=""), width=4, height=4, units="in", res=600))
  plot(itemlistperbot$respondent,itemlistperbot$itemrank,pch='.',xlab="Respondents",ylab="Items (1 = True Best)")
  points( rep(1021,30) , 1:30,  pch=".",col="red") 
dev.off() #only 129kb in size
pdf(file=paste(folder,"itembyresp_ex_fixedvadaptive.png",sep=""), width=4, height=4, units="in")
  plot(itemlistperbot$respondent,itemlistperbot$itemrank,pch='.',xlab="Respondents",ylab="Items (1 = True Best)")
  points( rep(1021,30) , 1:30,  pch=".",col="red") 
dev.off() #only 129kb in size
  


### ================== 
## Learning parameters
### ==================


# load in Ex bhist and Ex sehist
bhist <- read.csv(bhist, paste(folder,"DataDump/Ex bhist (TS - 120 items - 30 items per resp - split 30 none - misinf 0).csv",sep="") )
#or
load(res, paste(folder,"DataDump/res (TS - 120 items - 30 items per resp - split 30 none - misinf 0).RData",sep="") )
res$bhist$iter==niters 


names(sehist) <- c("iter","respondent", paste("item", 1:nitems, sep="_") )
names(bhist) <- c("iter","respondent", paste("item", 1:nitems, sep="_") )

iterindex <- res$bhist$iter==niters 

bh <- res$bhist[ iterindex , -c(1,2)] 
seh <- res$seist[ iterindex , -c(1,2)] 
tauh <- 1/seh^2


itemex <- order(colMeans(utilsall))[ c(30,119) ] #30th worst; 119th worst = 91st best; 2nd best
trueutils_itemex <- colMeans(utilsall)[itemex]
# colMeans(utilsall)[c("Item97","Item63")]
#     Item97     Item63 
# -0.9008919  1.5909986 


# 
plot( bh[ , itemex[1] ] )
plot( seh[ , itemex[1] ] )
plot( tauh[ , itemex[1] ] )

plot( bh[ , itemex[2] ] )
plot( seh[ , itemex[2] ] )
plot( tauh[ , itemex[2] ] )


bh_fixedsparse
bh_TS

# for two methods (FixedSparse and ThompsonSampling)
# for two items (high and low)
# for each point in time 20,...,1020

learningex <- data.frame(
	method <- factor( rep(c("FixedSparse","TS"), each=2*51) ), # FixedSparse / ThompsonSampling
	item_true_utility <- factor(  rep( rep(c("Low_Utility","High_Utility"),each=51), 2)  ), # High / Low
	item_true_rank <- nitems + 1 - rep( rep(itemex,each=51), 2),
	respondent <- rep(respseq, 2*2),
	bhist <- c( bh_fixedsparse[,itemex[1]], bh_fixedsparse[,itemex[2]], bh_TS[,itemex[1]], bh_TS[,itemex[2]] ) ,
	sehist <-  c( seh_fixedsparse[,itemex[1]], seh_fixedsparse[,itemex[2]], seh_TS[,itemex[1]], seh_TS[,itemex[2]] ) #
	)

ggplot(data=learningex, group=method ) + geom_line(x=respondent,y=bhist)
ggplot(data=learningex, group=method ) + geom_line(x=respondent,y=bhist)



# plotting all of the values of bhist

bhist$when <- rep(NA,nrow(bhist))
bhist$when[bhist$respondent] <- "early"
bhist$when[bhist$respondent] <- "late"
bh <- bhist[bhist$iter==1,]
brange <- range(bh[,-(1:2)])
plot(bh$respondent,bh$item_1 ,ylim=brange)
itemset = order( bh[51,-(1:2)] )[ c(25,50,75,100,110,120) ]
for (k in itemset){
	lines(bh$respondent, bh[,2 + k] )
}




