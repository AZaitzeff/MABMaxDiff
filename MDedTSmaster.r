MDedTSmaster <- function( arguments ) { # BEGIN FUNCTION MDedTSmaster
# Ver 2016-04-06 ES

folder <- arguments$folder
niters <- arguments$niters 
nrespondents <- arguments$nrespondents 
nitems<- arguments$nitems
nitemsperset<- arguments$nitemsperset
nquestions<- arguments$nquestions
nitemsperbot<- arguments$nitemsperbot
TSeps <- arguments$TSeps 
TSgamma <- arguments$TSgamma 
fixed_sparse <- arguments$fixed_sparse 
nmisinformed <- arguments$nmisinformed 
misinformed_baditem <- arguments$misinformed_baditem 
SAVEFLAG <- arguments$SAVEFLAG
greed <- arguments$greed
numrand <- arguments$numrand

library(mlogit)
library(MASS)

timestamp()


###### ARGUMENTS ###### 

#MAC:
# folder <- "/Users/ericmsch/Dropbox/Sawtooth/Bandit Code Files/"
#PC:
# folder <- "C:/Users/ericmsch/Dropbox/Sawtooth/Bandit Code Files/"

if (nitems==120){
data1<-read.csv( file=paste(folder,"HB_120items.csv",sep="") )
#data1<-rbind(c(1,1000,seq(-2,2,length.out=120)))
#head(data1)
MDDesign <-read.csv( file=paste(folder,"30itemMD_MD30_Design.csv",sep="") ) 
} else if (nitems==300){
data1<-read.csv( file=paste(folder,"HB_300items.csv",sep="") )
#data1<-rbind(c(1,1000,seq(-2,2,length.out=120)))
#head(data1)
MDDesign <-read.csv( file=paste(folder,"30itemMD_MD30_Design.csv",sep="") )
} else if (nitems==40){
data1<-read.csv( file=paste(folder,"HB_40items.csv",sep="") )
#data1<-rbind(c(1,1000,seq(-2,2,length.out=120)))
#head(data1)
MDDesign <-read.csv( file=paste(folder,"20itemMD_MD20_Design.csv",sep="") ) 
} else{
warning('We do not support that number of items')
}
outfolder <- folder
# niters <- 100
# nrespondents <- 1020
# nitems<-120
# nitemsperset<-5
# nquestions<-18
# nitemsperbot<-30

# fixed_sparse <- FALSE

# TSeps <- 1/3 # 0 is default, not split
# TSgamma <- 10 # 1 is default, not diffuse
TSnbig <- round(TSeps * nitemsperbot) # 10 =  (1/3) * 30 
TSnreg <- nitemsperbot - TSnbig # 20

# nmisinformed <- 50 # for misinformed start nmisinformed > 0
# misinformed_baditem <- 30 # 30th out of 120th best utility replaces all the top 3 items # for misinformed start
misinf <- (nmisinformed > 0)

###### END ARGUMENTS ###### 


## make sure consistent with  nitems, nitemsperbot, TSeps, TSgamma;
if(fixed_sparse==TRUE){
filesuffix <- paste(" (fixed_sparse - ",nitems, " items - ",nitemsperbot, " items per resp - misinf ", nmisinformed, " )",sep="")
}
else if(numrand >20 ){
filesuffix <- paste(" (learnthenearn - ",nitems, " items - ",nitemsperbot, " items per resp - misinf ", nmisinformed, " - alpha ", numrand/1000, " )",sep="")
}
else if(greed==TRUE){
filesuffix <- paste(" (greedy - ",nitems, " items - ",nitemsperbot, " items per resp - split ", TSnreg, " TS ", TSnbig, " TSx", TSgamma, " - misinf ", nmisinformed," )",sep="")
}
else{
filesuffix <- paste(" (TS - ",nitems, " items - ",nitemsperbot, " items per resp - split ", TSnreg, " TS ", TSnbig, " TSx", TSgamma, " - misinf ", nmisinformed," )",sep="")
}
# filesuffix <- paste(" (TS - 120 items - 30 items per resp - split none - misinformed)",sep="")

#filesuffix <- paste(" (TS - 120 items - 30 items per resp - split ", TSnreg, " TS ", TSnbig, " TSx", TSgamma, ")",sep="")


# simschedule: 
#   values = number of customers ('bots') 
#   length = number of iterations 
## base case is 100 iterations of 1020 'bots'

# simschedule<-rep(1020,100)
 simschedule<-rep(nrespondents,niters)
# simschedule<-rep(1020,1)


A<-rbind(diag(nitems-1),-1)
utilsall<-as.matrix(data1[,-1:-2])
dim(utilsall)
# Testing for parameter recovery: use evenly spread utilities
#utilsall<-matrix(seq(2,-2,length.out=nitems),ncol=nitems,byrow=T)


top10trueindex<-tail(order(colMeans(utilsall)),10)[10:1]
top3trueindex <-tail(order(colMeans(utilsall)), 3)[ 3:1]

# Keep track results:
# -- bhist = b is estimated mean coefficient
# -- hrhist = hr is hit rate, proportion of true top utilities captured by est mean coefs
# -- sehist = se is standard error of coefficient
bhist <- integer(0) #rbind(rep(0,nitems+1))
hrhist <- integer(0)
sehist <- integer(0)


itemcount <- NULL

#timestamp()

#l<-1 
timestart <- Sys.time()
for(l in 1:length(simschedule)) 
{
  nbot<-simschedule[l]
  if(nbot!=nrow(data1))
  {     
    simsample<-sample(1:nrow(utilsall),nbot,replace=TRUE)
    utils<-utilsall[simsample,]      
  }  else
  {
    utils<-utilsall
    simsample<-1:nrow(utilsall)
  }
  
  designmatrix<-matrix(NA,nbot,nitemsperset*nquestions)
  botitemlist<-matrix(NA,nbot,nitemsperbot)
  results<-matrix(NA,nbot*nquestions,nitemsperset*1+6)
  
  ID  <- rep(1:nbot,each=2*nitemsperset*nquestions)
  Set <- rep(1:nquestions,nbot,each=2*nitemsperset)
  
  nextitemlist<-NULL  
  
  bwindex<- rep(c(rep("Best",nitemsperset),rep("Worst",nitemsperset)),nbot*nquestions)
  
  length(bwindex)
  
  
  
  #colnames(results)<-c("Choice1","Choice2","Choice3","Choice4","Choice5","Best","Worst","PopUtil1","PopUtil2","PopUtil3","PopUtil4","PopUtil5","NoiseB1","NoiseB2","NoiseB3","NoiseB4","NoiseB5","NoiseW1","NoiseW2","NoiseW3","NoiseW4","NoiseW5","CompUtilB1","CompUtilB2","CompUtilB3","CompUtilB4","CompUtilB5","CompUtilW1","CompUtilW2","CompUtilW3","CompUtilW4","CompUtilW5","RespIDNum","QNum")
  colnames(results)<-c("Choice1","Choice2","Choice3","Choice4","Choice5","Best","Worst","BestInd","WorstInd","RespIDNum","QNum")
  for(i in 1:nbot)
  {


    #Estimate incremental scores.
    if(i %% 20 == 1 & i >= 20)
    { ## BEGIN if i == 21, 41, 61, ...
      #Works - parameter estimation
      resultssub<-results[1:((i-1)*nquestions),]
      
      Xb<-matrix(0,(i-1)*nquestions*nitemsperset,nitems)
      Xw<-matrix(0,(i-1)*nquestions*nitemsperset,nitems)
      X<-matrix(0,(i-1)*nquestions*nitemsperset*2,nitems-1)
      
      y <-rep(0,(i-1)*nquestions*nitemsperset*2)
      yb<-rep(0,(i-1)*nquestions*nitemsperset)
      yw<-rep(0,(i-1)*nquestions*nitemsperset)
      
      Xb[cbind(1:(nrow(resultssub)*nitemsperset),c(t(resultssub[,1:5])))]<-1    
       
      Xb[c(t(resultssub[,1:5]))==nitems,]<--1
      Xw<- -Xb
      
      bwindexsub <- bwindex[1:(nrow(Xb)*2)]
      
      X[bwindexsub=="Best",]<-Xb[,-nitems]
      X[bwindexsub=="Worst",]<-Xw[,-nitems] 
      
      yb[seq(0,(i-1)*nquestions*nitemsperset-1,by=nitemsperset)+resultssub[,"BestInd"]]<-1
      yw[seq(0,(i-1)*nquestions*nitemsperset-1,by=nitemsperset)+resultssub[,"WorstInd"]]<-1       
      
      y[bwindexsub=="Best"] <-yb
      y[bwindexsub=="Worst"]<-yw  
      
      IDsub  <- rep(1:(i-1),each=2*nitemsperset*nquestions)
      Setsub <- rep(1:nquestions,(i-1),each=2*nitemsperset)
      
      dallsub<- data.frame(
        IDsub  = IDsub,
        Setsub = Setsub,
        SetBestWorst = bwindexsub,
        Choice = y,
        X
      )
      
      system.time( 
        md1 <- mlogit(formula = y ~  X | 0, data=dallsub, alt.levels=paste(1:nitemsperset), shape="long")
      )      
      
      if (i %% 100 == 1) {
        timestamp();
        print(i-1);
      }
      
      #A is a nitem by (nitems-1) matrix to transform the dummy coded vcov matrix back to nitems by nitems
      est_vcov <- A%*%vcov(md1)%*%t(A)
      
      
      est_mean <- A%*%md1$coef
      
      b <- est_mean 

      se <- sqrt(diag(est_vcov))    
      
      bhist <- rbind(bhist,c(l,i-1,b))
      sehist <- rbind(sehist, c(l,i-1,se))
      
      top10simindex<-tail(order(b),10)
      hitratetop10<-mean(top10simindex %in% top10trueindex)
      
      top3simindex<-tail(order(b),3)
      hitratetop3<-mean(top3simindex %in% top3trueindex)
      
      hrhist<-rbind(hrhist,c(l,i-1,hitratetop3,hitratetop10))
    } ## END if i == 21, 41, 61, ...
    
    ## When should we ensure balance of items?

    # Initial period before updates
    # IF fixed_sparse = TRUE, then always do balance of items

    if (fixed_sparse==TRUE || i <= numrand)
    { 
      if(length(nextitemlist) < nitemsperbot)
      {
        nextitemlist<-c(nextitemlist,sample(1:nitems))
      }
      #print(MDDesign$Version==(i-1)%%max(MDDesign$Version)+1)
      #  nextitemlist <-NULL
      botitemlist[i,]<-nextitemlist[1:nitemsperbot]
      designmatrix[i,]<- nextitemlist[c(t(as.matrix(MDDesign[MDDesign$Version==(i-1)%%max(MDDesign$Version)+1,-(1:2)])))]
      nextitemlist    <- nextitemlist[-(1:(nitemsperbot))]
      
    } else
    {  # not initial period, not an update  
      
      
      #############    
      # eps-diffuse Thompson Sampling
      #
      # TSeps = 10/30  # how many are sampled from a diffuse distribution? e.g., 10 out of 30 items are drawn from a diffuse
      # TSgamma = 10  # how diffuse is it? e.g., the diffuse distn has 10 * standard deviation of regular distn
      # ----------
      # Regular Thompson Sample
      if(greed==FALSE){
        uk_samp_cur <- mvrnorm(n = 1, 
                               mu = est_mean, 
                               Sigma = est_vcov )
      }
      else{
        uk_samp_cur <- mvrnorm(n = 1, 
                               mu = est_mean, 
                               Sigma = est_vcov*.00001 )
      }
      
      # Diffuse Thompson Sample
      # TSgamma = 10
      uk_samp_cur_big <- mvrnorm(n = 1, 
                                 mu = est_mean, 
                                 Sigma = TSgamma*est_vcov )

      # How often is each item
      # (1:nitems)[
      #   est_mean + TSgamma*diag(est_vcov) > 
      #   est_mean[which.max(est_mean)] - TSgamma*est_vcov[which.max(est_mean),which.max(est_mean)]]
      
      # (colMeans(utilsall))[(1:nitems)] [ 
      #   est_mean + TSgamma*diag(est_vcov) > 
      #   est_mean[which.max(est_mean)] - TSgamma*est_vcov[which.max(est_mean),which.max(est_mean)]
      # ]

      # 20-10 Split
      # - Take top 20 from regular Thompson Sample (uk_samp_cur)
      # - Take top 10 from diffuse Thompson Sample (uk_samp_cur_big), 
      #    excluding 20 items already taken from regular Thompson Sample
      # #-----
      # cur_TS_order <- order(uk_samp_cur,decreasing=T)
      # cur_TSbig_order <- order(uk_samp_cur_big,decreasing=T)
      # cur_TS_overlap <- cur_TSbig_order %in% cur_TS_order[1:20]
      # nextitemlist <- c( cur_TS_order[1:20], cur_TSbig_order[!cur_TS_overlap][1:10] )
      # #-----

      # TSeps 
      # TSnbig
      # TSnreg
      #-----
      if (TSnbig > 0 && TSnreg>0){
        cur_TS_order <- order(uk_samp_cur,decreasing=T)
        cur_TSbig_order <- order(uk_samp_cur_big,decreasing=T)
        cur_TS_overlap <- cur_TSbig_order %in% cur_TS_order[1:TSnreg]
        nextitemlist <- c( cur_TS_order[1:TSnreg], cur_TSbig_order[!cur_TS_overlap][1:TSnbig] )
        #--- previous four lines in one line:
        ## nextitemlist <- c(order(uk_samp_cur,decreasing=T)[1:TSnreg],order(uk_samp_cur_big,decreasing=T)[!order(uk_samp_cur_big,decreasing=T) %in% order(uk_samp_cur,decreasing=T)[1:TSnreg]][1:TSnbig])
      } else if(TSnreg ==0){
        cur_TS_order <- order(uk_samp_cur_big,decreasing=T)
        nextitemlist <- cur_TS_order[1:TSnbig]
      } else {
        cur_TS_order <- order(uk_samp_cur,decreasing=T)
        nextitemlist <- cur_TS_order[1:TSnreg]
      }

      botitemlist[i,]<-nextitemlist
      designmatrix[i,]<-nextitemlist[c(t(as.matrix(MDDesign[MDDesign$Version==(i-1)%%max(MDDesign$Version)+1,-(1:2)])))]               

      #############    
      # ALTERNATIVE SAMPLING SCHEMES

      # for(j in 1:nquestions)
      # {
      #   Straw-man sample scheme
      #   t<-nquestions*nitemsperset/nitems*i        
      #   lambda<-3^2.5*t/(t+200)
      #   designmatrix[i,((j-1)*nitemsperset+1):(j*nitemsperset)]<-sample(1:nitems,nitemsperset,replace=F,prob=exp(lambda*b))
        
      #   Thompson Sample Top 30
      #   nextitemlist[1:nitemsperbot]<-order(uk_samp_cur[j,],decreasing=T)[1:30]
        
      #   Thompson Sample Top 20, pick 10 completely randomly from the rest
      #   nextitemlist[1:nitemsperbot]<-c(order(uk_samp_cur[j,],decreasing=T)[1:20],sample(1:nitems)[-order(uk_samp_cur[j,],decreasing=T)[1:15]][1:5])
                
      #   Thompson Sample Top 20, Thompson Sample Top 10 remaining from higher variance sample.
      #  nextitemlist[1:nitemsperbot]<-c(order(uk_samp_cur[j,],decreasing=T)[1:20],order(uk_samp_cur_big[j,-order(uk_samp_cur[j,],decreasing=T)[1:20]],decreasing=T)[1:10])
        
        
      #  botitemlist[i,]<-nextitemlist
      #  designmatrix[i,]<-nextitemlist[c(t(as.matrix(MDDesign[MDDesign$Version==(i-1)%%max(MDDesign$Version)+1,-(1:2)])))]               
        
      # }


    } # end conditions for each respondent i 
    
 

    # loop over questions
    # robot AI - this definitly could not be in a for loop
    for(j in 1:nquestions) {    
      setref<-designmatrix[i,((j-1)*nitemsperset+1):(j*nitemsperset)]  
      utilssub<-utils[i,setref] 
    
      # IF nmisinformed > 0, then change design after nmisinformed respondents
      # condition overwrites utilities for early respondents      
      # START misinformed overwrite      
      if(nmisinformed > 0 & i <= nmisinformed) {
       if( sum(setref %in% top10trueindex[1:3]) > 0 ) {
         utilssub[which(setref %in% top10trueindex[1:3])]<- sort(utils[i,])[misinformed_baditem]        
       }
      } # END misinformed overwrite

      noiseb<- -log(-log(runif(nitemsperset,0,1)))
      noisew<-  log(-log(runif(nitemsperset,0,1)))
      combutilb<-utilssub+noiseb
      combutilw<-utilssub+noisew
      best<-setref[which.max(combutilb)]
      worst<-setref[which.min(combutilw)]
      bestind<-which.max(combutilb)
      worstind<-which.min(combutilw)      
      results[(i-1)*nquestions+j,]<-c(setref,best,worst,bestind,worstind,i,j)      
    }  # END loop over questions

  } ## END loop over respondents 'bots' (i in 1:nbot)
  

  # Create final dataset after all data collected

  ID  <- rep(1:nbot,each=2*nitemsperset*nquestions)
  Set <- rep(1:nquestions,nbot,each=2*nitemsperset)
  
  Xb<-matrix(0,nbot*nquestions*nitemsperset,nitems)
  Xw<-matrix(0,nbot*nquestions*nitemsperset,nitems)
  X<-matrix(0,nbot*nquestions*nitemsperset*2,nitems-1)
  y<-rep(0,nbot*nquestions*nitemsperset*2)
  yb<-rep(0,nbot*nquestions*nitemsperset)
  yw<-rep(0,nbot*nquestions*nitemsperset)
  
  Xb[cbind(1:nrow(Xb),c(t(results[,1:5])))]<-1    
  Xb[c(t(results[,1:5]))==nitems,]<--1
  Xw<- -Xb
  
  X[which(bwindex=="Best") ,]<-Xb[,-nitems]
  X[which(bwindex=="Worst"),]<-Xw[,-nitems] 
  
  yb[seq(0,nbot*nquestions*nitemsperset-1,by=nitemsperset)+results[,"BestInd"]]<-1
  yw[seq(0,nbot*nquestions*nitemsperset-1,by=nitemsperset)+results[,"WorstInd"]]<-1 
  
  
  y[bwindex=="Best"] <-yb
  y[bwindex=="Worst"]<-yw  
  
  dall<- data.frame(
    ID  = ID,
    Set = Set,
    SetBestWorst = bwindex,
    Choice = y,
    X
  )
  
  system.time( 
    md1 <- mlogit(formula = y ~  X | 0, data=dall, alt.levels=paste(1:nitemsperset), shape="long")
  )
  
  #system.time( 
  #  md2 <- mnlogit(formula = y ~  X | 0, data=dall, alt.levels=paste(1:nitemsperset), shape="long")
  #)
  
  
  #tail(dall)
  #tail(results)


  ## Store estimates and hitrates for final respondent
  # since in-loop (i in 1:nbot) it used (i-1), we need to store the final respondent i = nbot
  
  est_vcov <- A%*%vcov(md1)%*%t(A)
  
  est_mean <- A%*%md1$coef

  b <- est_mean
  se <- sqrt(diag(est_vcov))

  bhist <- rbind(bhist,c(l,nbot,b))
  
  sehist <- rbind(sehist,c(l,nbot,se))
  
  top10simindex<-tail(order(b),10)
  hitratetop10<-mean(top10simindex %in% top10trueindex)
  
  top3simindex<-tail(order(b),3)
  hitratetop3<-mean(top3simindex %in% top3trueindex)
  
  hrhist<-rbind(hrhist,c(l,nbot,hitratetop3,hitratetop10))
  colnames(hrhist) <- c("iteration","respondent","hr_top3","hr_top10")

  top5trueutil<-tail(sort(colMeans(utilsall)),5)[]
  top5simutil <-tail(sort(b),5)
  
  top5probsum <-sum(exp(top5simutil)/(exp(top5simutil)+4))
  
  

  #timestamp()
  
  timestampstr<-format(Sys.time(), "%Y%m%d-%H%M%S")
  diagnostics<-data.frame(timestampstr,hitratetop3,hitratetop10,top5probsum)
  original_ids<-rep(simsample,each=nquestions)
  
  simchoicedata <- cbind(original_ids,results[,c(10,11,1:9)])

  if (SAVEFLAG == TRUE) {
  
  savechoicedata <- FALSE
  if (l %% 20 == 0) {savechoicedata <- TRUE}
  if(savechoicedata==TRUE){
    simchoicedatafilename <- paste(folder,"../DataDump/choicedata - ",timestampstr," - ",nbot," Resp Run Design with Answers",filesuffix,".csv",sep="")
    write.csv(simchoicedata, simchoicedatafilename)
  }


  bhistfilename<-paste(folder,"../DataDump/bhist",filesuffix,".csv",sep="")
  write.table(rbind("",bhist),bhistfilename,append=F,sep=",",col.names=T,row.names=F)

  sehistfilename<-paste(folder,"../DataDump/sehist",filesuffix,".csv",sep="")
  write.table(rbind("",sehist),sehistfilename,append=F,sep=",",col.names=T,row.names=F)
  
  hrhistfilename<-paste(folder,"../DataDump/hrhist",filesuffix,".csv",sep="")
  write.table(rbind("",hrhist),hrhistfilename,append=F,sep=",",col.names=T,row.names=F)

  
  coefficientsfile<-paste(folder,"../DataDump/",nbot," Resp Run Estimated Utilities",filesuffix,".csv",sep="")  
  # if(file.exists(coefficientsfile))
  # {
  #   write.table(rbind(b),coefficientsfile,append=T,sep=",",col.names=F,row.names=F)    
  # } else
  # {
  #   write.table(rbind(b),coefficientsfile,append=F,sep=",",col.names=T,row.names=F)  
  # }
  
  diagfilename<-paste(folder,"../DataDump/",nbot," Resp Run Diagnostics",filesuffix,".csv",sep="")
  # if(file.exists(diagfilename))
  # {
  #   write.table(diagnostics,diagfilename,append=T,sep=",",col.names=F,row.names=F)  
  # } else
  # {
  #   write.table(diagnostics,diagfilename,append=F,sep=",",col.names=T,row.names=F)
  # } 
  
  } # IF SAVEFLAG==TRUE

  #time_avg <- (Sys.time() - timestart)/l 
  #iters_remaining <- length(simschedule) - l
  #time_remaining <- time_avg * iters_remaining
  #print(paste("iter ",l," -- resp ",nbot," -- ",date(),sep=""))
  #print(paste("The remaining ",iters_remaining," iterations will take approx. ", time_remaining, sep=""  ))
  #print( paste( "- Finished world", w, "... at ... ", Sys.time() ) )
  #?save
  
  itemlistperbot <- data.frame(
    respondent = rep(1:nbot,each=nitemsperbot),
    item = c(t(botitemlist)),
    itemrank = order(order(colMeans(utilsall),decreasing=T))[c(t(botitemlist))],
    itemutil = colMeans(utilsall)[ c(t(botitemlist)) ]
    )


  respbyitem <- botitemlist
  itemcount <- rbind(itemcount, c( table(botitemlist))  )

  if (SAVEFLAG == TRUE){ 

  if (l %% 20 == 0) {saverespbyitem <- TRUE} else {saverespbyitem <- FALSE}
  if(saverespbyitem==TRUE){
    respbyitemfilename <- paste(folder,"../DataDump/itembyresp - ",timestampstr," - Item indicator per respondent ",filesuffix,".csv",sep="")
    write.csv(respbyitem, respbyitemfilename)
  }

  itemcountfilename<-paste(folder,"../DataDump/itemcount",filesuffix,".csv",sep="")
  write.table(rbind("",itemcount),itemcountfilename,append=F,sep=",",col.names=T,row.names=F) 
  # if(file.exists(itemcountfilename))
  # {
  #   write.table(itemcount,itemcountfilename,append=T,sep=",",col.names=F,row.names=F)  
  # } else
  # {
  #   write.table(rbind("",itemcount),itemcountfilename,append=F,sep=",",col.names=T,row.names=F)
  # }

  } # END IF SAVEFLAG == TRUE


} # end loop over iterations. for l in (1:length(simschedule))  

return( list(arguments=arguments, fixed_sparse=fixed_sparse, filesuffix=filesuffix,
  hrhist=hrhist, bhist=bhist, sehist=sehist, itemcount=itemcount,
  ex = list(respbyitem)
   ) )
##
} # END FUNCTION MDedTSmaster
##