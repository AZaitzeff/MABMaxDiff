# MDTSrunall.r

## Act like master file running subscripts 


library(mlogit)
library(MASS)


###### ARGUMENTS ###### 

OS = "MAC"
# OS = "PC"
if (OS=="PC"){ CC = "C:" } else {CC = ""} 

#folder <- "/Users/ericmsch/Dropbox/Sawtooth/Bandit Code Files/"
#folder <- "C:/Users/ericmsch/Dropbox/Sawtooth/Bandit Code Files/"
folder <- paste(CC,"/Users/azaitzeff/Documents/MultiArmedBandits/MABMaxDiff/",sep="")

niters = 100
nrespondents = 1020
nitems=120
nitemsperset=5
nquestions=18
nitemsperbot=30

nperupdate = 20
nupdates = nrespondents/nperupdate
respseq = seq(nperupdate,nrespondents,by=nperupdate)


# method-specific arguments

TSeps <- 1/3 # 0 is default, not split
TSgamma <- 10 # 1 is default, not diffuse

fixed_sparse <- FALSE

nmisinformed <- 50 # for misinformed start nmisinformed > 0
misinformed_baditem <- 30 # 30th out of 120th best utility replaces all the top 3 items # for misinformed start


###### END ARGUMENTS ###### 


# 120
# fixed sparse
# eps=0,gamma=1
# eps=10/30,gamma=10
# eps=10/30,gamma=20
# eps=15/30,gamma=10
# eps=15/30,gamma=20

# 120
# fixed sparse,misinformed
# eps=0,gamma=1,misinformed
# eps=10/30,gamma=10,misinformed
# eps=10/30,gamma=20,misinformed
# eps=15/30,gamma=10,misinformed
# eps=15/30,gamma=20,misinformed


# 40
# fixed sparse
# fixed sparse,misinformed
# eps=0,gamma=1
# eps=0,gamma=1,misinformed
# eps=5/20,gamma=10
# eps=5/20,gamma=10,misinformed


# 300
# fixed sparse
# fixed sparse,misinformed
# eps=0,gamma=1
# eps=0,gamma=1,misinformed
# eps=10/30,gamma=10
# eps=10/30,gamma=10,misinformed



# TS no split (eps=0,gamma=1), not misinformed # DONE



source( paste(folder,"MDedTSmaster.r",sep="") )
args = list(
	folder = folder,
	niters = 100, nrespondents = 1020, nitems=120,
	nitemsperset=5, nquestions=18, nitemsperbot=30,
	TSeps = 0, TSgamma = 1,
	fixed_sparse = FALSE,
	nmisinformed = 0, misinformed_baditem = 30, SAVEFLAG=TRUE
)
res <- MDedTSmaster(args)
save(res, paste(folder,"DataDump/res ", res$filesuffix, ".RData",sep="") )



# fixed not misinformed  ## DONE
source( paste(folder,"MDedTSmaster.r",sep="") )
args = list(
	folder = folder,
	niters = 100, nrespondents = 1020, nitems=120,
	nitemsperset=5, nquestions=18, nitemsperbot=30,
	TSeps = 0, TSgamma = 1,
	fixed_sparse = TRUE,
	nmisinformed = 0, misinformed_baditem = 30, SAVEFLAG=TRUE
)
res <- MDedTSmaster(args)
save(res, paste(folder,"DataDump/res ", res$filesuffix, ".RData",sep="") )


# fixed misinformed ## DONE
source( paste(folder,"MDedTSmaster.r",sep="") )
args = list(
	folder = folder,
	niters = 100, nrespondents = 1020, nitems=120,
	nitemsperset=5, nquestions=18, nitemsperbot=30,
	TSeps = 0, TSgamma = 1,
	fixed_sparse = TRUE,
	nmisinformed = 50, misinformed_baditem = 30, SAVEFLAG=TRUE
)
res <- MDedTSmaster(args)
save(res, paste(folder,"DataDump/res ", res$filesuffix, ".RData",sep="") )


# eps=0,gamma=1,misinformed # DONE


## MUST RERUN APRIL 10 WITH fixed_sparse = TRUE
# eps=10/30,gamma=10,misinformed
source( paste(folder,"MDedTSmaster.r",sep="") )
args = list(
	folder = folder,
	niters = 100, nrespondents = 1020, nitems=120,
	nitemsperset=5, nquestions=18, nitemsperbot=30,
	TSeps = 10/30, TSgamma = 10,
	fixed_sparse = FALSE,
	nmisinformed = 50, misinformed_baditem = 30, SAVEFLAG=TRUE
)
res <- MDedTSmaster(args)
save(res, paste(folder,"DataDump/res ", res$filesuffix, ".RData",sep="") )

## MUST RERUN APRIL 10 WITH fixed_sparse = TRUE
# eps=10/30,gamma=20,misinformed
source( paste(folder,"MDedTSmaster.r",sep="") )
args = list(
	folder = folder,
	niters = 100, nrespondents = 1020, nitems=120,
	nitemsperset=5, nquestions=18, nitemsperbot=30,
	TSeps = 10/30, TSgamma = 20,
	fixed_sparse = FALSE,
	nmisinformed = 50, misinformed_baditem = 30, SAVEFLAG=TRUE
)
res <- MDedTSmaster(args)
save(res, paste(folder,"DataDump/res ", res$filesuffix, ".RData",sep="") )




# eps=15/30,gamma=10,misinformed # NEVER HAVE RUN
source( paste(folder,"MDedTSmaster.r",sep="") )
args = list(
	folder = folder,
	niters = 100, nrespondents = 1020, nitems=120,
	nitemsperset=5, nquestions=18, nitemsperbot=30,
	TSeps = 15/30, TSgamma = 10,
	fixed_sparse = FALSE,
	nmisinformed = 50, misinformed_baditem = 30, SAVEFLAG=TRUE
)
res <- MDedTSmaster(args)
save(res, paste(folder,"DataDump/res ", res$filesuffix, ".RData",sep="") )



# eps=15/30,gamma=20,misinformed  # NEVER HAVE RUN
source( paste(folder,"MDedTSmaster.r",sep="") )
args = list(
	folder = folder,
	niters = 100, nrespondents = 1020, nitems=120,
	nitemsperset=5, nquestions=18, nitemsperbot=30,
	TSeps = 15/30, TSgamma = 20,
	fixed_sparse = FALSE,
	nmisinformed = 50, misinformed_baditem = 30, SAVEFLAG=TRUE
)
res <- MDedTSmaster(args)
save(res, paste(folder,"DataDump/res ", res$filesuffix, ".RData",sep="") )







#### obtain examples to show parameter learning


args = list(
	folder = folder,
	niters = 1, nrespondents = 1020, nitems=120,
	nitemsperset=5, nquestions=18, nitemsperbot=30,
	TSeps = 0, TSgamma = 1,
	fixed_sparse = TRUE,
	nmisinformed = 0, misinformed_baditem = 30, SAVEFLAG=FALSE
)
arguments = args
res <- MDedTSmaster(args)
save(res, paste(folder,"DataDump/Ex res (fixed_sparse - 120 items - 30 items per resp - misinf 0).RData",sep="") )
	write.csv(sehist, paste(folder,"DataDump/Ex sehist (fixed_sparse - 120 items - 30 items per resp - misinf 0).csv",sep="") )
	write.csv(bhist, paste(folder,"DataDump/Ex bhist (fixed_sparse - 120 items - 30 items per resp - misinf 0).csv",sep="") )	

## RERUN APRIL 10 to get Example world for 
args = list(
	folder = folder,
	niters = 1, nrespondents = 1020, nitems=120,
	nitemsperset=5, nquestions=18, nitemsperbot=30,
	TSeps = 0, TSgamma = 1,
	fixed_sparse = FALSE,
	nmisinformed = 0, misinformed_baditem = 30, SAVEFLAG=FALSE
)
arguments = args
# run inside of MDedTSmaster.r
save(res, paste(folder,"DataDump/Ex res (TS - 120 items - 30 items per resp - split 30 none - misinf 0).RData",sep="") )
	write.csv(sehist, paste(folder,"DataDump/Ex sehist (TS - 120 items - 30 items per resp - split 30 none - misinf 0).csv",sep="") )
	write.csv(bhist, paste(folder,"DataDump/Ex bhist (TS - 120 items - 30 items per resp - split 30 none - misinf 0).csv",sep="") )
## running on desktop
#

