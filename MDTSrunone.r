# MDTSrunall.r

## Act like master file running subscripts 

theargs<-commandArgs(trailingOnly = TRUE)

args<-as.numeric(theargs[1])

library(mlogit)
library(MASS)


###### ARGUMENTS ###### 

OS = "MAC"
# OS = "PC"
if (OS=="PC"){ CC = "C:" } else {CC = ""} 

#folder <- "/Users/ericmsch/Dropbox/Sawtooth/Bandit Code Files/"
#folder <- "C:/Users/ericmsch/Dropbox/Sawtooth/Bandit Code Files/"
folder <- paste0(CC,tools:::file_path_as_absolute("."),"/")

nrespondents <- 1020
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


niters <- 100
nrespondents <- 1020
nitems <- 120
nitemsperset <- 5
nquestions <- 18
nitemsperbot <- 30
TSeps <- 0 
TSgamma <- 1
fixed_sparse <- FALSE
nmisinformed <- 0
misinformed_baditem <- 30
SAVEFLAG <- TRUE

# 120
if (args==1){# fixed sparse
	fixed_sparse <- TRUE
} else if (args==2){# eps=0,gamma=1
	TSeps <- 0 
	TSgamma <- 1
}else if (args==3){# eps=10/30,gamma=10
	TSeps <- 10/30 
	TSgamma <- 10
}else if (args==4){# eps=10/30,gamma=20
	TSeps <- 10/30 
	TSgamma <- 20
}else if (args==5){# eps=15/30,gamma=10
	TSeps <- 15/30 
	TSgamma <- 10

}else if (args==6){# eps=15/30,gamma=20
	TSeps <- 15/30 
	TSgamma <- 20
}else if (args==7){# 120 # fixed sparse,misinformed
	nmisinformed <- 50
	misinformed_baditem <- nitems*.25
	fixed_sparse <- TRUE
}else if (args==8){ # eps=0,gamma=1,misinformed
	nmisinformed <- 50
	misinformed_baditem <- nitems*.25
	TSeps <- 0 
	TSgamma <- 1
}else if (args==9){# eps=10/30,gamma=10,misinformed
	nmisinformed <- 50
	misinformed_baditem <- nitems*.25
	TSeps <- 10/30
	TSgamma <- 10
}else if (args==10){ # eps=10/30,gamma=20,misinformed
	nmisinformed <- 50
	misinformed_baditem <- nitems*.25
	TSeps <- 10/30
	TSgamma <- 20
}else if (args==11){# eps=15/30,gamma=10,misinformed
	nmisinformed <- 50
	misinformed_baditem <- nitems*.25
	TSeps <- 15/30
	TSgamma <- 10
}else if (args==12){# eps=15/30,gamma=20,misinformed
	nmisinformed <- 50
	misinformed_baditem <- nitems*.25
	TSeps <- 15/30 
	TSgamma <- 20
}else if (args==13){# 40 # fixed sparse 
	nitems <- 40
	nquestions <- 12
	nitemsperbot <- 20
	fixed_sparse <- TRUE
}else if (args==14){# fixed sparse,misinformed
	nitems <- 40
	nquestions <- 12
	nitemsperbot <- 20
	fixed_sparse <- TRUE
	nmisinformed <- 50
	misinformed_baditem <- nitems*.25
}else if (args==15){# eps=0,gamma=1
	nitems <- 40
	nquestions <- 12
	nitemsperbot <- 20
	TSeps <- 0 
	TSgamma <- 1
}else if (args==16){# eps=0,gamma=1,misinformed
	nitems <- 40
	nitemsperbot <- 20
	nquestions <- 12
	nmisinformed <- 50
	misinformed_baditem <- nitems*.25
	TSeps <- 0 
	TSgamma <- 1
}else if (args==17){# eps=5/20,gamma=10
	nitems <- 40
	nitemsperbot <- 20
	nquestions <- 12
	TSeps <- 5/20 
	TSgamma <- 10
}else if (args==18){# eps=5/20,gamma=10,misinformed
	nitems <- 40
	nitemsperbot <- 20
	nquestions <- 12
	nmisinformed <- 50
	misinformed_baditem <- nitems*.25
	TSeps <- 5/20
	TSgamma <- 10
}else if (args==19){# 300 # fixed sparse
	nitems <- 300
	fixed_sparse <- TRUE
}else if (args==20){# fixed sparse,misinformed
	nitems <- 300
	fixed_sparse <- TRUE
	nmisinformed <- 50
	misinformed_baditem <- nitems*.25
}else if (args==21){# eps=0,gamma=1
	nitems <- 300
	TSeps <- 0 
	TSgamma <- 1
}else if (args==22){# eps=0,gamma=1,misinformed
	nitems <- 300
	nmisinformed <- 50
	misinformed_baditem <- nitems*.25
	TSeps <- 0 
	TSgamma <- 1
}else if (args==23){# eps=10/30,gamma=10
	nitems <- 300
	TSeps <- 10/30
	TSgamma <- 10

}else if (args==24){# eps=10/30,gamma=10,misinformed
	nitems <- 300
	nmisinformed <- 50
	misinformed_baditem <- nitems*.25
	TSeps <- 10/30
	TSgamma <- 10
}

print(misinformed_baditem)
source( paste(folder,"MDedTSmaster.r",sep="") )
	argz = list(
	folder = folder,
	niters = niters, nrespondents = nrespondents, nitems=nitems,
	nitemsperset=nitemsperset, nquestions=nquestions, nitemsperbot=nitemsperbot,
	TSeps = TSeps, TSgamma = TSgamma,
	fixed_sparse = fixed_sparse,
	nmisinformed = nmisinformed, misinformed_baditem = misinformed_baditem,
	SAVEFLAG=SAVEFLAG
	)
res <- MDedTSmaster(argz)
save(res,file=paste(folder,"../DataDump/res ", res$filesuffix, ".RData",sep="") )