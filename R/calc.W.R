calc.W <-
function(pars, covars, traits, THP){

	# Dimensions
	nt = ncol(traits)
	np = ncol(THP)
	nx = ncol(covars)
	n = nrow(covars)

	# Parental contributions
	indic = matrix(0, ncol=np, nrow=n)
	for( i in 1:n ){
		if( !is.na(pars[i,3]) ){
			indic[i,pars[i,3]] = indic[i,pars[i,3]] + 0.5  
			}
		if( !is.na(pars[i,4]) ){
			indic[i,pars[i,4]] = indic[i,pars[i,4]] + 0.5
			}
		}
		
	# Censoring in covariates
	censed = which( apply(covars,1,function(x)length(which(is.na(x)))) > 0 )
	if( length(censed) > 0 ){
		nclass = apply(covars[-censed,,drop=F],2,function(x)length(unique(x)))	
		binaries = which(nclass<=2)
		covars[censed,-binaries] = 0
		covars[censed,1] = 1 # censored observations go to corner class
		for( j in binaries ){	
			covars[censed,j] = mean(covars[,j], na.rm=T) # can be either
			}
		}
			
	# covars and indic are constants accross traits,
	# however they need to be changed into design matrices
	X = matrix(0, nrow=nt*n, ncol=nt*nx)
	Z = matrix(0, nrow=nt*n, ncol=nt*np)
	for( i in 1:nt ){
		lo = 1 + n*(i-1)
		hi = n*i
		lox = 1 + nx*(i-1)
		hix = nx*i
		loz = 1 + np*(i-1)
		hiz = np*i
		X[lo:hi,lox:hix] = covars
		Z[lo:hi,loz:hiz] = indic
		}
		
	W = cbind(X, Z)
	return(W)
	}

