MH <-
function(poster, ped, covars, traits, nmc, burnin, thin, blocks=NA, priors=NA, tmp=NA, binary=NA, alt=F){

	# Dependencies
	# library(corpcor)
	# library(MCMCpack)
	# library(SparseM)

	# Automate conversion
	ped <- as.matrix(ped)
	covars <- as.matrix(covars)
	traits <- as.matrix(traits)
	blocks <- as.matrix(blocks)
	
	# Phenotypic data
	X <- as.matrix(covars[,-1], ncol=ncol(covars)-1)
	X <- cbind(1, X) # 1 for grand mean
	y <- c(traits[,-1])
	N <- length(y)
	cens <- which(is.na(y))
	if( length(cens)>0 ){ y <- y[-cens] }
	if( N>=200 ){ sparse <- T } # sparse matrix tricks
	else{ sparse <- F }
	
	# Coancestry matrix & the other 'Kroenecker matrix'
	nop <- dim(poster)[1]
	if( max(poster)==0 ){ # Bayes spirit
    		for( i in 1:(dim(poster)[3]) ){
      			poster[,,i] <- 10^(-4)*diag(nop)
      			}
    		}
	w <- sample(1:(dim(poster)[3]), 1)
	ww <- w
	THP <- as.matrix(poster[,,w])
	if( alt ){ # one generation
		ped <- ped[,-1]
		censped <- apply(ped, 1, function(x)length(which(is.na(x))))		
		THB <- ind.theta(ped, censped)
		M <- calc.M(ped, THP, THB, censped)
		}
	else{
		wpop <- calc.loads(ped, THP)
		M <- calc.M.alt(wpop, THP, ped)
		}
	VR <- diag(ncol(M))
	if( sparse ){
		M <- as.matrix.csr(M)
		VR <- as.matrix.csr(VR) 
		}

	# Fundamental covariance matrices
	traits <- as.matrix(traits[,-1], ncol=ncol(traits)-1)
	ntr <- ncol(traits)
	G <- rwish(ntr, diag(ntr)/ntr) # rwish_ doesn't work yet
	E <- rwish(ntr, diag(ntr)/ntr)

	# Treating random/block effects
	bbit <- (length(blocks) > 1)
	if( bbit ){
		nb <- ncol(blocks) - 1
		blocks <- calc.blocks(blocks) # covariates into design matrices
		Blist <- array(NA, dim=c(ntr, ntr, nb))
		for( i in 1:nb ){
			Blist[,,i] <- rwish(ntr, diag(ntr)/ntr)
			}
		}
	else{ Blist <- NA }
	
	# Covariance matrix
	Sig <- const.sig(E, VR, G, M, blocks, Blist, bbit, sparse)
	if( length(cens)>0 ){ Sig <- Sig[-cens,-cens] }
	if( sparse ){ 
		Sig <- as.matrix.csr(Sig)
		if( is.na(tmp) ){ cSig <- SparseM::chol(Sig) }
		else{ cSig <- SparseM::chol(Sig, tmpmax=tmp) }
		}
	else{ cSig <- base::chol(Sig) }
	
	# Default priors
	if( length(priors)==1 ){
		priors <- as.list(1:4)
		names(priors) <- c("fixed","G","E","random")
		ng <- ncol(traits)*(ncol(THP)+ncol(X))
		priors[[1]] <- list(rep(0,ng), matrix(NA,ncol=1,nrow=1)) # for g
		priors[[2]] <- list(ntr+1, diag(ntr)/(ntr+1)) # for G
		priors[[3]] <- list(ntr+1, diag(ntr)/(ntr+1)) # for E
		if( bbit ){
			priors[[4]] <- as.list(1:nb)
			for( i in 1:nb ){ # for blocks stuff
				priors[[4]][[i]] <- list(ntr+1, diag(ntr)/(ntr+1))
				}
			}
		}
	else{
		priors <- priors
		names(priors) <- c("fixed","G","E","random")
		priors$fixed[[2]] <- as.matrix(priors$fixed[[2]])
		}
		
	# Design matrices
	g <- rnorm(ncol(traits)*(ncol(THP)+ncol(X)), 0, sqrt(max(poster)))
	V <- calc.V(g, THP, G, priors)
	if( alt ){ W <- calc.W(ped, X, traits, THP) }
	else{ W <- calc.W.alt(wpop, X, traits, THP) }
	if( length(cens)>0 ){ W <- W[-cens,] }			

	# Treating non-normal variables
	if( !is.na(binary) ){
		y_ <- y
		y <- update.y(y_, NA, g, W, Sig, binary, ntr, first=T)
		}
	
	# Densities
	like <- likelihood(y, g, W, cSig, sparse)
	priG <- pri.GG(G, priors)
	priE <- pri.E(E, priors)
	prig <- pri.g(g, THP, G, sparse, priors)
	if( bbit ){
		priBlocks <- sapply(1:nb, function(x) pri.B(Blist[,,x], x, priors))
		}
		
	output <- c(g,G,E,THP)
	if( bbit ){
		out2 <- c(Blist, recursive=T)
		}

	# Adjustment parameters
	dltG <- dltE <- 2*ncol(G)
	dltb <- 1
	iterno <- Eup <- Gup <- bup <- 0
	if( bbit ){ 
		dltB <- rep(2*ncol(G), nb) 
		Bup <- rep(0, nb) # not to be confused with bup
		}
	else{ dltB <- 10 }
	props <- rep( 1/(dim(poster)[3]), dim(poster)[3] )
	
	for( i in 1:nmc ){

		# Updating g, nicely packed
		V <- calc.V(g, THP, G, priors)
		try(g <- update.g(W, cSig, V, y, sparse, priors), silent=T) # numerical singularity causes fail
		like <- likelihood(y, g, W, cSig, sparse)
		prig <- pri.g(g, THP, G, sparse, priors)

		# Updating latent liabilities, if any
		if( !is.na(binary) ){ 
      y <- update.y(y_, y, g, W, Sig, binary, ntr, first=F) 
      like <- likelihood(y, g, W, cSig, sparse)
      }

		# Updating G, random walk
		newG <- G
		try(newG <- rwish_(dltG, G/dltG), silent=T)
		here <- dwish_(G, dltG, newG/dltG, log=T)
		there <- dwish_(newG, dltG, G/dltG, log=T)
		newpriG <- pri.GG(newG, priors)	
		newprig <- pri.g(g, THP, newG, sparse, priors)
		newSig <- const.sig(E, VR, newG, M, blocks, Blist, bbit, sparse)
		if( length(cens)>0 ){ newSig <- newSig[-cens,-cens] }
		if( sparse ){ 
			newSig <- as.matrix.csr(newSig)
			if( is.na(tmp) ){ newcSig <- SparseM::chol(newSig) }
			else{ newcSig <- SparseM::chol(newSig, tmpmax=tmp) }
			}
		else{ newcSig <- base::chol(newSig) }
		newlike <- likelihood(y, g, W, newcSig, sparse)
		accept <- newlike + newpriG + newprig + here - like - priG - prig - there
		a <- log(runif(1,0,1))
		if( !is.na(accept) ){
		if( a < accept ){
			G <- newG
			Sig <- newSig
			cSig <- newcSig
			like <- newlike
			priG <- newpriG
			prig <- newprig
			Gup <- Gup + 1
			}}

		# Updating E, random walk
		newE <- E
		try(newE <- rwish_(dltE, E/dltE), silent=T)
		here <- dwish_(E, dltE, newE/dltE, log=T)
		there <- dwish_(newE, dltE, E/dltE, log=T)
		newpriE <- pri.E(newE, priors)
		newSig <- const.sig(newE, VR, G, M, blocks, Blist, bbit, sparse)
		if( length(cens)>0 ){ newSig <- newSig[-cens,-cens] }
		if( sparse ){ 
			newSig <- as.matrix.csr(newSig)
			if( is.na(tmp) ){ newcSig <- SparseM::chol(newSig) }
			else{ newcSig <- SparseM::chol(newSig, tmpmax=tmp) }
			}
		else{ newcSig <- base::chol(newSig) }
		newlike <- likelihood(y, g, W, newcSig, sparse)
		accept <- newlike + newpriE + here - like - priE - there
		a <- log(runif(1,0,1))
		if( !is.na(accept) ){
		if( a < accept ){
			E <- newE
			Sig <- newSig
			cSig <- newcSig
			like <- newlike
			priE <- newpriE
			Eup <- Eup + 1
			}}
			
		# Updating theta, independence sampler
		if( runif(1,0,1) < 0.5 ){
			k <- sample(1:(dim(poster)[3]), 1, prob=props)
			}
		else{ k <- sample(1:(dim(poster)[3]), 1) }
		newTHP <- as.matrix(poster[,,k])
		if( alt ){ newM <- calc.M(ped, newTHP, THB, censped) }
		else{ newM <- calc.M.alt(wpop, newTHP, ped) }
		if( sparse ){ newM <- as.matrix.csr(newM) }
		newSig <- const.sig(E, VR, G, newM, blocks, Blist, bbit, sparse)
		if( length(cens)>0 ){ newSig <- newSig[-cens,-cens] }
		if( sparse ){ 
			newSig <- as.matrix.csr(newSig)
			if( is.na(tmp) ){ newcSig <- SparseM::chol(newSig) }
			else{ newcSig <- SparseM::chol(newSig, tmpmax=tmp) }
			}
		else{ newcSig <- base::chol(newSig) }
		newprig <- pri.g(g, newTHP, G, sparse, priors)
		newlike <- likelihood(y, g, W, newcSig, sparse)
		here <- log(props[w] + 0.5)
		there <- log(props[k] + 0.5)
		accept <- newlike + newprig - like - prig + here - there	
		a <- log(runif(1,0,1))
		if( !is.na(accept) ){
		if( a < accept ){	
			THP <- newTHP
			M <- newM
			Sig <- newSig
			cSig <- newcSig
			prig <- newprig
			like <- newlike
			w <- k
			}}
			
		# Updating block/random effects, random walk
		if( bbit ){
			for( k in 1:nb ){
				newB <- Blist
				try(newB[,,k] <- rwish_(dltB[k], newB[,,k,drop=F]/dltB[k]), silent=T)
				here <- dwish_(Blist[,,k], dltB[k], newB[,,k]/dltB[k], log=T)
				there <- dwish_(newB[,,k], dltB[k], Blist[,,k]/dltB[k], log=T)
				newpri <- pri.B(newB[,,k], k, priors)
				newSig <- const.sig(E, VR, G, M, blocks, newB, bbit, sparse)
				if( length(cens)>0 ){ newSig <- newSig[-cens,-cens] }
				if( sparse ){ 
					newSig <- as.matrix.csr(newSig)
					if( is.na(tmp) ){ newcSig <- SparseM::chol(newSig) }
					else{ newcSig <- SparseM::chol(newSig, tmpmax=tmp) }
					}
				else{ newcSig <- base::chol(newSig) }
				newlike <- likelihood(y, g, W, newcSig, sparse)
				accept <- newlike + newpri + here - like - priBlocks[k] - there
				a <- log(runif(1,0,1))
				if( !is.na(accept) ){
				if( a < accept ){
					Blist <- newB
					Sig <- newSig
					cSig <- newcSig
					like <- newlike
					priBlocks[k] <- newpri
					Bup[k] <- Bup[k] + 1
					}}
				}		
			}

		# Adapting proposals of G, E, etc.
		iterno <- iterno + 1
  	if( i < burnin ){
    	wgh <- 1 - 0.1*exp(-i/1000)
    	a <- 0.1*exp(-i/1000)
    	iterno <- wgh*iterno	
    	Gup <- wgh*Gup
    	Eup <- wgh*Eup
    	rateG <- Gup / iterno
    	rateE <- Eup / iterno
    	dltG_ <- exp(a*(0.23 - rateG))*dltG
    	dltE_ <- exp(a*(0.23 - rateE))*dltE
    	if( dltG_ > ntr ){
   		if( dltG_ < 200 ){
  			dltG <- dltG_ } }
    	if( dltE_ > ntr ){
   		if( dltE_ < 200 ){
  			dltE <- dltE_ } }
    	if( bbit ){
   		 for( i in 1:length(dltB) ){
   			Bup[i] <- wgh*Bup[i]
   			rateB <- Bup[i] / iterno
   			dltB_ <- exp(a*(0.23 - rateB))*dltB[i]
   			  if( dltB_ > ntr ){
    				if( dltB_ < 200 ){
   					  dltB[i] <- dltB_ }}
        			}
        	}
			}
			
		# Adapting proposal of theta
		ww <- c(ww, w)
		if( i==burnin ){ props <- study(ww, dim(poster)[3], maxpar=30) }
			
		# Info for user
		if( round(i/100)==i/100 ){ print(paste("iter",i), quote=F) }
		output <- rbind(output, c(g,G,E,THP))
		if( bbit ){
			out2 <- rbind(out2, c(Blist, recursive=T))
			}
		} # mc loop closes	
		
	# Thinning and burn-in
	output <- output[(burnin+1):nrow(output),]
	imax <- floor(nrow(output) / thin)
	totake <- thin*1:imax
	if( length(totake>1) ){
		output <- output[totake,]
		}	
	if( bbit ){
		out2 <- out2[(burnin+1):nrow(out2),,drop=F]
		if( length(totake>1) ){
			out2 <- out2[totake,,drop=F]
			}
		}
		
	# Conversion
	out <- convert(output, ncol(traits), ncol(THP), ncol(X))
	if( bbit ){ 
		out2 <- convert.2(out2, ncol(G), nb)
		out <- list(out, out2)
		names(out) <- c("main.result","block.ef")
		}
	
	return(out)	
	}

