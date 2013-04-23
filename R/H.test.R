H.test <-
function(popefpost, Gpost, THpost, env, silent=T){

	# Stupid scaling again
	nx = dim(popefpost)[1] * dim(popefpost)[2]
	npop = dim(THpost)[1]
	nmc = dim(THpost)[3]
	env = into.dist(env)

	# Posterior of Mantel statistic for phenotypic distance
	mobs = mrand = rep(NA, nmc)
	for( i in 1:nmc ){
		G = Gpost[,,i] # G matrix
		theta = THpost[,,i] # theta	
		C2 = matrix(mvrnorm( 1, rep(0, nx), 2*G %x% theta ), nrow=npop)
		D2 = into.dist(popefpost[,,i]) # observed distance matrix		
		C2 = into.dist(C2) # distance matrix from neutral model
		mobs[i] = mantel.stat(D2, env) # observed product moment
		mrand[i] = mantel.stat(C2, env) # randomized product moment
		}
	
	# Out	
	if( !silent ){
  	windows()
  	par(mfcol=c(1,2))
  	plot(mobs, type='l', ylab="observed product moment", xlab="iteration", main="Convergence check")
  	plot(mrand, type='l', ylab="product moment from neutral model", xlab="iteration", main="Convergence check")
  	}
  out = length(which(mobs>mrand)) / length(mobs)
	return(out)
	}

