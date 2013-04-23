S.test <-
function(popefpost, Gpost, THpost, silent=T, G.off=F, th.off=F, main=NA){

	# Turning off-diagonals off if needed
	if( th.off ){
		npop = dim(THpost)[1]
		if( npop>1 ){
			for( i in 2:npop ){
				for( j in 1:(i-1) ){
					THpost[i,j,] = THpost[j,i,] = 0 }}}}
	if( G.off ){
		ntrait = dim(Gpost)[1]
		if( ntrait > 1 ){
			for( i in 2:ntrait ){
				for( j in 1:(i-1) ){
					Gpost[i,j,] = Gpost[j,i,] = 0 }}}}

	# Mahalanobis!
	apost = popefpost
	nmc = dim(apost)[3]
	D = rep(NA, nmc)
	for( i in 1:nmc ){
		a = c(apost[,,i])
		G = Gpost[,,i]
		THP = THpost[,,i]
		Sig = 2*G %x% THP
		Sig_ = solve(Sig)
		mu = rep(0, length(a))
		D2 = t(mu - a) %*% Sig_ %*% (mu - a)
		D[i] = D2
		}
	
	# Posterior probabilities
	dfr = (dim(Gpost)[1])*(dim(apost)[1])
	cdf = pchisq(D, dfr)
	out = mean(cdf)
	if( !silent ){
		windows()
		if( is.na(main) ){ main = "Posterior trace" }
		plot(cdf, xlab="iteration", ylab="signal of selection (S)", main=main, type='l', lwd=2, ylim=c(0,1))
		lines(c(-length(cdf),2*length(cdf)), rep(0.2,2), lty='dashed')
		lines(c(-length(cdf),2*length(cdf)), rep(0.8,2), lty='dashed')
		lines(c(-length(cdf),2*length(cdf)), rep(0.5,2), lty='dashed')
		lines(c(-length(cdf),2*length(cdf)), rep(0.05,2), lty='dashed')
		lines(c(-length(cdf),2*length(cdf)), rep(0.95,2), lty='dashed')		
		}
	return(out)
	}

