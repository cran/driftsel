likelihood <-
function(y, g, W, cSig, sparse){
	mu = W %*% g
	f = mvdnorm.chol(y, mu, cSig, sparse, log=T)
	return(f)
	}

