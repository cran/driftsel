mvdnorm.chol <-
function(x, mu, cSig, sparse, log=F){
	dif = x - mu
	if( sparse ){ 
		SS = t(dif) %*% SparseM::backsolve(cSig, dif)
		}
	else{ SS = t(dif) %*% xx(cSig, dif) }
	SS = c(as.matrix(SS))
	if( sparse ){
		f1 = -(cSig@log.det)
		}
	else{ f1 = - sum(log(diag(cSig))) }
	f1 = min(c( f1, 1.0e280 ))
	SS = max(c( SS, -1.0e280 ))
	f1 = max(c( f1, -1.0e280 ))
	f = f1 - 0.5*SS
	if( !log ){
		f = exp(f)
		}
	return(f)
	}

