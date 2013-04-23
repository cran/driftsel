mvdnorm <-
function(x, mu, Sig, log=F, invert=T, sparse=F){
	if( sparse ){
		cSig = SparseM::chol(Sig)
		f = mvdnorm.chol(x, mu, cSig, TRUE, log=T)
		}
	else{
		Sig_ = solve(Sig)
		SS = t(x - mu) %*% Sig_ %*% (x - mu)
		f1 = -0.5*log(det(Sig))
		}
	f1 = min(c( f1, 1.0e280 ))
	SS = max(c( SS, -1.0e280 ))
	f1 = max(c( f1, -1.0e280 ))
	f = f1 - 0.5*SS	
	if( !log ){
		f = exp(f)
		}
	return(f)
	}

