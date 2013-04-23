dwish_ <-
function(x, v, S, log=F){ # non-normalized!
	S = as.matrix(S)
	x = as.matrix(x)
	S_ = pseudoinverse(S) # to avoid numerical singularity
	p = ncol(x)
	f = rep(NA,3)
	f[1] = 0.5*(v-p-1)*log(det(x))
	f[2] = 0.5*sum(diag(S_%*%x))
	f[3] = 0.5*v*log(det(S))
	f[which(f>1.0e280)] = 1.0e280
	f[which(f<(-1.0e280))] = -1.0e280
	f = f[1] - f[2] - f[3]
	if( !log ){
		f = exp(f)
		}
	return(f)
	}

