update.g <-
function(W, cSig, V, y, sparse, priors){		
	V_ = solve(V)
	if( sparse ){
		gam_ = (t(W) %*% SparseM::backsolve(cSig,W)) + V_
		gam_ = as.matrix(gam_)
		mu_sig = t(W) %*% SparseM::backsolve(cSig,matrix(y,ncol=1))
		mu_sig = mu_sig + V_ %*% priors$fixed[[1]]
		mu_sig = as.matrix(mu_sig)
		}
	else{
		gam_ = (t(W) %*% xx(cSig,W)) + V_
		mu_sig = t(W) %*% xx(cSig,matrix(y,ncol=1))
		mu_sig = mu_sig + V_ %*% priors$fixed[[1]]
		}
	g = Nh(gam_, mu_sig, sparse)
	return(g)
	}

