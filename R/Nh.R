Nh <-
function(invSig, mu_Sig, sparse){
	C = base::chol(invSig)
	u = rnorm(length(mu_Sig))
	gg = solve(t(C), mu_Sig)
	fi = solve(C, gg)
	u_ = solve(C, u)
	x = fi + u_
	return(x)
	}

