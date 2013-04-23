pri.g <-
function(g, THP, G, spar, priors){
	V = calc.V(g, THP, G, priors)
	f <- NA # unavoidable psd fix
	try(f <- mvdnorm(g, priors$fixed[[1]], V, log=T, sparse=F), silent=T)
	return(f)
	}

