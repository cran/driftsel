pri.GG <-
function(G, priors){
	f = dwish_(G, priors$G[[1]], priors$G[[2]], log=T)
	return(f)
	}

