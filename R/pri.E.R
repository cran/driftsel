pri.E <-
function(E, priors){
	f = dwish_(E, priors$E[[1]], priors$E[[2]], log=T)
	return(f)
	}

