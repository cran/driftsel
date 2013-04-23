pri.B <-
function(B, k, priors){
	prithis = priors$random[[k]]
	f = dwish_(B, prithis[[1]], prithis[[2]], log=T)
	return(f)
	}

