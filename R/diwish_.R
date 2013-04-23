diwish_ <-
function(x, v, S, log=F){
	f = diwish(x, v, S)
	if( log ){
		f = log(f)
		}
	f = max(c(f, -1.0e280))
	return(f)
	}

