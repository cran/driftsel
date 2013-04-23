rwish_ <-
function(v, S){
	limes = 200 # hard-coded maximal degree of freedom
	sing = TRUE
	while( sing ){
		X = rwish(v, S)
		try(sing <- check.check(X, limes) , silent=T)
		}
	X = 0.5*(X + t(X))
	return(X)
	}

