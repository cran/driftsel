riwish_ <-
function(v, S){
	sing = TRUE
	while( sing ){
		X = riwish(v, S)
		try(sing <- check.check(X) , silent=T)
		}
	return(X)
	}

