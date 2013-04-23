check.check <-
function(X, limes){
	solve(X/limes)
	solve(X*limes)
	return(FALSE) # this replaces TRUE which indicates problems
	}

