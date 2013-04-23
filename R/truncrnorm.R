truncrnorm <-
function(n, mux, sdx, signx){ # inefficient, calculates bins n times; yet redundant
	out = sapply(1:n, function(x) trunc1(runif(1,0,1), mux, sdx, signx) )
	return(out)
	}

