calc.V <-
function(g, THP, G, priors){ # changes in the default application
	if( is.na(priors$fixed[[2]][1,1]) ){
		ng = length(g)
		n = ng - nrow(G)*nrow(THP)
		V = matrix(0, ncol=ng, nrow=ng)
		VF = 100*diag(n)
		Va = 2*G %x% THP
		V[1:n,1:n] = VF
		V[(n+1):ng,(n+1):ng] = Va # i.e. Va is a function of (G, THP)
		}
	else{ V = priors$fixed[[2]] }	
	return(V)
	}

