xx <-
function(cA, B){
	nc = ncol(B)
	product = matrix(NA, nrow=nrow(cA), ncol=ncol(B))
	for( j in 1:nc ){
		yep = solve(t(cA), B[,j])
		product[,j] = solve(cA, yep)
		}
	return(product)
	}

