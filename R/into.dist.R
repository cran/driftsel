into.dist <-
function(covars){
	if( is.vector(covars) ){ covars = as.matrix(covars) }
	npop = nrow(covars)
	D = matrix(NA, npop, npop)
	for( j in 1:npop ){
		for( k in 1:j ){
			D[j,k] = sqrt(sum( (covars[k,] - covars[j,])^2 ))
			D[k,j] = D[j,k]
			}
		}	
	return(D)
	}

