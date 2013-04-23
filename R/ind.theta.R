ind.theta <-
function(pars, censped){
	n = nrow(pars)
	th = matrix(0, nrow=n, ncol=n)
	for( i in 1:n ){
		for( j in 1:i ){
			x = 0
			if( i==j ){ x = 0.5 }
			else{
				if( (censped[i]+censped[j])==0 ){
					x = 0.125*( 1*(pars[i,1]==pars[j,1]) + 1*(pars[i,2]==pars[j,2]) )
					}
				}	
			th[i,j] = th[j,i] = x
			}
		}
	return(th)
	}

