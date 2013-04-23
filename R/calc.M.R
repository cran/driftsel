calc.M <-
function(pars, THP, THB, censped){
	n = nrow(THB)
	dif = matrix(0, nrow=n, ncol=n)
	for( i in 1:n ){
		for( j in 1:i ){	
			x = 0
			if( (censped[i]+censped[j])==0 ){
				if( THB[i,j]>=0.25 ){ # full-sibs or self
					S = pars[i,3]
					D = pars[i,4]
					x = 0.125*( THP[S,S] + THP[D,D] )
					if( i==j ){ x = 2*x }
					}
				if( THB[i,j]==0.125 ){ # half-sibs
					if( pars[i,3]==pars[j,3] ){
						S = pars[i,3]
						x = 0.125*THP[S,S]
						}
					if( pars[i,4]==pars[j,4] ){
						D = pars[i,4]
						x = 0.125*THP[D,D]
						}
					}
				}
			dif[i,j] = dif[j,i] = x
			}
		}
	M = THB - dif	
	return(M)
	}

