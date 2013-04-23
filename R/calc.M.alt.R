calc.M.alt <-
function(wpop, THP, ped){

	# Population-level effects
	n = nrow(wpop)
	npop = ncol(THP)
	Cov_pp = theta = matrix(0, n, n)
	for( i in 1:n ){
		for( j in 1:i ){
			Cov_pp[i,j] = t(wpop[i,]) %*% THP %*% wpop[j,]
			Cov_pp[j,i] = Cov_pp[i,j]
			}
		}
		
	# Total coancestry
	founders = which(!is.na(ped[,4]))
	for( i in founders ){
		for( j in 1:i ){
			if( i==j ){ 
				theta[i,j] = 0.5 + 0.5*THP[ped[i,4],ped[i,4]] 
				}
			else{ theta[i,j] = THP[ped[i,4],ped[j,4]] }
			theta[j,i] = theta[i,j]
			}
		}
	offs = which(is.na(ped[,4]))
	for( i in offs ){
		s = ped[i,2] # id codes
		d = ped[i,3]
		s = min(which(ped[,1]==s)) # row numbers
		d = min(which(ped[,1]==d))
		for( j in 1:i ){
			if( j<i ){
				theta[i,j] = 0.5*(theta[j,s] + theta[j,d])
				}
			else{ # self
				theta[i,i] = 0.5 + 0.5*theta[s,d]
				}
			theta[j,i] = theta[i,j]
			}
		}
	
	# Residual covariance
	M = theta - Cov_pp
	return(M)
	}

