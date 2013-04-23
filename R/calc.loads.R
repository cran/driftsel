calc.loads <-
function(ped, THP){
	n = nrow(ped)
	npop = ncol(THP)
	wpop = matrix(0, nrow=n, ncol=npop)
	
	# Founders
	founders = which(!is.na(ped[,4]))
	for( i in founders ){
		wpop[i,ped[i,4]] = wpop[i,ped[i,4]] + 1
		}
	
	# Offspring
	maxf = max(founders)
	for( i in (maxf+1):n ){
		s = ped[i,2] # id codes
		d = ped[i,3]
		s = min(which(ped[,1]==s)) # row numbers
		d = min(which(ped[,1]==d))
		wpop[i,] = 0.5*(wpop[s,] + wpop[d,]) # 'pure loading'		
		}
		
	# Out	
	return(wpop)
	}

