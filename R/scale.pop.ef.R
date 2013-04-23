scale.pop.ef <-
function(posterior){
	nmc = dim(posterior)[3]
	phenodist = as.list(1:nmc)
	npop = dim(posterior)[1]
	ntrait = dim(posterior)[2]
	nc = npop*ntrait
	phenodist = as.list( rep(NA,nmc) )
	for( i in 1:nmc ){
		D = matrix(NA, npop, npop)
		pheno = posterior[,,i]
		phenodist[[i]] = into.dist(pheno)
		}
	return(phenodist)
	}

