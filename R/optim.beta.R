optim.beta <-
function(gridx, gridy, maxpar=6){
	eps = (gridx[2] - gridx[1]) / 2
	grdim = maxpar*10
	arange = brange = (1:grdim) / 10
	errormat = matrix(NA, ncol=grdim, nrow=grdim)
	for( i in 1:grdim ){
		for( j in 1:grdim ){
			a = arange[i]
			b = brange[j]
			f = sapply( gridx, function(x) (pbeta(x+eps, a, b) - pbeta(x-eps, a, b)) )
			f = f / sum(f)
			errormat[i,j] = sum((f - gridy)^2)
			}
		}
	num = which(errormat==min(errormat))
	ai = as.integer(grdim*(num/grdim - floor(num/grdim)))
	if( ai==0 ){ ai = grdim }
	bi = ceiling(num/grdim)
	a = arange[ai]
	b = brange[bi]
	f = sapply( gridx, function(x) (pbeta(x+eps, a, b) - pbeta(x-eps, a, b)) )
	f = f / sum(f)
	out = c(a, b)
	return(out)
	}

