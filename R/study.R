study <-
function(whistory, maxk, maxpar=30){
	y = whistory / (maxk+1)
	arange = brange = (10:(10*maxpar)) / 10
	grdim = length(arange)
	likemat = matrix(NA, ncol=grdim, nrow=grdim)
	for( i in 1:grdim ){
		for( j in 1:grdim ){
			a = arange[i]
			b = brange[j]
			likemat[i,j] = sum(dbeta(y, a, b, log=T))
			}
		}
	num = which(likemat==max(likemat))
	ai = as.integer(grdim*(num/grdim - floor(num/grdim)))
	if( ai==0 ){ ai = grdim }
	bi = ceiling(num/grdim)
	a = arange[ai]
	b = brange[bi]
	gridprop = (1:maxk) / (maxk+1)
	props = sapply(gridprop, function(x) dbeta(x, a, b))
	props = props / sum(props)
	return(props)
	}

