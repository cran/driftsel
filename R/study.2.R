study.2 <-
function(whistory, maxk, maxpar=6){
	whistory = round(whistory/maxk, 1)
	gridx = (1:9)/10
	gridy = sapply(gridx, function(x) length(which(whistory==x)))
	gridy = gridy / sum(gridy)
	parvec = optim.beta(gridx, gridy, maxpar=maxpar)
	gridprop = (1:maxk) / (maxk+1)
	props = sapply(gridprop, function(x) dbeta(x, parvec[1], parvec[2]))
	props = props / sum(props)
	return(props)
	}

