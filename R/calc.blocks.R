calc.blocks <-
function(blocks){
	blocks = as.matrix(blocks[,-1], nrow=nrow(blocks)) # IDs off
	nb = ncol(blocks)
	dimmat = nrow(blocks)
	output = array(0, dim=c(dimmat, dimmat, nb))
	for( i in 1:nb ){
		VB = matrix(0, ncol=dimmat, nrow=dimmat)
		levs = unique(blocks[,i])
		for( k in levs ){
			targ = which(blocks[,i]==k)
			VB[targ,targ] = 1
			}
		output[,,i] = VB
		}
	return(output)
	}

