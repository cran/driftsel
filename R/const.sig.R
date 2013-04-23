const.sig <-
function(E, VR, G, M, blocks, Blist, bbit, sparse){
	Sig = (2*G %x% M) + (E %x% VR) # so far so good
	if( bbit ){
		for( i in 1:(dim(blocks)[3]) ){
			VB = blocks[,,i]
			if( sparse ){
				VB = as.matrix.csr(VB)
				}
			Sig = Sig + (Blist[,,i] %x% VB)
			}
		}
	return(Sig)
	}

