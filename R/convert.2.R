convert.2 <-
function(dat, dd, nb){
	nn = nrow(dat)
	out2 = list(1:nb)
	for( k in 1:nb ){
		outthis = array(NA, dim=c(dd, dd, nn))
		targ = ((k-1)*dd^2) + 1:(dd^2)
		datthis = dat[,targ,drop=F]
		for( i in 1:nn ){
			outthis[,,i] = matrix(datthis[i,], nrow=dd, ncol=dd)
			}
		out2[[k]] = outthis
		}
	return(out2)
	}

