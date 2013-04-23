convert <-
function(dat, dimy, npop, dimx){
	out = list(1:5)
	n = nrow(dat)
	out[[1]] = array(NA, dim=c(dimx, dimy, n))
	out[[2]] = array(NA, dim=c(npop, dimy, n))
	out[[3]] = array(NA, dim=c(dimy, dimy, n))
	out[[4]] = array(NA, dim=c(dimy, dimy, n))
	out[[5]] = array(NA, dim=c(npop, npop, n))
	for( i in 1:n ){
		dati = dat[i,]
		out[[1]][,,i] = matrix(dati[1:(dimx*dimy)], nrow=dimx)
		dati = dati[-(1:(dimx*dimy))]
		out[[2]][,,i] = matrix(dati[1:(npop*dimy)], nrow=npop)
		dati = dati[-(1:(npop*dimy))]
		out[[3]][,,i] = matrix(dati[1:(dimy*dimy)], nrow=dimy)
		dati = dati[-(1:(dimy*dimy))]
		out[[4]][,,i] = matrix(dati[1:(dimy*dimy)], nrow=dimy)
		dati = dati[-(1:(dimy*dimy))]
		out[[5]][,,i] = matrix(dati[1:(npop*npop)], nrow=npop)		
		}
	names(out) = c("fixed.ef","pop.ef","G","E","theta")
	return(out)
	}

