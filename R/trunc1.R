trunc1 <-
function(U, mux, sdx, signx){
	gridx = (1:9999) / 10^4
	if( signx==-1 ){
		up = pnorm(0, mux, sdx)
		gridx = gridx*up
		}
	if( signx==1 ){
		lo = 1 - pnorm(0, mux, sdx)
		gridx = 1 - lo*gridx
		}
	vals = qnorm(gridx, mux, sdx)
	if( max(vals)==-Inf ){ vals = rep(-1.0e-07, 10^4) }
	else{ vals[which(vals==-Inf)] = min(vals[which(vals>-Inf)]) }
	if( min(vals)==Inf ){ vals = rep(1.0e-07, 10^4) }
	else{ vals[which(vals==Inf)] = max(vals[which(vals<Inf)]) }
	U = ceiling(9999*U)
	out = vals[U]
	return(out)
	}

