update.y <-
function(y_, y, g, W, Sig, binary, ntr, first=FALSE){

	# Basics
	mu <- W %*% g
	nin <- length(y) / ntr
	bins <- NA
	for( b in binary ){
		binb <- (1:nin) + (b-1)*nin
		bins <- c(bins, binb)
		}
	bins <- bins[-1]
	nonbins <- (1:length(y))[-bins]

	# First iteration
	if( first ){
		y <- y_
		for( i in bins ){
			if( y_[i]==1 ){ y[i] <- 0.01 }
			if( y_[i]==0 ){ y[i] <- -0.01 }
			}
		}
	
	# Other iterations
	else{
		y_[which(y_==0)] <- -1 # sign for normal truncation
		y <- y
		V <- solve(Sig)
		V <- as.matrix(V)
		Sig <- as.matrix(Sig)
		for( j in bins ){
			B <- -V[j,-j] / V[j,j]
			mu.j <- mu[j] + B %*% (y[-j] - mu[-j])
			s2.j <- Sig[j,j] - B %*% Sig[-j,j]
			if( s2.j <= 0 ){ s2.j <- 1.0e-04 }
			y[j] <- truncrnorm(1, mu.j, sqrt(s2.j), y_[j])
			}
		}		
	return(y)
	}
