viz.theta <-
function(thetapost, distance=T, center=F, main=NA){

	# Multidimensional scaling
	npop = dim(thetapost)[1]
	theta = D = matrix(NA, npop, npop)
	for( i in 1:npop ){
		for( j in 1:i ){
			theta[i,j] = theta[j,i] = mean(thetapost[i,j,])
			} }
	for( i in 1:npop ){
		for( j in 1:i ){
			D2 = 2*(theta[i,i] + theta[j,j] - 2*theta[i,j])
			D[i,j] = D[j,i] = sqrt(D2)
			} }
	D = rbind(D, sqrt(2*diag(theta)))
	D = cbind(D, c(sqrt(2*diag(theta)), 0))
	D = sqrt(2/pi)*D # scaling SD -> E absolute value
	fit = cmdscale(D, k=2)
	if( center ){ fit[nrow(fit),] = c(0,0) }
		
	# Plotting
	if( is.na(main) ){
		main = "Expected drift distances (units of ancestral SD)"
		if( !is.na(distance) ){
			if( !distance ){
				main = "Population-level coancestry"
				}
			}
		}	
	eps = max(abs(fit))*0.1
	M = max(abs(fit)) + eps
	cx1 = 1 - 1*is.na(distance)
	plot(fit[,1], fit[,2], ylab="MDS 2", main=main, pch=16, xlim=c(-M,M), ylim=c(-M,M), axes=T, xlab="MDS 1", cex=cx1)
	eps = max(abs(fit))*0.1
	colvec = 1:9
	colvec[7] = "orange"
	colvec[9] = "chartreuse"
	eps = (1 - 1*is.na(distance))*eps
	text(fit[1:npop,1]+eps, fit[1:npop,2], 1:npop, col=colvec)
	text(fit[npop+1,1]+eps, fit[npop+1,2], "A")
		
	# Distance information
	if( !is.na(distance) ){
		for( i in 1:(npop+1) ){
			for( j in 1:(npop+1) ){
				lines(c(fit[i,1],fit[j,1]), c(fit[i,2], fit[j,2]))
				}
			}	
		eps = 0.8*eps
		if( distance ){ dd = round(D,2) }
		else{
			self = diag(theta)
			dd = rbind(theta,self)
			dd = cbind(dd,c(self,0))
			dd = round(dd,2)
			}
		for( i in 1:nrow(fit) ){
			if( i>1 ){
				for( j in 1:(i-1) ){
					text(0.5*(fit[i,1]+fit[j,1]), 0.5*(fit[i,2]+fit[j,2])+eps, dd[i,j], cex=0.75)			
					}
				}
			}
		}
	}

