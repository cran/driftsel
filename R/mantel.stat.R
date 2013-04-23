mantel.stat <-
function(A, B){
	D = A*B
	out = 0
	for( i in 1:nrow(D) ){
		for( j in 1:i ){
			out = out + D[i,j] } }
	return(out) }

