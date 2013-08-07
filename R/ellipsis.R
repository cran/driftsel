ellipsis <-
function(mu, Sig, prob.mass, lwd, col){
  radius <- sqrt(qchisq(prob.mass, 2))
  theta <- 2*pi*(0:360) / 360
  unit.circle <- cbind(cos(theta), sin(theta))
  Q <- base::chol(Sig, pivot=TRUE)
  order <- order(attr(Q, "pivot"))
  ellipse <- t(mu + radius * t(unit.circle %*% Q[, order]))
  lines(ellipse[,1], ellipse[,2], lwd=lwd, col=col)
  } 
  
