check.order <-
function(traits, binary){
  if( !is.na(binary) ){
    if( length(binary) < ncol(traits) ){
      guide = 1:ncol(traits)
      nonbin = guide[-binary]
      if( min(binary) < max(nonbin) ){
        print("Order of traits changed in output for computational convenience,", quote=F)
        print("present order:", quote=F) 
        print(guide[c(nonbin,binary)])
        print("N.B. G matrix, pop.ef, etc.", quote=F)
        }
      }
    }
  }

