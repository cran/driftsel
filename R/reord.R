reord <-
function(traits, binary){
  if( is.na(binary) ){
    out = traits
    }
  else{
    if( length(binary) < (ncol(traits) - 1) ){
      id = traits[,1]
      traits = traits[,-1]
      guide = 1:ncol(traits)
      nonbin = guide[-binary]
      out = traits[,c(nonbin, binary)]
      out = cbind(id, out)
      }
    else{
      out = traits
      }
    }    
   return(out)
   }

