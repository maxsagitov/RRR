fack = function(x){
  y = 1
  for(i in 1:x){
    y = y*i
    cat(y)
  }
  #print(i)
  return(y)
}

fack = function(x){
  ifelse(x==0,1,x*fack(x-1))
}

fack(3)

fu = function(x){
  res = 1/mean(1/x)
}

lapply(mtcars, fu)

calcPolynomial = function(x, coefs){
  pol =0
  for (i in 1:length(coefs)){
    t = coefs[i]*(x^(i-1))
    pol = pol + t
  }
  return(pol)
}
calcPolynomial(2, 1:3)
