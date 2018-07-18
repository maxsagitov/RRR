x = c(rnorm(20),rnorm(20,3),seq(from=1,to=4,length.out=20))
y = c(rnorm(20),rnorm(20,3),seq(from=0,to=0.5,length.out=20))
col = c(rep('red',20),rep('blue',20),rep('green',20))
d=dist(cbind(x,y),method='euclidean')
h=hclust(d,method='complete')
plot(h)
dg = as.dendrogram(h)
colLab = function(n,cols) {
  leafs = order.dendrogram(n)
  leaf.col = unique(cols[leafs])
  if(length(leaf.col)==1){
    attr(n,'edgePar') = list(col=leaf.col)
  }
  n
}
dg = dendrapply(dg, colLab,col)
plot(dg,leaflab='none',main='Complete')