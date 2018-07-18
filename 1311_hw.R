# significant 65
# FDR 0.04301787

mydata = read.table("counts5.txt")

for (i in 1:8) {
  mydata[i] = mydata[i]/sum(mydata[i])
}

before = mydata[,1:4]
after = mydata[,5:8]

PV = numeric(20000) 
for (i in 1:20000) {
  PV[i] = t.test(before[i,],after[i,])$p.value 
}
#
h_alt = sum(PV < 0.05, na.rm = T)

PV_adj = p.adjust(PV, method = 'BH')
h_alt_adj = sum(PV_adj < 0.05, na.rm = T)

FDR = h_alt_adj/h_alt

#2
pv.thr = max(pv[pv.bh < 0.05])

combn(1:8, 4)
apply(comb[1:10], 2, function(s){
  cat(1)
  apply(d,1,function(x)t.test(x[s], x[-s])$p.value)
})
apply(pv.sh.bh < 0.05, 2, sum)
sgn.cnt = apply(pv.sh <= pv.thr, 2, sum)
hist(sgn.cnt[-1]/sgn.cnt[1])

"""
permureMean = function(x,y,n=1000){
  t = c(x,y)
  r = numeric(n)
  for(i in 1:n){
    t = sample(t)
    r[i] = mean(t[1:length(x)]) - mean(t[-(1:length(x))])
  }
  r
}
p = permureMean(before,after)
for (i in 1:100){
  newRow = order(mydata[1,])
  for (n in 1:8){
    
  }
}
"""
d = read.table("counts5.txt")
d = sweep(d,2,apply(d,2,sum), '/')
pv = apply(d, 1, function(x)t.test(x[1:4],))
d = as.matrix(d)
plot(d)
d
table(apply(d, 1, sum) == 0)
d = d[apply(d,1,sum)>0,]
dim(d)
