d = read.table("counts5.txt")
d = sweep(d,2,apply(d,2,sum), '/') #use '/' for every cell
d = as.matrix(d)
apply(d, 2, sum)
table(apply(d,1,sum) == 0)
d = d[apply(d,1,mean) > 0,] #could be 'var' instead of 'sum', or 'mean'
dim(d)

pv = apply(d,1,function(x)t.test(x[1:4], x[5:8])$p.value)
pv.bh = p.adjust(pv, method = 'BH')
sum(pv.bh<0.05)
pv.thr = max(pv[pv.bh<0.05])
comb = combn(1:8,4) #combinations for permutation of cols
pv.sh = apply(comb[,1:10],2,function(s){
  cat(1)
  apply(d,1,function(x)t.test(x[s],x[-s])$p.value)
})
pv.sh.bh = apply(pv.sh,2,p.adjust,method = 'BH')
apply(pv.sh.bh < 0.05,2,sum)
sgn.cnt = apply(pv.sh <= pv.thr,2,sum)
hist(sgn.cnt[-1]/sgn.cnt[1])
