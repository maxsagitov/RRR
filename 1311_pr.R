M1 = matrix(rnorm(50000), ncol=5)
M2 = matrix(c(rnorm(2500, mean = 3),rnorm(47500)), ncol=5, byrow = T)
PV = numeric(10000) 
for (i in 1:10000) {
  PV[i] = t.test(M1[i,],M2[i,])$p.value 
}
pv = sapply(1:nrow(c), function(i)t.test(c))
hist(pv)

#4
h_alt = sum(PV < 0.05)
h_alt_2 = sum(PV[501:10000] < 0.05)
FP = h_alt_2/h_alt
table(pv < 0.05)

#PV_FDR = 0.05*10000/h_alt

#5
PV_adj = p.adjust(PV, method = 'BH')
H_alt = sum(PV_adj < 0.05)
sum(pv.bh[(s+1):N] < 0.05)/sum(pv.bh < 0.05)
#PV_FDR = 0.05*PV_adj/10000
#h_alt_adj = sum(PV_adj < 0.05)
#real_FDR = 

#7
max(pv[pv.bh < 0.05])
dmin = sapply(1:nrow(c), function(i)mean(t[]))
hist(dmian[pv.bh < 0.05], 20)
abline(v = 3, col = 'red')
hist(dmean[1:500], 20)
for (i in 1:10000) {
  d[i] = t.test(M1[i,],M2[i,])$p.value 
}
h_alt
