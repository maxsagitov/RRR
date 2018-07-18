t = t.test(c(0,0),c(0,0))
names(t)
t$p.value
t.test(a,b, alternative = 'less', var.equal = T)$p.value

a = rnorm(100)
b = rnorm(100, a + 0.2, sd = 0.5)
t.test(a,b)$p.value
t.test(a-b)$p.value

d = matrix(c(649, 593, 220, 263), ncol = 2)
fisher.test(d)
fisher.test(d, a='g')

x = rnorm(100)
y = rnorm(100, x)
pca = prcomp(cbind(x,y))
names(pca)
pca$rotation
pca$center
