cancer = rnorm(15,1.5)
healthy = 1
hist(cancer,seq(min(cancer),max(cancer),length.out = 10),
     xlab='Gene A expression level')
abline(v=healthy,col='red')
text(1,par('usr')[4],'healthy',adj=c(-0.1,1),col='red')
table(cancer > healthy)

c = sum(cancer>healthy)
t = length(cancer)
binom.prob = function(s,n,p){
  choose(n,s)*p^s*(1-p)^(n-s)
}
binom.prob(c,t,0.5)
[1] 0.0138855
sum(binom.prob(c:t,t,0.5))
[1] 0.01757812
y = binom.prob(0:t,t,0.5)
plot(0:t,y,t='l',ylab='probability',
     xlab='# of patients with elevated expression')
polygon(c(c,c:t,c),c(0,y[c:t + 1],0),col='red')
polygon(c(t-c,(t-c):0,t-c),c(0,y[c:t + 1],0),col='blue')
2*sum(binom.prob(c:t,t,0.5))
[1] 0.03515625
binom.test(c,t,0.5)$p.value
[1] 0.03515625
xbinom.test(c,t,0.5,a='g')$p.value

healthy = rnorm(15,1)
ustat = function(x,y){
  sum(sapply(x,function(z)sum(z>y)))
}
ustat(cancer,healthy)
[1] 172
wilcox.test(cancer,healthy)
