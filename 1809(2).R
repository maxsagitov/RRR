T
num_var = c(1, 2.5, 4.5)
x = integer(10)
x[] = NA
y = NA
c(T, T)
typeof(c(T, T))
as.numeric(c('1')) + 2
f = factor(c('a', 'b', 'b'))
sex.factor <- factor(sex.char, levels = c('m', 'f'))
sex.char = c('m', 'm', 'm')
sex.factor
table(sex.factor)
x = 1:10
attr(x, 'my_attr') = 'ppc'
attributes(x)
my_attr(x)
names(x) = 21:30
x
x = c(a=1, b=3, c=-1, d=-5, d=10)
x[c(1, 4)]
x < 0
x [x<0][1]
x[(x<0)[1]]
a = matrix(1:6, ncol = 3, byrow = T)
a
b = array(1:12, c(2, 3, 2))
b
colnames(a) = c('a', 'b', 'c')
a
dimnames(a)
attributes(a)
x = data.frame(a=1:3, b=c(T,T,F), c = c('a', 'b', 'c'))
mtcars[mtcars[2] == 4, ]
subset(mtcars, mtcars[2]==min(mtcars[2]))
