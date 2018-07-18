x = runif(1)
if(x> 0.5){
  print(paste(x,'> 0.5'))
}else{
  cat(x, '<=0.5\n',sep='')
}
i=4
repeat{
  i = i-1
  cat(i)
  if(i == 0) break
  
  if(i %% 2 == 0){
    cat('even, ')
    next
  }
  cat('uneven, ')
}
my.rbinom = function(n, p=0.5){
  x = sum(runif(n,max=1,min=0)<p)
  return(x)
}
my.rbinom(100)
my.rbinom(100, 0.9)
func = list(sd, var)
for(f in func) print(f(1:10))
m = matrix(rnorm(100,10+rep(1:10, each = 10)), ncol = 10)
apply(m, 2, function(x){exp(mean(log(x)))})
apply(m, 1:2, sqrt)
sweep(m,2,apply(m, 2, mean),'/')

matrix = matrix(1:12, ncol = 3)
string = apply(matrix, 1, paste, collapse=',')
string
matrix
spl.string = strsplit(string,',')
do.call(rbind, spl.string)

sex = ifelse(runif(20)>0.5,'m','f')
height = ifelse(sex == 'm', rnorm(20, 185, 5), rnorm(20, 165,5))
typeof(height)
split.height = split(height, sex)
boxplot(split.height)

text = c('hi', 'world')
substr(text,2,4)
strsplit(text,'l|r')
sex = ifelse(runif(20)>0.5,'m','f')
height = ifelse(sex=='m',rnorm(20,185,5),rnorm(20,165,5))
split.height = split(height, sex)
height
