countWords = function(text = 'abbbaaa', len = 2){
  cw_matrix = initialize(nchar(text))
  for (i in 1:nchar(text)){
    words = substr(text, i, i+len-1)
    if (nchar(words) == len){
      cw_matrix[i] = words
    }
  }
  print(table(cw_matrix))
}
countWords()

createString = function(abc = c('A','T','G','C'), len = 10){
  txt = character(len)
  len = (runif(len, 1, length(abc)+1))%/%1
  count = 1
  for (i in len){
    txt[count] = abc[i]
    count = count + 1
  }
  print(paste(txt,collapse = ''))
}
createString()
