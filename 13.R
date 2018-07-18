countWords = function(text = c('aabbbfbbabrbrabe'), w = 'aabe', n = 2){
  l = nchar(w) #длина искомой подстроки
  cw = initialize(nchar(text)) # вектор для всех подстрок
  
  for (i in 1:nchar(text)){
    word = substr(text, i, i+l-1)
    if (nchar(word) == l){
      cw[i] = word
    }
  } # заполняем его
  
  let_w = unlist(strsplit(w, '')) # делим искомую подстроку на символы
  
  for (i in cw){
    cnt = 0
    let_word = unlist(strsplit(i, ''))
    for (z in 1:l){
      if (let_word[z] != let_w[z]){
        cnt = cnt + 1
      }
    }
    if (cnt < n){
      print(match(i,cw))
      cw[match(i, cw)] = 0
    }
  }
} # сравниваем символы каждой подстроки с искомой, считаем ошибки, если их меньше заданного выводим на печать позицию
