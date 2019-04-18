# word <- c("mar", "para", "suc")
# words <- data.frame(
#   word = word
# )
# save(words, file = "low.RData")

# word <- c("pneumonie", "enciclopedie", "silicovolcanic")
# words <- data.frame(
#   word = word
# )
# save(words, file = "hard.RData")

# 
#  word <- c("ananas", "birou", "masina")
#  words <- data.frame(
#    word = word
#  )
#  save(words, file = "data.RData")

read_letter <- function() {
  letter <- readline("Litera: ")
  return(letter)
}

hangman_plot <- function(mistakes) {
  size <- 40
  plot(c(-size, size), c(-size, size), type = "n", main = "Spanzuratoarea", xlab = "", ylab = "",
       xaxt = "n", yaxt = "n")
  segments(-30,-45, -30, 35)
  segments(-30, 35, -5, 35)
  
  if(mistakes >= 1) {
    segments(-5, 35, -5, 25)
  }
  if(mistakes >= 2) {
    r <- 5
    t <- seq(0, 2 * pi, length = 200)
    x <- r * cos(t)
    y <- r * sin(t)
    lines(x-5, y+20)
    lines(x/10-7, y/10+20)
    lines(x/10-3, y/10+20)
  }
  if(mistakes >= 3) {
    segments(-5, 15, -5, -15)
  }
  if(mistakes >= 4) {
    segments(-5, 10, 2, 0)
  }
  if(mistakes >= 5) {
    segments(-5, 10, -12, 0)
  }
  if(mistakes >= 6) {
    segments(-5, -15, 2, -25)
  }
  if(mistakes >= 7) {
    segments(-5, -15, -12, -25)
  }
}

start_game <- function() {
  letter <- read_letter()
  toDisplayNew <- ""
  found <- FALSE
  for (i in seq(1:n)) {
    if (tolower(substr(word,i,i)) == tolower(letter)) {
      toDisplayNew <- paste(toDisplayNew, substr(word,i,i), sep="")
      found <- TRUE
    } else { 
      toDisplayNew <- paste(toDisplayNew, substr(toDisplay,i,i), sep="") 
    }
  }
  
  if(found == FALSE) {
    mistakes <<- mistakes + 1
    hangman_plot(mistakes)
    if( usedLetters[1] == "none" ) {
      usedLetters[1] <<- letter
    } else {
      usedLetters <<- c(usedLetters, letter)
    }
  } else {
    
  }
  
  toDisplay <<- toDisplayNew
  cat("Pana acum ai ghicit: ", toDisplay, "\n")
  cat("Litere gresite: ", usedLetters, "\n")
}

init <- function() {
  selected_level_id <<- readline("SELECTEAZA NIVELUL: [1] = Usor; [2] = Mediu; [3] = Greu: ")
  
  cat("* JOCUL A INCEPUT *\n")
  selected_word_id <<- round(runif(1,1,nrow(words)))
  word <<- toString(words[selected_word_id,"word"])
  n <<- nchar(word)
  toDisplay <<- NULL
  usedLetters <<- c("none")
  mistakes <<- 0
  hangman_plot(mistakes)
  for (i in seq(1:n)) {    
    toDisplay <<- paste(toDisplay,"_",sep="") 
  }
  
  while( mistakes < 7 && toDisplay != word) {
    start_game()
  }
  
  if( toDisplay == word ) {
    msg <- c("Ai castigat, sefu' la cuvinte")
  } else {
    msg <- c("Mai citeste DEXu'")
  }
  cat(msg)
}

if (selected_level_id == 1)
{
  load("low.RData")
}
if (selected_level_id == 2)
{
  load("data.RData")
}
if (selected_level_id == 3)
{
  load("hard.RData")
}
init()