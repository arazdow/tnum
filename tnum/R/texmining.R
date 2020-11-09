library(janeaustenr)
library(dplyr)
library(stringr)

detectAllCap <- function(line){
  line <- trimws(line)
  lline <- toupper(line)
  return (line == lline)
}
detectChapter <- function(line){
    return (detectAllCap(line) && startsWith(line,"CHAPTER"))
}

detectVolume <- function(line){
  return (detectAllCap(line) && startsWith(line,"VOLUME"))
}

detectBreak <- function(line){
  if(line == "") return(TRUE)
  if(trimws(line) == "by") return(TRUE)
  if(stringr::str_detect(tolower(line),"jane austen")) return(TRUE)
  if(str_detect(line, "[(]\\d\\d\\d\\d[)]")) return(TRUE)
  return(FALSE)
}

detectBook <- function(line){
  if(detectAllCap(line)){
    if(detectChapter(line)) return(FALSE)
    if(detectVolume(line)) return(FALSE)
    return(TRUE)
  } else return(FALSE)
}

#' Title Truenumbers from books
#'
#' @param books as returned by tidytext
#'
#' @export
#'
tnBooksFromLines <- function(books){

  chapter <- ""
  volume <- ""
  sentence <- ""
  sentencenum <- 0
  paragraph <- 0
  curline <- 0
  state <- "header"
  blanks <- 0
  bks <- as.matrix(books)
  bklen <- length(books$text)
  tn <- list()
  for(j in 1:bklen){
    bookrow <- bks[j,]
    line <- bookrow[[1]]
    bk <- bookrow[[2]]
    bk <- stringr::str_replace_all(bk," ","_")
    bk <- stringr::str_replace_all(bk,"&","and")
    curline <- curline+1
    line <- trimws(line)
    lline <- toupper(line)
    if(detectBreak(line)){
      blanks <= blanks + 1
      if(blanks == 0){
        sentencenum <- 0
        sentence <- ""
      }
    } else {
    blanks <- 0
    if(detectAllCap(line)){
      if(detectChapter(line)) chapter <- line
      if(detectVolume(line)) volume <- line
      paragraph <- 0
    }
    else {
       # process text (headers eliminated above)
         ln <- line
         if(stringr::str_detect(ln,"\\.")){
           matchs <- stringr::str_locate_all(pattern ='\\.', ln)
           beg <- 0
           for(i in matchs[[1]]){
             sentence <- paste0(sentence,substr(ln,beg,i),collapse="")
             beg <- i
             subj <- paste0("austen:jane:",tolower(bk),"/",tolower(str_replace(chapter,"\\s+","-")),"/paragraph-",paragraph+1,"/sentence-",sentencenum+1,collapse = '')
             tn[[length(tn)+1]] <- tnum.makeObject(subj,"text",sentence,"")
           }
           sentence <- trimws(substr(ln,beg+1,nchar(ln)))
         }
       }
    }
  }
}
