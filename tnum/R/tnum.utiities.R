## TNUM Utilities

tokenize <- function(aString){
  tok <- str_replace_all(aString,"\\s+","_")
}

########################################################
#'@title load libraries required by TNUM
#'
#' @export

tnum.loadLibs <- function(){
  library(tnum)
  library(jsonlite)
  library(httr)
  library(lubridate)
  library(stringr)
  library(knitr)
}

#######################################################
#' @title ingest a data frame as verbatim truenumbers
#'
#' @param df  the data frame, as returned by read.csv() for example
#' @param subjectRoot string used as root of subject path
#' @param idColumn name of a column to be used at the end of the subject path
#' @param tag path used to tag all the created truenumbers
#' @export

tnum.ingestDataFrame <- function(df, subjectRoot, idColumn, tag = "origin:R/tnum"){

  dfRows <- dim(df)[[1]]
  dfCols <- dim(df)[[2]]

  for(i in 1:3){
    for(j in 1:dfCols){
      subj <- paste0(subjectRoot,df[i,]$idColumn)
      prop <- tokenize(names(df[i,][j]))
      val <- df[i,][[j]]

      #if value is mode character, quote it as a string
      if(is.character(val)){
        val <- str_trim(val, side = "both")
        val <- paste0("\"",val,"\"")
      }
      stmt <- tnum.buildStatement(subj,prop,val)
      print(stmt)
      tnResult <- tnum.postStatement(stmt,"",list(tag))
    }
  }


}
