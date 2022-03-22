## TNUM Utilities

tokenize <- function(aString){
  tok <- str_replace_all(str_trim(aString, side= "both"),"\\s+","_")
  tok <- str_replace_all(tok,"[^a-zA-Z0-9'_]","-")
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
#' @param subjectRoot string used as root of subject path. It should not end with : or /
#' @param idType string appended to subjectRoot to prefix the idColumn token. Must start and end with : or /
#' @param idColumn name of a column to be used at the end of the subject path to identify the row
#' @param tag path used to tag all the created truenumbers
#' @export

tnum.ingestDataFrame <- function(df, subjectRoot, idType = "/row:", idColumn, tag = "origin:R/tnum"){

  dfRows <- dim(df)[[1]]
  dfCols <- dim(df)[[2]]
  tnCount <- 0
  longProps <- list()

  for(i in 1:dfRows){
    for(j in 1:dfCols){
      subj <- paste0(subjectRoot,idType, tokenize(df[i,][[tokenize(idColumn)]]))
      colName <- names(df[i,][j])
      prop <- tokenize(colName)
      val <- df[i,][[j]]

      #if value is mode character, quote it as a string
      if(!is.na(val) && is.character(val)){
        val <- str_replace_all(val,"â€“", "" )
        val <- str_replace_all(val,"â€¦", "" )
        val <- paste0("\"",val,"\"")
      }
      if(!is.na(val) && !is.null(val) && (nchar(val) > 2)){
        ## deal with overlong column names
        if(nchar(colName) > 25){

          if(!(colName %in% longProps)){

            longProps <- c(longProps,list(colName))

            prop <- tokenize(substring(colName,1,25))
            prop <- paste0(prop,":t",which(longProps == colName))

            colStmt <- tnum.buildStatement(paste0(subjectRoot,"/column:",prop),
                                           "text:heading:full",paste0("\"",colName,"\""))
            print(paste0("  ---> ",colStmt))
            res <- tnum.postStatement(colStmt,"had to truncate this heading",list(tag))
          } else {
            prop <- tokenize(substring(colName,1,25))
            prop <- paste0(prop,":t",which(longProps == colName))
          }
        }

        stmt <- tnum.buildStatement(subj,prop,val)
        print(stmt)
        tnResult <- tnum.postStatement(stmt,"",list(tag))
        tnCount <- tnCount + 1

      }
    }
  }
    print(paste0(tnCount, " TNs posted"))
    return (tnCount)

}

########################################################
#'@title Get length of path
#'
#' @param path  phrase path
#' @returns number of segments
#' @export
#'

tnum.pathLength <- function(path){
  return(str_count(path,":|/") + 1)
}

########################################################
#'@title Get subpath of length N
#'
#' @param path the path
#' @param n length of desired sub-path
#' @return first n segments of path.
#' @export
#'

tnum.subPath <- function(path, n=1){
  p <- str_locate_all("foo:bar/blatz:biff/nod",":|/")
  return(substr(path,0,p[[1]][,1][[n]]-1))
}


