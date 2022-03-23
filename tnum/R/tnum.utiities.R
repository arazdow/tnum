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
#' @param subjectTerms a list of string pairs appended to subjectRoot build the subject. The
#' second string in each pair must be the name of a column in the df, and the first is a prefix
#' placed in the path before the value of that column.
#' @param propTerms similar to subjectTerms, but has pairs named by a df column name:
#'   list(colname = c("path:property", "unit of measure")).  The property path if not empty will be
#'   used as the TN property, and if empty, the column name will be used. The units are appended to the value.
#' @param tag path used to tag all the created truenumbers
#' @concept  ingestDataFrame(df, subjectRoot = "fauna",
#'                          subjectTerms = list(c(":american/","type"), c(":", "status")),
#'                          propTerms = list(c("density:population","pop-dens","items/acre")),
#'                          tag = "demo:ingest")
#'
#'   This will produce a TN for each cell in the row of the form:
#'
#'   fauna:american/bison:endangered has density:population = 2.45 items/acre which expands to:
#'
#'   population density of endangered bison, american fauna is 2.45 items/acre
#'
#' @export

tnum.ingestDataFrame <- function(df, subjectRoot, subjectTerms = list(), propTerms = list(),tag = "origin:R/tnum"){

  dfRows <- dim(df)[[1]]
  dfCols <- dim(df)[[2]]
  tnCount <- 0
  longProps <- list()

  for(i in 1:dfRows){
    subj <- subjectRoot
    for(pair in subjectTerms){
      subj = paste0(subj,pair[[1]], tokenize(df[i,][[pair[[2]]]]))
    }
    if(subj == subjectRoot){subj = paste0(subj,"/row:",i)}
    for(j in 1:dfCols){
      colName <- names(df[i,][j])
      prop <- tokenize(colName)
      if(!is.null(propTerms[[colName]])){
        prop <- propTerms[[colName]]
      }

      val <- df[i,][[j]]

      #if value is mode character, quote it as a string
      if(!is.na(val) && is.character(val)){
        val <- str_replace_all(val,"â€“", "" )
        val <- str_replace_all(val,"â€¦", "" )
        val <- paste0("\"",val,"\"")
      }
      if(!is.na(val) && !is.null(val) && (nchar(val) > 2)){
        ## deal with overlong column names
        if(nchar(prop) > 25){

          if(!(prop %in% longProps)){

            longProps <- c(longProps,list(prop))

            props <- tokenize(substring(colName,1,25))
            prop <- paste0(props,":t",which(longProps == colName))

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
#' @return first n terms of path.
#' @export
#'

tnum.subPath <- function(path, n=1){
  p <- str_locate_all("foo:bar/blatz:biff/nod",":|/")
  return(substr(path,0,p[[1]][,1][[n]]-1))
}


