

# Vars local to this file

#' @export
tnum.env <- new.env()
library(jsonlite)
library(lubridate)
library(httr)

########################################################
#'@title Call Gen1 Truenumber API
#'
#' @param command  the API command name
#' @param params list of parameter name-value pairs
#' @noRd

tnum.callApi <- function(command = "version", params = list()){
  result <-
    httr::GET(
      paste0("http://", tnum.env$tnum.var.ip, "/Numberflow/API"),
      query = c(list(
        cmd = command,
        ns = tnum.env$tnum.var.nspace,
        auth = tnum.env$tnum.var.auth
      ), params)

      )
  payload <- httr::content(result, as = "text", encoding = "UTF-8")
  jResult = fromJSON(payload)
  return(jResult)
}

########################################################
#'@title Connect and authenticate to Truenumbers server
#'
#' @param ip The endpoint address of the server. Default is a public cloud server
#' @param creds  username:password to authorize server access
#'
#' @return  List of numberspaces available on the server. The first one on the list is set as current
#' @export

tnum.authorize <- function(ip = NULL, creds = "user:password") {
  if (is.null(ip)) {
    ip = "metrics.truenum.com:8080"
  }
  message(paste0("Truenumbers (", ip, ")"))

  assign("tnum.var.ip", ip, envir = tnum.env)
  assign("tnum.var.auth", creds, envir = tnum.env)

  ## get list of numberspaces

  jResult <- tnum.callApi("get-spaces", list())
  nspaces <- list()

  if (!is.null(jResult$error)) {
    message(jResult$error)
    return()
  } else {
    for (x in jResult$numberspaces) {
      nspaces <- append(nspaces, x)
    }

    assign("tnum.var.nspaces", nspaces, envir = tnum.env)

    tnum.setSpace(nspaces[1])
    message(paste0("Available spaces: ", paste0(unique(nspaces), collapse = ", ")))
    message(paste0("Numberspace set to: ", tnum.getSpace()))

  }
}


########################################################
#'@title Set a particular numberspace as current
#'
#' @param name Name of numberspace
#'
#' @export
#'

tnum.setSpace <- function(name = "testspace") {
  if (name %in% tnum.env$tnum.var.nspaces) {
    assign("tnum.var.nspace", name, envir = tnum.env)
  } else {
    stop(paste0('server has no numberspace "', name, '"'))
  }
}


########################################################
#'@title Get current numberspace name
#'
#' @return name of the current numberspace
#' @export
#'

tnum.getSpace <- function() {
  returnValue(tnum.env$tnum.var.nspace)
}


########################################################
#'@title Query the truenumber server
#'
#' @param query  string in tnum query language
#' @param max    maximum number of truenumbers to return
#' @param start  begin return with this sequence number
#'
#' @returns list of json truenumber as received from server
#' @export
#'

tnum.query <- function(query = "* has *",
                       max = 10,
                       start = 0) {

  jResult = tnum.callApi("dashboard-search",
                         list(
                           string = "json",
                           qry = query,
                           limit = max,
                           offset = start
                           )
                         )
  numReturned <- length(jResult$truenumbers$subject)

  message(
    paste0(
      "Returned ",
       numReturned,
      " results"
    )
  )

  assign("tnum.var.result", jResult, envir = tnum.env)
  return(jResult$truenumbers)
}

########################################################
#'@title Get tags of TN guid
#'
#' @param guid  class id of  truenumber
#' @export

tnum.getTagsOfTn <- function(id) {

  jResult <-tnum.callApi("tags-of-guid",
               list(
                 guid = id
               )
  )
  return(jResult)

}

########################################################
#'@title  Delete tnums specified by a query
#'
#' @param query  string in tnum query language
#'
#' @export
#'

tnum.deleteByQuery <- function(query = "") {
  jResult <-
    tnum.callApi("dashboard-search", list(
        string = "json",
        qry = paste0(query," DELETE!")
      )
    )
  if(length(jResult) == 0){
    return("OK")
  } else {
    return(jResult)
  }
}



# Truenumber creation functions  #

########################################################
#'@title add a tag to a truenumber
#'
#' @param guid  ID of the tn to be tagged
#' @param tag  path of tag
#' @param text comment for tagging action
#' @export
#'

# create tag (fails quietly if exists already)
tnum.addTag <- function(guid, tag, text = "") {
  tnum.callApi("create-tag", list(
    srd = tag,
    comment = text,
    nspace = tnum.env$tnum.var.nspace
  )
  )

# apply tag

  jResult <-
    tnum.callApi("tag-base", list(
      srd = tag,
      comment = text,
      nspace = tnum.env$tnum.var.nspace,
      base = guid
    )
    )

}

########################################################
#'@title add a part to a truenumber (internal only)
#'
#' @param guid  ID of the tn to be tagged
#' @param tag  path of tag
#' @param text comment for tagging action
#' @noRd
#'

tnum.addPart <- function(guid, tag, text = "") {
  jResult <-
    tnum.callApi("tag-base", list(
      srd = tag,
      comment = text,
      nspace = tnum.env$tnum.var.nspace,
      part = TRUE,
      base = guid
    )
    )

}

########################################################
#'@title remove a tag from a truenumber
#'
#' @param tag  matching the tag(s) to remove
#' @param number  GUID of the number
#' @export
#'
tnum.removeTag <- function(tag, number) {
  jResult <-
    tnum.callApi("tag-base", list(
      srd = tag,
      comment = text,
      nspace = tnum.env$tnum.var.nspace,
      part = TRUE,
      base = guid
    )
    )

}

########################################################
#' @title Build a truenumber statement from parts
#'
#' @param subject TN subject
#' @param property TN property
#' @param value TN value
#' @param error std error for numerical value
#' @param unit  units of measure like m/s^2
#' @export
#'
tnum.buildStatement <- function(subject = "something",
                            property = "property",
                            value = NA,
                            error = NA,
                            unit = ""
                            )
{
  numval <- NA
  if (mode(value) == "numeric") {
    unitSuffix <- ""
    if (!(is.null(unit) || is.na(unit))) {
      unitSuffix <- paste0(" ", unit)
    }
    if (!(is.null(error) || is.na(error))) {
      numval <- paste0(value, " +/- ", error, unitSuffix)
    } else {
      numval <- paste0(value, unitSuffix)
    }
  } else {
    numval <- value
    if (!stringr::str_starts(numval, '"') &&
        !stringr::str_detect(numval, "^[0-9a-zA-Z/:\\-_]+$")) {
      numval <- str_replace_all(numval, "\"", "\\\\\\\"")
      numval <-
        paste0("\\\"", numval, "\\\"") ## if not SRD, and not quoted text, then add quotes
    }

  }

  thenumber <-
    paste0(subject,
           ' has ',
           property,
           ' = ',
           numval)


  return(thenumber)
}

########################################################
#' Post a truenumber statement
#'
#' @param stmt  well-formed truenumber sentence
#' @param  notes  text description associated with the truenumber
#' @param tags   a list of tags. Each can be a tag or c("tag","comment"). default is timestamp "R:2022:02:26:12:24:33:EST"
#' @returns the new truenumber JSON object
#' @export
#'
tnum.postStatement <- function(stmt,
                               notes = "",
                               tags = list(c("source:R",tnum.dateToken()))
                               )
{
  args <- list(
    ubox = stmt,
    ns = tnum.env$tnum.var.nspace
     )

  if(nchar(notes) > 1){
    args <- c(args, list(description = notes))
  }

  theTn = tnum.callApi("enter-unibox", args)

  if(!is.null(theTn$error)){
    return(theTn$error)
  }

  strtGuid <- stringr::str_locate(theTn, "\\?guid=")[[2]]+1
  endGuid <- stringr::str_locate(theTn, "\\&base=")[[1]] -1
  theGuid <- stringr::str_sub(theTn, strtGuid, endGuid )

  for(tg in tags){
    if(length(tg)>1){
      tnum.addTag(theGuid,tg[[1]],tg[[2]])
    } else {
      tnum.addTag(theGUid, tg)
    }
  }
  theTn <- tnum.callApi("htn-from-guid", list(guid = theGuid, core = "yes"))

  return(theTn)

}

########################################################
#' @title make a json array from a list of json objects
#' @param objectList  the list of json objects
#' @returns json array in R format (jsonlite)
#' @export

tnum.jsonArray <- function(objectList){
  jsonString <- "["
  delim <- ""
  for(ob in objectList){
    jsonString <- paste0(jsonString,delim,toJSON(ob))
    delim <- ","
  }
  jsonString <- paste0(jsonString,"]")

  return(fromJSON(jsonString))
}



########################################################
########################################################
#' Utility functions
#' @noRd
#'

tnum.dateToken <- function(){
  tokenized = stringr::str_replace_all(now(), " ","_")
  tokenized = stringr::str_replace_all(tokenized, ":",".")
}


