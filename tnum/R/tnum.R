
# Vars local to this file

#' @export
tnum.env <- new.env()


########################################################
#'@title Call Gen1 Truenumber API
#'
#' @param command  the API command name
#' @param params list of parameter name-value pairs
#' @noRd

tnum.callApi <- function(command = "version", params = list()){
  qr <- c(list(
    cmd = command,
    ns = tnum.env$tnum.var.nspace[[1]],
    auth = tnum.env$tnum.var.auth
  ), params)

  result <-
    httr::GET(
      paste0("http://", tnum.env$tnum.var.ip, "/Numberflow/API"),
      query = qr
      )
  assign("tnum.var.cookies", cookies(result), envir = tnum.env)
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
#' @param name Name of numberspace to set
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
                       start = 0,
                       fast = FALSE) {

  jResult = tnum.callApi("dashboard-search",
                         list(
                           string = "json",
                           qry = query,
                           limit = max,
                           offset = start,
                           simple = fast
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
  return(jResult$tags)

}

########################################################
#'@title Terminate HTTP session
#'
#' @export

tnum.terminateSession <- function() {

  jResult <-tnum.callApi("kill-session",
                         list()
  )
  return(jResult)

}


########################################################
#' @title Get all tags
#'
#' @return list of all tags in the numberspace
#' @export

tnum.getAllTags <- function(){

  tnum.callApi("get-field-set",
               list(field="subject",
                    context=paste0(tnum.getSpace(),":tags")
                    )
               )

}

########################################################
#' @title Get all subjects
#'
#' @return list of all subjects in the numberspace
#' @export

tnum.getAllSubjects <- function(){

  tnum.callApi("get-field-set",list(field = "subject"))

}

########################################################
#' @title Get all properties
#'
#' @return list of all properties in the numberspace
#' @export

tnum.getAllProperties <- function(){

  tnum.callApi("get-field-set",list(field = "property"))

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
    print("OK")
  } else {
    return(jResult)
  }
}



# Truenumber creation functions  #

########################################################
#' @title Create a tag for application later
#'
#' @param tag the path or phrase of the tag
#' @param text text of the comment associated with tag
#' @export

tnum.createTag <- function(tag, text = "") {
  mkTag <- tnum.callApi("create-tag", list(
    srd = tag,
    comment = text,
    nspace = tnum.env$tnum.var.nspace
  )
  )
}

########################################################
#' @title Delete a tag AND ALL IT'S APPLICATIONS!
#'
#' @param tag the path of the tag
#' @export

tnum.deleteTagFromNumberspace <- function(tag) {
  delTag <- tnum.callApi("delete-tag", list(
    tag = tag)
  )
}

########################################################
#'@title add a tag to a truenumber
#'
#' @param guid  ID of the tn to be tagged
#' @param tag  path of tag
#' @param text reason for application of tag
#' @export
#'

tnum.addTag <- function(guid, tag, text = "") {

  # create tag (fails quietly if exists already)
  tnum.createTag(tag)

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
#' @param number  GUID of the number
#' @param tag  matching the tag(s) to remove
#' @export
#'
tnum.removeTag <- function(number, tag) {
  jResult <-
    tnum.callApi("delete-tag-instance", list(
      tag = tag,
      guid = number
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


  return(str_trim(thenumber, side = "both"))
}

########################################################
#' Post a truenumber statement
#'
#' @param stmt  well-formed truenumber sentence
#' @param  notes  text description associated with the truenumber
#' @param tags   a list of tags. Each can be a tag or c("tag","comment"). default is timestamp "R:2022:02:26:12:24:33:EST"
#' @param  noreturn  if TRUE, suppresses return of the created TN
#' @param filedestination  if non-NULL, must be an open file to divert posts to
#' @returns the new truenumber JSON object (or NULL if suppressed)
#' @export
#'
tnum.postStatement <- function(stmt,
                               notes = "",
                               tags = list(paste0("source:R:",tnum.dateToken())),
                               noreturn = FALSE,
                               filedestination = NULL
                               )
{
  stmt1 <- str_remove_all(stmt,"â€”")

  # do file write if file open

  if(!is.null(filedestination)){
    writeLines(stmt, filedestination)
    if(notes != ""){
      writeLines(paste0("$ has description | ", trimws(notes)), theFile)
    }
    for(tg in tags){
      tp <- ""
      if(length(tg)>1){
        tp <- tg[[2]]
      }
      ts <- paste0("@ has ", tg[[1]])
      if(nchar(tp)>0){
        ts <- paste0(ts, " | ", tp)
      }
      writeLines(ts, filedestination)
    }

  } else {
  #get here if not writing file
  args <- list(
    ubox = stmt1,
    ns = tnum.env$tnum.var.nspace,
    tag = "origin:R/tnum"
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
      tnum.addTag(theGuid, tg)
    }
  }
  theTn <- NULL
  if(!noreturn){
    theTn <- tnum.callApi("htn-from-guid", list(guid = theGuid, core = "yes"))
  }

  return(theTn)
  }

}

########################################################
#'@title Post a TN file to the server
#'
#' @param infile   path to the file, usually the same given to the ingest function
#' @return post result
#' @export

tnum.writeTnFile <- function(infile) {
  ur <- paste0(
    "http://", tnum.env$tnum.var.ip,"/Numberflow/tspeakput.jsp?nsp=",
    tnum.env$tnum.var.nspace[[1]],
    "&auth=",
    ns = tnum.env$tnum.var.auth
  )
  result <-
    httr::POST(url = ur, body = list(y = httr::upload_file(infile)))
  assign("tnum.var.cookies", cookies(result), envir = tnum.env)

  return(result)


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
#' @title Start TN write caching in server
#' @export

tnum.startCache <- function(){
  tnum.callApi("start-cache", list())
}

########################################################
#' @title Stop TN write caching in server
#' @export

tnum.finishCache <- function(){
  tnum.callApi("finish-cache", list())
}



########################################################
########################################################
#' Utility functions
#' @noRd
#' @export

tnum.dateToken <- function(){
  tokenized = stringr::str_replace_all(now(), " ","/")
  tokenized = stringr::str_replace_all(tokenized, ":",".")
  return(tokenized)
}


