

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
#' @return  a list of json truenumbers
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

  return(jResult)
}



# Truenumber creation functions  #

########################################################
#' Build a truenumber sentence from parts
#'
#' @param subject
#' @param property
#' @param value
#' @param error
#' @param unit
#' @param notes
#'
tnum.buildTnStatement <- function(subject = "something",
                            property = "property",
                            value = NA,
                            error = NA,
                            unit = "",
                            notes = "")
{
  numval <- NA
  if (mode(value) == "numeric") {
    unitSuffix <- ""
    if (!(is.null(unit) || is.na(unit))) {
      unitSuffix <- paste0(" ", unit)
    }
    if (!(is.null(error) || is.na(error))) {
      numval <- paste0(value, " +/- ", numeric.error, unitSuffix)
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

  if(notes != ""){
    thenumber <-
      paste0(
        thenumber,
        ' //',
        notes
      )
  }

  return(thenumber)
}

