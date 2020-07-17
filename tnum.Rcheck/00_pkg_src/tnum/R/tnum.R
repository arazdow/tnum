
#' Connect and authenticate to Truenumbers server
#'
#' @param ip The endpoint address of the server. Default is a public cloud server
#'
#' @return  List of numberspaces available on the server. The first one on the list is set as current
#' @export

tnum.authorize <- function(ip = "54.166.186.11") {
  assign("tnum.var.ip", ip, envir = .GlobalEnv)
  result <- httr::POST(
    paste0("http://", ip, "/v1/gateway/"),
    body = paste0('{"email":"admin@truenumbers.com"}'),
    httr::accept("application/json"),
    httr::content_type("application/json")
  )

  token <- httr::content(result)$data$token

  ## get list of numberspaces
  result <- httr::GET(paste0("http://", ip, "/v1/numberspace/"),
                      httr::add_headers(Authorization = paste0("Bearer ", token)))
  nspaces <- list()

  for (x in httr::content(result)$data) {
    nspaces <- append(nspaces, x[[2]])
  }
  assign("tnum.var.nspace", nspaces[[1]], envir = .GlobalEnv)
  assign("tnum.var.nspaces", nspaces, envir = .GlobalEnv)
  assign("tnum.var.token", token, envir = .GlobalEnv)
  returnValue(nspaces)

}

#' Set a particulara numberspace as current
#'
#' @param name Name of numberspace
#'
#' @return  -none-
#' @export
#'

tnum.setspace <- function(name = "testspace") {
  if (name %in% tnum.var.nspaces) {
    assign("tnum.var.nspace", name, envir = .GlobalEnv)
  } else {
    stop(paste0('server has no numberspace "', name, '"'))
  }
}

#' Get current numberspace name
#'
#' @return -none-
#' @export
#'

tnum.getspace <- function() {
  returnValue(tnum.var.nspace)
}


#' Query the truenumber DB
#'
#' @param query  string in tnum query language
#' @param max    maximum nuber of truenumbers to return
#' @param start  begin return with this sequence number
#'
#' @return  a data.frame where each row is a returned truenumber
#' @export
#'

tnum.query <- function(query = "* has *",
                       max = 10,
                       start = 0) {
  args <-
    list(
      numberspace = tnum.var.nspace,
      limit = max,
      offset = start,
      tnql = query
    )

  result <-
    httr::content(httr::GET(
      paste0("http://", tnum.var.ip, "/v1/numberspace/numbers"),
      query = args,
      httr::add_headers(Authorization = paste0("Bearer ", tnum.var.token))
    ))
  numReturned <- length(result$data$truenumbers)
  if (numReturned > max) {
    numReturned <- max
  }
  first <- 0
  if (numReturned > 0) {
    first <- start + 1
  }
  message(
    paste0(
      "Returned ",
      first,
      " thru ",
      start + numReturned,
      " of ",
      result$data$meta$records,
      " results"
    )
  )

  assign("tnum.var.result", result, envir = .GlobalEnv)
  if (numReturned > 0) {
    returnValue(tnum.queryResultToDataframe(result, max))
  } else {
    returnValue()
  }
}

#' Title
#'
#' @param result
#'
#' @return
#' @export

tnum.queryResultToDataframe <- function(result, max) {
  decodenumber <- function(tn) {
    subj <- tn$subject[[1]]
    prop <- tn$property[[1]]
    taglist <- list()
    for (tag in tn$tags) {
      if (!startsWith(tag[[1]], '_')) {
        taglist <- append(taglist, tag$srd)
      }
    }

    if (tn$value$type == "numeric") {
      Nval <- tn$value$magnitude[[1]]
      tol <- tn$value$tolerance[[1]]
      if (tol != 0) {
        Nerr <- tol
      } else{
        Nerr <- NA
      }
      Cval <- NA
      posuns <- ""
      neguns <- ""
      for (unitpwr in tn$value$unitPowers) {
        if (unitpwr$p < 0) {
          if (unitpwr$p < -1) {
            neguns <- paste0(neguns, unitpwr$u, "^", -unitpwr$p, " ")
          } else {
            neguns <- paste0(neguns, unitpwr$u, " ")
          }
        } else {
          if (unitpwr$p > 1) {
            posuns <- paste0(posuns, " ", unitpwr$u, "^", unitpwr$p)
          } else {
            posuns <- paste0(" ", unitpwr$u)
          }
        }
      }
      uns <- posuns
      if (nchar(posuns) == 0 && nchar(neguns) > 0) {
        uns <- paste0("1/", neguns)
      } else if (nchar(posuns) > 0 && nchar(neguns) > 0) {
        uns <- paste0(posuns, "/", neguns)
      }

      if (nchar(uns) == 0 || uns == " unity") {
        uns <- NA
      }

    } else {
      Cval <- tn$value$value[[1]]
      Nval <- NA
      Nerr <- NA
      uns <- NA
    }
    gid <- as.character(tn[["_id"]])

    dat <- as.Date(tn$agent$dateCreated)

    rdf <-
      data.frame(
        subject = subj,
        property = prop,
        string.value = Cval,
        numeric.value = Nval,
        numeric.error = Nerr,
        units = uns,
        guid = gid,
        date = dat
      )
    rdf$tags <- list(taglist)

    returnValue(rdf)
  }

  #END local function

  retdf <- NULL

  if (is.null(result$data$truenumbers[[1]]$truenumbers)) {
    for (tn in result$data$truenumbers) {
      rowdf <- decodenumber(tn)

      if (is.null(retdf)) {
        retdf <- rowdf
      } else {
        retdf <- rbind(retdf, rowdf)
      }
    }

  } else {
    count <- max
    for (tnList in result$data$truenumbers) {
      tnGroup <- tnList$truenumbers
      for (tn in tnGroup) {
        rowdf <- decodenumber(tn)
        if (is.null(retdf)) {
          retdf <- rowdf
        } else {
          retdf <- rbind(retdf, rowdf)
        }

        count <- count - 1
        if (count == 0) {
          break
        }
      }
      if (count == 0) {
        break

      }
    }
  }

  returnValue(retdf)
}

tnum.dateAsToken <- function(){
  dt <- date()
  dt <- gsub(" ", "_",dt)
  dt <- gsub(":", "-",dt)
  return(dt)
}

# Truenumber creation functions


#' Title
#'
#' @param subject
#' @param property
#' @param value
#' @param error
#' @param units
#' @param tags
#'
#' @return
#' @export
#'
tnum.maketruenumber <- function(subject = "something",
                                property = "property",
                                Cvalue = NA,
                                Nvalue = NA,
                                error = NA,
                                units = "",
                                tags = list(),
                                noEmptyStrings=FALSE)
{
  notRealString <- function(strng){
    if(length(grep("[0-9,a-z]+",strng,ignore.case = TRUE)) == 1){
      return(FALSE)
    } else {
      return(TRUE)
    }
  }

  numval <- NA
  if (!is.na(Nvalue)) {
    unitSuffix <- ""
    if (!is.na(units) && nchar(units) > 0) {
      unitSuffix <- paste0(" ", units)
    }
    if (!is.na(error)) {
      numval <- paste0(Nvalue, " +/- ", error, unitSuffix)
    } else {
      numval <- paste0(Nvalue, unitSuffix)
    }
  } else {
    if (is.na(Cvalue) || (noEmptyStrings && notRealString(Cvalue))) {
      #if both values are NA return empty tnum
      return("{}")
    } else {
      numval <- Cvalue
    }
  }
  tagstr <- ""

  for (tag in tags) {
    if (nchar(tag) > 0 && !is.na(tag)) {
      if (nchar(tagstr) > 0) {
        tagstr <- paste0(tagstr, ",")
      }
      tagstr <- paste0(tagstr, '"', tag, '"')
    }
  }
  thenumber <-
    paste0(
      '{"subject":"',
      subject,
      '","property":"',
      property,
      '","value":"',
      numval,
      '","tags":[',
      tagstr,
      ']}'
    )
  returnValue(thenumber)
}

tnum.maketruenumbers <-
  function(subject,
           property,
           Cvalue,
           Nvalue,
           error,
           units,
           tags,
           noEmptyStrings=FALSE) {
    alljsonnums <-
      mapply(tnum.maketruenumber,
             subject,
             property,
             Cvalue,
             Nvalue,
             error,
             units,
             tags,
             noEmptyStrings)
    numnums <- length(alljsonnums)
    chunkcount <- 1
    chunksize <- 500
    chunks <- (numnums %/% chunksize) + 1
    remainder <- numnums %% chunksize
    for (i in 1:chunks) {
      startinx <- (i - 1) * chunksize + 1
      endinx <- startinx + chunksize -1
      if (endinx > numnums) {
        endinx <- startinx + remainder -1
      }

      jsonnums <- alljsonnums[startinx:endinx]
      jsonnums <- paste(jsonnums, collapse = ', ')
      jsonnums <- gsub(",\\{\\},", ",", jsonnums)
      jsonnums <- gsub("\\{\\},", "", jsonnums)
      jsonnums <- gsub(",\\{\\}", "", jsonnums)

      assign("tnum.var.postedJSON", jsonnums, envir = .GlobalEnv)
      args <-
        list(numberspace = tnum.var.nspace)
      result <- httr::POST(
        paste0("http://", tnum.var.ip, "/v1/numberspace/numbers"),
        query = args,
        httr::add_headers(Authorization = paste0("Bearer ", tnum.var.token)),
        body = paste0('{"truenumbers":[', jsonnums, ']}'),
        httr::accept("application/json"),
        httr::content_type("application/json")
      )
      message(paste0("posted ",startinx," to ",endinx, " of ",numnums))
    }

  }

#' Title
#'
#' @param gid
#' @param adds
#' @param removes
#'
#' @return
#' @export

tnum.add_remove_tags <- function(gid,
                                 adds = c(),
                                 removes = c()) {
  addstr <- paste0('"', paste(adds, collapse = '", "'), '"')
  remstr <- paste0('"', paste(removes, collapse = '", "'), '"')
  if (addstr == '""')
    addstr <- ""
  if (remstr == '""')
    remstr <- ""

  bodystr <-
    paste0('{"tags":[', addstr, '],"remove":[', remstr, ']}')
  message(bodystr)
  theurl <-
    paste0("http://", tnum.var.ip, "/v1/numberspace/numbers/", gid)
  message(theurl)
  result <- httr::PATCH(
    theurl,
    query = paste0("numberspace=", tnum.var.nspace),
    httr::add_headers(Authorization = paste0("Bearer ", tnum.var.token)),
    body = bodystr,
    httr::accept("application/json"),
    httr::content_type("application/json")
  )
  returnValue(httr::content(result))
}
