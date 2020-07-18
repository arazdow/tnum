

#' Vars local to this file
#'  @export

tnum.env <- new.env()

#' Connect and authenticate to Truenumbers server
#'
#' @param ip The endpoint address of the server. Default is a public cloud server
#'
#' @return  List of numberspaces available on the server. The first one on the list is set as current
#' @export

tnum.authorize <- function(ip = "54.166.186.11") {
  assign("tnum.var.ip", ip, envir = tnum.env)
  result <- httr::POST(
    paste0("http://", ip, "/v1/gateway/"),
    body = paste0('{"email":"admin@truenumbers.com"}'),
    httr::accept("application/json"),
    httr::content_type("application/json")
  )

  token <- httr::content(result)$data$token

  ## get list of numberspaces
  result <- httr::GET(
    paste0("http://", ip, "/v1/numberspace/"),
    httr::add_headers(Authorization = paste0("Bearer ", token))
  )
  nspaces <- list()

  for (x in httr::content(result)$data) {
    nspaces <- append(nspaces, x[[2]])
  }
  assign("tnum.var.nspace", nspaces[[1]], envir = tnum.env)
  assign("tnum.var.nspaces", nspaces, envir = tnum.env)
  assign("tnum.var.token", token, envir = tnum.env)
  returnValue(nspaces)

}

#' Set a particular numberspace as current
#'
#' @param name Name of numberspace
#'
#' @return  -none-
#' @export
#'

tnum.setSpace <- function(name = "testspace") {
  if (name %in% tnum.env$tnum.var.nspaces) {
    assign("tnum.var.nspace", name, envir = tnum.env)
  } else {
    stop(paste0('server has no numberspace "', name, '"'))
  }
}

#' Get current numberspace name
#'
#' @return -none-
#' @export
#'

tnum.getSpace <- function() {
  returnValue(tnum.env$tnum.var.nspace)
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
      numberspace = tnum.env$tnum.var.nspace,
      limit = max,
      offset = start,
      tnql = query
    )

  result <-
    httr::content(httr::GET(
      paste0("http://", tnum.env$tnum.var.ip, "/v1/numberspace/numbers"),
      query = args,
      httr::add_headers(Authorization = paste0("Bearer ", tnum.env$tnum.var.token))
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

  assign("tnum.var.result", result, envir = tnum.env)
  if (numReturned > 0) {
    returnValue(tnum.queryResultToDataframe(result, max))
  } else {
    returnValue()
  }
}


#' Delete tnums specified by a query
#'
#' @param query  string in tnum query language
#'
#' @export
#'

tnum.deleteByQuery <- function(query = "") {
  args <-
    list(numberspace = tnum.env$tnum.var.nspace,
         tnql = query)

  result <-
    httr::content(httr::DELETE(
      paste0("http://", tnum.env$tnum.var.ip, "/v1/numberspace/numbers"),
      query = args,
      httr::add_headers(Authorization = paste0("Bearer ", tnum.env$tnum.var.token))
    ))
  numReturned <- length(result$data$removed)

  message(result)
}

#' Tag tnums specified by a query
#'
#' @param query  string in tnum query language
#' @param adds   list of tags to add
#' @param removes   list of tags to remove
#'
#' @export
#'

tnum.tagByQuery <- function(query = "",
                            adds = list(),
                            removes = list()) {
  args <-
    list(numberspace = tnum.env$tnum.var.nspace,
         tnql = query)
  addstr <- paste0('"', paste(adds, collapse = '", "'), '"')
  remstr <- paste0('"', paste(removes, collapse = '", "'), '"')
  if (addstr == '""')
    addstr <- ""
  if (remstr == '""')
    remstr <- ""

  bodystr <-
    paste0('{"tags":[', addstr, '],"remove":[', remstr, ']}')

    theurl <-
      paste0("http://",
             tnum.env$tnum.var.ip,
             "/v1/numberspace/numbers/"
             )

    result <- httr::PATCH(
      theurl,
      query = args,
      httr::add_headers(Authorization = paste0("Bearer ", tnum.env$tnum.var.token)),
      body = bodystr,
      httr::accept("application/json"),
      httr::content_type("application/json")
    )
    return(result)
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
            neguns <- paste0(neguns, unitpwr$u, "^",-unitpwr$p, " ")
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
        units = uns
      )
    rdf$tags <- paste0(taglist, collapse=", ")  # was list(taglist)
    rdf$date <- dat
    rdf$guid <- gid

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

tnum.dateAsToken <- function() {
  dt <- date()
  dt <- gsub(" ", "_", dt)
  dt <- gsub(":", "-", dt)
  return(dt)
}

# Truenumber creation functions


#' Create a JSON truenumber from parts
#'
#' @param subject
#' @param property
#' @param string.value
#' @param numeric.value
#' @param numeric.error
#' @param units
#' @param tags
#' @param noEmptyStrings
#'
tnum.makeTruenumber <- function(subject = "something",
                                property = "property",
                                string.value = NA,
                                numeric.value = NA,
                                numeric.error = NA,
                                units = "",
                                tags = list(),
                                noEmptyStrings = FALSE)
{
  notRealString <- function(strng) {
    if (length(grep("[0-9,a-z]+", strng, ignore.case = TRUE)) == 1) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }

  numval <- NA
  if (!is.na(numeric.value)) {
    unitSuffix <- ""
    if (!is.na(units) && nchar(units) > 0) {
      unitSuffix <- paste0(" ", units)
    }
    if (!is.na(numeric.error)) {
      numval <- paste0(numeric.value, " +/- ", numeric.error, unitSuffix)
    } else {
      numval <- paste0(numeric.value, unitSuffix)
    }
  } else {
    if (is.na(string.value) || (noEmptyStrings && notRealString(string.value))) {
      #if both values are NA return empty tnum
      return("{}")
    } else {
      numval <- string.value
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

#' Create many truenumbers from lists of parts
#'
#' @param subject
#' @param property
#' @param string.value
#' @param numeric.value
#' @param numeric.error
#' @param units
#' @param tags
#' @param noEmptyStrings if true doesn't post TNs with empty values
#'
#' @return
#' @export
#'
tnum.postTruenumbers <-
  function(subject,
           property,
           string.value=NA,
           numeric.value=NA,
           numeric.error=NA,
           units=NA,
           tags,
           noEmptyStrings = FALSE) {

    alljsonnums <-
      mapply(
        tnum.makeTruenumber,
        subject,
        property,
        string.value,
        numeric.value,
        numeric.error,
        units,
        tags,
        noEmptyStrings
      )
    numnums <- length(alljsonnums)
    chunkcount <- 1
    chunksize <- 500
    chunks <- (numnums %/% chunksize) + 1
    remainder <- numnums %% chunksize
    for (i in 1:chunks) {
      startinx <- (i - 1) * chunksize + 1
      endinx <- startinx + chunksize - 1
      if (endinx > numnums) {
        endinx <- startinx + remainder - 1
      }

      jsonnums <- alljsonnums[startinx:endinx]
      jsonnums <- paste(jsonnums, collapse = ', ')
      jsonnums <- gsub(",\\{\\},", ",", jsonnums)
      jsonnums <- gsub("\\{\\},", "", jsonnums)
      jsonnums <- gsub(",\\{\\}", "", jsonnums)

      assign("tnum.var.postedJSON", jsonnums, envir = tnum.env)
      args <-
        list(numberspace = tnum.env$tnum.var.nspace)
      result <- httr::POST(
        paste0(
          "http://",
          tnum.env$tnum.var.ip,
          "/v1/numberspace/numbers"
        ),
        query = args,
        httr::add_headers(Authorization = paste0("Bearer ", tnum.env$tnum.var.token)),
        body = paste0('{"truenumbers":[', jsonnums, ']}'),
        httr::accept("application/json"),
        httr::content_type("application/json")
      )
      message(paste0("posted ", startinx, " to ", endinx, " of ", numnums))
    }

  }

#' Post a single truenumber from parts
#'
#' @param subject
#' @param property
#' @param string.value
#' @param numeric.value
#' @param numeric.error
#' @param units
#' @param tags
#'
#' @return
#' @export
#'
tnum.postTruenumber <-
  function(subject = "something",
           property = "property",
           string.value = NA,
           numeric.value = NA,
           numeric.error = NA,
           units = "",
           tags = list(),
           noEmptyStrings = FALSE) {
    jsonnum <-
        tnum.makeTruenumber(subject,
                            property,
                            string.value,
                            numeric.value,
                            numeric.error,
                            units,
                            tags,
                            noEmptyStrings)
    if(nchar(jsonnum)>5){
      assign("tnum.var.postedJSON", jsonnum, envir = tnum.env)
      args <-
        list(numberspace = tnum.env$tnum.var.nspace)
      result <- httr::POST(
        paste0(
          "http://",
          tnum.env$tnum.var.ip,
          "/v1/numberspace/numbers"
        ),
        query = args,
        httr::add_headers(Authorization = paste0("Bearer ", tnum.env$tnum.var.token)),
        body = paste0('{"truenumbers":[', jsonnum, ']}'),
        httr::accept("application/json"),
        httr::content_type("application/json")
      )
    }
  }

#' Add and remove tags to tnums specified by a list of their GUID's
#'
#' @param gids  guids of tnums to tag
#' @param adds   tags to add
#' @param removes tags to remove
#'
#' @return result of API call
#' @export

tnum.tagByGuids <- function(gids = c(),
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
  for (gid in gids) {
    theurl <-
      paste0("http://",
             tnum.env$tnum.var.ip,
             "/v1/numberspace/numbers/",
             gid)

    result <- httr::PATCH(
      theurl,
      query = paste0("numberspace=", tnum.env$tnum.var.nspace),
      httr::add_headers(Authorization = paste0("Bearer ", tnum.env$tnum.var.token)),
      body = bodystr,
      httr::accept("application/json"),
      httr::content_type("application/json")
    )
  }

}
