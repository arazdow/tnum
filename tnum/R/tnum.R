


# Vars local to this file

#' @export
tnum.env <- new.env()

#' Connect and authenticate to Truenumbers server
#'
#' @param ip The endpoint address of the server. Default is a public cloud server
#' @param key  token to authorize API access
#'
#' @return  List of numberspaces available on the server. The first one on the list is set as current
#' @export

tnum.authorize <- function(ip = "54.166.186.11", key) {
  assign("tnum.var.ip", ip, envir = tnum.env)
  assign("tnum.var.token", key, envir = tnum.env)

  token <- key

  ## get list of numberspaces
  result <- httr::GET(
    paste0("http://", ip, "/v1/numberspace/"),
    httr::add_headers(Authorization = paste0("Bearer ", token))
  )
  nspaces <- list()
  payload <- httr::content(result)
  if (!is.null(payload$code)) {
    message(payload$code)
  } else {
    for (x in httr::content(result)$data) {
      nspaces <- append(nspaces, x[[2]])
    }
    assign("tnum.var.nspace", nspaces[[1]], envir = tnum.env)
    assign("tnum.var.nspaces", nspaces, envir = tnum.env)
    assign("tnum.var.token", token, envir = tnum.env)
    tnum.setSpace("shared-testspace")
    message(paste0("Available spaces: ", paste0(nspaces, collapse = ", ")))
    message(paste0("Numberspace set to: ", tnum.getSpace()))
  }
}

#' Create new numberspace
#'
#' @param name name of space to create
#'
#' @export

tnum.createSpace <- function(name) {
  result <- httr::POST(
    paste0("http://", tnum.env$tnum.var.ip, "/v1/numberspace/"),
    httr::add_headers(Authorization = paste0("Bearer ", tnum.env$tnum.var.token)),
    body = paste0('{"numberspace":"', name, '"}'),
    httr::accept("application/json"),
    httr::content_type("application/json")
  )
  return(httr::content(result))
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
#' @param SI     if TRUE, returns the SI value
#' @param max    maximum nuber of truenumbers to return
#' @param start  begin return with this sequence number
#'
#' @return  a list of tnum class vaues
#' @export
#'

tnum.query <- function(query = "* has *",
                       SI = FALSE,
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
  return(tnum.queryResultToObjects(result, SI, max))
}

#' Convert tnum query result to an object list
#'
#' @param result from a tnum API query
#' @param SI if TRUE, the object will reflect numerical SI values
#' @param max maximum rows to return
#' @return  list of tnum objects


tnum.queryResultToObjects <-
  function(result, SI = FALSE, max = 100) {
    decodenumber <- function(tn) {
      subj <- tn$subject[[1]]
      prop <- tn$property[[1]]
      taglist <- list()
      for (tag in tn$tags) {
        if (!startsWith(tag[[1]], '_')) {
          taglist <- append(taglist, tag$srd)
        }
      }
      gid <- as.character(tn[["_id"]])
      dat <- as.Date(tn$agent$dateCreated)

      if (SI) {
        valstruc <- tn$si - value
      } else {
        valstruc <- tn$value
      }

      if (valstruc$type == "numeric" ||
          !is.null(valstruc$magnitude)) {
        Nval <- valstruc$magnitude[[1]]
        tol <- valstruc$tolerance[[1]]

        if (tol == 0) {
          tol <- NA
        }
        posuns <- ""
        neguns <- ""
        for (unitpwr in valstruc$unitPowers) {
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
        Nval <- tn$value$value[[1]]
        tol <- NA
        uns <- NA
      }

      return(tnum.makeObject(subj, prop, Nval, tol, uns, taglist, dat, gid)) # return object
    }

    #END local function

    retList <- list()

    if (is.null(result$data$truenumbers[[1]]$truenumbers)) {
      for (tn in result$data$truenumbers) {
        retList[[length(retList) + 1]] <- decodenumber(tn)
      }

    } else {
      count <- max
      for (tnList in result$data$truenumbers) {
        tnGroup <- tnList$truenumbers
        for (tn in tnGroup) {
          retList[[length(retList) + 1]] <- decodenumber(tn)
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

    return(retList)
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
           "/v1/numberspace/numbers/")

  result <- httr::PATCH(
    theurl,
    query = args,
    httr::add_headers(Authorization = paste0("Bearer ", tnum.env$tnum.var.token)),
    body = bodystr,
    httr::accept("application/json"),
    httr::content_type("application/json")
  )
  message(httr::content(result))
}

# utility to get attr from list
tnum.getAttrFromListOld <- function(obs, attname, num, rval) {
  al <- attr(obs[[1]], attname)
  if (is.null(al))
    return(rep(rval, num))
  else
    return(c(unlist(lapply(obs, attr, attname))))
}
tnum.getAttrFromList <- function(obs, attname, num, rval) {
  ll <- list()
  for(i in 1:length(obs)){
    atv <- attr(obs[[i]],attname)
    if(is.null(atv)){
      ll[[i]] <- rval
    } else if(length(atv)>1){
      ll[[i]] <- paste0('"',paste0(atv, collapse = ","),'"')
    } else {
      ll[[i]] <- atv
    }
  }
    return(unlist(ll))
}



#' make data frame from list of tnum objects
#'
#' @param objs tnum list
#'
#' @return data frame
#' @export
#'
tnum.objectsToDf <- function(objs) {
  len <- length(objs)
  subj <- tnum.getAttrFromList(objs, "subject", len, NA)
  prop <- tnum.getAttrFromList(objs, "property", len, NA)
  chrs <- vector(mode = "character")
  nums <- vector(mode = "numeric")
  for (i in 1:len) {
    if (mode(objs[[i]]) == "numeric") {
      nums[[i]] <- objs[[i]]
      chrs[[i]] <- NA
    } else {
      chrs[[i]] <- objs[[i]]
      nums[[i]] <- NA
    }
  }
  errs <-
    as.vector(mode = "numeric", tnum.getAttrFromList(objs, "error", len, NA))
  uns <- tnum.getAttrFromList(objs, "unit", len, NA)
  tgs <- tnum.getAttrFromList(objs, "tags", len, NA)
  dat <- tnum.getAttrFromList(objs, "date", len, NA)
  gid <- tnum.getAttrFromList(objs, "guid", len, NA)
  df <-
    data.frame(
      "subject" = subj,
      "property" = prop,
      "string.value" = chrs,
      "numeric.value" = nums,
      "error" = errs,
      "unit" = uns,
      "tags" = tgs,
      "date" = dat,
      "id" = gid
    )
  if(mode(df$date[[1]]) == "numeric")
         df$date <- as.Date(df$date, origin = "1970-01-01")
  return(df)
}


#' make a tnum object from numeric values in a tnum data frame
#'
#' @param df data frame as returned by tnum.query
#' @param numerics return df as numeric vector of only numeric values in the df. Defauls is FALSE
#'
#' @return an R object of class tnum
#' @export
#'

tnum.makeObject <-
  function(subject = "something",
           property = "some-property",
           value,
           error = NA,
           unit = NA,
           tags = NA,
           dat = NA,
           gid = NA) {
    if (!is.na(error))
      attr(value, "error") <- error
    if (!is.na(unit))
      attr(value, "unit") <- unit
    if (mode(tags) == "character")
      attr(value, "tags") <- tags
    if(is.list(tags)){
      attr(value, "tags") <- unlist(tags)
    }

    attr(value, 'class') <- "tnum"
    attr(value, 'subject') <- subject
    attr(value, 'property') <- property

    if (!is.na(gid))
      attr(value, "guid") <- gid
    if (!is.na(dat))
      attr(value, "date") <- dat
    else
      attr(value, "date") <- date()

    return(value)##return(sticky::sticky(value))
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
#' @param value
#' @param numeric.error
#' @param unit
#' @param tags
#' @param noEmptyStrings
#'
tnum.makeTnumJson <- function(subject = "something",
                              property = "property",
                              value = NA,
                              numeric.error = NA,
                              unit = "",
                              tags = list(),
                              noEmptyStrings = FALSE)
{
  notRealString <- function(strng) {
    if (length(grep("[0-9,a-z%]+", strng, ignore.case = TRUE)) == 1) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }

  numval <- NA
  if (mode(value) == "numeric") {
    unitSuffix <- ""
    if (!(is.null(unit) || is.na(unit) || notRealString(unit))) {
      unitSuffix <- paste0(" ", unit)
    }
    if (!(is.null(numeric.error) || is.na(numeric.error))) {
      numval <- paste0(value, " +/- ", numeric.error, unitSuffix)
    } else {
      numval <- paste0(value, unitSuffix)
    }
  } else {
    if ((noEmptyStrings && notRealString(value))) {
      #if both values are NA return empty tnum
      return("{}")
    } else {
      numval <- value
      if (!stringr::str_starts(numval, '"') &&
          !stringr::str_detect(numval, "^[0-9a-zA-Z/:\\-_]+$")) {
        numval <-
          paste0("\\\"", numval, "\\\"") ## if not SRD, and not quoted text, then add quotes
      }
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
#' @param value
#' @param numeric.error
#' @param unit
#' @param tags
#' @param noEmptyStrings if true doesn't post TNs with empty values
#'
#' @return

#'
tnum.postFromLists <-
  function(subject,
           property,
           value = NA,
           numeric.error = NA,
           unit = NA,
           tags = NA,
           noEmptyStrings = FALSE) {
    len <- length(subject)
    if (len == 1) {
      res <- tnum.postTnumFields(subject,
                                 property,
                                 value,
                                 numeric.error,
                                 unit,
                                 tags,
                                 noEmptyStrings)
      return(res)
    }

    if (is.logical(numeric.error))
      numeric.error <- rep(NA, len)
    if (is.logical(unit))
      unit <- rep(NA, len)
    if (is.logical(tags))
      tags <- rep(NA, len)

    alljsonnums <-
      mapply(
        tnum.makeTnumJson,
        subject,
        property,
        value,
        numeric.error,
        unit,
        tags,
        noEmptyStrings
      )
    numnums <- length(alljsonnums)
    chunkcount <- 0
    chunksize <- 25000
    jsonnums <- ""
    for (i in 1:numnums) {
      curnum <- alljsonnums[[i]]
      chunkcount <- chunkcount + nchar(curnum)
      jsonnums <- paste0(jsonnums, curnum, ",")
      if (chunkcount > chunksize || i == numnums) {
        jsonnums <- substr(jsonnums, 1, nchar(jsonnums) - 1)
        jsonnums <- gsub(",\\{\\},", ",", jsonnums)
        jsonnums <- gsub("\\{\\},", "", jsonnums)
        jsonnums <- gsub(",\\{\\}", "", jsonnums)

        assign("tnum.var.postedJSON", jsonnums, envir = tnum.env)
        args <-
          list(numberspace = tnum.env$tnum.var.nspace)
        payload <- paste0('{"truenumbers":[', jsonnums, ']}')
        result <- httr::POST(
          paste0(
            "http://",
            tnum.env$tnum.var.ip,
            "/v1/numberspace/numbers"
          ),
          encode = "json",
          query = args,
          httr::add_headers(Authorization = paste0("Bearer ", tnum.env$tnum.var.token)),
          body = payload,
          httr::accept("application/json"),
          httr::content_type("application/json")
        )
        message(paste0("Posting ", chunkcount, " characters"))
        chunkcount <- 0
        jsonnums <- ""
      }

    }
    message(paste0("posted ", numnums, " tnums"))
  }

# Post a single truenumber from parts
#

tnum.postTnumFields <-
  function(subject = "something",
           property = "property",
           value = NA,
           numeric.error = NA,
           unit = "",
           tags = list(),
           noEmptyStrings = FALSE) {
    jsonnum <-
      tnum.makeTnumJson(subject,
                        property,
                        value,
                        numeric.error,
                        unit,
                        tags,
                        noEmptyStrings)
    if (nchar(jsonnum) > 5) {
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

#' post a list or vector of tnum objects
#'
#' @param objects
#'
#' @return
#' @export

tnum.postObjects <-
  function(objects) {
    subject <- lapply(objects, attr, "subject")
    property <- lapply(objects, attr, "property")
    error <- lapply(objects, attr, "error")
    unit <- lapply(objects, attr, "unit")
    tags <- lapply(objects, attr, "tags")
    tnum.postFromLists(subject, property, objects, error, unit, tags)
  }

#' Add a column of single tags element-wise to list of tnums by GUID
#'
#' @param gids  guids of tnums to tag
#' @param adds   tags to add
#'
#' @return result of API call
#' @export

tnum.tagByGuids <- function(gids = c(),
                            adds = c()) {
  for (i in 1:length(gids)) {
    theurl <-
      paste0("http://",
             tnum.env$tnum.var.ip,
             "/v1/numberspace/numbers/",
             gids[[i]])
    bodystr <- paste0('{"tags":["', adds[[i]], '"],"remove":[]}')
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


#' Create a tnum vector value string "vector(23,-34.02...)" from an R vector or list
#'
#' @param numvec a vector or list
#'
#' @return a string "vector(23,-34.02...)"
#' @export

tnum.makeNumericVectorString <- function(numvec) {
  if (mode(numvec) == "numeric") {
    nvec <- numvec
  } else {
    nvec = rep(0.0, length(numvec))
  }
  vvals <- paste0("vector(", paste0(nvec, collapse = ","), ")")
  return(vvals[[1]])
}

#' Return numeric vector from string "vector(1.23,34.5....)"
#'
#' @param nvs
#'
#' @return vector of numbers
#' @export

tnum.decodeNumericVectorString <- function(nvs) {
  if (stringr::str_starts(nvs, '"vector\\(')) {
    csl <- substr(nvs, 9, nchar(nvs) - 1)
    Nvec <- readr::parse_number(unlist(strsplit(csl, ",")))
  } else {
    Nvec <- vector(0.0)
  }
  return(Nvec)
}
