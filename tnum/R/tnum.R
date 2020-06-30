#' Truenumber utility functions for R
#' @author True Engineering Technology, LLC Boston, MA USA
#' @references \url{http://www.truenum.com}

require(httr)
require(jsonlite)

tnum.var.nspace <- ""
tnum.var.nspaces <- ""
tnum.var.token = ""
tnum.var.ip = ""
tnum.var.result = ""

#' Connect and authenticate to Truenumbers server
#'
#' @param ip The endpoint address of the server. Default is a public cloud server
#'
#' @return  List of numberspaces available on the server. The first one on the list is set as current
#' @export  Writes the address given/used in global variable \code{tnum.var.ip}
#'          Writes the numberspace list in \code{tnum.var.nspaces}
#'          Writes the current numberspace name in \code{tnum.var.nspace}
#'
#' @examples
tnum.authorize <- function(ip = "54.166.186.11") {
  assign("tnum.var.ip", ip, envir = .GlobalEnv)
  result <- POST(
    paste0("http://", ip, "/v1/gateway/"),
    body = paste0('{"email":"admin@truenumbers.com"}'),
    accept("application/json"),
    content_type("application/json")
  )

  token <- content(result)$data$token
  assign("tnum.var.token", token, envir = .GlobalEnv)
  ## get list of numberspaces
  result <- GET(paste0("http://", ip, "/v1/numberspace/"),
                add_headers(Authorization = paste0("Bearer ", token)))
  nspaces <- list()

  for (x in content(result)$data) {
    nspaces <- append(nspaces, x[[2]])
  }
  assign("tnum.var.nspace", nspaces[[1]], envir = .GlobalEnv)
  assign("tnum.var.nspaces", nspaces, envir = .GlobalEnv)
  returnValue(nspaces)

}

#' Title
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
tnum.setspace <- function(name = "testspace") {
  if (name %in% tnum.nspaces) {
    assign("tnum.var.nspace", name, envir = .GlobalEnv)
  } else {
    stop(paste0('server has no numberspace "', name, '"'))
  }
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
tnum.getspace <- function() {
  returnValue(tnum.var.nspace)
}


#' Title
#'
#' @param query
#' @param max
#' @param start
#'
#' @return
#' @export
#'
#' @examples
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
    content(GET(
      paste0("http://", tnum.var.ip, "/v1/numberspace/numbers"),
      query = args,
      add_headers(Authorization = paste0("Bearer ", tnum.var.token))
    ))
  numReturned <- length(result$data$truenumbers)
  if (numReturned > max) {
    numReturned <- max
  }
  message(
    paste0(
      "Returned ",
      start + 1,
      " thru ",
      start + numReturned,
      " of ",
      result$data$meta$records,
      " results"
    )
  )

  assign("tnum.var.result", result, envir = .GlobalEnv)
  returnValue(tnum.simplify_result(result, max))
}

#' Title
#'
#' @param result
#'
#' @return
#' @export
#'
#' @examples
tnum.simplify_result <- function(result, max) {
  decodenumber <- function(tn) {
    ##data.frame(subjects, properties, Cvalues, Nvalues, Nerror, units,guids,dates)

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
      for (unitpwr in tn$unitPowers) {
        if (unitpwr$p < 0) {
          if (unitpwr$p < -1) {
            neguns <- append(neguns, paste0(unitpwr$u, "^",-unitpwr$p), " ")
          } else {
            neguns <- append(neguns, paste0(unitpwr$u, " "))
          }
        } else {
          if (unitpwr$p > 1) {
            posuns <- append(posuns, paste0(" ", unitpwr$u, "^", unitpwr$p))
          } else {
            posuns <- append(posuns, paste0(" ", unitpwr$u))
          }
        }
      }
      uns <- posuns
      if (nchar(posuns) == 0 && nchar(neguns) > 0) {
        uns <- paste0("1/", neguns)
      } else if (nchar(posuns) > 0 && nchar(neguns) > 0) {
        uns <- paste0(posuns, "/", neguns)
      }

      if (nchar(uns) == 0) {
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
      rowdf <- decodenumber(tnGroup)
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

  returnValue(retdf)
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
#' @examples
tnum.maketruenumber <- function(subject = "something",
                                property = "property",
                                value = "0",
                                error = "0",
                                units = "",
                                tags = c())
{
  if (mode(value) == "numeric") {
    if (error != 0) {
      numval <- paste0(value, " +/- ", error, " ", units)
    } else {
      numval <- paste0(value, " ", units)
    }
  } else {
    numval <- value
  }
  tagstr <- ""
  for (tag in tags) {
    if (nchar(tagstr) > 0) {
      tagstr <- paste0(tagstr, ",")
    }
    tagstr <- paste0(tagstr, '"', tag, '"')
  }
  thenumber <-
    paste0(
      '{"truenumbers":[{"subject":"',
      subject,
      '","property":"',
      property,
      '","value":"',
      numval,
      '","tags":[',
      tagstr,
      ']}]}'
    )
  message(thenumber)
  args <-
    list(numberspace = tnum.var.nspace)
  result <- POST(
    paste0("http://", tnum.var.ip, "/v1/numberspace/numbers"),
    query = args,
    add_headers(Authorization = paste0("Bearer ", tnum.var.token)),
    body = thenumber,
    accept("application/json"),
    content_type("application/json")
  )

  returnValue(content(result))
}
