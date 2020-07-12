#' Truenumber utility functions for working with Twitter/Tweets
#' @author True Engineering Technology, LLC Boston, MA USA
#' @references \url{http://www.truenum.com}
#'
library(rtweet)
library(tidyverse)

#' Authenticate twitteR package with Twitter account
#'

tnum.rtweet.authorize <- function() {
  require(httr)
  require(jsonlite)
  require(data.tree)
  require(rtweet)
  require(tidyverse)
  if (nchar(get_token()) < 5) {
    twitter_token <- rtweet::create_token(
      consumer_key = "EJJSOPMbniEdgyxhD9Q6rZDp1",
      consumer_secret = "tcMRH9XTmXd6nq9hAFsYtsHW5cwsymN32duLCNmQIoqb3amwja",
      access_token = "1274782526926700546-PJpruW5N5CTkzycTj9gsZSePBdHv97",
      access_secret = "pfUowEnIFZRWOzJ8DDETiCNtIkT0PheEBuJnOE4O6bZvl"
    )
  }

  return(get_token())
}

#' @title Post new tnums from rtweet search result
#'
#' tnums for each tweet:
#'  1. tnum for full text
#'  2. tnum for create date
#'  3. tnum for favorite count (not present if 0)
#'  4. tnum for retweet count (not present if 0)
#'  5  tnum for replied-to tweet subject (not present if not a reply)
#'  6. tnum for user location
#'
#' tags for each tweet:
#'  1. tagged if retweet
#'  2. tagged if truncated
#'  3. tagged with user device
#'
#' @param tweetTibble table of tweets as returned from rtweet::search_tweets()
#'
#' @return return code of tnum.maketruenumbers call
#' @export N/A
#'

tnum.rtweet.post_tweets_as_tnums <- function(tweetTibble) {
  # Functions needed for apply() processing of tweet vectors ##########

  # Pull out a platform name from the HTML source field of the tweet
  getTweetPlatform <- function(atag) {
    platName <- "unknown"
    if (grepl("ipad", atag, ignore.case = TRUE)) {
      platName <- "iPad"
    } else if (grepl("android", atag, ignore.case = TRUE)) {
      platName <- "Android"
    } else if (grepl("web", atag, ignore.case = TRUE)) {
      platName <- "Web"
    } else if (grepl("iphone", atag, ignore.case = TRUE)) {
      platName <- "iPhone"
    } else if (grepl("linkedin", atag, ignore.case = TRUE)) {
      platName <- "LinkedIn"
    } else if (grepl("tweetdeck", atag, ignore.case = TRUE)) {
      platName <- "TweetDeck"
    }
    returnValue(paste0("tweet/platform:", platName))
  }

  #  clean up tweet text so it works as a JSON string
  escapequotes <- function(strng) {
    returnValue(gsub("\n", "", gsub('"', '\\\\"', strng)))
  }

  # for boolean fields, if true, a tag is generated; no tag if false.
  tagboolean <- function(boolVal, theTag) {
    if (boolVal) {
      returnValue(theTag)
    } else {
      returnValue(NA)
    }
  }

  # for numeric fields, NA instead of a value if = zero
  NAifZero <- function(num) {
    if (num == 0) {
      returnValue(NA)
    } else {
      returnValue(num)
    }
  }

  # make reply tweet subject to use as value. If not a reply - return empty TN
  replyTweetIfReply <- function(sname, sid) {
    if (is.na(sname)) {
      return(NA)
    } else {
      returnValue(paste0("twitter/user:", sname, "/tweet:", sid))
    }
  }

  ## end of apply() functions ######################################


  tf <- tweetTibble
  numTweets <- nrow(tf)  # number of tweets

  # get user profiles for enriching tweet data
  users <- unique(tf$screenName)
  profiles <- twitteR::lookupUsers(users, TRUE)

  # make subject vector for all rows (tweets)
  tweet.subj.vector <-
    paste0("twitter/user:", tf$screenName, "/tweet:", tf$id)

  # make property, value and tag vectors for each truenum, and post for all rows

  # tags, common to all tnums of a tweet
  tweet.tags.platforms <-
    lapply(tf$statusSource, getTweetPlatform)

  tweet.tags.truncated <-
    lapply(tf$truncated, tagboolean, theTag = "tweet:truncated")

  tagList <- list()
  for (i in 1:numTweets) {
    tagList[[i]] <-
      list(tweet.tags.platforms[[i]], tweet.tags.truncated[[i]])
  }

  # ... property and value for tweet's text:
  tweet.prop.vector <- rep("text", numTweets)
  tweet.cvalue.vector <- lapply(tf$text, escapequotes)

  retVal <-   # write the text tnums to the server
    tnum.maketruenumbers(tweet.subj.vector,
                         tweet.prop.vector,
                         tweet.cvalue.vector,
                         NA,
                         NA,
                         NA,
                         tagList)

  # ... property and value for tweet's likes:
  tweet.prop.vector <- rep("likes", numTweets)
  tweet.nvalue.vector <- tf$favoriteCount

  retVal <-   # write the likes tnums to the server
    tnum.maketruenumbers(tweet.subj.vector,
                         tweet.prop.vector,
                         NA,
                         tweet.nvalue.vector,
                         NA,
                         "",
                         tagList)

  # ... property and value for tweet's creation date:
  tweet.prop.vector <- rep("date:creation", numTweets)
  tweet.cvalue.vector <- tf$created

  retVal <-   # write the creation date tnums to the server
    tnum.maketruenumbers(tweet.subj.vector,
                         tweet.prop.vector,
                         tweet.cvalue.vector,
                         NA,
                         NA,
                         NA,
                         tagList)

  # ... property and value for tweet's replied-to tweet:
  tweet.prop.vector <- rep("tweet:replied-to", numTweets)
  tweet.cvalue.vector <-
    mapply(replyTweetIfReply, tf$replyToSN, tf$replyToSID)
  retVal <-   # write the tweet replied to tnum to the server
    tnum.maketruenumbers(tweet.subj.vector,
                         tweet.prop.vector,
                         tweet.cvalue.vector,
                         NA,
                         NA,
                         NA,
                         tagList)
}
