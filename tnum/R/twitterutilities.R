

#' Authenticate twitteR package with Twitter account
#'
#' @return
#' @export
#'
tnum.twitteR.authorize <- function() {
  twitter_token <- twitteR::setup_twitter_oauth(
    consumer_key = "EJJSOPMbniEdgyxhD9Q6rZDp1",
    consumer_secret = "tcMRH9XTmXd6nq9hAFsYtsHW5cwsymN32duLCNmQIoqb3amwja",
    access_token = "1274782526926700546-PJpruW5N5CTkzycTj9gsZSePBdHv97",
    access_secret = "pfUowEnIFZRWOzJ8DDETiCNtIkT0PheEBuJnOE4O6bZvl"
  )
}

#' @title Post new tnums from twitteR query result
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
#' @param tweetList List of tweets as returned from twitteR::Search()
#'
#' @return return code of tnum.maketruenumbers call
#' @export
#'

tnum.twitteR.post_tweets_as_tnums <- function(tweetList, customTags = list()) {
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
    escaped <- gsub("\n", "", gsub('"', "'", strng))
   escaped <- gsub("\\\\", "\\\\\\\\", escaped)
    returnValue(dQuote(escaped))
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
  NAifZero <- function(num){
    if(num == 0){
      returnValue(NA)
    } else {
      returnValue(num)
    }
  }

  # make reply tweet subject to use as value. If not a reply - return empty TN
  replyTweetIfReply <- function(sname, sid){
    if(is.na(sname)){
      return(NA)
    } else {
      returnValue(paste0("twitter/user:", sname, "/tweet:", sid))
    }
  }

  ## end of apply() functions ######################################


  tf <- twitteR::twListToDF(tweetList)  #create data.frame from list
  numTweets <- nrow(tf)  # number of tweets

  # get user profiles for enriching tweet data
  users <- unique(tf$screenName)
  profiles <- twitteR::lookupUsers(users, TRUE)
  profilesdf <- twitteR::twListToDF(profiles)

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
      tlist <- list(tweet.tags.platforms[[i]], tweet.tags.truncated[[i]])
      if(length(customTags)>0){
        tlist <- append(tlist,customTags)
      }
      tagList[[i]] <- tlist
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
  tweet.cvalue.vector <- mapply(replyTweetIfReply,tf$replyToSN,tf$replyToSID)
  retVal <-   # write the tweet replied to tnum to the server
    tnum.maketruenumbers(tweet.subj.vector,
                         tweet.prop.vector,
                         tweet.cvalue.vector,
                         NA,
                         NA,
                         NA,
                         tagList)

  ##  add user profile information
  # ...
  numUsers <- length(users)
  user.tags.verified <-
    lapply(profilesdf$verified, tagboolean, theTag = "twitter/user:verified")
  tagList <- list()
  paste0("twitter/user:", profilesdf$screenName, "/tweet:", profilesdf$id)
  for (i in 1:numUsers) {
     tlist <- list(user.tags.verified[[i]])
     if(length(customTags)>0){
       tlist <- append(tlist,customTags)
     }
     tagList[[i]] <- tlist
  }

  user.subj.vector <- paste0("twitter/user:", profilesdf$screenName, "/profile:", profilesdf$id)
  user.prop.vector <- rep("date:creation", numUsers)
  user.cvalue.vector <- profilesdf$created
  retVal <-   # write the user's creation date
    tnum.maketruenumbers(user.subj.vector,
                         user.prop.vector,
                         user.cvalue.vector,
                         NA,
                         NA,
                         NA,
                         tagList)


  user.prop.vector <- rep("description", numUsers)
  user.cvalue.vector <- lapply(profilesdf$description, escapequotes)
  retVal <-   # write the user's description
    tnum.maketruenumbers(user.subj.vector,
                         user.prop.vector,
                         user.cvalue.vector,
                         NA,
                         NA,
                         NA,
                         tagList)

  user.prop.vector <- rep("followers", numUsers)
  user.nvalue.vector <- profilesdf$followersCount
  retVal <-   # write the user's followers
    tnum.maketruenumbers(user.subj.vector,
                         user.prop.vector,
                         NA,
                         user.nvalue.vector,
                         NA,
                         NA,
                         tagList)

  user.prop.vector <- rep("friends", numUsers)
  user.nvalue.vector <- profilesdf$friendsCount
  retVal <-   # write the user's followers
    tnum.maketruenumbers(user.subj.vector,
                         user.prop.vector,
                         NA,
                         user.nvalue.vector,
                         NA,
                         NA,
                         tagList)

  user.prop.vector <- rep("likes", numUsers)
  user.nvalue.vector <- profilesdf$favoritesCount
  retVal <-   # write the user's followers
    tnum.maketruenumbers(user.subj.vector,
                         user.prop.vector,
                         NA,
                         user.nvalue.vector,
                         NA,
                         NA,
                         tagList)

  user.prop.vector <- rep("location", numUsers)
  user.cvalue.vector <- lapply(profilesdf$location, escapequotes)
  retVal <-   # write the user's location
    tnum.maketruenumbers(user.subj.vector,
                         user.prop.vector,
                         user.cvalue.vector,
                         NA,
                         NA,
                         NA,
                         tagList)

}
