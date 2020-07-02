#' Truenumber utility functions for working with Twitter/Tweets
#' @author True Engineering Technology, LLC Boston, MA USA
#' @references \url{http://www.truenum.com}
#'
library(twitteR)

#' Authenticate twitteR package with Twitter account
#'

tnum.twt.authorize <- function(){
twitter_token <- setup_twitter_oauth(
  consumer_key = "EJJSOPMbniEdgyxhD9Q6rZDp1",
  consumer_secret = "tcMRH9XTmXd6nq9hAFsYtsHW5cwsymN32duLCNmQIoqb3amwja",
  access_token = "1274782526926700546-PJpruW5N5CTkzycTj9gsZSePBdHv97",
  access_secret = "pfUowEnIFZRWOzJ8DDETiCNtIkT0PheEBuJnOE4O6bZvl")
}

#' Create truenumber data frame from tweets dataframe
#'
#' @param tnumdf dataframe of tweets returned from twitteR package
#'
#' @return dataframe of truenumbers
#' @export N/A
#'
#'
#' @examples
tnum.twt.tweets2tnum <- function(tnumdf){

}


