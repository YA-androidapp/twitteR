# Copyright (c) 2017 YA-androidapp(https://github.com/YA-androidapp) All rights reserved.



# 1-1. inst from CRAN
# install.packages("twitteR", dependencies=T)

# or

# 1-2. inst from GitHub
# install.packages(c("devtools", "rjson", "bit64", "httr"))
# Restarting R session
# library(devtools)
# install_github("geoffjentry/twitteR")

# 2. Restarting R session





# 3. load
library(twitteR)
setwd("~/works/R/twitteR-master")
source("oauth_config.R")

# 4-1. OAuth
# PIN
# setup_twitter_oauth(tw.ck, tw.cs)

# or

# 4-2. when keys were already got
setup_twitter_oauth(tw.ck, tw.cs, tw.at, tw.as)



### end of common procs



user <- getUser("fooooobaaaarfooooobaaaarfooooobaaaar")



# add functions

getFavorites <- function(user, n = 20, maxID = NULL, sinceID = NULL, ...) {
    uParams <- parseUsers(user)
    cmd <- "favorites/list"
    params <- buildCommonArgs(max_id = maxID, since_id = sinceID)
    params[['user_id']] <- uParams[['user_id']]
    params[['screen_name']] <- uParams[['screen_name']]
    # twInterfaceObj$maxResults: 100
    # 'Requests per rate limit' of 'favorites/list': 15/user
    statusBase(cmd, params, n, 1500, ...)
}
environment(getFavorites) <- getNamespace("twitteR")

( favs <- getFavorites(user) )



getLists <- function (user, reverse = FALSE, ...) {
  uParams <- parseUsers(user)
  cmd <- "lists/list"
  params <- buildCommonArgs()
  params[["screen_name"]] <- uParams[["screen_name"]]
  params[["reverse"]] <- ifelse(reverse == TRUE, "true", "false")
  #return(doPagedAPICall(cmd, 100, params, ...))

  lists <- doPagedAPICall(cmd, 100, params, ...)
  listMatrix <- do.call(rbind, lists)
  rownames(listMatrix) <- NULL
  # listMatrix[, c("full_name", "slug")]
  listMatrix[, "full_name"]
}
environment(getLists) <- getNamespace("twitteR")

( lists <- getLists(user) )



getListMembers <- function(list_id=NULL, slug=NULL, user=NULL, n=20, include_entities=TRUE, skip_status=FALSE, ...) {
  if ( (is.null(list_id) || !is.numeric(list_id)) && ( is.null(slug) || slug=="") ) {
    warning("You have to specify the list using the list_id or slug!")
  }
  if ( is.null(list_id) && is.null(user) ) {
    warning("You have to specify the user!")
  }

  uParams <<- parseUsers(user)
  cmd <- "lists/members"
  params <- buildCommonArgs()


  if ( !is.null(list_id) ) {
    params[["list_id"]] <- uParams[["list_id"]]
  }else{
    params[["slug"]] <- slug
    params[["owner_screen_name"]] <- uParams[["screen_name"]]
  }

  params[["include_entities"]] <- ifelse(include_entities == TRUE, "true", "false")
  params[["skip_status"]] <- ifelse(skip_status == TRUE, "true", "false")
  #doCursorAPICall(cmd, 'users', num=n, params=params, method='GET', ...) # 20 results * 75 requests/window

  users <- doCursorAPICall(cmd, 'users', num=n, params=params, method='GET', ...)
  userMatrix <- do.call(rbind, users)
  rownames(userMatrix) <- NULL
  #userMatrix[, c("screen_name", "name")]
  userMatrix[, "screen_name"]
}
environment(getListMembers) <- getNamespace("twitteR")

( mems <- getListMembers(slug=slug, user=user, n=20, include_entities=TRUE) )
