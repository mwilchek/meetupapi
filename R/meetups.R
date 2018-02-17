#' Title
#'
#' @param key
#' @param fields
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_joined_meetups <- function(key,
                               fields = c("id", "name", "urlname", "link"),
                               ...) {

  dots <- list(...)
  .meetup_api_GET("self/groups", fields, key, dots)

}

#' Title
#'
#' @param urlname
#' @param key
#' @param fields
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_meetup_members <- function(urlname,
                               key,
                               fields = c("id", "name"),
                               ...) {

  dots <- list(...)
  method <- paste0(urlname, "/members/")
  .meetup_api_GET(method, fields, key, dots = dots)

}

#' Title
#'
#' @param urlname
#' @param key
#' @param fields
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_meetup_events <- function(urlname,
                              key,
                              fields = c("status", "id", "name"),
                              ...) {

  dots <- list(...,
               "status" = c("upcoming", "past"),
               "desc" = "true")

  method <- paste0(urlname, "/events")
  .meetup_api_GET(method, fields, key, dots = dots)

}

