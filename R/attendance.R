#' Title
#'
#' @param urlname
#' @param event_id
#' @param key
#' @param fields
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_event_rsvps <- function(urlname,
                            event_id,
                            key,
                            fields = c("response", "member"),
                            ...) {

  dots <- list(...)
  if (length(dots) == 0) {
    dots <- list("omit" = c("member.photo", "member.event_context"))
  }

  method <- paste0(urlname, "/events/", event_id, "/rsvps")
  .meetup_api_GET(method, fields, key, dots = dots, only_first = T)

}

#' Title
#'
#' @param urlname
#' @param event_id
#' @param key
#' @param fields
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_event_attendance <- function(urlname,
                                 event_id,
                                 key,
                                 fields = c("status", "member", "rsvp.response"),
                                 ...) {

  dots <- list(...)
  if (length(dots) == 0) {
    dots <- list("filter" = "all")
  }

  method <- paste0(urlname, "/events/", event_id, "/attendance")
  .meetup_api_GET(method, fields, key, dots = dots)

}


#' Title
#'
#' @param urlname
#' @param event_id
#' @param member_ids
#' @param member_status
#' @param key
#'
#' @return
#' @export
#'
#' @examples
mark_event_attendance <- function(urlname,
                                  event_id,
                                  member_ids,
                                  member_status,
                                  key) {

  # check that member status is length of id's
  if (length(member_status) != length(member_ids)) {
    stop("'member_status' must be the same length as 'member_ids'")
  }
  # check that member_status has only valid values
  chk_valid <- all(member_status %in% c("noshow", "abset", "attended"))
  if (!chk_valid) {
    stop("'member_status' contains invalid values")
  }

  # contstruct request
  method <- paste0(urlname, "/events/", event_id, "/attendance")
  req <- .construct_req(method, "", key, list("member" = member_ids,
                                              "status" = member_status))
  # send request
  httr::POST(req)

}
