#' Download requests in a queue
#'
#' @export
#' @param x one or more download Ids or [occ_download] output objects
#' @param status_ping (integer) seconds between pings checking status of
#' the download request. generally larger numbers for larger requests.
#' default: 10 (i.e., 10 seconds). must be 10 or greater
#' @return a list of `occ_download` class objects, see [occ_download_get()]
#' to fetch data
#' @details This function is a convenience wrapper around [occ_download()],
#' allowing the user to kick off any number of requests, while abiding by
#' GBIF rules of 3 concurrent requests per user.
#' @note see [downloads] for an overview of GBIF downloads methods
#'
#' @section How it works:
#' It works by using lazy evaluation to collect your requests into a queue.
#' Then it kicks of the first 3 requests. Then in a while loop, we check
#' status of those requests, and when any request finishes, we kick off the
#' next, and so on. So in theory, there may not always strictly be 3 running
#' concurrently, but the function will usually provide for 3 running
#' concurrently.
#' 
#' @examples \dontrun{
#' # pass in single or more than one occ_download output
#' x <- occ_download('taxonKey = 3119195', "year = 1976"),
#' out <- occ_download_wait(x)
#' 
#' y <- occ_download("country = NZ", "year = 1999", "month = 3"),
#' z <- occ_download("catalogNumber = Bird.27847588", "year = 1998", "month = 2")
#' out <- occ_download_wait(c(y, z))
#' 
#' # pass in 1 or more ids
#' occ_download_wait(c(x$id, y$id, z$id))
#' }
occ_download_wait <- function(x, status_ping = 10) {
  assert(x, "character")
  assert(status_ping, c('integer', 'numeric'))
  stopifnot(status_ping >= 10)
  if (length(x) == 0) stop("length(x) must be greater than 0")

  # get only occ_download objects
  res <- Filter(function(x) inherits(x, "occ_download"), res)

  results <- list()
  still_running <- TRUE
  while (still_running) {
    metas <- lapply(res, occ_download_meta)
    status <- vapply(metas, "[[", "", "status", USE.NAMES = FALSE)
    still_running <- !all(tolower(status) %in% c('succeeded', 'killed'))
    Sys.sleep(status_ping)
  }
  return(results)
}

#' @export
#' @rdname occ_download
as.occ_download <- function(x, validate = TRUE) UseMethod("as.occ_download")
as.occ_download.default <- function(x, validate = TRUE) {
  stop("no 'as.occ_download' method for ", class(x)[1L])
}
as.occ_download.character <- function(x, validate = TRUE) {
  format <- NA_character_
  if (validate) format <- occ_download_meta(x)$format
  structure(x, class = "occ_download",
    user = Sys.getenv("GBIF_USER", NA_character_),
    email = Sys.getenv("GBIF_EMAIL", NA_character_),
    format = format)
}
