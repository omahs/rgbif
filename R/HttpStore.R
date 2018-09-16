#' HttpStore
#' 
#' @export
#' @keywords internal
#' @section methods:
#' 
#' - count: number of requests recorded
#' - put: record a request
#' - pop: delete the last request
#' - wait: decide if should sleep or not. sleeps for amount
#' of time `sleep` (seconds)
#' 
#' @section wait method parameters:
#' 
#' - sleep (integer): seconds to sleep, default: 1 sec
#' - report (logical): whether to report sleep time or not as a message, 
#' default: `FALSE`
#' 
#' @details The original idea was to use dates in returned http response 
#' headers from GBIF, but apparently it seems if they serve a cached
#' response that the date header is from whenever that response was cached,
#' so it's not the time/date the response itself is served. Thus, we just 
#' use a timestamp created immediately after a response is returned.
#' 
#' @examples
#' x <- HttpStore$new()
#' x$count()
#' x$requests
#' x$put("Thu, 13 Sep 2018 02:51:08 GMT")
#' x$requests
#' x$count()
#' x$wait()
#' 
#' x$put(format(Sys.time(), format = "%d %B %Y %H:%M:%S", tz="UTC"))
#' x$wait()
#' 
#' # failure behavior
#' x <- rgbif:::HttpStore$new()
#' x$put(45)
#' x$wait()
#' x$put('asdfadfafasdfdffsdf')
#' x$requests
#' x$wait()
HttpStore <- R6::R6Class(
  "HttpStore", 
  public = list(
    requests = list(),

    count = function() length(self$requests),
    put = function(x) {
      self$requests <- c(self$requests, 
        as.character(lubridate::dmy_hms(as.character(x))))
    },
    wait = function(sleep = 1, report = FALSE) {
      now <- lubridate::now("UTC")
      if (self$count() > 0) {
        last <- tryCatch(
          lubridate::ymd_hms(self$requests[length(self$requests)]),
          warning = function(w) w
        )
        if (inherits(last, "warning") || is.na(last)) return(invisible())
        diff <- (now - last)
        diffatt <- attr(diff, "units")
        if (diffatt == "mins") diff <- diff * 60
        if (diffatt == "hours") diff <- diff * 60 * 60

        if (diff < sleep) {
          wait <- sleep - diff
          if (report) message("waiting ", wait, " sec")
          Sys.sleep(sleep - diff)
        }
      }
    }
  )
)

now_stamp <- function() {
  format(lubridate::now("UTC"), format = "%d %B %Y %H:%M:%S", tz="UTC")
}
