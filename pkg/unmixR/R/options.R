##' @rdname options
##' @import settings
.options <- options_manager (
  debuglevel = 0L
  )

##' unmixR's package options
##'
##' \pkg{unmixR} uses \pkg{settings} for option management. Options can either
##' be set by \code{unmixR.options (key = value)}
##'
##' The following package-specific options are defined:
##'
##' \tabular{lll}{
##' debuglevel \tab >= 0L \tab indicates how much debuging output is to be produced.
##' }
##'
##' @param ... either \code{key = value} pairs to set options or the names of
##'   the options to retrieve. If no paramaters are passed, a list of all
##'   options is returned.
##' @return list of options
##' @rdname options
##' @examples
##' unmixR.options ()
##' unmixR.options ("debuglevel")
##' unmixR.options (debuglevel = 0L)
##' @export
unmixR.options <- function (...) {
  stop_if_reserved (...)
  .options (...)
}

.test (unmixR.options) <- function (){

  ## check list of defined options against (manually kept) list of documented
  ## options.
  checkEquals (c("debuglevel"), names (.options ()))

}
