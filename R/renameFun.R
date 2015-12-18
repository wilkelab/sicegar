#' Title
#'
#' @param dat
#' @param newnames
#' @param oldnames
#'
#' @return Describe here what the function returns.
#' @export
#'
#' @examples
#' # add usage examples here
renameFun <-
function(dat, newnames, oldnames) {
  datnames <- colnames(dat)
  datnames[which(datnames %in% oldnames)] <- newnames
  colnames(dat) <- datnames
}
