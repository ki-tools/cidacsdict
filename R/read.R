#' Read data from a DATASUS source (SINASC/SIM)
#'
#' @param path a path to a dbf, dbc, or csv file
# @importFrom readr read_csv
#' @importFrom utils read.csv
#' @importFrom foreign read.dbf
#' @importFrom read.dbc read.dbc
#' @export
#' @examples
#' \dontrun{
#' fin <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/DNRES/DNRO2013.DBC"
#' fout <- tempfile(fileext = ".DBC")
#' download.file(fin, destfile = fout)
#' d <- read_datasus(fout)
#' }
read_datasus <- function(path) {
  if (grepl("\\.csv$", path)) {
    b <- utils::read.csv(path, stringsAsFactors = FALSE)
  } else if (grepl("\\.dbf$", path)) {
    b <- foreign::read.dbf(path)
  } else {
    b <- read.dbc::read.dbc(path)
  }
  for (ii in seq_along(b)) {
    if (is.factor(b[[ii]]))
      b[[ii]] <- as.character(b[[ii]])
  }

  names(b) <- tolower(names(b))
  b
}

