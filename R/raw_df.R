#' Raw data frame obtained from the RFID / PIT tag readers
#'
#' A dataset containing the putput with required formatted columns
#' "id", "time", "date", "antenna".
#'
#' @format A data frame with 808 rows and yy variables:
#' \describe{
#'   \item{id}{RFID /PIT tag id of individuals}
#'   \item{date}{date of read}
#'   \item{time}{time of read}
#'   \item{antenna}{antenna number}
#'   \item{GPS.coordinates}{time of read}
#'   \item{Identifier}{time of read}
#'   ...
#' }
#'
"raw_df"
