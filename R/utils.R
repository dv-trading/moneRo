

#' formatHashes
#'
#' pretty print for hash rate
#'
#' \itemize{
#'   \item 1 kH/s is 1,000 (one thousand) hashes per second.   
#'   \item 1 MH/s is 1,000,000 (one million) hashes per second.   
#'   \item 1 GH/s is 1,000,000,000 (one billion) hashes per second.   
#'   \item 1 TH/s is 1,000,000,000,000 (one trillion) hashes per second.   
#'   \item 1 PH/s is 1,000,000,000,000,000 (one quadrillion) hashes per second.   
#'   \item 1 EH/s is 1,000,000,000,000,000,000 (one quintillion) hashes per second.  
#' }
#'
#' @param x a number (of hashes)
#' @param units what units you want the hashes displayed in
#'   (e.g. "kH", "MH", etc.").  By default, the function will automatically
#'   try to figure out the best unit.
#' @param ... other arguments (not currently in use)
#' @author Garrett See
#' @return string
#' @references \url{http://bitcoin.stackexchange.com/a/21498}
#' @examples
#' \dontrun{
#' formatHashes(getNetHash())
#' }
#' @export
formatHashes <- function (x, units = "auto", ...) {
  units <- match.arg(units, c("H", "auto", "kH", "MH", "GH", "TH", "PH", "EH",
                              "KH"))
  if (units == "auto") {
    if (x >= 1000^6)
      units <- "EH"
    if (x >= 1000^5)
      units <- "PH"
    if (x >= 1000^4)
      units <- "TH"
    else if (x >= 1000^3)
      units <- "GH"
    else if (x >= 1000^2)
      units <- "MH"
    else if (x >= 1000)
      units <- "kH"
    else units <- "H"
  }
  switch(units, h = , H = paste(x, "H"),
         kH = , KH = paste(round(x/1000, 2L), "kH"),
         MH = paste(round(x/1000^2, 2L), "MH"),
         GH = paste(round(x/1000^3, 2L), "GH"),
         TH = paste(round(x/1000^4, 2L), "TH"),
         PH = paste(round(x/1000^5, 2L), "PH"),
         EH = paste(round(x/1000^6, 2L), "EH"))
}
