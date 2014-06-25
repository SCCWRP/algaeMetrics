
#' Read internal algae metadata table
#
#' @export
#' 
#' @examples
#' meta <- loadAlgaeMetaData()
#' View(meta)

loadAlgaeMetaData <- function(){
  read.csv(system.file("extdata", "algaeMetaData3.csv", package="algaeMetrics"),
                       stringsAsFactors=FALSE)
}