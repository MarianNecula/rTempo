#' @title Viewing definitions.
#' @description Function to view the definitions of the statistical indices
#' in R Studio's Viewer pane, after the table was downloaded. Table name is
#' given by the name of the data frame object loaded in the environment by
#' after downloading the table.
#' @author Toma, I., Tiru A., Necula, M.
#' @examples \dontrun{
#' def_tempo("table_name.html")
#' }
#' @export
# print Tempo definitions in R Studio viewer
def_tempo <- function(table){
  def <- file.path(getwd(), table)
  file.copy(def, tempdir())
  viewer <- getOption("viewer")
  viewer(file.path(tempdir(), table), height = 300)
}
