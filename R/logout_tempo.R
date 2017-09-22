#' @title Logout from Tempo Online session
#' @description The function provides logout from a Tempo Online
#' session by removing all the variables from the package environment.
#' If \code{\link{dir_tempo.R}} was used to create and set the working
#' directory the value of the working directory will reset to the
#' previous value.
#' @return Returns a reponse from the server.
#' @author Toma, I., Tiru A., Necula, M.
#' @examples \dontrun{
#' logout_tempo()
#' }
#' @export
logout_tempo <- function(){
  .InsEnv$session <- NULL
  on.exit(setwd(.InsEnv$working_dir))
}
