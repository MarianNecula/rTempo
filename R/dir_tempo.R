#' @title Create INS working directory
#' @description Function to create INS directory and set it as working directory.
#' @author Toma, I., Tiru A., Necula, M.
#' @examples  \dontrun{
#' def_tempo("table_name.html")
#' }
#' @export

dir_tempo <-  function(){
  .InsEnv$working_dir <- getwd()
  if(dir.exists(paste0(paste0(getwd(),"/INS"))) ==  TRUE){
    message("The INS directory exists.")
    setwd("INS")
  } else {
    dir.create(paste0(getwd(),"/INS"))
    setwd("INS")
  }
}
