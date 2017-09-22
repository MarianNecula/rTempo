#' @title Text-based navigation in Tempo Online
#' @description Navigate in Tempo Online by select the numbers coresponding
#' to the name of the statistical domain.
#' @return Returns an object of class List which stores a http
#' session based on the user's input.
#' @author Toma, I., Tiru A., Necula, M.
#' @examples \dontrun{
#' nav_tempo()
#' }
#' @export
nav_tempo <- function(){
  if(is.null(.InsEnv$session)){
    message("You are not logged in.\nPlease login using log_tempo().")
  } else{
    message ("Do you wish to dowload\n 1. A single table?\n 2. Bulk download a statistical sub-domanin of tables?\n 3. Logout?")
    numb_downl <- readline() %>% as.integer()
    if(numb_downl == "1"){
      # navigate in level 1 TEMPO
      pattern <-
        .InsEnv$session %>% xml2::read_html() %>% rvest::html_nodes("td.resultsValues:nth-child(2)>a.link[href*=ind]") %>% rvest::html_text()
      print(pattern)
      message("Please type the number of the statistical domain:")
      numar1 <- readline() %>% as.integer()
      .InsEnv$s1 <- .InsEnv$session %>% rvest::follow_link(pattern[numar1])

      # navigate in level 2 TEMPO
      .InsEnv$pattern2 <-
        .InsEnv$s1  %>% rvest::html_nodes("td.resultsValues:nth-child(2)>a.link[href*=ind]") %>% rvest::html_text()

      print(.InsEnv$pattern2)
      message("Please type the number of the statistical domain:")
      .InsEnv$numar2 <- readline() %>% as.integer()
      dl_table()
    } else if(numb_downl == "2"){
      # navigate in level 1 TEMPO
      pattern <-
        .InsEnv$session %>% xml2::read_html() %>% rvest::html_nodes("td.resultsValues:nth-child(2)>a.link[href*=ind]") %>% rvest::html_text()
      print(pattern)
      message("Please type the number of the statistical domain:")
      numar <- readline() %>% as.integer()
      .InsEnv$s1 <- .InsEnv$session %>% rvest::follow_link(pattern[numar])
      .InsEnv$pattern2 <-
      .InsEnv$s1  %>% rvest::html_nodes("td.resultsValues:nth-child(2)>a.link[href*=ind]") %>% rvest::html_text()
      dl_table()
    } else if(numb_downl == "3"){
    logout_tempo()
    }
    else {
      message ("Invalid selection")
    }
  }
}
