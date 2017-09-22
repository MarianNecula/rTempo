#' @title Searching Tempo Online by keywords
#' @description Function providing capabilities to search Tempo Online
#' by typing a keyword to be looked upon in Tempo Online.
#' @param search_word a vector of one or multiple strings provided by the user for querying Tempo Online
#' @author Toma, I., Tiru A., Necula, M.
#' @examples  \dontrun{
#' # starts the download for only one table selected by the user after the function call
#' search_tempo(search_word = c("population"))
#' # starts the download in bulk for all the tabels which contain the provided keywords
#' search_tempo(search_word = c("population", "inflation", "education"))
#' }
#' @export
search_tempo <- function(search_word =c()){
  if(is.null(.InsEnv$session)){
    message("You are not logged in.\nPlease login using log_tempo().")
  } else{
    if(.InsEnv$lang_number == 1){
      search_session <- .InsEnv$session %>% follow_link("Cautare TEMPO-Online")
      url_search <- search_session$url
      .InsEnv$search_lang <- "ro"
      search_submit <- "Cauta"
    } else{
      search_session <- .InsEnv$session %>% follow_link("Search TEMPO-Online")
      url_search <- search_session$url
      .InsEnv$search_lang <- "en"
      search_submit <- "Search"
    }
    if(length(search_word) > 1L){
      for (i in 1:length(search_word)){
    search_body <- as.list(c(lang = .InsEnv$search_lang, keyword = search_word[i], Submit = search_submit ))
    search_response <- rvest:::request_POST(search_session, url = "http://statistici.insse.ro/shop/index.jsp?page=searchResults&mod=cuv", body = search_body, encode = 'form', verbose())
    search_pattern <- search_response %>% xml2::read_html()  %>% rvest::html_nodes("a.link[href*=ind]") %>% rvest::html_text()
    tryCatch({if(length(search_pattern) == 0){
      stop("No matches have been found")
    }
    },error=function(e){cat("Error:", e, "\n")} )
    for(j in 1:length(search_pattern)){
    .InsEnv$s1 <- search_response
    .InsEnv$pattern2 <- search_pattern
    dl_table()
    }
      }
    } else {
    search_body <- as.list(c(lang = .InsEnv$search_lang, keyword = search_word, Submit = search_submit ))
    search_response <- rvest:::request_POST(search_session, url = "http://statistici.insse.ro/shop/index.jsp?page=searchResults&mod=cuv", body = search_body, encode = 'form', verbose())
    search_pattern <- search_response %>% xml2::read_html()  %>% rvest::html_nodes("a.link[href*=ind]") %>% rvest::html_text()
    tryCatch({if(length(search_pattern) == 0){
      stop("No matches have been found")
    }
    },error=function(e){cat("Error:", e, "\n")} )
    print(search_pattern)
    message("Please select the statistical table:")
    .InsEnv$numar2 <- readline() %>% as.integer()
    .InsEnv$s1 <- search_response
    .InsEnv$pattern2 <- search_pattern
    dl_table()
  }
  }
}
