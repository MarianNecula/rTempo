#' @keywords internal
form_js <-  function(session, patterns, k, ...) {


  s2_2 <- .InsEnv$s1 %>% rvest::follow_link(.InsEnv$pattern2[k])


  pattern3 <- s2_2 %>% xml2::read_html()
  form1_u <- rvest::html_form(s2_2)

  valori = c()
  body1 = c()
  rp = c()
  for (i in 1:(length(form1_u[[1]]$fields) - 5)) {
    opts2 <- form1_u[[1]][[5]][[i]][[3]] %>% as.list()
    number <- length(opts2)
    rp[i] <- c(as.integer(number))
    body1[i] <- unlist(rep(names(form1_u[[1]][[5]][i])))
    valori <- append(valori, unlist(form1_u[[1]][[5]][[i]][[3]], use.names = FALSE))
  }
  if (0 %in% rp ==  TRUE){
    if(is.null(.InsEnv$numar2) == FALSE){
      stop("Form populated dinamically! Implementation will come soon.")
    }
    message("Form populated dinamically! Implementation will come soon.")
    k = k+1
    form_js(.InsEnv$s1, .InsEnv$pattern2, k)
  } else {

    body1 <- rep(body1, rp)
    names(valori) <- body1
    val3 <-
      as.list(
        c(
          valori,
          ind = form1_u[[1]]$fields$ind$value,
          cauta = "1",
          page = "tempo4",
          lang = .InsEnv$lang_ins,
          seldb = "Cauta"
        )
      )
    s3_2 <-
      rvest:::request_POST(s2_2,
                           url = .InsEnv$url_tempo,
                           body = val3,
                           encode = "form")
    Sys.sleep(2)

    f3 <- rvest::html_form(s3_2)
    s4_2 <- rvest::submit_form(s3_2, f3[[3]])

    # save .csv file to disk in the working directory
    s4_2$response %>% httr::content(type = 'text/csv',
                                    col_names = TRUE,
                                    col_types = NULL) %>% write.csv(file = paste0(gsub("\\n", "",  .InsEnv$pattern2[k]), '.csv'))
    response <-
      httr::content(
        s4_2$response,
        type = 'text/csv',
        col_names = TRUE,
        col_types = NULL) %>% as.data.frame(stringAsFactors = default.stringsAsFactors())
    response <- response[-1,]

    # retrieve metadata
    tryCatch({session3 <- s2_2 %>% rvest::follow_link(css = "a[href*=metadata]")
    pattern_meta <-  session3 %>% xml2::read_html() %>% rvest::html_nodes("table.msrAdd")
    pattern_meta_name <-  paste(as.character(pattern_meta), collapse = '\n')
    write(pattern_meta_name, file = paste0(gsub(" -.*$", "",  .InsEnv$pattern2[k]), '-meta.html'))
    .InsEnv$table_name <- paste0(gsub(" -.*$", "",  .InsEnv$pattern2[k]), '-meta.html')
    },error=function(e){})

    # retrieve definitions
    pattern4 <- s2_2 %>% xml2::read_html() %>% rvest::html_nodes("table.tempoResults")
    pattern42 <- paste(as.character(pattern4[[2]]), collapse = '\n')
    write(pattern42, file = paste0(gsub(" -.*$", "",  .InsEnv$pattern2[k]), '.html'))
    .InsEnv$table_name <- paste0(gsub(" -.*$", "",  .InsEnv$pattern2[k]), '.html')

    assign(gsub(" -.*$", "",  .InsEnv$pattern2[k]), response, envir = globalenv())
  }
  on.exit(.InsEnv$numar2 <- NULL)
}
