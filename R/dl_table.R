#' @keywords internal
dl_table <- function(pattern2, numar2) {

  if(is.null(.InsEnv$numar2)){
    for (k in 1:length(.InsEnv$pattern2)){
      form_js(.InsEnv$s1, .InsEnv$pattern2, k)
    }
  } else{
    k = .InsEnv$numar2
    form_js(.InsEnv$s1, .InsEnv$pattern2, k)
  }
  on.exit(.InsEnv$numar2 <- NULL)
}
