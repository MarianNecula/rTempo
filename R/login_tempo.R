#' @title Login to a Tempo Online session
#' @description Login to a new session to access Tempo Online. Requires a valid Tempo Online account. If no arguments are provided returns an error message.
#' @param usr A character string supplying a valid e-mail adress, used to create a valid Tempo Online account.
#' @param passwd A character string supplying a valid password, used to create a valid Tempo Online account.
#' @return Returns an object of class \code{\link{List}} which stores a valid http session for downloading Tempo Online data.
#' @author Toma, I., Tiru A., Necula, M.
#' @examples  \dontrun{
#' login_tempo(usr = "valid_user.name@mail_server.com", passwd = "valid_uncrackable_password")
#' }
#' @export
login_tempo <- function(usr = NULL, passwd = NULL) {
  if (is.null(usr) != TRUE && is.null(passwd) != TRUE) {
      message("Please select Tempo Online language:\n 1. Romanian 2. English")
      .InsEnv$lang_number <- readline()
      if(.InsEnv$lang_number == 1){
        .InsEnv$url_tempo <- "http://statistici.insse.ro/shop/"
        .InsEnv$lang_ins <- "ro"
      } else {
        .InsEnv$url_tempo <- "http://statistici.insse.ro/shop/?lang=en"
        .InsEnv$lang_ins <- "en"
      }

    ins_auth <- rvest::html_session(.InsEnv$url_tempo)
    form_login_u <- ins_auth %>% rvest::html_form()
    form_login_f <- rvest::set_values(form_login_u[[1]], Login = usr, Password = passwd)

    .InsEnv$session <- rvest::submit_form(ins_auth, form_login_f, submit = NULL)

  }
  else {
    message("Please supply a valid username and password")
  }
  return(.InsEnv$session)
}
