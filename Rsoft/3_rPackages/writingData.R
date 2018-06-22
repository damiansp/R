#' A function for doing something
#'
#' This function takes some action. It also attempts to create a file on your
#' desktop called \code{data.txt}. If \code{data.txt} cannot be created a
#' warning is raised.
#' 
#' @param force If set to \code{TRUE}, \code{data.txt} will be created on the
#' user's Desktop if their Desktop exists. If this function is used in an
#' interactive session the user will be asked whether or not \code{data.txt}
#' should be created. The default value is \code{FALSE}.
#'
#' @export
some_function <- function(force=F) {
  # ... some code that does something useful ...
  if (!dir.exists(file.path("~", "Desktop"))) {
    warning("No Desktop found.")
  } else {
    if (!force && interactive()) {
      result <- (select.list(c("Yes", "No"), 
                 title="May this program create data.txt on your desktop?"))
      if (result == "Yes") {
        file.create(file.path("~", "Desktop", "data.txt"))
      }
    } else if (force) {
      file.create(file.path("~", "Desktop", "data.txt"))
    } else {
      warning("data.txt was not created on the Desktop.")
    }
  }
}