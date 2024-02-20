#' Send javascript expression to the browser
#'
#' @param ... character: JS expression (will be pasted together with no whitespace)
#'
#' @export
evaljs <- function(...) shiny::getDefaultReactiveDomain()$sendCustomMessage("evaljs", paste0(...))

js_add_class <- function(id, cls) evaljs("$('#", id, "').addClass('", cls, "');")
js_remove_class <- function(id, cls) evaljs("$('#", id, "').removeClass('", cls, "');")
