assert <- function(x, y) {
  if (!is.null(x)) {
    if (!inherits(x, y)) {
      stop(deparse(substitute(x)), " must be of class ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}



status <- function(type){
  switch(type,
       "200" = {
         cat(sprintf("Request returned successfully!!!"), sep = "\n")
         cat(sprintf("Update: %s", Sys.Date()), sep = "\n")
       },
       "429" = {
         stop("HTTP status 429 Too Many Requests (> 100 in 5 mins). Please wait 5 minutes.")
       },
       "400" = {
         stop("HTTP status 400 Bad Query Parameters.")
       })
}
