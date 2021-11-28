#' @title Explore connected papers in a visual graph
#' @description Connected Papers \url{https://www.connectedpapers.com} is a unique, visual tool to help researchers and applied scientists find and explore papers relevant to their field of work. You can use Connected Papers to: 1)get a visual overview of a new academic field;
#' 2)make sure you haven't missed an important paper; 3)create the bibliography for your thesis; 4)discover the most relevant prior and derivative works. Future versions try to access network graph data.
#' @rdname  browse_cp
#' @name browse_cp
#' @param query \code{string} To start, enter a paper identifier.
#' @param option Supported paper identifier services: 's2' for semantic scholar id, 'doi' for Doi, 'pmid' for pubmed id, 'arxiv' for arxiv id.
#' @param ... see \code{\link{browseURL}} to set more params.
#' @return Browse through the desktop browsers
#' @author Liuyong Ding
#' @importFrom utils browseURL
#' @details How does it work? Visit the website \url{https://www.connectedpapers.com/about} for more details.
#' @examples
#' \dontrun{
#' # via semantic scholar id
#' browse_cp('9397e7acd062245d37350f5c05faf56e9cfae0d6',"s2")
#'
#' # via doi
#' browse_cp('10.1109/TIP.2014.2299154',"doi")
#'
#' # via pubmed id
#' browse_cp('32171076',"pmid")
#'
#' # via arxiv id
#' browse_cp('1512.03385',"arxiv")
#' }
#' @export

browse_cp <- function(query = NULL,option = "s2",...) {
  assert(query, "character")
  assert(option, "character")
  cat(sprintf("last Update %s",Sys.Date()-1),sep = "\n")
  url <- 'https://www.connectedpapers.com/api/redirect/'
  option <- match.arg(option, c("s2","doi","pmid","arxiv"))
  switch(as.character(option),
         "s2" = {browseURL(paste0(url,"s2/",query),...)},
         "doi" = {browseURL(paste0(url,"doi/",query,...))},
         "pmid" = {browseURL(paste0(url,"pmid/",query),...)},
         "arxiv" = {browseURL(paste0(url,"arxiv/",query),...)})
}
