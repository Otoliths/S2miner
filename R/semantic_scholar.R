#' @title A comma-separated list of the fields available for use in API calls
#' @description This function get fields for fetching paper and author data from the Semantic Scholar corpus.
#' @rdname get_fields
#' @name get_fields
#' @format The following list of the fields to be returned are recognized:
#' \itemize{
#' \item paper_search : fields on searching for papers by keyword.
#' \item paper : fields on looking up details about a paper.
#' \item paper_authors : fields on looking up details about a paper's authors.
#' \item paper_citations : fields on fetching details about the papers the cite this paper (i.e. papers in whose bibliography this paper appears).
#' \item paper_references : fields on fetching details about the papers cited by this paper (i.e. appearing in this paper's bibliography).
#' \item author : fields on looking up details about an author.
#' \item author_papers : fields on fetching the papers of an author in batches,only retrieves the most recent 10,000 citations/references for papers belonging to the batch.
#' }
#' @return list representing fields parms.
#' @author Liuyong Ding
#' @importFrom httr GET
#' @importFrom httr status_code
#' @importFrom jsonlite fromJSON
#' @details See for website \url{https://api.semanticscholar.org/graph/v1} details.
#' @examples
#' \dontrun{
#' # get all fileds
#' get_fields()
#'
#' # check type of fileds
#' names(get_fields())
#'
#' # get author of fileds
#' get_fields()[["author"]]
#' }
#' @export
get_fields <- function() {
  url <- "https://api.semanticscholar.org/graph/v1/swagger.json"
  res <- httr::GET(url)
  status(as.character(httr::status_code(res)))
  if (httr::status_code(res) == 200){
    result <- jsonlite::fromJSON(url)
    cat(sprintf("Welcome to use Literature Graph Service (%s)",result[["info"]][["version"]]), sep = "\n")
    S2_author_fields <- c(names(result[["definitions"]][["AuthorWithPapers"]][["properties"]]),
                          paste0("papers",".",names(result[["definitions"]][["PaperWithLinks"]][["properties"]])[1:13])
    )
    S2_author_papers_fields <- c(names(result[["definitions"]][["PaperWithLinks"]][["properties"]]),
                                 paste0("citations",".",names(result[["definitions"]][["PaperInfo"]][["properties"]])),
                                 paste0("references",".",names(result[["definitions"]][["PaperInfo"]][["properties"]]))
    )
    S2_paper_search_fields <- c(names(result[["definitions"]][["PaperWithLinks"]][["properties"]])[1:13],
                                paste0("authors",".",names(result[["definitions"]][["AuthorInfo"]][["properties"]]))
    )
    S2_paper_fields <- c(names(result[["definitions"]][["FullPaper"]][["properties"]]),
                         paste0("authors",".",names(result[["definitions"]][["Author"]][["properties"]])[1:7]),
                         paste0("citations",".",names(result[["definitions"]][["PaperInfo"]][["properties"]])),
                         paste0("references",".",names(result[["definitions"]][["PaperInfo"]][["properties"]]))
    )
    S2_paper_authors_fields <- c(names(result[["definitions"]][["AuthorWithPapers"]][["properties"]]),
                                 paste0("papers",".",names(result[["definitions"]][["PaperWithLinks"]][["properties"]])[1:13])
    )
    S2_paper_citations_fields <- c(names(result[["definitions"]][["Citation"]][["properties"]])[1:3],
                                   names(result[["definitions"]][["PaperWithLinks"]][["properties"]])[1:13]
    )
    return(list(
      author = S2_author_fields,
      author_papers = S2_author_papers_fields,
      paper_search = S2_paper_search_fields,
      paper = S2_paper_fields,
      paper_authors = S2_paper_authors_fields,
      paper_citations = S2_paper_citations_fields,
      paper_references = S2_paper_citations_fields
    ))
  }

}


#' @title Searching for papers by keyword
#' @description This function retrieves Semantic Scholar \url{https://www.semanticscholar.org} data for a keyword search.
#' @rdname search_papers
#' @name search_papers
#' @param keyword \code{string} A plain-text search query string. No special query syntax is supported.Supports AND and OR and inclusion and exclusion of terms, for example "title=bibliometrics" or "+Epidemic+Modeling+Canada-COVID".See blog post \url{https://medium.com/ai2-blog/building-a-better-search-engine-for-semantic-scholar-ea23a0b661e7} for a description of our search relevance algorithm.
#' @param offset \code{integer} Default: 0. When returning a list of results, start with the element at this position in the list.
#' @param limit \code{integer} Default: 100. The maximum number of results to return. The sum of offset and limit must be < 10000.
#' @param fields \code{string} A comma-separated list of the fields to be returned, support one or more fileds, for example fileds = get_fields()[['paper_search']] was all fields to be returned. See Format or see \code{\link{get_fields}} to set more params
#' @format The following case-sensitive paper fields are recognized:
#' \itemize{
#' \item paperId : S2 generated research paper ID (Always included).
#' \item externalIds : Other catalog IDs for this paper, if known. Supports ArXiv, MAG, ACL, PubMed, Medline, PubMedCentral, DBLP, DOI.
#' \item title : Research paper title (Included if no fields are specified).
#' \item url : URL on the Semantic Scholar website.
#' \item abstract : Extracted abstract of the paper.
#' \item venue : Extracted publication venue for this paper.
#' \item year : Year this paper was published as integer.
#' \item referenceCount : The number of reference for this paper.
#' \item citationCount : The number of citation for this paper.
#' \item influentialCitationCount : The number of influential citation for this paper.
#' \item isOpenAccess : \url{https://www.openaccess.nl/en/what-is-open-access}
#' \item fieldsOfStudy : A list of high-level academic categories.
#' \item authors : Author(s) for this paper (Up to 500 will be returned).
#' \itemize{
#' \item authorId : Always included
#' \item name : Always included
#' }
#' }
#' @return list representing paper objects
#' @details You can use the Semantic Scholar API endpoint up to 100 requests per 5 minutes to test your application idea. To access a higher rate limit, complete the form \url{https://www.semanticscholar.org/product/api#Partner-Form} to request authentication for your project.
#' Current API users may have noticed that some paper requests would error out if the paper data was too large. The new service avoids this problem by, first, allowing you to specify exactly the data you’re interested in and, second, allowing you to paginate through large result sets, such as papers with many citations.
#' @author Liuyong Ding
#' @importFrom utils URLencode
#' @importFrom httr GET
#' @importFrom httr status_code
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#' res <- search_papers(keyword = "bibliometrics",fields = c("title","abstract","authors"))
#' }
#' @export
search_papers <- function(keyword, offset = 0, limit = 100, fields = NULL){
  stopifnot(!is.null(keyword), is.character(keyword))
  stopifnot(!is.null(offset), is.integer(as.integer(offset)))
  stopifnot(!is.null(limit), is.integer(as.integer(limit)))
  baseurl <- "https://api.semanticscholar.org/graph/v1/paper/search?"
  fields <- match.arg(fields, get_fields()[["paper_search"]],several.ok = TRUE)
  res <- httr::GET(url = baseurl, query = list(query = URLencode(keyword),offset = offset, limit = limit, fields = paste0(fields, collapse = ",")))
  status(as.character(httr::status_code(res)))
  if (httr::status_code(res) == 200){
    cat("Downloading ....", sep = "\n")
    result <- jsonlite::fromJSON(paste0(baseurl,"query=",URLencode(keyword),"&offset=",offset,"&limit=",limit,"&fields=",paste0(fields, collapse = ",")))
    cat(sprintf("The total number of queries: %s", result[["total"]]), sep = "\n")
    cat(sprintf("The number of returns: %s", nrow(result[["data"]])), sep = "\n")
    cat(sprintf("starting position for this batch: %s", result[["offset"]]), sep = "\n")
    cat(sprintf("starting position of the next batch: %s", result[["next"]]), sep = "\n")
    cat("Done!!!", sep = "\n")
    if (length(result$data) > 0) {
      class(result$data) <- c("tbl_df", "tbl", "data.frame")
    }
    return(result)
  }
}

#' @title Lookup details about a paper
#' @description This function fetch a paper data from the Semantic Scholar corpus.
#' @rdname lookup_paper
#' @name lookup_paper
#' @param paper_id \code{string} Supported paper identifier services: Semantic Scholar ID, Semantic Scholar numerical ID, Digital Object Identifier, arXiv.rg, Microsoft Academic Graph, Association for Computational Linguistics, PubMed/Medline, PubMed Central and  URL from one of the sites listed below. See Format for detalis.
#' @param fields \code{string} A comma-separated list of the fields to be returned, support one or more fileds, for example fileds = get_fields()[['papers']] was all fields to be returned. See Format or see \code{\link{get_fields}} to set more params.
#' @format The following types of paper_id are supported:
#' \itemize{
#' \item <sha> - a Semantic Scholar ID, e.g. 649def34f8be52c8b66281af98ae884c09aef38b
#' \item CorpusId:<id> - Semantic Scholar numerical ID, e.g. 215416146
#' \item DOI:<doi> - a Digital Object Identifier, e.g. DOI:10.18653/v1/N18-3011
#' \item ARXIV:<id> - arXiv.rg, e.g. ARXIV:2106.15928
#' \item MAG:<id> - Microsoft Academic Graph, e.g. MAG:112218234
#' \item ACL:<id> - Association for Computational Linguistics, e.g. ACL:W12-3903
#' \item PMID:<id> - PubMed/Medline, e.g. PMID:19872477
#' \item PMCID:<id> - PubMed Central, e.g. PMCID:2323736
#' \item URL:<url> - URL from one of the sites listed below, e.g. URL:https://arxiv.org/abs/2106.15928v1
#' \itemize{
#' \item semanticscholar.org \url{https://www.semanticscholar.org}
#' \item arxiv.org \url{https://arxiv.org/}
#' \item aclweb.org \url{https://www.aclweb.org/}
#' \item acm.org \url{https://www.acm.org/}
#' \item biorxiv.org \url{https://www.biorxiv.org/}
#' }
#' }
#'
#' The following returned results are recognized:
#' \itemize{
#' \item paperId \code{string}
#' \item externalIds \code{object} Other catalog IDs for this paper, if known. Supports ArXiv, MAG, ACL, PubMed, Medline, PubMedCentral, DBLP, DOI.
#' \item url \code{string} URL on the Semantic Scholar website
#' \item title \code{string} This field will be provided if no fields are specified.
#' \item abstract \code{string}
#' \item venue \code{string}
#' \item year \code{integer}
#' \item referenceCount \code{integer}
#' \item citationCount \code{integer}
#' \item influentialCitationCount \code{integer} \url{https://www.semanticscholar.org/faq#influential-citations}
#' \item isOpenAccess \code{boolean} \url{https://www.openaccess.nl/en/what-is-open-access}
#' \item fieldsOfStudy \code{object} A list of high-level academic categories.
#' \item authors \code{object} 	Array of objects (Author Info).
#' \item citations \code{object} Array of objects (Paper Info)
#' \item references \code{object} Array of objects (Paper Info
#' \item embedding \code{object} 	object (Embedding)
#' \item tldr \code{object} Too Long; Didn't Read. Artificial Intelligence \url{https://www.semanticscholar.org/product/semantic-reader} summarizes academic articles for you.
#' }
#'
#' @return list representing paper objects
#' @details See website \url{https://api.semanticscholar.org/graph/v1#operation/get_graph_get_paper} for details.
#' @author Liuyong Ding
#' @importFrom utils URLencode
#' @importFrom httr GET
#' @importFrom httr status_code
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#' res <- lookup_paper(paper_id = "649def34f8be52c8b66281af98ae884c09aef38b")
#' }
#' @export
lookup_paper <- function(paper_id,fields = c("externalIds","title","abstract","year","tldr")){
  stopifnot(!is.null(paper_id), is.character(paper_id),length(paper_id) == 1)
  baseurl <- "https://api.semanticscholar.org/graph/v1/paper/"
  fields <- match.arg(fields, get_fields()[["paper"]],several.ok = TRUE)
  url <- paste0(baseurl,URLencode(paper_id),"?fields=",paste0(fields, collapse = ","))
  res <- httr::GET(url = url)
  status(as.character(httr::status_code(res)))
  if (httr::status_code(res) == 200){
    cat("Downloading ....", sep = "\n")
    result <- jsonlite::fromJSON(url)
    cat(sprintf("Scholar: %s", result[["name"]]), sep = "\n")
    cat("Done!!!", sep = "\n")
    return(result)
  }
}

#' @title Traversing the literature graph of a paper's information
#' @description This function fetch details about the papers the cite this paper (i.e. papers in whose bibliography this paper appears) and cited by this paper (i.e. appearing in this paper's bibliography)
#' @rdname traverse_paper
#' @name traverse_paper
#' @param paper_id \code{string} Supported paper identifier services: Semantic Scholar ID, Semantic Scholar numerical ID, Digital Object Identifier, arXiv.rg, Microsoft Academic Graph, Association for Computational Linguistics, PubMed/Medline, PubMed Central and  URL from one of the sites listed below. See Format for detalis.
#' @param offset \code{integer} Default: 0. When returning a list of results, start with the element at this position in the list.
#' @param limit \code{integer} Default: 100. The maximum number of results to return. The sum of offset and limit must be < 10000.
#' @param type \code{string} Supported paper query services: "authors","citations", OR "references".
#' @param fields \code{string} A comma-separated list of the fields to be returned, support one or more fileds, for example fileds = get_fields()[['paper_citation']] for "citations" was all fields to be returned. See Format or see \code{\link{get_fields}} to set more params.
#' @format The following types of paper_id are supported:
#' \itemize{
#' \item <sha> - a Semantic Scholar ID, e.g. 649def34f8be52c8b66281af98ae884c09aef38b
#' \item CorpusId:<id> - Semantic Scholar numerical ID, e.g. 215416146
#' \item DOI:<doi> - a Digital Object Identifier, e.g. DOI:10.18653/v1/N18-3011
#' \item ARXIV:<id> - arXiv.rg, e.g. ARXIV:2106.15928
#' \item MAG:<id> - Microsoft Academic Graph, e.g. MAG:112218234
#' \item ACL:<id> - Association for Computational Linguistics, e.g. ACL:W12-3903
#' \item PMID:<id> - PubMed/Medline, e.g. PMID:19872477
#' \item PMCID:<id> - PubMed Central, e.g. PMCID:2323736
#' \item URL:<url> - URL from one of the sites listed below, e.g. URL:https://arxiv.org/abs/2106.15928v1
#' \itemize{
#' \item semanticscholar.org \url{https://www.semanticscholar.org}
#' \item arxiv.org \url{https://arxiv.org/}
#' \item aclweb.org \url{https://www.aclweb.org/}
#' \item acm.org \url{https://www.acm.org/}
#' \item biorxiv.org \url{https://www.biorxiv.org/}
#' }
#' }
#' @return list representing paper objects
#' @details See website \url{https://api.semanticscholar.org/graph/v1#operation/get_graph_get_paper_authors} for details.
#' @author Liuyong Ding
#' @importFrom utils URLencode
#' @importFrom httr GET
#' @importFrom httr status_code
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#' res <- traverse_paper(paper_id = "649def34f8be52c8b66281af98ae884c09aef38b")
#' }
#' @export
traverse_paper <- function(paper_id, offset = 0, limit = 100, type = "citations", fields = NULL){
  stopifnot(!is.null(paper_id), is.character(paper_id),length(paper_id) == 1)
  stopifnot(!is.null(offset), is.integer(as.integer(offset)))
  stopifnot(!is.null(limit), is.integer(as.integer(limit)))
  baseurl <- "https://api.semanticscholar.org/graph/v1/paper/"
  type <- match.arg(type, c("authors","citations","references"))
  switch(type,
         "authors" = {
           fields <- match.arg(fields, get_fields()[["paper_authors"]],several.ok = TRUE)
           url <- paste0(baseurl,URLencode(paper_id),"/",type,"?fields=",paste0(fields, collapse = ","),"&offset=",offset,"&limit=",limit)
           res <- httr::GET(url = url)
           status(as.character(httr::status_code(res)))
         },
         "citations" = {
           fields <- match.arg(fields, get_fields()[["paper_citations"]],several.ok = TRUE)
           url <- paste0(baseurl,URLencode(paper_id),"/",type,"?fields=",paste0(fields, collapse = ","),"&offset=",offset,"&limit=",limit)
           res <- httr::GET(url = url)
           status(as.character(httr::status_code(res)))
         },
         "references" = {
           fields <- match.arg(fields, get_fields()[["paper_references"]],several.ok = TRUE)
           url <- paste0(baseurl,URLencode(paper_id),"/",type,"?fields=",paste0(fields, collapse = ","),"&offset=",offset,"&limit=",limit)
           res <- httr::GET(url = url)
           status(as.character(httr::status_code(res)))
         })
  if (httr::status_code(res) == 200){
    cat("Downloading ....", sep = "\n")
    result <- jsonlite::fromJSON(url)
    cat("Done!!!", sep = "\n")
    if (length(result$data) > 0) {
      class(result$data) <- c("tbl_df", "tbl", "data.frame")
    }
    return(result)
  }
}


#' @title Show author profile plot
#' @description This function show scholar profile plot
#' @rdname lookup_author
#' @name lookup_author
#' @param author_id \code{string} A plain-text search query string with author identifier.
#' @param open_url \code{logical} Default: FALSE. Is open scholar profile homepage.
#' @param plot \code{logical} Default: TRUE. Is show scholar profile plot.
#' @param size Size of label,default: size = 3.
#' @param ... Other arguments set by \code{\link{geom_col}}
#' @importFrom httr GET
#' @importFrom httr status_code
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr count
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr across
#' @importFrom dplyr ends_with
#' @importFrom tidyr gather
#' @importFrom magrittr `%>%`
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 theme_minimal
#' @importFrom  ggplot2 labs
#' @importFrom ggplot2 theme
#' @return a length-2 list of \code{\link{ggplot}} object and raw data.
#' @author Liuyong Ding
#' @examples
#' \dontrun{
#' res <- lookup_author(author_id = "48980001")
#' res$plot
#' }
#' @export
lookup_author <-function(author_id, open_url = FALSE, plot = TRUE, size = 3, ...){
  stopifnot(!is.null(author_id), is.character(author_id),length(author_id) == 1)
  baseurl <- "https://api.semanticscholar.org/graph/v1/author/"
  res <- httr::GET(url = paste0(baseurl,author_id), query = list(fields = paste0(get_fields()[["author"]], collapse = ",")))
  status(as.character(httr::status_code(res)))
  if (httr::status_code(res) == 200){
    cat("Downloading ....", sep = "\n")
    result <- jsonlite::fromJSON(paste0(baseurl,author_id,"?fields=",paste0(get_fields()[["author"]], collapse = ",")))
    cat(sprintf("Scholar: %s", result[["name"]]), sep = "\n")
    cat("Done!!!", sep = "\n")
  }
  if(plot){
    res1 <- result[["papers"]] %>% dplyr::count(year) %>% na.omit()
    res2 <- result[["papers"]] %>% dplyr::select(year,citationCount,influentialCitationCount) %>% na.omit() %>%
      dplyr::group_by(year) %>% dplyr::summarize(dplyr::across(dplyr::ends_with("Count"), list(mean), .names = "{.col}"))
    names(res1)[2] <- "Publications"
    names(res2)[2:3] <- c("Citations","Highly Influential Citations")
    dt <- tidyr::gather(merge(res1,res2), key = "element", value = "value", -year)
    dt$value <- round(dt$value,1)
    dt$element <- factor(dt$element,levels = c("Publications","Citations","Highly Influential Citations"))
    plot <- dt %>% ggplot2::ggplot(aes_string("year","value"))+
      #geom_bar(stat = "identity", width = 1) +
      ggplot2::geom_col(ggplot2::aes_string(fill = "element"))+
      ggplot2::facet_grid(element~.,scales = "free")+
      ggplot2::geom_text(ggplot2::aes_string(label = "value",col = "element"), vjust = -0.2, size = size)+
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(x = NULL, y = "Count",
           title = result[["name"]],
           subtitle = paste("Publications:",result[["paperCount"]],";",
                            "Citations:",result[["citationCount"]],";",
                            "Highly Influential Citations:",sum(result[["papers"]][["influentialCitationCount"]]),";",
                            "h-index:",result[["hIndex"]]),
           caption = paste("Update:", Sys.Date()))+
      ggplot2::theme(legend.position = "none")
      return(list(plot = plot,rawdata = result))
  }
  if(open_url){
    browseURL(result[["url"]])
  }
  return(list(rawdata = result))
}

#' @title Traversing the literature graph of an author's papers
#' @description This function fetch the papers of an author in batches.
#' @rdname traverse_author
#' @name traverse_author
#' @param author_id \code{string} A plain-text search query string with author identifier.
#' @param offset \code{integer} Default: 0. When returning a list of results, start with the element at this position in the list.
#' @param limit \code{integer} Default: 100. The maximum number of results to return. The sum of offset and limit must be < 10000.
#' @param fields \code{string} A comma-separated list of the fields to be returned, support one or more fileds, for example fileds = get_fields()[['author_papers']] was all fields to be returned. See Format or see \code{\link{get_fields}} to set more params
#' @return list representing author's papers
#' @details Only retrieves the most recent 10,000 citations/references for papers belonging to the batch.
#' @author Liuyong Ding
#' @importFrom httr GET
#' @importFrom httr status_code
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#' res <- traverse_author(author_id = "48980001",fields = c("title","abstract","authors","year"))
#' }
#' @export
traverse_author <- function(author_id, offset = 0, limit = 100, fields = NULL){
  stopifnot(!is.null(author_id), is.character(author_id),length(author_id) == 1)
  stopifnot(!is.null(offset), is.integer(as.integer(offset)))
  stopifnot(!is.null(limit), is.integer(as.integer(limit)))
  baseurl <- "https://api.semanticscholar.org/graph/v1/author/"
  fields <- match.arg(fields, get_fields()[["author_papers"]],several.ok = TRUE)
  url = paste0(baseurl,author_id,"/papers?fields=",paste0(fields, collapse = ","),"&limit=",limit,"&offset=",offset)
  res <- httr::GET(url = url)
  status(as.character(httr::status_code(res)))
  if (httr::status_code(res) == 200){
    cat("Downloading ....", sep = "\n")
    result <- jsonlite::fromJSON(url)
    cat(sprintf("starting position for this batch: %s", result[["offset"]]), sep = "\n")
    cat(sprintf("starting position of the next batch: %s", result[["next"]]), sep = "\n")
    cat("Done!!!", sep = "\n")
    if (length(result$data) > 0) {
      class(result$data) <- c("tbl_df", "tbl", "data.frame")
    }
    return(result)
  }
}
