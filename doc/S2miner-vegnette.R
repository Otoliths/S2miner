## -----------------------------------------------------------------------------
library("S2miner")

## -----------------------------------------------------------------------------
# Supports AND and OR and inclusion and exclusion of terms, for example "title=bibliometrics" or "+Epidemic+Modeling+Canada-COVID".See blog post https://medium.com/ai2-blog/building-a-better-search-engine-for-semantic-scholar-ea23a0b661e7 for a description of our search relevance algorithm.
res1 <- search_papers(keyword = "bibliometrics",fields = c("title","abstract","authors"))

res1[["data"]]

## -----------------------------------------------------------------------------
# This function fetch a paper data from the Semantic Scholar corpus.
# Supported paper identifier services: Semantic Scholar ID, Semantic Scholar numerical ID, Digital Object Identifier, arXiv.rg, Microsoft Academic Graph, Association for Computational Linguistics, PubMed/Medline, PubMed Central and URL from one of the sites listed below. See Format for detalis.
res2 <- lookup_paper(paper_id = "649def34f8be52c8b66281af98ae884c09aef38b")

# get abstract
print(res2[["abstract"]])

# Too Long; Didn't Read.Artificial Intelligence summarizes academic articles for you.
print(res2[["tldr"]][["text"]])

## -----------------------------------------------------------------------------
# This function fetch an author data from the Semantic Scholar corpus.
res3 <- lookup_author(author_id = "48980001")

# Show author profile plot
res3$plot

## -----------------------------------------------------------------------------
# Traversing the literature graph of an author's papers
res4 <- traverse_author(author_id = "48980001",fields = c("title","abstract","authors","year"))
res4[["data"]]

# Traversing the literature graph of a paper's informatio
res5 <- traverse_paper(paper_id = "649def34f8be52c8b66281af98ae884c09aef38b")
res5[["data"]]


## -----------------------------------------------------------------------------

browse_cp('9397e7acd062245d37350f5c05faf56e9cfae0d6',"s2")


