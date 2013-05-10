\name{webdoc}
\alias{webdoc}
\title{Representation of a webpage in R}
\usage{
  webdoc(doctype, ...)
}
\description{
  Build a web document with the appropriate doctype
}
\examples{
elements = list("apples", "oranges", "bananas")
webdoc("html5",html_head("My first page"),html_body("Hello world"), unordered.list(elements))
}

