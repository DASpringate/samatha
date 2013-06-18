\name{webdoc}
\alias{webdoc}
\title{Representation of a webpage in R}
\usage{
  webdoc(doctype, ...)
}
\arguments{
  \item{doctype}{a doctype header, e.g. one of the elements
  of the list doctype}

  \item{\dots}{the contents of the webpage, typically a
  call to html_head() and a call to html_body()}
}
\value{
  an object of class Samatha.Webdoc which is a character
  vector of length 1
}
\description{
  Build a web document with the appropriate doctype
}
\examples{
{
elements = list("apples", "oranges", "bananas")
webdoc("html5",html_head("My first page"),html_body("Hello world", unordered.list(elements)))
}
}

