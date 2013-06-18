\name{include.css}
\alias{include.css}
\title{CSS include representation}
\usage{
  include.css(stylesheets)
}
\arguments{
  \item{stylesheets}{a character vector of css files (all
  css files should be stored in site/basename(site)/css)}
}
\value{
  character vector of length 1
}
\description{
  Include a list of external stylesheet files
}
\examples{
include.css(c("mysheeet.css", "sheet2.css", "sheet3.css"))
}
\seealso{
  include.js
}

