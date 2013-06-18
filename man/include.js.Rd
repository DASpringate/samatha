\name{include.js}
\alias{include.js}
\title{js include representation}
\usage{
  include.js(scripts)
}
\arguments{
  \item{scripts}{a character vector of js files (all js
  files should be stored in site/basename(site)/js)}
}
\value{
  character vector of length 1
}
\description{
  Include a list of external javascript files
}
\examples{
include.js(c("script1.js", "script2.js", "script3.js"))
}
\seealso{
  include.css
}

