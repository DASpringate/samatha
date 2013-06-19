\name{javascript.tag}
\alias{javascript.tag}
\title{Functions for creating HTML elements. These are all wrappers around the m() function
javascript tag representation}
\usage{
  javascript.tag(script)
}
\arguments{
  \item{script}{character string representation of
  javascript code}
}
\value{
  character
}
\description{
  Wraps the script string in script tags and a CDATA
  section
}
\examples{
javascript.tag("Some javascript")
}

