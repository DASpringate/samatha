\name{ordered.list}
\alias{ordered.list}
\title{ordered list representation}
\usage{
  ordered.list(elements, list.opts = list(),
    element.opts = list())
}
\arguments{
  \item{elements}{list of elements matching the elements of
  the html ordered list}

  \item{list.opts}{list of optional attributes to be
  applied to the <ol> tag}

  \item{element.opts}{list of optional attributes to be
  applied to each <li> tag in the list}
}
\value{
  character
}
\description{
  Wraps a list of strings into an ordered list
}
\examples{
{
elements = list("apples", "oranges", "bananas")
ordered.list(elements)
}
}

