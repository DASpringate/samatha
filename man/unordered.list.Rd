\name{unordered.list}
\alias{unordered.list}
\title{unordered list representation}
\usage{
  unordered.list(elements, list.opts = list(),
    element.opts = list())
}
\arguments{
  \item{elements}{list of elements matching the elements of
  the html unordered list}

  \item{list.opts}{list of optional attributes to be
  applied to the <ul> tag}

  \item{element.opts}{list of optional attributes to be
  applied to each <li> tag in the list}
}
\value{
  character
}
\description{
  Wraps a list of strings into an unordered list
}
\examples{
{
elements = list("apples", "oranges", "bananas")
unordered.list(elements)
}
}

