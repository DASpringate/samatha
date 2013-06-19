\name{img.link}
\alias{img.link}
\title{image tag representaion}
\usage{
  img.link(uri, opts = list())
}
\arguments{
  \item{uri}{character link to the image}

  \item{opts}{list of optional tag attributes}
}
\value{
  character
}
\description{
  Builds an image element
}
\examples{
img.link("www.beautifulthings.com/12538675", opts = list(alt = "A lovely picture of something"))
}

