\name{skeleton}
\alias{skeleton}
\title{Sets up directory structure for a new Samatha site
2 main subdirectories: template (for source code for the site) and basename(site) for the html etc. of the site itself}
\usage{
  skeleton(site)
}
\arguments{
  \item{site}{path to the directory you want to set up}
}
\value{
  logical TRUE if successful, FALSE if directory already
  exists
}
\description{
  Sets up the directory structure for a new static site
}
\examples{
\dontrun{
mysite <- "/home/david/github/mysite"
skeleton(mysite)
}
}

