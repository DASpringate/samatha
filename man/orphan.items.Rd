\name{orphan.items}
\alias{orphan.items}
\title{find orphaned pages and posts}
\usage{
  orphan.items(dest.items, source.items)
}
\arguments{
  \item{dest.items}{a character vector of things in the
  destination directory}

  \item{source.items}{a character vector of things in the
  source directory}
}
\value{
  logical vector of things in dest.items that are not in
  source.items
}
\description{
  This function looks for pages and posts that don't have
  any associated source, and therefore need to be deleted
}

