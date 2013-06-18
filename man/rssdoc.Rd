\name{rssdoc}
\alias{rssdoc}
\title{Representation of an RSS page in R}
\usage{
  rssdoc(title, link, description, categories, rssitems)
}
\arguments{
  \item{title}{title of the rss feed}

  \item{link}{domain name for the rss feed}

  \item{description}{decription of the rss feed}

  \item{categories}{character vector of categories for the
  whole feed}

  \item{rssitems}{list of items to be included in the feed}
}
\value{
  an object of class Samatha.Rssdoc, which is a character
  vector of length 1 representing an rss feed
}
\description{
  Representation of an RSS page in R
}

