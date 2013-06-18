\name{render.rss}
\alias{render.rss}
\title{Builds an rss file with all posts in the site
Typically, the parameters would be population from a config.R file}
\usage{
  render.rss(site, domain, rss.title, rss.description,
    rss.categories)
}
\arguments{
  \item{site}{the path to the site}

  \item{domain}{domain name for the site}

  \item{rss.title}{title of the rss feed}

  \item{rss.description}{description of the feed}

  \item{rss.categories}{category tags for the whole feed}
}
\value{
  NULL writes to file as a side effect
}
\description{
  Builds an rss file with all posts in the site Typically,
  the parameters would be population from a config.R file
}
\seealso{
  rssdoc
}

