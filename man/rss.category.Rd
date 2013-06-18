\name{rss.category}
\alias{rss.category}
\title{Creates rss feeds for the given categories.
items are parsed from the main rss feed using xpath
New rss category feeds are placed in site/site/tags/CATEGORY_NAME.xml}
\usage{
  rss.category(site, domain, categories)
}
\arguments{
  \item{site}{character - Full path to your Samatha site}

  \item{domain}{character - The domain name of your site}

  \item{categories}{- character vector of the categories
  you want to produce seperate rss feeds for}
}
\description{
  Creates rss feeds for the given categories. items are
  parsed from the main rss feed using xpath New rss
  category feeds are placed in
  site/site/tags/CATEGORY_NAME.xml
}

