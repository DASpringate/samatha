\name{m}
\alias{m}
\title{Generic representaion of an HTML tag in R}
\usage{
  m(tag, ..., opts = list(),
    specials = list(id = "#", class = "\\."),
    escape.html.p = FALSE, singleton = FALSE)
}
\arguments{
  \item{tag}{character The name of the html/xml tag to be
  used}

  \item{\dots}{the content of the tag. Eventually coerced
  into character vector. Can be list or function}

  \item{opts}{list of key=values for tag attributes}

  \item{{}{specials list of shortcuts for setting ID and
  class attributes. Should be familiar to css users.
  Potentially extendable to to other attributes... }}

  \item{excape.html.p}{logical predicate. Should all html
  be escaped? calls escape.html() on the final character
  output if TRUE}

  \item{singleton}{logical predicate. If TRUE the tag is of
  form <tag body>}
}
\value{
  a character vector of length 1
}
\description{
  Render arguments to a string of markup (HTML but also
  generic XML)
}
\examples{
{
m("p")
m("div")
# Any strings after that become the content of the tag
m("p", "This is a paragraph")
# Tags can be nested inside of tags and everything ends up concatenated
m("p", "Goodbye", m("strong", "cruel"), "world")
# You can specify attributes by supplying a list after the content
m("span",  "bar", opts = list(class = "foo"))
#' There are CSS-style shortcuts for setting ID and class
m("p#my-p", m("span.pretty", "hey"))
m("span", opts = list(id = "foo", class = "bar"), "baz")
# You can escape a string using the (escape-html) function
m("p", m("script", "Do something evil", escape.html.p = TRUE))
# Also caters for singleton tags:
m("meta", opts = list(charset = "utf-8"), singleton = TRUE)
}
}

