\name{m}
\alias{m}
\title{Generic representaion of an HTML tag in R}
\usage{
  m(tag, ..., opts = list(),
    specials = list(id = "#", class = "\\."),
    escape.html.p = FALSE)
}
\description{
  Render arguments to a string of markup (HTML but also
  generic XML)
}
\examples{
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
}

