\name{html}
\alias{html}
\title{samatha}
\usage{
  html(tag, ..., opts = list(),
    specials = list(id = "#", class = "\\."),
    escape.html.p = FALSE)
}
\description{
  Render arguments to a string of HTML.
}
\examples{
html("p")
# Any strings after that become the content of the tag
html("p", "This is a paragraph")
# Tags can be nested inside of tags and everything ends up concatenated
html("p", "Goodbye", html("strong", "cruel"), "world")
# You can specify attributes by supplying a list after the content
html("span",  "bar", opts = list(class = "foo"))
#' There are CSS-style shortcuts for setting ID and class
html("p#my-p", html("span.pretty", "hey"))
html("span", opts = list(id = "foo", class = "bar"), "baz")
# You can escape a string using the (escape-html) function
html("p", html("script", "Do something evil", escape.htmp.p = TRUE))
}

