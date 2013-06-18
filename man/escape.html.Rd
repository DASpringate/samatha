\name{escape.html}
\alias{escape.html}
\title{Escapes html tags}
\usage{
  escape.html(s)
}
\arguments{
  \item{s}{a character string representing an html tag}
}
\value{
  character sting representing an escaped html tag
}
\description{
  Escapes html by converting &,<,> and quotes to inactive
  forms
}
\examples{
escape.html("<p><script>Do something evil</script></p>")
}

