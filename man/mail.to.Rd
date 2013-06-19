\name{mail.to}
\alias{mail.to}
\title{mail to tag representaion}
\usage{
  mail.to(email, ..., subject = "", opts = list())
}
\arguments{
  \item{email}{character an email address}

  \item{subject}{character subject heading for the email}

  \item{\dots}{character vector The body of the email}

  \item{opts}{list of optional tag attributes}
}
\value{
  character
}
\description{
  { Wraps content in html hyperlink with the supplied email
  address. If no content provided the email address is
  supplied as content }
}
\examples{
\dontrun{
# can't use ampersand in roxygen2 comments!
mail.to("me.at.me.com")
mail.to("me.at.me.com", "email me")
mail.to("me.at.me.com", "email me", subject = "you are great")
}
}

