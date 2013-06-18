\name{mail.to}
\alias{mail.to}
\title{mail to tag representaion}
\usage{
  mail.to(email, ..., subject = "", opts = list())
}
\value{
  character
}
\description{
  {Wraps content in html hyperlink with the supplied email
  address. If no content provided the email address is
  supplied as content}
}
\examples{
#mail.to("me.at.me.com")
#mail.to("me.at.me.com", "email me")
#mail.to("me.at.me.com", "email me", subject = "you are great")
}

