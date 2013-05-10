#' Samatha static site development server
#' Not currently working 

require(Rook)

s <- Rhttpd$new()
#s$start(quiet=TRUE)
s$start("localhost:8080")


## End(Not run)
s$add(name="Samatha",
      app=Builder$new(
          Static$new(
              urls = c("/css", "/img", "/js", "/resources"),
              root = "~/github/samatha/testsite/testsite/"),
          Brewery$new(url="/",root="~/github/samatha/testsite/testsite/"),
          Redirect$new("/index.html")))

s$add(name="test", app = File$new("~/github/samatha/testsite/testsite/"))



s$browse("Samatha") # Opens a browser window to the app.

s$remove(all=TRUE)


