
(function(page){
    links <- list(c("/index.html", "Home"),
                  c("/about.html", "About"),
                  c("/research.html", "Research"),
                  c("/papers.html", "Papers"),
                  c("https://github.com/DASpringate", "Github"))
    webdoc("html5",
           head(
               m("title", "David Springate's blog"),
               m("div.container-fluid well",
                 m("h1", "What is this?"),
                 m("p", "David Springate's personal blog on programming, data, informatics, biostatistics and evolution")),
               m("div.subnav",
                 m("div.nav nav-pills",
                   unordered.list(lapply(links, function(x) link.to(x[1], x[2]))))),
               include.css(stylesheets = c("/css/bootstrap.min.css",
                                           "/css/smartphone.css"))),
           body(m("div.container-fluid",
                  m("div.row-fluid",
                    m("div.span1"),
                    m("div.span10", page),
                    m("div.span1")),
                  m("div.span12",
                    m("div.span2"),
                    m("div.span8",
                      m("footer.footer",
                        m("p.right back_to_top",
                          link.to("#", "&uArr; Page Top")),
                        m("p",
                          link.to("http://twitter.com/datajujitsu", "@datajujitsu"),
                          "&nbsp; 2013")))))))
})(page)