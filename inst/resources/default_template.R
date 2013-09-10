
(function(page){
    links <- list(c("/index.html", "Home"),
                  c("/pages/about.html", "About"))
    blogroll <- list(c("http://www.r-bloggers.com/", "R bloggers"),
                     c("http://planet.clojure.in/", "Planet Clojure"),
                     c("http://planetbigdata.com/", "Planet Big Data"),
                     c("http://www.statsblogs.com/", "Statsblogs"))
    webdoc("html5",
           html_head(title = "Example Samatha blog",
                     '<meta charset="utf-8"><meta content="width=device-width, initiali-scale=1.0, user-scalable=yes" name="viewport">',
                     '<link href="/css/bootstrap.min.css" rel="stylesheet" type="text/css">',
                     '<link href="/css/bootstrap.min.css" rel="stylesheet" type="text/css">',
                     '<link href="/css/smartphone.css" media="only screen and (max-device-width:480px)" rel="stylesheet" type="text/css">'),
           html_body(
               m("div.container-fluid well",
                 m("h1", "Example Samatha blog"),
                 m("p", "A simple blog site to Illustrate Samatha")),
               m("div.subnav",
                 unordered.list(lapply(links, function(x) link.to(x[1], x[2])),
                                list.opts = list(class="nav nav-pills"))),
               m("div.container-fluid",
                 m("div.row-fluid",
                   m("div.span1"),
                   m("div.span9", page),
                   m("div.span2",
                     m("h3", "Tags"),
                     html.taglist(site),m("h3", "Blogs"),
                     unordered.list(lapply(blogroll, function(x) link.to(x[1], x[2]))))),
                 m("div.span12",
                   m("div.span2"),
                   m("div.span8",
                     m("footer.footer",
                       m("p.right back_to_top",
                         link.to("#", "&uArr; Page Top")),
                       m("p", link.to("https://github.com/DASpringate/samatha", "Built in R with Samatha"))))))))
})(page)