layout <- "default_template.R"
title <- "home"

about.the.blog <- include.markdown(file.path(site, "template/resources/about_blog.md"))

page <- content(
    m("h1", "About this site"),
    m("div.row-fluid", 
      about.the.blog))

