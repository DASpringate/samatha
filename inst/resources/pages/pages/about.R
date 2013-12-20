about.the.blog <- include.markdown(file.path(site, "template/resources/about_blog.md"))

page <- make.samatha.page(content = content(m("h1", "About this site"),
                                            m("div.row-fluid",
                                              about.the.blog)),
                          title = "about",
                          layout = "default_template.R")

