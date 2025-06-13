#launch shiny app
browseURL("http://127.0.0.1:1234")  # instead of launch.browser = TRUE

shiny::runApp("app", port = 1234, launch.browser = FALSE)
