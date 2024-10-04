# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
# options(shiny.port = httpuv::randomPort())

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
options(shiny.launch.browser = .rs.invokeShinyWindowExternal)

App()


# profvis::profvis({
#   shiny::runApp(shinyApp(
#     ui = app_ui,
#     server = app_server
#   ))
# }, prof_output = getwd())
#
# profvis::profvis(prof_input = 'file7aac21c238e2.Rprof')


# slick <- readRDS('../nswo-mse/nswo.slick')
