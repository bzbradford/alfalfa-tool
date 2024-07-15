## Alfalfa Weather Tool ##
# Ben Bradford, UW-Madison



#- Renv ----

# renv::init()         # initiate renv if not already
# renv::dependencies() # show project dependencies
# renv::clean()        # remove unused packages
# renv::update()       # update project libraries
# renv::snapshot()     # save updated lock file to project
# renv::restore()      # restore versions from lockfile


#- Testing ----

# shiny::devmode(TRUE)
# shiny::devmode(FALSE)


#- Run ----

shiny::shinyApp(ui, server, enableBookmarking = "url")
