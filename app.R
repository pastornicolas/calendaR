library(shiny)
library(ggplot2)
library(ggforce)
library(geomtextpath)
library(DT)

# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

source("./R/funciones.R")
source("./R/modcats.R")
source("./R/ui.R")
source("./R/server.R")

shinyApp(
  ui = ui,
  server = server
)
