library(shiny)
library(ggplot2)
library(ggforce)
library(geomtextpath)
library(DT)


source("./R/funciones.R")
source("./R/modcats.R")
source("./R/ui.R")
source("./R/server.R")

shinyApp(
  ui = ui,
  server = server
)
