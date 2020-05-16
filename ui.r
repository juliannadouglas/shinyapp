library(shiny)

shinyUI(fluidPage(
  #User dropbox
  selectInput("state", "Choose state", choices=c("election2000",
                                                 "election2004",
                                                 "election2008",
                                                 "election2012",
                                                 "election2016"))
  #Print table to UI
  ,
  # tableOutput("table1")
  plotOutput("incidentPlot")
))