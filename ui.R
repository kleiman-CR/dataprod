library(shiny)

goods <- read.csv ("goods.csv", header = FALSE,
                   col.names = c ("id", "name"),
                   colClasses = c ("numeric", "character"))

shinyUI(pageWithSidebar(
  headerPanel("Goods prices in Rival Regions"),
  sidebarPanel(
    h3('About'),
    p('Rival Regions, a Russian strategy browser game, features a player-driven market of ingame rescources and goods. This app displays a summary of median price and its 0.25 and 0.75 quantiles for the selected good:'),
    selectInput("good", 
                label = "Choose a good",
                choices = goods$name,
                selected = goods$name[1])
  ),
  mainPanel(
    h3('Good prices for last 30 days'),
    plotOutput ('plot')
  )
))