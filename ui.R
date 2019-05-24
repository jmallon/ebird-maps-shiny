
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggvis)

EBD_USA <- read.csv("data/me_crime.csv", stringsAsFactors=FALSE)
EBD_WORLD <- read.csv("data/me_crime.csv", stringsAsFactors=FALSE)

shinyUI(fluidPage(
  titlePanel("ggvis shiny maps"),
  sidebarLayout(
    sidebarPanel(
      p(strong("NOTE: "), "This can take a few seconds to startup. Even after that, there may be some delay with more complex maps due to how ggvis renders them."),
      hr(),
      p(),
      hr(),
      p("Written by ", a(href="http://terpconnect.com/~jmmallon", "@jmallon")),
      p("Shiny App sourced from ", a(href="https://github.com/hrbrmstr/ggvis-maps", "github")),
      width=3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("US Target Counties", wellPanel(p("Highlights the counties where the most birds are missing from your life list", code("scale_"))), ggvisOutput("drought")),
        tabPanel("World Target Countries", wellPanel(p("Highlights the top 5 countries where the most birds are missing from your life list")) ,ggvisOutput("launch"))
      ),
      width=9
    )
  )
))
