library(shiny)
library(shinyjs)
library(DT)
library(plotly)

htmlTemplate("template.html",
    effect.plot = plotOutput("effect.plot", height = "70px"),
    effect = textOutput("effect", inline = TRUE),
    lower.or.higher = textOutput("lower.or.higher", inline = TRUE),
    effect.ci = textOutput("effect.ci", inline = TRUE),
    significance = textOutput("significance", inline = TRUE)
)

