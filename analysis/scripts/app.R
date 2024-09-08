#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(here)
#library(privateEC, here::here("data_simulation", "proj_libs"))
library(lme4, here::here("data_simulation", "proj_libs"))
library(dplyr, here::here("data_simulation", "proj_libs"))
library(ggplot2, here::here("data_simulation", "proj_libs"))
library(flextable, here::here("data_simulation", "proj_libs"))
library(officer, here::here("data_simulation", "proj_libs"))
library(DT, here::here("data_simulation", "proj_libs"))
library(captioner, here::here("data_simulation", "proj_libs"))
library(npdr, here::here("data_simulation", "proj_libs"))
library(tidyr, here::here("data_simulation", "proj_libs"))
library(GGally, here::here("data_simulation", "proj_libs"))

script_dir <- here::here("analysis", "scripts")
source(here::here(script_dir, "privateEC_sim-funcs.R"))
source(here::here(script_dir, "main-effect_plus_interaction-effect_continuous-features_simulation.R"))

# Define UI for application that draws a histogram
ui <- fluidPage(
  title = "Data Simulation Example",
  titlePanel("Simulated Dataset: Main Effects, Interaction Effects, and Gaussian Noise"),
  sidebarLayout(
    sidebarPanel(
      
      # Simulation Parameters
      numericInput("num.samples", "Number of samples in simulated dataset:", 100, max = 500),
      
      numericInput("num.variables", "Number of independent variables in simulated dataset:", 100, max = 500),
      
      sliderInput("pct.imbalance", "Proportion of samples in the 'positive' class:",
                  min = 0, max = 1, value = 0.5),
      
      sliderInput("pct.signals", "Proportion of features with significant statistical effects:",
                  min = 0, max = 1, value = 0.1),
      
      sliderInput("main.bias", "Effect size for simulated main effects:",
                   min = 0, max = 5, value = 0.8),
      
      sliderInput("interaction.bias", "Effect size for simulated interaction effects:",
                   min = 0, max = 1, value = 0.2),
      
      sliderInput("pct.mixed", "Proportion of functional features with interaction effects:", 
                  min = 0, max = 1, value = 0.5),
      
      sliderInput("pct.train", "Proportion of samples in training dataset:",
                  min = 0, max = 1, value = 0.5),
      
      sliderInput("pct.holdout", "Proportion of samples in holdout dataset:",
                  min = 0, max = 1, value = 0.5),
      
      sliderInput("pct.validation", "Proportion of samples in validation dataset:",
                  min = 0, max = 1, value = 0),
      
      textInput("label", "Specify the name of the outcome or dependent variable:",
                value = "class"),
      
      actionButton("update", "Update View")
      
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Table", fluidRow(DTOutput('tbl'))))
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$tbl <- renderDT(
      sim_mixed_fn(input$num.samples,
                   input$num.variables,
                   input$pct.imbalance,
                   input$pct.signals,
                   input$main.bias,
                   input$interaction.bias,
                   input$pct.mixed,
                   input$pct.train,
                   input$pct.holdout,
                   input$pct.validation,
                   input$label) |> 
        mutate(across(where(is.numeric), ~format(round(.x, digits = 3), nsmall = 3))),
      options = list(
        dom          = "Bfrtip",
        buttons      = c(I("colvis"), "copy", "csv", "pdf"),
        initComplete = DT::JS(
          "function(settings, json){", "$(this.api().table().header()).css({
        'background-color': '#4067E2', 'color': '#fff'});", "}"
        ),
        pageLength = 10,
        scrollX = TRUE
      )
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
