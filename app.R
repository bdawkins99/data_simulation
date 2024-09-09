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
#library(lme4)
library(dplyr)
library(ggplot2)
library(ggpubr)
#library(flextable)
#library(officer, here::here("data_simulation", "proj_libs"))
library(DT)
#library(captioner, here::here("data_simulation", "proj_libs"))
library(npdr)
library(tidyr)
library(GGally, here::here("data_simulation", "proj_libs"))

script_dir <- here::here("analysis", "scripts")
source(here::here(script_dir, "privateEC_sim-funcs.R"))
source(here::here(script_dir, "main-effect_plus_interaction-effect_continuous-features_simulation.R"))

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .dataTables_wrapper .dataTables_scrollBody {
        height: 500px !important;
        overflow-y: auto;
      }
    "))
  ),
  title = "Data Simulation Example",
  titlePanel(
    div(
      tags$h1("Simulating Datasets:", style = "font-size: 50px; font-weight: bold;"),
      tags$h1("Main Effects, Interaction Effects, and Gaussian Noise", style = "font-size: 50px;")
    )
  ),
  br(), br(), br(), 
  sidebarLayout(
    sidebarPanel(
      
      # Simulation Parameters
      numericInput("seed", "Enter a seed for reproducibility:", 1234, min = 1),
      
      numericInput("num.samples", "Number of samples in simulated dataset:", 100, max = 500),
      
      numericInput("num.variables", "Number of independent variables in simulated dataset:", 100, max = 500),
      
      sliderInput("pct.imbalance", "Proportion of samples in the 'positive' class:",
                  min = 0, max = 1, value = 0.5),
      
      sliderInput("pct.signals", "Proportion of features with significant statistical effects:",
                  min = 0, max = 1, value = 0.1),
      
      sliderInput("main.bias", "Effect size for simulated main effects:",
                   min = 0, max = 5.1, value = 0.8),
      
      sliderInput("interaction.bias", "Effect size for simulated interaction effects:",
                   min = 0, max = 1, value = 0.2),
      
      sliderInput("pct.mixed", "Proportion of functional features with interaction effects:", 
                  min = 0, max = 1, value = 0.5),
      
      textInput("label", "Specify the name of the outcome or dependent variable:",
                value = "class")
      
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Table", fluidRow(br(), br(), DTOutput('tbl'))),
                  tabPanel("Main Effects Plot", fluidRow(br(), br(), plotOutput("plotMain"))),
                  tabPanel("Interaction Effects Plot", fluidRow(br(), br(), plotOutput("plotInt"))))
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  sim_res <- reactive({
    sim_mixed_fn(input$seed,
                 input$num.samples,
                 input$num.variables,
                 input$pct.imbalance,
                 input$pct.signals,
                 input$main.bias,
                 input$interaction.bias,
                 input$pct.mixed,
                 input$label)
    })

    output$tbl <- renderDT(
      sim_res() |> 
        mutate(across(where(is.numeric), ~format(round(.x, digits = 3), nsmall = 3))),
      rownames     = FALSE,
      style        = "bootstrap",
      extensions   = "Buttons",
      options = list(
        dom          = "Bfrtip",
        buttons      = c(I("colvis"), "copy", "csv", "pdf"),
        initComplete = DT::JS(
          "function(settings, json){", "$(this.api().table().header()).css({
        'background-color': '#4067E2', 'color': '#fff'});", "}"
        ),
        pageLength = 20,
        scrollX = TRUE,
        scrollY = TRUE
      )
    )
    
    output$plotMain <- renderPlot({
      lab.sym <- rlang::sym(input$label)
      n.main <- round(input$num.variables * input$pct.signals * (1 - input$pct.mixed))
      plot_df <- sim_res() |> 
        select(all_of(c(paste0("simvar", 1:ifelse(n.main < 5, n.main, 5)), input$label))) |> 
        mutate(Outcome = factor(case_when(
          !!lab.sym == 1 ~ "Positive",
          TRUE ~ "Negative"
        ), levels = c("Positive", "Negative"))) |> 
        pivot_longer(cols = paste0("simvar", 1:ifelse(n.main < 5, n.main, 5)), 
                     values_to = "Center-scaled Feature Value", names_to = "Feature") |> 
        mutate(Feature = as.factor(Feature))
      
      ggplot(plot_df, aes(x     = Feature, 
                          y     = `Center-scaled Feature Value`, 
                          color = Outcome, 
                          fill  = Outcome, 
                          alpha = Outcome)) +
        geom_boxplot(color = "darkgray", alpha = 0.5) +
        geom_point(position = position_jitterdodge(seed = 1234), alpha = 0.7) +
        scale_color_manual(breaks = c("Positive", "Negative"), 
                           values = c("#7a0177", "#225ea8")) +
        scale_fill_manual(breaks = c("Positive", "Negative"), 
                          values = c("#7a0177", "#225ea8")) +
        stat_compare_means(aes(group = Outcome), method = "t.test", 
                           label.y = 1.1 * max(plot_df |> 
                                                 pull("Center-scaled Feature Value")),
                           size = 6) +
        theme_bw() + 
        ggtitle("Simulated Features With Main Effect") +
        theme(axis.title.x    = element_blank(),
              legend.position = "top",
              legend.text     = element_text(size = 14),
              legend.title    = element_text(size = 16, face = "bold"),
              axis.text       = element_text(size = 16),
              axis.title.y    = element_text(size = 18, color = "#252525"),
              plot.title      = element_text(size = 22, face = "bold", 
                                             color = "black", hjust = 0.5))
    }, height = 600)
    
    output$plotInt <- renderPlot({
      lab.sym <- rlang::sym(input$label)
      n.main <- round(input$num.variables * input$pct.signals * (1 - input$pct.mixed))
      n.int <- round(input$num.variables * input$pct.signals * input$pct.mixed)
      upperVal <- ifelse(n.int < 5, (n.main + 1 + n.int), (n.main + 1 + 4))
      
      plot_df <- sim_res() |> 
        select(all_of(c(paste0("simvar", (n.main + 1):upperVal), "class"))) |> 
        mutate(Outcome = factor(case_when(
          !!lab.sym == 1 ~ "Case",
          TRUE ~ "Control"
        ), levels = c("Case", "Control")))
      
      ggpairs(plot_df, aes(color = Outcome, fill = Outcome, alpha = 0.7),
              columns = 1:ifelse(n.int < 5, n.int, 5)) +
        discrete_scale(aesthetics = c("fill", "color"), 
                       palette = grDevices::colorRampPalette(c("#7a0177", "#225ea8"))) +
        xlab("Center-scaled Feature Value") +
        ylab("Center-scaled Feature Value") +
        ggtitle("Simulated Features With Interaction Effect") +
        theme_bw() +
        theme(strip.text = element_text(face = "bold", size = 14),
              axis.title = element_text(size = 18, color = "#252525"),
              plot.title      = element_text(size = 22, face = "bold", 
                                             color = "black", hjust = 0.5),
              axis.text       = element_text(size = 14))
    }, height = 800)
}

# Run the application 
shinyApp(ui = ui, server = server)
