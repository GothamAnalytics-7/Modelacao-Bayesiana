library(shiny)
library(blavaan)
library(lavaan)
library(rstan)
library(dplyr)
library(brms)
library(psych)
library(bayesplot)

# Lista de variáveis por construto
construtos_vars <- list(
  "Student Ethics" = paste0("ET", 1:13),
  "Motivation" = paste0("Mot", 1:15),
  "Self-Efficacy" = paste0("SE", 1:6),
  "Resilience" = paste0("R", 1:6),
  "Knowledge Articulation" = paste0("KA", 1:5),
  "Team Strain" = paste0("TS", 1:17),
  "Cooperative Classroom Environment" = paste0("CCE", 1:11)
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .custom-btn {
        background-color: #007BFF !important;
        color: white !important;
        font-weight: bold;
        border-radius: 6px;
      }
      iframe {
        width: 100%;
        height: 400px;
        border: 1px solid #ccc;
        border-radius: 8px;
        padding: 10px;
      }
    "))
  ),
  
  titlePanel("Part 1: CFA Bayesiana"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("construto", "Escolher o Construto:",
                  choices = names(construtos_vars)),
      
      uiOutput("varSelectUI"),
      
      checkboxInput("std_lv", "Padronizar variáveis latentes (std.lv)", TRUE),
      checkboxInput("use_stan", "Usar Stan backend", TRUE),
      sliderInput("n_chains", "Número de Cadeias (n.chains)", min = 1, max = 4, value = 3),
      sliderInput("burn_in", "Burn-in", min = 100, max = 5000, step = 100, value = 1000),
      sliderInput("sample_estimate", "Nº Amostras", min = 500, max = 5000, step = 100, value = 2000),
      
      actionButton("runModel", "Executar o Modelo", class = "custom-btn")
    ),
    
    mainPanel(
      uiOutput("popup"),
      tags$h4("Resumo do Modelo:"),
      tags$div(style = "border: 1px solid #ccc; padding: 10px; border-radius: 8px; background-color: #f9f9f9;",
               verbatimTextOutput("modelSummary")),
      tags$h4("Fit Measures:"),
      tags$div(style = "border: 1px solid #ccc; padding: 10px; border-radius: 8px; background-color: #f9f9f9;",
               verbatimTextOutput("fitMeasures"))
    )
  )
)

server <- function(input, output, session) {
  # Certifique-se que df está carregado
  data(df) # ou carregue fora, dependendo do seu ambiente
  
  output$varSelectUI <- renderUI({
    vars <- construtos_vars[[input$construto]]
    selectizeInput("selectedVars", "Selecionar as variáveis do construto:",
                   choices = vars, multiple = TRUE, selected = vars)
  })
  
  observeEvent(input$runModel, {
    showModal(modalDialog(
      title = "A executar o modelo...",
      "O modelo está a ser corrido. Isto pode levar alguns instantes.",
      footer = NULL,
      easyClose = TRUE
    ))
  })
  
  modelResult <- eventReactive(input$runModel, {
    req(input$selectedVars)
    
    model_str <- paste0(input$construto, " =~ ", paste(input$selectedVars, collapse = " + "))
    
    fit <- bcfa(model_str, data = df,
                std.lv = input$std_lv,
                n.chains = input$n_chains,
                burnin = input$burn_in,
                sample = input$sample_estimate,
                target = ifelse(input$use_stan, "stan", "jags"))
    
    removeModal()  # Fechar popup após execução
    
    return(fit)
  })
  
  output$modelSummary <- renderPrint({
    req(modelResult())
    summary(modelResult(), standardized = TRUE, rsquare = TRUE)
  })
  
  output$fitMeasures <- renderPrint({
    req(modelResult())
    fitMeasures(modelResult())
  })
}

shinyApp(ui = ui, server = server)
