library(shiny)
library(blavaan)
library(lavaan)
library(rstan)
library(dplyr)
library(brms)
library(psych)
library(bayesplot)
library(semPlot)

options(blavaan.dir = "temp_bcfa")
dir.create("temp_bcfa", showWarnings = FALSE)

construtos_vars <- list(
  "Student Ethics" = paste0("ET", 1:13),
  "Motivation" = paste0("Mot", 1:15),
  "Self-Efficacy" = paste0("SE", 1:6),
  "Resilience" = paste0("R", 1:6),
  "Knowledge Articulation" = paste0("KA", 1:5),
  "Team Strain" = paste0("TS", 1:17),
  "Cooperative Classroom Environment" = paste0("CCE", 1:20)
)

default_vars <- list(
  "Student Ethics" = c("ET12", "ET13"),
  "Motivation" = c("Mot5", "Mot8", "Mot11"),
  "Self-Efficacy" = c("SE1", "SE2", "SE3", "SE4", "SE5", "SE6"),
  "Resilience" = c("R2", "R5", "R6"),
  "Knowledge Articulation" = c("KA1", "KA2", "KA3", "KA4", "KA5"),
  "Team Strain" = c("TS10", "TS11", "TS12", "TS13", "TS14", "TS15", "TS16", "TS17"),
  "Cooperative Classroom Environment" = c("CCE1", "CCE3", "CCE4", "CCE5", "CCE8", "CCE9", "CCE10", "CCE11")
)

plot_sem_model <- function(model, title = "") {
  semPaths(model,
           what = "std",  # Usar as soluções padronizadas
           layout = "spring",  # Layout para as variáveis
           edge.label.cex = 0.6,  # Ajustar tamanho do rótulo das arestas
           sizeMan = 3,  # Tamanho das variáveis manifestas
           sizeLat = 5,  # Tamanho das variáveis latentes
           nCharNodes = 4,  # Limitar o número de caracteres nos nós
           residuals = TRUE,  # Incluir resíduos
           intercepts = FALSE,  # Excluir interceptos
           optimizeLatRes = TRUE,  # Otimizar a resolução das variáveis latentes
           edge.color = "darkgreen",  # Cor das arestas
           color = list(lat = "skyblue", man = "white"),  # Cor dos nós latentes e manifestos
           node.width = 2,  # Largura dos nós
           mar = c(8, 8, 8, 8))  # Margens para o gráfico
}

ui <- navbarPage(
  title = "Data on higher education student ethics model",
  theme = shinythemes::shinytheme("flatly"),
  
  navbarMenu(
    "Part 1: EDA",
    tabPanel(
      "Subaba 1",
      sidebarLayout(
        sidebarPanel(
          tags$h4("Configurações futuras"),
          selectInput("dummyConstruto", "Selecionar Construto:",
                      choices = names(construtos_vars),
                      selected = names(construtos_vars)[1])
        ),
        mainPanel(
          p("Conteúdo futuro aqui...")
        )
      )
    ),
    tabPanel("Subaba 2", p("Mais conteúdo futuro"))
  ),
  
  # Primeiro menu com a aba CFA Bayesiana
  navbarMenu(
    "Part 2: Modelos",
    tabPanel(
      "CFA Bayesiana Separada",
      sidebarLayout(
        sidebarPanel(
          selectInput("construto", "Escolher o Construto:",
                      choices = names(construtos_vars)),
          
          uiOutput("varSelectUI"),
          
          checkboxInput("std_lv", "Padronizar variáveis latentes (std.lv)", TRUE),
          checkboxInput("use_stan", "Usar Stan backend", TRUE),
          sliderInput("n_chains", "Número de Cadeias (n.chains)", min = 3, max = 5, value = 3),
          sliderInput("burn_in", "Burn-in", min = 100, max = 5000, step = 100, value = 500),
          sliderInput("sample_estimate", "Nº Amostras", min = 500, max = 5000, step = 100, value = 1000),
          
          actionButton("runModel", "Executar o Modelo", class = "custom-btn")
        ),
        
        mainPanel(
          uiOutput("popup"),
          tags$h4("Modelo SEM:"),
          plotOutput("semPlot"),
          tags$h4("Resumo do Modelo:"),
          tags$div(style = "border: 1px solid #ccc; padding: 10px; border-radius: 8px; background-color: #f9f9f9;",
                   verbatimTextOutput("modelSummary")),
          tags$h4("Fit Measures:"),
          tags$div(style = "border: 1px solid #ccc; padding: 10px; border-radius: 8px; background-color: #f9f9f9;",
                   verbatimTextOutput("fitMeasures"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  options(blavaan.dir = "bcfa_tmp")
  dir.create("bcfa_tmp", showWarnings = FALSE)
  rstan_options(auto_write = TRUE)
  options(mc.cores = 1)
  
  df <- read.csv("../data/4_conjuntos(dev)/vars_artigo.csv")
  
  output$varSelectUI <- renderUI({
    vars <- construtos_vars[[input$construto]]
    
    selected_vars <- default_vars[[input$construto]]
    selectizeInput("selectedVars", "Selecionar as variáveis do construto:",
                   choices = vars, 
                   multiple = TRUE, 
                   selected = selected_vars)
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
    
    removeModal() 
    
    return(fit)
  })
  
  output$semPlot <- renderPlot({
    req(modelResult())
    
    plot_sem_model(modelResult(), title = "Modelo CFA Bayesiano")
  })
  
  output$modelSummary <- renderPrint({
    req(modelResult())
    
    summary(modelResult())
  })
  
  output$fitMeasures <- renderPrint({
    req(modelResult())
    fitMeasures(modelResult())
  })
}

shinyApp(ui = ui, server = server)
