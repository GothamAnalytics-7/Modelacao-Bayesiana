library(shiny)
library(blavaan)
library(lavaan)
library(rstan)
library(dplyr)
library(brms)
library(psych)
library(bayesplot)
library(semPlot)
library(shinythemes)
library(ggplot2)
library(corrplot)
library(DT)
library(fitdistrplus)

df <- read.csv("../data/4_conjuntos(dev)/vars_artigo.csv")

options(blavaan.dir = "temp_bcfa")
dir.create("temp_bcfa", showWarnings = FALSE)

construtos_vars <- list(
  "Student Ethics" = paste0("ET", 1:13),
  "Motivation" = paste0("Mot", 1:15),
  "Self Efficacy" = paste0("SE", 1:6),
  "Resilience" = paste0("R", 1:6),
  "Knowledge Articulation" = paste0("KA", 1:5),
  "Team Strain" = paste0("TS", 1:17),
  "Cooperative Classroom Environment" = paste0("CCE", 1:20)
)

default_vars <- list(
  "Student Ethics" = c("ET12", "ET13"),
  "Motivation" = c("Mot5", "Mot8", "Mot11"),
  "Self Efficacy" = c("SE1", "SE2", "SE3", "SE4", "SE5", "SE6"),
  "Resilience" = c("R2", "R5", "R6"),
  "Knowledge Articulation" = c("KA1", "KA2", "KA3", "KA4", "KA5"),
  "Team Strain" = c("TS10", "TS11", "TS12", "TS13", "TS14", "TS15", "TS16", "TS17"),
  "Cooperative Classroom Environment" = c("CCE1", "CCE3", "CCE4", "CCE5", "CCE8", "CCE9", "CCE10", "CCE11")
)

run_item_means <- function(df, construtos_lista) {
  df_resultado <- data.frame(matrix(nrow = nrow(df), ncol = 0))
  for (nome_construto in names(construtos_lista)) {
    itens <- construtos_lista[[nome_construto]]
    media_itens <- rowMeans(df[, itens, drop = FALSE], na.rm = TRUE)
    df_resultado[[paste0("mean_", nome_construto)]] <- media_itens
  }
  return(df_resultado)
}

run_CFA_extract_scores <- function(df, construtos_lista) {
  df_resultado <- data.frame(matrix(nrow = nrow(df), ncol = 0))
  modelos <- list()
  
  for (nome_construto in names(construtos_lista)) {
    itens <- construtos_lista[[nome_construto]]
    formula_cfa <- paste0(nome_construto, " =~ ", paste(itens, collapse = " + "))
    
    modelo <- lavaan::cfa(formula_cfa, data = df, std.lv = TRUE)
    modelos[[nome_construto]] <- modelo
    
    scores <- lavPredict(modelo)
    if (is.vector(scores)) {
      df_resultado[[paste0("score_", nome_construto)]] <- scores
    } else if (is.matrix(scores)) {
      df_resultado[[paste0("score_", nome_construto)]] <- scores[, 1]
    }
  }
  
  return(list(scores_df = df_resultado, modelos = modelos))
}

plot_sem_model <- function(model, title = "") {
  semPaths(model,
           what = "std",
           layout = "spring",
           edge.label.cex = 0.6,
           sizeMan = 3,
           sizeLat = 5,
           nCharNodes = 4,
           residuals = TRUE,
           intercepts = FALSE,
           optimizeLatRes = TRUE,
           edge.color = "darkgreen",
           color = list(lat = "skyblue", man = "white"),
           node.width = 2,
           mar = c(8, 8, 8, 8))
}

ui <- navbarPage(
  title = "Data on higher education student ethics model",
  theme = shinythemes::shinytheme("flatly"),
  
  navbarMenu(
    "Part 1: EDA",
    tabPanel(
      "Dados",
      sidebarLayout(
        sidebarPanel(
          tags$h4("Visualizar Distribuição de Variável"),
          selectInput("selectedVarDist", "Selecionar Variável:",
                      choices = names(df),
                      selected = names(df)[1])
        ),
        mainPanel(
          tags$h4("Distribuição da Variável Selecionada:"),
          plotOutput("varDistPlot"),
          verbatimTextOutput("bestFitDist"),
          tags$h4("Tabela com os dados do artigo (questionário):"),
          DT::dataTableOutput("fullDataTable"),
        )
      )
    ),
    tabPanel(
      "Gráficos",
      sidebarLayout(
        sidebarPanel(
          tags$h4("Escolha do construto"),
          selectInput("edaConstruto", "Selecionar Construto:",
                      choices = names(construtos_vars),
                      selected = names(construtos_vars)[1]),
          uiOutput("edaVarSelectUI")
        ),
        mainPanel(
          tags$h4("Matriz de Correlação"),
          plotOutput("correlationPlot"),
          tags$h4("Métricas Psicométricas"),
          verbatimTextOutput("constructMetrics")
        )
      )
    )
  ),
  
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
    ),
    
    # NOVA ABA ADICIONADA AQUI
    tabPanel(
      "SEM c/ CFA ou Média por Construto antes",
      sidebarLayout(
        sidebarPanel(
          radioButtons("method_choice", "Escolher método:", choices = c("Média" = "media", "CFA" = "cfa")),
          uiOutput("multiConstrutoVarSelectUI"),
          actionButton("runScoreCalc", "Executar Cálculo")
        ),
        mainPanel(
          tags$h4("Resultados:"),
          DT::dataTableOutput("scoreTable")
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
  # ----- ABA DADOS -----
  
  output$varDistPlot <- renderPlot({
    req(input$selectedVarDist)
    var_data <- df[[input$selectedVarDist]]
    var_data <- as.numeric(as.character(var_data))
    var_data <- var_data[!is.na(var_data)]
    
    fit_norm <- fitdistrplus::fitdist(var_data, "norm")
    fit_lognorm <- tryCatch(fitdistrplus::fitdist(var_data, "lnorm"), error = function(e) NULL)
    fit_gamma <- tryCatch(fitdistrplus::fitdist(var_data, "gamma"), error = function(e) NULL)
    fit_exp <- tryCatch(fitdistrplus::fitdist(var_data, "exp"), error = function(e) NULL)
    
    fits <- list(norm = fit_norm, lnorm = fit_lognorm, gamma = fit_gamma, exp = fit_exp)
    aic_vals <- sapply(fits, function(f) if (!is.null(f)) f$aic else Inf)
    best_dist <- names(which.min(aic_vals))
    best_fit <- fits[[best_dist]]
    
    x_vals <- seq(min(var_data), max(var_data), length.out = 500)
    
    dist_curve <- switch(best_dist,
                         norm = dnorm(x_vals, mean = best_fit$estimate["mean"], sd = best_fit$estimate["sd"]),
                         lnorm = dlnorm(x_vals, meanlog = best_fit$estimate["meanlog"], sdlog = best_fit$estimate["sdlog"]),
                         gamma = dgamma(x_vals, shape = best_fit$estimate["shape"], rate = best_fit$estimate["rate"]),
                         exp = dexp(x_vals, rate = best_fit$estimate["rate"]),
                         rep(NA, length(x_vals)) # fallback!!!!
    )

    dist_df <- data.frame(x = x_vals, y = dist_curve, type = "Melhor ajuste")
    
    p <- ggplot(data.frame(x = var_data), aes(x = x)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
      geom_density(aes(y = ..density.., color = "Densidade empírica"), size = 1) +
      geom_line(data = dist_df, aes(x = x, y = y, color = type), size = 1.2) +
      scale_color_manual(name = "Curvas",
                         values = c("Densidade empírica" = "darkblue", "Melhor ajuste" = "red")) +
      labs(title = paste("Distribuição de:", input$selectedVarDist),
           x = input$selectedVarDist,
           y = "Densidade") +
      theme_minimal() +
      theme(legend.position ="top",legend.text =element_text(size = 14),legend.title =element_text(size = 16))
    
    p
  })
  
  
  output$bestFitDist <- renderPrint({
    req(input$selectedVarDist)
    var_data <- df[[input$selectedVarDist]]
    var_data <- as.numeric(as.character(var_data))
    var_data <- var_data[!is.na(var_data)]
    

    fit_norm <- fitdistrplus::fitdist(var_data, "norm")
    fit_lognorm <- tryCatch(fitdistrplus::fitdist(var_data, "lnorm"), error = function(e) NULL)
    fit_gamma <- tryCatch(fitdistrplus::fitdist(var_data, "gamma"), error = function(e) NULL)
    fit_exp <- tryCatch(fitdistrplus::fitdist(var_data, "exp"), error = function(e) NULL)
    
    fits <- list(norm = fit_norm, lnorm = fit_lognorm, gamma = fit_gamma, exp = fit_exp)
    aic_vals <- sapply(fits, function(f) if (!is.null(f)) f$aic else Inf)
    best_dist <- names(which.min(aic_vals))
    
    cat("Melhor distribuição (menor AIC):", best_dist, "\n")
    print(fits[[best_dist]])
  })
  
  output$fullDataTable <- DT::renderDataTable({
    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ----- ABA EDA -----
  output$edaVarSelectUI <- renderUI({
    vars <- construtos_vars[[input$edaConstruto]]
    selected <- default_vars[[input$edaConstruto]]
    selectizeInput("edaSelectedVars", "Selecionar Variáveis:",
                   choices = vars, selected = selected, multiple = TRUE)
  })
  
  eda_filtered_data <- reactive({
    req(input$edaSelectedVars)
    df %>% 
      dplyr::select(dplyr::any_of(input$edaSelectedVars)) %>%
      mutate(across(everything(), ~ as.numeric(as.character(.))))
  })
  
  output$correlationPlot <- renderPlot({
    data <- eda_filtered_data()
    req(ncol(data) >= 2)
    
    corr_matrix <- cor(data, use = "pairwise.complete.obs")
    
    corrplot::corrplot(corr_matrix,
                       method = "ellipse",
                       type = "upper",
                       tl.col = "black",
                       tl.cex = 0.8,
                       addCoef.col = "black",
                       number.cex = 1.0,
                       mar = c(1, 1, 2, 1),
                       title = paste("Matriz de Correlação -", input$edaConstruto))
  })
  
  output$constructMetrics <- renderPrint({
    data <- eda_filtered_data()
    req(ncol(data) >= 2)
    
    alpha_result <- psych::alpha(data)
    cronbach_alpha <- alpha_result$total$raw_alpha
    
    fa_result <- fa(data, nfactors = 1, rotate = "none")
    loadings <- fa_result$loadings[, 1]
    
    cat("Alpha de Cronbach: ", round(cronbach_alpha, 2), "\n")
    cat("Cargas Fatoriais:\n")
    print(round(loadings, 2))
  })
  
  # ----- ABA CFA BAYESIANA -----
  output$varSelectUI <- renderUI({
    vars <- construtos_vars[[input$construto]]
    selected_vars <- default_vars[[input$construto]]
    selectizeInput("selectedVars", "Selecionar as variáveis do construto:",
                   choices = vars, multiple = TRUE, selected = selected_vars)
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
  
  # ----- NOVA ABA: Média ou CFA com Tabela -----
  output$multiConstrutoVarSelectUI <- renderUI({
    lapply(names(construtos_vars), function(nome) {
      selectizeInput(
        inputId = paste0("multi_", nome),
        label = paste("Variáveis para", nome),
        choices = construtos_vars[[nome]],
        selected = default_vars[[nome]],
        multiple = TRUE
      )
    })
  })
  
  observeEvent(input$runScoreCalc, {
    selected_vars <- lapply(names(construtos_vars), function(nome) {
      input[[paste0("multi_", nome)]]
    })
    names(selected_vars) <- names(construtos_vars)
    
    if (input$method_choice == "media") {
      resultado <- run_item_means(df, selected_vars)
    } else {
      resultado <- run_CFA_extract_scores(df, selected_vars)$scores_df
    }
    
    resultado_rounded <- resultado %>%
      mutate(across(everything(), ~ round(.x, 8)))
    
    output$scoreTable <- DT::renderDataTable({
      DT::datatable(resultado_rounded, options = list(pageLength = 10, scrollX = TRUE))
    })
  })
}

shinyApp(ui = ui, server = server)
