library(tidyverse)
library(glue)
library(deSolve)
library(scales)

sir_model <- function(t, b, g, tot_pop, inf_pop, prop, out_plot) {
  require(deSolve)
  
  init <- c(
    S = 1 - (inf_pop/tot_pop),
    I = (inf_pop/tot_pop),
    R = 0
  )
  
  parameters <- c(bet = b, gamm = g)
  
  time <- seq(0, t, by = t/(2*length(1:t)))
  
  eqn <- function(time, state, parameters) {
    with(as.list(c(state, parameters)),{
      dS <- -bet*S*I
      dI <- bet*S*I-gamm*I
      dR <- gamm*I
      return(list(c(dS, dI, dR)))
    })
  }
  
  out <- ode(y = init, times = time, eqn, parms = parameters)
  out_df <- as.data.frame(out)
  if (prop == FALSE) {
    out_df <- out_df %>%
      mutate(
        S = S * tot_pop,
        I = I * tot_pop,
        R = R * tot_pop
      )
  }
  
  if (out_plot == FALSE) {
    return(out_df)
  }
  
  else {
  require(ggplot2)
  mytheme <- theme_minimal() +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.title  = element_text(face = "bold"),
          legend.background = element_rect(fill="#FFFFFF", color = "#FFFFFF")
    )
  theme_set(mytheme)
  
  
  title <- bquote("SIR Model: Basic")
  subtit <- bquote(list(beta==.(parameters[1]), ~gamma==.(parameters[2])))
  
  res <- ggplot(out_df, aes(x = time)) +
    ggtitle(bquote(atop(bold(.(title)), atop(bold(.(subtit))))))+
    geom_line(aes(y = S, color = "Susceptible"), size = 1) +
    geom_line(aes(y = I, color = "Infected"), size = 1) +
    geom_line(aes(y = R, color = "Recovered"), size = 1) +
    ylab(label = "Proportion") +
    xlab(label = "Time (days)") +
    theme(legend.justification = c(1, 0), legend.position = c(1, 0.8)) +
    scale_color_manual("Compartments", 
                       breaks = c("Susceptible", "Infected", "Recovered"),
                       values = c("red", "green", "blue"))
  
  if (prop == FALSE) {
    res <- res +
      scale_y_continuous(labels = comma) +
      ylab(label = "Count")
  }
  
  print(res)
  }
}

ui <- fluidPage(
  tags$style("* { font-family: Arial; }"),
  
  titlePanel("Explore a basic SIR model"),
  
  sidebarLayout(
    sidebarPanel(
      p("Explore the different aspects of a SIR model."),
      p("Based on: ", a("eugejjoh's blog", target="_blank", href="https://eugejjoh.wordpress.com/2017/01/04/sir-model-with-desolve-ggplot2/")),
      
      numericInput("R_0", HTML(paste0("R",tags$sub("0"))), value = 3),
      numericInput("time", "Time (days)", value = 30),
      numericInput("pop", "Total population", value = 10000000),
      numericInput("inf", "Initial infected", value = 500),
      numericInput("inf_p", "Infectious period (days)", value = 3),
      selectInput("proportion", "Scale",
                   choices = list("Proportion" = TRUE, "Count" = FALSE),
                   selected = TRUE)
      ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot"),
                           downloadButton(outputId = "downloadPlot", label = "Download Plot")),
                  tabPanel("Table", DT::dataTableOutput("table"),
                           downloadButton("downloadTable", "Download Table")))
    )
  )
)

server <- function(input, output) {
  
  
  
  plot_input <- reactive({
    
    sir_model(
      t        = input$time,
      b        = input$R_0 / input$inf_p,
      g        = 1 / input$inf_p,
      tot_pop  = input$pop,
      inf_pop  = input$inf,
      prop     = input$proportion,
      out_plot = TRUE
      )
    
  })
  
  output$plot <- renderPlot({ plot_input() })
  
  d <- reactive({
    sir_model(
      t        = input$time,
      b        = input$R_0 / input$inf_p,
      g        = 1 / input$inf_p,
      tot_pop  = input$pop,
      inf_pop  = input$inf,
      prop     = input$proportion,
      out_plot = FALSE
    )
  })
  
  output$table <- DT::renderDataTable({
    d()
  })
  
  output$downloadTable <- downloadHandler(
    filename = function() {
      glue("sir_model_data.txt")
    },
    content = function(file) {
      write_tsv(d(), file)
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = glue("sir_model_plot.pdf"),
    content  = function(file) {
      pdf(file, width = 7, height = 5)
      print(plot_input())
      dev.off()
    }
  )
  
}

shinyApp(ui, server)

