library(shiny)
library(ggplot2)
library(dplyr)
library(pwr)

# Define UI
ui <- fluidPage(
  titlePanel("A Mean against a Constant (t Test)"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("outputType", "Request:",
                   choices = c("Statistical Power" = "power", "Sample Size" = "size")),
      
      # Conditional panels that show inputs based on the selected output type
      conditionalPanel(
        condition = "input.outputType == 'power'",
        numericInput("mu", "Mean (mu)", value = 0.5),
        numericInput("mu0", "Constant to test against (mu0)", value = 0),
        numericInput("sd", "Standard deviation (sd)", value = 1),
        numericInput("alpha", "Type I error rate (alpha)", value = 0.05),
        selectInput("alternative", "Alternative hypothesis",
                    choices = c("Not equal" = "two.sided",
                                "Greater" = "greater",
                                "Less" = "less")),
        numericInput("n", "Sample size (n)", value = 30)
      ),
      
      conditionalPanel(
        condition = "input.outputType == 'size'",
        numericInput("mu", "Mean (mu)", value = 0.5),
        numericInput("mu0", "Constant to test against (mu0)", value = 0),
        numericInput("sd", "Standard deviation (sd)", value = 1),
        numericInput("alpha", "Type I error rate (alpha)", value = 0.05),
        numericInput("power", "Statistical power (1 - Beta)", value = 0.8),
        selectInput("alternative", "Alternative hypothesis",
                    choices = c("Not equal" = "two.sided",
                                "Greater" = "greater",
                                "Less" = "less"))
      ),
      
      actionButton("update", "Update")
    ),
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("result")
    )
  )
)

# Define server logic
server <- function(input, output) {
  results <- eventReactive(input$update, {
    if (input$outputType == "power") {
      pwr.t.test(d = (input$mu - input$mu0) / input$sd, 
                 n = input$n, 
                 sig.level = input$alpha, 
                 alternative = input$alternative,
                 type = "one.sample")
    } else {
      pwr.t.test(d = (input$mu - input$mu0) / input$sd, 
                 power = input$power,
                 sig.level = input$alpha, 
                 alternative = input$alternative,
                 type = "one.sample")
    }
  })

  output$plot <- renderPlot({
    req(results())
    df <- data.frame(
      x = seq(-4, 8, length.out = 300),
      y = dt((seq(-4, 8, length.out = 300) - input$mu) / (input$sd / sqrt(input$n)), df = input$n - 1)
    )
    ggplot(df, aes(x = x, y = y)) +
      geom_line() +
      geom_area(data = df %>% filter(x >= qt(1 - input$alpha, df = input$n - 1)), aes(x = x, y = y), fill = "blue", alpha = 0.5) +
      geom_vline(xintercept = qt(1 - input$alpha, df = input$n - 1), linetype = "dashed", color = "red") +
      labs(title = "Distribution of t values", x = "t value", y = "Density")
  })
  
  output$result <- renderText({
    req(results())
    if (input$outputType == "power") {
      sprintf("Statistical Power: %.3f", results()$power)
    } else {
      sprintf("Required Sample Size: %d", ceiling(results()$n))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

