library(shiny)
library(ggplot2)

# Define the user interface
ui <- fluidPage(
  titlePanel("Interactive Distribution Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("variableType",
                   label = "Variable Type:",
                   choices = list("Discrete" = "discrete", "Continuous" = "continuous"),
                   selected = "discrete"),
      
      conditionalPanel(
        condition = "input.variableType == 'discrete'",
        sliderInput("n",
                    label = "Number of Trials:",
                    min = 1,
                    max = 100,
                    value = 10),
        sliderInput("p",
                    label = "Probability of Success:",
                    min = 0.1,
                    max = 1,
                    value = 0.5,
                    step = 0.01),
        radioButtons("distType",
                     label = "Distribution Type:",
                     choices = list("PMF" = "pmf", "CDF" = "cdf"),
                     selected = "pmf")
      ),
      
      conditionalPanel(
        condition = "input.variableType == 'continuous'",
        sliderInput("mu",
                    label = "Mean (μ):",
                    min = -50,
                    max = 50,
                    value = 0),
        sliderInput("sigma",
                    label = "Standard Deviation (σ):",
                    min = 0.1,
                    max = 10,
                    value = 2),
        radioButtons("distType",
                     label = "Distribution Type:",
                     choices = list("PDF" = "pdf"),
                     selected = "pdf")
      )
    ),
    
    mainPanel(
      plotOutput("distributionPlot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  output$distributionPlot <- renderPlot({
    if (input$variableType == "discrete") {
      n <- input$n
      p <- input$p
      x <- 0:n
      
      if (input$distType == "pmf") {
        y <- dbinom(x, size = n, prob = p)
        plot_title <- "Binomial Distribution PMF"
      } else {
        y <- pbinom(x, size = n, prob = p)
        plot_title <- "Binomial Distribution CDF"
      }
      
      data <- data.frame(x, y)
      ggplot(data, aes(x = x, y = y)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(title = plot_title, x = "Number of Successes", y = "Probability") +
        theme_minimal()
      
    } else if (input$variableType == "continuous") {
      mu <- input$mu
      sigma <- input$sigma
      x <- seq(mu - 4*sigma, mu + 4*sigma, length.out = 1000)
      y <- dnorm(x, mean = mu, sd = sigma)
      data <- data.frame(x, y)
      
      ggplot(data, aes(x = x, y = y)) +
        geom_line(color = "steelblue") +
        labs(title = "Normal Distribution PDF", x = "Value", y = "Density") +
        theme_minimal()
    }
  })
}

# Run the application 
shinyApp(ui, server)
