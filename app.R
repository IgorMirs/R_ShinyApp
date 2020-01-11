library(shiny)
library(spuRs)

ui <- fluidPage(
  titlePanel("",windowTitle = "Root finding methods"),
  # Divide the window on sidebar and main panel 
  sidebarLayout(position = "right",
        
        #input of function        
        sidebarPanel(
          img(src = "shiny.png", height = 70, width = 70, align = "right"),        
          h5("Choose function:"),
          selectInput(inputId = "fun", 
                    "",
                      choices = c("cos(x)" = 1,
                                  "cos(x) - x" = 2, 
                                   "x^2 - 2*x" = 3, 
                                   "log(x) − exp(−x)" = 4),
                                    width = 300),
        
        #input of method
        radioButtons(inputId = "method",
                           h3("Method"),
                           choices = list("Fixed-point iteration" = 1, 
                                          "The secant method" = 2, 
                                          "The bisection method" = 3,
                                          "The Newton-Raphson method" = 4),
                           selected = 1
                           ),
        # first initial point input
        sliderInput(inputId = "initial",
                     h3("Initial point"),
                     value = -1,
                     min = -3,
                     max = 3,
                      step = 0.2
                     ),
        
        # second initial point input
        sliderInput(inputId = "initial_2",
                    h4("Second initial point (for secant and bisections methods)"),
                    value = 1,
                    min = -3,
                    max = 3,
                    step = 0.2
        )

        ),
        
        mainPanel( 
                h1 ("Root finding methods", align = "center" ),
                h3("Function plot"),
                plotOutput("distPlot"),    #plot
                h3("Method steps"),
                verbatimTextOutput("text")    #method output 
        )
    
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    sec_m <- function (ftn, x0, x1, tol = 1e-09, max.iter = 100) 
    {
        xold <- x0
        xnow <- x1
        xnew <- xnow - ftn(xnow) * (xnow - xold)/(ftn(xnow) - ftn(xold))
        iter <- 0
        cat("At iteration 1 value of x is:", xnew, "\n")
        while ((abs(xnew - xold) > tol) && (iter < max.iter)) {
            xold <- xnow
            xnow <- xnew
            xnew <- xnow - ftn(xnow) * (xnow - xold)/(ftn(xnow) - ftn(xold))
            iter <- iter + 1
            cat("At iteration", iter, "value of x is:", xnew, "\n")
        }
        if (abs(xnew - xold) > tol) {
            cat("Algorithm failed to converge\n")
            return(NULL)
        }
        else {
            cat("Algorithm converged\n")
            return(xnew)
        }
    }
    
output$distPlot <- renderPlot({
    
    ifelse (input$fun == 1, f <- function(x) return (cos(x)),
    ifelse (input$fun == 2, f <- function(x) return (cos(x) - x), 
    ifelse (input$fun == 3, f <- function(x) return (x^2 - 2*x),
    ifelse (input$fun == 4, f <- function(x) return (log(x) - exp(-x))))))
        
    #printing the functionds    
    curve(f, from = -2, to = pi, ylab = "Function")
    abline (h = 0, lty = 5)
#    abline(v = 0.7390851, lty = 5)
})
  
output$text <- renderPrint({
  
      #define functions   
    ifelse (input$fun == 1, f <- function(x) return (cos(x)),
    ifelse (input$fun == 2, f <- function(x) return (cos(x) - x), 
    ifelse (input$fun == 3, f <- function(x) return (x^2 - 2*x),
    ifelse (input$fun == 4, f <- function(x) return (log(x) - exp(-x))))))
    
    #define functions for fixed point
    ifelse (input$fun == 1, f_fix <- function(x) return (acos(x)),
    ifelse (input$fun == 2, f_fix <- function(x) return (cos(x)), 
    ifelse (input$fun == 3, f_fix <- function(x) return (x^2 / 2),
    ifelse (input$fun == 4, f_fix <- function(x) return (x - log(x) + exp(-x))))))
  
    #define functions for Newton method
    ifelse( input$fun == 1, f_newton <- function(x){
      calc <- cos(x)
      dcalc <- -sin(x)
      return(c(calc,dcalc))
    },
    ifelse( input$fun == 2, f_newton <- function(x){
      calc <- cos(x) - x
      dcalc <- -sin(x) - 1
      return(c(calc,dcalc))
    },
    ifelse( input$fun == 3, f_newton <- function(x){
      calc <- x^2 - 2*x
      dcalc <- 2*x - 2
      return(c(calc,dcalc))
    },
    ifelse( input$fun == 4, f_newton <- function(x){
      calc <- log(x) - exp(-x)
      dcalc <- 1/x + exp(-x)
      return(c(calc,dcalc))
    }))))
    
      #choosing the right method
      ifelse (input$method == 1, fixedpoint(f_fix, input$initial),
      ifelse (input$method == 2, sec_m(f, input$initial, input$initial_2),
      ifelse (input$method == 3, bisection(f, input$initial, input$initial_2),
      ifelse (input$method == 4, newtonraphson(f_newton, input$initial)))))

})

}

# Run the application 
shinyApp(ui = ui, server = server)
