#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for slider demo app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Mortgage Calculator"),
  h4(tags$a(href = "https://www.antoinesoetewey.com/", "Antoine Soetewey")),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar to demonstrate various slider options ----
    sidebarPanel(

      # Input: Simple integer interval ----
      numericInput("principal", "Principal (loan amount)", 200000, min = 0, step = 1000),
      hr(),
      numericInput("interest", "Annual interest rate (in %)", 2, min = 0, max = 100, step = 0.01),
      hr(),
      sliderInput("length", "Length of the loan (in years)",
        min = 0,
        max = 30,
        value = 25,
        step = 1
      ),
      hr(),
      radioButtons(
        inputId = "month",
        label = "Table",
        choices = c(
          "Monthly" = TRUE,
          "Yearly" = FALSE
        )
      ),
      hr(),
      checkboxInput("plot", "Display plot?", TRUE),
      hr(),
      HTML('<p>Report a <a href="https://github.com/AntoineSoetewey/mortgage-calculator/issues">bug</a> or view the <a href="https://github.com/AntoineSoetewey/mortgage-calculator">code</a>. See more information about this app in this <a href="https://www.statsandr.com/blog/">article xxx edit link</a>.</p><p>Back to <a href="https://www.antoinesoetewey.com/">www.antoinesoetewey.com</a> or <a href="https://www.statsandr.com/">www.statsandr.com</a>.</p>')
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Table summarizing the values entered ----
      uiOutput("text"),
      br(),
      plotOutput("distPlot"),
      br(),
      DT::dataTableOutput("tbl"),
      br(),
      p(em("Disclosure: Note that this practical guide on asset allocation is based on the book A Random Walk Down Wall Street by Burton G. Malkiel. This application does not include investment advice or recommendations, nor a financial analysis. This application is intended for information only and you invest at your own risks. I cannot be held liable for any decision made based on the information contained in this application, nor for its use by third parties.")),
      br(),
      br()
    )
  )
)

# Define server logic for slider examples ----
server <- function(input, output) {
  mortgage <- function(P = 500000, I = 6, L = 30, amort = TRUE, plotData = TRUE) {
    J <- I / (12 * 100)
    N <- 12 * L
    M <- P * J / (1 - (1 + J)^(-N))
    monthPay <<- M
    # Calculate Amortization for each Month
    if (amort == T) {
      Pt <- P # current principal or amount of the loan
      currP <- NULL
      while (Pt >= 0) {
        H <- Pt * J # this is the current monthly interest
        C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
        Q <- Pt - C # this is the new balance of your principal of your loan
        Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
        currP <- c(currP, Pt)
      }
      monthP <- c(P, currP[1:(length(currP) - 1)]) - currP
      aDFmonth <<- data.frame(
        Amortization = c(P, currP[1:(length(currP) - 1)]),
        Monthly_Payment = monthP + c((monthPay - monthP)[1:(length(monthP) - 1)], 0),
        Monthly_Principal = monthP,
        Monthly_Interest = c((monthPay - monthP)[1:(length(monthP) - 1)], 0),
        Year = sort(rep(1:ceiling(N / 12), 12))[1:length(monthP)]
      )
      aDFyear <- data.frame(
        Amortization = tapply(aDFmonth$Amortization, aDFmonth$Year, max),
        Annual_Payment = tapply(aDFmonth$Monthly_Payment, aDFmonth$Year, sum),
        Annual_Principal = tapply(aDFmonth$Monthly_Principal, aDFmonth$Year, sum),
        Annual_Interest = tapply(aDFmonth$Monthly_Interest, aDFmonth$Year, sum),
        Year = as.vector(na.omit(unique(aDFmonth$Year)))
      )
      aDFyear <<- aDFyear
    }
    if (plotData == T) {
      barplot(t(aDFyear[, c(3, 4)]),
        col = c("blue", "red"),
        main = "Annual Interest and Principal Payments",
        sub = "The data for this plot is stored in aDFyear.",
        xlab = "Years", ylab = "$ Amount",
        legend.text = c("Principal", "Interest"),
        ylim = c(0, max(aDFyear$Annual_Payment) * 1.3)
      )
    }
  }

  output$text <- renderUI({
    mortgage(P = input$principal, I = input$interest, L = input$length, plotData = FALSE)
    HTML(paste0(
      "<b>", "Monthly payment: ", format(round(monthPay, digits = 2), big.mark = ","), "</b>",
      "<br>",
      "<b>", "Total cost: ", format(round(monthPay * 12 * input$length, digits = 2), big.mark = ","), "</b>"
    ))
  })

  output$distPlot <- renderPlot({
    mortgage(P = input$principal, I = input$interest, L = input$length, plotData = input$plot)
  })

  # Data output
  output$tbl <- DT::renderDataTable({
    mortgage(P = input$principal, I = input$interest, L = input$length, plotData = FALSE)
    df_month <- DT::datatable(data.frame(round(aDFmonth, 2)),
      extensions = "Buttons",
      options = list(
        lengthChange = FALSE,
        dom = "Blrtip",
        buttons = c("copy", "csv", "excel", "pdf", "print"),
        pageLength = 15
      )
    )

    df_year <- DT::datatable(data.frame(round(aDFyear, 2)),
      extensions = "Buttons",
      options = list(
        lengthChange = FALSE,
        dom = "Blrtip",
        buttons = c("copy", "csv", "excel", "pdf", "print"),
        pageLength = 15
      )
    )
    if (input$month == TRUE) {
      df_month
    } else {
      df_year
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
