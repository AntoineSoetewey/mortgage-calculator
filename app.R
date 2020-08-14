#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(reshape2)
library(scales)
options(scipen = 999)

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
      sliderInput("length", "Duration of the loan (in years)",
        min = 0,
        max = 30,
        value = 25,
        step = 1
      ),
      hr(),
      checkboxInput("plot", "Display plot?", TRUE),
      hr(),
      HTML('<p>Report a <a href="https://github.com/AntoineSoetewey/mortgage-calculator/issues">bug</a> or view the <a href="https://github.com/AntoineSoetewey/mortgage-calculator">code</a>. See more information about this app in this <a href="https://www.statsandr.com/blog/mortgage-calculator-r-shiny">article</a>. Back to <a href="https://www.antoinesoetewey.com/">www.antoinesoetewey.com</a> or <a href="https://www.statsandr.com/">www.statsandr.com</a>.</p>')
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
      p(em("Disclosure: Note that this application does not include investment advice or recommendations, nor a financial analysis. This application is intended for information only and you invest at your own risks. I cannot be held liable for any decision made based on the information contained in this application, nor for its use by third parties.")),
      p(em("This R Shiny app is partially based on the R code of Prof. Thomas Girke.")),
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
    if (amort == TRUE) {
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
        Month = 1:length(currP),
        Year = sort(rep(1:ceiling(N / 12), 12))[1:length(monthP)],
        Balance = c(currP[1:(length(currP))]),
        Payment = monthP + c((monthPay - monthP)[1:(length(monthP))]),
        Principal = monthP,
        Interest = c((monthPay - monthP)[1:(length(monthP))])
      )
      aDFmonth <<- subset(aDFmonth, Year <= L * 12)
      aDFyear <- data.frame(
        Amortization = tapply(aDFmonth$Balance, aDFmonth$Year, max),
        Annual_Payment = tapply(aDFmonth$Payment, aDFmonth$Year, sum),
        Annual_Principal = tapply(aDFmonth$Principal, aDFmonth$Year, sum),
        Annual_Interest = tapply(aDFmonth$Interest, aDFmonth$Year, sum),
        Year = as.factor(na.omit(unique(aDFmonth$Year)))
      )
      aDFyear <<- aDFyear
    }
    if (plotData == TRUE) {
      aDFyear2 <- aDFyear %>%
        rename(
          Interest = Annual_Interest,
          Payment = Annual_Payment,
          Principal = Annual_Principal
        )
      aDFyear2$Year <- as.factor(aDFyear2$Year)
      aDFyear2 <- melt(aDFyear2[, c("Interest", "Principal", "Year")], id.vars = "Year")

      ggplot(aDFyear2, aes(x = Year, y = value, fill = variable)) +
        geom_bar(position = "fill", stat = "identity") +
        labs(y = "Payment") +
        scale_y_continuous(labels = percent) +
        theme_minimal() +
        theme(legend.title = element_blank(), legend.position = "top")
    }
  }

  output$text <- renderUI({
    mortgage(P = input$principal, I = input$interest, L = input$length, plotData = FALSE)
    HTML(paste0(
      "<h3>", "Summary", "</h3>",
      "Principal (loan amount): ", format(round(input$principal, 2), big.mark = ","),
      "<br>",
      "Annual interest rate: ", input$interest, "%",
      "<br>",
      "Term: ", input$length, " years (", input$length * 12, " months)",
      "<br>",
      "<b>", "Monthly payment: ", format(round(monthPay, digits = 2), big.mark = ","), "</b>",
      "<br>",
      "<b>", "Total cost: ", "</b>", format(round(input$principal, 2), big.mark = ","), " (principal) + ", format(round(monthPay * 12 * input$length - input$principal, 2), big.mark = ","), " (interest) = ", "<b>", format(round(monthPay * 12 * input$length, digits = 2), big.mark = ","), "</b>"
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
        lengthChange = TRUE,
        dom = "Blrtip",
        buttons = c("copy", "csv", "excel", "pdf", "print"),

        lengthMenu = list(c(-1, 10, 12, 15, 25, 50, 100), c("All", "10", "12", "15", "25", "50", "100"))
      ),
      rownames = FALSE
    ) %>%
      formatCurrency(c("Balance", "Payment", "Principal", "Interest"), currency = "", interval = 3, mark = ",")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
