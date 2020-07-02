#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(reshape2)
# install.packages("formattable")
# library(formattable)
# install.packages("stringr")
# library(stringr)

# Define UI for slider demo app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Mortgage Calculator"),
    h4(tags$a(href = "https://www.antoinesoetewey.com/", "Antoine Soetewey")),
    
    tabsetPanel(
        # Create tab1
        tabPanel(
            title = "Where to invest?",
            
            # Sidebar layout with input and output definitions ----
            sidebarLayout(
                
                # Sidebar to demonstrate various slider options ----
                sidebarPanel(
                    
                    # Input: Simple integer interval ----
                    sliderInput("age", "How old are you?",
                                min = 0, max = 100,
                                value = 30
                    ),
                    hr(),
                    numericInput("amount_to_invest", "How much are you willing to invest?", 1000, min = 0, step = 100),
                    hr(),
                    HTML('<p>Report a <a href="https://github.com/AntoineSoetewey/optimal-asset-allocation/issues">bug</a> or view the <a href="https://github.com/AntoineSoetewey/optimal-asset-allocation">code</a>. See more information about this app in this <a href="https://www.statsandr.com/blog/practical-guide-on-optimal-asset-allocation/">article</a>.</p><p>Back to <a href="https://www.antoinesoetewey.com/">www.antoinesoetewey.com</a> or <a href="https://www.statsandr.com/">www.statsandr.com</a>.</p>')
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                    
                    # Output: Table summarizing the values entered ----
                    tableOutput("values"),
                    tags$style(
                        HTML("tr:nth-child(1) {font-weight: bold;}
                 tr:nth-child(2) { white-space: pre; }
                 tr:nth-child(3) { white-space: pre; }
                 tr:nth-child(4) { white-space: pre; }
                 tr:nth-child(5) {font-weight: bold;}
                 tr:nth-child(6) { white-space: pre; }
                 tr:nth-child(7) { white-space: pre; }
                 tr:nth-child(8) { white-space: pre; }
                 tr:nth-child(9) {font-weight: bold;}
                 tr:nth-child(10) { white-space: pre; }
                 tr:nth-child(11) {font-weight: bold;}
                 tr:nth-child(12) { white-space: pre; }
                 ")
                    ),
                    plotOutput("distPlot"),
                    br(),
                    p(em("Disclosure: Note that this practical guide on asset allocation is based on the book A Random Walk Down Wall Street by Burton G. Malkiel. This application does not include investment advice or recommendations, nor a financial analysis. This application is intended for information only and you invest at your own risks. I cannot be held liable for any decision made based on the information contained in this application, nor for its use by third parties.")),
                    br(),
                    br()
                )
            )
        ),
        # Create tab2
        tabPanel(
            title = "Compare with your portfolio",
            # Sidebar layout with input and output definitions ----
            sidebarLayout(
                
                # Sidebar to demonstrate various slider options ----
                sidebarPanel(
                    
                    # Input: Simple integer interval ----
                    sliderInput("age2", "How old are you?",
                                min = 0, max = 100,
                                value = 30
                    ),
                    hr(),
                    "Your portfolio:",
                    br(),
                    br(),
                    numericInput("stocks", "Stocks", 0, min = 0, step = 100),
                    numericInput("bonds", "Bonds", 0, min = 0, step = 100),
                    numericInput("real_estate", "Real estate", 0, min = 0, step = 100),
                    numericInput("cash", "Cash", 0, min = 0, step = 100),
                    hr(),
                    HTML('<p>Report a <a href="https://github.com/AntoineSoetewey/optimal-asset-allocation/issues">bug</a> or view the <a href="https://github.com/AntoineSoetewey/optimal-asset-allocation">code</a>. See more information about this app in this <a href="https://www.statsandr.com/blog/practical-guide-on-optimal-asset-allocation/">article</a>.</p><p>Back to <a href="https://www.antoinesoetewey.com/">www.antoinesoetewey.com</a> or <a href="https://www.statsandr.com/">www.statsandr.com</a>.</p>')
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                    br(),
                    textOutput("total_amount"),
                    br(),
                    plotOutput("distPlot2"),
                    br(),
                    p(em("Disclosure: Note that this practical guide on asset allocation is based on the book A Random Walk Down Wall Street by Burton G. Malkiel. This application does not include investment advice or recommendations, nor a financial analysis. This application is intended for information only and you invest at your own risks. I cannot be held liable for any decision made based on the information contained in this application, nor for its use by third parties.")),
                    br(),
                    br()
                )
            )
        )
    )
)

# Define server logic for slider examples ----
server <- function(input, output) {
    
    # Reactive expression to create data frame of all input values ----
    sliderValues <- reactive({
        if (input$age <= 35) {
            stocks1_optperc <- 0.27 / 0.55 * 0.7
            stocks2_optperc <- 0.14 / 0.55 * 0.7
            stocks3_optperc <- 0.14 / 0.55 * 0.7
            stocks_optperc <- stocks1_optperc + stocks2_optperc + stocks3_optperc
            bonds1_optperc <- 0.075 / 0.275 * 0.15
            bonds2_optperc <- 0.075 / 0.275 * 0.15
            bonds3_optperc <- 0.125 / 0.275 * 0.15
            bonds_optperc <- bonds1_optperc + bonds2_optperc + bonds3_optperc
            realestate_optperc <- 0.10
            cash_optperc <- 0.05
            total_optperc <- stocks_optperc + bonds_optperc + realestate_optperc + cash_optperc
            
            df <- data.frame(
                asset = c(
                    "Stocks:",
                    "    US Stocks",
                    "    Developed International Markets Stocks",
                    "    Emerging International Markets Stocks",
                    "Bonds and Bond Substitutes:",
                    "    US Corporate Bonds",
                    "    Emerging Market Government Bonds",
                    "    Dividend Growth Fund",
                    "Real Estate:",
                    "    Real Estate Equities",
                    "Cash:",
                    "    Cash"
                ),
                ticker = c(
                    "",
                    "SWTSX or VTSMX",
                    "SWISX or VTMGX",
                    "VEIEX or FFMAX",
                    "",
                    "VICSX or LQD",
                    "VGAVX",
                    "DGRW or VDIGX",
                    "",
                    "VGSIX or FRXIX",
                    "",
                    "FXLXX or VMMXX"
                ),
                optimal_percentage = c(
                    "",
                    paste(round(stocks1_optperc * 100, 2), "%"),
                    paste(round(stocks2_optperc * 100, 2), "%"),
                    paste(round(stocks3_optperc * 100, 2), "%"),
                    "",
                    paste(round(bonds1_optperc * 100, 2), "%"),
                    paste(round(bonds2_optperc * 100, 2), "%"),
                    paste(round(bonds3_optperc * 100, 2), "%"),
                    "",
                    paste(round(realestate_optperc * 100, 2), "%"),
                    "",
                    paste(round(cash_optperc * 100, 2), "%")
                ),
                optimal_amount = c(
                    "",
                    round(stocks1_optperc * input$amount_to_invest, 2),
                    round(stocks2_optperc * input$amount_to_invest, 2),
                    round(stocks3_optperc * input$amount_to_invest, 2),
                    "",
                    round(bonds1_optperc * input$amount_to_invest, 2),
                    round(bonds2_optperc * input$amount_to_invest, 2),
                    round(bonds3_optperc * input$amount_to_invest, 2),
                    "",
                    round(realestate_optperc * input$amount_to_invest, 2),
                    "",
                    round(cash_optperc * input$amount_to_invest, 2)
                )
            )
            names(df) <- c("Asset", "Ticker", "Optimal percentage", "Optimal amount")
            df
        } else if (input$age >= 36 & input$age <= 44) {
            stocks1_optperc <- 0.27 / 0.55 * 0.65
            stocks2_optperc <- 0.14 / 0.55 * 0.65
            stocks3_optperc <- 0.14 / 0.55 * 0.65
            stocks_optperc <- stocks1_optperc + stocks2_optperc + stocks3_optperc
            bonds1_optperc <- 0.075 / 0.275 * 0.20
            bonds2_optperc <- 0.075 / 0.275 * 0.20
            bonds3_optperc <- 0.125 / 0.275 * 0.20
            bonds_optperc <- bonds1_optperc + bonds2_optperc + bonds3_optperc
            realestate_optperc <- 0.10
            cash_optperc <- 0.05
            total_optperc <- stocks_optperc + bonds_optperc + realestate_optperc + cash_optperc
            
            df <- data.frame(
                asset = c(
                    "Stocks:",
                    "    US Stocks",
                    "    Developed International Markets Stocks",
                    "    Emerging International Markets Stocks",
                    "Bonds and Bond Substitutes:",
                    "    US Corporate Bonds",
                    "    Emerging Market Government Bonds",
                    "    Dividend Growth Fund",
                    "Real Estate:",
                    "    Real Estate Equities",
                    "Cash:",
                    "    Cash"
                ),
                ticker = c(
                    "",
                    "SWTSX or VTSMX",
                    "SWISX or VTMGX",
                    "VEIEX or FFMAX",
                    "",
                    "VICSX or LQD",
                    "VGAVX",
                    "DGRW or VDIGX",
                    "",
                    "VGSIX or FRXIX",
                    "",
                    "FXLXX or VMMXX"
                ),
                optimal_percentage = c(
                    "",
                    paste(round(stocks1_optperc * 100, 2), "%"),
                    paste(round(stocks2_optperc * 100, 2), "%"),
                    paste(round(stocks3_optperc * 100, 2), "%"),
                    "",
                    paste(round(bonds1_optperc * 100, 2), "%"),
                    paste(round(bonds2_optperc * 100, 2), "%"),
                    paste(round(bonds3_optperc * 100, 2), "%"),
                    "",
                    paste(round(realestate_optperc * 100, 2), "%"),
                    "",
                    paste(round(cash_optperc * 100, 2), "%")
                ),
                optimal_amount = c(
                    "",
                    round(stocks1_optperc * input$amount_to_invest, 2),
                    round(stocks2_optperc * input$amount_to_invest, 2),
                    round(stocks3_optperc * input$amount_to_invest, 2),
                    "",
                    round(bonds1_optperc * input$amount_to_invest, 2),
                    round(bonds2_optperc * input$amount_to_invest, 2),
                    round(bonds3_optperc * input$amount_to_invest, 2),
                    "",
                    round(realestate_optperc * input$amount_to_invest, 2),
                    "",
                    round(cash_optperc * input$amount_to_invest, 2)
                )
            )
            names(df) <- c("Asset", "Ticker", "Optimal percentage", "Optimal amount")
            df
        } else if (input$age >= 45 & input$age <= 64) {
            stocks1_optperc <- 0.27
            stocks2_optperc <- 0.14
            stocks3_optperc <- 0.14
            stocks_optperc <- stocks1_optperc + stocks2_optperc + stocks3_optperc
            bonds1_optperc <- 0.075
            bonds2_optperc <- 0.075
            bonds3_optperc <- 0.125
            bonds_optperc <- bonds1_optperc + bonds2_optperc + bonds3_optperc
            realestate_optperc <- 0.125
            cash_optperc <- 0.05
            total_optperc <- stocks_optperc + bonds_optperc + realestate_optperc + cash_optperc
            
            df <- data.frame(
                asset = c(
                    "Stocks:",
                    "    US Stocks",
                    "    Developed International Markets Stocks",
                    "    Emerging International Markets Stocks",
                    "Bonds and Bond Substitutes:",
                    "    US Corporate Bonds",
                    "    Emerging Market Government Bonds",
                    "    Dividend Growth Fund",
                    "Real Estate:",
                    "    Real Estate Equities",
                    "Cash:",
                    "    Cash"
                ),
                ticker = c(
                    "",
                    "SWTSX or VTSMX",
                    "SWISX or VTMGX",
                    "VEIEX or FFMAX",
                    "",
                    "VICSX or LQD",
                    "VGAVX",
                    "DGRW or VDIGX",
                    "",
                    "VGSIX or FRXIX",
                    "",
                    "FXLXX or VMMXX"
                ),
                optimal_percentage = c(
                    "",
                    paste(round(stocks1_optperc * 100, 2), "%"),
                    paste(round(stocks2_optperc * 100, 2), "%"),
                    paste(round(stocks3_optperc * 100, 2), "%"),
                    "",
                    paste(round(bonds1_optperc * 100, 2), "%"),
                    paste(round(bonds2_optperc * 100, 2), "%"),
                    paste(round(bonds3_optperc * 100, 2), "%"),
                    "",
                    paste(round(realestate_optperc * 100, 2), "%"),
                    "",
                    paste(round(cash_optperc * 100, 2), "%")
                ),
                optimal_amount = c(
                    "",
                    round(stocks1_optperc * input$amount_to_invest, 2),
                    round(stocks2_optperc * input$amount_to_invest, 2),
                    round(stocks3_optperc * input$amount_to_invest, 2),
                    "",
                    round(bonds1_optperc * input$amount_to_invest, 2),
                    round(bonds2_optperc * input$amount_to_invest, 2),
                    round(bonds3_optperc * input$amount_to_invest, 2),
                    "",
                    round(realestate_optperc * input$amount_to_invest, 2),
                    "",
                    round(cash_optperc * input$amount_to_invest, 2)
                )
            )
            names(df) <- c("Asset", "Ticker", "Optimal percentage", "Optimal amount")
            df
        } else {
            stocks1_optperc <- 0.27 / 0.55 * 0.40
            stocks2_optperc <- 0.14 / 0.55 * 0.40
            stocks3_optperc <- 0.14 / 0.55 * 0.40
            
            bonds1_optperc <- 0.075 / 0.275 * 0.35
            bonds2_optperc <- 0.075 / 0.275 * 0.35
            bonds3_optperc <- 0.125 / 0.275 * 0.35
            
            realestate_optperc <- 0.15
            cash_optperc <- 0.10
            
            
            df <- data.frame(
                asset = c(
                    "Stocks:",
                    "    US Stocks",
                    "    Developed International Markets Stocks",
                    "    Emerging International Markets Stocks",
                    "Bonds and Bond Substitutes:",
                    "    US Corporate Bonds",
                    "    Emerging Market Government Bonds",
                    "    Dividend Growth Fund",
                    "Real Estate:",
                    "    Real Estate Equities",
                    "Cash:",
                    "    Cash"
                ),
                ticker = c(
                    "",
                    "SWTSX or VTSMX",
                    "SWISX or VTMGX",
                    "VEIEX or FFMAX",
                    "",
                    "VICSX or LQD",
                    "VGAVX",
                    "DGRW or VDIGX",
                    "",
                    "VGSIX or FRXIX",
                    "",
                    "FXLXX or VMMXX"
                ),
                optimal_percentage = c(
                    "",
                    paste(round(stocks1_optperc * 100, 2), "%"),
                    paste(round(stocks2_optperc * 100, 2), "%"),
                    paste(round(stocks3_optperc * 100, 2), "%"),
                    "",
                    paste(round(bonds1_optperc * 100, 2), "%"),
                    paste(round(bonds2_optperc * 100, 2), "%"),
                    paste(round(bonds3_optperc * 100, 2), "%"),
                    "",
                    paste(round(realestate_optperc * 100, 2), "%"),
                    "",
                    paste(round(cash_optperc * 100, 2), "%")
                ),
                optimal_amount = c(
                    "",
                    round(stocks1_optperc * input$amount_to_invest, 2),
                    round(stocks2_optperc * input$amount_to_invest, 2),
                    round(stocks3_optperc * input$amount_to_invest, 2),
                    "",
                    round(bonds1_optperc * input$amount_to_invest, 2),
                    round(bonds2_optperc * input$amount_to_invest, 2),
                    round(bonds3_optperc * input$amount_to_invest, 2),
                    "",
                    round(realestate_optperc * input$amount_to_invest, 2),
                    "",
                    round(cash_optperc * input$amount_to_invest, 2)
                )
            )
            names(df) <- c("Asset", "Ticker", "Optimal percentage", "Optimal amount")
            df
        }
    })
    
    # Show the values in an HTML table ----
    output$values <- renderTable({
        sliderValues()
    })
    
    output$distPlot <- renderPlot({
        if (input$age <= 35) {
            stocks1_optperc <- 0.27 / 0.55 * 0.7
            stocks2_optperc <- 0.14 / 0.55 * 0.7
            stocks3_optperc <- 0.14 / 0.55 * 0.7
            stocks_optperc <- stocks1_optperc + stocks2_optperc + stocks3_optperc
            bonds1_optperc <- 0.075 / 0.275 * 0.15
            bonds2_optperc <- 0.075 / 0.275 * 0.15
            bonds3_optperc <- 0.125 / 0.275 * 0.15
            bonds_optperc <- bonds1_optperc + bonds2_optperc + bonds3_optperc
            realestate_optperc <- 0.10
            cash_optperc <- 0.05
            total_optperc <- stocks_optperc + bonds_optperc + realestate_optperc + cash_optperc
        } else if (input$age >= 36 & input$age <= 44) {
            stocks1_optperc <- 0.27 / 0.55 * 0.65
            stocks2_optperc <- 0.14 / 0.55 * 0.65
            stocks3_optperc <- 0.14 / 0.55 * 0.65
            stocks_optperc <- stocks1_optperc + stocks2_optperc + stocks3_optperc
            bonds1_optperc <- 0.075 / 0.275 * 0.20
            bonds2_optperc <- 0.075 / 0.275 * 0.20
            bonds3_optperc <- 0.125 / 0.275 * 0.20
            bonds_optperc <- bonds1_optperc + bonds2_optperc + bonds3_optperc
            realestate_optperc <- 0.10
            cash_optperc <- 0.05
            total_optperc <- stocks_optperc + bonds_optperc + realestate_optperc + cash_optperc
        } else if (input$age >= 45 & input$age <= 64) {
            stocks1_optperc <- 0.27
            stocks2_optperc <- 0.14
            stocks3_optperc <- 0.14
            stocks_optperc <- stocks1_optperc + stocks2_optperc + stocks3_optperc
            bonds1_optperc <- 0.075
            bonds2_optperc <- 0.075
            bonds3_optperc <- 0.125
            bonds_optperc <- bonds1_optperc + bonds2_optperc + bonds3_optperc
            realestate_optperc <- 0.125
            cash_optperc <- 0.05
            total_optperc <- stocks_optperc + bonds_optperc + realestate_optperc + cash_optperc
        } else {
            stocks1_optperc <- 0.27 / 0.55 * 0.40
            stocks2_optperc <- 0.14 / 0.55 * 0.40
            stocks3_optperc <- 0.14 / 0.55 * 0.40
            stocks_optperc <- stocks1_optperc + stocks2_optperc + stocks3_optperc
            bonds1_optperc <- 0.075 / 0.275 * 0.35
            bonds2_optperc <- 0.075 / 0.275 * 0.35
            bonds3_optperc <- 0.125 / 0.275 * 0.35
            bonds_optperc <- bonds1_optperc + bonds2_optperc + bonds3_optperc
            realestate_optperc <- 0.15
            cash_optperc <- 0.10
            total_optperc <- stocks_optperc + bonds_optperc + realestate_optperc + cash_optperc
        }
        
        # # pie chart with percentages
        # slices <- c(stocks_optperc, bonds_optperc, realestate_optperc, cash_optperc)
        # lbls <- c("stocks", "bonds", "real estate", "cash")
        # pct <- round(slices/sum(slices)*100)
        # lbls <- paste(lbls, pct) # add percents to labels
        # lbls <- paste(lbls,"%",sep="") # add % to labels
        # pie(slices,labels = lbls, col=rainbow(length(lbls))#,
        #     #main="Optimal assets allocation"
        #     )
        slices <- c(stocks_optperc, bonds_optperc, realestate_optperc, cash_optperc) * 100
        lbls <- c("Stocks", "Bonds", "Real estate", "Cash")
        df <- data.frame(lbls = lbls, slices = slices)
        p <- ggplot(data = df, aes(x = reorder(lbls, -slices), y = slices)) +
            geom_bar(stat = "identity", fill = "steelblue") +
            # scale_x_discrete(limits=c("Stocks", "Bonds", "Real estate", "Cash")) +
            theme_minimal() +
            geom_text(aes(label = slices), vjust = 1.6, color = "white", size = 3.5) +
            labs(
                title = "Optimal asset allocation",
                x = "Asset", y = "Weight (%)"
            ) +
            theme(
                axis.text = element_text(size = 12),
                axis.title = element_text(size = 14, face = "bold"),
                plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
            )
        p
    })
    
    output$distPlot2 <- renderPlot({
        if (input$age2 <= 35) {
            stocks1_optperc <- 0.27 / 0.55 * 0.7
            stocks2_optperc <- 0.14 / 0.55 * 0.7
            stocks3_optperc <- 0.14 / 0.55 * 0.7
            stocks_optperc <- stocks1_optperc + stocks2_optperc + stocks3_optperc
            bonds1_optperc <- 0.075 / 0.275 * 0.15
            bonds2_optperc <- 0.075 / 0.275 * 0.15
            bonds3_optperc <- 0.125 / 0.275 * 0.15
            bonds_optperc <- bonds1_optperc + bonds2_optperc + bonds3_optperc
            realestate_optperc <- 0.10
            cash_optperc <- 0.05
            total_optperc <- stocks_optperc + bonds_optperc + realestate_optperc + cash_optperc
        } else if (input$age2 >= 36 & input$age2 <= 44) {
            stocks1_optperc <- 0.27 / 0.55 * 0.65
            stocks2_optperc <- 0.14 / 0.55 * 0.65
            stocks3_optperc <- 0.14 / 0.55 * 0.65
            stocks_optperc <- stocks1_optperc + stocks2_optperc + stocks3_optperc
            bonds1_optperc <- 0.075 / 0.275 * 0.20
            bonds2_optperc <- 0.075 / 0.275 * 0.20
            bonds3_optperc <- 0.125 / 0.275 * 0.20
            bonds_optperc <- bonds1_optperc + bonds2_optperc + bonds3_optperc
            realestate_optperc <- 0.10
            cash_optperc <- 0.05
            total_optperc <- stocks_optperc + bonds_optperc + realestate_optperc + cash_optperc
        } else if (input$age2 >= 45 & input$age2 <= 64) {
            stocks1_optperc <- 0.27
            stocks2_optperc <- 0.14
            stocks3_optperc <- 0.14
            stocks_optperc <- stocks1_optperc + stocks2_optperc + stocks3_optperc
            bonds1_optperc <- 0.075
            bonds2_optperc <- 0.075
            bonds3_optperc <- 0.125
            bonds_optperc <- bonds1_optperc + bonds2_optperc + bonds3_optperc
            realestate_optperc <- 0.125
            cash_optperc <- 0.05
            total_optperc <- stocks_optperc + bonds_optperc + realestate_optperc + cash_optperc
        } else {
            stocks1_optperc <- 0.27 / 0.55 * 0.40
            stocks2_optperc <- 0.14 / 0.55 * 0.40
            stocks3_optperc <- 0.14 / 0.55 * 0.40
            stocks_optperc <- stocks1_optperc + stocks2_optperc + stocks3_optperc
            bonds1_optperc <- 0.075 / 0.275 * 0.35
            bonds2_optperc <- 0.075 / 0.275 * 0.35
            bonds3_optperc <- 0.125 / 0.275 * 0.35
            bonds_optperc <- bonds1_optperc + bonds2_optperc + bonds3_optperc
            realestate_optperc <- 0.15
            cash_optperc <- 0.10
            total_optperc <- stocks_optperc + bonds_optperc + realestate_optperc + cash_optperc
        }
        
        slices_opt <- 100 * c(stocks_optperc, bonds_optperc, realestate_optperc, cash_optperc)
        lbls_opt <- c("stocks", "bonds", "real estate", "cash")
        grandtotal_amount <- sum(c(input$stocks, input$bonds, input$real_estate, input$cash), na.rm = TRUE)
        slices_actual <- round(100 * c(input$stocks / grandtotal_amount, input$bonds / grandtotal_amount, input$real_estate / grandtotal_amount, input$cash / grandtotal_amount), 2)
        
        output$total_amount <- renderText({
            print(paste0("Portfolio's total amount: ", grandtotal_amount))
        })
        df <- data.frame(x = slices_opt, y = slices_actual, lbls = lbls_opt)
        df <- melt(df, id.vars = "lbls")
        p <- ggplot(df, aes(x = reorder(lbls, -value), y = value, fill = variable)) +
            geom_bar(stat = "identity", position = "dodge") +
            theme_minimal() +
            labs(
                title = "Optimal vs portfolio asset allocation",
                x = "Asset", y = "Weight (%)"
            ) +
            theme(
                axis.text = element_text(size = 12),
                axis.title = element_text(size = 14, face = "bold"),
                plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
            ) +
            scale_fill_discrete(name = "", labels = c("Optimal", "Portfolio"))
        p
    })
}

# Run the application
shinyApp(ui = ui, server = server)
