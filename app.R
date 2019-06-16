# this is a shiny web application used to calculate monthly payment
# associated with housing

library(yaml)
library(shiny)
library(tidyverse)
library(RColorBrewer)
library(DT)
library(zoo)

setwd("~/Dropbox/my-git/house-hunting-dashboard/")
default <- read_yaml("default.yml")
load("redfin-data.Rdata")
# ===========================================
df <- mod_data %>% select(
    "neighborhood",
    'period_end',
    "Avg Sale To List",
    "Homes Sold",
    "Inventory",
    "Median Dom",
    "New Listings",
    "median_sale_price",
    "Avg Sale To List Mom",
    "Avg Sale To List Yoy",
    "Homes Sold Mom",
    "Homes Sold Yoy",
    "Inventory Mom",
    "Inventory Yoy",
    "Median Dom Mom",
    "Median Dom Yoy",
    "Median Sale Price Mom",
    "Median Sale Price Yoy",
    "New Listings Mom",
    "New Listings Yoy"
) %>% rename(
    "Median Sale Price" = "median_sale_price"
)%>% gather(
    key = "metrics",
    value = "value",
    -c("period_end", "neighborhood")
)
# ===========================================
myColors <- brewer.pal(length(unique(mod_data$neighborhood)), "Set2")
names(myColors) <- unique(mod_data$neighborhood)
# ===========================================
theme <- theme_get() + theme(
    text = element_text(family = "Noto Sans"),
    panel.background = element_blank(),
    legend.position = "bottom",
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 18),
    axis.text = element_text(size = 14)
)

theme_set(theme)

options("scipen"=100, "digits"=4)
# Function used to calculate monthly payment
calMonthPaymnt <- function(house_price, downpay, interest_rate, tax, terms = 30, hoa = 0, insurance = 0) {
    dp <- downpay
    intrate <- interest_rate/12
    mon_terms <- 12 * terms
    monthly_mortgage <- house_price * (1 - dp) * (intrate * (1 + intrate)^mon_terms)/((1+intrate)^mon_terms - 1)
    mon_tax <- tax/12
    out <- monthly_mortgage + mon_tax + hoa + insurance
    return(out)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("House Hunting Dashboard"),
    
    tabsetPanel(type = "tabs",
                tabPanel("Monthly Payment Calculator",
                         sidebarLayout(
                             sidebarPanel(
                                 
                                 actionButton("do", "Export to Default"),
                                 p("Click the button to save the current values as Default"),
                                 
                                 textInput("income",
                                           "Gross Income ($K):",
                                           value = default$income), # end textInput
                                 
                                 textInput("price",
                                           "Housing Price ($K):",
                                           value = default$price), # end textInput
                                 
                                 sliderInput("downpay",
                                             "Down Payment (%):",
                                             min = 0, 
                                             max = 100,
                                             value = default$downpay), # end sliderInput
                                 
                                 sliderInput("interest_rate",
                                             "Interest Rate:",
                                             min = 0, 
                                             max = 25, 
                                             value = default$interest_rate, 
                                             step = 0.1), # end sliderInput
                                 
                                 radioButtons("terms",
                                              "Loan Terms:",
                                              choices = c(15, 30),
                                              inline = TRUE,
                                              selected = default$terms), # end radioButtons
                                 
                                 textInput("tax",
                                           "Property Tax:",
                                           value = default$tax), # end textInput
                                 
                                 sliderInput("hoa",
                                             "HOA Fee:",
                                             min = 0, 
                                             max = 500, 
                                             value = default$hoa, 
                                             step = 10), # end sliderInput
                                 
                                 sliderInput("insurance",
                                             "Insurance:",
                                             min = 0, 
                                             max = 500, 
                                             value = default$insurance, 
                                             step = 10) # end sliderInput           
                             ), # end sidebarPanel
                             # Show a plot of the generated distribution
                             mainPanel(
                                 
                                 fluidRow(
                                     column(width = 10, align = 'left',
                                            textOutput("description"),
                                            tags$head(tags$style("#description{
                                                         font-size: 20px;
                                                         font-style: bold;
                                                         font-family: Noto Sans;
                                                         }"
                                            )
                                            )
                                     ), # end column,
                                     fluidRow(
                                         column(width = 10, align = 'center',
                                                plotOutput("housing", height = 200),
                                                plotOutput("payment", height = 100) 
                                         ) # end column
                                     )
                                 )
                             ) # end mainPanel
                         ) # end siderBarLayout
                ), # end tabPanel
                
                tabPanel("Median Sale Prices",
                         fluidRow(
                             column(
                                 width = 6, align = 'center',
                                 plotOutput("salePlot")  
                             ),
                             
                             column(width = 6, align = 'center',
                                    DT::dataTableOutput("volatilityTable")  
                             ) # end column
                         ) #end fluidRow
                ), # end tabPanel
                
                tabPanel("General Housing Market Info",
                         sidebarLayout(
                             sidebarPanel(
                                 selectInput("metric", "Select a metric:",
                                             choices = unique(df$metrics),
                                             selected = "Avg Sale To List"
                                 ),
                                 tags$head( tags$style( type = "text/css", '
      .irs-line-mid{
                                                        background: #428bca ;
                                                        border: 1px solid #428bca ;
                                                        }
                                                        .irs-line-right{
                                                        background: #428bca ;
                                                        }
                                                        .irs-bar {
                                                        background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
                                                        border-top: 1px solid #CCC ;
                                                        border-bottom: 1px solid #CCC ;
                                                        }
                                                        .irs-bar-edge {
                                                        background: inherit ;
                                                        border: inherit ;
                                                        }
                                                        
                                                        ')),
                                 sliderInput("year", "Select an initial year",
                                             min = 2012,
                                             max = 2018, 
                                             value = 2012
                                 ),
                                 
                                 checkboxGroupInput("nbh", "Select a neighborhood:",
                                                    choices = unique(mod_data$neighborhood),
                                                    selected = "Libertyville"
                                 ),
                                 
                                 verbatimTextOutput("info"),
                                 textAreaInput("inComments", 
                                           label = "Comments:", 
                                           value = "Enter text...",
                                           rows = 8),
                                 
                                 actionButton("load", "Load comments"),
                                 
                                 actionButton("save", "Save comments")
                             ),
                             mainPanel(
                                 plotOutput("metricPlot", click = "plot_click")
                             )
                         ) # end sidebrLayout
                ) # end tabPanel
    )# end tabsetPanel
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # start mortgage tab
    # use this function to collect input parameters
    # to update default yml file
    updateDefault <- reactive({
        data.frame(
            interest_rate = input$interest_rate,
            downpay = input$downpay,
            tax = input$tax,
            terms = input$terms,
            hoa = input$hoa,
            insurance = input$insurance,
            income = input$income,
            price = input$price
        )
    })
    
    # create a detailed description
    output$description <- renderText({
        monthly_income <- as.numeric(input$income)/12 * 1000
        monthly_payment <- calMonthPaymnt(
            house_price = as.numeric(input$price) * 1000, 
            downpay = input$downpay/100, 
            interest_rate = input$interest_rate/100, 
            tax = as.numeric(input$tax), 
            terms = as.numeric(input$terms), 
            hoa = as.numeric(input$hoa), 
            insurance = as.numeric(input$insurance)
        ) %>% round(0)
        out <- paste0(
            "You pay down $", formatC(as.numeric(input$downpay)/100 * as.numeric(input$price) * 1000, format="d", big.mark=","), 
            " for a home of $", formatC(as.numeric(input$price) * 1000, format="d", big.mark=","), ".\n",
            "Your total monthly payment including mortgage, property tax, HOA fee, and insurance will be $", formatC(monthly_payment, format="d", big.mark=","), 
            " as part of your gross monthly income of $", formatC(round(monthly_income), format="d", big.mark=","), "."
        )
    })
    
    # barchart for housing price and down payment
    output$housing <- renderPlot({
        data.frame(
            value = c(
                as.numeric(input$price), 
                as.numeric(input$price) * as.numeric(input$downpay)/100
            ),
            type = c("home price", "down payment")
        ) %>% ggplot(aes(x = type, y = value)) + 
            geom_bar(stat = "identity", fill = "#56B4E9", width = 0.25) + theme(
                panel.background = element_blank(),
                axis.text = element_text(size = 14),
                axis.title = element_text(size = 16)
            ) + coord_flip() + xlab("") + ylab("")
    }, width = 500, height = 150)
    
    # payment pie chart
    output$payment <- renderPlot({
        monthly_income <- as.numeric(input$income)/12 * 1000
        monthly_payment <- calMonthPaymnt(
            house_price = as.numeric(input$price) * 1000, 
            downpay = input$downpay/100, 
            interest_rate = input$interest_rate/100, 
            tax = as.numeric(input$tax), 
            terms = as.numeric(input$terms), 
            hoa = as.numeric(input$hoa), 
            insurance = as.numeric(input$insurance)
        ) %>% round(0)
        
        ggplot(data.frame(
            type = c("monthly payment", "disposable income"),
            value = c(monthly_payment/monthly_income * 100, 
                      (monthly_income - monthly_payment)/monthly_income * 100)
        ), aes(x = "", y = value, fill = type)) + 
            geom_bar(stat = "identity", width = 1) +
            geom_hline(yintercept = c(0, 28), colour = "#E69F00",  size = 2.25) +
            theme(
                panel.background = element_blank(),
                axis.text = element_blank(),
                axis.title = element_blank(), 
                legend.position = "bottom",
                plot.title = element_text(size=20, face="bold"),
                legend.title = element_blank(),
                legend.text = element_text(size = 20),
                plot.caption = element_text(size = 16)
            ) + coord_polar("y") + 
            scale_fill_manual(values=c("#999999", "#56B4E9")) + 
            labs(caption = "** Monthly payment should not exceed 28% of income")
    }, width = 500, height = 400)
    
    # click button to save values
    observeEvent(input$do, {
        write_yaml(updateDefault(), file = "default.yml")
        showNotification("writing to YML file", duration = 5)
    })
    
    ## end mortgage tab
    
    # start sale price tab
    output$salePlot <- renderPlot({
        s <- input$volatilityTable_rows_selected
        
        nbh <- levels(factor(mod_data$neighborhood))[s]
        
        mod_data %>% dplyr::filter(neighborhood %in% nbh) %>% 
            ggplot(aes(
                x = period_end, 
                y = real_median_sale_price, 
                colour = neighborhood)
            ) +  geom_line() + 
            scale_colour_manual(name = NULL,values = myColors) +
            xlab("Month") + ylab("Real Median Sale Prices ($K)") +
            ggtitle("Real Median Housing Price Trend (Current Price)") +
            guides(fill = guide_legend(nrow = 2,byrow = F))
    }, width = 500, height = 500)
    
    output$volatilityTable <- renderDataTable({
        DT::datatable(
            mod_data %>% group_by(neighborhood) %>%
                summarise(
                    volatility = 100 * sd(real_median_sale_price)/mean(real_median_sale_price),
                    sigma = sd(real_median_sale_price)
                ), options = list(dom = 't')
        ) %>% formatRound(columns = c("volatility", "sigma"), digits=2)
    })
    # end sale price tab
    
    # start metrics tab
    output$metricPlot <- renderPlot({
        df %>% dplyr::filter(
            neighborhood %in% input$nbh & 
                metrics == input$metric &
                period_end >= as.Date(paste0(input$year, "-01-01"), "%Y-%m-%d")
        ) %>%
            ggplot(aes(
                x = period_end, 
                y = value, 
                colour = neighborhood)
            ) +  geom_line() + geom_point() + 
            scale_colour_manual(name = NULL,values = myColors) +
            xlab("Month") + ylab("Value") +
            guides(fill = guide_legend(nrow = 2,byrow = F))
    }, width = 600, height = 600)
    
    # use this function to update comments
    saveComments <- reactive({
        data.frame(
            comments = input$inComments
        )
    })
    
    # print dot axes
    output$info <- renderPrint({
        nearPoints(
            df %>% dplyr::filter(
                neighborhood %in% input$nbh & 
                metrics == input$metric &
                period_end >= as.Date(paste0(input$year, "-01-01"), "%Y-%m-%d")
            ) %>% select(neighborhood, period_end, value),
            input$plot_click
        )
    })
    
    observeEvent(input$load, {
        text <- read_yaml(file = "comments.yml")
        updateTextInput(session, "inComments", value = paste(text$comments))
    })
    
    observeEvent(input$save, {
        write_yaml(saveComments(), file = "comments.yml")
        showNotification("writing to YML file", duration = 5)
    })
    # end metrics tab
}

# Run the application 
shinyApp(ui = ui, server = server)