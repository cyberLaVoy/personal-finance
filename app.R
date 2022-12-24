library(shiny)
library(shinythemes)
library(here)
library(DT)
library(ggplot2)
library(scales)

source(here::here("R", 'munge.R'))

ui <- fluidPage( theme=shinytheme("flatly"),
  
  titlePanel("To the Moon!"),
  sidebarLayout(
    # UI INPUT ####
    sidebarPanel(
       sliderInput("years", 
                  "Projected Years",
                  min=0,
                  max=72,
                  value=12),     
      numericInput("net_worth", 
                   "Initial Net Worth",
                   net_worth),
      numericInput("net_monthly_income", 
                   "Net Monthly Income",
                   net_monthly_income),
      sliderInput("assests_contribution_percentage", 
                  "Uniform Assets Contribution Percentage",
                  min=0,
                  max=100,
                  value=100), 
      sliderInput("crypto_contribution_percentage", 
                  "Uniform Crypto Contribution Percentage",
                  min=0,
                  max=100,
                  value=50),
      numericInput("airdrop_amount", 
                   "Airdrop Amount",
                   0)     
    ),
    # UI OUTPUT ####
    mainPanel(
      tabsetPanel(
        tabPanel("Wealth Projection",
                 wellPanel(
                   h3("Projection"),
                   plotOutput("wealth_projection_plot"),
                   h3("Totals by Type"),
                   plotOutput("totals_summary_plot"),
                   h3("Assets by Type"),
                   plotOutput("assets_summary_plot"),
                   h3("Debts by Type"),
                   plotOutput("debts_summary_plot")
                 )
        ),
        tabPanel("Data",
                 wellPanel(
                   h3("Wealth Projection"),
                   DT::dataTableOutput("wealth_projection_table"),
                   h3("Assets"),
                   DT::dataTableOutput("assets_data_table"),
                   h3("Debts"),
                   DT::dataTableOutput("debts_data_table"),
                   h3("Income"),
                   DT::dataTableOutput("income_data_table"),
                   h3("Expenses"),
                   DT::dataTableOutput("expenses_data_table")
                 )
        ),
        tabPanel("Metrics",
                 wellPanel(
                   h3("Summary"),
                   p( paste("Net Worth:", comma(net_worth)) ),
                   p( paste("Passive Monthly Change:", comma(passive_monthly_change)) ),
                   p( paste("Speculative Passive Monthly Change:", comma(speculative_passive_monthly_change)) ),
                   p( paste("Net Monthly Income:", comma(net_monthly_income)) ),
                   p( paste("Net Monthly Compound Interest:", comma(net_monthly_compound_interest)) ),
                   
                   h3("Income"),
                   p( paste("Total Monthly Income:", comma(total_monthly_income)) ),
                   p( paste("Monthly Passive Income:", comma(monthly_passive_income)) ),
                   p( paste("Monthly Speculative Passive Income:", comma(monthly_speculative_passive_income)) ),
                   
                   h3("Expenses"),
                   p( paste("Total Monthly Expenses:", comma(total_monthly_expenses)) ),
                   p( paste("Total ACH Required Monthly Expenses:", comma(total_monthly_ach_required_expenses)) ),
                   
                   h3("Assets"),
                   p( paste("Total Assets:", comma(total_assets)) ),
                   p( paste("Monthly Assets Compound Interest:", comma(monthly_assets_compound_interest)) ),
                   p( paste("Monthly Speculative Return:", comma(monthly_speculative_return)) ),
                   
                   h3("Debts"),
                   p( paste("Total Debts:", comma(total_debts)) ),
                   p( paste("Monthly Debts Compound Interest:", comma(monthly_debts_compound_interest)) )
                   
                 )
        ),
        tabPanel("Contributions",
                 wellPanel( h4("Uniform Monthly Contributions"),
                            htmlOutput("contributions"),
                            h4("Air Drop Contributions"),
                            DT::dataTableOutput("airdrop_contributions_table")
                 )
        )
      )
    )
  )
)

server <- function(input, output) {
  # REACTIVE ELEMENTS ####
  reactive_airdrop_contributions <- reactive({
    calculate_airdrop_assets_contributions(input$airdrop_amount, input$crypto_contribution_percentage, assets) %>%
      filter(contribution > 0)
  }) 
  reactive_contributions <- reactive({
    contributions <- calculate_contributions(input$assests_contribution_percentage, 
                                             input$crypto_contribution_percentage,
                                             input$net_monthly_income)    
    tags$div(
          p(paste("Crypto:", contributions$crypto)),
          p(paste("Stocks:", contributions$stock)),
          p(paste("Traditional Savings:", contributions$usd))
    )
  })
  reactive_wealth_projection <- reactive({
     calculate_wealth_projection(assets, 
                                 debts, 
                                 input$years, 
                                 input$assests_contribution_percentage, 
                                 input$crypto_contribution_percentage,
                                 input$net_monthly_income,
                                 input$airdrop_amount,
                                 input$net_worth,
                                 "With Airdrop")      
  })
  reactive_wealth_projection_minus_airdop <- reactive({
    calculate_wealth_projection(assets, 
                               debts, 
                               input$years, 
                               input$assests_contribution_percentage, 
                               input$crypto_contribution_percentage,
                               input$net_monthly_income,
                               0,
                               input$net_worth,
                               "Without Airdrop")     
  })
 
  
  # DATA TABLE RENDER ####
  output$assets_data_table <- DT::renderDataTable(assets, rownames=FALSE)
  output$debts_data_table <- DT::renderDataTable(debts, rownames=FALSE)
  output$income_data_table <- DT::renderDataTable(income, rownames=FALSE)
  output$expenses_data_table <- DT::renderDataTable(expenses, rownames=FALSE)
  
  output$airdrop_contributions_table <- DT::renderDataTable(reactive_airdrop_contributions(), rownames=FALSE)
  output$wealth_projection_table <- DT::renderDataTable( reactive_wealth_projection()$wealth_projection, rownames=FALSE)
 
  # UI RENDER ####
  output$contributions <- renderUI({
    reactive_contributions()
  })

  # PLOT RENDER ####
  output$wealth_projection_plot <- renderPlot({
    df <- reactive_wealth_projection()$wealth_projection
    df_minus_airdrop <- reactive_wealth_projection_minus_airdop()$wealth_projection
    binded_df <- rbind(df, df_minus_airdrop)
    ggplot( binded_df, 
            aes(x=month, y=net_worth, mapping=label, color=label) ) +
      geom_line() +
      geom_label(data = filter(df, 
                               net_worth == last(net_worth)), 
                 aes(label = comma(net_worth)) ) +
      geom_label(data = filter(df_minus_airdrop, 
                               net_worth == last(net_worth)), 
                 aes(label = comma(net_worth)) ) +     
      scale_y_continuous(labels = comma)
  })
  output$totals_summary_plot <- renderPlot({
    wealth_projection <- reactive_wealth_projection()
    ggplot(get_totals_by_type(wealth_projection$assets, wealth_projection$debts), aes(x=type, y=total_overall, fill=type)) +
      geom_bar(stat="identity") +
      geom_text( aes(label=comma(total_overall)), vjust=-.3 )
  })
  output$assets_summary_plot <- renderPlot({
    wealth_projection <- reactive_wealth_projection()
    ggplot(get_totals_by_type(wealth_projection$assets, wealth_projection$debts), aes(x=type, y=total_asset, fill=type)) +
      geom_bar(stat="identity") +
      geom_text( aes(label=comma(total_asset)), vjust=-.3 )
  }) 
  output$debts_summary_plot <- renderPlot({
    wealth_projection <- reactive_wealth_projection()
    ggplot(get_totals_by_type(wealth_projection$assets, wealth_projection$debts), aes(x=type, y=total_debt, fill=type)) +
      geom_bar(stat="identity") +
      geom_text( aes(label=comma(total_debt)), vjust=-.3 )
  })  
  
}

shinyApp(ui, server, options=list("host"="0.0.0.0", port=4721))
