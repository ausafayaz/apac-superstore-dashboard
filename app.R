
# APAC Superstore Profitability and Market Pressure Dashboard
# Interactive R Shiny dashboard: profitability, discounting and market conditions in APAC (2011-2014)

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)
library(scales)


# Reading Superstore dataset


superstore_raw <- read_excel("data/superstore.xlsx")

superstore <- superstore_raw %>%
  mutate(
    Order_Year = year(Order.Date),
    Order_Month = floor_date(Order.Date, "month"),
    
    Country_WB = case_when(
      Country == "Hong Kong" ~ "Hong Kong SAR, China",
      Country == "Myanmar (Burma)" ~ "Myanmar",
      Country == "South Korea" ~ "Korea, Rep.",
      Country == "Vietnam" ~ "Viet Nam",
      TRUE ~ Country                      #changed country names to match with other datasets
    ),
    
    Profit_Margin = Profit / Sales,
    Shipping_Burden = Shipping.Cost / Sales,
    
    Profit_Status = case_when(
      Profit < 0 ~ "Loss-making",
      TRUE ~ "Profitable"
    ),
    
    Discount_Level = case_when(
      Discount == 0 ~ "No Discount",
      Discount > 0 & Discount <= 0.2 ~ "Low Discount",
      Discount > 0.2 & Discount <= 0.5 ~ "Medium Discount",
      Discount > 0.5 ~ "High Discount"
    )
  ) %>%
  filter(
    Market == "APAC",
    Order_Year >= 2011,
    Order_Year <= 2014
  ) %>%
  select(
    Order_Year,
    Order_Month,
    Country,
    Country_WB,
    Market,
    Region,
    Category,
    Sub.Category,
    Segment,
    Sales,
    Profit,
    Discount,
    Shipping.Cost,
    Quantity,
    Ship.Mode,
    Order.Priority,
    Profit_Margin,
    Shipping_Burden,
    Profit_Status,
    Discount_Level
  )


# cleaning World Bank datasets


clean_world_bank <- function(file_name, value_column_name) {
  
  data <- read.csv(file_name, skip = 4, check.names = FALSE)
  
  data_clean <- data %>%
    select(`Country Name`, `2011`, `2012`, `2013`, `2014`) %>%
    pivot_longer(
      cols = c(`2011`, `2012`, `2013`, `2014`),
      names_to = "Order_Year",
      values_to = value_column_name
    ) %>%
    mutate(
      Order_Year = as.numeric(Order_Year)
    ) %>%
    rename(
      Country_WB = `Country Name`
    )
  
  return(data_clean)
}


# Reading and clean external datasets


population <- clean_world_bank("data/population.csv", "Population")
cpi <- clean_world_bank("data/cpi.csv", "Inflation")
gdp <- clean_world_bank("data/gdp.csv", "GDP_per_Capita")

economic_data <- population %>%
  left_join(cpi, by = c("Country_WB", "Order_Year")) %>%
  left_join(gdp, by = c("Country_WB", "Order_Year"))


# Joining Superstore with external datasets


dashboard_data <- superstore %>%
  left_join(economic_data, by = c("Country_WB", "Order_Year"))

external_unique_data <- dashboard_data %>%
  distinct(
    Country,
    Order_Year,
    Population,
    Inflation,
    GDP_per_Capita
  )


# Filter choices in dashboard 


country_choices <- c("All APAC Countries", sort(unique(dashboard_data$Country)))
category_choices <- c("All Categories", sort(unique(dashboard_data$Category)))
segment_choices <- c("All Segments", sort(unique(dashboard_data$Segment)))


# User Interface


ui <- dashboardPage(
  
  dashboardHeader(title = "APAC Superstore"),
  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    
    hr(),
    
    sliderInput(
      inputId = "year_range",
      label = "Select Year Range:",
      min = 2011,
      max = 2014,
      value = c(2011, 2014),
      step = 1,
      sep = ""
    ),
    
    selectInput(
      inputId = "country_filter",
      label = "Select Country:",
      choices = country_choices,
      selected = "All APAC Countries"
    ),
    
    selectInput(
      inputId = "category_filter",
      label = "Select Category:",
      choices = category_choices,
      selected = "All Categories"
    ),
    
    selectInput(
      inputId = "segment_filter",
      label = "Select Segment:",
      choices = segment_choices,
      selected = "All Segments"
    ),
    
    selectInput(
      inputId = "metric_filter",
      label = "Country Comparison Metric:",
      choices = c(
        "Profit Margin",
        "Total Sales",
        "Total Profit",
        "Shipping Burden",
        "Sales per Million People"
      ),
      selected = "Profit Margin"
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        .box { border-radius: 6px; }
        .small-note { font-size: 13px; line-height: 1.4; color: #333333; }
      "))
    ),
    
    tabItems(
      
      tabItem(
        tabName = "dashboard",
        
        fluidRow(
          box(
            width = 12,
            title = "What This Dashboard Solves",
            status = "primary",
            solidHeader = TRUE,
            p(tags$b("Problem: "), 
              "In the APAC region, high sales do not always mean strong performance. Profit can be reduced by discounts, shipping cost, and different market conditions across countries."),
            p(tags$b("Dashboard solution: "), 
              "This dashboard combines Superstore transaction data with World Bank population, inflation, and GDP per capita data to identify profitable markets, weak areas, and market pressure."),
            p(tags$b("Expected result: "), 
              "Managers can compare APAC countries, product categories, discount risk, sales mix, and inflation pressure before making pricing, discount, and shipping decisions.")
          )
        ),
        
        fluidRow(
          valueBoxOutput("sales_box", width = 3),
          valueBoxOutput("profit_box", width = 3),
          valueBoxOutput("margin_box", width = 3),
          valueBoxOutput("shipping_box", width = 3)
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "1. Category Profit Margin Trend",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("category_margin_plot", height = "420px"),
            hr(),
            p(class = "small-note",
              tags$b("How this answers the problem: "),
              "This chart shows whether each category is becoming more or less profitable over time. It helps reveal whether sales performance is supported by healthy profit margins, instead of only looking at revenue."
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "2. APAC Country Performance with Market Context",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("country_plot", height = "600px"),
            hr(),
            p(class = "small-note",
              tags$b("How this answers the problem: "),
              "This chart compares APAC countries using the selected performance metric. Profit Margin is calculated as Total Profit divided by Total Sales, multiplied by 100. Shipping Burden is calculated as Shipping Cost divided by Total Sales, multiplied by 100. Sales per Million People uses the population dataset to adjust sales by market size. The GDP grouping adds external market context so performance can be compared with economic conditions."
            )
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "3. Profit Status by Discount Level",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("discount_plot", height = "390px"),
            hr(),
            p(class = "small-note",
              tags$b("How this answers the problem: "),
              "This chart shows whether higher discounts are linked with a larger share of loss-making orders. It directly supports the discount-risk part of the problem."
            )
          ),
          
          box(
            width = 6,
            title = "4. Sales Share by Category",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("category_share_plot", height = "390px"),
            hr(),
            p(class = "small-note",
              tags$b("How this answers the problem: "),
              "This chart shows which categories dominate APAC sales. When compared with the profit margin trend, it helps identify whether major sales categories are also commercially strong."
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "5. Average Inflation vs Profit Margin",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("inflation_plot", height = "430px"),
            hr(),
            p(class = "small-note",
              tags$b("How this answers the problem: "),
              "This chart connects external market pressure with Superstore profitability. Each bubble represents one country, bubble size shows total sales, and colour shows GDP context. It helps check whether countries with higher average inflation also show weaker profit margins."
            )
          )
        )
      ),
      
      tabItem(
        tabName = "about",
        
        fluidRow(
          box(
            width = 12,
            title = "Dataset and Cleaning Notes",
            status = "primary",
            solidHeader = TRUE,
            p("The Superstore dataset was filtered to the APAC market and the years 2011 to 2014 using Order Date."),
            p("The external datasets are World Bank population, inflation, and GDP per capita files. They are cleaned inside the app and joined to Superstore using country and year."),
            p("External data averages are calculated from unique country-year rows to avoid repeating GDP, population, and inflation values across transaction rows."),
            p("Some country names were adjusted to match World Bank naming. Hong Kong was matched with Hong Kong SAR, China. South Korea was matched with Korea, Rep. Myanmar (Burma) was matched with Myanmar. Vietnam was matched with Viet Nam."),
            p("Taiwan may not appear in external-data comparison charts because it is not available in the selected World Bank datasets.")
          )
        )
      )
    )
  )
)

# 
# 7.for filtering years etc in the dashboard
# 

server <- function(input, output) {
  
  filtered_data <- reactive({
    
    data <- dashboard_data %>%
      filter(
        Order_Year >= input$year_range[1],
        Order_Year <= input$year_range[2]
      )
    
    if (input$country_filter != "All APAC Countries") {
      data <- data %>%
        filter(Country == input$country_filter)
    }
    
    if (input$category_filter != "All Categories") {
      data <- data %>%
        filter(Category == input$category_filter)
    }
    
    if (input$segment_filter != "All Segments") {
      data <- data %>%
        filter(Segment == input$segment_filter)
    }
    
    return(data)
  })
  
  filtered_external_data <- reactive({
    
    data <- external_unique_data %>%
      filter(
        Order_Year >= input$year_range[1],
        Order_Year <= input$year_range[2]
      )
    
    if (input$country_filter != "All APAC Countries") {
      data <- data %>%
        filter(Country == input$country_filter)
    }
    
    return(data)
  })
  
  
  # Summary boxes
  
  
  output$sales_box <- renderValueBox({
    
    total_sales <- sum(filtered_data()$Sales, na.rm = TRUE)
    
    valueBox(
      value = dollar(total_sales),
      subtitle = "Total Sales",
      color = "blue"
    )
  })
  
  output$profit_box <- renderValueBox({
    
    total_profit <- sum(filtered_data()$Profit, na.rm = TRUE)
    
    valueBox(
      value = dollar(total_profit),
      subtitle = "Total Profit",
      color = "green"
    )
  })
  
  output$margin_box <- renderValueBox({
    
    margin <- sum(filtered_data()$Profit, na.rm = TRUE) /
      sum(filtered_data()$Sales, na.rm = TRUE)
    
    valueBox(
      value = paste0(round(margin * 100, 1), "%"),
      subtitle = "Profit Margin",
      color = "yellow"
    )
  })
  
  output$shipping_box <- renderValueBox({
    
    shipping_burden <- sum(filtered_data()$Shipping.Cost, na.rm = TRUE) /
      sum(filtered_data()$Sales, na.rm = TRUE)
    
    valueBox(
      value = paste0(round(shipping_burden * 100, 1), "%"),
      subtitle = "Shipping Cost / Sales",
      color = "red"
    )
  })
  
  
  # Chart 1: Category profit margin trend
  
  
  output$category_margin_plot <- renderPlotly({
    
    margin_data <- filtered_data() %>%
      group_by(Order_Year, Category) %>%
      summarise(
        Total_Sales = sum(Sales, na.rm = TRUE),
        Total_Profit = sum(Profit, na.rm = TRUE),
        Profit_Margin = Total_Profit / Total_Sales,
        .groups = "drop"
      )
    
    p <- ggplot(
      margin_data,
      aes(
        x = Order_Year,
        y = Profit_Margin,
        colour = Category,
        group = Category,
        text = paste(
          "Year:", Order_Year,
          "<br>Category:", Category,
          "<br>Profit Margin:", round(Profit_Margin * 100, 1), "%",
          "<br>Total Sales:", dollar(Total_Sales),
          "<br>Total Profit:", dollar(Total_Profit)
        )
      )
    ) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      scale_x_continuous(breaks = c(2011, 2012, 2013, 2014)) +
      scale_y_continuous(labels = function(x) paste0(round(x * 100, 0), "%")) +
      labs(
        x = "Year",
        y = "Profit Margin",
        colour = "Category"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # Chart 2: Country performance with market context
  
  
  output$country_plot <- renderPlotly({
    
    business_country_data <- filtered_data() %>%
      group_by(Country) %>%
      summarise(
        Total_Sales = sum(Sales, na.rm = TRUE),
        Total_Profit = sum(Profit, na.rm = TRUE),
        Profit_Margin = sum(Profit, na.rm = TRUE) / sum(Sales, na.rm = TRUE),
        Shipping_Burden = sum(Shipping.Cost, na.rm = TRUE) / sum(Sales, na.rm = TRUE),
        .groups = "drop"
      )
    
    external_country_data <- filtered_external_data() %>%
      group_by(Country) %>%
      summarise(
        Average_Population = mean(Population, na.rm = TRUE),
        Average_GDP_per_Capita = mean(GDP_per_Capita, na.rm = TRUE),
        .groups = "drop"
      )
    
    country_data <- business_country_data %>%
      left_join(external_country_data, by = "Country") %>%
      mutate(
        Sales_per_Million_People = Total_Sales / (Average_Population / 1000000),
        GDP_Group = case_when(
          is.na(Average_GDP_per_Capita) ~ "GDP data unavailable",
          Average_GDP_per_Capita >= 40000 ~ "High GDP per capita",
          Average_GDP_per_Capita >= 10000 & Average_GDP_per_Capita < 40000 ~ "Medium GDP per capita",
          Average_GDP_per_Capita < 10000 ~ "Lower GDP per capita"
        )
      )
    
    if (input$metric_filter == "Profit Margin") {
      country_data <- country_data %>%
        mutate(
          Metric_Value = Profit_Margin * 100,
          Metric_Label = paste0(round(Profit_Margin * 100, 1), "%")
        )
      y_label <- "Profit Margin (%)"
    }
    
    if (input$metric_filter == "Total Sales") {
      country_data <- country_data %>%
        mutate(
          Metric_Value = Total_Sales,
          Metric_Label = dollar(Total_Sales)
        )
      y_label <- "Total Sales"
    }
    
    if (input$metric_filter == "Total Profit") {
      country_data <- country_data %>%
        mutate(
          Metric_Value = Total_Profit,
          Metric_Label = dollar(Total_Profit)
        )
      y_label <- "Total Profit"
    }
    
    if (input$metric_filter == "Shipping Burden") {
      country_data <- country_data %>%
        mutate(
          Metric_Value = Shipping_Burden * 100,
          Metric_Label = paste0(round(Shipping_Burden * 100, 1), "%")
        )
      y_label <- "Shipping Cost as % of Sales"
    }
    
    if (input$metric_filter == "Sales per Million People") {
      country_data <- country_data %>%
        mutate(
          Metric_Value = Sales_per_Million_People,
          Metric_Label = round(Sales_per_Million_People, 2)
        )
      y_label <- "Sales per Million People"
    }
    
    country_data <- country_data %>%
      filter(!is.na(Metric_Value)) %>%
      arrange(Metric_Value)
    
    if (nrow(country_data) == 1) {
      
      p <- ggplot(
        country_data,
        aes(
          x = Country,
          y = Metric_Value,
          fill = GDP_Group,
          text = paste(
            "Country:", Country,
            "<br>Selected Metric:", input$metric_filter,
            "<br>Metric Value:", Metric_Label,
            "<br>Total Sales:", dollar(Total_Sales),
            "<br>Total Profit:", dollar(Total_Profit),
            "<br>Profit Margin:", paste0(round(Profit_Margin * 100, 1), "%"),
            "<br>Shipping Burden:", paste0(round(Shipping_Burden * 100, 1), "%"),
            "<br>Average GDP per Capita:", dollar(Average_GDP_per_Capita)
          )
        )
      ) +
        geom_col(width = 0.35) +
        labs(
          x = "Selected Country",
          y = y_label,
          fill = "GDP Context"
        ) +
        theme_minimal()
      
    } else {
      
      p <- ggplot(
        country_data,
        aes(
          x = reorder(Country, Metric_Value),
          y = Metric_Value,
          fill = GDP_Group,
          text = paste(
            "Country:", Country,
            "<br>Selected Metric:", input$metric_filter,
            "<br>Metric Value:", Metric_Label,
            "<br>Total Sales:", dollar(Total_Sales),
            "<br>Total Profit:", dollar(Total_Profit),
            "<br>Profit Margin:", paste0(round(Profit_Margin * 100, 1), "%"),
            "<br>Shipping Burden:", paste0(round(Shipping_Burden * 100, 1), "%"),
            "<br>Average GDP per Capita:", dollar(Average_GDP_per_Capita)
          )
        )
      ) +
        geom_col(width = 0.6) +
        coord_flip() +
        labs(
          x = "Country",
          y = y_label,
          fill = "GDP Context"
        ) +
        theme_minimal()
    }
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # Chart 3: Profit status by discount level
  
  
  output$discount_plot <- renderPlotly({
    
    discount_data <- filtered_data() %>%
      mutate(
        Discount_Level = factor(
          Discount_Level,
          levels = c("No Discount", "Low Discount", "Medium Discount", "High Discount")
        )
      ) %>%
      group_by(Discount_Level, Profit_Status) %>%
      summarise(
        Number_of_Orders = n(),
        .groups = "drop"
      )
    
    p <- ggplot(
      discount_data,
      aes(
        x = Discount_Level,
        y = Number_of_Orders,
        fill = Profit_Status,
        text = paste(
          "Discount Level:", Discount_Level,
          "<br>Profit Status:", Profit_Status,
          "<br>Number of Orders:", Number_of_Orders
        )
      )
    ) +
      geom_col(position = "fill") +
      scale_y_continuous(labels = function(x) paste0(round(x * 100, 0), "%")) +
      labs(
        x = "Discount Level",
        y = "Share of Orders",
        fill = "Profit Status"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # Chart 4: Sales share by category
  
  
  output$category_share_plot <- renderPlotly({
    
    category_data <- filtered_data() %>%
      group_by(Category) %>%
      summarise(
        Total_Sales = sum(Sales, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Sales_Share = Total_Sales / sum(Total_Sales)
      )
    
    plot_ly(
      category_data,
      labels = ~Category,
      values = ~Total_Sales,
      type = "pie",
      textinfo = "label+percent",
      hoverinfo = "text",
      text = ~paste(
        "Category:", Category,
        "<br>Total Sales:", dollar(Total_Sales),
        "<br>Sales Share:", round(Sales_Share * 100, 1), "%"
      )
    )
  })
  
  
  # Chart 5: Average inflation vs profit margin
  
  
  output$inflation_plot <- renderPlotly({
    
    business_inflation_data <- filtered_data() %>%
      group_by(Country) %>%
      summarise(
        Total_Sales = sum(Sales, na.rm = TRUE),
        Total_Profit = sum(Profit, na.rm = TRUE),
        Profit_Margin = sum(Profit, na.rm = TRUE) / sum(Sales, na.rm = TRUE),
        .groups = "drop"
      )
    
    external_inflation_data <- filtered_external_data() %>%
      filter(!is.na(Inflation)) %>%
      group_by(Country) %>%
      summarise(
        Average_Inflation = mean(Inflation, na.rm = TRUE),
        Average_GDP_per_Capita = mean(GDP_per_Capita, na.rm = TRUE),
        .groups = "drop"
      )
    
    inflation_data <- business_inflation_data %>%
      left_join(external_inflation_data, by = "Country") %>%
      filter(!is.na(Average_Inflation)) %>%
      mutate(
        GDP_Group = case_when(
          is.na(Average_GDP_per_Capita) ~ "GDP data unavailable",
          Average_GDP_per_Capita >= 40000 ~ "High GDP per capita",
          Average_GDP_per_Capita >= 10000 & Average_GDP_per_Capita < 40000 ~ "Medium GDP per capita",
          Average_GDP_per_Capita < 10000 ~ "Lower GDP per capita"
        )
      )
    
    p <- ggplot(
      inflation_data,
      aes(
        x = Average_Inflation,
        y = Profit_Margin,
        colour = GDP_Group,
        size = Total_Sales,
        text = paste(
          "Country:", Country,
          "<br>GDP Context:", GDP_Group,
          "<br>Average Inflation:", round(Average_Inflation, 2), "%",
          "<br>Profit Margin:", round(Profit_Margin * 100, 1), "%",
          "<br>Total Sales:", dollar(Total_Sales),
          "<br>Total Profit:", dollar(Total_Profit),
          "<br>Average GDP per Capita:", dollar(Average_GDP_per_Capita)
        )
      )
    ) +
      geom_point(alpha = 0.75) +
      scale_y_continuous(labels = function(x) paste0(round(x * 100, 0), "%")) +
      labs(
        x = "Average Inflation Rate for Selected Years",
        y = "Profit Margin for Selected Years",
        colour = "GDP Context",
        size = "Bubble Size: Total Sales"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
}


shinyApp(ui = ui, server = server)