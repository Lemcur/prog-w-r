library(shinydashboard)
library(WDI)


# DATA PREPARING
countries <- c(
  "AR", "AM", "AT", "BE", "BG", "CO", "CZ", "DK", "DO", "EG",
  "EE", "FI", "FR", "GR", "HN", "HU", "IS", "ID", "IT", "JO",
  "LV", "LT", "LU", "MW", "MT", "MX", "MD", "MN", "NP", "NL",
  "NG", "NO", "PA", "PY", "PL", "RO", "SK", "SI", "ZA", "ES",
  "SE", "TH", "TN", "GB", "UY"
)

describing_indicators <- c(
  'gdp_per_capita' = 'NY.GDP.PCAP.CD',
  'gdp_per_capita_ppp' = 'NY.GDP.PCAP.PP.CD',
  'life_expectancy' = 'SP.DYN.LE00.IN',
  'income_inequality' = 'SI.POV.GINI',
  'enrollment_tetriary' = 'SE.TER.ENRR'
)
work_indicators <- c(
  'weekly_hours_worked_15_64' = 'JI.TLF.1564.WK.TM'
)

describing_data_2010 = WDI(country=countries, indicator=describing_indicators, start=2010, end=2010)
work_data_data_2010 = WDI(country=countries, indicator=work_indicators, start=2010, end=2010)
# data clearing
merged_data <- merge(describing_data_2010, work_data_data_2010, by="country")
null_cleared <- merged_data[complete.cases(merged_data), ] # I left an example of how I removed NULLs from my data sets
data <- null_cleared[c('country', 'iso2c.x', 'gdp_per_capita', 'gdp_per_capita_ppp', 'life_expectancy', 'income_inequality', 'enrollment_tetriary', 'weekly_hours_worked_15_64')]
# d <- data[c('gdp_per_capita', 'gdp_per_capita_ppp', 'life_expectancy', 'income_inequality', 'enrollment_tetriary', 'weekly_hours_worked_15_64')]


ui <- dashboardPage(
  dashboardHeader(title = "Statystyka opisowa"),
  dashboardSidebar(
    # dodanie menu bocznego
    sidebarMenu(
      menuItem("Statystyki", tabName = "Statystyki", icon = icon("chart-bar")),
      menuItem("Histogram", icon = icon("chart-bar"), tabName = "Histogram",
               badgeLabel = "Histogram", badgeColor = "red"),
      menuItem("Boxplot", icon = icon("chart-bar"), tabName = "Boxplot",
               badgeLabel = "Boxplot", badgeColor = "olive"),
      menuItem("Wykres", icon = icon("chart-bar"), tabName = "Wykres",
               badgeLabel = "Wykres", badgeColor = "light-blue"),
      menuItem("Korelacja", icon = icon("chart-bar"), tabName = "Korelacja",
               badgeLabel = "Korelacja", badgeColor = "yellow")
    ), # koniec menu
    # dodajemy wejście
    selectInput(
      'vector', h3("Wybierz wektor"),
      choices = c(
        'gdp_per_capita',
        'gdp_per_capita_ppp',
        'life_expectancy',
        'income_inequality',
        'enrollment_tetriary',
        'weekly_hours_worked_15_64'
      ),
      multiple=FALSE, selected='gdp_per_capita'
    ),
    selectInput(
      'country', h3("Wybierz kraj"),
      choices = countries,
      multiple = TRUE, selected = countries[1:5]
    ),
    actionButton("select_all", "Zaznacz wszystkie"),
    actionButton("unselect_all", "Odznacz wszystkie")
  ),
  dashboardBody(
    # Boxes powinny być w wierszach lub kolumnach
    fluidRow(
      tabItems(
        tabItem(tabName = "Statystyki",
                h2("Statystyki opisowe"),
                box(
                  tableOutput("op"),
                  # wyświetlenie tekstu (kwantyli)
                  h3(verbatimTextOutput('print1'))
                )
        ),
        tabItem(tabName = "Histogram",
                h2("Histogram"),
                plotOutput("plot1", height = 550, width=450)
        ),
        tabItem(tabName = "Boxplot",
                h2("Wykres ramka z wąsami"),
                plotOutput("plot2", height =450,width = 450)
        ),
        tabItem(tabName = "Wykres",
                plotOutput("plot3", height = 450, width = 450)
        ),
        tabItem(tabName = "Korelacja",
                h2("Korelacja paramertrow"),
                box(
                  h3(verbatimTextOutput('correlation'))
                )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  observeEvent(input$select_all, {
    updateSelectInput(session, "country", selected = countries)
  })

  observeEvent(input$unselect_all, {
    updateSelectInput(session, "country", selected = c(""))
  })
  
  # statystyki opisowe
  output$op <-renderTable({
    selected_data <- data[data$iso2c.x %in% input$country, ]
    data.frame(cbind(c("Długość wektora","Minimum","Średnia","Odchylenie standardowe",
                       "Mediana","Medianowe odchylenie bezwzgledne","Maksimum"),
                     c(length(selected_data[, input$vector]),
                       min(selected_data[, input$vector]),
                       mean(selected_data[, input$vector]),
                       sd(selected_data[, input$vector]),
                       median(selected_data[, input$vector]),
                       mad(selected_data[, input$vector]),
                       max(selected_data[, input$vector])
                     )
    )
    )
  })
  # liczenie kwantyli
  output$print1 <- renderPrint({
    selected_data <- data[data$iso2c.x %in% input$country, ]
    print("Kwantyle")
    quantile(selected_data[, input$vector]) # TODO: Fix this width
  })

  output$plot1 <- renderPlot({
    selected_data <- data[data$iso2c.x %in% input$country, ]
    hist(selected_data[, input$vector], col="lightblue",border = "darkblue",probability = T)
    lines(density(selected_data[, input$vector]),col="red",lty="dotdash",lwd=3)
    grid()
  })

  output$plot2 <- renderPlot({
    selected_data <- data[data$iso2c.x %in% input$country, ]
    boxplot(selected_data[, input$vector], col='red', border="darkblue",horizontal = T)
  })
  # poprawione dane w wykresie ;  xaxt = "n" oraz dodanie własnych etykiet
  output$plot3 <- renderPlot({
    selected_data <- data[data$iso2c.x %in% input$country, ]
    plot(selected_data[, input$vector], col='red', type="b", pch=19)

  })

  output$correlation <- renderPrint({
    selected_data <- data[data$iso2c.x %in% input$country, c('gdp_per_capita', 'gdp_per_capita_ppp', 'life_expectancy', 'income_inequality', 'enrollment_tetriary', 'weekly_hours_worked_15_64')]
    print("Korelacja")
    cor(selected_data[, -1])
  })
}

shinyApp(ui, server)
