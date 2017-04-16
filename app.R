library(shiny)
library(ggplot2)
library(reshape2)
library(dplyr)
library(ggvis)

# Life expectancy
life_expectancy <- read.csv("API_SP.DYN.LE00.IN_DS2_en_csv_v2/API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv", 
                            skip=4, header=TRUE)
life_expectancy <- life_expectancy[c(1, 5:59)]
life_expectancy_region <- read.csv("API_SP.DYN.LE00.IN_DS2_en_csv_v2/Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv", 
                                   header=TRUE)[c("TableName", "Region")]
colnames(life_expectancy_region) <- c("Country.Name", "Region")
life_expectancy <- merge(x=life_expectancy, y=life_expectancy_region, by="Country.Name")
life_expectancy <- melt(life_expectancy, id = c("Country.Name", "Region"))
colnames(life_expectancy) <- c("Country", "Region", "Year", "Life.expectancy")
life_expectancy <- life_expectancy[!is.na(life_expectancy$Life.expectancy), ]

# Fertility rate
fertility_rate <- read.csv("API_SP.DYN.TFRT.IN_DS2_en_csv_v2/API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv", 
                            skip=4, header=TRUE)
fertility_rate <- fertility_rate[c(1, 5:59)]
fertility_rate_region <- read.csv("API_SP.DYN.TFRT.IN_DS2_en_csv_v2/Metadata_Country_API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv", 
                                   header=TRUE)[c("TableName", "Region")]
colnames(fertility_rate_region) <- c("Country.Name", "Region")
fertility_rate <- merge(x=fertility_rate, y=fertility_rate_region, by="Country.Name")
fertility_rate <- melt(fertility_rate, id = c("Country.Name", "Region"))
colnames(fertility_rate) <- c("Country", "Region", "Year", "Fertility.rate")
fertility_rate <- fertility_rate[!is.na(fertility_rate$Fertility.rate), ]

# cleaned data frame
df <- merge(x=life_expectancy, y=fertility_rate, by=c("Country", "Region", "Year"))
df$Year <- as.integer(gsub("X", "", as.character(df$Year)))
df <- df[df$Region != "", ] 
df$Region <- factor(df$Region)
df$Country <- as.character(df$Country)

#################################################################
ui <- fluidPage(
  headerPanel('Fertility rate vs life expectancy'),
  sidebarPanel(
    sliderInput(inputId="num", label="Year", min=min(df$Year), max=max(df$Year), value=min(df$Year), step=1, animate=TRUE),
    selectInput(inputId='region', label='Region', choices=c("ALL", levels(df$Region)), selected="ALL")
  ),
  mainPanel(
    plotOutput('plot1', hover = "plot_hover"),
    verbatimTextOutput("info")
  )
)

server <- function(input, output) {
  selectedData <- reactive({
    if (input$region == "ALL"){
      return (df[df$Year == input$num, ])
    } 
    else{
      return (df[df$Region == input$region & df$Year == input$num, ]) 
    }
  })
  hoverPoint <- reactive({
    return (input$plot_hover)
  })
  output$plot1 <- renderPlot({
    p <- ggplot(data=selectedData(), aes(x=Life.expectancy, y=Fertility.rate, colour=factor(Region))) + 
      geom_point() +
      coord_cartesian(xlim=c(10,90),ylim=c(0.5,9)) 
    return (p)
  })
  output$info <- renderText({
    point <- nearPoints(df=selectedData(), coordinfo=hoverPoint(), threshold=1000,
                        xvar="Life.expectancy", yvar="Fertility.rate", maxpoints=1)
    return (point$Country)
  })
}

shinyApp(ui = ui, server = server)