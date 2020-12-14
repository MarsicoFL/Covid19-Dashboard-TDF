install.packages("shiny")
library(shiny)
library(readxl)
library(ggplot2)
library(datasets)
library(textshape)
library(tidyverse)
library(lubridate)
install.packages("textshape")
View(WorldPhones)

datos <- read_excel("/Users/ceciliaguillametchargue/Desktop/Prueba.xlsx", sheet = 1) 
datos <- spread(datos, key = Ciudad, value = Casos)
datos <- column_to_rownames(datos)
datos <- as.data.frame(datos)
ggplot(datos, aes(x = Mes, y = Casos, group = Ciudad)) + 
  geom_line()

ui <- fluidPage(
  title = "Casos por ciudad",
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "ciudad",
                label = "Ciudad",
                choices = c("Ushuaia", "Rio Grande")
                )
    ),
    mainPanel(
       plotOutput("plot")
              )
      )
)

server <- function(input, output) {
  output$plot <- renderPlot({ 
    barplot(datos[,input$ciudad], 
           main = input$ciudad,
           xlab = "Meses",
           ylab = "Casos")       
    })
}

shinyApp(ui = ui, server = server)
