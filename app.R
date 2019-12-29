library(shiny)
library(dplyr)
library(httr)
library(rsconnect,warn.conflicts = FALSE)
library(ggplot2)
library(readr)
library(readxl)

rsconnect::setAccountInfo(name="shiners", token="FEF4587DA64BFB8EDFADE946C3D1CF79", 
                          secret="T6JWK7lSGT78RI15Onp3XNQV/L9JxG+jreCVv1Ti")

tmp=tempfile(fileext=".xlsx")

download.file("https://github.com/pjournal/mef03g-ShineRs/blob/master/GNI2.xlsx?raw=true",destfile=tmp,mode='wb')
raw_data=readxl::read_excel(tmp)
file.remove(tmp)



ui<-fluidPage(
  sidebarLayout(
    sidebarPanel(
      h1("Plot Parameters"),
      textInput("title", "Plot title", "Gross national income by gender"),
      selectInput("country", "Country:", choices=raw_data$Country),
      sliderInput("year", "Years:", 1995, 2017, 1, 1, animate = TRUE)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  
  output$plot <- renderPlot({
    
    
    raw_data <- raw_data %>% filter(year<=input$year & Country==input$country)
    
    ggplot(raw_data,aes(x=year,y=values,fill=Gender)) + geom_bar(stat="identity")
  })
}

shinyApp(ui = ui, server = server)
