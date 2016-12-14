
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Sales and Unshipped Commodities Checking System"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("ProcessingType", "使用型態", 
                  c("銷售數量", "未出貨數量", "存貨過低警示")),
      conditionalPanel(condition = "input.ProcessingType != '存貨過低警示'",
                       fileInput("mainfile", "資料輸入", multiple = TRUE),
                       uiOutput("SpeceficDate", inline = TRUE),
                       textInput("itemname", "商品名稱"),
                       textInput("spec", "顏色規格")),
      conditionalPanel(condition = "input.ProcessingType == '存貨過低警示'",
                       fileInput("stockfile", "資料輸入", width = "100%"),
                       textInput("threshold", "警示門檻", width = "100%"))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      dataTableOutput("minicake"),
      dataTableOutput("hugecake")
    )
  )
))
