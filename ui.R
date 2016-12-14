
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
      fileInput("mainfile","資料輸入", multiple = TRUE),
      radioButtons("ProcessingType", "使用型態", c("銷售數量", "未出貨數量")),
      uiOutput("SpeceficDate", inline = TRUE),
      textInput("itemname", "商品名稱"),
      textInput("spec", "顏色規格")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      dataTableOutput("minicake")
    )
  )
))