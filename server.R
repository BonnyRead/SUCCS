
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(data.table)
library(magrittr)
library(plyr)
library(dplyr)

shinyServer(function(input, output) {
  
  options(shiny.maxRequestSize=50*1024^2) 

  datafile <- reactive({
    inFile1 <- input$mainfile
    if (is.null (inFile1)) {
      return (NULL)
    } else {
      inFile1 %>% rowwise() %>%
        do({
          read.csv(.$datapath, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
        }) %>% return
    }
  })
  
  filtertingdatafile <- reactive({
    if (is.null (datafile())) {
      return (NULL)
    } else if (input$ProcessingType == "未出貨數量") {
      tmpfile1 <- datafile() %>% setDT
      tmpfile1 <- tmpfile1[訂單日期 >= input$SelectDate[1] &
                           訂單日期 <= input$SelectDate[2]]
      tmpfile1 <- tmpfile1[!(訂單狀態 == "已取消" | 訂單狀態 == "已完成")]
      tmpfile1 <- tmpfile1[送貨狀態 == "備貨中"]
      calculate <- tmpfile1[, .(unshipping = sum(數量)), .(商品名稱, 選項)]
      calculate[is.na(選項), 選項 := ""] %>% return
    } else if (input$ProcessingType == "銷售數量") {
      tmpfile2 <- datafile() %>% setDT()
      tmpfile2 <- tmpfile2[訂單日期 >= input$SelectDate[1] &
                           訂單日期 <= input$SelectDate[2]]
      tmpfile2 <- tmpfile2[訂單狀態 != "已取消", .(sales = sum(數量)),
                               .(商品名稱, 選項)]
    }
  })
  
  output$SpeceficDate <- renderUI({
    if (is.null(datafile())) {
      return (NULL)
    } else {
      date.summary <- datafile() %>% setDT
      maxdate <- date.summary[, max(訂單日期)]
      mindate <- date.summary[, min(訂單日期)]
      dateRangeInput("SelectDate", "選擇日期", start = mindate, end = maxdate,
                    min = mindate)
    }
  })
  
  output$minicake <- renderDataTable({
    if (is.null (filtertingdatafile())) {
      return (NULL)
    } else {
      tmpfile2 <- filtertingdatafile()
      tmpfile2[grepl(input$itemname, 商品名稱) & grepl(input$spec, 選項)] %>% 
        return
    }
  })
})
