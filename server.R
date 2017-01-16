
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(data.table)
library(plyr)
library(dplyr)
library(rvest)
library(stringr)
library(checkenc)

shinyServer(function(input, output) {
  
  options(shiny.maxRequestSize = 50 * 1024 ^ 2) 

  datafile <- reactive({
    inFile1 <- input$mainfile
    if (is.null (inFile1)) {
      return (NULL)
    } else {
      coding1 <- lapply(inFile1$datapath, checkenc) %>% unlist %>% unique
      lapply(inFile1$datapath, function(k) {
        read.csv(k, fileEncoding = coding1, stringsAsFactors = FALSE)} ) %>%
        rbind.fill %>% setDT %>% return
    } 
  })
  
  stocking.file <- reactive({
    if (is.null (input$stockfile)) {
      return (NULL)
    } else {
      inFile2 <- input$stockfile
      coding2 <- checkenc(inFile2$datapath)
      read.csv(inFile2$datapath, stringsAsFactors = FALSE,
               fileEncoding = coding2) %>% setDT %>% return
    }
  })
  
  filteringdatafile1 <- reactive({
    if (is.null (datafile())) {
      return (NULL)
    } else if (input$ProcessingType == "未出貨數量") {
      tmpfile1 <- datafile() %>% setDT
      tmpfile1 <- tmpfile1[訂單日期 >= input$SelectDate[1] &
                           訂單日期 <= input$SelectDate[2]]
      tmpfile1 <- tmpfile1[!(訂單狀態 == "已取消" | 訂單狀態 == "已完成")]
      tmpfile1 <- tmpfile1[送貨狀態 == "備貨中"]
      calculate <- tmpfile1[, .(unshipping = sum(數量)), .(商品名稱, 選項)]
      tmpfile2 <- datafile()[, .(商品名稱, 選項, 商店貨號)] %>% unique
      tmpfile2 <- tmpfile2[! duplicated(tmpfile2[, .(商品名稱, 選項)])]
      calculate <- merge(tmpfile2, calculate, by = c("商品名稱", "選項"))
      calculate[is.na(選項), 選項 := ""] %>% return
    } 
  })
  
  filteringdatafile2 <- reactive({
    if (is.null(datafile())) {
      return (NULL)
    } else if (input$ProcessingType == "銷售數量") {
      tmpfile2 <- datafile() %>% setDT()
      tmpfile2 <- tmpfile2[訂單日期 >= input$SelectDate[1] &
                                 訂單日期 <= input$SelectDate[2]]
      tmpfile2[訂單狀態 != "已取消", .(sales = sum(數量)),
                               .(商品名稱, 選項)] %>% return
    }
  })
  
  AllProductName <- reactive({
    if (is.null(stocking.file())) {
      return (NULL)
    } else {
      AllPage <- paste0("http://www.bonnyread.com.tw/products?page=", 1 : 100)
      lapply(AllPage, function(k) {
        k %>% read_html %>%
          html_nodes(xpath = "//div[@class='title text-primary-color']") %>%
          html_text(trim = TRUE)
      }) %>% unlist %>% return
    }
  })
  
  output$SpeceficDate <- renderUI({
    if (is.null(datafile())) {
      return (NULL)
    } else {
      date.summary <- datafile() %>% setDT
      date.summary[, 訂單日期 := as.Date(訂單日期)]
      maxdate <- date.summary[, max(訂單日期)]
      mindate <- date.summary[, min(訂單日期)]
      dateRangeInput("SelectDate", "選擇日期", start = mindate, end = maxdate,
                    min = mindate)
    }
  })
  
  pluto <- reactive({
    if (is.null(datafile())) {
      return (NULL)
    } else if (input$ProcessingType == "未出貨數量") {
      tmpfile2 <- filteringdatafile1()
      tmpfile2[grepl(input$itemname, 商品名稱) & grepl(input$spec, 選項) &
                 grepl(input$itemID, 商店貨號)] %>% return
    } else if (input$ProcessingType == "銷售數量") {
      tmpfile2 <- filteringdatafile2()
      tmpfile2[grepl(input$itemname, 商品名稱) & grepl(input$spec, 選項)] %>% 
        return
    }
  })
  
  martian <- reactive({
    if (is.null (stocking.file())) {
      return (NULL)
    } else {
      pro.stock.file <- stocking.file()
      pro.threshold <- as.numeric(input$threshold)
      Name.On.Website <- AllProductName()
      Name.On.Website <- str_replace_all(Name.On.Website,
                                      "(\\[.*?\\])|\\【.*?\\】", "") %>% trimws
      tempcheck <- pro.stock.file[, .(checkmulti = .N > 1), 商品名稱]
      processed.multi <- pro.stock.file[商品名稱 %in% tempcheck[checkmulti ==
                                        TRUE, 商品名稱], .SD[-1, ], 商品名稱]
      processed.notmulti <- pro.stock.file[商品名稱 %in% tempcheck[checkmulti ==
                                        FALSE, 商品名稱]]
      processed.stockfile <- rbind.fill(list(processed.multi, processed.notmulti),
                                        fill = TRUE) %>% setDT
      processed.stockfile <- processed.stockfile[庫存.數量 < pro.threshold]
      processed.stockfile[, 商品名稱 := str_replace_all(商品名稱,
                                "(\\[.*?\\])|\\【.*?\\】", "" ) %>% trimws]
      processed.stockfile <- processed.stockfile[商品名稱 %in% Name.On.Website]
      processed.stockfile[, .(商品名稱, 選項)]
    }
  })
  
  output$minicake <- renderDataTable({
    pluto()
  })
  
  output$hugecake <- renderDataTable({
    martian()
  })
  
  output$mediumcake <- renderDataTable({
    if (is.null (datafile())) {
      return (NULL)
    } else {
      Z.rack <- datafile() %>% setDT()
      Z.rack <- Z.rack[訂單狀態 != "已取消" & 訂單日期 >= Sys.Date() -15 & 
                   訂單日期 <= Sys.Date() - 1, .(ranking = sum(數量)),
                   .(商品名稱, 選項)] %>% setorder(-ranking) 
      if (input$ExcludeItem != "") {
        exclude.item <- gsub(",", "|", input$ExcludeItem)
        Z.rack <- Z.rack[!grepl(exclude.item, 商品名稱)]
        Z.rack <- Z.rack[1 : 50]
      } else {
        Z.rack <- Z.rack[1 : 50]
      }
      Z.rack$PID <- paste0("Za", rep(1 : 5, each = 10), "-",
                           rep(1 : 10, times = 5))
      Z.rack[, c("PID", "商品名稱", "選項"), with = FALSE] %>% return
    }
  })
  
  output$DownloadWarningFile <- downloadHandler(
    filename = function() {
      paste('LowStocking', 'csv', sep = ".")
    },
    content = function(file) {
      write.csv(martian(), file, row.names = FALSE, quote = FALSE,
                fileEncoding = "big5")
    }
  )
  
  output$PickMe <- downloadHandler(
    filename = function() {
      paste('UnshippingStock', 'csv', sep = ".")
    },
    content = function(file) {
      write.csv(pluto(), file, row.names = FALSE, quote = FALSE,
                fileEncoding = "big5")
    }
  )
})
