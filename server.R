pacman::p_load(shiny, shinythemes, leaflet, sp, DT, formattable, RColorBrewer, dichromat, dplyr)
load('election.results_leaflet.RData')

function(input, output) {
  
  ###################################################
  ############ The results tab functions ############
  ###################################################
  
  current.party <- ''
  
  sketch_results = htmltools::withTags(table(
    class = 'stripe',
    tableHeader(c("Округ", "Результат, %"))
  ))
  
  filteredData_results <- reactive({
    party.column <- paste0(input$party, ".pct")
    russian.election.districts@data <- select(russian.election.districts@data,
                                              district, party =one_of(party.column))
    results.interval <- russian.election.districts@data$party %>% range()
    if (current.party != party.column){
      req(input$results_range[2] == results.interval[2], cancelOutput = T)
      current.party <<- party.column
    }
    russian.election.districts$color <- colorBin("Greens", results.interval, 
                                                 20)(russian.election.districts@data$party)
    low.tail <-  russian.election.districts$party < input$results_range[1] 
    upper.tail <- russian.election.districts$party > input$results_range[2] 
    if (any(low.tail))
      russian.election.districts@data[low.tail,]$color <- "#3399FF"
    if (any(upper.tail))  
      russian.election.districts@data[upper.tail,]$color <- "#FF9933"
    russian.election.districts$popups <- paste0(russian.election.districts$district, 
                                                "<br><strong>Партия: </strong>",
                                                names(which(parties.titles == input$party)),
                                                "<br><strong>Результат: </strong>",
                                                russian.election.districts$party, "%")
    
    russian.election.districts
  })
  
  output$party_result <- renderText({
    HTML(paste0('<h4>Результат: ',total.results[input$party], '%</h4>'))
  })
  
  output$sources <- renderText({
    HTML(paste0('Источники:', '<br>',
                'результаты голосования - ','<a href="http://www.vybory.izbirkom.ru/region/region/izbirkom?action=show&root=1&tvd=100100067795854&vrn=100100067795849&region=0&global=1&sub_region=0&prver=0&pronetvd=0&vibid=100100067795854&type=233"><b>ЦИК РФ</b></a>', '<br>',
                'геоданные округов - ', '<a href="http://gis-lab.info/qa/duma2016.html"><b>GIS-Lab</b></a>'
    ))
  })
  
  # Define here the slider widget, because its limits depend on the selected party
  output$slider_results <-  renderUI({
    x <- select(russian.election.districts@data, one_of(paste0(input$party, ".pct"))) %>%
      range()
    sliderInput("results_range", "Фильтр",
                x[1], x[2],
                value= x,
                ticks = FALSE)
  })
  
  
  output$hist_results <- renderPlot({
    x <- subset(filteredData_results()@data, color != "#3399FF" & color != "#FF9933")
    if (nrow(x) == 0)
      return(NULL)
    
    hist(x$party,
         xlim = range(x$party),
         main = paste0("Отобрано округов: ", nrow(x)),
         xlab = "Полученные проценты",
         ylab = "Число округов",
         col = '#00DD00',
         border = 'white')
  })
  
  output$RusMap_results <- renderLeaflet({
    leaflet(filteredData_results())  %>%
      setView(lng = 99, lat = 65, zoom = 3) %>% 
      addPolygons(fillColor = ~color, 
                  fillOpacity = 0.8, 
                  color = "black", 
                  weight = 1,
                  popup = ~popups)
  })
  
  output$MskMap_results <- renderLeaflet({
    Msk.map@data <- filteredData_results()@data[Msk.districts,]
    leaflet(Msk.map)  %>%
      setView(lng = 37.3, lat = 55.67, zoom = 8) %>% 
      addPolygons(fillColor = ~color, 
                  fillOpacity = 0.8, 
                  color = "black", 
                  weight = 1,
                  popup = ~popups)
  })
  
  output$SpbMap_results <- renderLeaflet({
    Spb.map@data <- filteredData_results()@data[Spb.districts,]
    leaflet(Spb.map)  %>%
      setView(lng = 30.00, lat = 59.95, zoom = 8) %>% 
      addPolygons(fillColor = ~color, 
                  fillOpacity = 0.8, 
                  color = "black", 
                  weight = 1,
                  popup = ~popups)
  })
  
  output$leftTable_results <- renderDataTable({
    df <- subset(filteredData_results()@data, color == "#3399FF", select = c(district, party))
    if (nrow(df) < 5){
      df <- arrange(filteredData_results()@data, party)[1:5,]
    }
    else{
      df <- arrange(df, party)  
    }  
    
    datatable(
      df,
      container = sketch_results, options = list(pageLength = 5, dom = 'tip'), 
      rownames = FALSE
    )
  })
  
  output$rightTable_results <- renderDataTable({
    df <- subset(filteredData_results()@data, color == "#FF9933", select = c(district, party))
    if (nrow(df) < 5){
      df <- arrange(filteredData_results()@data, desc(party))[1:5,]
    }
    df <- arrange(df, desc(party))
    datatable(
      df,
      container = sketch_results, options = list(pageLength = 5, dom = 'tip'), 
      rownames = FALSE
    )
  })
  
  ###################################################################################################  
  
  
  ###################################################
  ############ The turnout tab functions ############
  ###################################################  
  
  sketch_turnout = htmltools::withTags(table(
    class = 'stripe',
    tableHeader(c("Округ", "Явка, %"))
  ))
  
  filteredData_turnout <- reactive({
    russian.election.districts@data <- select(russian.election.districts@data,
                                              district, turnout)
    results.interval <- russian.election.districts@data$turnout %>% range()
    
    russian.election.districts$color <- colorBin("Greens", results.interval,
                                                 20)(russian.election.districts@data$turnout)
    lower.tail <- russian.election.districts$turnout < input$turnout_range[1] 
    upper.tail <- russian.election.districts$turnout > input$turnout_range[2] 
    if (any(lower.tail))
      russian.election.districts@data[lower.tail,]$color <- "#3399FF"
    if (any(upper.tail))  
      russian.election.districts@data[upper.tail,]$color <- "#FF9933"
    russian.election.districts$popups <- paste0(russian.election.districts$district, 
                                                "<br><strong>Явка: </strong>",
                                                russian.election.districts$turnout, "%")
    
    russian.election.districts
  })
  
  output$total_turnout <- renderUI({
    HTML(paste0('<h4>Явка составила: ',total.results["turnout"], '%</h4>'))
    
  })
  
  output$slider_turnout <-  renderUI({
    x <- russian.election.districts@data$turnout %>% range()
    sliderInput("turnout_range", "Фильтр",
                x[1], x[2],
                value= x, step = 0.1,
                ticks = FALSE)
  })
  
  output$message_turnout <- renderUI({
    selected.districts.number <- sum(filteredData_turnout()$color != "#3399FF" & 
                                       filteredData_turnout()$color != "#FF9933")
    HTML(paste0('<b>Отобрано округов: ',selected.districts.number, '</b><br>',
                'Таблица ниже показывает разницу результатов партий
                в отобранных округах по сравнению с их результатами по всей России'))
    
  })
  
  output$RusMap_turnout <- renderLeaflet({
    leaflet(filteredData_turnout())  %>%
      setView(lng = 99, lat = 65, zoom = 3) %>% 
      addPolygons(fillColor = ~color, 
                  fillOpacity = 0.8, 
                  color = "black", 
                  weight = 1,
                  popup = ~popups)
  })
  
  output$MskMap_turnout <- renderLeaflet({
    Msk.map@data <- filteredData_turnout()@data[Msk.districts,]
    leaflet(Msk.map)  %>%
      setView(lng = 37.3, lat = 55.67, zoom = 8) %>% 
      addPolygons(fillColor = ~color, 
                  fillOpacity = 0.8, 
                  color = "black", 
                  weight = 1,
                  popup = ~popups)
  })
  
  output$SpbMap_turnout <- renderLeaflet({
    Spb.map@data <- filteredData_turnout()@data[Spb.districts,]
    leaflet(Spb.map)  %>%
      setView(lng = 30.00, lat = 59.95, zoom = 8) %>% 
      addPolygons(fillColor = ~color, 
                  fillOpacity = 0.8, 
                  color = "black", 
                  weight = 1,
                  popup = ~popups)
  })
  
  output$leftTable_turnout <- renderDataTable({
    df <- subset(filteredData_turnout()@data, color == "#3399FF", select = c(district, turnout))
    if (nrow(df) < 5){
      df <- arrange(filteredData_turnout()@data, turnout)[1:5,]
    }
    else{
      df <- arrange(df, turnout)  
    }  
    
    datatable(
      df,
      container = sketch_turnout, options = list(pageLength = 5, dom = 'tip'), 
      rownames = FALSE
    )
  })
  
  output$rightTable_turnout <- renderDataTable({
    df <- subset(filteredData_turnout()@data, color == "#FF9933", select = c(district, turnout))
    if (nrow(df) < 5){
      df <- arrange(filteredData_turnout()@data, desc(turnout))[1:5,]
    }
    df <- arrange(df, desc(turnout))
    datatable(
      df,
      container = sketch_turnout, options = list(pageLength = 5, dom = 'tip'), 
      rownames = FALSE
    )
  })
  
  output$changesTable <- renderFormattable({
    req(input$turnout_range[1] > 0, cancelOutput = T)
    total.figures <- select(russian.election.districts@data, one_of(c(parties.titles, "invalid_ballots"))) %>% 
      colSums() %>% roundfixS()
    df <- subset(russian.election.districts@data, 
                 turnout >= input$turnout_range[1] & turnout <= input$turnout_range[2], 
                 select = c(parties.titles, "invalid_ballots"))
    new.figures <- colSums(df) %>% roundfixS()
    diff <- (new.figures - total.figures)[parties.titles]
    largest.decrease <- which.min(diff)
    largest.increase <- which.max(diff)
    parties.titles.short <- names(parties.titles)
    parties.titles.short[2] <- "Комм. России"
    parties.titles.short[3] <- "Пенсионеры"
    parties.titles.short[6] <- "Гр. платформа"
    parties.titles.short[14] <- "Справ. Россия"
    df <- data.frame('Партия' = parties.titles.short, 'Изменение' = percent(diff/100))
    rownames(df) <- NULL
    
    if (largest.increase != largest.decrease){
      largest.decrease.party <- df[largest.decrease, 'Партия']
      largest.increase.party <- df[largest.increase, 'Партия']
    }
    else{
      largest.decrease.party <- ''
      largest.increase.party <- ''
    }
    
    formattable(df, list(
      'Партия' = formatter(
        "span",
        style = x ~ style("background-color" = ifelse(x == largest.decrease.party,
                                                      "#ff6347",
                                                      ifelse(x == largest.increase.party,
                                                             "#54ff9f", "white")))
      ),
      'Изменение' = formatter(
        "span",
        style = x ~ style(color = ifelse(x < 0 , "#DC3912", ifelse(x > 0, "#109618", "white"))),
        x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
    )
    )
  })
  #####################################################################################################
  
  
  ###################################################
  ############ The Comparing tab functions ##########
  ###################################################
  
  
  sketch_diff = htmltools::withTags(table(
    class = 'stripe',
    tableHeader(c("Округ", "Разница, %"))
  ))
  
  output$message_diff <- renderUI({
    HTML('Гистограмма и карты показывают разницу полученных процентов голосов между 
         Партиями 1 и 2.<br>
         Оранжевая палитра &#8211 больший результат у Партии 1,<br>
         синяя палитра &#8211 у Партии 2.')
  })
  
  filteredData_diff <- reactive({
    party1.column <- paste0(input$party1, ".pct")
    party2.column <- paste0(input$party2, ".pct")
    
    if (input$party1 != input$party2){
      russian.election.districts@data <- select(russian.election.districts@data,
                                                district,
                                                party1 = one_of(party1.column), 
                                                party2 = one_of(party2.column))
      russian.election.districts$diff <- round(russian.election.districts$party1 - russian.election.districts$party2,2)
      russian.election.districts@data <- select(russian.election.districts@data, -c(party1, party2))
      sym.diff.interval <- range(russian.election.districts$diff)
      if (- sym.diff.interval[1] > sym.diff.interval[2]){
        sym.diff.interval[2] <- - sym.diff.interval[1]
      }
      else{
        sym.diff.interval[1] <- - sym.diff.interval[2]
      }
      russian.election.districts$color <- colorBin(colorschemes$BluetoOrange.10, sym.diff.interval, 10)(russian.election.districts$diff)
    }
    else{
      russian.election.districts@data <- select(russian.election.districts@data,
                                                district)
      russian.election.districts$diff <- 0
      russian.election.districts$color <- "white"
    }
    
    russian.election.districts$popups <- paste0(russian.election.districts$district, 
                                                "<br><strong>Разница: </strong>",
                                                russian.election.districts$diff, "%")
    russian.election.districts
  })
  
  
  output$hist_diff <- renderPlot({
    x <- filteredData_diff()$diff
    hist(x,
         xlim = range(x),
         main = "Разница результатов",
         xlab = "Проценты",
         ylab = "Число округов",
         col = '#00DD00',
         border = 'white')
  })
  
  output$RusMap_diff <- renderLeaflet({
    leaflet(filteredData_diff())  %>%
      setView(lng = 99, lat = 65, zoom = 3) %>% 
      addPolygons(fillColor = ~color, 
                  fillOpacity = 0.8, 
                  color = "black", 
                  weight = 1,
                  popup = ~popups)
  })
  
  output$MskMap_diff <- renderLeaflet({
    Msk.map@data <- filteredData_diff()@data[Msk.districts,]
    leaflet(Msk.map)  %>%
      setView(lng = 37.3, lat = 55.67, zoom = 8) %>% 
      addPolygons(fillColor = ~color, 
                  fillOpacity = 0.8, 
                  color = "black", 
                  weight = 1,
                  popup = ~popups)
  })
  
  output$SpbMap_diff <- renderLeaflet({
    Spb.map@data <- filteredData_diff()@data[Spb.districts,]
    leaflet(Spb.map)  %>%
      setView(lng = 30.00, lat = 59.95, zoom = 8) %>% 
      addPolygons(fillColor = ~color, 
                  fillOpacity = 0.8, 
                  color = "black", 
                  weight = 1,
                  popup = ~popups)
  })
  
  output$leftTable_diff <- renderDataTable({
    df <- arrange(filteredData_diff()@data, diff)[1:5,]
    datatable(
      df,
      container = sketch_diff, options = list(pageLength = 5, dom = 'tip'), 
      rownames = FALSE
    )
  })
  
  output$rightTable_diff <- renderDataTable({
    df <- arrange(filteredData_diff()@data, desc(diff))[1:5,]
    datatable(
      df,
      container = sketch_diff, options = list(pageLength = 5, dom = 'tip'), 
      rownames = FALSE
    )
  })
  
}