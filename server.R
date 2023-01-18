shinyServer(function(input, output, session) {
  
  output$Plot1 <- renderTable({
    
    raiseInflasiSub <- inflasi %>%
      summarise(`Naik/Turun Inflasi` = dataInflasi[Year == input$tahunpembanding & Month == input$bulanpembanding] - dataInflasi[Year == input$tahundasar & Month == input$bulandasar]) %>%
      mutate(`Dari Tahun` = input$tahundasar,
             `Dari Bulan` = input$bulandasar, 
             `Ke Tahun` = input$tahunpembanding,
             `Ke Bulan` = input$bulanpembanding) %>% 
      select(`Dari Tahun`,`Dari Bulan`, `Ke Tahun`, `Ke Bulan`, `Naik/Turun Inflasi`)
    raiseInflasiSub
  },align = "c")
  
  output$grafikKalkulasiInflasi <- renderPlotly({
    
    kalkulasiInflasidf <- rbind(inflasi[inflasi$Year == input$tahundasar & inflasi$Month == input$bulandasar, ], inflasi[inflasi$Year == input$tahunpembanding & inflasi$Month == input$bulanpembanding, ])
    
    kalkulasiInflasidf <- kalkulasiInflasidf %>% 
      mutate(label = glue(
        "Tahun: {Year}
        Inflasi: {dataInflasi}"))
    
    kalkulasiInflasidf$Month <- factor(kalkulasiInflasidf$Month,
                            levels = c("Januari", "Februari", "Maret", "April", "Mei", "Juni", "Juli", "Agustus", "September", "Oktober", "November", "Desember"))
    
    kalkulasiplot <- ggplot(kalkulasiInflasidf, aes(x = Month, y = dataInflasi, group = 1)) +
      geom_point(aes(text = label), col = "#090939", size = 6.5, show.legend = F) +
      geom_line( col = "#ffd300", linetype = "dashed", arrow = arrow(), size = 0.5) +
      labs(y = "Nilai Inflasi",
           x = NULL) +
      theme(
        panel.grid = element_line(color = "grey"),
        axis.title.y = element_text(face = "bold", size = 9, color ="white"),
        axis.text.x = element_text(size = 8, face = "bold", color ="white"),
        axis.text.y = element_text(color ="white"),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = NA),
        legend.position = "none")
    
    ggplotly(kalkulasiplot, tooltip = "text")
    
  })
  
  output$Tab1 <- renderPlotly({
    
    avgInflasi <- inflasi %>% 
      filter(Year != "2002") %>% 
      group_by(Year)
    
    ggplot(avgInflasi, aes(x = Year, y = dataInflasi)) +
      geom_boxplot(outlier.colour = "red", col = "#ffd300", fill = "#090939") + 
      labs(x = NULL, y = NULL)+
      theme(
        plot.background = element_rect(fill = NA),
        panel.background = element_rect(fill = NA),
        axis.text = element_text(color = "white"),
        panel.grid.major.x = element_blank()
      ) 
    
  })
  
  output$Tab2 <- renderPlotly({
    
    avgInflasi <- inflasi %>% 
      filter(Year != "2002") %>% 
      group_by(Year)
    
    avgInflasi1 <- avgInflasi %>% 
      group_by(Year) %>% 
      summarise(`Rata - rata Inflasi Tahunan` = round(mean(dataInflasi),2)) %>% 
      mutate(label = glue(
        "Rata-rata Inflasi:
    {`Rata - rata Inflasi Tahunan`}"
      )) %>% 
      arrange(desc(`Rata - rata Inflasi Tahunan`)) %>% 
      head(10)
    
    avgInflasi1plot <- ggplot(avgInflasi1, aes(x = `Rata - rata Inflasi Tahunan`, y = Year)) +
      geom_segment(aes(y = reorder(Year, `Rata - rata Inflasi Tahunan`), yend = reorder(Year, `Rata - rata Inflasi Tahunan`), x = 0 , xend = `Rata - rata Inflasi Tahunan`, col = `Rata - rata Inflasi Tahunan`), show.legend = F) +
      geom_point(aes(col = `Rata - rata Inflasi Tahunan`, text = label), show.legend = F, size = 3) +
      scale_color_gradient(low = "#ffce47" , high = "#7724c3") +
      labs(x = NULL,
           y = NULL) +
      theme(
        plot.background = element_rect(fill = NA),
        panel.background = element_rect(fill = NA),
        axis.text = element_text(colour = "white"),
        legend.position = "none"
      )
    
    ggplotly(avgInflasi1plot, tooltip = "text")
    
  })
  
  output$plot2 <- renderPlotly({
    
    inflasi1 <- inflasi %>% 
      filter(Year != "2002") %>% 
      mutate(Month = as.numeric(Month),
             `Nilai Inflasi` = dataInflasi,
             Period = Periode,
             Periode = glue("{Year}/{Month}"),
             Periode = yearmonth(parse_datetime(Periode, format= "%Y/%m")))
    
    inflasiplot <- ggplot(inflasi1, aes(x = Periode, y = `Nilai Inflasi`, group = 1)) +
      geom_line(size = 0.8, aes(col = Year)) +
      labs(x = NULL,
           y = "Nilai Inflasi",
           title = "INFLASI INDONESIA PERIODE JAN 2003 -  SEP 2022") +
      geom_hline(yintercept = 0, color = "red") +
    scale_color_discrete() +
      theme_light() +
      theme(
        plot.background = element_rect(fill = NA),
        panel.background = element_rect(fill = NA),
        plot.title = element_text(colour = "white", face = "bold", size = 14, hjust = 0.5),
        axis.text = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white", face = "bold"),
        legend.position = "none")
    
    ggplotly(inflasiplot)
    
  })
  
  output$plotP2 <- renderPlotly({
    
    year.ctg <- allSub %>% 
      filter(Year == "2022", 
             Category == "Perumahan (Summary All Cat)" |   
               Category == "Transportasi (Summary All Cat)" |
               Category == "Jasa Pelayanan Makanan & Minuman" |
               Category == "Makanan, Minuman & Tembakau (Summary All Cat)") %>% 
      mutate(Category = str_replace(Category, "\\(Summary All Cat\\)", ""),
             Month = substr(Month, 1, 3),
             label = glue(
               "Kategori: {Category}
                Inflasi: {Indonesia}"
             )) %>% 
      select(Category, Year, Month, Indonesia, label) 
    
    year.ctg$Month <- factor(year.ctg$Month,
                                levels = c("Jan", "Feb", "Mar", "Apr", "Mei", "Jun", "Jul", "Agu", "Sep", "Okt", "Nov", "Des"))
    
    year.ctg.plot <- ggplot(year.ctg, aes(x = Month, y = Indonesia)) +
      geom_line(aes(group = Category, col = Category), show.legend = F,) +
      geom_point(aes(col = Category, text = label), show.legend = F) +
      labs(x = NULL, y= NULL,
           title = "Inflasi berdasarkan Kategori (Tahun 2022)") +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
      scale_color_brewer(palette = "Set2") +
      theme_minimal()+
      theme(legend.position = "none",
            plot.title = element_text(color = "white",vjust = 0.5, family = "serif"),
            axis.text = element_text(color = "white", size = 8),
            plot.background = element_rect(fill = NA),
            panel.background = element_rect(fill = NA))

    ggplotly(year.ctg.plot, tooltip = "text")
    
  })
  
  output$plotP2_2 <- renderPlotly({
    
    ctg.by.year <- allSub %>% 
      filter(Category == "Perumahan (Summary All Cat)" |
               Category == "Transportasi (Summary All Cat)" |
               Category == "Jasa Pelayanan Makanan & Minuman" |
               Category == "Makanan, Minuman & Tembakau (Summary All Cat)") %>%
      mutate(Category = str_replace(Category, "\\(Summary All Cat\\)", ""),
             Month = substr(Month, 1, 3),
             label = glue(
               "Tahun: {Year}
               Tingkat Inflasi: {Indonesia}"
             )) %>% 
      select(Category, Year, Month, Indonesia, label)
    ctg.by.year$Month <- factor(ctg.by.year$Month,
                           levels = c("Jan", "Feb", "Mar", "Apr", "Mei", "Jun", "Jul", "Agu", "Sep", "Okt", "Nov", "Des"))
    
    plotcategoryYear <- plotcategoryYear <- ggplot(ctg.by.year, aes(x = Month, y = Indonesia)) +
      geom_line(aes(group = Year, col = Year), show.legend = F) +
      geom_point(aes(col = Year, text = label), show.legend = F) +
      facet_wrap(~Category,ncol = 1, scales = "free") +
      labs(x = NULL,
           y = NULL) +
      scale_color_brewer(palette = "Set2") +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = NA),
        axis.text = element_text(color = "white", size = 8),
        panel.grid = element_line(color = "grey"),
        plot.title = element_text(color = "white", face = "bold", vjust = 0),
        strip.text = element_text(size = 8, color = "white"),
        strip.background = element_rect(fill = "black")
      )
    
    ggplotly(plotcategoryYear, tooltip = "text")
    
  })
  
  output$plotP2_3 <- renderPlotly({
    
    df4pivot <- allSub %>% 
      select(-(Indonesia)) %>% 
      mutate(Month = substr(Month, 1, 3))
    
    
    allSubPivot <- pivot_longer(df4pivot, 
                                cols = -c(Category, Year, Month), 
                                names_to = "Kota", 
                                values_to = "Inflasi")
      
    allSubPivot$Month <- factor(allSubPivot$Month,
                             levels = c("Jan", "Feb", "Mar", "Apr", "Mei", "Jun", "Jul", "Agu", "Sep", "Okt", "Nov", "Des"))
    dtailsub <- allSubPivot %>% 
      filter(Kota == input$pilihkota & Category == input$detailCategory) %>% 
      mutate(label = glue(
        "Tahun: {Year}
         Inflasi: {Inflasi}"))
    
    plotx <- ggplot(dtailsub, aes(x = Month, y = Inflasi)) + 
      geom_line(aes(group = Year, col = Year), show.legend = F) +
      geom_point(aes(col = Year, text = label), show.legend = F) +
      labs(x = NULL,
           y = NULL) +
      theme_light()+
      theme(legend.position = "none",
            plot.background = element_rect(fill = NA),
            panel.background = element_rect(fill = NA),
            axis.text = element_text(color = "black"))
    ggplotly(plotx, tooltip = "text")
    
  })
  
  output$table2 <- renderDataTable({
    df4pivot <- allSub %>% 
      select(-(Indonesia)) %>% 
      mutate(Month = substr(Month, 1, 3))
    
    
    allSubPivot <- pivot_longer(df4pivot, 
                                cols = -c(Category, Year, Month), 
                                names_to = "Kota", 
                                values_to = "Inflasi")
    
    tablelist <- allSubPivot %>% 
      filter(Inflasi >= input$slider1[1],
             Inflasi <= input$slider1[2],
             Year == "2022",
             Category == "Jasa Angkutan Penumpang") %>% 
      select(Kota, Inflasi) %>% 
      arrange(Inflasi)
    
  })
 
    
  output$link1 <- renderUI({
    
    url <- a("BPS.go.id", href = "https://www.bps.go.id/subject/3/inflasi.html#subjekViewTab3")
    tagList("URL link:", url)
  
  })
  
  

  
  
})