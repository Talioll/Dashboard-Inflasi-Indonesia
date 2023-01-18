dashboardPage(
  dashboardHeader(
    title = "INFLASI INDONESIA",
    titleWidth = 250
  ),
  # --------------------------- SIDEBAR 
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Halaman Utama", 
               icon = icon("fa-solid fa-sack-dollar"), 
               tabName = "Page1"),
      menuItem("Grafik Inflasi", 
               icon = icon("fa-solid fa-chart-line"), 
               tabName = "Page2"),
      menuItem("Informasi Sumber Data", 
               icon = icon("fa-solid fa-book"), 
               tabName = "Page3")
    )
  ),
  
  # --------------------------- BODY
  dashboardBody(
    shinyDashboardThemes(theme = "grey_dark"),
    
    #--------------------- Create Tab Items for Page
    
    tabItems(
      #Page 1 create per column
      tabItem(tabName = "Page1",
             
              fluidRow(
                column(width = 1,
                       selectInput(inputId = "tahundasar",
                                   label = "Pilih Tahun Dasar",
                                   choices = yearlist,
                                   selected = "2022",
                                   width = 100),
                       selectInput(inputId = "bulandasar",
                                   label = "Pilih Bulan Dasar",
                                   choices = MonthList,
                                   selected = "September",
                                   width = 120),
                       selectInput(inputId = "tahunpembanding",
                                   label = "Pilih Tahun Pembanding",
                                   choices = yearlist,
                                   selected = "2022",
                                   width = 100),
                       selectInput(inputId = "bulanpembanding",
                                   label = "Pilih Bulan Pembanding",
                                   choices = MonthList,
                                   selected = "Agustus",
                                   width = 120)
                ),
                column(width = 4,
                       box(width = 15,
                           title = "KALKULASI INFLASI",
                           solidHeader = T, 
                           status = "info",
                           height = 400,
                           tableOutput(outputId = "Plot1"),
                           plotlyOutput(outputId = "grafikKalkulasiInflasi", 
                                        height = 250,width = 400)
                           
                       )
                ),
                column(width = 7,
                       # -------------------Tab Box Session
                       tabBox(
                         width = NULL,
                         height = 400,
                         title = "Rata-rata Inflasi Tahunan",
                         selected = "10 Rata-rata Inflasi Tahunan Tertinggi",
                         id = "tabP1",
                         
                         # ----------------Tab 1
                         tabPanel("Tahun 2003 - 2022",
                                  icon = icon("fa-regular fa-calendar-days"),
                                  plotlyOutput(outputId = "Tab1",
                                               height = 330)),
                         # ----------------Tab 2
                         tabPanel("10 Rata-rata Inflasi Tahunan Tertinggi",
                                  icon = icon("fa-solid fa-arrow-up"),
                                  fluidRow(
                                    column(width = 4,
                                           infoBox(
                                             title = "Inflasi Tertinggi",
                                             value = "18.5%",
                                             subtitle = "November 2005",
                                             color = "purple",
                                             icon = icon("fa-solid fa-sack-xmark"),
                                             fill = T,
                                             width = 15),
                                           infoBox(
                                             title = "Inflasi Terendah",
                                             value = "1.32%",
                                             subtitle = "Agustus 2020",
                                             color = "orange",
                                             icon = icon("fa-solid fa-sack-dollar"),
                                             fill = T,
                                             width = 15),
                                           infoBox(
                                             title = "",
                                             value = "1.56%",
                                             subtitle = "Rata-rata Terendah Tahun 2021",
                                             color = "purple",
                                             icon = icon("fa-solid fa-money-bill-trend-up"),
                                             fill = T,
                                             width = 15),
                                    ),
                                    column(width = 7,
                                           fluidRow(plotlyOutput(outputId = "Tab2",
                                                                 height = 330)))
                                    
                                  )
                             )
                        )
                  )
                
              ),
              fluidRow(
                box(
                  width = 12,
                  height = 500,
                  
                  plotlyOutput(outputId = "plot2", height = 450)
                )
                
              )
              
              
      ),
      #Page 2 
      tabItem(tabName = "Page2", 
              fluidRow(
                column(width = 4,
                       box(width = NULL,
                    
                           solidHeader = T, 
                           height = 680,
                           title = "Perbandingan Inflasi Tahunan per Kategori",
                           plotlyOutput(outputId = "plotP2_2", height = 600),
                           
                           
                           )
                ),
                column(width = 4,
                        plotlyOutput(outputId = "plotP2", height = 250),
                       box(width = NULL, 
                           height = 430,
                           solidHeader = T, 
                           title = "Inflasi Daerah berdasarkan Kategori",
                           
                           selectInput(inputId = "detailCategory",
                                       label = "Pilih Kategori",
                                       selected = "Jasa Angkutan Angkutan",
                                       choices = sort(detailCtgList)),
                           selectInput(inputId = "pilihkota",
                                       label = "Pilih Kota",
                                       selected = "Medan",
                                       choices = sort(Kota)),
                           plotlyOutput(outputId = "plotP2_3", height = 200)
                           )
                       
                       ),
                column(width = 4,
                       box(width = NULL, 
                          height = 700,
                          solidHeader = T, 
                          title = "List Kota dengan Range Inflasi",
                          sliderInput(inputId = "slider1", 
                                      label = "Pilih Range Inflasi",
                                      min = -5,
                                      max = 16, value = c(1,2), dragRange = T),
                          DTOutput(outputId = "table2")
                          
                          ))
                
              )
      
      ),
          
      #Page 3
      tabItem(tabName = "Page3",
              fluidRow(
                box(
                  width = 12,
                  status = "warning",
                  title = "Data Source",
                  solidHeader = T,
                  collapsible = T,
                  collapsed = T,
                uiOutput(outputId = "link1")
              
                )
              )
      )
    )
  )
  
)

