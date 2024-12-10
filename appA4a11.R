#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(devtools)
library(planr)
library(tidyverse)
library(sparkline)
library(reactable)
library(reactablefmtr)
library(DT)
library(highcharter)
library(rhandsontable)
library(data.table)
# for the Demand & Supply Planning calculations : the library planr
# Others
library(htmltools)



mystyle <- '
  .animal { width: 10em; }
  #bee {
    font-style: italic; 
    color: yellow;
    background-color: black;
  }
  li#bee::marker { content: "🐝 "; }
'
recepcion <- data.frame(
  UM = c("Camiones recibidos","Paletas totales a almacenar","Paletas recibidas enteras","Paletas por armar","Bultos de paletas por armar","Pallets a descargar con montacargas"),
  Cantidad = c(30.0, 1500.0, 600.0, 900.0, 3600.0, 500)
)

ingresoRecepcion <- data.frame(
  act = c("AperturaUnidad", "DescargaPalletMontacarga","ValidacionPallet_Armadopallet","ValidacionPalletsCompletos","EtiquetadoPalletEingreso","CierreCamion"),
  #act = c("Leer etiqueta de pallet","Dirigirse hacia ubicación de almacenamiento","Elevar pallet y colocar en ubicación","Bajar uñas de elevador","Retornar hacia canal"),
  resp = c("ALMACENERO","OPERADOR ELEVADOR","ALMACENERO","ALMACENERO","ALMACENERO","ALMACENERO"),
  UM = c("Camiones recibidos","Pallets a descargar con montacargas","Paletas por armar","Paletas recibidas enteras","Paletas totales a almacenar","Camiones recibidos"),
  #Purchase_Date = c("2023-11-01", "2024-05-15", "2021-03-20", "2022-12-25", "2022-06-10", "2023-07-22"),
  #Recep = factor(c("Low", "Medium", "Medium", "Medium", "Low", "High"), levels = c("Low", "Medium", "High")),
  TiempoObservado = c(19, 60, 60, 9, 9,14.5),
  f = c(1.11, 1.1, 1.1, 1.1, 1.1,1.1),
  v = c(1.0, 1.0, 1.0, 1.0, 1.0,1),
  TiempNormal = c(21.1, 66.7, 66, 9.9,9.9, 16.0),
  Suplemento = c(1.2, 1.2, 1.2,  1.2, 1.2,1.2),
  TiempStd = c(24.9, 78.7,77.9, 11.7, 11.7, 18.8)
)

configAlmacen<-data.frame(
  concepto = c("% Tipo A","% Tipo B","% Tipo C","Distancia promedio tipo A (m) - d1","Distancia promedio tipo B (m) - d2","Distancia promedio tipo C (m) - d3","Velocidad promedio (km/h)","Distancia promedio (m)","Tiempo promedio recorrido (s)"),
  valores = c(50,30,20,30,40,50,8,35,17)
)

almacenamiento <- data.frame(
  #act = c("AperturaUnidad", "DescargaPalletMontacarga","ValidacionPallet_Armadopallet","ValidacionPalletsCompletos","EtiquetadoPalletEingreso","CierreCamion"),
  act = c("Leer etiqueta de pallet","Dirigirse hacia ubicación de almacenamiento","Elevar pallet y colocar en ubicación","Bajar uñas de elevador","Retornar hacia canal"),
  resp = c("OPERADOR ELEVADOR","OPERADOR ELEVADOR","OPERADOR ELEVADOR","OPERADOR ELEVADOR","OPERADOR ELEVADOR"),
  UM = c("Paletas Totales a almacenar"," ","Paletas Totales a almacenar","Paletas Totales a almacenar"," "),
  #Purchase_Date = c("2023-11-01", "2024-05-15", "2021-03-20", "2022-12-25", "2022-06-10", "2023-07-22"),
  #Recep = factor(c("Low", "Medium", "Medium", "Medium", "Low", "High"), levels = c("Low", "Medium", "High")),
  TiempoObservado = c(30, 30, 15, 15, 30),
  f = c(1.11, 1.1, 1.1, 1.1, 1.1),
  v = c(1.0, 1.0, 1.0, 1.0, 1.0),
  TiempoNormal = c(33.1, 33.0, 16.5, 16.9, 33),
  Suplemento = c(1.2, 1.2, 1.2,  1.2, 1.2),
  TiempStd = c(38.9, 38.9, 19.5, 19.5, 38.9)
)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinythemes::shinytheme("sandstone"),
                navbarPage(
                  tags$header("Dimensionamiento,slotting y productividad",style = "font-weight: 1500; font-size: 32; color: #eb4034;"),
                 tags$hr(),
                  #tabpanel1
                  tabPanel("Lectura de datos",
                           # Application title
                           titlePanel("Data Input"),
                           # Sidebar with a slider input for number of bins 
                           #sidebarLayout(
                           sidebarPanel(
                            fileInput("file1", "Choose CSV File",
                                       multiple = FALSE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")),
                             actionButton("submitbutton22", "Presenta tabla de datos", class = "btn btn-primary"),
                             # Input: Checkbox if file has header ----
                             checkboxInput("header", "Header", TRUE),
                             
                             # Input: Select separator ----
                             radioButtons("sep", "Separator",
                                          choices = c(Comma = ",",
                                                      Semicolon = ";",
                                                      Tab = "\t"),
                                          selected = ","),
                             
                             # Input: Select quotes ----
                             radioButtons("quote", "Quote",
                                          choices = c(None = "",
                                                      "Double Quote" = '"',
                                                      "Single Quote" = "'"),
                                          selected = '"'),
                             
                             # Horizontal line ----
                             tags$hr(),
                             radioButtons("disp", "Display",
                                          choices = c(Head = "head",
                                                      All = "all"),
                                          selected = "head"),
                             
                             tags$hr(),
                             
                             
                             # Button
                             downloadButton("Data de productos subida", "Download"),
                             
                             hr()
                             
                             ,br(), br(),
                             
                             h5("Built with",
                                img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                "by",
                                img(src = "https://www.r-project.org/logo/Rlogo.svg", height = "25px"),
                                
                                img(src = "https://www.python.org/static/community_logos/python-logo-inkscape.svg", height = "25px")) #)
                             
                               #h5("Elaborado por Jose Antonio Taquia Gutierrez")
                              #  ,
                             
                               #helpText(a("www.taquiagutierrez.com",target="_blank", href="https://www.taquiagutierrez.com")
                             
                           ),
                           
                           
                           
                           # Show a plot of the generated distribution
                           mainPanel(
                             
                             tableOutput("contents")
                             #textInput("txt", "Enter the text to display below:"),
                             # textOutput("text")
                             
                             
                             
                           )
                  ),
                  ########################################################
                  ########################################################
                  ##############                       ###################
                  ##############       tabpanel   2    ###################
                  ##############                       ###################
                  ########################################################
                  ########################################################
                  
                  tabPanel("Requerimientos de almacenamiento",headerPanel('Requirements'),
                           
                           # Input values
                           sidebarPanel( HTML("<h3>Input parameters</h3>"),
                                         tags$hr(), 
                                         # Copy the chunk below to make a group of checkboxes
                                         checkboxGroupInput("checkGroup", label = h3("Preparacion"), 
                                                            choices = list("Recepcion" = 1, "Almacen" = 2, "Fraccionamiento y despacho" = 3,"Devoluciones"=4),
                                                            selected = 1),
                                         tags$hr(), 
                                         actionButton("submitbutton77", "Demanda", class = "btn btn-primary"),
                                         tags$hr(), 
                                         fluidPage(fluidRow(column(5, numericInput('HorasTotales', 'HorasTotalesTrabajo', 8, min = 1, max = 10)),column(5, numericInput('HorasTotalesEfectivas', 'HorasEfectivas', 8, min = 1, max = 10)))),
                                         tags$hr(), 
                                         #fluidPage(fluidRow(column(5, numericInput('CamionesRecibidos', 'HorasTrabajoEfectivas', 1000, min = 1, max = 20000)),column(5, numericInput('Picking', 'Picking Promedio', 3000, min = 1, max = 20000)))),
                                         tags$hr(),
                                         checkboxGroupInput("checkGroup", label = h3("Tipo de operatividad"), 
                                                            choices = list("Ingresa caja entera o saldo y se despacha por unidades" = 1, "Ingresa caja  entera y se despacha por caja entera" = 2, "Ingresa paleta entera y se despacha por paleta entera" = 3,"Ingresa caja entera y se despacha  por unidad y caja entera"=4,"Ingresa caja entera y se despacha  por caja y paleta entera"=5,"Ingresa paleta entera y se despacha  por caja y paleta entera"=6,"Ingresa paleta entera y se despacha  por unidad y paleta entera"=7),
                                                            selected = 1)
                           ), 
                           # Horizontal line ----
                           
                           tags$hr(),
                           
                           
                           # Button
                           
                           #uiOutput("ui"),
                           mainPanel(
                           
                          
                           reactableOutput("values2"),
                          
                          h3("Configuracion general de operaciones"),
                          br(),
                          fluidRow(column(12, rHandsontableOutput("table2output"))), 
                          br(),
                          uiOutput("moreControls2"),
                          br(),
                          h3("Tabla de operaciones luego de configuracion general"),
                          br(),
                          fluidRow(column(12, rHandsontableOutput("hotable1"))),
                          br(),
                          h3("Tiempos Totales Almacenero considerando tiempos unitarios en Proceso de Recepcion"),
                          br(),
                          fluidRow(column(12, rHandsontableOutput("hotable2"))),
                          br(),
                          h3("Tiempos Totales Montacargas considerando tiempos unitarios Proceso de Recepcion"),
                          br(),
                          fluidRow(column(12, rHandsontableOutput("hotable3"))),
                          br(),
                          h3("Tiempos Unitarios Proceso de Recepcion"),
                          br(),
                          fluidRow(column(12, rHandsontableOutput("table3output"))),
                          br(),
                          h3("Configuracion Operacion de Almacenamiento"),
                          br(),
                          fluidRow(column(12, rHandsontableOutput("hotable5"))),
                          br(),
                          h3("Tiempos Totales Operador Elevador considerando tiempos unitarios Proceso Almacenamiento"),
                          br(),
                          fluidRow(column(12, rHandsontableOutput("hotable4"))),
                          br(),
                          br(),
                          h3("Tiempos Unitarios Proceso de Almacenamiento"),
                          br(),
                          fluidRow(column(12, rHandsontableOutput("table4output"))),
                          br(),
                          h3("Resultados de productividad del proceso de recepcion"),
                          br(),
                          uiOutput("moreControls3"),
                          br(),
                          
                          
                           fluidRow(downloadButton(
                             outputId = "download_button",
                             label = "Download", 
                             icon = icon("download"))
                             
                           
                           )
                           
                           )),
                  
                  
                  ########################################################
                  ########################################################
                  ##############                       ###################
                  ##############       tabpanel   3    ###################
                  ##############                       ###################
                  ########################################################
                  ########################################################
                  
                  tabPanel("Alertas de proyeccion de inventarios",
                           # Input values
                           sidebarPanel( HTML("<h3>Input parameters</h3>"),
                                         tags$hr(), 
                                         wellPanel(actionButton("submitbutton78", "Start optimal search", class = "btn btn-primary")),
                                         # Input: Select separator ----
                                         wellPanel(radioButtons("Opciones del reporte", "Presenta resultados",
                                                                choiceNames = c("Best value",
                                                                                "Location sequence obtained"), choiceValues = c(0,1),
                                                                selected = "Best value")),
                                         wellPanel(sliderInput("Capacidad",
                                                               "Rangos de analisis:",
                                                               min = 400,  max = 1000,  value = 500, step=100)),
                                         hr()
                                         
                                         #,actionButton("submitbutton", "Submit", class = "btn btn-primary")
                                         
                                         #,numericInput("obs2", "Observations:", 4, min = 1, max = 100)
                                         
                                         # Built with Shiny by RStudio
                                         ,br(), br(),
                                         
                                         h5("Built with",
                                            img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                            "by",
                                            img(src = "https://www.r-project.org/logo/Rlogo.svg", height = "25px"),
                                            
                                            img(src = "https://www.python.org/static/community_logos/python-logo-inkscape.svg", height = "25px"))
                           ),
                           
                           mainPanel(
                             #fluidRow(tags$label(column(6,h3('Output'),offset = 5))),
                             #fluidRow(column(6,img(src='Buho5.jpg', align = "right"), offset=2)),
                             #fluidRow(),
                             #fluidRow(verbatimTextOutput("txtout")),
                             #fluidRow(tableOutput("values22")),
                             reactableOutput("values3")
                             #tableOutput("table22"),
                             #dataTableOutput("table22")
                             #google_mapOutput(outputId = "lima1")
                             
                             
                           )
                           
                           
                           
                  )),# End tabPanel    3
                  
                  
                  ########################################################
                  ########################################################
                  ##############                       ###################
                  ##############       tabpanel   4    ###################
                  ##############                       ###################
                  ########################################################
                  ########################################################
                 
                  
                  
)# navbarPage
# fluidpage
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #######################################################################
  #######################################################################
  #######################################################################
  ##############################              ###########################
  ############################## TABPANEL 1   ###########################    
  ##############################              ###########################
  #######################################################################
  #######################################################################
  #######################################################################
  
  output$contents <- renderTable({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    data3<-read.csv(file$datapath, header = input$header)
     })
  
  #######################################################################
  #######################################################################
  #######################################################################
  ##############################              ###########################
  ############################## TABPANEL 2   ###########################    
  ##############################              ###########################
  #######################################################################
  #######################################################################
  #######################################################################
  observeEvent(input$table1output,{
    df <- hot_to_r(input$table1output)
    df <- as.data.frame(df)
    table_changes <- input$table1output$changes$changes
    print(sum(df$Weight))
    output$moreControls <- renderUI({
      #print(sum(port$Weight))
      sum(df$Weight)
    })
  }, ignoreInit = TRUE, ignoreNULL = TRUE
  )
  ###########################################
  ###########################################
  ###########################################
  observeEvent(input$table3output,{
    df2 <- hot_to_r(input$table3output)
    df2 <- as.data.frame(df2)
    #table_changes <- input$table3output$changes$changes
    #df2[1,6]==sum(df2[1,4],df2[1,5])*2
    
    #print(sum(df$Weight))
    output$moreControls2 <- renderUI({
       x=sum(df2[1,4],df2[1,5])*2
       paste0("% Recepción cajas: ",x, sep=" ","%Recepción pallets enteros: ",x, sep=" ","%  Recepción pallets con montacargas ",x , "%")
       
    })
  }, ignoreInit = TRUE, ignoreNULL = TRUE
  )
  
  ###########################################
  ###########################################
  ###########################################
  observeEvent(input$table2output,{
    df2 <- hot_to_r(input$table2output)
    df2 <- as.data.frame(df2)
    #table_changes <- input$table2output$changes$changes
    
    output$moreControls2 <- renderUI({
      x=(df2[4,2]/df2[2,2])*100
      y=100-x
      z=100-y
      paste0("% Recepción cajas: ",x, sep=" ","%Recepción pallets enteros: ",y, sep=" ","%  Recepción pallets con montacargas ",z, "%")
      
    })
    
    
    
  }, ignoreInit = TRUE, ignoreNULL = TRUE
  )
  previous <- reactive({recepcion})
  MyChanges <- reactive({
    if(is.null(input$table2output)){return(previous())}
    else if(!identical(previous(),input$table2output)){
      # hot.to.df function will convert your updated table into the dataframe
      mytable <- as.data.frame(hot_to_r(input$table2output))
      # here the second column is a function of the first and it will be multipled by 100 given the values in the first column
     # mytable <- mytable[1:6,]
      
      # Add some test cases
      #mytable[,1][is.na(mytable[,1])] <- 1
      #mytable[,2][is.na(mytable[,2])] <- 1
      mytable[5,2] <- mytable[4,2]*mytable[5,2]
      mytable
    }
  })
  output$hotable1 <- renderRHandsontable({rhandsontable(MyChanges())}) 
  ###########################################
  ###########################################
  ###########################################
  previous2 <- reactive({ingresoRecepcion})
  MyChanges2 <- reactive({
    if(is.null(input$table3output)){return(previous2())}
    else if(!identical(previous2(),input$table3output)){
      # hot.to.df function will convert your updated table into the dataframe
      mytable2 <- as.data.frame(hot_to_r(input$table3output))
      df1<-mytable2
      df1 <- df1 |> group_by(resp, UM) |> summarise(TiempoTotalOperacion = sum(TiempStd))
      df1
      #mytable2[5,4] <- mytable2[4,4]*mytable2[5,4]
      #mytable2
    }
  })
  output$hotable2 <- renderRHandsontable({rhandsontable(MyChanges2())})
  
  ###########################################
  ###########################################
 
  
  observeEvent(input$table3output,{ 
    parte1 <- as.data.frame(hot_to_r(input$table3output))
    parte2 <- as.data.frame(hot_to_r(input$hotable1 ))
    dfA<-parte1 
    dfA <- dfA |> group_by(UM,resp) |> summarise(TiempoTotalOperacion = sum(TiempStd))
    dfB <-parte2
    library(dplyr)
    dat = full_join(dfA, dfB, )
    dat <- dat[dat$resp=="ALMACENERO",]
    dat<-na.omit(dat)
    print(dat)
    print (dfB[2,2])
    print(sum(dat$TiempoTotalOperacion*dat$Cantidad)/dfB[2,2] )
    })  
  ###########################################
  ###########################################
  ###########################################
  previous3 <- reactive({ingresoRecepcion})
  MyChanges3 <- reactive({
    if(is.null(input$table4output)){return(previous3())}
    else if(!identical(previous3(),input$table4output)){
      # hot.to.df function will convert your updated table into the dataframe
      mytable3 <- as.data.frame(hot_to_r(input$table4output))
      df1<-mytable3
      df1 <- df1 |> group_by(resp, UM) |> summarise(TiempoTotalOperacion = sum(TiempStd))
      df1
      #mytable2[5,4] <- mytable2[4,4]*mytable2[5,4]
      #mytable2
    }
  })
  output$hotable3 <- renderRHandsontable({rhandsontable(MyChanges3())})
  ###########################################
  ###########################################
  ###########################################
  
  ###########################################
  previous4 <- reactive({almacenamiento})
  MyChanges4 <- reactive({
    if(is.null(input$table4output)){return(previous4())}
    else if(!identical(previous4(),input$table4output)){
      # hot.to.df function will convert your updated table into the dataframe
      mytable4 <- as.data.frame(hot_to_r(input$table4output))
      df1<-mytable4
      df1 <- df1 |> group_by(resp, UM) |> summarise(TiempoTotalOperacion = sum(TiempStd))
      df1$TiempoTotalOperacion[2]=df1$TiempoTotalOperacion[2]+2*17
      df1
    }
  })
  output$hotable4 <- renderRHandsontable({rhandsontable(MyChanges4())})
  ###########################################
  ###########################################
  
  previous5 <- reactive({configAlmacen})
  MyChanges5 <- reactive({
    if(is.null(input$table5output)){return(previous5())}
    else if(!identical(previous5(),input$table5output)){
    mytable5 <- as.data.frame(hot_to_r(input$table5output))
    #print(mytable5[])
    df1<-mytable5
    df1 <- df1 |> group_by(resp, UM) |> summarise(TiempoTotalOperacion = sum(TiempStd))
    df1$TiempoTotalOperacion[2]=df1$TiempoTotalOperacion[2]+2*mytable4$valores[9]
    
    }
  })
  output$hotable5 <- renderRHandsontable({rhandsontable(MyChanges5())})
  ###########################################
  
  
  
  ###########################################
  #values = reactiveValues(hot = port)
  table <- reactiveValues()
  # table$table1 <-port
  table$table2 <-recepcion
  table$table3 <-ingresoRecepcion
  table$table4 <-almacenamiento
  table$table5 <-configAlmacen
  #table$table6 <-resultados1
  
  output$table1output <- renderRHandsontable({rhandsontable(table$table1)})
  output$table2output <- renderRHandsontable({rhandsontable(table$table2)})
  output$table3output <- renderRHandsontable({rhandsontable(table$table3)})
  output$table4output <- renderRHandsontable({rhandsontable(table$table4)})
  output$table5output <- renderRHandsontable({rhandsontable(table$table5)})
  output$table6output <- renderRHandsontable({rhandsontable(table$table6)})

  output$hotable2 <- renderRHandsontable({rhandsontable(MyChanges2())})
  output$hotable3 <- renderRHandsontable({rhandsontable(MyChanges3())})
  output$hotable4 <- renderRHandsontable({rhandsontable(MyChanges4())})
  output$hotable5 <- renderRHandsontable({rhandsontable(MyChanges5())})
  
  # generate table
  output$houseplant_table <- renderRHandsontable(
    rhandsontable(
      recepcion,
      rowHeaders = NULL,
      colHeaders = c("Actividad", "UM")
    ) 
  )
  
  
  
  output$download_button <- downloadHandler(
    filename = "houseplant_data.csv",
    content = function(file) {
      write.csv(hot_to_r(input$houseplant_table), file)
    }
  )
  
  
  
  #output$values2<-renderTable({
      
    #reactive(
   #   if (input$submitbutton77>0) {
        df1 <- read.csv("mysavefile.csv", header=TRUE, sep = ",")
    
    #glimpse(df1)
    #-----------------
    # Get Summary of variables
    #-----------------
    
    # set a working df
    #df1 <- blueprint
    
    # aggregate
    df1 <- df1 |> group_by(DFU) |>
      summarise(Demand = sum(Demand),
                Opening = sum(Opening),
                Supply = sum(Supply),
                Min.Cov = mean(Min.Cov),
                Max.Cov = mean(Max.Cov)
      )
    
    # let's calculate the share of Demand
    df1$Demand.pc <- df1$Demand / sum(df1$Demand)
    
    
    # keep Results
    Value_DB <- df1
    
    
    
    
    #-----------------
    # Get Sparklines Demand
    #-----------------
    
    # set a working df
    #df1 <- blueprint
    df1 <- read.csv("mysavefile.csv", header=TRUE, sep = ",")
    # replace missing values by zero
    df1$Demand <- df1$Demand |> replace_na(0)
    
    # aggregate
    df1 <- df1 |> group_by(DFU, Period) |> summarise(Quantity = sum(Demand))
    
    # generate Sparkline
    df1 <- df1 |> group_by(DFU) |> summarise(Demand.Quantity = list(Quantity))
    
    # keep Results
    Demand_Sparklines_DB <- df1
    
    
    #-----------------
    # Get Sparklines Supply
    #-----------------
    
    # set a working df
    #df1 <- blueprint
    df1 <- read.csv("mysavefile.csv", header=TRUE, sep = ",")
    # replace missing values by zero
    df1$Supply <- df1$Supply |> replace_na(0)
    
    # aggregate
    df1 <- df1 |> group_by(DFU, Period) |> summarise(Quantity = sum(Supply))
    
    # generate Sparkline
    df1 <- df1 |> group_by(DFU) |> summarise(Supply.Quantity = list(Quantity))
    
    # keep Results
    Supply_Sparklines_DB <- df1
    
    
    
    
    #-----------------
    # Merge dataframes
    #-----------------
    
    # merge
    df1 <- left_join(Value_DB, Demand_Sparklines_DB)
    df1 <- left_join(df1, Supply_Sparklines_DB)
    
    
    # reorder columns
    df1 <- df1 |> select(DFU,
                         Min.Cov, Max.Cov, 
                         Demand, Demand.pc, Demand.Quantity, Opening,
                         Supply, Supply.Quantity)
    
    
    # get results
    Summary_DB <- df1
    
    glimpse(Summary_DB)
    
    #--------------------------------------------------------------------------------------
    #    A Function for a bar chart in the background of the cell
    #--------------------------------------------------------------------------------------
    
    # Render a bar chart in the background of the cell
    bar_style <- function(width = 1, fill = "#e6e6e6", height = "75%", align = c("left", "right"), color = NULL) {
      align <- match.arg(align)
      if (align == "left") {
        position <- paste0(width * 100, "%")
        image <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", fill, position)
      } else {
        position <- paste0(100 - width * 100, "%")
        image <- sprintf("linear-gradient(90deg, transparent %1$s, %2$s %1$s)", position, fill)
      }
      list(
        backgroundImage = image,
        backgroundSize = paste("100%", height),
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center",
        color = color
      )
    }
    
    #and now let’s create the table, using the packages reactable and reactablefmtr :
    
    output$values2 <- renderReactable({
      
    reactable(df1,compact = TRUE,
              
              defaultSortOrder = "desc",
              defaultSorted = c("Demand"),
              defaultPageSize = 20,
              
              columns = list(
                
                `DFU` = colDef(name = "DFU"),
                
                
                `Demand`= colDef(
                  name = "Total Demand (units)",
                  aggregate = "sum", footer = function(values) formatC(sum(values),format="f", big.mark=",", digits=0),
                  format = colFormat(separators = TRUE, digits=0),
                  style = list(background = "yellow",fontWeight = "bold")
                ),
                
                
                `Demand.pc`= colDef(
                  name = "Share of Demand (%)",
                  format = colFormat(percent = TRUE, digits = 1)
                ), # close %
                
                
                `Supply`= colDef(
                  name = "Total Supply (units)",
                  aggregate = "sum", footer = function(values) formatC(sum(values),format="f", big.mark=",", digits=0),
                  format = colFormat(separators = TRUE, digits=0)
                ),
                
                
                
                `Opening`= colDef(
                  name = "Opening Inventories (units)",
                  aggregate = "sum", footer = function(values) formatC(sum(values),format="f", big.mark=",", digits=0),
                  format = colFormat(separators = TRUE, digits=0)
                ),
                
                
                Demand.Quantity = colDef(
                  name = "Projected Demand",
                  cell = function(value, index) {
                    sparkline(df1$Demand.Quantity[[index]])
                  }),
                
                
                
                
                Supply.Quantity = colDef(
                  name = "Projected Supply",
                  cell = function(values) {
                    sparkline(values, type = "bar"
                              #chartRangeMin = 0, chartRangeMax = max(chickwts$weight)
                    )
                  }),
                
                
                
                `Min.Cov`= colDef(
                  name = "Min Coverage (Periods)",
                  style = function(value) {
                    bar_style(width = value / max(df1$Min.Cov), fill = "hsl(208, 70%, 90%)")
                  }
                ),
                
                
                `Max.Cov`= colDef(
                  name = "Max Coverage (Periods)",
                  style = function(value) {
                    bar_style(width = value / max(df1$Max.Cov), fill = "hsl(0,79%,72%)")
                  }
                )
                
                
                
                
                
                
              ), # close columns list
              
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              
              
              columnGroups = list(
                
                colGroup(name = "Demand",
                         columns = c("Demand",
                                     "Demand.pc",
                                     "Demand.Quantity")),
                
                colGroup(name = "Supply",
                         columns = c("Supply", "Supply.Quantity"))
                
                
              )
     
    
              
    ) # se cierra la tabla reactable
    
    
    
     # } # se cierra la primera parte del if
    
    })
    
    #}) # cierra el renderTable
    #######################################################################
    #######################################################################
    #######################################################################
    ##############################              ###########################
    ############################## TABPANEL 3   ###########################    
    ##############################              ###########################
    #######################################################################
    #######################################################################
    #######################################################################

    
    output$values3 <- renderReactable({
    
      #1.2) Calculate Projected Inventories
      #Let’s apply the proj_inv() function :
      # set a working df
      #df1 <- blueprint
      df1 <- read.csv("mysavefile.csv", header=TRUE, sep = ",")
      df1 <- df1[, !(colnames(df1) %in% c("X"))]
      df1 <- as.data.frame(df1)
      
      
      # calculate
      calculated_projection_and_analysis <- planr::proj_inv(data = df1, 
                                                            DFU = DFU, 
                                                            Period = Period, 
                                                            Demand =  Demand, 
                                                            Opening = Opening, 
                                                            Supply = Supply,
                                                            Min.Cov = Min.Cov, 
                                                            Max.Cov = Max.Cov)
      
      # format as dataframe
      calculated_projection_and_analysis <-as.data.frame(calculated_projection_and_analysis)
      
      head(calculated_projection_and_analysis)
      
      #2) Analysis
      
      # filter data
      Selected_DB <- filter(calculated_projection_and_analysis, calculated_projection_and_analysis$DFU == "Item 000001")
      
      
      glimpse(Selected_DB)
      
      #First, let’s create a function status_PI.Index()
      
      # create a function status.PI.Index
      status_PI.Index <- function(color = "#aaa", width = "0.55rem", height = width) {
        span(style = list(
          display = "inline-block",
          marginRight = "0.5rem",
          width = width,
          height = height,
          backgroundColor = color,
          borderRadius = "50%"
        ))
      }
      #Let’s create a table using reactable and reactablefmtr :
      
      # set a working df
      df1 <- Selected_DB
      
      # remove not needed column
      df1 <- df1 |> select(-DFU)
      
      
      # create a f_colorpal field
      df1 <- df1 |> mutate(f_colorpal = case_when( Calculated.Coverage.in.Periods > 6 ~ "#FFA500", 
                                                   Calculated.Coverage.in.Periods > 2 ~ "#32CD32",
                                                   Calculated.Coverage.in.Periods > 0 ~ "#FFFF99",
                                                   TRUE ~ "#FF0000" ))
      
      
      
      #-------------------------
      # Create Table
      
      
      
      reactable(df1, resizable = TRUE, showPageSizeOptions = TRUE, 
                
                striped = TRUE, highlight = TRUE, compact = TRUE, 
                defaultPageSize = 20,
                
                columns = list(
                  
                  
                  Demand = colDef(
                    name = "Demand (units)",
                    
                    cell = data_bars(df1, 
                                     #round_edges = TRUE
                                     #value <- format(value, big.mark = ","),
                                     #number_fmt = big.mark = ",",
                                     fill_color = "#3fc1c9",
                                     #fill_opacity = 0.8, 
                                     text_position = "outside-end"
                    )
                    
                  ),
                  
                  
                  
                  Calculated.Coverage.in.Periods = colDef(
                    name = "Coverage (Periods)",
                    maxWidth = 90,
                    
                    cell= color_tiles(df1, color_ref = "f_colorpal")
                  ),
                  
                  
                  f_colorpal = colDef(show = FALSE), # hidden, just used for the coverages
                  
                  
                  
                  `Projected.Inventories.Qty`= colDef(
                    name = "Projected Inventories (units)",
                    format = colFormat(separators = TRUE, digits=0),
                    
                    style = function(value) {
                      if (value > 0) {
                        color <- "#008000"
                      } else if (value < 0) {
                        color <- "#e00000"
                      } else {
                        color <- "#777"
                      }
                      list(color = color
                           #fontWeight = "bold"
                      )
                    }
                  ),
                  
                  
                  
                  Supply = colDef(
                    name = "Supply (units)",
                    cell = data_bars(df1, 
                                     
                                     #round_edges = TRUE
                                     #value <- format(value, big.mark = ","),
                                     #number_fmt = big.mark = ",",
                                     fill_color = "#3CB371",
                                     #fill_opacity = 0.8, 
                                     text_position = "outside-end"
                    )
                    #format = colFormat(separators = TRUE, digits=0)
                    #number_fmt = big.mark = ","
                  ),
                  
                  
                  
                  PI.Index = colDef(
                    name = "Analysis",
                    
                    cell = function(value) {
                      color <- switch(
                        value,
                        TBC = "hsl(154, 3%, 50%)",
                        OverStock = "hsl(214, 45%, 50%)",
                        OK = "hsl(154, 64%, 50%)",
                        Alert = "hsl(30, 97%, 70%)",
                        Shortage = "hsl(3, 69%, 50%)"
                      )
                      PI.Index <- status_PI.Index(color = color)
                      tagList(PI.Index, value)
                    }),
                  
                  
                  
                  `Safety.Stocks`= colDef(
                    name = "Safety Stocks (units)",
                    format = colFormat(separators = TRUE, digits=0)
                  ),
                  
                  `Maximum.Stocks`= colDef(
                    name = "Maximum Stocks (units)",
                    format = colFormat(separators = TRUE, digits=0)
                  ),
                  
                  `Opening`= colDef(
                    name = "Opening Inventories (units)",
                    format = colFormat(separators = TRUE, digits=0)
                  ),
                  
                  
                  `Min.Cov`= colDef(name = "Min Stocks Coverage (Periods)"),
                  
                  `Max.Cov`= colDef(name = "Maximum Stocks Coverage (Periods)"),
                  
                  
                  # ratios
                  `Ratio.PI.vs.min`= colDef(name = "Ratio PI vs min"),
                  
                  `Ratio.PI.vs.Max`= colDef(name = "Ratio PI vs Max")
                  
                  
                  
                  
                ), # close columns lits
                
                columnGroups = list(
                  colGroup(name = "Projected Inventories", columns = c("Calculated.Coverage.in.Periods", 
                                                                       "Projected.Inventories.Qty")),
                  
                  colGroup(name = "Stocks Levels Parameters", columns = c("Min.Cov", 
                                                                          "Max.Cov",
                                                                          "Safety.Stocks",
                                                                          "Maximum.Stocks")),
                  
                  colGroup(name = "Analysis Features", columns = c("PI.Index", 
                                                                   "Ratio.PI.vs.min",
                                                                   "Ratio.PI.vs.Max"))
                  
                )
                
      ) # close reactable
    
    
})
    
    #######################################################################
    #######################################################################
    #######################################################################
    ##############################              ###########################
    ############################## TABPANEL 4   ###########################    
    ##############################              ###########################
    #######################################################################
    #######################################################################
    #######################################################################
    
    output$values4 <- renderReactable({
      #df1 <- blueprint
      df1 <- read.csv("mysavefile.csv", header=TRUE, sep = ",")
      df1 <- df1[, !(colnames(df1) %in% c("X"))]
      df1 <- as.data.frame(df1)
      
      
      # calculate
      calculated_projection_and_analysis <- planr::proj_inv(data = df1, 
                                                            DFU = DFU, 
                                                            Period = Period, 
                                                            Demand =  Demand, 
                                                            Opening = Opening, 
                                                            Supply = Supply,
                                                            Min.Cov = Min.Cov, 
                                                            Max.Cov = Max.Cov)
      
      # format as dataframe
      calculated_projection_and_analysis <-as.data.frame(calculated_projection_and_analysis)
      
      head(calculated_projection_and_analysis)
      
      #2) Analysis
      
      # filter data
      Selected_DB <- filter(calculated_projection_and_analysis, calculated_projection_and_analysis$DFU == "Item 000001")
      
      
      #glimpse(Selected_DB)
      
      #First, let’s create a function status_PI.Index()
      
      # create a function status.PI.Index
      status_PI.Index <- function(color = "#aaa", width = "0.55rem", height = width) {
        span(style = list(
          display = "inline-block",
          marginRight = "0.5rem",
          width = width,
          height = height,
          backgroundColor = color,
          borderRadius = "50%"
        ))
      }
      #Let’s create a table using reactable and reactablefmtr :
      
      # set a working df
      df1 <- Selected_DB
      
      # remove not needed column
      df1 <- df1 |> select(-DFU)
      
      #2.2) For multiple items
      
      # set a working dataframe
      #df1 <- calculated_projection_and_analysis
      df1 <- calculated_projection_and_analysis
      
      #------------------------------
      # Filter
      
      # filter Period based on those Starting and Ending Periods
      df1 <- filter(df1, df1$Period >= "2022-07-03" & df1$Period <= "2022-09-25")
      
      
      # Highlight only the OverStock situations
      df1$PI.Index <- if_else(df1$PI.Index == "OverStock", "OverStock", "")
      # replace missing values by zero
      df1$Demand <- df1$Demand |> replace_na(0)
      
      # keep results 
      Initial_DB <- df1
      
      
      
      #------------------------------
      # Transform
      
      
      
      #--------
      # Create a Summary database
      #--------
      
      # set a working df
      df1 <- Initial_DB
      
      # aggregate
      df1 <- df1 |> group_by(DFU) |> summarise(Demand.Qty = sum(Demand))
      
      # Get Results
      Value_DB <- df1
      
      
      
      #--------
      # Create the SRA
      #--------
      
      # set a working df
      df1 <- Initial_DB
      
      #------------------------------
      # keep only the needed columns
      df1 <- df1 |> select(DFU, Period, PI.Index)
      
      # spread data
      df1 <- df1 %>% spread(Period, PI.Index)
      
      # replace missing values by zero
      df1[is.na(df1)] <- 0
      
      # Get Results
      SRA_DB <- df1
      
      
      
      
      #--------
      # Merge both database
      #--------
      
      # merge both databases
      df1 <- left_join(Value_DB, SRA_DB)
      
      
      ##############################################
      # Sort by Demand.Qty descending
      df1 <- df1 |> arrange(-Demand.Qty)
      
      
      # rename column
      df1 <- df1 |> rename("Total Demand (units)" = Demand.Qty)
      
      
      # Get Results
      Interim_DB <- df1
      # set a working df
      df1 <- Interim_DB
      
      #2.3) Cockpit
      
      #------------------------------
      # Get data
      df1 <- calculated_projection_and_analysis
      
      
      #------------------------------
      # Filter
      
      # filter Period based on those Starting and Ending Periods
      df1 <- filter(df1, df1$Period >= "2022-07-03" & df1$Period <= "2022-09-25")
      
      
      # keep this initial dataset
      Initial_DB <- df1
      
      
      
      
      
      #-----------------
      # Get Summary of variables
      #-----------------
      
      # set a working df
      df1 <- Initial_DB
      
      # aggregate
      df1 <- df1 |> group_by(DFU) |>
        summarise(Demand = sum(Demand),
                  Opening = sum(Opening),
                  Supply = sum(Supply)
        )
      
      # let's calculate the share of Demand
      df1$Demand.pc <- df1$Demand / sum(df1$Demand)
      
      
      # keep Results
      Value_DB <- df1
      
      
      
      
      #-----------------
      # Get Sparklines Demand
      #-----------------
      
      # set a working df
      df1 <- Initial_DB
      
      # replace missing values by zero
      df1$Demand <- df1$Demand |> replace_na(0)
      
      # aggregate
      df1 <- df1 |> group_by(DFU, Period) |>
        summarise(Quantity = sum(Demand))
      
      # generate Sparkline
      df1 <- df1 |> group_by(DFU) |> summarise(Demand.Quantity = list(Quantity))
      
      # keep Results
      Demand_Sparklines_DB <- df1
      
      
      #-----------------
      # Get Sparklines Supply
      #-----------------
      
      # set a working df
      df1 <- Initial_DB
      
      # replace missing values by zero
      df1$Supply <- df1$Supply |> replace_na(0)
      
      # aggregate
      df1 <- df1 |> group_by(DFU, Period) |>
        summarise(Quantity = sum(Supply))
      
      # generate Sparkline
      df1 <- df1 |> group_by(DFU) |> summarise(Supply.Quantity = list(Quantity))
      
      # keep Results
      Supply_Sparklines_DB <- df1
      
      
      
      
      
      
      #-----------------
      # Get Sparklines Projected Inventories
      #-----------------
      
      # set a working df
      df1 <- Initial_DB
      
      # replace missing values by zero
      df1$Projected.Inventories.Qty <- df1$Projected.Inventories.Qty |> replace_na(0)
      
      # aggregate
      df1 <- df1 |> group_by(DFU, Period) |> summarise(Quantity = sum(Projected.Inventories.Qty))
      
      # generate Sparkline
      df1 <- df1 |> group_by(DFU) |> summarise(PI.Quantity = list(Quantity))
      
      # keep Results
      PI_Sparklines_DB <- df1
      
      
      
      
      
      #--------
      # Check if OverStock
      #--------
      
      # set a working df
      df1 <- Initial_DB
      
      # focus on OverStocks, by filtering data
      df1$PI.Index.Value <- if_else(df1$PI.Index == "OverStock", 1, 0)
      
      # aggregate
      df1 <- df1 |> group_by(DFU) |> summarise(OverStock = max(PI.Index.Value))
      
      # Get Results
      OverStock_DB <- df1
      
      
      
      #--------
      # Check if Alert
      #--------
      
      # set a working df
      df1 <- Initial_DB
      
      # focus on Alert, by filtering data
      df1$PI.Index.Value <- if_else(df1$PI.Index == "Alert", 1, 0)
      
      # aggregate
      df1 <- df1 |> group_by(DFU) |> summarise(Alert = max(PI.Index.Value))
      
      # Get Results
      Alert_DB <- df1
      
      
      
      
      #--------
      # Check if Shortage
      #--------
      
      # set a working df
      df1 <- Initial_DB
      
      # focus on Shortage, by filtering data
      df1$PI.Index.Value <- if_else(df1$PI.Index == "Shortage", 1, 0)
      
      # aggregate
      df1 <- df1 |> group_by(DFU) |> summarise(Shortage = max(PI.Index.Value))
      
      # Get Results
      Shortage_DB <- df1
      
      
      
      
      
      
      #-----------------
      # Merge dataframes
      #-----------------
      
      # merge
      df1 <- left_join(Value_DB, Demand_Sparklines_DB)
      df1 <- left_join(df1, Supply_Sparklines_DB)
      df1 <- left_join(df1, PI_Sparklines_DB)
      df1 <- left_join(df1, OverStock_DB)
      df1 <- left_join(df1, Alert_DB)
      df1 <- left_join(df1, Shortage_DB)
      
      
      # reorder columns
      df1 <- df1 |> select(DFU, Demand, Demand.pc, Demand.Quantity,
                           Supply, Supply.Quantity,
                           Opening,
                           PI.Quantity,
                           OverStock,
                           Alert,
                           Shortage)
      
      
      
      
      # replace figures by values
      df1$OverStock <- if_else(df1$OverStock == 1, "Y", "")
      df1$Alert <- if_else(df1$Alert == 1, "Y", "")
      df1$Shortage <- if_else(df1$Shortage == 1, "Y", "")
      
      
      
      # get results
     # Summary_DB <- df1
      
      #--------------------------------------------------------------------------------------
      #    A Function to define a Badge Status in the reactable
      #--------------------------------------------------------------------------------------
      
      status_badge <- function(color = "#aaa", width = "9px", height = width) {
        span(style = list(
          display = "inline-block",
          marginRight = "8px",
          width = width,
          height = height,
          backgroundColor = color,
          borderRadius = "50%"
        ))
      }
      
      reactable(df1,compact = TRUE,
                
                defaultSortOrder = "desc",
                defaultSorted = c("Demand"),
                defaultPageSize = 20,
                
                columns = list(
                  
                  `DFU` = colDef(name = "DFU"),
                  
                  
                  `Demand`= colDef(
                    name = "Total Demand (units)",
                    aggregate = "sum", footer = function(values) formatC(sum(values),format="f", big.mark=",", digits=0),
                    format = colFormat(separators = TRUE, digits=0),
                    style = list(background = "yellow",fontWeight = "bold")
                  ),
                  
                  
                  `Demand.pc`= colDef(
                    name = "Share of Demand (%)",
                    format = colFormat(percent = TRUE, digits = 1)
                  ), # close %
                  
                  
                  `Supply`= colDef(
                    name = "Total Supply (units)",
                    aggregate = "sum", footer = function(values) formatC(sum(values),format="f", big.mark=",", digits=0),
                    format = colFormat(separators = TRUE, digits=0)
                  ),
                  
                  
                  
                  `Opening`= colDef(
                    name = "Opening Inventories (units)",
                    aggregate = "sum", footer = function(values) formatC(sum(values),format="f", big.mark=",", digits=0),
                    format = colFormat(separators = TRUE, digits=0)
                  ),
                  
                  
                  Demand.Quantity = colDef(
                    name = "Projected Demand",
                    cell = function(value, index) {
                      sparkline(df1$Demand.Quantity[[index]])
                    }),
                  
                  
                  
                  
                  Supply.Quantity = colDef(
                    name = "Projected Supply",
                    cell = function(values) {
                      sparkline(values, type = "bar"
                                #chartRangeMin = 0, chartRangeMax = max(chickwts$weight)
                      )
                    }),
                  
                  
                  PI.Quantity = colDef(
                    name = "Projected Inventories",
                    cell = function(values) {
                      sparkline(values, type = "bar"
                                #chartRangeMin = 0, chartRangeMax = max(chickwts$weight)
                      )
                    }),
                  
                  
                  
                  OverStock = colDef(
                    name = "OverStock",
                    
                    cell = function(value) {
                      color <- switch(
                        value,
                        N = "hsl(120,61%,50%)",
                        Y = "rgb(135,206,250)"
                      )
                      badge <- status_badge(color = color)
                      tagList(badge, value)
                    }),
                  
                  
                  Alert = colDef(
                    name = "Alert",
                    
                    cell = function(value) {
                      color <- switch(
                        value,
                        N = "hsl(120,61%,50%)",
                        Y = "hsl(39,100%,50%)"
                      )
                      badge <- status_badge(color = color)
                      tagList(badge, value)
                    }),
                  
                  
                  Shortage = colDef(
                    name = "Shortage",
                    
                    cell = function(value) {
                      color <- switch(
                        value,
                        N = "hsl(120,61%,50%)",
                        Y = "hsl(16,100%,50%)"
                      )
                      badge <- status_badge(color = color)
                      tagList(badge, value)
                    })
                  
                  
                  
                  
                  
                  
                ), # close columns list
                
                defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
                
                
                columnGroups = list(
                  
                  colGroup(name = "Demand",
                           columns = c("Demand",
                                       "Demand.pc",
                                       "Demand.Quantity")),
                  
                  colGroup(name = "Supply",
                           columns = c("Supply", "Supply.Quantity")),
                  
                  
                  colGroup(name = "Inventories",
                           columns = c("Opening", "PI.Quantity")),
                  
                  
                  colGroup(name = "Analysis",
                           columns = c("OverStock", "Alert", "Shortage"))
                  
                  
                )
                
      ) # close reactable
      
      
      
      
  
})

########################################################################
########################################################################
########################################################################
########################################################################

    
        
  } # cierra el server

# Run the application 
shinyApp(ui = ui, server = server)
