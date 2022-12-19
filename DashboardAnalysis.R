options(shiny.maxRequestSize = 30*1024^2)
options(scipen = 999)
options(useFancyQuotes = FALSE)
options(warn = -1)


# Load R packages
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)
library(RMySQL)
library(DBI)
library(scales)
library(shinydashboard)
library(shinyscreenshot)

# Loading GIF
img <- 'https://c.tenor.com/5o2p0tH5LFQAAAAi/hug.gif'

# GIF Size
imgsize <- "auto 10%"

# Database table name
table_name = "trx_report"

# Database connection
connectFunction <- function(){
  # Connect to database
  mydb = dbConnect(MySQL(), user='root', password='NEW_USER_PASSWORD', dbname='test', host='127.0.0.1')
  query_colnames <- paste0("SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_SCHEMA = Database() AND TABLE_NAME = '",table_name,"'" ," AND COLUMN_NAME != 'row_names'")
  # Obtain all column names
  all_colnames <- dbGetQuery(mydb, query_colnames)
  # Disconnect from database
  dbDisconnect(mydb)
  
  all_colnames
}

getMerchant <- function(){
  
    # Query database
    query_merchant <- paste("SELECT DISTINCT merchantID FROM ",table_name)
    # connect to database
    mydb = dbConnect(MySQL(), user='root', password='NEW_USER_PASSWORD', dbname='test', host='127.0.0.1')
    merchants <- dbGetQuery(mydb,query_merchant)
    # disconnect from database
    dbDisconnect(mydb)
    
    merchants<-sort(merchants$merchantID)
    merchants
}

getChannel <- function(){
  
  # Query database
  query_channel <- paste("SELECT DISTINCT channel FROM ",table_name)
  # connect to database
  mydb = dbConnect(MySQL(), user='root', password='NEW_USER_PASSWORD', dbname='test', host='127.0.0.1')
  channels <- dbGetQuery(mydb,query_channel)
  # disconnect from database
  dbDisconnect(mydb)
  
  channels<-sort(channels$channel)
  
  channels
}

getCurrency <- function(){
  
  # Query database
  query_currency <- paste("SELECT DISTINCT currency FROM ",table_name)
  # connect to database
  mydb = dbConnect(MySQL(), user='root', password='NEW_USER_PASSWORD', dbname='test', host='127.0.0.1')
  currencies <- dbGetQuery(mydb,query_currency)
  # disconnect from database
  dbDisconnect(mydb)
  currencies<-sort(currencies$currency)
  
  currencies
}

getStatus <- function(){
  
  # Query database
  query_status <- paste("SELECT DISTINCT status FROM ",table_name)
  # connect to database
  mydb = dbConnect(MySQL(), user='root', password='NEW_USER_PASSWORD', dbname='test', host='127.0.0.1')
  stats <- dbGetQuery(mydb,query_status)
  # disconnect from database
  dbDisconnect(mydb)
  
  stats <- sort(stats$status)
  stats
}

# Function to check whether the input is of a date type
is.date <- function(x) inherits(x, 'Date')


### DEFINE UI ###
ui <- fluidPage(theme = shinytheme("yeti"),
                
                
                navbarPage(
                  "Dashboard Template",
                  
                  
                  tabPanel("Create Visualisation",
                           # input panel
                           sidebarPanel(
                             
                             textInput("titleInput","Enter the Title"),

                             uiOutput("yVar"),uiOutput("xVar"), 
                             
                             
                             dateInput("from", "From:",value = ,width='110px'),
                             dateInput("to", "To:",width='110px'),
                             uiOutput("merchantCol"),uiOutput("channelCol"),uiOutput("currencyCol"),uiOutput("statusCol"),
                             
                             uiOutput("display"),
                             uiOutput("legend_selection"),
                             uiOutput("functions"),
                             
                             # Javasript Code
                             singleton(tags$head(HTML("
                                  <script type='text/javascript'>
                                  
                                  /* When recalculating starts, show loading screen */
                                  $(document).on('shiny:recalculating', function(event) {
                                  $('div#divLoading').addClass('show');
                                  });
                                  
                                  /* When new value or error comes in, hide loading screen */
                                  $(document).on('shiny:value shiny:error', function(event) {
                                  $('div#divLoading').removeClass('show');
                                  });
  
                                  </script>"))),
                             
                             # CSS Code
                             singleton(tags$head(HTML(paste0("
                                    <style type='text/css'>
                                    #divLoading
                                    {
                                      display : none;
                                      }
                                      #divLoading.show
                                      {
                                      display : block;
                                      position : fixed;
                                      z-index: 100;
                                      background-image : url('",img,"');
                                      background-size:", imgsize, ";
                                      background-repeat : no-repeat;
                                      background-position : center;
                                      left : 0;
                                      bottom : 0;
                                      right : 0;
                                      top : 0;
                                      }
                                      #loadinggif.show
                                      {
                                      left : 50%;
                                      top : 50%;
                                      position : absolute;
                                      z-index : 101;
                                      -webkit-transform: translateY(-50%);
                                      transform: translateY(-50%);
                                      width: 100%;
                                      margin-left : -16px;
                                      margin-top : -16px;
                                      }
                                      div.content {
                                      width : 800px;
                                      height : 800px;
                                      }
                                      
                                    </style>")))),
                             
                             # HTML Code   
                             
                             box(tags$body(HTML("<div id='divLoading'> </div>")),width = 12, height = 50),
                             
                             uiOutput("transition"),
                             actionButton("buttonSave", "Graph Preview"),
                             actionButton("buttonAdd", "Add to Dashboard"),
                             actionButton("download", "Save Dashboard")
                             
                             
                           ), # sidebarPanel
                           
                           # preview graph panel
                           mainPanel(
                             h1("Visualisation Preview"),
                             # Output: Graph ----
                             plotlyOutput(outputId = "graphPlot",height ='700px')
                          
                             
                             
                           ) # mainPanel
                           
                  ) #Navbar 
                  
                ), # navbarPage 
                #Dashboard
                titlePanel(h1("Dashboard",
                style='color:#fff;background-color:#383838;
                     padding-left: 15px;font-size:35px;')),
                fluidPage(id = "allPlots",uiOutput("plots"))
) # fluidPage


### DEFINE SERVER FUNCTION ###  
server <- function(input, output,session) {
  
  ### REACTIVE VARIABLES ###
  
  # gets the x variable name, will be used to change the plot legends
  xVarName <- reactive({
    input$xInput
  }) 
  
  # gets the Y variable name, will be used to change the plot legends
  yVarName <- reactive({
    input$yInput
  }) 
  
  # gets the legend variable name, will be used to change the plot legends
  lengendSelection <- reactive({
    input$legendInput
  }) 
  
  # Filters the data by the input of the user
  filter_data <- reactive({
    
    merchants <- input$merchant_selection
    channels <- input$channel_selection
    status <- input$status_selection
    currencies <- input$currency_selection
    
    if (input$yInput == "percentage"){
      if(input$legendInput != "None"){
        query_filter = paste0("Select * ,sum(trx_num) as 'rateCount' FROM ",table_name, " WHERE date>='",input$from,"' AND date<='",input$to,"' ")
        
      }
      else{
        query_filter = paste0("Select date ,sum(trx_num) as 'rateCount' FROM ",table_name, " WHERE date>='",input$from,"' AND date<='",input$to,"'")
        
      }
      if(input$xInput =="date"){
        if(input$legendInput != "None"){
          total_query = paste0("Select date, merchantID ,sum(trx_num) as 'totalCount' FROM ",table_name, " WHERE date>='",input$from,"' AND date<='",input$to,"' group by date, merchantID")
          
        }
        else{
          total_query = paste0("Select date ,sum(trx_num) as 'totalCount' FROM ",table_name, " WHERE date>='",input$from,"' AND date<='",input$to,"'")
          
        }
      }
      else{
        if(input$legendInput != "None"){
          total_query = paste0("Select date, merchantID ,sum(trx_num) as 'totalCount' FROM ",table_name, " WHERE date>='",input$from,"' AND date<='",input$to,"' group by ", input$legendInput)
        }
        else{
          total_query = paste0("Select date, merchantID ,sum(trx_num) as 'totalCount' FROM ",table_name, " WHERE date>='",input$from,"' AND date<='",input$to, "'")
          
        }
      }
    }
    else{
      query_filter<-paste0("Select * FROM ",table_name, " WHERE date>='",input$from,"' AND date<='",input$to,"' ")
    }
      
    if(!is.null(merchants)){
      merchantSelected <- paste(sQuote(input$merchant_selection), collapse=",")
      merchantSelected <- gsub(" ' ",' " ',merchantSelected)
      merchantQuery <- paste0(" AND merchantID IN (",merchantSelected,")")
      query_filter<-paste0(query_filter,merchantQuery)
      if(input$legendInput == "None"){
        total_query <- paste0(total_query,merchantQuery)
      }
    }
    if(!is.null(channels)){
      channelSelected <- paste(sQuote(input$channel_selection), collapse=",")
      channelQuery <- paste0(" AND channel IN (",channelSelected,")")
      query_filter<-paste0(query_filter,channelQuery)
      if(input$legendInput == "None"){
        total_query <- paste0(total_query,channelQuery)
      }
    }
    if(!is.null(status)){
      statusSelected <- paste(sQuote(input$status_selection), collapse=",")
      statusQuery <- paste0(" AND status IN (",statusSelected,")")
      query_filter<-paste0(query_filter,statusQuery)

    }
    if(!is.null(currencies)){
      currencySelected <- paste(sQuote(input$currency_selection), collapse=",")
      currencyQuery <- paste0(" AND currency IN (",currencySelected,")")
      query_filter<-paste0(query_filter,currencyQuery)
      if(input$legendInput == "None"){
        total_query <- paste0(total_query,currencyQuery)
      }
      
    }
    
    mydb = dbConnect(MySQL(), user='root', password='NEW_USER_PASSWORD', dbname='test', host='127.0.0.1')
    
    if (input$yInput == "percentage"){
      if(input$xInput == "date"){
        if(input$legendInput != "None"){
          query_filter = paste0(query_filter," group by date, merchantID")
          
        }
        else{
          query_filter = paste0(query_filter," group by date")
          total_query = paste0(total_query," group by date")
        }

      }
      else{
        if(input$legendInput != "None"){
          query_filter = paste0(query_filter, " group by merchantID")
        }
      }
      filter <- dbGetQuery(mydb,query_filter)
      filter2 <- dbGetQuery(mydb,total_query)
      
      data.frame(filter)
      data.frame(filter2)
      
      if(input$legendInput != "None"){
        filter <- merge(filter,filter2,by=c("date","merchantID"))
        filter$percentage <- round((filter$rateCount / filter$totalCount)*100,2)
      }
      else{
        filter <- merge(filter,filter2,by=c("date"))
        filter$percentage <- round((filter$rateCount/filter$totalCount)*100,2)
      }
      
    }
    else{
      
      filter <- dbGetQuery(mydb,query_filter)
      data.frame(filter)
    }
    
    
    dbDisconnect(mydb)
    
    filter
  })
  

  
  # Defining & initializing the reactiveValues object
  counter <- reactiveValues(countervalue = 0) 
  
  ### RENDER OBJECTS ###
  
  # Description for display model
  output$text <- renderText({ 
    if (input$displayModel == "Line Chart"|input$displayModel == "Stacked Bar Chart"|input$displayModel == "Bar Chart"){
      paste("Select Display Model", "(X-axis: Qualitative, Y-axis: Quantitative)", sep="\n")
    }
    else{
      paste("Select Display Model", "(X-axis: Quantitative, Y-axis: Quantitative)", sep="\n")
    }
    
  })

  # UI selection input for X axis
  output$xVar <- renderUI({
    if (input$yInput == "percentage"){
      selectInput("xInput", "X axis: ",
                  choices= c("date"))
    }
    else {
      selectInput("xInput", "X axis:",
                  choices=connectFunction())
    }
  })
  
  # UI selection input for Y axis
  output$yVar <- renderUI({
    selectInput("yInput", "Y axis:",
                choices= c("bill_amt","actual_amt","trx_num","percentage"))
  })
  
  # UI selection input for legend
  output$legend_selection <- renderUI({
    if(input$yInput == "percentage"){
      if(is.null(input$merchant_selection)){
        selectInput("legendInput", "Select Legend (Only merchantID available as legend) :",
                    choices=c("None"))
      }
      else{
        selectInput("legendInput", "Select Legend (Only merchantID available as legend) :",
                    choices=c("None","merchantID"))
      }
      
    }
    else{
      selectInput("legendInput", "Select Legend (Group by) :",
                  choices=c("None","channel","merchantID","status","currency"))
    }
    
  })
  
  
  output$display <- renderUI({
    if (input$yInput == "percentage"){
      selectInput("displayModel",textOutput("text"),c("Line Chart"))
    }
    else{
      selectInput("displayModel",textOutput("text"),c("Scatter Plot","Bar Chart","Stacked Bar Chart","Line Chart"))
    }
  })
  
  
  # UI selection input for merchant
  output$merchantCol <- renderUI({
    
    pickerInput("merchant_selection", "Merchants:", choices = getMerchant(),options = list(`actions-box` = TRUE,`title` = "All",`dropupAuto` = FALSE), multiple = TRUE  )
    
  })
  
  # UI selection input for channel
  output$channelCol <- renderUI({
    
    pickerInput("channel_selection", "Channels:", choices = getChannel(), options = list(`actions-box` = TRUE,`title` = "All",`dropupAuto` = FALSE) , multiple = TRUE)
    
  })
  
  # UI selection input for channel
  output$statusCol <- renderUI({
    
    pickerInput("status_selection", "Status:", choices = getStatus(), options = list(`actions-box` = TRUE,`title` = "All",`dropupAuto` = FALSE) , multiple = TRUE)
    
  })
  
  # UI selection input for currency
  output$currencyCol <- renderUI({
    
    pickerInput("currency_selection", "currency:", choices = getCurrency(), options = list(`actions-box` = TRUE,`title` = "All",`dropupAuto` = FALSE) , multiple = TRUE)
    
  })
  
  # UI selection input for function
  output$functions <- renderUI({
    if(input$yInput == "trx_num"){
      selectInput("function_selection", "Function:",c("min","max","count"),multiple = FALSE)
    }
    if(input$displayModel != "Scatter Plot" & input$yInput != "percentage" ){
      selectInput("function_selection", "Function:",c("sum","mean","median","min","max","count"),multiple = FALSE)
    }
  })
  
  # UI checkbox input for transition over time
  output$transition <- renderUI({
    if(input$xInput !="date" & input$yInput != "percentage"){
      checkboxInput("transition", "Transition over time", FALSE)
    }
  })
  
  # Generating the preview plot
  output$graphPlot <- renderPlotly({
    prev_plot <- savePlot()
    ggplotly(prev_plot)
  })
  
  # Generating the plots to be shown in the dashboard
  output$plots <- renderUI({
    
    addPlot()
    plot_output_list <- lapply(seq_along(1:counter$countervalue), function(i)  {
      plotname <- paste("plot", i, sep="")
      column(width = 4,
             tags$div(style = "margin-top: 10px; margin-bottom: 10px;",
                      plotlyOutput(outputId = plotname)
             ))
      
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  
  ### REACTIVE OBSERVERS ###
  
  # Continually monitors any changes to the input title
  observe({
    updateTextInput(session,'titleInput')
  }) 
  
  # Continually monitors any changes to the x-axis input
  observe({

    updateSelectInput(session, 'xInput', choices = connectFunction())

  }) 
  
  # Continually monitors any changes to the y-axis input
  observe({

    updateSelectInput(session, 'yInput', choices = c("bill_amt","actual_amt","trx_num","percentage"))

  }) 
  
  
  # Continually monitors any changes to the legend input
  observe({
    
    updateSelectInput(session, 'legendInput', choices = c("None","channel","merchantID","status","currency"))

  }) 
  
  # Continually monitors any changes to the merchant input
  observe({

    updatePickerInput(session, 'merchant_selection', choices = getMerchant())
    
  })    
  
  # Continually monitors any changes to the channel input
  observe({
    
    updatePickerInput(session, 'channel_selection', choices = getChannel())
    
  }) 
  
  # Continually monitors any changes to the currency input
  observe({
    
    updatePickerInput(session, 'currency_selection', choices = getCurrency())
    
  }) 
  
  # Continually monitors any changes to the status input
  observe({
    
    updatePickerInput(session, 'status_selection', choices = getStatus())
    
  }) 
  
  # Continually monitors any changes to the function input
  observe({
    updateSelectInput(session, 'function_selection', choices = c("sum","mean","median","min","max","count"))
  })   
  
  
  ### OBSERVE EVENT AND EVENT REACTIVE FOR EVENT HANDLING ###
  
  # Show notification when "Graph preview" button is pressed
  observeEvent(input$buttonSave, {
    
    showNotification("Loading Graph Preview")
    
  })
  
  # To save plot generated by users
  savePlot <- eventReactive(input$buttonSave,{
    df <- filter_data()
    validate(
      need(nrow(df)!=0,'NO DATA.')
    )

    df$date = as.Date(df$date)
    
    if(input$legendInput != "None"){
      
      validate(
        need(((typeof(df[[input$yInput]])=="double" | typeof(df[[input$yInput]])=="integer")&!is.date(df[[input$yInput]]) )| input$yInput == "percentage", 'Y-axis needs to be a quantitative data')
      )
      validate(
        need(df[[input$xInput]]!=df[[input$yInput]], 'Axes cannot be the same')
      )
      
      if (input$displayModel != "Scatter Plot" & input$yInput != "percentage"){
        if(input$function_selection == "count"){
          fun = "length"
        }
        
        else{
          fun = input$function_selection
        }
        
        if(input$xInput=='date'||(!is.null(input$transition) && !input$transition)){
          df_function <- aggregate(df[[input$yInput]], list(df[[input$xInput]],df[[input$legendInput]]), FUN=fun)
        }
        else{
          df_function <- aggregate(df[[input$yInput]], list(df[[input$xInput]],df[[input$legendInput]],df[['date']]), FUN=fun)
          names(df_function)[names(df_function) == 'Group.3'] <- 'date'
          
          dfa <- df_function %>% complete(date,Group.1, fill = list(x= 0))
          names(dfa)[names(dfa) == 'Group.1'] <- input$xInput
          names(dfa)[names(dfa) == 'x'] <- input$yInput
          legend <- 'legend'
          names(dfa)[names(dfa) == 'Group.2'] <- legend
          x_axis_animation <- dfa[[input$xInput]]
          y_axis_animation <- dfa[[input$yInput]]
          legend_input_animation <- dfa[[legend]]
          
        }
 
        names(df_function)[names(df_function) == 'Group.1'] <- input$xInput
        names(df_function)[names(df_function) == 'x'] <- input$yInput
        legend <- 'legend'
        names(df_function)[names(df_function) == 'Group.2'] <- legend
        x_axis <- df_function[[input$xInput]]
        y_axis <- df_function[[input$yInput]]
        legend_input <- df_function[[legend]]
        
        }
    
    
      if (input$displayModel == "Line Chart"){
        if(input$xInput=='date' || (!is.null(input$transition) && !input$transition)){
          if(input$yInput == "percentage"){
            p <- plot_ly(df, x = df[[input$xInput]], y = df[[input$yInput]],color = df[[input$legendInput]], type = 'scatter', mode = 'lines') %>%
              layout(title = input$titleInput)
          }
          else{
            p <- plot_ly(df_function, x = x_axis, y = y_axis,color = legend_input, type = 'scatter', mode = 'lines') %>%
              layout(title = input$titleInput)
          }
        }
        else{
          if(input$yInput == "percentage" & (!is.null(input$transition) && !input$transition)){
            p <- plot_ly(df, x = df[[input$xInput]], y = df[[input$yInput]],color = df[[input$legendInput]], type = 'scatter', mode = 'lines') %>%
              layout(title = input$titleInput)
          }
          
          else{
            p <- plot_ly(dfa, x = x_axis_animation, y = y_axis_animation,color = legend_input_animation, frame=(as.factor(dfa[['date']])), type = 'scatter', mode = 'lines') %>% 
              layout(title = input$titleInput)%>% 
              layout(showlegend = FALSE) %>%  
              animation_opts(frame = 1500, transition = 1000, easing = "elastic", redraw = FALSE) %>%
              animation_slider(
                currentvalue = list(prefix = "DATE: ", font = list(color="red"))
            )
          }
        }
          
      }
      
      else if(input$displayModel == "Bar Chart"){
        if(input$xInput=='date'|| (!is.null(input$transition) && !input$transition)){
          if(input$yInput == "percentage"){
            p <- plot_ly(df, x = df[[input$xInput]], y = df[[input$yInput]],color = df[[input$legendInput]], type = 'bar') %>%
              layout(title = input$titleInput)
          }
          else{
            p <- plot_ly(df_function, x = x_axis, y = y_axis,color = legend_input, type = 'bar') %>% 
              layout(title = input$titleInput)   
          }
        }
        else{
          if(input$yInput == "percentage" & (!is.null(input$transition) && !input$transition)){
            p <- plot_ly(df, x = df[[input$xInput]], y = df[[input$yInput]],color = df[[input$legendInput]], type = 'bar') %>%
              layout(title = input$titleInput)
          }
          else{
            p <- plot_ly(dfa, x = x_axis_animation, y = y_axis_animation,color = legend_input_animation, frame=(dfa[['date']]), type = 'bar') %>% 
              layout(title = input$titleInput)%>% 
              layout(showlegend = FALSE) %>%  
              animation_opts(frame = 1500, transition = 1000, easing = "elastic", redraw = FALSE) %>%
              animation_slider(
                currentvalue = list(prefix = "DATE: ", font = list(color="red"))
              )
          }
        }
      }
      else if(input$displayModel == "Stacked Bar Chart"){
        if(input$xInput=='date'||(!is.null(input$transition) && !input$transition)){
          if(input$yInput == "percentage"){
            p <- plot_ly(df, x = df[[input$xInput]], y = df[[input$yInput]],color = df[[input$legendInput]], type = 'bar') %>%
              layout(title = input$titleInput, barmode = 'stack')
          }
          else{
            p <- plot_ly(df_function, x = x_axis, y = y_axis,color = legend_input, type = 'bar') %>% 
              layout(title = input$titleInput , barmode = 'stack') 
          }
        }
        else{
          if(input$yInput == "percentage" & (!is.null(input$transition) && !input$transition)){
            p <- plot_ly(df, x = df[[input$xInput]], y = df[[input$yInput]],color = df[[input$legendInput]], type = 'bar') %>%
              layout(title = input$titleInput , barmode = 'stack')
          }
          else {          
            p <- plot_ly(dfa, x = x_axis_animation, y = y_axis_animation,color = legend_input_animation, frame=(as.factor(dfa[['date']])), type = 'bar') %>% 
              layout(title = input$titleInput , barmode = 'stack')%>% 
              layout(showlegend = FALSE) %>%  
              animation_opts(frame = 1500, transition = 1000, easing = "elastic", redraw = FALSE) %>%
              animation_slider(
                currentvalue = list(prefix = "DATE: ", font = list(color="red"))
              )}
          
        }
      }
      else{
        x_axis <- df[[input$xInput]]
        y_axis <- df[[input$yInput]]
        legend_input <- df[[input$legendInput]]
        if(input$xInput=='date'||!input$transition){
          if(input$yInput == "percentage" & (!is.null(input$transition) && !input$transition)){
            p <- plot_ly(df, x = df[[input$xInput]], y = df[[input$yInput]],color = df[[input$legendInput]],type = 'scatter', mode = 'markers') %>%
              layout(title = input$titleInput)
          }
          else{
            p <- plot_ly(df,x =  x_axis, y = y_axis, color = legend_input, type = 'scatter', mode = 'markers') %>% layout(title = input$titleInput)
          }
        }
        else{
          if(input$yInput == "percentage" & (!is.null(input$transition) && !input$transition)){
            p <- plot_ly(df, x = df[[input$xInput]], y = df[[input$yInput]],color = df[[input$legendInput]],type = 'scatter', mode = 'markers') %>%
              layout(title = input$titleInput)
          }
          else{
            p <- plot_ly(df,x =  x_axis, y = y_axis, color = legend_input, frame=(as.factor(df[['date']])), type = 'scatter', mode = 'markers') %>% 
              layout(title = input$titleInput)%>% 
              layout(showlegend = FALSE)%>%  
              animation_opts(frame = 1500, transition = 1000, easing = "elastic", redraw = FALSE) %>%
              animation_slider(
                currentvalue = list(prefix = "DATE: ", font = list(color="red"))
            )
          }
        }   
      }
    }
    else{
      validate(
        need(((typeof(df[[input$yInput]])=="double" | typeof(df[[input$yInput]])=="integer")&!is.date(df[[input$yInput]]) )| input$yInput == "percentage", 'Y-axis needs to be a quantitative data')
      )
      validate(
        need(input$xInput!=input$yInput, 'Axes cannot be the same')
      )
      
      if (input$displayModel != "Scatter Plot" & input$yInput != "percentage"){
        if(input$function_selection == "count"){
          fun = "length"
        }
        else{
          fun = input$function_selection
        }
        if(input$xInput=='date'||(!is.null(input$transition) && !input$transition)){
          df_function <- aggregate(df[[input$yInput]], list(df[[input$xInput]]), FUN=fun)
        }
        else{
          df_function <- aggregate(df[[input$yInput]], list(df[[input$xInput]],df[['date']]), FUN=fun)
          names(df_function)[names(df_function) == 'Group.2'] <- 'date'
          
          dfa <- df_function %>% complete(date,Group.1, fill = list(x= 0))
          names(dfa)[names(dfa) == 'Group.1'] <- input$xInput
          names(dfa)[names(dfa) == 'x'] <- input$yInput
          x_axis_animation <- dfa[[input$xInput]]
          y_axis_animation <- dfa[[input$yInput]]
        }
  
        names(df_function)[names(df_function) == 'Group.1'] <- input$xInput
        names(df_function)[names(df_function) == 'x'] <- input$yInput
        
        x_axis <- df_function[[input$xInput]]
        y_axis <- df_function[[input$yInput]]
      }
      
      if (input$displayModel == "Line Chart"){
        if(input$xInput=='date'||(!is.null(input$transition) && !input$transition)){
          if(input$yInput == "percentage"){
            p <- plot_ly(df, x = df[[input$xInput]], y = df[[input$yInput]], type = 'scatter', mode = 'lines') %>%
              layout(title = input$titleInput)
          }
           else {
             p <- plot_ly(df_function, x = x_axis, y = y_axis, type = 'scatter', mode = 'lines') %>% 
              layout(title = input$titleInput)}
        }
        else{
          if(input$yInput == "percentage" & (!is.null(input$transition) && !input$transition)){
            p <- plot_ly(df, x = df[[input$xInput]], y = df[[input$yInput]], type = 'scatter', mode = 'lines') %>%
              layout(title = input$titleInput)
          }
          else{          
            p <- plot_ly(dfa, x = x_axis_animation, y = y_axis_animation, frame=(as.factor(dfa[['date']])), type = 'scatter', mode = 'lines') %>% 
              layout(title = input$titleInput)%>% 
              layout(showlegend = FALSE) %>%  
              animation_opts(frame = 1500, transition = 1000, easing = "elastic", redraw = FALSE) %>%
              animation_slider(
                currentvalue = list(prefix = "DATE: ", font = list(color="red"))
              )}
        }
        
      }
      
      else if(input$displayModel == "Bar Chart"){
        if(input$xInput=='date'||(!is.null(input$transition) && !input$transition)){
          if(input$yInput == "percentage"){
            p <- plot_ly(df, x = df[[input$xInput]], y = df[[input$yInput]], type = 'bar') %>%
              layout(title = input$titleInput)
          }
          else{
            p <- plot_ly(df_function, x = x_axis, y = y_axis, type = 'bar') %>% 
              layout(title = input$titleInput)
          }
        }
        else{
          if(input$yInput == "percentage" & (!is.null(input$transition) && !input$transition)){
            p <- plot_ly(df, x = df[[input$xInput]], y = df[[input$yInput]], type = 'bar') %>%
              layout(title = input$titleInput)
          }
          else{
          p <- plot_ly(dfa, x = x_axis_animation, y = y_axis_animation, frame=(as.factor(dfa[['date']])), type = 'bar') %>% 
            layout(title = input$titleInput)%>% 
            layout(showlegend = FALSE)  %>%  
            animation_opts(frame = 1500, transition = 1000, easing = "elastic", redraw = FALSE) %>%
            animation_slider(
              currentvalue = list(prefix = "DATE: ", font = list(color="red"))
            )
          }
          
        }
      }
      
      else if(input$displayModel == "Stacked Bar Chart"){
        if(input$xInput=='date'||(!is.null(input$transition) && !input$transition)){
          if(input$yInput == "percentage" ){
            p <- plot_ly(df, x = df[[input$xInput]], y = df[[input$yInput]], type = 'bar') %>% 
              layout(title = input$titleInput , barmode = 'stack')
          }
          else{
            p <- plot_ly(df_function, x = x_axis, y = y_axis, type = 'bar') %>% 
              layout(title = input$titleInput , barmode = 'stack') 
          }
        }
        else{
          if(input$yInput == "percentage" & !input$transition){
            p <- plot_ly(df, x = df[[input$xInput]], y = df[[input$yInput]], type = 'bar') %>%
              layout(title = input$titleInput, barmode = 'stack')
          }
          else{
            p <- plot_ly(dfa, x = x_axis_animation, y = y_axis_animation, frame=(as.factor(dfa[['date']])), type = 'bar') %>% 
              layout(title = input$titleInput , barmode = 'stack')%>% 
              layout(showlegend = FALSE) %>%  
              animation_opts(frame = 1500, transition = 1000, easing = "elastic", redraw = FALSE) %>%
              animation_slider(
                currentvalue = list(prefix = "DATE: ", font = list(color="red"))
              )
          }
        }
      }
      else{
        x_axis <- df[[input$xInput]]
        y_axis <- df[[input$yInput]]
        if(input$xInput=='date'|| (!is.null(input$transition) && !input$transition)){
          if(input$yInput == "percentage" ){
            p <- plot_ly(df, x = df[[input$xInput]], y = df[[input$yInput]],  type = 'scatter', mode = 'markers') %>% 
              layout(title = input$titleInput)
          }
          else{
            p <- plot_ly(df,x =  x_axis, y = y_axis, type = 'scatter', mode = 'markers') %>% layout(title = input$titleInput)
          }
        }
        else{
          if(input$yInput == "percentage" & (!is.null(input$transition) && !input$transition)){
            p <- plot_ly(df, x = df[[input$xInput]], y = df[[input$yInput]], type = 'bar') %>%
              layout(title = input$titleInput, barmode = 'stack')
          }
          else{          
            p <- plot_ly(df,x =  x_axis, y = y_axis, frame=(as.factor(df[['date']])), type = 'scatter', mode = 'markers') %>% 
              layout(showlegend = FALSE) %>%
              layout(title = input$titleInput)%>%  
              animation_opts(frame = 1500, transition = 1000, easing = "elastic", redraw = FALSE) %>%
              animation_slider(
                currentvalue = list(prefix = "DATE: ", font = list(color="red"))
              )}
        }
      }
    
    p
    }
  })
  

  # Show notification and also increment counter value when the "Add to dashboard button" is pressed
  observeEvent(input$buttonAdd,{
    df <- filter_data()
    if((((typeof(df[[input$yInput]])=="double" | typeof(df[[input$yInput]])=="integer")&!is.date( df[[input$yInput]]))&input$xInput!=input$yInput)){
      counter$countervalue <- counter$countervalue + 1
      showNotification("Added to Dashboard")
    }
    
    
  },priority = 1)

  # To add plot generated by users to dashboard
  addPlot <- eventReactive(input$buttonAdd,{  

    plotname <- paste("plot", counter$countervalue, sep="")

    input$buttonAdd
    
    p <- savePlot()
      # Generating the plots in the dashboard
      output[[paste("plot", counter$countervalue, sep="")]] <- renderPlotly({
        ggplotly(p)
      })
    
  })
  
  # To be able to download and save the whole dashboard
  observeEvent(input$download, {
    screenshot(id="allPlots")
  })
  

  }# server


### CREATE SHINY OBJECT ###
shinyApp(ui = ui, server = server)

