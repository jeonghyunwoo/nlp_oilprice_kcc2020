#'1. tidyquant::quandl_search로 관련 지수를 가져온다
#'2. reticulate와 goog_news.py를 이용해 뉴스를 크롤링한다 
#'3. 1,2를 shinyapp으로 통합한다 
#' wti crude: oda/poilwti_usd
#' 오류로그: renderDT에는 DToutput을써야한다. datatableOutput은 결과안나옴 
# 
library(pacman)
p_load(tidyquant,tidyverse,timetk,shiny,DT,reticulate,plotly,dygraphs,conflicted)
# conflict_prefer('dataTableOutput','shiny')
conflict_prefer('layout','plotly')
conflict_prefer('filter','dplyr')
# oil price 검색 
source('d:/data/quandl_api.r')
rs = quandl_search('wti',silent=T) %>% 
  filter(premium == FALSE) %>%
  mutate_if(is.character,str_to_lower) %>% 
  transmute(code=str_c(database_code,'/',dataset_code),
            name,
            db = database_code,
            premium,
            from=oldest_available_date,
            to = newest_available_date,
            type,description) 
ui = fluidPage(
  # h2("Quandl",style='color:darkorange'),
  # ,img(src='quandl1.png',width=50,height=10))
  titlePanel(title=div(img(src='quandl1.png',height='15%',width='15%'))),
  # titlePanel(title=div(h2("Quandl",style='color:orangered;font-family:Raleway'))),
  hr(),
  tabsetPanel(
    tabPanel(p('plot',style='font-family:Raleway'),
             fluidRow(
               column(3,
                      textInput('qry',h3('quandl search',style='color:firebrick;font-family:Raleway'),
                                value = 'wti crude'),
                      selectInput('code',h3('quandl codes',style='color:steelblue;font-family:Raleway'),choices = NULL),
                      selectInput('colm',h3('column select',style='color:steelblue;font-family:Raleway'),choices= NULL)
                      ),
               column(9,
                      h3('Time Series Trend',style = 'color:forestgreen;font-family:Raleway'),
                      plotlyOutput('plot'),
                      hr(),
                      h3('code search results',style = 'color:firebrick;font-family:Raleway'),
                      # dataTableOutput('qry_rst')
                      DTOutput('qry_rst')
                      )
               )
             ),
    tabPanel(p('data',style='font-family:Raleway'),
             fluidRow(
               hr(),
               mainPanel(
                 DTOutput('plot_data',width='100%')
               )
               
             ))
    )
  )

server = function(input,output,session){
  qry_rs = reactive({
    quandl_search(input$qry,silent=T) %>% 
      filter(premium == FALSE) %>% 
      mutate_if(is.character, str_to_lower) %>% 
      transmute(code = str_c(database_code,'/',dataset_code),
                name,
                db = database_code,
                type, description)
    
  })
  output$qry_rst = renderDT(qry_rs(),options=list(pageLength=5))
  
  code_rs = reactive({
    tq_get(input$code,get='quandl')
  })
  
  colms = reactive({
    nm = names(code_rs())
    # nm[!str_detect(nm,'date')]
    nm[-1]
  })
  
  
  observe({
    updateSelectInput(session,'code',
                      choices = qry_rs()$code)
  })
  observe({
    updateSelectInput(session,'colm',
                      choices = colms())
  })
  output$plot_data = renderDT({
    code_rs()
  })

  output$plot = renderPlotly({
    code_rs() %>% 
      rename_if(is.Date,~str_replace(.,'.*','date')) %>%
      plot_ly() %>% 
      add_lines(x=~date,y=~get(input$colm)) %>% 
      layout(title = list(text=qry_rs() %>% 
                            filter(code==input$code) %>% 
                            pull(name) %>% 
                            str_to_title(),
                          weight = 'bold',
                          x = 0, color = toRGB('grey30'), size=13, tickness=100),
             yaxis = list(title = input$colm),
             xaxis = list(
               # rangeselector = list(
               #   buttons = list(
               #     list(
               #       count = 3,
               #       label = "3 mo",
               #       step = "month",
               #       stepmode = "backward"),
               #     list(
               #       count = 6,
               #       label = "6 mo",
               #       step = "month",
               #       stepmode = "backward"),
               #     list(
               #       count = 1,
               #       label = "1 yr",
               #       step = "year",
               #       stepmode = "backward"),
               #     list(
               #       count = 1,
               #       label = "YTD",
               #       step = "year",
               #       stepmode = "todate"),
               #     list(step = "all"))),
               
               rangeslider = list(type = "date")))
  })
}

shinyApp(ui,server)
