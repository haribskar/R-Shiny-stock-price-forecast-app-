library(shiny)
library(tseries)
library(forecast)
library(magrittr)
library(timeDate)
library(dplyr)
library(zoo)


# Source helpers ----
#source("timeSeries.R")

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
	titlePanel("S&P500 Forecasting"),

	  # Sidebar layout with input and output definitions ----
		sidebarLayout(

		  # Sidebar panel for inputs ----
			sidebarPanel(

			  # Input: Select a file ----
				fileInput("upload_file", "Choose CSV File",
							multiple = FALSE,
							accept = c("text/csv",
							"text/comma-separated-values,text/plain",
							".csv")),

			  # Horizontal line ----
				tags$hr(),
				
				# Input: column value ----
				selectInput("opt", "Select a column:",choices = c('select','open','high','low','close','volume'),selected = 'open'),
				
				tags$hr(),

			  # Input: Select a graph ----
				selectInput("graphs", "Select a graph:",choices = c('Select','TimeSeries','DecomposedSeries','DifferencedSeries'),selected = 'Select'),
		
			  # Horizontal line ----
				tags$hr(),

			  # Input: Select a test ----
				selectInput("testobj", "Select ADF test for Stationarity:",choices = c('Select','Actual','Differenced'),selected = 'Select'),

			  # Horizontal line ----
				tags$hr(),
				

			  # Input: Enter Lag value ----
				numericInput("pacflags", "Lags", value="0"),
	  
			  # Horizontal line ----
				tags$hr(),
				
				# Input: ACF/PACF ----
				selectInput("acfpobj", "Select a correlation function:",choices = c('Select','PACF-Actual','PACF-Differenced','ACF-Actual','ACF-Differenced'),selected = 'Select'),
				
				# Horizontal line ----
				tags$hr(),
	  

			  # Input: Enter P,D,Q value ----
				numericInput("pval", "P", value=""),
				numericInput("dval", "D", value=""),
				numericInput("qval", "Q", value=""),
	  
			  # Horizontal line ----
				tags$hr(),
	  
				# Input: ARIMA ----
				selectInput("arimaobj", "Select a model:",choices = c('Select','ARIMA'),selected = 'Select'),
				
      
			  # Horizontal line ----
				tags$hr(),
	  
			  # Input: Enter number of days value ----
				numericInput("fndays", "Number of days to forecast:", value=0),
	  
			  # Horizontal line ----
				tags$hr(),
	  
				# Input: Forecast ----
				selectInput("fcobj", "Select to Forecast:",choices = c('Select','Forecast-Plot','Forecast-Values','Forecast-Residuals'), selected = 'Select')

			),

		  # Main panel for displaying outputs ----
			mainPanel(

			  # Output: Data file ----
			  tableOutput("head"),
				plotOutput("graphs"),
				verbatimTextOutput('adf'),
				plotOutput("acfpacf"),
				verbatimTextOutput('arimaop'),
				plotOutput("fplot"),
				tableOutput('fout'),
				plotOutput("fres")
				# plotOutput("tests"),
				# plotOutput("nDays")

			)

		)

)

# Define server logic to read selected file ----
server <- function(input, output) {
	
	# output$msg <- renderText(input$getContents,{
		
			# "Uploaded successfully"
		
	# })
	
  output$head <- renderTable({
	  # input$upload_file will be NULL initially. After the user selects and uploads a file

		req(input$upload_file)
	  dt <<- read.csv(input$upload_file$datapath)
		return(head(dt))
	})
	
	output$graphs <- renderPlot({
	  dt[is.na(dt)] <- 0
	  dt$date <- as.Date(dt$date, format = "%Y-%m-%d")
	  wd <<- getWorkingdates()
	  cv <- "open"
	  cv <- toString(getColOpt(input$opt))
	  its <<- create_ts(which(colnames(dt) == cv),wd)
	  vec <<- dt[,which(colnames(dt) == cv)]
	  itsDiff_1 <<- diff(its, differences=1)
	  
	  if(input$graphs == 'TimeSeries'){
	    plot.ts(its, main = "Time-Series plot", col = "blue")
	  }
	  else if(input$graphs == 'DecomposedSeries'){
	    ty <- checkAddMul(its)
	    itscomp<- decompose(its,ty)
	    plot(itscomp)
	  }
	  else if(input$graphs == 'DifferencedSeries'){
	    plot.ts(itsDiff_1,col = "blue",main = "Differenced Time Series")
	  }
	})
	
	output$adf <- renderText({ 
	  if(input$testobj == 'Actual'){
	    ##  Augmented Dickey-Fuller Test for Stationarity
	    a <- adftest(0)
	    paste(a$method," for actual time series"," P-Value:",a$p.value)
	    }
	  else if(input$testobj == 'Differenced'){
	    a <- adftest(1)
	    paste(a$method," for differenced time series"," P-Value:",a$p.value)
	  }
	  })
	
	output$acfpacf <- renderPlot({
	  if(input$acfpobj == 'PACF-Actual'){
	    pacf(vec, lag.max=input$pacflags,main = "PACF of Time Series")
	  }
	  else if(input$acfpobj == 'PACF-Differenced'){
	    pacf(itsDiff_1, lag.max=input$pacflags,main = "PACF of Differenced Series")
	  }
	  else if(input$acfpobj == 'ACF-Actual'){
	    acf(vec, lag.max=input$pacflags,main = "ACF of Time Series")
	  }
	  else if(input$acfpobj == 'ACF-Differenced'){
	    acf(itsDiff_1, lag.max=input$pacflags,main = "ACF of Differenced Series")
	  }
	})
	
	output$arimaop <- renderText({ 
	  if(input$arimaobj == 'ARIMA'){
	    ##  Augmented Dickey-Fuller Test for Stationarity
	    amv <<- arimafun()
	    paste("AIC:",amv$aic," AICc:",amv$aicc," BIC:",amv$bic)
	  }
	})
	
	output$fplot <- renderPlot({
	  if(input$fcobj != 'Select'){
	    fc <<- forecastfun()
	    plot(fc, col = "darkgreen")
	  }
	})
	
	output$fout <- renderTable({ 
	  if(input$fcobj == 'Forecast-Values'){
	    yy <- fc$mean
	    cd <- data.frame(ForecastedPrice = c(yy))
	    return(cd)
	  }
	})
	
	output$fres <- renderPlot({
	  if(input$fcobj == 'Forecast-Residuals'){
	    hist(fc$residuals,main = "Forecast Residuals",col='lightblue')
	  }
	})
	
	output$value <- renderText({ input$lags })
	output$value <- renderText({ input$pval })
	output$value <- renderText({ input$qval })
	output$value <- renderText({ input$dval })
	output$value <- renderText({ input$fndays })
	
	
	## other functions
	getWorkingdates <- function(){
	  dates <- seq(as.Date("2013-02-08"),as.Date("2018-02-07"),by = "day")
	  week_days <- dates[!is.weekend(dates)]
	  working_days <- week_days[!getUSHolidays(week_days)]
	  return(working_days)
	}
	
	is.weekend <- function(x) ((as.numeric(x)-2) %% 7) < 2
	
	getUSHolidays <- function(x) {
	  years = as.POSIXlt(x)$year+1900
	  years = unique(years)
	  holidays <- NULL
	  for (y in years) {
	    if (y >= 1885)
	      holidays <- c(holidays, as.character(USNewYearsDay(y)))
	    if (y >= 1885)
	      holidays <- c(holidays, as.character(USIndependenceDay(y)))
	    if (y >= 1885)
	      holidays <- c(holidays, as.character(USThanksgivingDay(y)))
	    if (y >= 1885)
	      holidays <- c(holidays, as.character(USChristmasDay(y)))
	  }
	  holidays = as.Date(holidays,format="%Y-%m-%d")
	  ans = x %in% holidays
	  return(ans)
	}
	
	create_ts <- function(col_idx,wd){
	  ## Create a time series object
	  i_ts <- as.numeric(dt[,col_idx]) %>%
	    tsclean(replace.missing = TRUE, lambda = NULL) %>%
	    ts(start = c(2013, as.numeric(format(wd[1], "%j"))),
	       frequency = 257)
	  return(i_ts)
	}
	
	adftest <- function(t){
	  if(t==0){
	    adfa <- adf.test(vec, alternative = "stationary", k = trunc((length(actualSeries)-1)^(1/3)))
	    return(adfa)
	  }
	  else if(t==1)
	  {
	    adfd <- adf.test(itsDiff_1, alternative = "stationary", k = trunc((length(differncedSeries)-1)^(1/3)))
	    return(adfd)
	  }
	
	  }
	
	arimafun <- function(){
	  M_arima <- Arima(vec, order=c(input$pval,input$dval,input$qval),seasonal = list(order = c(input$pval,input$dval,input$qval)))
	  return(M_arima)
	}
	
	forecastfun <- function(){
	  Mforecasts <- forecast(amv, h = input$fndays)
	  return(Mforecasts)
	}
	
	getColOpt <- function(cn){
	  if(cn == 'open'){
	    return("open")
	  }
	  else if(cn == 'high'){
	    return("high")
	  }
	  else if(cn == 'low'){
	    return("low")
	  }
	  else if(cn == 'close'){
	    return("close")
	  }
	  else if(cn == 'volume'){
	    return("volume")
	  }
	  
	}
	
	checkAddMul <- function(x){
	  da <- decompose(x,"additive")
	  dm <- decompose(x,"multiplicative")
	  ts_type = compare_ssacf(da$random, dm$random)
	  return(ts_type)
	}
	
	#Ref: https://www.r-bloggers.com/is-my-time-series-additive-or-multiplicative/
	ssacf<- function(x) { sum(acf(x, na.action = na.omit)$acf^2)}
	
	compare_ssacf<-function(add,mult) {ifelse(ssacf(add)< ssacf(mult), "additive", "multiplicative")} 
	
}

# Create Shiny app ----
shinyApp(ui, server)
