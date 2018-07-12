library(shiny)
library(tseries)
library(forecast)
library(magrittr)
library(timeDate)
library(dplyr)
library(zoo)

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
				
			  # Input: Select a column ----
				selectInput("ohlcv", "Select a column:",
							choices = c('select','open','high','low','close','volume'),
							selected = 'select'),
				
			  # Horizontal line ----
				tags$hr(),

			  # Input: Select a graph ----
				selectInput("graphs", "Select a graph:",
							choices = c('Select','TimeSeries','DecomposedSeries','DifferencedSeries'),
							selected = 'Select'),
		
			  # Horizontal line ----
				tags$hr(),

			  # Input: Select a test ----
				selectInput("tests", "Select ADF test for Stationarity:",
							choices = c('Select','Actual','Differenced'),
							selected = 'Select'),

			  # Horizontal line ----
				tags$hr(),
				
			  # Input: Enter Lag value ----
				numericInput("lags", "Lags", value="0"),
	  
			  # Horizontal line ----
				tags$hr(),
				
			  # Input: ACF/PACF ----
				selectInput("acfp", "Select a correlation function:",
							choices = c('Select','PACF-Actual','PACF-Differenced','ACF-Actual',	'ACF-Differenced'),
							selected = 'Select'),
				
			  # Horizontal line ----
				tags$hr(),

			  # Input: Enter P,D,Q value ----
				numericInput("pval", "P", value=""),
				numericInput("dval", "D", value=""),
				numericInput("qval", "Q", value=""),
	  
			  # Horizontal line ----
				tags$hr(),
	  
			  # Input: ARIMA ----
				selectInput("arima", "Select a model:",
							choices = c('Select','ARIMA'),
							selected = 'Select'),
				      
			  # Horizontal line ----
				tags$hr(),
	  
			  # Input: Enter number of days value ----
				numericInput("ndays", "Number of days to forecast:", value=0),
	  
			  # Horizontal line ----
				tags$hr(),
	  
			  # Input: Forecast ----
				selectInput("forecast", "Select to Forecast:",
							choices = c('Select','Forecast-Plot','Forecast-Values'), 
							selected = 'Select')

			),

		  # Main panel for displaying outputs ----
			mainPanel(

			  # Output: Data file ----
				tableOutput("dataTable"),
				plotOutput("graphs"),
				verbatimTextOutput('adf'),
				plotOutput("acfpacf"),
				verbatimTextOutput('arimaPlot'),
				plotOutput("forecastPlot"),
				tableOutput('forecastTable')
				
			)

		)

)

# Define server logic to read selected file ----
server <- function(input, output) {
	
	getContents <- reactive({
	  # input$upload_file will be NULL initially. After the user selects and uploads a file
		req(input$upload_file)
		uf <<- read.csv(input$upload_file$datapath)
		uf[is.na(uf)] <- 0
		uf$date <- as.Date(uf$date, format = "%Y-%m-%d")
		return(uf)
	})
	
	output$dataTable <- renderTable({		
		head(getContents())
	})
	
	output$graphs <- renderPlot({
		its <<- create_ts(which(colnames(getContents()) == getColOpt(input$ohlcv)),getWorkingdates())
		actualSeries <<- getContents()[,which(colnames(getContents()) == getColOpt(input$ohlcv))]
		differncedSeries <<- diff(its, differences=1)
	  
		if(input$graphs == 'TimeSeries'){
			plot.ts(its, main = "Time-Series plot", col = "blue")
		} else if(input$graphs == 'DecomposedSeries'){
			stldecomp = stl(its, s.window="periodic")
			plot(stldecomp)
		} else if(input$graphs == 'DifferencedSeries'){
			plot.ts(differncedSeries,col = "blue")
		}
	})
	
	output$adf <- renderText({ 
	  ##  Augmented Dickey-Fuller Test for Stationarity
		if(input$tests == 'Actual'){
			a <- adftest(0)
			paste(a$method,"for actual time series"," P-Value:",a$p.value)
	    } else if(input$tests == 'Differenced'){
			a <- adftest(1)
			paste(a$method,"for differenced time series"," P-Value:",a$p.value)
		}
	})
	
	output$acfpacf <- renderPlot({
		if(input$acfp == 'PACF-Actual'){
			pacf(actualSeries, lag.max=input$lags)
		} else if(input$acfp == 'PACF-Differenced'){
			pacf(differncedSeries, lag.max=input$lags)
		} else if(input$acfp == 'ACF-Actual'){
			acf(actualSeries, lag.max=input$lags)
		} else if(input$acfp == 'ACF-Differenced'){
			pacf(differncedSeries, lag.max=input$lags)
		}
	})
	
	output$arimaPlot <- renderText({ 
		if(input$arima == 'ARIMA'){
	      ##  Augmented Dickey-Fuller Test for Stationarity
			amv <<- arimafunc()
			paste("AIC:",amv$aic," AICc:",amv$aicc," BIC:",amv$bic)
		}
	})
	
	output$forecastPlot <- renderPlot({
		if(input$forecast == 'Forecast-Plot'){
			if(input$ndays == 0){
				error()
			} else {
				fc <<- forecastfunc()
				plot(fc, col = "darkgreen")
			}
		}
	})
	
	output$forecastTable <- renderTable({
		if(input$forecast == 'Forecast-Values'){
			if(input$ndays == 0){
				error()
			} else {
				yy <- fc$mean
				cd <- data.frame(ForecastedPrice = c(yy))
				return(cd)
			}
		}
	})
	
	error <- function(){
		print("enter valid input")
	}
	
	output$value <- renderText({ input$lags })
	output$value <- renderText({ input$pval })
	output$value <- renderText({ input$qval })
	output$value <- renderText({ input$dval })
	output$value <- renderText({ input$ndays })
	
	
	## other functions
	
	getWorkingdates <- function(){
		dates <- seq(as.Date("2013-02-08"),as.Date("2018-02-07"),by = "day")
		week_days <- dates[!is.weekend(dates)]
		workingdates <- week_days[!getUSHolidays(week_days)]
		return(workingdates)
	}
	
	is.weekend <- function(x) {
		((as.numeric(x)-2) %% 7) < 2
	}
	
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
	
	getColOpt <- function(cv){
		if(cv == 'select'){
			print("Choose a column")
		} else if(cv == 'open'){
			return("open")
		} else if(cv == 'high'){
			return("high")
		} else if(cv == 'low'){
			return("low")
		} else if(cv == 'close'){
			return("close")
		} else if(cv == 'volume'){
			return("volume")
		}
	}
	
	create_ts <- function(col_idx, wd){
		wd <- getWorkingdates()
	  ## Create a time series object
		if (input$ohlcv == 'select'){
			print("select please")
		} else {
			i_ts <- as.numeric(getContents()[,col_idx]) %>%
			tsclean(replace.missing = TRUE, lambda = NULL) %>%
			ts(start = c(2013, as.numeric(format(wd[1], "%j"))),
			frequency = 257)
			return(i_ts)
		}
	}
	
	adftest <- function(a){
	  if(a==0){
	    adfa <- adf.test(actualSeries, alternative = "stationary", k = trunc((length(actualSeries)-1)^(1/3)))
	    return(adfa)
	  }
	  else if(a==1)
	  {
	    adfd <<- adf.test(differncedSeries, alternative = "stationary", k = trunc((length(differncedSeries)-1)^(1/3)))
	    return(adfd)
	  }
	  }
	
	arimafunc <- function(){
	  M_arima <- Arima(actualSeries, order=c(input$pval,input$dval,input$qval),
						seasonal = list(order = c(input$pval,input$dval,input$qval)))
	  return(M_arima)
	}
	
	forecastfunc <- function(){
			Mforecasts <- forecast(amv, h = input$ndays)
			return(Mforecasts)
	}
	  
}

# Create Shiny app ----
shinyApp(ui, server)
