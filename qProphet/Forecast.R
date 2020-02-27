setwd("/Users/quinnx/Documents/GitHub/qProphet")
source('./Functions/load libraries.R')
source('./Functions/load data.R')
source('./Functions/model functions.R')
source('./Functions/output functions.R')

## Forecasting Inputs
date_to_forecast = as.Date("2020-02-23") 
backtest = TRUE

## Read in historical data
history = getHistory()

## Input data known at start of tour
input = getInput()

## Prepare model-training data
model = getModel()

## Model Accuracy Statistics
# model.accuracy    = cross_validation(initial=104, model=model, horizon=1, units='weeks', period=1)
# model.performance = performance_metrics(model.accuracy)
# model.crossvalid  = plot_cross_validation_metric(df_cv = model.accuracy, metric='mse')

## Run forecasts (apply the trained model to each player input)
modelResults = runModel()

## Format output combining forecasts and actuals, and adding in salaries
outDF = formatFcst()

## Final output and plots / tables saved
output = formatOutput(input,history,outDF)
saveResults(output, date_to_forecast, tourname=unique(input$tour))

# install.packages("ggpubr")
# library("ggpubr")
# ggscatter(history, x = "salary", y = "ownership", 
#           add = "reg.line", conf.int = TRUE, 
#           cor.coef = TRUE, cor.method = "pearson")
# 
# ggscatter(history, x = "worldrank", y = "ownership", 
#           add = "reg.line", conf.int = TRUE, 
#           cor.coef = TRUE, cor.method = "pearson")
# 
# 
# p <- plot_ly(
#   history, x = ~salary, y = ~linestar, z = ~worldrank) %>% 
#   #color = ~tour) %>% #, colors = c('#BF382A', '#0C4B8E')
#   add_markers() 
# 
# 
# library(akima)
# h = history %>% filter(!is.na(salary) & !is.na(linestar) & !is.na(worldrank)) 
# #plot_ly 3D plot
# s = interp(x = h$salary, y = h$linestar, z = h$worldrank,duplicate = "mean") # prepare for plot_ly plot
# p <- plot_ly(x = s$x, y = s$y, z = s$z,colorscale = 'Jet')%>% # plot_ly
#   add_surface()
# 
