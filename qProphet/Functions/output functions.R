## Function to format output into one list with various formats
formatOutput = function(input,history,outDF){
  output = list()
  output[['input']]         = input
  output[['history']]       = history
  output[['results']]       = outDF
  output[['results_wide']]  = output[['results']] %>% unique() %>% group_by(name,ds,statistic) %>% summarise(ownership = mean(ownership)) %>% ungroup() %>% spread(statistic,ownership)
  output[['results_full']]  = output[['results_wide']]  %>% left_join(., bind_rows(output[['history']] %>% select(-ownership),input %>% select(-y)))
  return(output)
}

## Function to save results in various formats
saveResults = function(output,date_to_forecast,tourname){
  
  output_name_prefix = if(backtest==TRUE){"Backtest"}else{"Forecast"}
  
  fileName     = paste(format(date_to_forecast,'%Y%m%d'),format(Sys.time(),'%Y%m%d%HH%MM%SS')) %>% as.character()
  dir.create(file.path(getwd(),paste0('Output/',output_name_prefix,'/',fileName)), showWarnings = FALSE)
  folderName   = paste0(getwd(),'/Output/',output_name_prefix,'/',fileName,'/')
  
  input_for_forecast = output$input
  df_wide      = output$results_wide
  df_wide_full = output$results_full
  
  # Plot PNG
  plot = ggplot(df_wide)+
    ggtitle("Projected Ownership")+
    geom_ribbon(aes(ymin=lower, ymax=upper, x=ds), fill = "blue", alpha = 0.3)+
    geom_line(aes(x=ds, y=median), color='blue')+
    geom_point(aes(x=ds, y=actual), size = 0.5)+
    scale_y_continuous(limits=c(0,40))+
    facet_wrap(.~name,ncol=10)+
    theme_light()+
    theme(axis.title=element_blank())
  ggsave(plot = plot, filename=paste(folderName,'PNG',fileName,'.png'), height = 14, width = 12, unit = 'in')
  
  # Interactive Plot HTML
  plotlyPlot = ggplotly(plot, height = 1400, width = 1800)
  htmlwidgets::saveWidget(as_widget(plotlyPlot), paste(folderName,'HTMLPLOT',fileName,'.html'))

# Interactive Table HTML
forecastTourOnly = df_wide %>% filter(ds==date_to_forecast) %>%
  select(name,ds,actual,median,lower,upper) %>%
  left_join(.,input_for_forecast) %>%
  mutate_at(vars(actual,lower,median,upper),round,digits=2) %>%
  select(Player=name,Date=ds,Salary=salary,Actual=actual,Median=median,Lower=lower,Upper=upper) %>% unique() 

write_tableHTML(
  tableHTML(forecastTourOnly %>%  mutate_at(vars(Actual,Median,Lower,Upper),round,digits=2) %>% arrange(desc(Salary),desc(Median)), caption = tourname) %>%
    tableHTML::add_theme("scientific"), file = paste(folderName,'HTMLTABLE',fileName,'.html'))

}


