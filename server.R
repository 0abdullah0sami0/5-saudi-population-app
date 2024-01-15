shinyServer(function(input,output,session){
  
  plot <- reactive({
    
    # Plot <- Population %>% 
    #   mutate(year = as.Date(paste0("01-01-",year),format = "%d-%m-%Y")) %>%
    #   group_by(year,region) %>%
    #   summarise(Population = sum(population)) %>%
    #   
    #   ggplot(aes(x = year, y = Population , color = region)) +
    #   geom_line() +
    #   geom_point(aes(text = format(year,"%Y"))) +
    #   xlab("Year") +
    #   ylab("Population (in Millions)") +
    #   scale_color_manual(values = c("#cad2c5","#84a98c","#52796f","#03045e","#9b2226","#bb3e03","#0077b6","#354f52","#ca6702","#2f3e46","#ee9b00","#00b4d8","#2b2d42")) +
    #   theme_classic() +
    #   scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
    #   scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
    #   theme(plot.margin = margin(2,0.5,0.5,0.5,"cm"),plot.title = element_text(size = 12), legend.position = "none")
    
    Plot <- PopulationinbroadAgeGroups %>% 
      group_by(broad_age_groups, age_groups) %>% 
      summarise(Population = sum(population)) %>%
      mutate(`Age Groups` = age_groups) %>%
      
      ggplot(aes(  x = `Age Groups`, y = Population, fill = broad_age_groups))+
      geom_bar(position = "dodge", stat='identity')+
      coord_flip()+
      theme_classic() +
      scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
      theme(plot.margin = margin(2,0.5,0.5,0.5,"cm"),plot.title = element_text(size = 12)) +
      xlab("Age Groups") +
      ylab("Population (in Millions)") +
      scale_fill_discrete(name = "Broad Age Groups")
    
    ggplotly(Plot,tooltip = c("x", "y")) %>%
      layout(title = list(text = paste0("Saudi Arabia's Population by Age Groups",
                                        '<br>',
                                        '<sup>',
                                        'Source: portal.saudicensus.sa','</sup>')))
  })
  
  # Arabic page
  output$plot1 <- renderPlotly({
    plot()
  })
  
  # English page
  output$plot2 <- renderPlotly({
    plot()
  })
  
})
  