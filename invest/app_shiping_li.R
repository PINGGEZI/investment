library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

#UI
u <- fluidPage(
  titlePanel("Saving Investment Plan"),
  fluidRow(
    
    #First Column
    column(3,offset = 1,
      #First Column: This is initial amount.
      sidebarLayout(
        sliderInput("init","Initial Amount",
                     min = 0, max = 1000000, value = 1000,
                     step = 500, sep = ",",pre = "$"),
      #First Column: This is  Annual Contribution
        sliderInput("annual","Annual Contribution",
                     min = 0, max = 50000, value = 2000,
                     step = 500, sep = ",",pre = "$")
        )
    ),
    
    
    #Second Column 
    column(3,
      #Second Column: This is Return Rate.
      sliderInput("return","Return Rate (in %)",
                  min = 0, max = 20, value = 5,
                  step = 0.1),
      #Second Column: This is Growth Rate.
      sliderInput("growth","Growth Rate (in %)",
                  min = 0, max = 20, value = 2,
                  step = 0.1)
    ),

    
    
    #Third Column 
    column(3,
      #Third Column: This is Year.
      sliderInput("year","Year",
                  min = 0, max = 50, value = 10,
                  step = 1),
      #Third Column: Facet Choice.
      selectInput("facet","Facet?",
                  c("No","Yes"))
    )
  ),
  
    #plots
    h4("Timeline"),
    plotOutput("timeline"),
  
    h4("Balances"),
    verbatimTextOutput("balance",placeholder  = TRUE)

)





#Server
s <- function(input, output){
  
  current_vals <- reactive({
    future_value <- function(amount, rate, years){
      return(amount * (1 + rate)^years)
    }
    
    
    annuity <- function(contrib, rate, years){
      return(contrib * ((1 + rate)^years - 1)/rate)
    }
    
    
    growing_annuity <- function(contrib, rate, growth, years){
      return(contrib*((1 + rate)^years - (1  + growth)^years)/(rate - growth))
    }
    
    modalities <- data.frame(year = numeric(),no_contrib = numeric(),fixed_contrib = numeric(),growing_contrib = numeric())
    for(i in 0:input$year){
      fv <- future_value(input$init,input$return * 0.01,i)
      fav <- fv + annuity(input$annual,input$return * 0.01,i)
      faga <- fv + growing_annuity(input$annual,input$return * 0.01,input$growth * 0.01,i)
      modalities <- rbind(modalities,
                          data.frame(year = i,
                                     no_contrib = fv,
                                     fixed_contrib = fav,
                                     growing_contrib = faga))
    }
    modalities
  })
  
  fac <- reactive({input$facet == "No"})
  
  
  output$timeline <- renderPlot({
    
    if(fac()){
      ggplot(current_vals(),aes(x = year)) + 
        geom_line(aes(y = no_contrib,color = "no_contrib"))+
        geom_line(aes(y = fixed_contrib,color = "fixed_contrib"))+
        geom_line(aes(y = growing_contrib,color = "growing_contrib")) +
        geom_point(aes(y = no_contrib,color = "no_contrib"),size = 1) +
        geom_point(aes(y = fixed_contrib,color = "fixed_contrib"),size = 1)+
        geom_point(aes(y = growing_contrib,color = "growing_contrib"),size = 1) +
        ylab("Return(dollars)") +
        ggtitle("Three Modes of investing") +
        theme(plot.margin = margin(1,1,1,1, "cm"))
    }
    
    else{
      modalities <- current_vals() %>%
        gather(key = "variables", value = "balance",no_contrib:growing_contrib)

      ggplot(modalities,aes(x = year,y = balance,height = 100)) + 
        geom_line(aes(colour = variables)) +
        geom_point(aes(colour = variables),size = 1) +
        facet_wrap(.~variables) +
        geom_area(aes(fill = variables),alpha = 0.6) + 
        ggtitle("Three Modes of investing") +
        theme(plot.margin = margin(1,1,1,1, "cm"))
    }
    })
  
  
  output$balance <- renderPrint({
    current_vals()
  })
    
  
}

shinyApp(u,s) 


