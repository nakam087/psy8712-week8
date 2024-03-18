library(shiny)
library(tidyverse)
library(rsconnect)

ui <- fluidPage(

    # Application title
    titlePanel("shiny_week8"),

    # Sidebar with select inputs 
    sidebarLayout(
        sidebarPanel(
          #input to select gender
            selectInput("gender",
                        "Please select the gender of the participants",
                       choices = c("Male","Female","All"),
                       selected="All"),
            #input to include error bars
            selectInput("error",
                        "Would you like error bands displayed?",
                        choices = c("Yeah", "Nah"),
                        selected = "Yeah"),
            #input to filter participants by date
            selectInput("date",
                        "Do you want to include or exclude participants that completed the assessment before July 1, 2017?",
                        choices=c("Include, please!", "Exclude, please!"), selected="Include, please!")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot")
        )
    )
)

server <- function(input, output) {

    output$plot <- renderPlot({
      #loading in "skinny" data
       markdown_data<- readRDS("./data.rds")
       #filters data if Male or Female
       #if All, original data kept
       if (input$gender!="All"){
         markdown_data<-markdown_data<- markdown_data%>%
           filter(gender==input$gender)
       }
       #filters data if excluding
       #if include, original data kept
       if (input$date=="Exclude, please!"){
         markdown_data<-markdown_data<-markdown_data%>%
           filter(timeEnd<ymd("2017-07-01"))
       }
       #if yes to error bars, runs se=T
       if(input$error == "Yeah"){
         markdown_data%>%
           ggplot(aes(q1q6_mean,q8q10_mean))+
           geom_point()+
           geom_smooth(method="lm", color="purple")+
           labs(x="Q1 - Q6 means", y="Q8 - Q10 means")
      #if no to error bars, runs se=F
       }else{
         markdown_data%>%
           ggplot(aes(q1q6_mean,q8q10_mean))+
           geom_point()+
           geom_smooth(method="lm", color="purple",se=F)+
           labs(x="Q1 - Q6 means", y="Q8 - Q10 means")
       }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
