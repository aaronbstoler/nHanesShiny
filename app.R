library(shiny,shinyWidgets)
#source("nHanes/nHanesAnalysis.R")


ui <- fluidPage(
  
  
  
  titlePanel("nHanes Data Exploration Tool"),
  
  
  
  sidebarLayout(
    
    sidebarPanel(
      
      checkboxGroupInput("surveyCycle",label="Survey Cycles to Include:",choices = c("2017-2018" = "20172018"),selected="20172018"),
      
      pickerInput(
        "phthalates",
        label = "Phthalates to Include:",
        choices = c("DBP"="dbp","DiBP"="dibp","BBP"="bbp","DEHP"="dehp","DINP_chap"="dinpchap","DINP"="dinp","DIDP"="didp","DEHTP"="dehtp"),
        options = list("actions-box" = TRUE,"dropupAuto"=TRUE,title="Select at least one"),
        multiple = TRUE),
      pickerInput("toxref",
        label = "Toxicity Reference:",
        choices = c("DBP"="dbp","DiBP"="dibp","BBP"="bbp","DEHP"="dehp","DINP_chap"="dinpchap","DINP"="dinp","DIDP"="didp","DEHTP"="dehtp"),
        options = list("dropupAuto"=TRUE,title="Select one"),multiple = FALSE),   
      sliderTextInput(
        inputId = "age",
        label = "Age Range:", 
        choices = c(0, 3, 5, 11, 14, 45, 64, 100),
        selected = c(3,64),
        grid = TRUE),
      checkboxGroupInput("gender",
        label="Gender:",
        choices=c("Male"="M","Female"="F")),
      pickerInput("ethnicity",
        label="Ethnicity:",
        choices=c("Mexican"="Mexican","Hispanic"="Hispanic","Non-hispanic White"="A_NHWhite","Non-hispanic Black"="NHBlack","Other"="0"),
        options=list("actions-box"=TRUE,"dropupAuto"=TRUE,title="Select at least one"),
        multiple = TRUE),
      checkboxGroupInput("percentiles",
        label="Percentiles:",
        choices=c("25th"="25th","50th"="50th","95th"="95th")),
      actionButton("genOutput","Generate Output")
    ),
    
    
    
    mainPanel(
      plotOutput("qPlot")

    )
    
  )
  
)





server <- function(input, output,session) {
 
  genPlot <- eventReactive(input$genOutput, {
    quantiledf <- surveydesign(input$age[1],input$age[2],input$gender,input$ethnicity)
  })
  
  output$qPlot <- renderPlot({createPlot(genPlot())})
}



# Run the application

shinyApp(ui = ui, server = server)