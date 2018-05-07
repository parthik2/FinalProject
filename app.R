#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

setwd("/Users/AjaySuresh/NYC_Public_Schools/")

dataset = read.csv('SCHOOL_FULL_DATA.csv')

library(shiny)
library(ggplot2)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("NYC Public School Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h1("Scores", align = "center"),
      h2("vs", align = "center"),
      
      #Dropdown menu
      selectInput(inputId = "ds",
                  label = "Choose a dataset",
                  choices = c("Poverty - Distribution", "Ethnicity Distribution",
                              "Ethnicity - African American",
                              "Ethnicity - Hispanic",
                              "Ethnicity - Asian",
                              "Ethnicity - White"))
      
      #Load data
      #submitButton("Load Data")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("lm")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  linReg = reactive({
    if(input$ds == "Ethnicity - African American"){
     linearMod = lm(MAT_mean_score ~ Black, data = dataset)
     summary(linearMod)
    }
    else if(input$ds=="Ethnicity - Asian")
    {
      linearMod = lm(MAT_mean_score ~ Asian, data = dataset)
      summary(linearMod)
    }
    else if(input$ds == "Ethnicity - White")
    {
      linearMod = lm(MAT_mean_score ~ White, data = dataset)
      summary(linearMod)
    }
    else if(input$ds == "Ethnicity - Hispanic")
    {
      linearMod = lm(MAT_mean_score ~ Hispanic, data = dataset)
      summary(linearMod)
    }
  })
  
  p = reactive({
     if(input$ds == "Poverty - Distribution"){
      p = sum(dat$Poverty)
      np = sum(1-dat$Poverty)
      dat$num_pov = dat$Poverty * dat$Total.Enrollment
      dat$num_not_pov = (1-dat$Poverty) * dat$Total.Enrollment
      n_p = 0 
      for (num in dat$num_pov) { 
        if (!is.na(num)) {
          n_p = n_p + num
        }
      }
      n_np = 0
      for (num in dat$num_not_pov) { 
        if (!is.na(num)) {
          n_np = n_np + num
        }
      }
      Percents = c(p, (n-p)) / n
      Numbers = c(n_p, n_np)
      labels = c("Poverty", "Not Poverty")
      df = data.frame(Percents, Numbers, labels)
      
      require("gridExtra")
      
      plt2 = ggplot(df, aes(x=labels, y=Numbers, fill=labels)) + geom_bar(stat='identity') + 
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position="none") +
        labs(title="Student Poverty -- Distribution")
      
      
      plt3 = ggplot(dat, aes(MAT_mean_score, fill=(Poverty<.5))) + 
        geom_histogram() +
        labs(title="School Mean Test Score -- Poverty")
      plt3
      grid.arrange(plt2, plt3, ncol=2)
      
    }
    else if(input$ds == "Ethnicity Distribution")
    {
      w = sum(dat$White)
      b = sum(dat$Black)
      a = sum(dat$Asian)
      h = sum(dat$Hispanic)
      n = 0
      for (num in dat$Total.Enrollment) { 
        if (!is.na(num)) {
          n = n + num
        }
      }
      t = w + b + a + h
      Percents = c(b,h,w,a) / t
      Numbers = Percents * n
      labels = c("Black", "Hispanic", "White", "Asian")
      df = data.frame(Percents, Numbers, labels)
      
      plt1 = ggplot(df, aes(x=1, y=Percents, fill=labels)) + geom_bar(stat='identity', width=0.01) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank()) +
        labs(title="Student Ethnicity -- Cumulative")
      plt2 = ggplot(df, aes(x=labels, y=Numbers, fill=labels)) + geom_bar(stat='identity') + 
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank()) +
        labs(title="Student Ethnicity -- Distribution")
      grid.arrange(plt1, plt2, ncol=2)
    }
    else if(input$ds == "Ethnicity - Hispanic"){
       ggplot(dat, aes(x=Hispanic, y=MAT_mean_score, color=("red"), fill=FALSE, alpha=.25)) +
        geom_point() +
        labs(title="Test Score -- Ethnicity") +
        theme(legend.position="none")
      
      ggplot(dat, aes(x=Hispanic, y=MAT_mean_score, color=(Poverty<.5), fill=FALSE, alpha=.25)) +
        geom_point() +
        labs(title="Test Score -- Ethnicity")
    }
    else if(input$ds == "Ethnicity - Asian"){
       ggplot(dat, aes(x=Asian, y=MAT_mean_score, color=("red"), fill=FALSE, alpha=.25)) +
        geom_point() +
        labs(title="Test Score -- Ethnicity") +
        theme(legend.position="none")
      
      
      ggplot(dat, aes(x=Asian, y=MAT_mean_score, color=(Poverty<.5), fill=FALSE, alpha=.25)) +
        geom_point() +
        labs(title="Test Score -- Ethnicity")
      
    }
    else if(input$ds == "Ethnicity - White"){
       ggplot(dat, aes(x=White, y=MAT_mean_score, color=("red"), fill=FALSE, alpha=.25)) +
        geom_point() +
        labs(title="Test Score -- Poverty") +
        theme(legend.position="none")
      
        ggplot(dat, aes(x=White, y=MAT_mean_score, color=(Poverty<.5), fill=FALSE, alpha=.25)) +
        geom_point() +
        labs(title="Test Score -- Ethnicity")
      
    }
    else if(input$ds == "Ethnicity - African American"){
       ggplot(dat, aes(x=Black, y=MAT_mean_score, color=("red"), fill=FALSE, alpha=.25)) +
        geom_point() +
        labs(title="Test Score -- Ethnicity") +
        theme(legend.position="none")
      
      ggplot(dat, aes(x=Black, y=MAT_mean_score, color=(Poverty<.5), fill=FALSE, alpha=.25)) +
        geom_point() +
        labs(title="Test Score -- Ethnicity")
    }
  })
  
  output$plot = renderPlot({plot(p(), data = dataset)})
  output$lm = renderPrint({linReg()})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

