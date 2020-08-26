library(shiny)
library(ggplot2)


fields <- c("name","category","count")

ui <- fluidPage(
  
  # Application title
  titlePanel("Input a single categorical variable"),
  
  # Sidebar with reactive inputs
  sidebarLayout(
    sidebarPanel(
      textInput("name",label = "Variable Name"),
      textInput("category",label ="Category"),
      numericInput("count",label ="Frequency", value=1, min=1,max=NA),
      actionButton("save","Add"),
      actionButton(inputId = "reset", label = "Clear all data")
    ),
    
    # a table of reactive outputs
    mainPanel(
      mainPanel(
        
        DT::dataTableOutput("responses", width = 500), plotOutput("barplot"), plotOutput("piechart"), tags$hr()
      )
      
    )
    
    
  )

  
)


# Define server logic 
server <- function(input, output,session) {
  
  #create a data frame called responses
  saveData <- function(data) {
    data <- as.data.frame(t(data))
    if (exists("responses")) {
      responses <<- rbind(responses, data)
     
    } else {
      responses <<- data

    }
  }
  
  loadData <- function() {
    if (exists("responses")) {
     
      responses
    }
  }
  
  #create a data frame called responses
  deletData <- function() {
    
    if (exists("responses")) {
      responses <<- NULL
    } else {
      responses <<- data
    }
  }
  
  
  # Whenever a field is filled, aggregate all form data
  #formData is a reactive function
  formData <- reactive({
    data <- sapply(fields[2:3], function(x) input[[x]])
   
  })
  
  # When the Save button is clicked, save the form data
  observeEvent(input$save, {
    saveData(formData())
  })
  
  # When the clear button is clicked, clear the form data
  observeEvent(input$reset, {
    deletData()
  })
  
  
  # Show the previous responses
  # (update with current response when save is clicked)
  output$responses <- DT::renderDataTable({
    input$save
    loadData()
  })     
  

  
  # generate bar chart
  output$barplot <- renderPlot({

    plotdata<-data.frame({
      input$save
      loadData()
    })
    
    plotdata$count<-c(levels(plotdata$count))
  
    
    if (is.null(plotdata$category)==F & is.null(plotdata$count)==F){
    ggplot(data=plotdata, aes(x=category, y=as.numeric(count))) +
      geom_bar(stat="identity", fill="steelblue")+
      theme_minimal()+labs(title=paste("Bar Plot of", input[[fields[1]]], sep=" "),
                           x ="Category", y = "Frequency counts")}
  })
  
  output$piechart <- renderPlot({

    plotdata2<-data.frame({
      input$save
      loadData()
    })

    plotdata2$count<-c(levels(plotdata2$count))

    if (is.null(plotdata2$category)==F & is.null(plotdata2$count)==F){

      ggplot(data=plotdata2, aes(x=0, y=as.numeric(count), fill=category))+
        geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)+
        scale_fill_brewer(palette="Blues")+
        theme(axis.text.x=element_blank(), axis.text.y=element_blank(),panel.background = element_blank())+
        labs(title=paste("Pie Chart of", input[[fields[1]]], sep=" "), x ="Category", y = "Frequency counts")

    }
    
    
  })
  

  
}

shinyApp(ui, server)
