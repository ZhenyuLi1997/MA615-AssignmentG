# Load the packages 
library(dplyr)
library(shiny)
library(ggplot2)


# Questions the app addresses
# 1 List of prizes
# 2 Years of the selected prize
# 3 Prize institution composition
# 4 Genre composition
# 5 Winner demographic composition
# 6 Winner academic background composition

# Possible improvements: just two tabs
# 1 Prizes: apply filter based on choice
# 2 Authors: same as Prize

##########################################################################
ui <- fluidPage(
  tabsetPanel(
    # Tab 1: Prizes
    tabPanel("Prizes",
             h4("Prizes"),
             h5("A list of all prizes"),
             tableOutput("glimpsetable")),
    
    # Tab 2: Prize Years
    tabPanel("Prize Years",
             selectInput("Year", label = "Which prize?", choices = "N/A"),
             tableOutput("yearlist")),
    
    # Tab 3: Prize Institutions
    tabPanel("Prize Institutions",
             h3("Prize Institutions"),
             h4("Top 1 Institutions"),
             h5("Based on how many prizes they administer"),
             tableOutput("topinst"),
             plotOutput("instplot")),
    
    # Tab 4: Genre
    tabPanel("Genre",
             h3("Genre"),
             h4("Top 3 Genres"),
             h5("Based on how many prizes are for the genre"),
             tableOutput("topgenre"),
             plotOutput("genreplot")), 
    
    # Tab 5: Author Gender
    tabPanel("Author Gender",
             h3("Author Gender"),
             selectInput("winner", label = "Winner only?", choices = c("Yes", "No")),
             tableOutput("gendertable"),
             plotOutput("genderplot")), 
    
    # Tab 6: Author Ethnicity
    tabPanel("Author Ethnicity",
             h3("Author Ethnicity"),
             selectInput("winner", label = "Winner only?", choices = c("Yes", "No")),
             tableOutput("ethntable"),
             plotOutput("ethnplot")),
    
    # Tab 7: Author Degree
    tabPanel("Author Degree",
             h3("Author Ethnicity"),
             selectInput("winner", label = "Winner only?", choices = c("Yes", "No")),
             tableOutput("degreetable"),
             plotOutput("degreeplot")),
    
    # Tab 8: Author Academic Field
    tabPanel("Author Academic Field",
             h3("Author Academic Field"),
             selectInput("winner", label = "Winner only?", choices = c("Yes", "No")),
             tableOutput("topfield"),
             plotOutput("fieldplot")),
    )
)




##########################################################################

server <- function(input, output, session){
  
  # Load the dataset from the csv file 
  dataset <- reactive({
    read.csv("prizes.csv")
  })
  # Update the selection choice for tab " Years of the Prize"
  observe({
    updateSelectInput(session, "Year", choices = unique(dataset()$Prize.Name))
  })

  # Tab 1: Prizes
  output$glimpsetable <- renderTable({
    prize.list <- dataset() %>%
      select(Prize.Name, Inst, Genre) %>%
      rename("Name" = "Prize.Name",
             "Institution" = Inst) %>%
      distinct() %>%
      arrange(`Name`)
    data.frame(prize.list)
  })
  
  
  
  # Tab 2: Prize Years
  years <- reactive({
    year.list <- dataset() %>%
      filter(Prize.Name == input$Year) %>%
      select(Year) %>%
      unique 
  })
  output$yearlist <- renderTable({
    years()
  })

  
  
  # Tab 3: Prize Institution
  inst <- reactive({
    dataset() %>% select(Prize.Name, Inst) %>%
      distinct() %>%
      mutate(Inst = if_else(Inst=="Samuel Johnson Prize for Non-Fiction Limited", "Samuel Johnson Prize", Inst)) %>%
      group_by(Inst) %>%
      summarize(Count = n())
  })
  
  output$topinst <- renderTable({
    inst() %>%
      arrange(desc(Count)) %>%
      rename("Institution" = "Inst",
             "# of Prizes" = "Count") %>%
      filter(row_number() == 1)
  })
  
  output$instplot <- renderPlot({
    inst() %>%
      ggplot(aes(x=Inst, y=Count)) +
      geom_bar(stat="identity") 
  })
  
  
  
  # Tab 4: Genre 
  genre <- reactive({
    dataset() %>% select(Genre) %>%
      group_by(Genre) %>%
      summarize(Count = n()) %>%
      arrange(desc(Count))})
    
  output$topgenre <- renderTable({
    genre() %>%
      slice(1:3)
  })
  
  output$genreplot <- renderPlot({
    genre() %>%
      ggplot(aes(x=Genre, y=Count)) +
      geom_bar(stat="identity")
  })
  
  
  
  # Tab 5: Author Gender
  gender <- reactive({
    data <- dataset() %>% 
      select(gender, person_role) 
    
    if (input$winner == "Yes"){
      data <- data %>%
        filter(person_role == "winner")
    } else{
    }
    
    data %>%
      rename(Gender = gender) %>%
      group_by(Gender) %>%
      summarize(Count = n())
  })
  
  output$gendertable <- renderTable({
      gender() 
  })
  
  output$genderplot <- renderPlot({
    gender() %>%
      ggplot(aes(x=Gender, y=Count)) +
      geom_bar(stat="identity")
  })
  
  
  
  # Tab 6: Author Ethnicity
  ethn <- reactive({
    data <- dataset() %>% 
      select(ethnicity_macro, person_role) 
    
    if (input$winner == "Yes"){
      data <- data %>%
        filter(person_role == "winner")
    } else{
    }
    
    data %>%
      rename(Ethnicity = ethnicity_macro) %>%
      group_by(Ethnicity) %>%
      summarize(Count = n())
  })
  
  output$ethntable <- renderTable({
    ethn() 
  })
  
  output$ethnplot <- renderPlot({
    ethn() %>%
      ggplot(aes(x=Ethnicity, y=Count)) +
      geom_bar(stat="identity")
  })
  
  
  
  # Tab 7: Author Degree
  degree <- reactive({
    data <- dataset() %>% 
      select(highest_degree, person_role) 
    
    if (input$winner == "Yes"){
      data <- data %>%
        filter(person_role == "winner")
    } else{
    }
    
    data <- data %>%
      rename(Degree = highest_degree) %>%
      group_by(Degree) %>%
      summarize(Count = n()) %>% 
      arrange(match(Degree, c("None", "Certificate of Education", "Diploma", 
                              "Bachelors", "Masters", "MD",
                              "Juris Doctor", "Doctorate", "Postgraduate",
                              "Unknown")))
    
    data$Degree <- factor(data$Degree, levels=data$Degree)
    
    data
  })
  
  output$degreetable <- renderTable({
    degree() 
  })
  
  output$degreeplot <- renderPlot({
    degree() %>%
      ggplot(aes(x=Degree, y=Count)) +
      geom_bar(stat="identity")
  })
  
  
  
  # Tab 8: Author Academic Field
  field <- reactive({
    data <- dataset() %>% 
      select(degree_field_category, person_role) 
    
    if (input$winner == "Yes"){
      data <- data %>%
        filter(person_role == "winner")
    } else{
    }
    
    data %>%
      rename(Field = degree_field_category) %>%
      group_by(Field) %>%
      summarize(Count = n())
  })
  
  output$topfield <- renderTable({
    field() %>%
      arrange(desc(Count)) %>%
      slice(1:3)
  })
  
  output$fieldplot <- renderPlot({
    field() %>%
      ggplot(aes(x=Field, y=Count)) +
      geom_bar(stat="identity")
  })
}

shinyApp(ui, server)