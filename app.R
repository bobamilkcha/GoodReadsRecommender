##VISUALIZATION DASHBOARD & UI
library(shiny)
library(shinydashboard)
library(plotly)
source("recs_eval_functions.R")

#Ui function
ui <- dashboardPage(
  dashboardHeader(title = "Book Recommendation Dashboard", titleWidth = 250),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Recommendations", tabName = "recommendations", icon = icon("book")),
      menuItem("Visualizations", tabName = "visualizations", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    # Apply brownish champagne theme styles
    tags$head(tags$style(HTML('
      /* Header */
      .main-header {background-color: #DDCBAD !important;}
      
      /* Sidebar */
      .main-sidebar {background-color: #DDCBAD !important; color: white !important;}
      .sidebar-menu > li > a {color: white !important; background-color: #DDCBAD !important;}
      .sidebar-menu > li > a:hover {color: white !important; background-color: #A69579 !important;}
      
      /* Content */
      .content-wrapper {background-color: #F4E0D3 !important;}
      
      /* Box Styling */
      .box {border-top: 3px solid #D1B29B !important;}
      
      /* Table */
      table {background-color: white !important; border-radius: 10px !important;}
      th {background-color: #C4A381 !important; color: white !important;}
      td {color: #4A3F35 !important;}

      /* Button Styling */
      .btn-custom {background-color: #D2B48C !important; color: white !important; border: none;}
      .btn-custom:hover {background-color: #B38B6D !important;}
    '))),
    
    tabItems(
      tabItem(tabName = "recommendations",
              fluidRow(
                box(
                  title = "Book Recommendation Inputs", status = "primary", solidHeader = TRUE,
                  width = 12,
                  textInput("book_title", "Enter Book Title:", value = "Book A"),
                  numericInput("n_recommendations", "Number of Recommendations:", value = 3, min = 1, max = 5),
                  selectInput("method", "Recommendation Method", choices = c("tfidf", "metadata")),
                  actionButton("get_recommendations", "Get Recommendations", class = "btn-custom")
                )
              ),
              fluidRow(
                box(
                  title = "Recommended Books", status = "primary", solidHeader = TRUE,
                  width = 12,
                  tableOutput("recommendations_table")
                )
              ),
              # Display pre-defined list of books
              wellPanel(
                h4("Books for Testing Purposes(Copy Paste into Book Input)"),
                tags$ul(
                  tags$li("Learning and Teaching with Maps"),
                  tags$li("Reforming Medicine"),
                  tags$li("The Digital Effect"),
                  tags$li("Life in the Word: A Journal"),
                  tags$li("Nobody's Boy"),
                  tags$li("Gus and Grandpa"),
                  tags$li("Hummingbird"),
                  tags$li("The Boy in the Burning House"),
                  tags$li("Off to the Side: A Memoir"),
                  tags$li("Resurrection")
                )
              ),
      ),
      tabItem(tabName = "visualizations",
              fluidRow(
                box(
                  title = "Recommendations Visualization", status = "primary", solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("recommendation_chart")
                )
              )
      )
    )
  )
)

# Server function
server <- function(input, output, session) {
  
  # Reactive function to store recommendations
  recommended_books <- reactive({
    req(input$get_recommendations)  # Ensure button is clicked
    
    book_title <- input$book_title
    n_recommendations <- input$n_recommendations
    method <- input$method
    
    Sys.sleep(2)
    # Call appropriate recommendation function
    if (method == "tfidf") {
      recommend_books_tf(book_title, n_recommendations)
    } else if (method == "metadata") {
      recommend_books_metadata(book_title, n_recommendations)
    } else {
      return(NULL)  # Return NULL if no valid method
    }
  })
  
  # Render recommendations table
  output$recommendations_table <- renderTable({
    books_info <- data.frame(Name = recommended_books())  # Ensure it's a data frame
    if (nrow(books_info) == 0) {
      return(data.frame(Message = "No recommendations found!"))
    }
    books_info
  })
  
  # Render recommendation visualization
  output$recommendation_chart <- renderPlotly({
    req(recommended_books())  # Ensure recommendations exist
    
    book_counts <- as.data.frame(table(recommended_books()))
    colnames(book_counts) <- c("Book", "Count")
    
    if (nrow(book_counts) == 0) return(NULL)
    
    plot_ly(book_counts, x = ~Book, y = ~Count, type = "bar",
            marker = list(color = 'rgb(214, 184, 124)', line = list(color = 'rgb(157, 135, 88)', width = 1.5))) %>%
      layout(title = "Recommendation Frequency",
             xaxis = list(title = "Books"),
             yaxis = list(title = "Frequency"))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

#CONNECTING TO SHINYAPP.IO
library(rsconnect)
rsconnect::setAccountInfo(name='bobachan', token='8F1BA7DBFD943D23A427083B1B899612', secret='DyQ/pbwCAEnk1E9BpZUkChhku3MtpfCBpafvn1zP')
rsconnect::deployApp("C:/Users/syasy/OneDrive/Documents/Data Science/GoodReadsRating")

