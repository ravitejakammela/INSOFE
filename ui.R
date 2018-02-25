library(shiny)
library(shinydashboard)


shinyUI(
  dashboardPage(skin = "purple",
  dashboardHeader(title = "Preventive Maintenance in Manufacturing Industries",titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Data",tabName = "dashboard",icon = icon("dashboard")),
    menuItem("Visualizations"),
    menuSubItem("Plot1",tabName = "plt1"),
    menuSubItem("Plot2",tabName = "plt2"),
    menuSubItem("plot3",tabName = "plt3"),
    menuItem("Predictions",tabName = "Remaining")
  )),
  dashboardBody(
    fluidRow(
      box(
        width = 5,
             radioButtons(
               "fileType_Input",
               label = h4("Choose File type"),
               choices = list(".csv/txt" = 1, ".xlsx" = 2),
               selected = 1,
               inline = TRUE
             ),
             fileInput(
               'file1',
               h4('Upload Items List'),
               accept = c(
                 'text/csv',
                 'text/comma-separated-values,text/plain',
                 '.csv',
                 '.xlsx'
               )))
      
    ),
    tabItems(
      tabItem(tabName = "dashboard",
             tableOutput("table")
              ),
      
       tabItem(tabName = "plt1",
               h1("Survival plot of all machines"),
               plotOutput("plot1")
              ),
      tabItem(tabName = "plt2",
              h1("Survival plot of different providers"),
              plotOutput("plot2")
             ),
      tabItem(tabName = "plt3",
              h1("Boxplots for Machines, Teams and Providers"),
              plotOutput("plot3")
              ),
      tabItem(tabName = "Remaining",
              h1("Time remaining for a machine to break"),
              tableOutput("file3")
              )
      
    )
  )
  ))


