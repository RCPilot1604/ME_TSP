#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Math Exploration - Modified TSP"),
    helpText("Please select a simple shape from the dropdown or upload a csv of 2 columns with your coordinates of the various points [WORK-IN-PROGRESS]. This app will optimize a path that covers all the points ONCE (and DOES NOT return to the original point); TSP but does not return. An actual TSP app may be released soon using more professional Concorde TSP"),
    helpText("As of now this app supports Hungarian Algorithm, Nearest Neighbour and Brute Force"),
    strong(h4("By Ethan Chua, 2020")),
    h5("Feel free to use, : )"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            uiOutput("selectInput"),
            uiOutput("upload"),
            uiOutput("FileSelect"),
            uiOutput("length"),
            uiOutput("breadth"),
            uiOutput("x_spacing"),
            uiOutput("y_spacing"),
            uiOutput("start_x"),
            uiOutput("start_y"),
            uiOutput("outnumfirst"),
            uiOutput("outnumsecond"),
            uiOutput("outnumthird"),
            uiOutput("startSelect"),
            uiOutput("showPath"),
            uiOutput("outputBf")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Plot", plotOutput("plot")), 
                tabPanel("Main Data", tableOutput("table")),
                tabPanel("Distance Matrix", helpText("Matrix of Distance between X coordinates"), DT::dataTableOutput("dist_matrix_x"), helpText("Matrix of Distance between Y coordinates"), DT::dataTableOutput("dist_matrix_y"), helpText("Matrix of Shortest Dist between 2 coordinates (Using Pythagoras')"), DT::dataTableOutput("combined_matrix")),
                tabPanel("Hungarian", value = "hung", helpText("Step One: Find the smallest entry in each row and subtract it from every entry in its row. Find the smallest entry in each column and subtract it from every entry in its column."),
                         DT::dataTableOutput("stepone"), helpText("Step Two: Draw lines through the columns and rows of the matrix, connecting all the 0s using as a few lines as possible."),
                         DT::dataTableOutput("steptwo"),helpText("Step Three: Testing for optimality, the number of lines drawn should equal to the number of rows / columns in the matrix. If this is the case, move to Step 5. Else, move to Step 4. "),
                         DT::dataTableOutput("stepthree"), helpText("Step Four: Find the smallest entry not covered by one of our lines. Subtract this number from each row which has at least one entry uncovered and add it to each column which has at least one entry uncovered. Repeat Step 2 and Step 3."),
                         DT::dataTableOutput("stepfour"),helpText("Step Five: Starting from any column, choose the smallest value in the column and determine the optimum path by repeating the steps by moving to the column whose value is the same as the chosen row in the previous column."), 
                         DT::dataTableOutput("stepfive")
                ),
                tabPanel("Nearest Neighbour", value = "nn", helpText("Step One: Starting from a random column, find the entry with the smallest value in the column."),
                         DT::dataTableOutput("stepone_nn"), helpText("Step Two: Proceed to move to the column whose row has the smallest value in step 1. Strike out the row which was visited in Step 1."),
                         DT::dataTableOutput("steptwo_nn"),helpText("Step Three: Repeat Step 1 and 2 until all the points have been visited. "),
                         DT::dataTableOutput("stepthree_nn"), helpText(""),
                         DT::dataTableOutput("stepfour_nn"),helpText(""), 
                         DT::dataTableOutput("stepfive_nn")
                ),
                tabPanel("Brute Force", value = "bf", helpText("Step One: Iterate through every permutation which starts with the column that you selected to be the first column"),
                         DT::dataTableOutput("stepone_bf"), helpText("Step Two: If the cost of the route is smaller than the highscore, save it to the variable highscore. Save the arrangement to highscore matrix."),
                         DT::dataTableOutput("steptwo_bf"),helpText("Step Three: Return highscore and highscore matrix"),
                         DT::dataTableOutput("stepthree_bf"), helpText(""),
                         DT::dataTableOutput("stepfour_bf"),helpText(""), 
                         DT::dataTableOutput("stepfive_bf")
                ),
                tabPanel("Output Plots", plotOutput("outputPlot"),
                         uiOutput("stepnumber"),
                         verbatimTextOutput("stats"),
                         verbatimTextOutput("stats_nn"),
                         verbatimTextOutput("stats_bf"),
                         verbatimTextOutput("time_comp"),
                         plotOutput("time_comp_plot")
                ),
                id = "panel"
            ),
        )
    )
))
