#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(shiny.reactlog = TRUE);

library(shiny);
library(ggplot2);
library(grid);
library(tidyr);
library(dplyr);
library(DT);
library(microbenchmark);
library(arrangements);



shinyServer(function(input, output) {
    
    number_of_lines <- reactiveVal(0);
    lines_at_step_two <- reactiveVal(0);
    lines <- reactiveVal(0);
    
    output$selectInput <- renderUI({
        selectInput("shape","Choose the shape",c("Rectangle","Circle","Triangle","Mesh","Random","Upload"))
    })
    
    output$length <- renderUI({
        if(input$shape=="Rectangle"){
            numericInput("length","Input number of points for the length",10, min=2, max=100)
        }
        else if(input$shape == "Circle"){
            numericInput("radius","Input radius of circle",1, min=1, max=100)
        }
        else if(input$shape == "Triangle"){
            numericInput("first_length","Input the length of the first side",1,min=1,max=100)
        }
        else if(input$shape == "Mesh"){
            numericInput("length","Input number of points for the length",10,min=1,max=100)
        }
        else if(input$shape == "Random"){
            numericInput("rando_number","Input the number of random points",10,min=1,max=1000)
        } else if(input$shape == "Upload"){
            fileInput("CSVIn", "Upload your .csv file here", multiple = TRUE, accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"), width = NULL, buttonLabel = "Browse...", placeholder = "No file selected")
        }
    })
    output$breadth <- renderUI({
        if(input$shape=="Rectangle"){
            numericInput("breadth","Input number of points for the breadth",10, min=2, max=100)    
        } else if(input$shape == "Triangle"){
            numericInput("second_length","Input the length of the second side",1,min=1,max=100)
        }else if(input$shape=="Mesh"){
            numericInput("breadth","Input number of points for the breadth",10, min=2, max=100)    
        }else if(input$shape=="Random"){
            checkboxInput("whole","Whole numbers only?",value = FALSE)
        } else if(input$shape == "Upload"){
            if(is.null(input$CSVIn)){return(helpText("Please input CSV!"));}
            selectInput("dataSelect","Select your data from list of data",input$CSVIn$name) 
        }
    })
    output$x_spacing <- renderUI({
        if(input$shape=="Rectangle"){
            numericInput("x_spacing","Input spacing between x",1,min=1,max=100)
        }
        else if(input$shape == "Triangle"){
            numericInput("alpha","Input the bottom right angle",45,min=1,max=100)
        }
        else if(input$shape=="Mesh"){
            numericInput("x_spacing","Input spacing between x",1, min=1, max=100)    
        }
        else if(input$shape == "Random"){
            numericInput("lower","Input the lower bound", 0, min=0, max = 1000)
        } else if(input$shape == "Upload"){
            if(is.null(input$CSVIn)){return();}
            selectInput("x_col","Choose the x column", colnames(data()));
        }
    })
    output$y_spacing <- renderUI({
        if(input$shape=="Rectangle"){
            numericInput("y_spacing","Input spacing between y",1,min=1,max=100)
        }
        else if(input$shape=="Circle"){
            numericInput("x_center","Input the x-center coordinate for the circle",0, min=0, max = 100)
        }
        else if(input$shape == "Triangle"){
            numericInput("tri_x_center","Input the x-coordinate of the origin of the shape",0,min=1,max=100)
        }
        else if(input$shape=="Mesh"){
            numericInput("y_spacing","Input spacing between y",1, min=1, max=100)    
        }
        else if(input$shape == "Random"){
            numericInput("upper","Input the upper bound", input$rando_number, min=0, max = 1000)
        } else if(input$shape == "Upload"){
            if(is.null(input$CSVIn)){return();}
            selectInput("y_col","Choose the y column", colnames(data()));
        }
    })
    output$start_x <- renderUI({
        if(input$shape=="Rectangle"){
            numericInput("start_x","Input the start x coordinate for the shape",0, min=0, max = 100)
        }
        else if(input$shape=="Circle"){
            numericInput("y_center","Input the y-center coordinate for the circle",0, min=0, max = 100)
        }
        else if(input$shape == "Triangle"){
            numericInput("tri_y_center","Input the y-coordinate of the origin of the shape",0,min=1,max=100)
        } else if(input$shape=="Mesh"){
            numericInput("start_x","Input the start x coordinate for the shape",0, min=0, max = 100)
        }
    })
    output$start_y<- renderUI({
        if(input$shape=="Rectangle"){
            numericInput("start_y","Input the start y coordinate for the shape",0, min=0, max = 100)
        }
        else if(input$shape == "Circle"){
            numericInput("num_points", "Input the number of points in the circle",20, min=4, max= 100);
        }
        else if(input$shape == "Triangle"){
            numericInput("tri_rotation","Input the angle of rotation of the triangle about the origin",0,min=1,max=100)
        } else if(input$shape=="Mesh"){
            numericInput("start_y","Input the start y coordinate for the shape",0, min=0, max = 100)
        }
    })
    output$outnumfirst <- renderUI({
        if(input$shape == "Triangle"){
            numericInput("num_first","Input the number of points for first length",10,min=1,max=100)
        }
        else{return()}
    })
    output$outnumsecond <- renderUI({
        if(input$shape == "Triangle"){
            numericInput("num_second","Input the number of points for second length",10,min=1,max=100)
        }
        else{return()}
    })
    output$outnumthird <- renderUI({
        if(input$shape == "Triangle"){
            numericInput("num_third","Input the number of points for third length",10,min=1,max=100)
        }
        else{return()}
    })
    output$outputBf <- renderUI({
        checkboxInput("gobf","By checking this box I consent to waiting a ridiculously long time for data under sections 'Brute Force' and 'Output Plots'", value = FALSE)
    })
    output$showPath <- renderUI({
        if(input$gobf){
            dropdown <- c("All","Hungarian","Nearest Neighbour","Brute Force","None");
        } else{
            dropdown <- c("All","Hungarian","Nearest Neighbour","None");
        }
        return(selectInput("showpath", "Choose which paths to show in Output Plots", dropdown))
    })
    
    
    
    #Create a dataframe that represents a shape by using coordinates of each of the points
    #Create a dataframe that shows the cost of travelling to each of the points (adjacency matrix)
    #Then solve using the Hungarian Algorithm
    
    data <- reactive({
        if(input$shape == "Upload"){
            if(is.null(input$CSVIn)){return();}
            upload <- input$CSVIn$datapath[input$CSVIn$name==input$dataSelect] %>% read.csv();
            upload <- na.omit(upload);
            return(upload);
        }
    })
    coordinates <- reactive({
        data_x <- data.frame(matrix(ncol=2,nrow=0)); #Create an empty dataframe with x and y coordinates as 0
        colnames(data_x) <- c("x","y");
        
        if(input$shape == "Rectangle"){
            for(val in 1:input$length){
                to_add <- data.frame(x=((val-1) * input$x_spacing)+input$start_x,y=(input$breadth-1)*input$y_spacing+input$start_y); #Generate the top line
                to_add2 <- data.frame(x=((val-1) * input$x_spacing)+input$start_x,y=input$start_y); #Generate the bottom line
                colnames(to_add) <- c("x","y");
                colnames(to_add2) <- c("x","y");
                data_x<- rbind(data_x,to_add, to_add2);
            }
        }
        else if(input$shape == "Circle"){
            num <- input$num_points;
            angles <- data.frame(x=seq(0, 2 * pi, length = num+1));
            #print(angles);
            data_x <- cbind(cos(angles) * input$radius + input$x_center, sin(angles) * input$radius + input$y_center);
            data_x <- data_x[(1:nrow(data_x)-1),];
            colnames(data_x) <- c("x","y");
            #print(data_x);
        }
        else if(input$shape == "Mesh"){
            data_x <- data.frame(x = rep(seq(input$start_x, input$start_x + input$x_spacing * input$length-1, input$x_spacing),input$breadth));
            # data_x_2 <- data.frame(x=rep(input$start_x,input$breadth));
            # data_x <- rbind(data_x_2,data_x);
            # print("mesh x head:")
            # print(data_x);
        }
        else if(input$shape == "Triangle"){
            #First Length is the length of the base, second is to the left, third is to the right
            #Taking an average of all the 3 points will give us the centroid of the triangle
            #Creating the base along the x axis
            
            first_length <- data.frame(x= seq(-(input$first_length/2),(input$first_length/2),length = input$num_first));
            first_length$y <- rep(0,input$num_first);
            
            #print("First length is: ");
            #print(first_length);
            
            # angle <- acos((input$first_length/2)/input$second_length); #Obtain the angle subtended by the first and second lengths
            # second_length <- data.frame(angle = rep(angle,input$num_second));
            # second_length$x <- seq(0,input$first_length/2,length = input$num_second);
            # second_length$y <- second_length$x * cos(second_length$angle);
            # second_length$y <- rev(second_length$y);
            
            x_minus_x1 <- input$first_length/2 - (input$second_length * cos(input$alpha * pi / 180))
            second_length <- data.frame(x= (seq(input$first_length/2, x_minus_x1, length = input$num_second)), y= seq(0, input$second_length * sin(input$alpha * pi / 180), length = input$num_second));
            #gamma <- atan(((input$second_length)*sin(input$alpha * pi / 180) - input$first_length)/(input$first_length * sin(input$alpha * pi / 180)));
            
            first_line_cood_min <- cbind(min(first_length$x),0);
            first_line_cood_max <- cbind(max(first_length$x), 0);
            second_line_cood_max <- cbind(min(second_length$x), max(second_length$y));
            second_line_cood_min <- first_line_cood_max;
            third_line_cood_min <- first_line_cood_min;
            third_line_cood_max <- second_line_cood_max;
            third_line_gradient <- (second_line_cood_max[2]-first_line_cood_min[2])/(second_line_cood_max[1]-first_line_cood_min[1]);
            second_line_gradient <- (second_line_cood_max[2]-second_line_cood_min[2])/(second_line_cood_max[1]-second_line_cood_min[1]);
            first_line_gradient <- 0;
            c_first_line <- 0;
            c_second_line <- second_line_cood_max[2] - second_line_gradient * second_line_cood_max[1];
            c_third_line <- second_line_cood_max[2] - third_line_gradient * second_line_cood_max[1];
            
            third_length <- data.frame(x = seq(first_line_cood_min[1], second_line_cood_max[1], length = input$num_third));
            third_length$y <- third_length$x * third_line_gradient + c_third_line;
            
            
            # print("Second length is: ");
            # print(second_length);
            
            # print("Third length is: ");
            # print(third_length);
            
            eqn_of_line <- function(data1){
                #Choose the largest and smallest coodinates
                data1 <- data.frame(x=data1[,1],y=data1[,2]);
                max_x <- data1[1,];
                min_x <- data1[2,];
                gradient <- (max_x[,2]-min_x[,2])/(max_x[,1]-min_x[,1]);
                c <- max_x[,2] - gradient * max_x[,1];
                toReturn <- cbind(gradient, c);
                return(toReturn);
            }
            midpoint <- function(cood1, cood2){
                coordinates_of_midpoint <- cbind((cood1[1]+cood2[1])/2,(cood1[2]+cood2[2])/2);
                return(coordinates_of_midpoint);
            }
            
            #Finding the centroids; poins of intersection between 
            first_to_third_vertex <- cbind(midpoint(first_line_cood_max, first_line_cood_min)) %>% rbind(third_line_cood_max);
            # print("First to third vertex:")
            # print(first_to_third_vertex);
            eqn_of_first_midpoint <- eqn_of_line(first_to_third_vertex);
            
            second_to_first_vertex <- cbind(midpoint(second_line_cood_max, second_line_cood_min)) %>% rbind(first_line_cood_min);
            # print("Second to first vertex:")
            # print(second_to_first_vertex);
            eqn_of_second_midpoint <- eqn_of_line(second_to_first_vertex);
            
            third_to_second_vertex  <- cbind(midpoint(third_line_cood_max, third_line_cood_min)) %>% rbind(first_line_cood_max);
            # print("Third to Second vertex:")
            # print(third_to_second_vertex);
            eqn_of_third_midpoint <- eqn_of_line(third_to_second_vertex);
            
            midpoint_x <- (eqn_of_first_midpoint[2]-eqn_of_second_midpoint[2])/(eqn_of_second_midpoint[1]-eqn_of_first_midpoint[1]);
            midpoint_y <- midpoint_x * eqn_of_first_midpoint[1] + eqn_of_first_midpoint[2];
            #print(cbind(midpoint_x,midpoint_y));
            
            midpoint1_x <- (eqn_of_second_midpoint[2]-eqn_of_third_midpoint[2])/(eqn_of_third_midpoint[1]-eqn_of_second_midpoint[1]);
            midpoint1_y <- midpoint1_x * eqn_of_first_midpoint[1] + eqn_of_first_midpoint[2];
            #print(cbind(midpoint1_x,midpoint1_y));
            
            #Shifting the points down so that the centroid of the triangle is at the origin
            Area <- 0.5 * input$first_length * input$second_length * sin(input$alpha);
            height <- 2 * Area / input$first_length;
            
            first_length$y <- first_length$y + input$tri_y_center - midpoint_y;
            second_length$y <- second_length$y + input$tri_y_center - midpoint_y;
            third_length$y <- third_length$y + input$tri_y_center - midpoint_y;
            
            first_length$x <- first_length$x + input$tri_x_center - midpoint_x;
            second_length$x <- second_length$x + input$tri_x_center - midpoint_x;
            third_length$x <- third_length$x + input$tri_x_center - midpoint_x;
            
            data_x <- rbind(first_length, select(second_length,x,y), select(third_length,x,y));
        } else if(input$shape == "Random"){
            if(input$whole){
                data_x <- data.frame(x=sample(input$lower:input$upper,input$rando_number-1), y=sample(input$lower:input$upper,input$rando_number-1));
                
            }
            else{
                data_x <- data.frame(x=runif(input$rando_number-1, input$lower, input$upper), y= runif(input$rando_number-1, input$lower, input$upper));
            }
        } else if(input$shape == "Upload"){
            if(is.null(input$CSVIn)){
                return();
            }
            data_x <- as.data.frame(data()[input$x_col]);
            colnames(data_x) <-c("x");
        }
        data_y <- data.frame(matrix(ncol=2,nrow=0)); #Create an empty dataframe with x and y coordinates as 0
        colnames(data_y) <- c("x","y");
        if(input$shape == "Rectangle"){
            for(val in 1:(input$breadth-1)){
                to_add3 <- data.frame(x=input$start_x, y=((val-1) * input$y_spacing)+input$start_y); #Generate the  left side
                to_add4 <- data.frame(x=input$start_x + (input$length-1)*input$x_spacing, y=((val-1) * input$y_spacing)+input$start_y); #Generate the right side
                colnames(to_add3) <- c("x","y");
                colnames(to_add4) <- c("x","y");
                data_y <- rbind(data_y, to_add3, to_add4);
            }
        }
        else if(input$shape == "Circle"){}
        else if(input$shape == "Mesh"){
            data_y <- data.frame(y = rep(seq(input$start_y, input$start_y + input$y_spacing * input$breadth-1, input$y_spacing),each = (input$length)));
            #data_x <- data.frame(x = rep(seq(input$start_x, input$start_x + input$x_spacing * input$length, input$x_spacing),input$breadth));
            # print("mesh y head:")
            # print(data_y);
        }
        else if(input$shape == "Triangle"){
            data_y <- data.frame(x=0,y=0);
        }
        else if(input$shape == "Random"){
        } else if(input$shape == "Upload"){
            if(is.null(input$CSVIn)){
                return();
            }
            data_y <- as.data.frame(data()[input$y_col]);
            colnames(data_y) <- c("y");
        }
        
        if(input$shape == "Mesh" | input$shape == "Upload"){
            data_combined <- cbind(data_x,data_y)
            # print("data_combined");
            # print(data_combined);
        } else{
            data_combined  <- rbind(data_x,data_y);
        }
        # print("data combined:")
        # print(data_combined)
        data_combined  <- rbind(c(0,0),data_combined);
        print("added zeros");
        if(input$shape == "Mesh"){
            return((data_combined));
        } else {
            return(distinct(data_combined))
        }
    })
    
    distance_matrix_x <- reactive({
        data <- coordinates(); 
        matrix_x <- data.frame(matrix(nrow= nrow(data), ncol = nrow(data))); #difference in x values between each point
        for(val1 in 1:nrow(data)){
            for(val2 in 1:nrow(data)){ 
                if(val1 == val2){
                    matrix_x[val2,val1] <- 999999;
                    #print(data[val1,val1]);
                    #break;
                }
                else{
                    matrix_x[val2,val1] <- data[val2,1] - data[val1,1];
                }
            }
        }
        return(matrix_x);
    })
    distance_matrix_y <- reactive({
        data <- coordinates(); 
        matrix_y <- data.frame(matrix(nrow= nrow(data), ncol = nrow(data))); #difference in y values between each point
        for(val1 in 1:nrow(data)){
            for(val2 in 1:nrow(data)){
                if(val1 == val2){
                    matrix_y[val2,val1] <- 999999;
                    #break;
                }
                else{
                    matrix_y[val2,val1] <- data[val2,2] - data[val1,2];
                }
            }
        }
        return(matrix_y);
    })
    combined_matrix <- reactive({
        x <- distance_matrix_x();
        y <- distance_matrix_y();
        data <- coordinates();
        matrix <- data.frame(matrix(nrow= nrow(data), ncol = nrow(data)));
        
        for(val1 in 1:nrow(data)){
            for(val2 in 1:nrow(data)){
                if(val1 == val2){
                    data[2,val1] <- 0;
                    #break;
                }
                else{
                    matrix[val1,val2] <- sqrt(x[val1,val2]**2 + y[val1,val2]**2);
                }
            }
        }
        matrix <- matrix %>% mutate_all(~replace(., is.na(.), 100000));
        return(matrix);
    })
    
    #Start doing the Hungarian Algorithm
    stepone <- reactive({
        print("stepone");
        #Step One: Find the smallest entry in each row and subtract it from every entry in its row. Find the smallest entry in each column and subtract it from every entry in its column
        matrix <- combined_matrix();
        # print(head(matrix))
        for(i in 1:nrow(matrix)){
            toSubtract <- min(matrix[i,]);
            print(toSubtract);
            matrix[i,] <- matrix[i,]-toSubtract;
        }
        for(j in 1:ncol(matrix)){
            toSubtract <- min(matrix[,j]);
            print(toSubtract)
            matrix[,j] <- matrix[,j]-toSubtract
        }
        return(zapsmall(matrix));
    })
    #Remove the smallest value in each row from the row; smallest value of each column from the column
    
    steptwo <- reactive({
        print("Step two");
        #Create two matrices - one will store the original value of step one,
        #Another will be used solely to carry out the placement of the lines. 
        matrix <- stepone();
        #Line iteral iterates through the number of zeros for rows and columns and is used to determine the position of the lines.
        line_iteral <- data.frame(matrix(ncol=3,nrow = 2 * ncol(matrix))); #create a matrix to store the number of zeros in each row and column
        colnames(line_iteral) <- c("Position","Num","Type");
        
        #Lines to draw is a local variable that stores the lines that are to be drawn. 
        lines_to_draw <- matrix(nrow = 2,ncol = ncol(matrix)); #Create another matrix to store the positions of the lines (1 means line, 0 means No line)
        colnames(lines_to_draw) <- colnames(matrix);
        lines_to_draw[1,] <- 0;
        lines_to_draw[2,] <- 0;
        lines_at_step_two(lines_to_draw);
        
        #Iterate through the entire matrix, in each cell, return no. 0s in rows - no. of 0s in columns
        for(i in 1:nrow(matrix)){
            line_iteral[i,1] <- i; #First column is the index of row
            line_iteral[i,2] <- sum(matrix[i,] == 0); #Sum of zeros
            line_iteral[i,3] <- "Row" #Placeholder to differentiate row from column
        } #Sum up all the zeros in the rows
        for(j in 1:ncol(matrix)){
            line_iteral[j+ncol(matrix),1] <- j; #First column is the index of column
            line_iteral[j+ncol(matrix),2] <- sum(matrix[,j] == 0); ##Sum of zeros
            line_iteral[j+ncol(matrix),3] <- "Col"; #Placeholder to differentiate row from column
        } #Sum up all the zeros in the columns
        
        total_zeros <- sum(line_iteral[1:ncol(matrix),2]);
        # print(total_zeros)
        
        
        while(total_zeros > 0){ #In this while loop we repeat the process of drawing lines until no more zeros remain
            largest <- line_iteral %>% slice(which.max(Num)); #Find the largest number of zeroes in the line iteral
            if(largest[3] == "Row"){
                lines_to_draw[1,as.numeric(largest[1])] <- 1; #Append "1" to the position to signify a line being drawn 
                matrix[as.numeric(largest[1]),] <- 1000000; #Wash out the entire row with 1000000 to prevent the zero from being selected again. 
            }
            else{
                matrix[,as.numeric(largest[1])] <- 1; #Wash out the entire row with 1000000 to prevent the zero from being selected again
                lines_to_draw[2,as.numeric(largest[1])] <- 1; #Append "1" to the position to signify a line being drawn 
            }
            for(i in 1:nrow(matrix)){
                #line_iteral[i,1] <- i; #already done
                line_iteral[i,2] <- sum(matrix[i,] == 0); #recalculate the sum of zeroes in each row
                #line_iteral[i,3] <- "Row" #already done
            } #Sum up all the zeros in the rows
            for(j in 1:ncol(matrix)){
                #line_iteral[j+ncol(matrix),1] <- j; #already done
                line_iteral[j+ncol(matrix),2] <- sum(matrix[,j] == 0);
                #line_iteral[j+ncol(matrix),3] <- "Col"; #already done
            } #Sum up all the zeros in the columns
            total_zeros <- sum(line_iteral[1:ncol(matrix),2])
        }
        #print(line_iteral);
        # print("Lines to draw: ")
        # print(lines_to_draw);
        lines_at_step_two(lines_to_draw);
        lines(lines_to_draw);
        number_of_lines(sum(lines_to_draw[1,]) + sum(lines_to_draw[2,]));
        # print("Number of lines");
        # print(number_of_lines());
        return(matrix)
        
    })
    
    transposed <- reactive({
        matrix <- stepone();
        transposed <- as.data.frame(t(lines_at_step_two()));
        # print("In transposed: lines at step two: ")
        # print(lines_at_step_two())
        matrix <- cbind(matrix, transposed[,1]);
        matrix <- rbind(lines_at_step_two()[2,],matrix);
        return(matrix);
    })
    stepthree <- reactive({
        if(input$panel == "nn"){
            return(combined_matrix());
        }
        num_lines <- number_of_lines();
        print("Step three");
        matrix_clean <- stepone(); #Taking a clean matrix from step one
        matrix <- steptwo(); #Taking the matrix which has lines drawn on it
        
        # print("matrix_clean - clean matrix from step 1");
        # print(matrix_clean);
        # 
        # print("Matrix - dirty matrix from step 1");
        # print(matrix);
        
        line_iteral <- data.frame(matrix(ncol=3, nrow = 2*ncol(matrix))); #create a matrix to store the number of zeros in each row and column
        colnames(line_iteral) <- c("Position","Num","Type"); 
        lines_to_draw <- lines(); #Create another matrix to store the positions of the lines (1 means line, 0 means No line); row 1 is for "rows", row 2 is for "columns"
        # print("Lines to draw");
        # print(lines_to_draw);
        
        while(num_lines < ncol(matrix_clean)){
            
            print("No. of lines not the same!");
            smallest <- as.numeric(min(unlist(matrix))); #Take the smallest UNCOVERED value from the Matrix w lines on it (Gave the covered elements a very high value)
            # print("Smallest:");
            # print(smallest);
            for(i in 1:nrow(matrix_clean)){ #Subtract the smallest value from UNCOVERED entries and add the least value to COVERED entries
                for(j in 1:ncol(matrix_clean)){
                    if(lines_to_draw[1,i] == 0 & lines_to_draw[2,j]==0){
                        matrix_clean[i,j] <- matrix_clean[i,j] - smallest;
                        #print(paste("Subtracted smallest value of: ", toString(smallest), " to row ", toString(i), "and column", toString(j)));
                    }
                    else if(lines_to_draw[1,i] == 1 & lines_to_draw[2,j] == 1){
                        matrix_clean[i,j] <- matrix_clean[i,j] + smallest;
                        #print(paste("Added smallest value of: ", toString(smallest), " to row ", toString(i), "and column", toString(j)));
                    }
                }
            }
            print("Saving matrix_clean");
            matrix3 <- matrix_clean; #Store the untouched matrix in a reactive global var
            
            lines_to_draw[1,] <- 0; #Reset the matrix which stores the position of the lines
            lines_to_draw[2,] <- 0;
            
            # print("matrix_clean - edited matrix");
            # print(matrix_clean);
            for(i in 1:nrow(matrix_clean)){
                line_iteral[i,1] <- i;
                line_iteral[i,2] <- sum(matrix_clean[i,] == 0);
                line_iteral[i,3] <- c("Row");
            } #Sum up all the zeros in the rows
            for(j in 1:ncol(matrix_clean)){
                line_iteral[j+ncol(matrix_clean),1] <- j;
                line_iteral[j+ncol(matrix_clean),2] <- sum(matrix_clean[,j] == 0);
                line_iteral[j+ncol(matrix_clean),3] <- c("Col");
            } #Sum up all the zeros in the columns
            total_zeros <- sum(line_iteral[1:(ncol(matrix)),2]);
            # print("line_iteral line 468: ");
            # print(line_iteral);
            #  print("Total Zeros:");
            #  print(total_zeros);
            while(total_zeros > 0){
                largest <- line_iteral %>% slice(which.max(Num));
                #print(largest);
                if(largest[3] == "Row"){
                    matrix_clean[as.numeric(largest[1]),] <- 99999999;
                    lines_to_draw[1,as.numeric(largest[1])] <- 1;
                }
                else{
                    matrix_clean[,as.numeric(largest[1])] <- 99999999;
                    lines_to_draw[2,as.numeric(largest[1])] <- 1;
                }
                for(i in 1:nrow(matrix_clean)){
                    line_iteral[i,1] <- i;
                    line_iteral[i,2] <- sum(matrix_clean[i,] == 0);
                    line_iteral[i,3] <- "Row"
                } #Sum up all the zeros in the rows
                for(j in 1:ncol(matrix_clean)){
                    line_iteral[j+ncol(matrix_clean),1] <- j;
                    line_iteral[j+ncol(matrix_clean),2] <- sum(matrix_clean[,j] == 0);
                    line_iteral[j+ncol(matrix_clean),3] <- "Col";
                } #Sum up all the zeros in the columns
                total_zeros <- sum(line_iteral[1:ncol(matrix_clean),2]);
                # print("Total Zeros");
                # print(total_zeros);
            }
            #print(line_iteral)
            num_lines <- sum(lines_to_draw[1,]) + sum(lines_to_draw[2,]);
            print("Number of lines:");
            print(num_lines);
            # print("Lines to draw:");
            # print(lines_to_draw);
            matrix <- matrix_clean;
            matrix_clean <- matrix3;
            # print("No of columns: ");
            # print(ncol(matrix_clean))
            if(num_lines >= ncol(matrix_clean)){
                break;
            }
        }
        matrix3 <- matrix_clean;
        #lines(lines_to_draw);
        print("End of step 3");
        return(matrix_clean); 
    })
    output$startSelect <- renderUI({
        selectInput("StartSelect","Choose your Start Column",colnames(stepthree()),"X1");
    })
    stepfive  <- reactive({
        print("step five")
        data <- coordinates();
        final_matrix <- stepthree(); #Take the default to be step
        cost_matrix <- combined_matrix();
        if(ncol(final_matrix)<2){
            return(helpText("Input a dataset of dimension n > 1"));
        }
        if(is.null(input$StartSelect)){
            current_column <- "X1";
            next_column <-  "X1";
        } else{
            current_column <- input$StartSelect; 
        }
        current_column_number <- as.numeric(which(colnames(final_matrix)==current_column));#Get the column number of the selected column
        output_matrix <- data.frame(matrix(nrow = 4, ncol = ncol(final_matrix))); 
        rownames(output_matrix) <- c("Column number","x_value","y_value", "cost_to_next_point");
        #Create the output matrix which will order the points
        #Row 1 - the column number corresponding to the column index
        #Row 2 - the x-value of the column 
        #Row 3 - the y value of the column
        #Row 4 - the cost of travelling from this point to the next point
        for(i in 1:ncol(final_matrix)){
            next_column_number <- which.min(final_matrix[,current_column_number]);
            output_matrix[1,i] <- current_column_number; #Append the position we are currently on to the sequential position on the output matrix
            output_matrix[2,i] <- data[current_column_number,1];
            output_matrix[3,i] <- data[current_column_number,2];
            if(i < ncol(output_matrix)){
                output_matrix[4,i] <- cost_matrix[next_column_number,current_column_number];
            } else{
                output_matrix[4,i] <- 0;
            }
            final_matrix[current_column_number,] <- 999999; #Wash the entire row with large value to prevent the point that we just left from being chosen again
            current_column_number <- next_column_number; #Update the current column number
        }
        return(output_matrix);
    })
    output$stats <- renderText({
        text <- "Hungarian Algorithm: Total Cost = ";
        data <- stepfive();
        sum <- sum(data[4,]);
        text <- paste(text, toString(sum));
        return(text);
    })
    output$stats_nn <- renderText({
        text <- "Nearest Neighbour: Total Cost=";
        data <- stepfive_nn();
        sum <- sum(data[4,]);
        text <- paste(text,toString(sum));
        return(text);
    })
    output$stats_bf <- renderText({
        if(input$gobf){
            text <- "Brute Froce: Total Cost=";
            data <- stepfive_bf();
            sum <- sum(data[4,]);
            text <- paste(text,toString(sum));
            return(text);
        } else{
            return("Please consent to waiting for bf");
        }
    })
    stepfive_nn  <- reactive({
        print("step five_nn")
        data <- coordinates();
        final_matrix <- combined_matrix(); #Take the default to be step
        if(ncol(final_matrix)<2){
            return(helpText("Input a dataset of dimension n > 1"));
        }
        if(is.null(input$StartSelect)){
            current_column <- "X1";
            next_column <-  "X1";
        } else{
            current_column <- input$StartSelect; 
        }
        current_column_number <- as.numeric(which(colnames(final_matrix)==current_column));#Get the column number of the selected column
        output_matrix <- data.frame(matrix(nrow = 4, ncol = ncol(final_matrix))); 
        rownames(output_matrix) <- c("Column number","x_value","y_value","cost_to_next_point");
        #Create the output matrix which will order the points
        #Row 1 - the column number corresponding to the column index
        #Row 2 - the x-value of the column 
        #Row 3 - the y value of the column
        #Row 4 - the cost from travelling from this point to the next point
        for(i in 1:ncol(final_matrix)){
            # print("current column number");
            # print(current_column_number);
            next_column_number <- which.min(final_matrix[,current_column_number]);
            # print("next column number");
            # print(next_column_number);
            output_matrix[1,i] <- current_column_number; #Append the position we are currently on to the sequential position on the output matrix
            output_matrix[2,i] <- data[current_column_number,1];
            output_matrix[3,i] <- data[current_column_number,2];
            if(i < ncol(output_matrix)){
                output_matrix[4,i] <- final_matrix[next_column_number,current_column_number];
            } else{
                output_matrix[4,i] <- 0;
            }
            final_matrix[current_column_number,] <- 999999; #Wash the entire row with large value to prevent the point that we just left from being chosen again
            current_column_number <- next_column_number; #Update the current column number
        }
        return(output_matrix);
    })
    stepfive_bf <- reactive({
        if(input$gobf){
            print("stepfive bf");
            final_matrix <- combined_matrix();
            data <- coordinates();
            if(ncol(final_matrix)<2){
                return(helpText("Input a dataset of dimension n > 1"));
            }
            if(is.null(input$StartSelect)){
                current_column <- "X1";
                next_column <-  "X1";
            } else{
                current_column <- input$StartSelect; 
            }
            current_column_number <- as.numeric(which(colnames(final_matrix)==current_column));#Get the column number of the selected column
            output_matrix <- data.frame(matrix(nrow = 4, ncol = ncol(final_matrix))); 
            rownames(output_matrix) <- c("Column number","x_value","y_value","cost_to_next_point");
            #Create the output matrix which will order the points
            #Row 1 - the column number corresponding to the column index
            #Row 2 - the x-value of the column 
            #Row 3 - the y value of the column
            #Row 4 - the cost from travelling from this point to the next point
            
            highscore <- 999999;
            group_iteral <- npermutations(ncol(final_matrix)-1);
            # print("group iteral");
            # print(group_iteral);
            
            for(i in (1 + group_iteral * (current_column_number-1)):(group_iteral * (current_column_number))){
                output_matrix[1,] <- permutations(ncol(final_matrix), index = i);
                # print("output matrix after adding the order");
                # print(output_matrix);
                for(j in 1:ncol(output_matrix)){
                    output_matrix[2,j] <- data[output_matrix[1,j],1]; #put the x value of data into the final matrix
                    # print("output matrix after adding the x values");
                    # print(output_matrix);
                    output_matrix[3,j] <- data[output_matrix[1,j],2]; #put the y value of data into the finial matrix
                    # print("output matrix after adding the y values");
                    # print(output_matrix);
                    if(j < ncol(output_matrix)){
                        output_matrix[4,j] <- final_matrix[output_matrix[1,j+1],output_matrix[1,j]]; #Put the cost into the cost matrix
                        #print("output matrix after adding the costs");
                        #print(output_matrix);
                    }
                    else{
                        output_matrix[4,j] <- 0;
                    }
                    
                }
                if(sum(output_matrix[4,])<highscore){
                    highscorematrix <- output_matrix;
                    highscore <- sum(output_matrix[4,]);
                    # print("New Highscore:");
                    # print(highscorematrix);
                    # print("Score:");
                    # print(highscore);
                }
            }
            return(highscorematrix);
        } else{
            return(helpText("Please consent to waiting"))
        }
    })
    transposed2 <- reactive({
        matrix <- stepthree();
        transposed <- as.data.frame(t(lines()));
        print("In transposed: lines at step two: ")
        print(lines());
        matrix <- cbind(matrix, transposed[,1]);
        matrix <- rbind(lines()[2,],matrix);
        return(matrix);
    })
    
    #Start doing the Nearest Neighbour Algorithm
    output$stepone_nn <- DT::renderDataTable({
        DT::datatable(stepfive_nn());
    })
    
    output$stepthree_bf <- DT::renderDataTable({
        DT::datatable(stepfive_bf());
    })
    gg <- reactive({
        ggplot <- ggplot(data=coordinates(),mapping = aes(x="x", y="y")) + geom_point(mapping = aes(x=coordinates()[,1], y=coordinates()[,2]));
        return(ggplot);
    })
    output$plot <- renderPlot({
        gg();
    })
    output$stepnumber <- renderUI({
        numericInput("StepNo","Step Number:",1,min=1,max=nrow(as.data.frame(t(stepfive()))),step=1)
    });
    output$outputPlot <- renderPlot({
        data <- as.data.frame(t(stepfive()));
        stepthree <- stepthree();
        print(data)
        data_nn <- as.data.frame(t(stepfive_nn()));
        colname <- colnames(stepthree)[data[input$StepNo,1]];
        print(colname)
        visited <- data[1:input$StepNo,1];
        print("Visited")
        print(visited)
        topthree <- stepthree %>% select(colname) %>% mutate(ID = seq(1,nrow(stepthree),1));
        topthree <- topthree %>% subset(!(topthree$ID %in% visited)) %>% top_n(-3, get(colname))
        print("Top three:");
        print(topthree);
        if(input$gobf){
            data_bf <- as.data.frame(t(stepfive_bf()));
        }
        colnames(data) <- c("ID","x","y","cost");
        rownames(data) <- data$ID;
        print(data)
        #data$ID <- as.numeric(levels(data$ID))[data$ID]
        gg <- ggplot(data=data,mapping = aes(x=x, y=y, label = rownames(data))) + geom_point(mapping = aes(x=data[,2], y=data[,3]));
        gg <- gg + geom_text(vjust = 0, nudge_x = 0.0, nudge_y = 0.5)
        for(j in 1:input$StepNo){
            gg <- gg + geom_segment(x = data[(j),2], y = data[(j),3], xend = data[(j+1),2], yend = data[(j+1),3], color = "purple", data = data, size = 1, arrow = arrow(length = unit(0.5, "cm")));
        }
        for(i in input$StepNo:(nrow(data))){
            if(input$showpath == "All" | input$showpath == "Hungarian"){
                if(i == input$StepNo+1){
                   gg <- gg + geom_segment(x = data[input$StepNo,2], y = data[input$StepNo,3], xend = data[(i),2], yend = data[(i),3], color = "red", data = data, size = 1, arrow = arrow(length = unit(0.5, "cm")));
                } else{
                    if(data[i,1] %in% topthree$ID){
                        gg <- gg + geom_segment(x = data[input$StepNo,2], y = data[input$StepNo,3], xend = data[(i),2], yend = data[(i),3], color = "green", data = data, size = 1, arrow = arrow(length = unit(0.5, "cm")));   
                    }
                }
            } 
            if(input$showpath == "All" | input$showpath == "Nearest Neighbour"){
                if(i == input$StepNo){
                    gg <- gg + geom_segment(x = data_nn[input$StepNo,2], y = data_nn[input$StepNo,3], xend = data_nn[(i+1),2], yend = data_nn[(i+1),3], color = "red", data = data_nn, size = 1, arrow = arrow(length = unit(0.5, "cm")));
                }else{
                    if(data[i,1] %in% topthree$ID){
                    gg <- gg + geom_segment(x = data_nn[input$StepNo,2], y = data_nn[input$StepNo,3], xend = data_nn[(i+1),2], yend = data_nn[(i+1),3], color = "green", data = data_nn, size = 1, arrow = arrow(length = unit(0.5, "cm")));  
                    }
                }
            }
        }
        if(input$showpath == "All" | input$showpath == "Hungarian"){
            gg <- gg + geom_segment(x = data[i,2], y = data[i,3], xend = data[(i+1),2], yend = data[(i+1),3], mapping = aes(color = "hung"), data = data, size = 1, arrow = arrow(length = unit(0.5, "cm")));
        } 
        if(input$showpath == "All" | input$showpath == "Nearest Neighbour"){
            gg <- gg + geom_segment(x = data_nn[i,2], y = data_nn[i,3], xend = data_nn[(i+1),2], yend = data_nn[(i+1),3], mapping = aes(color = "nn"), data = data_nn, size = 1, arrow = arrow(length = unit(0.5, "cm")));
        }
        if(input$showpath == "All" | input$showpath == "Brute Force"){
            if(input$gobf){
                gg <- gg + geom_segment(x = data_bf[i,2], y = data_bf[i,3], xend = data_bf[(i+1),2], yend = data_bf[(i+1),3], mapping = aes(color = "bf"), data = data_bf, size = 1, arrow = arrow(length = unit(0.5, "cm")));
            }
        }
        filename = paste("plot", input$StepNo, ".png", sep = "");
        png(as.name(filename));
        print(gg);
        dev.off();
        return(gg);
    })

    time_comp <- reactive({
        if(input$gobf){
            return(microbenchmark(stepfive(),stepfive_nn(),stepfive_bf()));
        } else{
            return(microbenchmark(stepfive(),stepfive_nn()));
        }
    })
    output$time_comp <- renderPrint({
        return(time_comp());
    })
    output$time_comp_plot <- renderPlot({
        ggplot2::autoplot(time_comp());
    })
    output$table <- renderTable({
        coordinates();
    })
    output$dist_matrix_x <- DT::renderDataTable({
        distance_matrix_x();
    })
    output$dist_matrix_y <- DT::renderDataTable({
        distance_matrix_y();
    })
    output$combined_matrix <- DT::renderDataTable({
        combined_matrix();
    })
    output$stepone <- DT::renderDataTable({
        DT::datatable(stepone(),options = list(lengthMenu = c(100,1000,10000), pageLength = 100));
    })
    output$steptwo <- DT::renderDataTable({
        DT::datatable(transposed(), options = list(lengthMenu = c(100,1000,10000), pageLength = 100)) %>% formatStyle(colnames(lines_at_step_two())[lines_at_step_two()[2,]==1], backgroundColor = "yellow") %>% formatStyle(colnames(steptwo()), valueColumns = "transposed[, 1]", target = "row", backgroundColor = styleEqual(c(1,0), c("yellow","white")));
    })
    output$stepthree <- DT::renderDataTable({
        DT::datatable(stepthree(), options = list(lengthMenu = c(100,1000,10000), pageLength = 100));
    })
    output$stepfour <- DT::renderDataTable({
        DT::datatable(transposed2(), options = list(lengthMenu = c(100,1000,10000), pageLength = 100)) %>% formatStyle(colnames(lines())[lines()[2,]==1], backgroundColor = "yellow") %>% formatStyle(colnames(steptwo()), valueColumns = "transposed[, 1]", target = "row", backgroundColor = styleEqual(c(1,0), c("yellow","white"))); 
    })
    output$stepfive <- DT::renderDataTable({
        DT::datatable(stepfive());
    })
})