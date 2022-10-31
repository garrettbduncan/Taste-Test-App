
library(shiny)
library(gt)
library(tidyverse)
library(GGally)

shinyServer(function(input, output) {
    
    # Get these constants
    numPeople <- reactive(input$num_people)
    numFoods <- reactive(input$num_food_items)
    
    output$names_entering <- renderUI({
        lapply(1:numPeople(), function(i) {
            column(width = 12,
                   textInput(paste0('person_', i),
                             paste0(i, '.'),
                             value = '')
            )
        })
    })
    
    # Output the key
    output$admin_food_key <- renderUI({
        lapply(1:numFoods(), function(j) {
            column(width = 12,
                   textInput(paste0('food_', j),
                             paste0('(', LETTERS[j], ')'),
                             value = '')
            )
        })
    })
    
    ########################################################################
    ########################################################################
    ###### 2nd page output
    
    output$voting_top_row <- renderUI({
        # Get the number of people and food items
        
        ### Output slots for the number of food items
        
        # Adjust the width based on the number of food items
        col_width <- floor(12/(numFoods() + 1))
        
        # Output a top row of food items
        fluidRow(
            column(width = col_width, p('')), # A blank spot
            
            # Display the index of food items
            lapply(1:numFoods(), function(j) {
                column(width = col_width,
                       h3(style = 'text-align: center;', paste0('(', LETTERS[j], ')')))
            })
        )
        
    })
    
    # Get the number of people
    output$voting <- renderUI({
        # Get the number of people and food items
        # Adjust the width based on the number of food items
        col_width <- floor(12/(numFoods() + 1))
        
        # Now output the slots for each persons vote for each food item
        lapply(1:numPeople(), function(i) { # For each person
            fluidRow(
                # Output each name of the person
                column(width = col_width,
                       h3(paste0(input[[paste0('person_', i)]]))),
                lapply(1:numFoods(), function(j) { # For each food item
                    # Output places where we can vote
                    column(width = col_width,
                           numericInput(
                               inputId = paste0('person_', i, '_food_', j),
                               # paste0(input[[paste0('person_', i)]], ', Food ', j),
                               label = '',
                               value = 6,
                               min = 0,
                               max = 10,
                               step = 0.25
                           )
                    )
                })
            )
        })
    })
    
    ########################################################################
    ########################################################################
    ###### 3rd page output
    
    #### All the stuff from the action button
    observeEvent(input$run_results, {
        
        # Make this a list
        input_list <- reactiveValuesToList(input)
        
        # Create the table
        votes <- matrix(0, ncol = numFoods(), nrow = numPeople())
        
        # Turn this into a list
        input_list <- reactiveValuesToList(input)
        
        # Populate the table
        for (i in 1:numPeople()) {
            for (j in 1:numFoods()) {
                votes[i, j] <- unlist(input_list[[paste0('person_', i, '_food_', j)]])
            }
        }
        
        # Make it a DF
        people_names <- sapply(1:numPeople(), function(i) input[[paste0('person_', i)]]) # Get the person names
        restaurant_names <- sapply(1:numFoods(), function(j) input[[paste0('food_', j)]]) # Get the restaurant names
        votes <- as.data.frame(votes) # Make it a DF
        names(votes) <- restaurant_names # Fix the row and column names
        row.names(votes) <- people_names
        
        
        ###### THE KEY AND VOTES
        output$food_key <- render_gt({
            data.frame('ID' = LETTERS[1:numFoods()],
                       'Restaurant' = restaurant_names) %>% 
                gt() %>% 
                tab_header('Key to blind taste test')
        })
        
        ### Ranking for each person
        output$each_person_rank <- render_gt({
            # Output the ranking for each person
            apply(round(votes, 2), MARGIN = 1, FUN = function(this_row) {
                paste0(restaurant_names[order(this_row, decreasing = TRUE)], ' (', this_row[order(this_row, decreasing = TRUE)], ')')
            }) %>% 
                as.data.frame() %>% # Make it a dataframe
                mutate(rank_help = paste0('#', 1:numFoods())) %>% 
                gt::gt(rowname_col = 'rank_help') %>% 
                tab_header('Each person\'s votes, ranked') %>% 
                tab_style(style = cell_text(align = 'center'),
                          locations = list(cells_body(),
                                           cells_column_labels()))
        })
        
        ###### SCORING/AWARDS
        # The medalists
        output$weighted_rank <- render_gt({
            # Get the top three for each person
            top_three_each <- apply(votes, MARGIN = 1, FUN = function(this_row) {
                restaurant_names[order(this_row, decreasing = TRUE)[1:3]]
            })
            
            # Then add up the number of 1st place votes multiplied by 3 points, 2nd place votes multiplied by 2 points, etc.
            medal_counts <- map_dfr(.x = 1:3, .f = function(i) {
                    table(top_three_each[i, ])#*(4-i)
                }) %>% t() %>% as.data.frame()
            
            weighted_tally <- map_dfr(.x = 1:3, .f = function(i) {
                table(top_three_each[i, ])*(4-i)
            }) %>% 
                colSums(na.rm = TRUE) %>% data.frame('Total' = .)
            
            merge(medal_counts, weighted_tally, by = 'row.names') %>% 
                arrange(desc(Total)) %>% 
                summarize(across(everything(), ~ifelse(is.na(.x), 0, .x))) %>% 
                gt(rowname_col = 'Row.names') %>% 
                fmt_number(everything(), decimals = 2) %>% 
                tab_header('', subtitle = '3 points awarded for a 1st place vote, 
                           2 points for a 2nd place vote, 1 point for a 3rd place vote, 
                           0 points for anything else.') %>%  
                tab_style(style = cell_borders(sides = 'left'),
                          locations = cells_body(columns = Total,
                                                 rows = everything()
                                                 
                          )) %>% 
                tab_style(style = cell_text(align = 'center'),
                          locations = list(cells_body(),
                                           cells_column_labels())) %>% 
                cols_label(V1 = html('1st Place<br>Votes'),
                           V2 = html('2nd Place<br>Votes'),
                           V3 = html('3rd Place<br>Votes'),
                           Total = html('Total<br>Score'))
            
        })
        
        # The overall classification
        output$overall <- render_gt({
            # Add up the votes
            overall_tally <- colSums(votes)
            average_tally <- colMeans(votes)
            
            # Create the dataframe
            data.frame('Restaurant' = restaurant_names,
                       'Average' = unname(average_tally[restaurant_names]),
                       'TotalPoints' = unname(overall_tally[restaurant_names])) %>%
                arrange(desc(TotalPoints)) %>% 
                gt(rowname_col = 'Restaurant') %>% 
                tab_header(title = '', 
                           subtitle = 'The total number of points to each restaurant') %>% 
                cols_label(Average = 'Average Score',
                           TotalPoints = 'Total Score') %>% 
                tab_style(style = cell_text(align = 'center'),
                          locations = list(cells_body(),
                                           cells_column_labels()))
        })
        
        output$overall_winner <- renderText({
            # Add up the votes
            total_tally <- colSums(votes)
            names(total_tally[which.max(total_tally)])
            
        })
        
        # The gold medalists
        output$pure_winner <- render_gt({
            # Get the number of golds
            golds_tally <- apply(votes, 1, FUN = function(this_row) {
                restaurant_names[which.max(this_row)]
            }) %>% table()
            
            # Create the dataframe
            data.frame('Restaurant' = restaurant_names,
                       'Golds' = unname(golds_tally[restaurant_names])) %>%
                select(Restaurant, Golds = Golds.Freq) %>% 
                arrange(desc(Golds)) %>% 
                mutate(Golds = ifelse(is.na(Golds), 0, Golds)) %>% 
                gt(rowname_col = 'Restaurant') %>% 
                tab_header(title = '', 
                           subtitle = 'The total number of first place votes for each restaurant') %>% 
                cols_label(Golds = 'Tally') %>% 
                tab_style(style = cell_text(align = 'center'),
                          locations = list(cells_body(),
                                           cells_column_labels()))
        })
        
        ###### PERSON ANALYSIS
        ## The correlation table
        output$cor_table <- render_gt({
            # Return a table of correlations (between people)
            cor_table <- t(votes) %>% 
                cor(use = 'complete.obs') %>% 
                round(3) %>% 
                as.data.frame()
            
            # Change the column and row names
            names(cor_table) <- people_names
            row.names(cor_table) <- people_names
            
            # Output the table
            cor_table %>% 
                gt(rownames_to_stub = TRUE) %>% 
                tab_header('Person-to-person voting correlations (how similarly you vote to others)') %>% 
                tab_style(style = cell_text(align = 'center'),
                          locations = list(cells_body(),
                                           cells_column_labels()))
        })
        
        limitRange <- function(data, mapping, ...) { 
            ggplot(data = data, mapping = mapping, ...) + 
                geom_point(...) + 
                # geom_smooth(method = "lm", se = FALSE) +
                scale_y_continuous(limits = c(0, 10)) +
                scale_x_continuous(limits = c(0, 10)) 
        }
        
        # This is the pairs plot (plot of each voting)
        output$pairs_plot <- renderPlot({
            # Create a graph
            to_graph <- votes %>%
                t() %>%
                as.data.frame() %>% 
                rownames_to_column('Restaurant') %>% 
                mutate(Restaurant = as.factor(Restaurant))
            
            ggpairs(to_graph, columns = 2:ncol(to_graph),
                    lower = list(continuous = limitRange))
            
        })
        
    })
    
    #### End of server script
})
