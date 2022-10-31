
library(shiny)
library(tidyverse)
library(gt)

shinyUI(fluidPage(
  
  navbarPage('Duncan Taste Test App',
             ######## WELCOME ########
             tabPanel('Welcome',
                      h3('Welcome!'),
                      br(),
                      p("A favorite Duncan activity when the family is together is to
                          do a blind taste test where every family member gets to rank the food items.
                          A simple voting system can capture most of the fun, but why stop there?
                          The inner statistician in me thinks we can have some more fun!"),
                      hr(),
                      h3('The General Idea:'),
                      p(strong('Step 1: Setup'), '\nSpecify the test adminstrator (the family member who will know which labeled food will come from where) and enter the number and names of participants and food items.'),
                      br(),
                      p(strong('Step 2: Voting'), '\nEach participant will give each food item a score between 1 and 10.'),
                      br(),
                      p(strong('Step 3: Results'), '\nView the results! Some things we\'ll look at:'),
                      br(),
                      p('Who in your group you vote most similarly to, different ways of measuring the champion, which votes were outliers, etc.')
             ),
             ######## SETUP ########
             tabPanel('Step 1: Setup',
                      
                      # Application title
                      titlePanel("Taste Tester App"),
                      
                      h3('Instructions:'),
                      br(),
                      p("Specify the person in the party to be the adminstrator. This person can participate, but will know which randomized food items come from which restaurant (i.e. that chicken sandwich A comes from Wendy's and chicken sandwich B comes from Chick Fil A). This person will enter (secretly) which food items are to which restaurant."),
                      p('Also enter the number and names of participants and food items. You can do a max of 20 people and 11 food items.
                        Note that once you enter in the names and food items and start voting, changing the number of people or the food items will require you to re-enter votes.'),
                      
                      # The sliding panel
                      fluidRow(
                        column(width = 6,
                               sliderInput("num_people",
                                           "Number of People:",
                                           min = 2,
                                           max = 20,
                                           value = 4,
                                           step = 1)
                        ), column(6,
                                  sliderInput("num_food_items",
                                              "Number of Food Items:",
                                              min = 2,
                                              max = 11,
                                              value = 4,
                                              step = 1)
                        )
                      ),
                      br(), hr(),
                      
                      # Get the names of the participants and food items
                      splitLayout(
                        # The left half
                        fluidRow(
                          uiOutput('names_entering')
                        ),
                        
                        # The right half
                        fluidRow(
                          uiOutput('admin_food_key')
                        )
                      )
             ),
             
             ######## THE VOTING ########
             tabPanel('Step 2: Voting',
                      p('After entering the names of the participants (and the admin types in the key), next is the best part--EAT!! As you eat and vote on whatever dish you taste testing, keep these things in mind:'),
                      tags$li('Keep your votes between 0-10.'),
                      tags$li('Let 0 mark your least favorite, and 10 represent your most favorite.'),
                      tags$li('The number score you give a restaurant also has to do with the ranking. For example, I might give Food A a 6.5 and Food B a 7. You like them both alright, but you would rank Food B over food A. These rankings are important for the different categories a restaurant can win.'),
                      tags$li('You are not the only thing being judged LOL. but it\'s all fun and games ya know?'),
                      p('Once each person has had a taste, enter in the scores below for each person. After entering the votes, press the "Submit Votes" button at the bottom of this page.'),
                      fluidRow(column(width = 12,
                                      uiOutput('voting_top_row'),
                                      br(),
                                      uiOutput('voting')
                      )),
                      column(8, p('Once you\'ve submitted the votes, move onto the next page to find out what restaurant won and some other fun things!'),
                      ),
                      column(1,
                             actionButton('run_results', 'Submit Votes',
                                          style = 'background-color: #001af6; color: #ffffff')
                      )
             ),
             
             ######## RESULTS ########
             tabPanel('Step 3: Results',
                      fluidRow(
                        h2('The Great Reveal', style = 'text-align: center;'),
                        p('Below, on the left is the key--what letters correspond to what restaurant. On the right is each persons votes, ranked from highest to lowest.'),
                        # ,
                        tags$div( # FIXME
                          column(width = 12,
                                 column(width = 6,
                                        gt_output('food_key')),
                                 column(width = 6,
                                        gt_output('each_person_rank')),
                          ),
                          style = 'margin-bottom:50px;'
                        ),
                        br(), HTML('<hr>'),
                        
                        # br(), hr(style = "border-top: 1px solid #000000;"),
                        h2('The Scoring', style = 'text-align: center;'),
                        h3('The overall winner is: ', textOutput('overall_winner'), style = 'text-align: center;'),
                        column(width = 12,
                               column(width = 4, 
                                      h3('WEIGHTED AVERAGE', style = 'text-align: center;'),
                                      gt_output('weighted_rank')),
                               column(width = 4,
                                      h3('OVERALL CLASSIFICATION', style = 'text-align: center;'),
                                      gt_output('overall')),
                               column(width = 4,
                                      h3('GOLD CHASER', style = 'text-align: center;'),
                                      gt_output('pure_winner')),
                        ),
                        
                        br(), hr(style = "border-top: 1px solid #000000;"),
                        h2('Person Analysis', style = 'text-align: center;'),
                        p('You\'re not the only thing being judged here :o'),
                        column(width = 12,
                               column(width = 6,
                                      gt_output("cor_table"), style = 'text-align: center;'),
                               column(width = 6,
                                      plotOutput("pairs_plot"), style = 'text-align: center;')
                        )
                      )
             )
  )
  
))
