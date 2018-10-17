#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)

# Define UI for application that draws a histogram
dashboardPage( dashboardHeader(title='EducationNC',titleWidth = 500),
               dashboardSidebar(
                 sidebarMenu( menuItem("Main",tabName = 'Main',icon=icon("th")),
                              menuItem("Grant Goals", tabName = "goals", icon = icon("signal")),
                              menuItem("Loyalty", icon = icon("child"), tabName = "loyalty",
                                        badgeColor = "green")
                 )#end sidebarmenu
                 ),#endsidebar

               dashboardBody(
                 tabItems(
                   tabItem(tabName = 'Main',
                           fluidRow(
                             box(sidebarMenu(menuItem("Voting",tabName = 'voting',icon=icon("thumbs-up",lib='glyphicon')
                                                      ),
                                             menuItem("Articles",tabName = 'articles',icon=icon("book",lib='glyphicon')
                                             ),
                                             menuItem("Emails",tabName = 'emails',icon=icon("envelope")
                                             )
                                             )#sidebarMenu
                                 ,width=2),#box
                               box(
                               tabItems(tabItem(tabName='voting',
                                                h1('Responses per week'),
                                                fluidRow(column(dateInput('votedate','Date View',value = Sys.Date()-30)
                                                                ,width=6)#col
                                                         
                                                ),
                                                plotlyOutput('votechart')
                                                  ),
                                        tabItem(tabName='articles',
                                                h1('Article Reads'),
                                                fluidRow(column(dateInput('articledate','Date View',value = Sys.Date()-14)
                                                         ,width=6),#col
                                                column(numericInput('timemin','Time Minimum',value = 0)
                                                       ,width=6)#column
                                                ),#fr
                                                fluidRow(column(plotlyOutput('articlereads'),width=12)),
                                                fluidRow(column(dataTableOutput('article_df'),width=12)),
                                                fluidRow(column(h1('Publishing by Date'),p('The top article from 
                                                    each day is looking at reads within the timeframe given.'),width=12)),
                                                fluidRow(column(plotlyOutput('publishdate'),width=12))
                                                
                                        ),
                                        tabItem(tabName='emails',
                                                h1("Emails"),
                                                fluidRow(column(selectInput('emailsegment','Email Segment',c('All','Reach Roundup','Awake58','Other')),width=12)),
                                                plotlyOutput('email_plot'),
                                                dataTableOutput('email_df')
                                                )
                                          ),#tabitems
                                 width=10)#box
                             
                           )#fr
                           ),
                   # First tab content
                   tabItem(tabName = "goals",
                           fluidRow(
                             h1('Goals'),
                             box(
                               h2('Goal 1. Average 50,000 people taking a survey per year.'),
                               h3(textOutput("text_reachvotes")),
                               plotlyOutput("reachvotes"),
                               h2('Goal 2. Reach 15,000 members in Reach NC Voices.'),
                               plotlyOutput("subscribers"),
                               h2('Goal 3. Average 52,500 website visits a month 
                                  from May of this year thru May of next year.'),
                               plotlyOutput("article"),
                               width=12
                             ))
                   ),

                   # Second tab content
                   tabItem(tabName = "loyalty",
                           h2("Loyalty Scores"),
                           fluidRow(
                             column(box(plotOutput("loyaltyplot", height = 500),width=12),
                                    box(dataTableOutput('loyaltydf'),width = 12)
                                    
                                    ,width=8),#col

                             column(
                               fluidRow(box(h2('Loyal Members'),strong(span(textOutput('loyaltyscore')),
                                                        style="font-size:90px"),
                                                        div(HTML("<em>Loyal Members hit the
                                                        threshold for two consecutive months</em>")
                                                        )
                                            )
                                        ),
                               fluidRow(
                                   box(
                                   title = "Weights",
                                   numericInput('article','Article view',value = 1),
                                   numericInput('survey','Survey taken',value = 1),
                                   numericInput('email','Email opened',value = 1),
                                   numericInput('maxscore','Max Score',value=20),
                                   numericInput('loyaltylevel','Loyalty Threshold',value=10),width=12
                                 )#box'
                               )#fr
                             ,width=4)#col

                   )
                 ))
               )#end dashboardBody
)#end dashboardPage
