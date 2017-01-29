install.packages(shiny)
install.packages(trackeR)

library(shiny)
library(trackeR)
data(runs)

# Using Shiny package to visualize the data and make it more interactive
# navbarPage is an application layout which is in UI file to make an webpage
#Historically this function was used in ui.R files to register a user interface with Shiny. 
#It is no longer required as of Shiny 0.10; simply ensure that the last expression to be returned from ui.R is a user interface. 
#This function is kept for backwards compatibility with older applications. It returns the value that is passed to it.
navbarPage("TrackeR Package",
           tabPanel("Data",  #tabpanel is used to make tabs in the webpage to divide the layout into mutiple tabs
                    sidebarLayout(  #Sidebar layout provides intial left hand layout for creating different widgets
                      sidebarPanel( #Different widgets such as radio buttons, slider bars will be created here in the sidebar panel
                        checkboxGroupInput('show_vars1', 'Columns in runs to show:', names(as.data.frame(runs)),
                                           selected = names(as.data.frame(runs))), 
                        #CheckboxGroupInput is a widget which takes in mutiple inputs when we check the options created here
                        helpText('For the rider1 data, we can select variables to show in the table;
                                 for the rider2 example, we use bSortClasses = TRUE so that sorted
                                 columns are colored since they have special CSS classes attached;
                                 for the iris data, we customize the length menu so we can display 5
                                 rows per page.')
                        #helpText function is used write any text where the progrmmer can provide more details to 
                        #user about the interaction of the application
                      ),
                      mainPanel( # main panel is where the output of the various funtions which are executed in the server page will be showed
                        tabsetPanel( #tabset panel is used to divide the output mail page panel in multiple tabs
                          tabPanel('Runs Dataset',
                                   dataTableOutput("mytable1")),  # dataTableOutput function shows the output of a dataframe as a table output which is more interactive with search functions in each column and for the entire dataset
                          tabPanel('Before Creating Sessions',
                                   dataTableOutput("mytable2")),
                          tabPanel('After Creating Sessions',
                                   dataTableOutput("mytable3"))
                        ) )
                    )),
           tabPanel("Plots", 
                    headerPanel("Plots showing different metrics"), #Header Panel function dispalys the heading which is writeen in it.
                    sidebarLayout( 
                      sidebarPanel(
                        #Create a sidebar panel containing input controls that can in turn be passed to sidebarLayout.
                        #Create a select list that can be used to choose a single or multiple items from a list of values.
                        #inputId	-- The input slot that will be used to access the value.
                        #label	-- Display label for the control, or NULL for no label.
                        #choices	-- List of values to select from. If elements of the list are named then that name rather than the value is displayed to the user.
                        #selected--The initially selected value (or multiple values if multiple = TRUE). If not specified then defaults to the first value for single-select lists and no values for multiple select lists.
                        selectInput("session1", "From Activity Session:",
                                    c("1","2","3","4","5","6","7","8","9","10",
                                      "11","12","13","14","15","16","17","18","19","20"),selected = 1),
                        #selectInput function creates a drop down list with the options provided here
                        selectInput("session2", "To Activity Session:",
                                    c("1","2","3","4","5","6","7","8","9","10",
                                      "11","12","13","14","15","16","17","18","19","20"),selected = 1),
                        #Create a group of checkboxes that can be used to toggle multiple choices independently. The server will receive the input as a character vector of the selected values.
                        #aruguments are similar to selectInput function expalined above
                        checkboxGroupInput('show_vars2', 'Check Plots to show', 
                                           choices = list("Heart Rate"="heart.rate",
                                                          "Altitude"= "altitude", 
                                                          "Pace" = "pace",
                                                          "Latitude" = "latitude",
                                                          "Longitude"= "longtide",
                                                          "Distance"= "distance",
                                                          "Cadence" = "cadence"),
                                           selected = "heart.rate")
                      ),
                      
                      # Show a tabset that includes a plot, summary, and table view
                      # of the generated distribution
                      mainPanel(
                        tabPanel("Plot",plotOutput("plot",height = "850px"))
                      )
                    )),
           
           #Created this tab to show the maps of the a particular session based on the selected session
           tabPanel("Maps",
                    headerPanel("Cycling Route"),
                    sidebarPanel(
                      selectInput("session3", "Activity Session:",
                                  c("1","2","3","4","5","6","7","8","9","10",
                                    "11","12","13","14","15","16","17","18","19","20"),selected = 1),width = 1),  
                    mainPanel(      
                      #Use leafletOutput() to create a UI element, and renderLeaflet() to render the map widget
                      leafletOutput(outputId = "main_plot", width="135%",height = 880)
                    )),
           
           #created Summary tab to show the summary of each session in the form a datatable based on the sessions selected
           tabPanel("Summary",
                    headerPanel("Summary of Sessions"),
                    
                    # Sidebar with sliders that demonstrate various available
                    # options
                    sidebarLayout(
                      sidebarPanel(
                        #created select input function to take the from and to activity sessions
                        selectInput("session4", "From Activity Session:",
                                    c("1","2","3","4","5","6","7","8","9","10",
                                      "11","12","13","14","15","16","17","18","19","20"),selected = 1),
                        selectInput("session4a", "To Activity Session:",
                                    c("1","2","3","4","5","6","7","8","9","10",
                                      "11","12","13","14","15","16","17","18","19","20"),selected = 1),
                        
                        # Decimal interval with step value
                        #Constructs a slider widget to select a numeric value from a range.
                        # inputId	--The input slot that will be used to access the value.
                        #label	-- Display label for the control, or NULL for no label.
                        #min	-- The minimum value (inclusive) that can be selected.
                        #max	-- The maximum value (inclusive) that can be selected.
                        #animate	-- TRUE to show simple animation controls with default settings; FALSE not to; or a custom settings list, such as those created using animationOptions.
                        #interval	-- The interval, in milliseconds, between each animation step.
                        #loop	-- TRUE to automatically restart the animation when it reaches the end.
                        sliderInput("decimal", "Moving Threshold:",
                                    min = 0.5, max = 5, value = 0.5, step= 0.1, animate=
                                      animationOptions(interval=300, loop=FALSE))
                      ),
                      # Show a table summarizing the values entered
                      mainPanel(
                        dataTableOutput("summary")
                      )
                    )),
           
           # Create Summary Plots tab panel to show the plots of average heart rate , average speed, distance, duration 
           # for either total or moving
           tabPanel("Summary Plots",
                    headerPanel("Summary Plots for entire sessions"),
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons("splot", "Summary type:",
                                     c("Average Heart Rate" = "avgHeartRate",
                                       "Average Speed" = "avgSpeed",
                                       "Distance" = "distance",
                                       "Duration" = "duration")),
                        checkboxGroupInput('show_vars3', 'Type Plots to show', 
                                           choices = list("Total" = "total",
                                                          "Moving" = "moving"),
                                           selected = "total")
                      ),
                      
                      # Show a tabset that includes a plot, summary, and table view
                      # of the generated distribution
                      mainPanel(
                        plotOutput("sumplot",width = "90%",height = "650px")
                      )
                    )),
           
           # Created 
           tabPanel("W Prime",
                    headerPanel("Qunatifying Work Capacity"),
                    # Sidebar with controls to select the random distribution type
                    # and number of observations to generate. Note the use of the
                    # br() element to introduce extra vertical spacing
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("session5", "Activity Session:",
                                    c("1","2","3","4","5","6","7","8","9","10",
                                      "11","12","13","14","15","16","17","18","19","20"),selected = 1),
                        selectInput("cps","Critical power/speed ratio:",
                                    c("1","2","3","4","5","6","7"),selected = 1)
                      ),
                      
                      # Show a tabset that includes a plot, summary, and table view
                      # of the generated distribution
                      mainPanel(
                        plotOutput("wplot",width = "90%",height = "750px")
                      )
                    )),
           
          
           tabPanel("Concentration Profiles",
                    headerPanel("Plots of Concentration Profiles"),
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("session6", "From Activity Session:",
                                    c("1","2","3","4","5","6","7","8","9","10",
                                      "11","12","13","14","15","16","17","18","19","20"),selected = 1),
                        
                        selectInput("session7", "To Activity Session:",
                                    c("1","2","3","4","5","6","7","8","9","10",
                                      "11","12","13","14","15","16","17","18","19","20"),selected = 1),
                        # radioButtons fuction creates a radio button where it can accomadate multiple options and select one option at a time unlike chekcboxInput function
                        radioButtons("cprof", "Profile:",
                                     c("Speed" = "speed",
                                       "Heart Rate" = "heart.rate"
                                     ))),
                      mainPanel(
                        plotOutput("cprofplot",width = "90%",height = "750px"))
                    )),
           
           
           tabPanel("Comparing Speed Performance of Athlets",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("session8", "From Activity Session:",
                                    c("1","2","3","4","5","6","7","8","9","10",
                                      "11","12","13","14","15"),selected = 1),
                        
                        selectInput("session9", "To Activity Session:",
                                    c("1","2","3","4","5","6","7","8","9","10",
                                      "11","12","13","14","15"),selected = 1)
                      ),
                      mainPanel(
                        plotOutput("perfplot1",width = "90%",height = "450px"),
                        plotOutput("perfplot2",width = "90%",height = "450px")
                      )
                    ))
           
        )