install.packages(shiny)
install.packages(trackeR)

rider1 <- readTCX(file ='F:/Data Analytics using R/Project1/Num3.tcx', timezone = "GMT")
rider2 <- trackeRdata(rider1, units = NULL, cycling = T, sessionThreshold = 2,
                      correctDistances = FALSE, country = NULL, mask = TRUE,
                      fromDistances = TRUE, lgap = 30, lskip = 5, m = 11)
rider2 <- as.data.frame(rider2)
runTr1 <- readContainer('F:/Data Analytics using R/Project1/Rider/rider1.tcx', type = "tcx", timezone = "GMT")
runTr2 <- readContainer('F:/Data Analytics using R/Project1/Rider/rider2.tcx', type = "tcx", timezone = "GMT")

library(shiny)
library(trackeR)
data(runs)


#Defines the server-side logic of the Shiny application. 
#This generally involves creating functions that map user inputs to various kinds of output. 
#In older versions of Shiny, it was necessary to call shinyServer() in the server.
#R file, but this is no longer required as of Shiny 0.10. Now the server.R file may simply return the appropriate server function (as the last expression in the code), without calling shinyServer().
shinyServer(function(input, output) {
  # a large table, reactive to input$show_vars
  #Historically this function was used in ui.R files to register a user interface with Shiny. 
  #It is no longer required as of Shiny 0.10; simply ensure that the last expression to be returned from ui.
  #R is a user interface. This function is kept for backwards compatibility with older applications. 
  #It returns the value that is passed to it.
  #this function takes input form the ui page provided by the user.
  output$mytable1 = renderDataTable({
    as.data.frame(runs)[, input$show_vars1, drop = FALSE]
  })
  # sorted columns are colored now because CSS are attached to them
  # In the second datatable for rider1 we provided options to sort the columns
  output$mytable2 = renderDataTable({
    as.data.frame(rider1)
  }, options = list(bSortClasses = TRUE))
  # customize the length drop-down menu; display 5 rows per page by default
  output$mytable3 = renderDataTable({
    as.data.frame(rider2)
    # In the third dataframe, we included options to diplay custom number of results rather than default values.
    # User defined options are 5, 30 or 50 records at a single point of time.
  }, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  
  
  ################
  #Wraps a normal expression to create a reactive expression. 
  #Conceptually, a reactive expression is a expression whose result will change over time.
  #This takes from session input from the UI page provided by the user.
  fromdatasetinput <- reactive({
    switch(input$session1,
           "1"=1,
           "2"=2,
           "3"=3,
           "4"=4,
           "5"=5,
           "6"=6,
           "7"=7,
           "8"=8,
           "9"=9,
           "10"=10, "11"=11,"12"=12,"13"=13,"14"=14,"15"=15,"16"=16,"17"=17,"18"=18,"19"=19,"20"=20)
  })
  #This takes to session input from the UI page provided by the user.
  todatasetinput <- reactive({
    switch(input$session2,
           "1"=1,
           "2"=2,
           "3"=3,
           "4"=4,
           "5"=5,
           "6"=6,
           "7"=7,
           "8"=8,
           "9"=9,
           "10"=10, "11"=11,"12"=12,"13"=13,"14"=14,"15"=15,"16"=16,"17"=17,"18"=18,"19"=19,"20"=20)
  })
  #Renders a reactive plot that is suitable for assigning to an output slot.
  output$plot <- renderPlot({print(plot(runs,session = fromdatasetinput():todatasetinput(), what = as.list(input$show_vars2)))
  })
  
  ##############
  # This takes input from map tab such as the for which activity session we want to see the map route
  mapinput <- reactive({
    switch(input$session3,
           "1"=1,
           "2"=2,
           "3"=3,
           "4"=4,
           "5"=5,
           "6"=6,
           "7"=7,
           "8"=8,
           "9"=9,
           "10"=10, "11"=11,"12"=12,"13"=13,"14"=14,"15"=15,"16"=16,"17"=17,"18"=18,"19"=19,"20"=20)
  })
  #Use leafletOutput() to create a UI element, and renderLeaflet() to render the map widget.
  # Plot the route ran/cycled during training on an interactive map. 
  #Internet connection is required to download the background map. 
  #Icons are by Maps Icons Collection
  output$main_plot <- renderLeaflet({    
    leafletRoute(runs, session = mapinput())})
  
  ################
  
  # Server code to render the summary of the cycling or running data in the form a data frame
  summaryinput1 <- reactive({
    switch(input$session4,
           "1"=1,
           "2"=2,
           "3"=3,
           "4"=4,
           "5"=5,
           "6"=6,
           "7"=7,
           "8"=8,
           "9"=9,
           "10"=10,"11"=11,"12"=12,"13"=13,"14"=14,"15"=15,"16"=16,"17"=17,"18"=18,"19"=19,"20"=20)
  })
  summaryinput2 <- reactive({
    switch(input$session4a,
           "1"=1,
           "2"=2,
           "3"=3,
           "4"=4,
           "5"=5,
           "6"=6,
           "7"=7,
           "8"=8,
           "9"=9,
           "10"=10,"11"=11,"12"=12,"13"=13,"14"=14,"15"=15,"16"=16,"17"=17,"18"=18,"19"=19,"20"=20)
  })
  
  # Show the values using an HTML table
  # Makes a reactive version of the given function that returns a data frame (or matrix),
  #which will be rendered with the DataTables library. Paging, searching, filtering, 
  #and sorting can be done on the R side using Shiny as the server infrastructure.
  output$summary <- renderDataTable({
    summary(runs, session = summaryinput1():summaryinput2(), movingThreshold = input$decimal )
  })
  
  ##########################
  
  workinput1 <- reactive({
    switch(input$session5,
           "1"=1,
           "2"=2,
           "3"=3,
           "4"=4,
           "5"=5,
           "6"=6,
           "7"=7,
           "8"=8,
           "9"=9,
           "10"=10, "11"=11,"12"=12,"13"=13,"14"=14,"15"=15,"16"=16,"17"=17,"18"=18,"19"=19,"20"=20)
  })
  
  workinput2 <- reactive({
    switch(input$cps,
           "1"=1,
           "2"=2,
           "3"=3,
           "4"=4,
           "5"=5,
           "6"=6,
           "7"=7)
  })
  output$wplot <- renderPlot({print(plot(Wprime(runs, session = workinput1(), quantity = "expended", cp = workinput2(), version = "2012"), scaled = TRUE))
  })
  
  ##############
  # renderplot to show the summary of the rider
  
  output$sumplot <- renderPlot({
    plot(summary(runs), group = input$show_vars3,
         what = input$splot)
  })
  
  ###############
  
  #Sever page to plot concentration profiles
  
  cprofinput1 <- reactive({
    switch(input$session6,
           "1"=1,
           "2"=2,
           "3"=3,
           "4"=4,
           "5"=5,
           "6"=6,
           "7"=7,
           "8"=8,
           "9"=9,
           "10"=10, "11"=11,"12"=12,"13"=13,"14"=14,"15"=15,"16"=16,"17"=17,"18"=18,"19"=19,"20"=20)
  })
  cprofinput2 <- reactive({
    switch(input$session7,
           "1"=1,
           "2"=2,
           "3"=3,
           "4"=4,
           "5"=5,
           "6"=6,
           "7"=7,
           "8"=8,
           "9"=9,
           "10"=10, "11"=11,"12"=12,"13"=13,"14"=14,"15"=15,"16"=16,"17"=17,"18"=18,"19"=19,"20"=20)
  })
  # Thresholding for variables in trackeRdata objects using Thresold function
  # Generate training distribution profiles using distributionProfile function.
  # Generic function for smoothing. Used to clear off the noise the data.
  # Generate training concentration profiles, so that we can view in which running speed or heart beat rate, user is running or cycling
  # Used plot function to plo the graphs
  output$cprofplot <- renderPlot({
    print(
      plot(
        concentrationProfile(
          smoother(
            distributionProfile(
              threshold(runs), 
              session = cprofinput1():cprofinput2(),
              what = input$cprof),
            cores =2)),
        multiple = TRUE, smooth = FALSE))
  })
  
  
  ###########################
  
  
  perfinput1 <- reactive({
    switch(input$session8,
           "1"=1,
           "2"=2,
           "3"=3,
           "4"=4,
           "5"=5,
           "6"=6,
           "7"=7,
           "8"=8,
           "9"=9,
           "10"=10, "11"=11,"12"=12,"13"=13,"14"=14,"15"=15)
  })
  perfinput2 <- reactive({
    switch(input$session9,
           "1"=1,
           "2"=2,
           "3"=3,
           "4"=4,
           "5"=5,
           "6"=6,
           "7"=7,
           "8"=8,
           "9"=9,
           "10"=10, "11"=11,"12"=12,"13"=13,"14"=14,"15"=15)
  })
  output$perfplot1 <- renderPlot({
    plot(zones(runTr1[perfinput1():perfinput2()], what = "speed", breaks = c(0, 2:6, 12.5)))
  })
  output$perfplot2 <- renderPlot({
    plot(zones(runTr2[perfinput1():perfinput2()], what = "speed", breaks = c(0, 2:6, 12.5)))
  })
  
})