library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(lubridate)
library(DT)
library(grid)
library(scales)
library(usmap)
library(reshape2)

# read in and clean data
setwd("/Users/nice/Documents/CS424/Project1")
all_elec <- read.table(file = "annual_generation_state.csv", sep = ",", header = TRUE) 
# convert strings into numeric values
all_elec$GENERATION..Megawatthours. <- as.numeric(gsub(",", "", all_elec$GENERATION..Megawatthours.))
# remove empty rows
clean_elec <- subset(all_elec, STATE != "  ")
# match US Totals 
clean_elec$STATE <- toupper(clean_elec$STATE)
# factor after cleaning code
clean_elec$STATE <- factor(clean_elec$STATE)
clean_elec$TYPE.OF.PRODUCER <-factor(clean_elec$TYPE.OF.PRODUCER)
clean_elec$ENERGY.SOURCE <- factor(clean_elec$ENERGY.SOURCE)
# keep nonnegative values
clean_elec <- subset(clean_elec, GENERATION..Megawatthours. >= 0)
# remove extra energy sources and refactor
clean_elec <- subset(clean_elec, ENERGY.SOURCE != "Other" & ENERGY.SOURCE != "Other Gases" &
                     ENERGY.SOURCE != "Other Biomass" & ENERGY.SOURCE != "Pumped Storage")
clean_elec$ENERGY.SOURCE <- factor(clean_elec$ENERGY.SOURCE)
# simplify names of energy sources
levels(clean_elec$ENERGY.SOURCE)[levels(clean_elec$ENERGY.SOURCE)=="Hydroelectric Conventional"] <- "Hydro"
levels(clean_elec$ENERGY.SOURCE)[levels(clean_elec$ENERGY.SOURCE)=="Solar Thermal and Photovoltaic"] <- "Solar"
levels(clean_elec$ENERGY.SOURCE)[levels(clean_elec$ENERGY.SOURCE)=="Wood and Wood Derived Fuels" ] <- "Wood"
# keep only one type of producer
simple_elec <- subset(clean_elec, clean_elec$TYPE.OF.PRODUCER=="Total Electric Power Industry")

# https://stackoverflow.com/questions/34568417/r-nested-aggregate
# specify data for U.S. 
us_energy <- subset(simple_elec, STATE=="US-TOTAL")
us_energy2 <- subset(simple_elec, STATE=="US-TOTAL" & ENERGY.SOURCE != "Total")

# combine data as aggregate to graph later
energy_sources <- aggregate(GENERATION..Megawatthours. ~ YEAR + ENERGY.SOURCE, us_energy, sum)
energy_sources2 <- aggregate(GENERATION..Megawatthours. ~ YEAR + ENERGY.SOURCE, us_energy2, sum)
# collect total in order to generate percent 
total_source <- subset(energy_sources, ENERGY.SOURCE == "Total")
energy_sources$GENERATION.PERCENT<-round(energy_sources$GENERATION..Megawatthours./total_source$GENERATION..Megawatthours.,2)
energy_sources2$GENERATION.PERCENT<-round(energy_sources2$GENERATION..Megawatthours./total_source$GENERATION..Megawatthours.,2)

# for mapping, add fips column and remove non-states
map_energy <- subset(aggregate(GENERATION..Megawatthours. ~ YEAR + STATE + ENERGY.SOURCE, simple_elec, sum), STATE!="DC" & STATE!="US-TOTAL")
map_energy$STATE <- factor(map_energy$STATE)
map_energy$fips <- fips(map_energy$STATE)

# mapping list that converts state name to abbreviation
states <- c(state.name, "Washington D.C.", "United States")
abb <- c(state.abb, "DC", "US-TOTAL")
states_map <- c(states=abb)

# mapping list that converts state abbreviation to name
abb1 <- c(state.abb, "U.S.")
state1 <- c(state.name, "United States")
abb_map <- c(state1=abb1)

# for checkbox input and select input
# collect order of all energies
all_energies <- c("All", "Coal", "Geothermal", "Hydro", "Natural Gas", "Nuclear", "Petroleum", "Solar", "Wind", "Wood", "Total")
# collect order of Illinois energies
il_energies <- c("All", "Coal", "Hydro", "Natural Gas", "Nuclear", "Petroleum", "Solar", "Wind", "Wood", "Total")
# remove "All" and "Total" from energy sources
n_energies <- c("Coal", "Geothermal", "Hydro", "Natural Gas", "Nuclear", "Petroleum", "Solar", "Wind", "Wood")
# collect all years of data 
all_years <- c("All")
all_years <- c(all_years, paste(1990:2019))
n_years <- c(paste(1990:2019))

# function to generate the type of scale for graphs
generationScale <- function(sReactive, sReactive2){
  e_source <- sReactive()
  e_source2 <- sReactive2()
  # evaluate max and return formatting
  maxVal <- max(c(e_source$GENERATION..Megawatthours..x, e_source2$GENERATION..Megawatthours..x))
  if( maxVal > 999999999){
    return(unit_format(unit="B", scale=1e-9))
  }
  else if(maxVal <= 999999999 & (maxVal > 999999)){
    return(unit_format(unit="M", scale=1e-6))
  }
  else{
    return(comma)
  }
}

# function to generate the top limit for graph
maxLimit <- function(sReactive, sReactive2){
  e_source <- sReactive()
  e_source2 <- sReactive2()
  return( max(c(e_source$GENERATION..Megawatthours..x, e_source2$GENERATION..Megawatthours..x)))
}

# color palette https://jacksonlab.agronomy.wisc.edu/2016/05/23/15-level-colorblind-friendly-palette/
geothermal <- "#004949"
petroleum <- "#ffb6db"
solar <- "#b6dbff"
wood <- "#490092" 
hydro <-"#ff6db6"
wind <- "#920000" 
nuclear <- "#006ddb"
natural <- "#b66dff"
coal <- "#009292"
total <- "#db6d00"

# color order to map
color_all <- c(geothermal, petroleum, wood, solar, wind, hydro, nuclear, natural, coal, total)
color_e <- c(geothermal, petroleum, wood, solar, wind, hydro, nuclear, natural, coal)

order <- c("Geothermal", "Petroleum","Wood", "Solar", "Wind","Hydro", "Nuclear", "Natural Gas", "Coal", "Total")
order2 <- c("Geothermal", "Petroleum","Wood", "Solar", "Wind","Hydro", "Nuclear", "Natural Gas", "Coal")

# final color map
colorm <- c("Coal" = coal, "Geothermal" = geothermal, "Hydro" = hydro, "Natural Gas" = natural, 
            "Nuclear" = nuclear, "Petroleum" = petroleum, "Solar" = solar, "Total" = total, "Wind" = wind, "Wood" = wood)

# shiny dashboard layout
ui <- dashboardPage(
  dashboardHeader(title="CS 424 Project 1"),
  dashboardSidebar(
    sidebarMenu(
      # three pages for project 1 
      menuItem("U.S. Electricity Generation", tabName = "page1", icon = icon("flag-usa")),
      menuItem("State Electricity Generation", tabName = "page2", icon = icon("dashboard")),
      menuItem("About", tabName = "about", icon = icon("question-circle"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "page1",
              fluidRow(
                column(8, 
                       fluidRow(
                         tabBox(
                           title = "Stacked Bar Charts for Electrical Power Generation in the U.S.", side = "right",
                           # The id lets us use input$tabset1 on the server to find the current tab
                           id = "tabset1", height = "325px", selected = "Value",
                           tabPanel("Percent", plotOutput("plot2", height="250px")),
                           tabPanel("Value", plotOutput("plot1", height="250px")),
                           width = NULL
                         )
                       ), # end bar graph
                       fluidRow(
                         tabBox(
                           title = "Line Plots for Electrical Power Generation in the U.S.", side = "right",
                           # The id lets us use input$tabset1 on the server to find the current tab
                           id = "tabset2", height = "275px", selected = "Value",
                           tabPanel("Percent", plotOutput("plot4", height="225px"), 
                                    fluidRow(
                                      
                                      column(width=10, checkboxGroupInput("choice2", "Choose Energy Source:", inline=TRUE, 
                                                                            choices =
                                                                              list("Coal", "Geothermal", "Hydro", "Natural Gas", "Nuclear",
                                                                                   "Petroleum", "Solar", "Wind", "Wood")
                                      ),
                                      column(width = 1, checkboxInput("iall2", "All", TRUE, width='10px'))
                                      )
                                     
                                    )
                            ),
                           tabPanel("Value", plotOutput("plot3", height="225px"), 
                                    fluidRow(
                                      
                                      column(width=10, checkboxGroupInput("choice1", "Choose Energy Source:", inline=TRUE, 
                                                                          choices =
                                                                            list("Coal", "Geothermal", "Hydro", "Natural Gas", "Nuclear",
                                                                                 "Petroleum", "Solar", "Wind", "Wood", "Total")
                                      ),
                                      column(width = 1, checkboxInput("iall1", "All", TRUE, width='10px'))
                                      )
                                      
                                    )
                            ),
                           width = NULL
                         )
                       ) # end line graph
                  ), # end column with line and bar graph
                
                tabBox(
                  title = "Raw Numbers for Electrical Power Generation", side = "right",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset3", height = "600px", selected = "Value",
                  tabPanel("Percent", dataTableOutput("tab2", height = 600)),
                  tabPanel("Value", dataTableOutput("tab1", height = 600)),
                  width = 4)
                ) # end of overall row
              ), # end U.S. level tab 
      tabItem(tabName = "page2",
              fluidRow(
                column(3, offset=1, selectInput("loc1", "Select a Region/State:", choices=states, selected="United States")),
                column(3, selectInput("en1", "Select an Energy Source:", choices=all_energies, selected="All")),
                column(3, selectInput("yr1", "Select a Year:", choices=all_years, selected="All")),
                column(width = 1, checkboxInput("hm1", "Activate Heat Map", FALSE))
              ),
              fluidRow(id = "plotGrid1",
                column(6,box(width=NULL,height="125px", plotOutput("plot5", height="110px")),box(width=NULL,height="125px", plotOutput("plot6", height="110px"))),
                column(6,box(width=NULL,height="125px", plotOutput("plot7", height="110px")),box(width=NULL,height="125px", plotOutput("plot8", height="110px")))
              ),
              hidden(fluidRow(id = "mapSpace1",
                       column(12,box(width=6,height="250px", plotOutput("map1", height="240px")),box(width=6,height="250px", plotOutput("map2", height="240px")))
                       )
              ),
              fluidRow(
                column(3, offset=1, selectInput("loc2", "Select a Region/State:", choices=states, selected="Illinois")),
                column(3,  selectInput("en2", "Select an Energy Source:", choices=il_energies, selected="All")),
                column(3, selectInput("yr2", "Select a Year:", choices=all_years, selected="All"))
              ),
              fluidRow(id="plotGrid2",
                column(6, box(width=NULL,height="125px", plotOutput("plot9", height="110px")),box(width=NULL,height="125px", plotOutput("plot10", height="110px"))),
                column(6,box(width=NULL,height="125px", plotOutput("plot11", height="110px")),box(width=NULL,height="125px", plotOutput("plot12", height="110px")))
              ),
              hidden(fluidRow(id = "mapSpace2",
                              column(12,box(width=6,height="250px", plotOutput("map3", height="240px")),box(width=6,height="250px", plotOutput("map4", height="240px")))
                              )
              )
              
              ), # end of STATE level tab
      tabItem(tabName = "about",
              box(title="About the Data", solidHeader=TRUE, HTML("The data is provided by the U.S. Energy Information Administration. 
                  The data collects information about eletrical power generation within the United States from 1990-2019. 
                  The link to the data can be found here: https://www.eia.gov/electricity/data/state/ <br/><br/> 
                  The data contains the following attributes: <br/>
                                                                 <b>YEAR:</b> the year the data was collected, <br/>
                                                                 <b>STATE:</b> the state/region the data was collected in, <br/>
                                                                 <b>TYPE.OF.PRODUCER</b>: the type of electrical power producer, <br/> 
                                                                 <b>ENERGY.SOURCE:</b> the type of energy source, <br/>
                                                                 <b>GENERATION..Megawatthours.:</b> the amount of generation")),
              box(title="About the Application", solidHeader=TRUE, HTML("This application was written by Yazmin Reyes. Its objective is to visualize electrical power generation across the United States from 1990-2020. It was written in R and Shiny with the assistance of the following: <br/>
                                                                        <b>Professor Andrew Johnson's Shiny App Example:</b> https://www.evl.uic.edu/aej/424/evlWeatherForR.zip <br/>
                                                                        <b>Shiny Dashboard Library:</b> https://rstudio.github.io/shinydashboard/ <br/>
                                                                        <b>R Documentation (shinyjs):</b> https://www.rdocumentation.org/packages/shinyjs/versions/0.6 <br/>
                                                                        <b>R Documentation (plot_usmap):</b> https://www.rdocumentation.org/packages/usmap/versions/0.5.2/topics/plot_usmap <br/>
                                                                        <b>R Documentation (ggplot2):</b> https://www.rdocumentation.org/packages/ggplot2/versions/3.3.3 <br/>
                                                                        References to <b>Stack Overflow</b> and <b>Piazza</b> for specific R/Shiny/data usage <br/><br/>
                                                                        Last Revised: 2/12/2021"))
      ) # end of ABOUT tab
    )
  )
)

server <- function(input, output, session) { 
  theme_set(theme_grey(base_size = 12)) 
  # check inputs for reactive plotting for plots 3 and 4 
  esourceReactive1 <- reactive({subset(energy_sources, ENERGY.SOURCE %in% input$choice1)})
  esourceReactive2 <- reactive({subset(energy_sources, ENERGY.SOURCE %in% input$choice2)})
  
  # update UI according to available data for page 2 comparison page
  observeEvent(input$loc1,{
    req(!input$hm1)
    updateSelectInput(session, "en1", choices = c("All", levels(factor(subset(simple_elec, STATE==states_map[states==input$loc1])$ENERGY.SOURCE))),
                      selected = "All")
    updateSelectInput(session, "yr1", choices = c("All", levels(factor(subset(simple_elec, STATE==states_map[states==input$loc1])$YEAR))),
                      selected = "All")
  }, ignoreInit = TRUE)
  
  observeEvent(input$loc2,{
    req(!input$hm1)
    updateSelectInput(session, "en2", choices = c("All", levels(factor(subset(simple_elec, STATE==states_map[states==input$loc2])$ENERGY.SOURCE))),
                      selected = "All")
    updateSelectInput(session, "yr2", choices = c("All", levels(factor(subset(simple_elec, STATE==states_map[states==input$loc2])$YEAR))),
                      selected = "All")
  }, ignoreInit = TRUE)
 
   observeEvent(input$en1,{
    req(!input$hm1)
    updateSelectInput(session, "yr1", choices = 
                      if(input$en1!="All"){c("All", levels(factor(subset(simple_elec, ENERGY.SOURCE==input$en1 & STATE==states_map[states==input$loc1])$YEAR)))}
                      else{c("All", levels(factor(subset(simple_elec, STATE==states_map[states==input$loc1])$YEAR)))},
                      selected = "All")
  }, ignoreInit = TRUE)
 
  observeEvent(input$en2,{
    req(!input$hm1)
    updateSelectInput(session, "yr2", choices = 
                      if(input$en1!="All"){c("All", levels(factor(subset(simple_elec, ENERGY.SOURCE==input$en2 & STATE==states_map[states==input$loc2])$YEAR)))}
                      else{c("All", levels(factor(subset(simple_elec, STATE==states_map[states==input$loc2])$YEAR)))},
                      selected = "All")
  }, ignoreInit = TRUE)
 
   # checks to make sure heat map isn't on and generates dataframe to plot according to location, energy, and year
   stateReactive1 <- reactive({ 
    req(!input$hm1)
    tot <- subset(simple_elec, (STATE %in% states_map[states==input$loc1]))
    tot <- aggregate(GENERATION..Megawatthours. ~ YEAR + ENERGY.SOURCE, tot, sum)
    specific <- subset(tot, (ENERGY.SOURCE %in% if(input$en1 == "All"){levels(energy_sources$ENERGY.SOURCE)}else{input$en1}) &
                         (YEAR %in% if(input$yr1 == "All"){c(1990:2019)}else{input$yr1}))
    tot <- subset(tot, ENERGY.SOURCE == "Total")
    merge(specific, tot, by = "YEAR")
  })
  
   # checks to make sure heat map isn't on and generates dataframe to plot according to location, energy, and year
  stateReactive2 <- reactive({ 
    req(!input$hm1)
    tot <- subset(simple_elec, (STATE %in% states_map[states==input$loc2]))
    tot <- aggregate(GENERATION..Megawatthours. ~ YEAR + ENERGY.SOURCE, tot, sum)
    specific <- subset(tot, (ENERGY.SOURCE %in% if(input$en2 == "All"){levels(energy_sources$ENERGY.SOURCE)}else{input$en2}) &
                         (YEAR %in% if(input$yr2 == "All"){c(1990:2019)}else{input$yr2}))
    tot <- subset(tot, ENERGY.SOURCE == "Total")
    merge(specific, tot, by = "YEAR")
    })
  
  # checks to make sure heat map mode is on to make changes in map with energy and year 
  mapReactive1 <- reactive({
    req(input$hm1)
    filtered <- subset(map_energy, ENERGY.SOURCE == input$en1 & YEAR == input$yr1)
    tot <- subset(map_energy, ENERGY.SOURCE == "Total" & YEAR == input$yr1)
    merge(filtered, tot, by = "fips")
  })
  
  # checks to make sure heat map mode is on to make changes in map with energy and year 
  mapReactive2 <- reactive({
    req(input$hm1)
    filtered <- subset(map_energy, ENERGY.SOURCE == input$en2 & YEAR == input$yr2)
    tot <- subset(map_energy, ENERGY.SOURCE == "Total" & YEAR == input$yr2)
    merge(filtered, tot, by = "fips")    
  })
  
  # checks if heat map mode was triggered on/off to update UI accordingly
  observeEvent(input$hm1,{
    if(input$hm1){
      updateSelectInput(session, "loc1", choices = states, selected = "United States")
      shinyjs::disable(id = "loc1")
      updateSelectInput(session, "loc2", choices = states, selected = "United States")
      shinyjs::disable(id = "loc2")
      
      shinyjs::hide(id="plotGrid1")
      shinyjs::show(id="mapSpace1")
      shinyjs::hide(id="plotGrid2")
      shinyjs::show(id="mapSpace2")
      
      updateSelectInput(session, "en1", choices = n_energies, selected = "Coal")
      updateSelectInput(session, "yr1", choices = n_years)
      updateSelectInput(session, "en2", choices = n_energies, selected = "Hydro")
      updateSelectInput(session, "yr2", choices = n_years)
    }
    else{
      updateSelectInput(session, "loc2", choices = states, selected = "United States")
      shinyjs::enable("loc1")
      updateSelectInput(session, "loc2", choices = states, selected = "Illinois")
      shinyjs::enable("loc2")
      
      shinyjs::hide(id="mapSpace1")
      shinyjs::show(id="plotGrid1")
      shinyjs::hide(id="mapSpace2")
      shinyjs::show(id="plotGrid2")
      
      updateSelectInput(session, "en1", choices = all_energies, selected = "All")
      updateSelectInput(session, "yr1", choices = all_years, selected = "All")
      updateSelectInput(session, "en2", choices = il_energies, selected = "All")
      updateSelectInput(session, "yr2", choices = all_years, selected = "All")
    }

  }, ignoreInit = TRUE)
  
  # page 1 plot
  output$plot1 <- renderPlot({
    # https://stackoverflow.com/questions/9563368/create-stacked-barplot-where-each-stack-is-scaled-to-sum-to-100
    ggplot(energy_sources2, aes(x=YEAR, y=GENERATION..Megawatthours., fill=factor(ENERGY.SOURCE, levels = order))) + geom_bar(stat="identity") + labs(x = "Year", y = "Generation (MWh)") +  
      scale_y_continuous(labels = unit_format(unit="B", scale=1e-9)) + coord_cartesian(ylim = c(0e9, 4.1e9)) + scale_fill_manual(values = colorm, name = "Energy Source Type") 
  })
  
  # page 1 plot
  output$plot2 <- renderPlot({
    ggplot(energy_sources2, aes(x=YEAR, y=GENERATION.PERCENT, fill=factor(ENERGY.SOURCE, levels = order2))) + geom_bar( stat="identity") + labs(x = "Year", y = "% of Total Generation (MWh)") +
      scale_y_continuous(labels = scales::percent_format()) + scale_fill_manual(values = colorm, name = "Energy Source Type") + coord_cartesian(ylim=c(0, 1))
  })
  
  # page 1 plot
  output$plot3 <- renderPlot({
    e_source <- esourceReactive1()
    # update UI if either "All" or the checkbox input was triggered
    observeEvent(input$iall1,{
      updateCheckboxGroupInput(session, "choice1", "Choose Energy Source:", inline=TRUE, selected = if(input$iall1){NULL}else{input$choice1},
                               choices =
                                 list("Coal", "Geothermal", "Hydro", "Natural Gas", "Nuclear",
                                      "Petroleum", "Solar", "Wind", "Wood", "Total"))
    }, ignoreInit = TRUE)
    
    observeEvent(input$choice1,{
      updateCheckboxInput(session, "iall1", "All", value = if(!is.null(input$choice1)){FALSE}else{TRUE})
    }, ignoreInit = TRUE)
    
    if(input$iall1){
      e_source <- energy_sources
    }
    ggplot(e_source, aes(x=YEAR, y=GENERATION..Megawatthours., group=ENERGY.SOURCE, color=ENERGY.SOURCE)) + geom_line() + labs(x = "Year", y = "Generation (MWh)") + scale_color_manual(name = "Energy Source Type", values = colorm )  +  
      scale_y_continuous(labels = unit_format(unit="B", scale=1e-9))  
  })
  
  # page 1 plot
  output$plot4 <- renderPlot({
    e_source <- esourceReactive2()
    # update UI if either "All" or the checkbox input was triggered
    observeEvent(input$iall2,{
      updateCheckboxGroupInput(session, "choice2", "Choose Energy Source:", inline=TRUE, selected = if(input$iall2){NULL}else{input$choice2},
                               choices =
                                 list("Coal", "Geothermal", "Hydro", "Natural Gas", "Nuclear",
                                      "Petroleum", "Solar", "Wind", "Wood"))
    }, ignoreInit = TRUE)
    
    observeEvent(input$choice2,{
      updateCheckboxInput(session, "iall2", "All", value = if(!is.null(input$choice2)){FALSE}else{TRUE})
    }, ignoreInit = TRUE)
    
   if(input$iall2){
     e_source <- energy_sources
   }
    e_source <- subset(e_source, ENERGY.SOURCE != "Total")
    ggplot(e_source, aes(x=YEAR, y=GENERATION.PERCENT, group=ENERGY.SOURCE, color=ENERGY.SOURCE)) + geom_line() + labs(x = "Year", y = "% of Total Generation") + 
      scale_color_manual(name = "Energy Source Type", values = colorm)  +  scale_y_continuous(labels = scales::percent_format()) 
  })
  
  # page 2 plot
  output$plot5 <- renderPlot({
    req(!input$hm1)
    e_source <- stateReactive1()
    e_source <- subset(e_source, ENERGY.SOURCE.x != "Total")
    ggplot(e_source, aes(x=as.Date(as.character(YEAR), "%Y"), y=GENERATION..Megawatthours..x, fill=factor(ENERGY.SOURCE.x, levels = order))) + geom_bar(stat="identity") + labs(x = "Year", y = "Generation (MWh)", title = paste("Electrical Power Generation Amount by Source -", input$loc1)) +  
      scale_y_continuous(labels = generationScale(stateReactive1, stateReactive2)) + scale_x_date(date_labels="%Y") + scale_fill_manual(values = colorm, name = "Energy Source Type") + 
      theme(legend.text = element_text(size=8), legend.key.size = unit(0.5, "line"), legend.title = element_text(size=8),
            axis.title.y=element_text(size=8), axis.title.x=element_text(size=8), plot.title=element_text(size=9)) + coord_cartesian(ylim = c(0, maxLimit(stateReactive1, stateReactive2)))
    
  })
  
  # page 2 plot
  output$plot6 <- renderPlot({
    req(!input$hm1)
    e_source <- stateReactive1()
    e_source <- subset(e_source, ENERGY.SOURCE.x != "Total")
    e_source$GENERATION.PERCENT <- round(e_source$GENERATION..Megawatthours..x/e_source$GENERATION..Megawatthours..y,2)
    ggplot(e_source, aes(x=as.Date(as.character(YEAR), "%Y"), y=GENERATION.PERCENT, fill=factor(ENERGY.SOURCE.x, levels = order2))) + geom_bar( stat="identity") + labs(x = "Year", y = paste("% of", abb_map[state1==input$loc1], "Generation"), title = paste("Source % of Total Electrical Power Generation -", input$loc1)) +
      scale_y_continuous(labels = scales::percent_format()) + scale_x_date(date_labels="%Y") + scale_fill_manual(values = colorm, name = "Energy Source Type") +
      theme(legend.text = element_text(size=8), legend.key.size = unit(0.5, "line"), legend.title = element_text(size=8),
            axis.title.y=element_text(size=8), axis.title.x=element_text(size=8), plot.title=element_text(size=9)) + coord_cartesian(ylim=c(0, 1))
    
  })
  
  # page 2 plot
  output$plot7 <- renderPlot({
    req(!input$hm1)
    e_source <- stateReactive1()
    # e_source <- subset(e_source, ENERGY.SOURCE.x != "Total")
    if(nrow(e_source)==1 | input$yr1 != "All"){
      ggplot(e_source, aes(x=as.Date(as.character(YEAR), "%Y"), y=GENERATION..Megawatthours..x, group=ENERGY.SOURCE.x, color=ENERGY.SOURCE.x)) + geom_point(shape=19, size=3) + labs(x = "Year", y = "Generation (MWh)", title = paste("Electrical Power Generation Amount by Source -", input$loc1)) + 
        scale_color_manual(name = "Energy Source Type", values = colorm )  +  scale_y_continuous(labels = generationScale(stateReactive1, stateReactive2))  + scale_x_date(date_labels="%Y") +
        theme(legend.text = element_text(size=8), legend.key.size = unit(0.5, "line"), legend.title = element_text(size=8),
              axis.title.y=element_text(size=8), axis.title.x=element_text(size=8), plot.title=element_text(size=9)) + coord_cartesian(ylim = c(0, maxLimit(stateReactive1, stateReactive2)))
    }
    else{
    ggplot(e_source, aes(x=as.Date(as.character(YEAR), "%Y"), y=GENERATION..Megawatthours..x, group=ENERGY.SOURCE.x, color=ENERGY.SOURCE.x)) + geom_line() + labs(x = "Year", y = "Generation (MWh)", title = paste("Electrical Power Generation Amount by Source -", input$loc1)) + 
        scale_color_manual(name = "Energy Source Type", values = colorm )  +  scale_y_continuous(labels = generationScale(stateReactive1, stateReactive2)) + scale_x_date(date_labels="%Y") +
        theme(legend.text = element_text(size=8), legend.key.size = unit(0.5, "line"), legend.title = element_text(size=8),
              axis.title.y=element_text(size=8), axis.title.x=element_text(size=8), plot.title=element_text(size=9)) + coord_cartesian(ylim = c(0, maxLimit(stateReactive1, stateReactive2)))
   }
  })
  
  # page 2 plot
  output$plot8 <- renderPlot({
    req(!input$hm1)
    e_source <- stateReactive1()
    e_source$GENERATION.PERCENT <- round(e_source$GENERATION..Megawatthours..x/e_source$GENERATION..Megawatthours..y,2)
    e_source <- subset(e_source, ENERGY.SOURCE.x != "Total")
    if(nrow(e_source)==1 || input$yr1 != "All"){
      ggplot(e_source, aes(x=as.Date(as.character(YEAR), "%Y"), y=GENERATION.PERCENT, group=ENERGY.SOURCE.x, color=ENERGY.SOURCE.x)) + geom_point(shape=19, size=3) + labs(x = "Year", y = paste("% of", abb_map[state1==input$loc1], "Generation"), title = paste("Source % of Total Electrical Power Generation -", input$loc1)) + 
        scale_color_manual(name = "Energy Source Type", values = colorm)  + coord_cartesian(ylim = c(0, 1)) + scale_y_continuous(labels = scales::percent_format()) + scale_x_date(date_labels="%Y") + 
        theme(legend.text = element_text(size=8), legend.key.size = unit(0.5, "line"), legend.title = element_text(size=8),
              axis.title.y=element_text(size=8), axis.title.x=element_text(size=8), plot.title=element_text(size=9)) + coord_cartesian(ylim=c(0, 1))
    }
    else{
    ggplot(e_source, aes(x=as.Date(as.character(YEAR), "%Y"), y=GENERATION.PERCENT, group=ENERGY.SOURCE.x, color=ENERGY.SOURCE.x)) + geom_line() + labs(x = "Year", y = paste("% of", abb_map[state1==input$loc1], "Generation"), title = paste("Source % of Total Electrical Power Generation -", input$loc1)) + 
      scale_color_manual(name = "Energy Source Type", values = colorm)  +  scale_y_continuous(labels = scales::percent_format()) + scale_x_date(date_labels="%Y") +  
        theme(legend.text = element_text(size=8), legend.key.size = unit(0.5, "line"), legend.title = element_text(size=8),
              axis.title.y=element_text(size=8), axis.title.x=element_text(size=8), plot.title=element_text(size=9)) + coord_cartesian(ylim=c(0, 1))
  }
  })
  
  # page 2 plot
  output$plot9 <- renderPlot({
    req(!input$hm1)
    e_source <- stateReactive2()
    e_source <- subset(e_source, ENERGY.SOURCE.x != "Total")
    ggplot(e_source, aes(x=as.Date(as.character(YEAR), "%Y"), y=GENERATION..Megawatthours..x, fill=factor(ENERGY.SOURCE.x, levels = order))) + geom_bar(stat="identity") + labs(x = "Year", y = "Generation (MWh)", title = paste("Electrical Power Generation Amount by Source -", input$loc2)) +  
      scale_y_continuous(labels =generationScale(stateReactive1, stateReactive2)) +  scale_x_date(date_labels="%Y") + scale_fill_manual(values = colorm, name = "Energy Source Type") +
      theme(legend.text = element_text(size=8), legend.key.size = unit(0.5, "line"), legend.title = element_text(size=8),
            axis.title.y=element_text(size=8), axis.title.x=element_text(size=8), plot.title=element_text(size=9)) + coord_cartesian(ylim = c(0, maxLimit(stateReactive1, stateReactive2)))
  })
  
  # page 2 plot
  output$plot10 <- renderPlot({
    req(!input$hm1)
    e_source <- stateReactive2()
    e_source <- subset(e_source, ENERGY.SOURCE.x != "Total")
    e_source$GENERATION.PERCENT <- round(e_source$GENERATION..Megawatthours..x/e_source$GENERATION..Megawatthours..y,2)
    ggplot(e_source, aes(x=as.Date(as.character(YEAR), "%Y"), y=GENERATION.PERCENT, fill=factor(ENERGY.SOURCE.x, levels = order2))) + geom_bar( stat="identity") + labs(x = "Year", y = paste("% of", abb_map[state1==input$loc2], "Generation"), title = paste("Source % of Total Electrical Power Generation -", input$loc2)) +
      scale_y_continuous(labels = scales::percent_format()) + scale_x_date(date_labels="%Y") + scale_fill_manual(values = colorm, name = "Energy Source Type") +
      theme(legend.text = element_text(size=8), legend.key.size = unit(0.5, "line"), legend.title = element_text(size=8),
            axis.title.y=element_text(size=8), axis.title.x=element_text(size=8), plot.title=element_text(size=9)) + coord_cartesian(ylim=c(0, 1))
  })
  
  # page 2 plot
  output$plot11 <- renderPlot({
    req(!input$hm1)
    e_source <- stateReactive2()
    if(nrow(e_source)==1 | input$yr2 != "All"){
      ggplot(e_source, aes(x=as.Date(as.character(YEAR), "%Y"), y=GENERATION..Megawatthours..x, group=ENERGY.SOURCE.x, color=ENERGY.SOURCE.x)) + geom_point(shape=19, size=3) + labs(x = "Year", y = "Generation (MWh)", title = paste("Electrical Power Generation Amount by Source -", input$loc2)) + 
        scale_color_manual(name = "Energy Source Type", values = colorm )  +  scale_y_continuous(labels = generationScale(stateReactive1, stateReactive2)) + scale_x_date(date_labels="%Y") +
        theme(legend.text = element_text(size=8), legend.key.size = unit(0.5, "line"), legend.title = element_text(size=8),
              axis.title.y=element_text(size=8), axis.title.x=element_text(size=8), plot.title=element_text(size=9)) + coord_cartesian(ylim = c(0, maxLimit(stateReactive1, stateReactive2)))
    }
    else{
    ggplot(e_source, aes(x=as.Date(as.character(YEAR), "%Y"), y=GENERATION..Megawatthours..x, group=ENERGY.SOURCE.x, color=ENERGY.SOURCE.x)) + geom_line() + labs(x = "Year", y = "Generation (MWh)", title = paste("Electrical Power Generation Amount by Source -", input$loc2)) + 
        scale_color_manual(name = "Energy Source Type", values = colorm )  +  scale_y_continuous(labels = generationScale(stateReactive1, stateReactive2)) + scale_x_date(date_labels="%Y") +
        theme(legend.text = element_text(size=8), legend.key.size = unit(0.5, "line"), legend.title = element_text(size=8),
              axis.title.y=element_text(size=8), axis.title.x=element_text(size=8), plot.title=element_text(size=9)) + coord_cartesian(ylim = c(0, maxLimit(stateReactive1, stateReactive2)))
      
    }
  })
  
  # page 2 plot
  output$plot12 <- renderPlot({
    req(!input$hm1)
    e_source <- stateReactive2()
    e_source$GENERATION.PERCENT <- round(e_source$GENERATION..Megawatthours..x/e_source$GENERATION..Megawatthours..y,2)
    e_source <- subset(e_source, ENERGY.SOURCE.x != "Total")
    if(nrow(e_source)==1 | input$yr2 != "All"){
      ggplot(e_source, aes(x=as.Date(as.character(YEAR), "%Y"), y=GENERATION.PERCENT, group=ENERGY.SOURCE.x, color=ENERGY.SOURCE.x)) + geom_point(shape=19, size=3) + labs(x = "Year", y = paste("% of", abb_map[state1==input$loc2], "Generation"), title = paste("Source % of Total Electrical Power Generation -", input$loc2)) + 
        scale_color_manual(name = "Energy Source Type", values = colorm)  +  scale_y_continuous(labels = scales::percent_format()) + scale_x_date(date_labels="%Y") +
        theme(legend.text = element_text(size=8), legend.key.size = unit(0.5, "line"), legend.title = element_text(size=8),
              axis.title.y=element_text(size=8), axis.title.x=element_text(size=8), plot.title=element_text(size=9)) + coord_cartesian(ylim=c(0, 1))
    }
    else{
    ggplot(e_source, aes(x=as.Date(as.character(YEAR), "%Y"), y=GENERATION.PERCENT, group=ENERGY.SOURCE.x, color=ENERGY.SOURCE.x)) + geom_line() + labs(x = "Year", y = paste("% of", abb_map[state1==input$loc2], "Generation"), title = paste("Source % of Total Electrical Power Generation -", input$loc2)) + 
      scale_color_manual(name = "Energy Source Type", values = colorm)  +  scale_y_continuous(labels = scales::percent_format()) + scale_x_date(date_labels="%Y") +
        theme(legend.text = element_text(size=8), legend.key.size = unit(0.5, "line"), legend.title = element_text(size=8),
              axis.title.y=element_text(size=8), axis.title.x=element_text(size=8), plot.title=element_text(size=9)) + coord_cartesian(ylim=c(0, 1))
      
    }
  })
  
  # page 2 map
  output$map1 <- renderPlot({
    req(input$hm1)
    map_data <- mapReactive1()
    plot_usmap(region="states", data=map_data, values = "GENERATION..Megawatthours..x") + labs(title = paste(input$en1, "Electrical Generation Amount per State in", input$yr1)) + 
      scale_fill_continuous(low = "white", high=colorm[input$en1], name = paste(input$en1, "Generation (MWh)"), label = scales::comma) +
      theme(legend.position = "right", legend.background=element_rect( fill = "grey90"), panel.background = element_rect( fill = "grey90")) 
  })
  
  # page 2 map
  output$map2 <- renderPlot({
    req(input$hm1)
    map_data <- mapReactive1()
    map_data$GENERATION.PERCENT <- round(map_data$GENERATION..Megawatthours..x/map_data$GENERATION..Megawatthours..y,2)
    plot_usmap(region="states", data=map_data, values = "GENERATION.PERCENT") + labs(title = paste(input$en1, "% of Total Electrical Generation per State in", input$yr1)) + 
      scale_fill_continuous(low = "white", high=colorm[input$en1], name = paste(input$en1, "Generation (%)"),  limits = c(0,1), label = scales::percent_format()) +
      theme(legend.position = "right", legend.background=element_rect( fill = "grey90"), panel.background = element_rect( fill = "grey90"))
  })
  
  # page 2 map
  output$map3 <- renderPlot({
    req(input$hm1)
    map_data <- mapReactive2()
    plot_usmap(region="states", data=map_data, values = "GENERATION..Megawatthours..x") + labs(title = paste(input$en2, "Electrical Generation Amount per State in", input$yr2)) + 
      scale_fill_continuous(low = "white", high=colorm[input$en2], name = paste(input$en2, "Generation (MWh)"), label = scales::comma 
                            ) +
      theme(legend.position = "right", legend.background=element_rect( fill = "grey90"), panel.background = element_rect( fill = "grey90"))
  })
  
  # page 2 map
  output$map4 <- renderPlot({
    req(input$hm1)
    map_data <- mapReactive2()
    map_data$GENERATION.PERCENT <- round(map_data$GENERATION..Megawatthours..x/map_data$GENERATION..Megawatthours..y,2)
    plot_usmap(region="states", data=map_data, values = "GENERATION.PERCENT") + labs(title = paste(input$en2, "% of Total Electrical Generation per State in", input$yr2)) + 
      scale_fill_continuous(low = "white", high=colorm[input$en2], name = paste(input$en2, "Generation (%)"), limits = c(0,1), label = scales::percent_format()) +
      theme(legend.position = "right", legend.background=element_rect( fill = "grey90"), panel.background = element_rect( fill = "grey90"))
  })
  
  # page 1 table
  output$tab1 <- DT::renderDataTable(
    DT::datatable({    percent_energy <- subset(energy_sources, select = -c(GENERATION.PERCENT))
                       val <- comma(percent_energy$GENERATION..Megawatthours.)
                       percent_energy$GEN <- val
                       percent_energy <- subset(percent_energy, select = -c(GENERATION..Megawatthours.))
    }, 
    options = list(searching = FALSE, pageLength = 12, lengthChange = FALSE, order = list(list(0, 'asc')),
                   columns = list(
                     list(title = 'Year'),
                     list(title = 'Energy Source'),
                     list(title = 'Generation (MWh)')),
                   columnDefs = list(
                     list(className = 'dt-left', targets = 0:1),
                     list(className = 'dt-right', targets = 2)
                   )
                   
    ), rownames = FALSE, 
    ) 
  )
  
  # page 1 table
  output$tab2 <- DT::renderDataTable(
    DT::datatable({ 
      percent_energy <- subset(energy_sources, select = -c(GENERATION..Megawatthours.))
      val <- label_percent(accuracy=1)(c(percent_energy$GENERATION.PERCENT))
      percent_energy$GEN <- val
      percent_energy <- subset(percent_energy, select = -c(GENERATION.PERCENT))
    }, 
    options = list(searching = FALSE, pageLength = 12, lengthChange = FALSE, order = list(list(0, 'asc')),
                   columns = list(
                     list(title = 'Year'),
                     list(title = 'Energy Source'),
                     list(title = '% of Total Generation')),
                   columnDefs = list(
                     list(className = 'dt-left', targets = 0:1),
                     list(className = 'dt-right', targets = 2)
                     )
                   
    ), rownames = FALSE, 
    )
  )
}

shinyApp(ui, server)

  