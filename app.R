# COVID-19 stats tracker--USA version
# 5 April 2020
# Mikaela Springsteen, contactmspringsteen@gmail.com

# including code adapted from
# https://github.com/ceefluz/radar

# packages

if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinyjs)) install.packages("shinyjs", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(shinycssloaders)) install.packages("shinycssloaders", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(withr)) install.packages("withr", repos = "http://cran.us.r-project.org")
if(!require(rintrojs)) install.packages("rintrojs", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(lazyeval)) install.packages("lazyeval", repos = "http://cran.us.r-project.org")

# update data to be used
#source("jhu_data.R")

# import data
covid_cases <- read.csv("covid_cases_usa.csv")
covid_cases <- select(covid_cases, -County)
covid_cases$County <- covid_cases$Combined_Key
covid_cases$Cases <- covid_cases$Totalper100_000

# Shiny ui
ui <- dashboardPage(
  # header
  dashboardHeader(title = "Counting Covid-19: US Counties", titleWidth = 300,
                  
                  tags$li(a(tags$i("*compare infection rates for other regions here*"), href = "https://mikaelaspringsteen.github.io/countingcovid19/"), class = "dropdown"),
                  dropdownMenu(type = "notifications", 
                    icon = icon("question"), 
                    badgeStatus = NULL,
                    headerText = tags$i("Questions? Suggestions? Want to request", tags$br(), 
                                        "a stat be added to the app? Get in touch at", tags$br(),
                                        "contactmspringsteen@gmail.com")),
                  tags$li(a("ABOUT THIS APP", href = "https://github.com/mikaelaspringsteen/counting-covid19-counties/blob/master/README.md"), class = "dropdown")),
  # sidebar
  dashboardSidebar(
    useShinyjs(),
    introjsUI(),
    width = 300,
    tags$br(),
    h5("Select a single filter or combine", align = "center"),
    h5("several to visualize their impact on" , align = "center"),
    h5("tracking the spread of the virus.", align = "center"),
    tags$hr(),
    introBox(data.step = 3, data.intro = "Click here to update graphs with your selections or to reset any highlights set from the controls to the right of the graph.",
    fluidRow(
      column(1, offset = 3,
      actionButton("updategraph", tags$b("Update graph"))
      )
    )
    ),
    introBox(data.step = 2, data.intro = "Selecting variables here will highlight any counties on the graph which match those characteristics.",
    sidebarMenu(
      introBox(data.step = 1, data.intro = "Select states here. You may also isolate the course of the virus in specific counties by double clicking on the county name to the right of the graph. WARNING: increasing the number of states selected will increase the time to load the graph.",
      uiOutput("states")
      ),
      menuItem("Population statistics", tabName = "populationstatistics",
               checkboxInput(
                 inputId = "popcheck", 
                 label = "Population (in hundred thousands)", 
                 value = FALSE
                 ),
               sliderInput(
                 inputId = "popinput",
                 label = NULL,
                 min = floor(min(covid_cases$Population_hundthou, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$Population_hundthou, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$Population_hundthou, na.rm = TRUE)), ceiling(max(covid_cases$Population_hundthou, na.rm = TRUE))),
                 step = 1
                 ),
               checkboxInput(
                 inputId = "hscheck", 
                 label = "% of population with up to a high school education", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "hsinput",
                 label = NULL,
                 min = floor(min(covid_cases$HS, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$HS, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$HS, na.rm = TRUE)), ceiling(max(covid_cases$HS, na.rm = TRUE))),
                 step = 1
               ),
               checkboxInput(
                 inputId = "bacheck", 
                 label = "% of population with at least a BA", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "bainput",
                 label = NULL,
                 min = floor(min(covid_cases$BAorHigher, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$BAorHigher, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$BAorHigher, na.rm = TRUE)), ceiling(max(covid_cases$BAorHigher, na.rm = TRUE))),
                 step = 1
               ),
               checkboxInput(
                 inputId = "malecheck", 
                 label = "% of population which is male", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "maleinput",
                 label = NULL,
                 min = floor(min(covid_cases$Male, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$Male, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$Male, na.rm = TRUE)), ceiling(max(covid_cases$Male, na.rm = TRUE))),
                 step = 1
               ),
               checkboxInput(
                 inputId = "whitecheck", 
                 label = "% of population which is white (with no other ethnicities declared)", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "whiteinput",
                 label = NULL,
                 min = floor(min(covid_cases$WhiteAlone, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$WhiteAlone, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$WhiteAlone, na.rm = TRUE)), ceiling(max(covid_cases$WhiteAlone, na.rm = TRUE))),
                 step = 1
               ),
               checkboxInput(
                 inputId = "blackcheck", 
                 label = "% of population which is black (with no other ethnicities declared)", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "blackinput",
                 label = NULL,
                 min = floor(min(covid_cases$BlackAlone, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$BlackAlone, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$BlackAlone, na.rm = TRUE)), ceiling(max(covid_cases$BlackAlone, na.rm = TRUE))),
                 step = 1
               ),
               checkboxInput(
                 inputId = "asiancheck", 
                 label = "% of population which is Asian (with no other ethnicities declared)", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "asianinput",
                 label = NULL,
                 min = floor(min(covid_cases$AsianAlone, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$AsianAlone, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$AsianAlone, na.rm = TRUE)), ceiling(max(covid_cases$AsianAlone, na.rm = TRUE))),
                 step = 1
               ),
               checkboxInput(
                 inputId = "hispaniccheck", 
                 label = "% of population which is Hispanic", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "hispanicinput",
                 label = NULL,
                 min = floor(min(covid_cases$Hispanic, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$Hispanic, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$Hispanic, na.rm = TRUE)), ceiling(max(covid_cases$Hispanic, na.rm = TRUE))),
                 step = 1
               ),
               checkboxInput(
                 inputId = "multiplecheck", 
                 label = "% of population which declares two or more ethnicities", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "multipleinput",
                 label = NULL,
                 min = floor(min(covid_cases$Multiple, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$Multiple, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$Multiple, na.rm = TRUE)), ceiling(max(covid_cases$Multiple, na.rm = TRUE))),
                 step = 1
               )
      ),
      menuItem("Economic statistics", tabName = "economicstatistics",
               pickerInput(
                 inputId = "econtypeinput", 
                 label = h5("Economic type"),
                 choices = c("Nonspecialized", "Farming", "Mining", "Manufacturing", "Government", "Recreation"),
                 multiple = TRUE, 
                 options = list(`actions-box` = TRUE)
               ),
               pickerInput(
                 inputId = "metroinput", 
                 label = h5("Is in a metropolitan area"),
                 choices = c("Yes", "No"),
                 multiple = TRUE
               ),
               pickerInput(
                 inputId = "urbinfinput", 
                 label = h5("Urban influence code"),
                 choices = c("1 - Large (metro, 1 million+ residents)", "2 - Small (metro, <1 million residents)", "3 - Micropolitan (adjacent to large metro area)", "4 - Noncore (adjacent to large metro area)", "5 - Micropolitan (adjacent to small metro area)", "6 - Noncore (adjacent to small metro, with a town of ≥ 2,500)", "7 - Noncore (adjacent to small metro, with a town of ≤ 2,500)", "8 - Micropolitan (not adjacent to metro area)", "9 - Noncore (adjacent to micro area, with a town of 2,500 to 19,999)", "10 - Noncore (adjacent to micro area, with no town of ≥ 2,500)", "11 - Noncore (not adjacent to micro area, with a town of ≥ 2,500)"),
                 multiple = TRUE, 
                 options = list(`actions-box` = TRUE)
               ),
               checkboxInput(
                 inputId = "povertycheck", 
                 label = "% of population in poverty", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "povertyinput",
                 label = NULL,
                 min = floor(min(covid_cases$Poverty, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$Poverty, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$Poverty, na.rm = TRUE)), ceiling(max(covid_cases$Poverty, na.rm = TRUE))),
                 step = 1
               ),
               checkboxInput(
                 inputId = "unemploymentcheck", 
                 label = "Unemployment rate", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "unemploymentinput",
                 label = NULL,
                 min = floor(min(covid_cases$Unemployment, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$Unemployment, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$Unemployment, na.rm = TRUE)), ceiling(max(covid_cases$Unemployment, na.rm = TRUE))),
                 step = 1
               ),
               checkboxInput(
                 inputId = "incomecheck", 
                 label = "Median household income ($)", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "incomeinput",
                 label = NULL,
                 min = floor(min(covid_cases$HHIncome_med, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$HHIncome_med, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$HHIncome_med, na.rm = TRUE)), ceiling(max(covid_cases$HHIncome_med, na.rm = TRUE))),
                 step = 1
               ),
               checkboxInput(
                 inputId = "ownerocccheck", 
                 label = "% of housing which is owner-occupied", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "owneroccinput",
                 label = NULL,
                 min = floor(min(covid_cases$Owner_Occupied, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$Owner_Occupied, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$Owner_Occupied, na.rm = TRUE)), ceiling(max(covid_cases$Owner_Occupied, na.rm = TRUE))),
                 step = 1
               )
      ),
      menuItem("Health statistics", tabName = "healthstatistics",
               checkboxInput(
                 inputId = "uninsuredcheck", 
                 label = "% of population which is uninsured", 
                 value = FALSE
               ),
               sliderInput(
                 inputId = "uninsuredinput",
                 label = NULL,
                 min = floor(min(covid_cases$Uninsured, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$Uninsured, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$Uninsured, na.rm = TRUE)), ceiling(max(covid_cases$Uninsured, na.rm = TRUE))),
                 step = 1
               )
      )
    )
  )
  ),
  # body
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    introBox(data.step = 4, data.intro = "Switch between tabs to see different Covid-19 metrics. A description of the graph is located below each panel.",
    tabsetPanel(
      tabPanel("Cases",
               introBox(data.step = 5, data.intro = "Each graph is interactive. Hover over points/lines for more information, or find more settings (including a home button to reset axes) at the top right of each graph.",
               fluidRow(column(12, uiOutput("cases_graph")))
               ),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               fluidRow(column(12, helpText("The grey dotted line represents the conditional mean for all counties. The blue represents the mean for the highlighted counties. The light bands represent standard error.", tags$br(),"Cases have been scaled to represent the number of confirmed cases for every 100,000 people in each country, to simplify comparison betweeen counties.", tags$br(), "If a county is not testing many people, this number is probably lower than that county's actual infection rate as mild cases go undetected.")))
      ),
      tabPanel("Deaths",
               fluidRow(column(12, uiOutput("case_fatality_graph"))),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               fluidRow(column(12, helpText("The grey dotted line represents the conditional mean for all counties. The blue represents the mean for the highlighted counties. The light bands represent standard error.", tags$br(), "Also known as the 'case fatality rate' (or CFR), this number is calculated by dividing the number of detected cases by the number of reported deaths.", tags$br(), "If a county is not testing many people the CFR may be artifically high as mild cases go undetected. Inaccurately recording Covid-19 deaths may result in an artificially low CFR.")))
      )
    )
    )
  )
)

# Shiny server
server <- function(input, output, session) {
  # intro message
  observeEvent("", {
    showModal(modalDialog(
      easyClose = TRUE,
      title = tags$b("Counting Covid-19: US Counties"),
      tags$b(tags$i("Please note: this app is based on a very large dataset, and the graphs will take some time to load. Adding states to the graph will further increase load times. Please be patient.")),
      tags$br(),
      tags$hr(),
      tags$b("What we know about the infection or death rate of Covid-19 depends on one thing:"),
      tags$br(),
      tags$b("how good are we at counting the people who have Covid-19?"),
      tags$br(),
      tags$br(),
      "The number of people tested and confirmed to have the virus (usually described as 'cases' of the virus), is lower than the total number of people who have the virus (called 'infections') because not everyone who has the virus will be tested. Some people, especially those with mild symptoms or those unable to access the healthcare system, will not be tested. This difference between the number of 'cases' and the number of 'infections' can vary from region to region, and will impact that region's apparent number of cases and their apparent mortality rate. This is why there is such a range of rates across the country.", "For more, see ", tags$a(href = "https://www.npr.org/sections/goatsandsoda/2020/03/27/821958435/why-death-rates-from-coronavirus-can-be-deceiving", "Why 'Death Rates' From Coronavirus Can Be Deceiving"), "or ", tags$a(href = "https://www.bbc.com/future/article/20200401-coronavirus-why-death-and-mortality-rates-differ", "Coronavirus: Why death and mortality rates differ"), ".",
      tags$br(),
      tags$br(),
      "Testing more people will result in better, more accurate data about Covid-19's infection and mortality rate, but what is it that makes certain regions better at testing than others? Is it money? Something about the population? Their system of healthcare?",
      tags$br(),
      tags$br(),
      tags$b("Exploring what characteristics are associated with increased testing, lower case rates, or lower case fatality rates might help explain what makes some areas better at counting cases of Covid-19 than others."),
      tags$br(),
      tags$hr(),
      tags$i("For information about combating the spread of the virus, or about symptoms and treatment, there are a number of excellent resources run by infectious disease experts and medical professionals, including the ", tags$a(href = "https://www.who.int/emergencies/diseases/novel-coronavirus-2019", "WHO"), "and ", tags$a(href = "https://www.cdc.gov/coronavirus/2019-nCoV/index.html", "CDC"), "for public health information, the ", tags$a(href = "https://www.nih.gov/health-information/coronavirus", "NIH"), "and ", tags$a(href = "https://www.gisaid.org/", "GISAID"), "for research information, and ", tags$a(href = "https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "JHU"), "for data."),
      tags$br(),
      tags$br(),
      footer = tagList(
        actionButton(inputId = "intro", label = tags$b("See how it works")))
    ))
  })
  # start intro tour
  observeEvent(input$intro,{
    removeModal()
  })
  observeEvent(input$intro,
               introjs(session, options = list("nextLabel" = "Continue",
                                               "prevLabel" = "Previous",
                                               "doneLabel" = "Let's go!",
                                               "showStepNumbers" = "false"))
  )
  # general settings
  options(list(scipen = 99))
  # state selection
  output$states <- renderUI({
    stateslist <- unique(as.character(covid_cases$State))
    pickerInput(
      inputId = "statesinput", label = h5("Select states to include in plot"), 
      choices = sort(stateslist), 
      selected = c("New York", "Michigan", "California"),
      multiple = TRUE, 
      options = list(`actions-box` = TRUE)
    )
  })
  # create minimal dataset
  min_covid_case <- reactive({
    select(covid_cases, State, County, Date, Day, Cases, DeathRate) %>%
      filter(State %in% input$statesinput)
  })
  # enable inputs if variable is checked
  observeEvent(input$popcheck, {
    if (input$popcheck == FALSE) {
      disable("popinput")
      updateSliderInput(
        session,
        inputId = "popinput",
        label = NULL,
        min = floor(min(covid_cases$Population_hundthou, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Population_hundthou, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Population_hundthou, na.rm = TRUE)), ceiling(max(covid_cases$Population_hundthou, na.rm = TRUE))),
        step = 1
      )
    } else {
      enable("popinput")
      updateSliderInput(
        session,
        inputId = "popinput",
        label = NULL,
        min = floor(min(covid_cases$Population_hundthou, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Population_hundthou, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Population_hundthou, na.rm = TRUE)), ceiling(max(covid_cases$Population_hundthou, na.rm = TRUE))),
        step = 1
      )
    }
  })
  observeEvent(input$hscheck, {
    if (input$hscheck == FALSE) {
      disable("hsinput")
      updateSliderInput(
        session,
        inputId = "hsinput",
        label = NULL,
        min = floor(min(covid_cases$HS, na.rm = TRUE)),
        max = ceiling(max(covid_cases$HS, na.rm = TRUE)),
        value = c(floor(min(covid_cases$HS, na.rm = TRUE)), ceiling(max(covid_cases$HS, na.rm = TRUE))),
        step = 1
      )
    } else {
      enable("hsinput")
      updateSliderInput(
        session,
        inputId = "hsinput",
        label = NULL,
        min = floor(min(covid_cases$HS, na.rm = TRUE)),
        max = ceiling(max(covid_cases$HS, na.rm = TRUE)),
        value = c(floor(min(covid_cases$HS, na.rm = TRUE)), ceiling(max(covid_cases$HS, na.rm = TRUE))),
        step = 1
      )
    }
  })
  observeEvent(input$bacheck, {
    if (input$bacheck == FALSE) {
      disable("bainput")
      updateSliderInput(
        session,
        inputId = "bainput",
        label = NULL,
        min = floor(min(covid_cases$BAorHigher, na.rm = TRUE)),
        max = ceiling(max(covid_cases$BAorHigher, na.rm = TRUE)),
        value = c(floor(min(covid_cases$BAorHigher, na.rm = TRUE)), ceiling(max(covid_cases$BAorHigher, na.rm = TRUE))),
        step = 1
      )
    } else {
      enable("bainput")
      updateSliderInput(
        session,
        inputId = "bainput",
        label = NULL,
        min = floor(min(covid_cases$BAorHigher, na.rm = TRUE)),
        max = ceiling(max(covid_cases$BAorHigher, na.rm = TRUE)),
        value = c(floor(min(covid_cases$BAorHigher, na.rm = TRUE)), ceiling(max(covid_cases$BAorHigher, na.rm = TRUE))),
        step = 1
      )
    }
  })
  observeEvent(input$ownerocccheck, {
    if (input$ownerocccheck == FALSE) {
      disable("owneroccinput")
      updateSliderInput(
        session,
        inputId = "owneroccinput",
        label = NULL,
        min = floor(min(covid_cases$Owner_Occupied, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Owner_Occupied, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Owner_Occupied, na.rm = TRUE)), ceiling(max(covid_cases$Owner_Occupied, na.rm = TRUE))),
        step = 1
      )
    } else {
      enable("owneroccinput")
      updateSliderInput(
        session,
        inputId = "owneroccinput",
        label = NULL,
        min = floor(min(covid_cases$Owner_Occupied, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Owner_Occupied, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Owner_Occupied, na.rm = TRUE)), ceiling(max(covid_cases$Owner_Occupied, na.rm = TRUE))),
        step = 1
      )
    }
  })
  observeEvent(input$uninsuredcheck, {
    if (input$uninsuredcheck == FALSE) {
      disable("uninsuredinput")
      updateSliderInput(
        session,
        inputId = "uninsuredinput",
        label = NULL,
        min = floor(min(covid_cases$Uninsured, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Uninsured, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Uninsured, na.rm = TRUE)), ceiling(max(covid_cases$Uninsured, na.rm = TRUE))),
        step = 1
      )
    } else {
      enable("uninsuredinput")
      updateSliderInput(
        session,
        inputId = "uninsuredinput",
        label = NULL,
        min = floor(min(covid_cases$Uninsured, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Uninsured, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Uninsured, na.rm = TRUE)), ceiling(max(covid_cases$Uninsured, na.rm = TRUE))),
        step = 1
      )
    }
  })
  observeEvent(input$povertycheck, {
    if (input$povertycheck == FALSE) {
      disable("povertyinput")
      updateSliderInput(
        session,
        inputId = "povertyinput",
        label = NULL,
        min = floor(min(covid_cases$Poverty, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Poverty, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Poverty, na.rm = TRUE)), ceiling(max(covid_cases$Poverty, na.rm = TRUE))),
        step = 1
      )
    } else {
      enable("povertyinput")
      updateSliderInput(
        session,
        inputId = "povertyinput",
        label = NULL,
        min = floor(min(covid_cases$Poverty, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Poverty, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Poverty, na.rm = TRUE)), ceiling(max(covid_cases$Poverty, na.rm = TRUE))),
        step = 1
      )
    }
  })
  observeEvent(input$unemploymentcheck, {
    if (input$unemploymentcheck == FALSE) {
      disable("unemploymentinput")
      updateSliderInput(
        session,
        inputId = "unemploymentinput",
        label = NULL,
        min = floor(min(covid_cases$Unemployment, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Unemployment, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Unemployment, na.rm = TRUE)), ceiling(max(covid_cases$Unemployment, na.rm = TRUE))),
        step = 1
      )
    } else {
      enable("unemploymentinput")
      updateSliderInput(
        session,
        inputId = "unemploymentinput",
        label = NULL,
        min = floor(min(covid_cases$Unemployment, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Unemployment, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Unemployment, na.rm = TRUE)), ceiling(max(covid_cases$Unemployment, na.rm = TRUE))),
        step = 1
      )
    }
  })
  observeEvent(input$incomecheck, {
    if (input$incomecheck == FALSE) {
      disable("incomeinput")
      updateSliderInput(
        session,
        inputId = "incomeinput",
        label = NULL,
        min = floor(min(covid_cases$HHIncome_med, na.rm = TRUE)),
        max = ceiling(max(covid_cases$HHIncome_med, na.rm = TRUE)),
        value = c(floor(min(covid_cases$HHIncome_med, na.rm = TRUE)), ceiling(max(covid_cases$HHIncome_med, na.rm = TRUE))),
        step = 1
      )
    } else {
      enable("incomeinput")
      updateSliderInput(
        session,
        inputId = "incomeinput",
        label = NULL,
        min = floor(min(covid_cases$HHIncome_med, na.rm = TRUE)),
        max = ceiling(max(covid_cases$HHIncome_med, na.rm = TRUE)),
        value = c(floor(min(covid_cases$HHIncome_med, na.rm = TRUE)), ceiling(max(covid_cases$HHIncome_med, na.rm = TRUE))),
        step = 1
      )
    }
  })
  observeEvent(input$malecheck, {
    if (input$malecheck == FALSE) {
      disable("maleinput")
      updateSliderInput(
        session,
        inputId = "maleinput",
        label = NULL,
        min = floor(min(covid_cases$Male, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Male, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Male, na.rm = TRUE)), ceiling(max(covid_cases$Male, na.rm = TRUE))),
        step = 1
      )
    } else {
      enable("maleinput")
      updateSliderInput(
        session,
        inputId = "maleinput",
        label = NULL,
        min = floor(min(covid_cases$Male, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Male, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Male, na.rm = TRUE)), ceiling(max(covid_cases$Male, na.rm = TRUE))),
        step = 1
      )
    }
  })
  observeEvent(input$whitecheck, {
    if (input$whitecheck == FALSE) {
      disable("whiteinput")
      updateSliderInput(
        session,
        inputId = "whiteinput",
        label = NULL,
        min = floor(min(covid_cases$WhiteAlone, na.rm = TRUE)),
        max = ceiling(max(covid_cases$WhiteAlone, na.rm = TRUE)),
        value = c(floor(min(covid_cases$WhiteAlone, na.rm = TRUE)), ceiling(max(covid_cases$WhiteAlone, na.rm = TRUE))),
        step = 1
      )
    } else {
      enable("whiteinput")
      updateSliderInput(
        session,
        inputId = "whiteinput",
        label = NULL,
        min = floor(min(covid_cases$WhiteAlone, na.rm = TRUE)),
        max = ceiling(max(covid_cases$WhiteAlone, na.rm = TRUE)),
        value = c(floor(min(covid_cases$WhiteAlone, na.rm = TRUE)), ceiling(max(covid_cases$WhiteAlone, na.rm = TRUE))),
        step = 1
      )
    }
  })
  observeEvent(input$blackcheck, {
    if (input$blackcheck == FALSE) {
      disable("blackinput")
      updateSliderInput(
        session,
        inputId = "blackinput",
        label = NULL,
        min = floor(min(covid_cases$BlackAlone, na.rm = TRUE)),
        max = ceiling(max(covid_cases$BlackAlone, na.rm = TRUE)),
        value = c(floor(min(covid_cases$BlackAlone, na.rm = TRUE)), ceiling(max(covid_cases$BlackAlone, na.rm = TRUE))),
        step = 1
      )
    } else {
      enable("blackinput")
      updateSliderInput(
        session,
        inputId = "blackinput",
        label = NULL,
        min = floor(min(covid_cases$BlackAlone, na.rm = TRUE)),
        max = ceiling(max(covid_cases$BlackAlone, na.rm = TRUE)),
        value = c(floor(min(covid_cases$BlackAlone, na.rm = TRUE)), ceiling(max(covid_cases$BlackAlone, na.rm = TRUE))),
        step = 1
      )
    }
  })
  observeEvent(input$asiancheck, {
    if (input$asiancheck == FALSE) {
      disable("asianinput")
      updateSliderInput(
        session,
        inputId = "asianinput",
        label = NULL,
        min = floor(min(covid_cases$AsianAlone, na.rm = TRUE)),
        max = ceiling(max(covid_cases$AsianAlone, na.rm = TRUE)),
        value = c(floor(min(covid_cases$AsianAlone, na.rm = TRUE)), ceiling(max(covid_cases$AsianAlone, na.rm = TRUE))),
        step = 1
      )
    } else {
      enable("asianinput")
      updateSliderInput(
        session,
        inputId = "asianinput",
        label = NULL,
        min = floor(min(covid_cases$AsianAlone, na.rm = TRUE)),
        max = ceiling(max(covid_cases$AsianAlone, na.rm = TRUE)),
        value = c(floor(min(covid_cases$AsianAlone, na.rm = TRUE)), ceiling(max(covid_cases$AsianAlone, na.rm = TRUE))),
        step = 1
      )
    }
  })
  observeEvent(input$hispaniccheck, {
    if (input$hispaniccheck == FALSE) {
      disable("hispanicinput")
      updateSliderInput(
        session,
        inputId = "hispanicinput",
        label = NULL,
        min = floor(min(covid_cases$Hispanic, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Hispanic, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Hispanic, na.rm = TRUE)), ceiling(max(covid_cases$Hispanic, na.rm = TRUE))),
        step = 1
      )
    } else {
      enable("hispanicinput")
      updateSliderInput(
        session,
        inputId = "hispanicinput",
        label = NULL,
        min = floor(min(covid_cases$Hispanic, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Hispanic, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Hispanic, na.rm = TRUE)), ceiling(max(covid_cases$Hispanic, na.rm = TRUE))),
        step = 1
      )
    }
  })
  observeEvent(input$multiplecheck, {
    if (input$multiplecheck == FALSE) {
      disable("multipleinput")
      updateSliderInput(
        session,
        inputId = "multipleinput",
        label = NULL,
        min = floor(min(covid_cases$Multiple, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Multiple, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Multiple, na.rm = TRUE)), ceiling(max(covid_cases$Multiple, na.rm = TRUE))),
        step = 1
      )
    } else {
      enable("multipleinput")
      updateSliderInput(
        session,
        inputId = "multipleinput",
        label = NULL,
        min = floor(min(covid_cases$Multiple, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Multiple, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Multiple, na.rm = TRUE)), ceiling(max(covid_cases$Multiple, na.rm = TRUE))),
        step = 1
      )
    }
  })
  # create selected dataset
  selected_covid_case <- reactive({
    popfilter <- quote(between(Population_hundthou, as.numeric(input$popinput[1]), as.numeric(input$popinput[2])))
    hsfilter <- quote(between(HS, as.numeric(input$hsinput[1]), as.numeric(input$hsinput[2])))
    bafilter <- quote(between(BAorHigher, as.numeric(input$bainput[1]), as.numeric(input$bainput[2])))
    owneroccfilter <- quote(between(Owner_Occupied, as.numeric(input$owneroccinput[1]), as.numeric(input$owneroccinput[2])))
    uninsuredfilter <- quote(between(Uninsured, as.numeric(input$uninsuredinput[1]), as.numeric(input$uninsuredinput[2])))
    povertyfilter <- quote(between(Poverty, as.numeric(input$povertyinput[1]), as.numeric(input$povertyinput[2])))
    unemploymentfilter <- quote(between(Unemployment, as.numeric(input$unemploymentinput[1]), as.numeric(input$unemploymentinput[2])))
    incomefilter <- quote(between(HHIncome_med, as.numeric(input$incomeinput[1]), as.numeric(input$incomeinput[2])))
    malefilter <- quote(between(Male, as.numeric(input$maleinput[1]), as.numeric(input$maleinput[2])))
    whitefilter <- quote(between(WhiteAlone, as.numeric(input$whiteinput[1]), as.numeric(input$whiteinput[2])))
    blackfilter <- quote(between(BlackAlone, as.numeric(input$blackinput[1]), as.numeric(input$blackinput[2])))
    asianfilter <- quote(between(AsianAlone, as.numeric(input$asianinput[1]), as.numeric(input$asianinput[2])))
    hispanicfilter <- quote(between(Hispanic, as.numeric(input$hispanicinput[1]), as.numeric(input$hispanicinput[2])))
    multiplefilter <- quote(between(Multiple, as.numeric(input$multipleinput[1]), as.numeric(input$multipleinput[2])))
    covid_cases %>%
      select(
        State, County, Date, Day, Cases, DeathRate, EconType, MetroArea, UrbanInfCode,
        if (input$popcheck == FALSE) {"State"} else {"Population_hundthou"},
        if (input$hscheck == FALSE) {"State"} else {"HS"},
        if (input$bacheck == FALSE) {"State"} else {"BAorHigher"},
        if (input$ownerocccheck == FALSE) {"State"} else {"Owner_Occupied"},
        if (input$uninsuredcheck == FALSE) {"State"} else {"Uninsured"},
        if (input$povertycheck == FALSE) {"State"} else {"Poverty"},
        if (input$unemploymentcheck == FALSE) {"State"} else {"Unemployment"},
        if (input$incomecheck == FALSE) {"State"} else {"HHIncome_med"},
        if (input$malecheck == FALSE) {"State"} else {"Male"},
        if (input$whitecheck == FALSE) {"State"} else {"WhiteAlone"},
        if (input$blackcheck == FALSE) {"State"} else {"BlackAlone"},
        if (input$asiancheck == FALSE) {"State"} else {"AsianAlone"},
        if (input$hispaniccheck == FALSE) {"State"} else {"Hispanic"},
        if (input$multiplecheck == FALSE) {"State"} else {"Multiple"}
      ) %>%
      filter(
        State %in% input$statesinput,
        if (input$popcheck == FALSE) {!is.na(State)} else {!!popfilter},
        if (input$hscheck == FALSE) {!is.na(State)} else {!!hsfilter},
        if (input$bacheck == FALSE) {!is.na(State)} else {!!bafilter},
        if (input$ownerocccheck == FALSE) {!is.na(State)} else {!!owneroccfilter},
        if (input$uninsuredcheck == FALSE) {!is.na(State)} else {!!uninsuredfilter},
        if (input$povertycheck == FALSE) {!is.na(State)} else {!!povertyfilter},
        if (input$unemploymentcheck == FALSE) {!is.na(State)} else {!!unemploymentfilter},
        if (input$incomecheck == FALSE) {!is.na(State)} else {!!incomefilter},
        if (input$malecheck == FALSE) {!is.na(State)} else {!!malefilter},
        if (input$whitecheck == FALSE) {!is.na(State)} else {!!whitefilter},
        if (input$blackcheck == FALSE) {!is.na(State)} else {!!blackfilter},
        if (input$asiancheck == FALSE) {!is.na(State)} else {!!asianfilter},
        if (input$hispaniccheck == FALSE) {!is.na(State)} else {!!hispanicfilter},
        if (input$multiplecheck == FALSE) {!is.na(State)} else {!!multiplefilter},
        if (is.null(input$econtypeinput)) {!is.na(State)} else {EconType %in% input$econtypeinput},
        if (is.null(input$metroinput)) {!is.na(State)} else {MetroArea %in% input$metroinput},
        if (is.null(input$urbinfinput)) {!is.na(State)} else {UrbanInfCode %in% input$urbinfinput}
      )
  })
  # cases graph
  cases_plot <- reactive({
    validate(
      need(input$statesinput != "", "Please select at least 1 state from the dropdown to the left."))
    validate(
      need(try(select(selected_covid_case(), State) != ""), "There are no counties matching the selected criteria.\nPlease select fewer variables, adjust the range of those already selected, or add additional states from the dropdown to the left."))
    plot <- 
      with_options(list(digits = 1),
      ggplotly(
      ggplot(selected_covid_case()) +
      geom_line(data = min_covid_case(), aes(x = Day, y = Cases, group = County,
                                             text = paste(County, "<br>Day: ", Day, "<br>Cases: ", round(Cases, digits = 1))), color = "#bdc3c7", show.legend = FALSE) +
      geom_line(aes(x = Day, y = Cases, color = County, group = County,
                    text = paste(County, "<br>Day: ", Day, "<br>Cases: ", round(Cases, digits = 1))), show.legend = FALSE) +
      geom_smooth(aes(x = Day, y = Cases), data = min_covid_case(),
        method = "loess", se = FALSE, color = "#bdc3c7", size = .5, alpha = .6, linetype = "dotted") +
      geom_ribbon(aes(x = Day, y = Cases), data = min_covid_case(),
        stat = "smooth", method = "loess", alpha = .15) +
      geom_smooth(aes(x = Day, y = Cases),
        method = "loess", se = FALSE, color = "#3c8dbc", size = .5, alpha = .6, linetype = "dotted") +
      geom_ribbon(aes(x = Day, y = Cases),
        stat = "smooth", method = "loess", alpha = .15) +
      labs(
        title = "Confirmed Covid-19 cases",
        x = "Days from 50th in-state case", y = "Detected cases per 100,000 people") +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_log10(expand = c(0, 0)) +
      theme(text = element_text(family = "Georgia"),
        panel.background = element_rect(fill = "#f7f5f0", colour = "#f7f5f0"),
        plot.title = element_text(face = "italic"),
        plot.subtitle = element_text(face = "italic"),
        axis.title = element_text(face = "italic"),
        plot.caption = element_text(face = "italic"),
        panel.grid.major = element_line(colour = "#D5D3CC", size = rel(.5)), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = NULL,
        axis.line.x = element_line(colour = "#908f85"),
        plot.margin = unit(c(2, 1, 2, 1), "lines")),
      height = 600,
      tooltip = "text"
      )
      )
  })
  output$cases_plot <- renderPlotly({
    input$updategraph
    isolate({
      cases_plot()
    })
  })
  output$cases_graph <- renderUI({
      withSpinner(
        plotlyOutput("cases_plot"),
        type = 1,
        color = "#3c8dbc"
      )
  })
  # cfr graph
  case_fatality_plot <- reactive({
    validate(
      need(input$statesinput != "", "Please select at least 1 state from the dropdown to the left."))
    validate(
      need(try(select(selected_covid_case(), State) != ""), "There are no counties matching the selected criteria.\nPlease select fewer variables, adjust the range of those already selected, or add additional states from the dropdown to the left."))
    plot <- 
      with_options(list(digits = 1),
      ggplotly(
      ggplot(selected_covid_case()) +
      geom_line(data = min_covid_case(), aes(x = Day, y = DeathRate, group = County,
                                             text = paste(County, "<br>Day: ", Day, "<br>Death Rate: ", paste(round(100*DeathRate, 2), "%", sep = ""))), color = "#bdc3c7", show.legend = FALSE) +
      geom_line(aes(x = Day, y = DeathRate, color = County, group = County,
                    text = paste(County, "<br>Day: ", Day, "<br>Death Rate: ", paste(round(100*DeathRate, 2), "%", sep = ""))), show.legend = FALSE) +
      geom_smooth(aes(x = Day, y = DeathRate), data = min_covid_case(),
                    method = "loess", se = FALSE, color = "#bdc3c7", size = .5, alpha = .6, linetype = "dotted") +
      geom_ribbon(aes(x = Day, y = DeathRate), data = min_covid_case(),
                    stat = "smooth", method = "loess", alpha = .15) +
      geom_smooth(aes(x = Day, y = DeathRate),
                    method = "loess", se = FALSE, color = "#3c8dbc", size = .5, alpha = .6, linetype = "dotted") +
      geom_ribbon(aes(x = Day, y = DeathRate),
                    stat = "smooth", method = "loess", alpha = .15) +
      labs(
        title = list(text = paste0("Reported Covid-19 death rate", "<br>", "<sup>",
                                   "","<sup>")),
        x = "Days from 50th in-state case", y = "Percent of detected cases resulting in a death") +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0), breaks = c(.01, .05, .1, .15, .2), labels = scales::percent) +
      theme(text = element_text(family = "Georgia"),
            panel.background = element_rect(fill = "#f7f5f0", colour = "#f7f5f0"),
            plot.title = element_text(face = "italic"),
            plot.subtitle = element_text(face = "italic"),
            axis.title = element_text(face = "italic"),
            plot.caption = element_text(face = "italic"),
            panel.grid.major = element_line(colour = "#D5D3CC", size = rel(.5)), 
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.ticks = element_blank(),
            axis.text.x = NULL,
            axis.line.x = element_line(colour = "#908f85"),
            plot.margin = unit(c(2, 1, 2, 1), "lines")),
      height = 600
      )
      )
  })
  output$case_fatality_plot <- renderPlotly({
    input$updategraph
    isolate({
      case_fatality_plot()
    })
  })
  output$case_fatality_graph <- renderUI({
    withSpinner(
      plotlyOutput("case_fatality_plot"),
      type = 1,
      color = "#3c8dbc"
    )
  })
  
}

# Shiny app
shinyApp(ui, server)