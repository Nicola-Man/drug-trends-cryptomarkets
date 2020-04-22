#For weekly DNet data
#Drug Trends Programme - DNeT/NIDIP team
library(shiny)
library(shinycustomloader) #for gif loader
library(DT)
library(plotly)

DrgAZ <- readRDS("DrgAZ.rds")
MktAZ <- readRDS("MktAZ.rds")

ui <- function(req) {
  bootstrapPage('',
    theme="DT-theme.css",
    tags$head(
    includeScript("google_analytics.js"),
# https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny
      tags$script(src="dimension.js"),
    #https://stackoverflow.com/questions/30096187/favicon-in-shiny
    #https://www.w3.org/2005/10/howto-favicon
      tags$link(rel="icon", type="image/png", href="favicon.png")
    ),

    navbarPage(
    header = singleton(tags$head(includeScript("google_analytics.js"))),
    title="Cryptomarket drug listings",

    # Visualisation  tab  ------------------------------------------------
          tabPanel( "Plot",
            mainPanel( width = 9,
              fluidPage(
                conditionalPanel(
                  condition = "input.Drop == 'Gant'",
                  withLoader(plotlyOutput("GantPlot", width = "100%", height = "100%"), #inline=TRUE),
                    type="image", loader="DT_NIDIP_tween.gif", proxy.height = "600px"),
                  includeHTML("fnoteGant.html")
                ),
                conditionalPanel(
                  condition = "input.Drop != 'Gant'",
                  withLoader(plotlyOutput("DNetPlot", width = "100%", height = "100%"), #inline=TRUE),
                    type="image", loader="DT_NIDIP_tween.gif", proxy.height = "600px"),
                  includeHTML("fnoteDNet.html")
                )
              )
            ),

            sidebarPanel( width = 3,

              radioButtons("Drop", "Plot:",
                choices = c("Gantt chart"="Gant",
                  "Specific drug"="Drug",
                  "Specific market"="Market"
                ), #inline = T,
                selected = c("Gant")
              ),

              conditionalPanel(
                condition = "input.Drop != 'Gant'",
                radioButtons("Sort", label = "Sort choices:", #inline = T,
                  choices = c("alphabetically"="AZ","by total number of listings"="NL")
                )
              ),
              conditionalPanel(
                condition = "input.Drop == 'Market'",
                selectInput("DMarket", label = "Market:",
                  choices = c(
                      "All markets"="All", MktAZ
                    ),"Empire Market"
                )
              ),
              conditionalPanel(
                condition = "input.Drop!= 'Market'",
                  selectInput("DDrug", label = "Drug:",
                    choices = c(
                      "All drugs"="All", DrgAZ
                    )
                  )
              ),
              conditionalPanel(
                condition = "input.Drop == 'Gant'",
                checkboxInput("Detail",
                  label=HTML("Show interruptions & weekly data <br><b>(may be slow to load)</b>"),
                  value=F
                ),
                conditionalPanel(
                  condition = "input.Detail == true & input.DDrug != 'All'",
                  checkboxInput("Heat",
                    label="Show heatmap of percentage of listings",
                    value=F
                  ),
                  conditionalPanel(
                    condition = "input.Heat == true",
                    radioButtons("Colour", label="Heatmap colour:", inline = T,
                      choices=c("rainbow","3-colour")
                    )
                  )
                )
              ),

                sliderInput("date", "Date:",
                  min = as.Date("30Jan2014","%d%b%Y"), max = as.Date("31Jan2020","%d%b%Y"),
                  value = c(as.Date("1Jan2019","%d%b%Y"), as.Date("31Jan2020","%d%b%Y")),
                  step = 7, timeFormat = "%d/%m/%Y"#, animate = T
                ),

              conditionalPanel(
                condition = "input.Drop == 'Market'",
                checkboxInput("ADrug", label=HTML("<b>Drug:</b>"),value=TRUE
                ),
  #Based on- https://stackoverflow.com/questions/5335535/how-to-indent-a-div
                HTML("<div style='margin-left: 6%;'>"),
                  checkboxGroupInput("BDrug", NULL, # label = "Drug:",
                    choices = DrgAZ,
                    selected = c("Benzodiazepines","Cannabis","Cocaine","Heroin")
                ),
                HTML("</div>"),
                  radioButtons("plot", "Show as:",
                    choices = c("Line plot"="line", "Stacked area plot"="area"
                    ), inline = T,
                    selected = c("line")
                  )
              ),

              conditionalPanel(
                condition = "input.Drop == 'Drug'",
                checkboxInput("AMarket", label=HTML("<b>Market:</b>"),value=TRUE
                ),
                HTML("<div style='margin-left: 6%;'>"),
                checkboxGroupInput(
                  "BMarket", NULL, #label = "Market:",
                  c("Apollon",
                    "Berlusconi",
                    "Cannazon",
                    "CGMC",
                    "Cryptonia Market",
                    "DarkBay",
                    "Dark Market",
                    "Dream Market",
                    "Empire Market",
                    "Monopoly Market",
                    "Tochka",
                    "Wall Street Market",
                    "White House Market"
                  ),
                  selected =
                  c("Dream Market",
                    "Empire Market",
                    "Berlusconi",
                    "Tochka",
                    "Apollon",
                    "Wall Street Market",
                    "Cryptonia Market",
                    "Cannazon",
                    "Dark Market",
                    "CGMC",
                    "White House Market",
                    "Monopoly Market",
                    "DarkBay")
                ),
                HTML("</div>")
              ),
              conditionalPanel( # edit when number of vendors is updated ######
                condition = "input.plot != 'area' & input.Yax != 'vendor' &
                (input.Drop== 'Drug' | (input.Drop== 'Market'))", # & input.DMarket != 'All'))", # test All market line plot#####
                checkboxInput("Tot", "Show total as shaded area")
              )
            )
          ),

          # Notes tab ---------------------------------------------------------------
          tabPanel( "Explanatory notes", includeHTML("notesExplanatory.html")),

          # Publication tab ------------------------------------------------------------
          tabPanel("Publications and acknowledgement", includeHTML("Publications.html")),
  #####indeterminate checkbox update not working yet - addEventListener not working???
  #https://www.w3schools.com/js/js_htmldom_eventlistener.asp
      tags$script(src="DropChk.js")
    )
  )
}
