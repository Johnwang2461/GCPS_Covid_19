library(shiny)
library(shinythemes)
library(shinydashboard)
library(rsconnect)
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(lubridate)
library(dygraphs)
library(reshape)
library(anytime)
library(forecast)

rsconnect::setAccountInfo(name='johnwang2461', token='4028E38EE9507D0DE263C9908A552D87', secret='bY1I5a9ZeNLEFoT13uFrLZ0lOsFfj6wkPy8Xu5K6')

GCPS_Close_Contact <- read_csv("https://raw.githubusercontent.com/Johnwang2461/GCPS_Covid_19/master/GCPS_Close_Contact.csv")
GCPS_Positive_Cases <- read_csv("https://raw.githubusercontent.com/Johnwang2461/GCPS_Covid_19/master/GCPS_Positive_Cases.csv")
GCPS_Suspected_Cases <- read_csv("https://raw.githubusercontent.com/Johnwang2461/GCPS_Covid_19/master/GCPS_Suspected_Cases.csv")
GCPS_Total_Cases <- read_csv("https://raw.githubusercontent.com/Johnwang2461/GCPS_Covid_19/master/GCPS_Total_Cases.csv")

GCPS_Close_Contact$Date <- anydate(GCPS_Close_Contact$Date)
GCPS_Positive_Cases$Date <- anydate(GCPS_Positive_Cases$Date)
GCPS_Suspected_Cases$Date <- anydate(GCPS_Suspected_Cases$Date)
GCPS_Total_Cases$Date <- anydate(GCPS_Total_Cases$Date)

Schools <- colnames(GCPS_Total_Cases)[2:141]

# Define UI for application that draws a histogram
ui <- navbarPage("GCPS Covid-19 Data", theme=shinytheme("flatly"),
                 tabPanel("Covid-19 Cases",
                          sidebarLayout(
                              sidebarPanel(
                              selectizeInput("variable", "School:", choices = c("All", "Alcova ES",	"Alford ES", "Anderson-Livsey ES",	"Annistown ES",	
                                                                              "Arcado ES",	"Archer HS",	"Baggett ES",	"Baldwin ES",	"Bay Creek MS",
                                                                              "Beaver Ridge ES",	"Benefield ES",	"Berkeley Lake ES",	"Berkmar HS",
                                                                              "Berkmar MS",	"Bethesda ES",	"Britt ES",	"Brookwood ES",	"Brookwood HS",
                                                                              "Buice Center",	"Burnette ES",	"Camp Creek ES",	"Cedar Hill ES",
                                                                              "Centerville ES",	"Central Gwinnett HS",	"Chattahoochee ES",
                                                                              "Chesney ES",	"Coleman MS",	"Collins Hill HS",	"Cooper ES",
                                                                              "Corley ES",	"Couch MS",	"Craig ES",	"Creekland MS",	"Crews MS",
                                                                              "Dacula ES",	"Dacula HS",	"Dacula MS",	"Discovery HS",	"Duluth HS",
                                                                              "Duluth MS",	"Duncan Creek ES",	"Dyer ES",	"Ferguson ES",	"Five Forks MS",
                                                                              "Fort Daniel ES",	"Freeman's Mill ES",	"GIVE East",	"GIVE West",
                                                                              "Grace Snell MS",	"Graves ES",	"Grayson ES",	"Grayson HS",	"Gwin Oaks ES",	
                                                                              "Gwinnett Online Campus",	"Gwinnett School of Mathematics, Science and Technology",
                                                                              "Harbins ES",	"Harmony ES",	"Harris ES",	"Head ES",	"Hopkins ES",	"Hull MS",
                                                                              "International Transition Center",	"Ivy Creek ES",	"Jackson ES",	"Jenkins ES",	"Jones MS",
                                                                              "Jordan MS",	"Kanoheda ES",	"Knight ES",	"Lanier HS",	"Lanier MS",
                                                                              "Lawrenceville ES",	"Level Creek ES",	"Lilburn ES",	"Lilburn MS",
                                                                              "Lovin ES",	"Magill ES",	"Mason ES",	"Maxwell HS of Technology",	
                                                                              "McClure Health Science HS",	"McConnell MS",	"McKendree ES",	"Meadowcreek ES",
                                                                              "Meadowcreek HS",	"Mill Creek HS",	"Minor ES",	"Moore MS",	"Mountain Park ES",
                                                                              "Mountain View HS",	"Mulberry ES",	"Nesbit ES",	"Norcross ES",	"Norcross HS",
                                                                              "North Gwinnett HS",	"North Gwinnett MS",	"Northbrook MS",	"Norton ES",
                                                                              "Oakland Meadow School",	"Osborne MS",	"Parkview HS",	"Parsons ES",
                                                                              "Partee ES",	"Patrick ES",	"Paul Duke STEM HS",	"Peachtree ES",
                                                                              "Peachtree Ridge HS",	"Pharr ES",	"Phoenix HS",	"Pinckneyville MS",
                                                                              "Puckett's Mill ES",	"Radloff MS",	"Richards MS",	"Riverside ES",
                                                                              "Roberts ES",	"Rock Springs ES",	"Rockbridge ES",	"Rosebud ES",
                                                                              "Shiloh ES",	"Shiloh HS",	"Shiloh MS",	"Simonton ES",	"Simpson ES",
                                                                              "Snellville MS",	"South Gwinnett HS",	"Starling ES",	"Stripling ES",
                                                                              "Sugar Hill ES",	"Summerour MS",	"Suwanee ES",	"Sweetwater MS",
                                                                              "Sycamore ES",	"Taylor ES",	"Trickum MS",	"Trip ES",	"Twin Rivers MS",
                                                                              "Walnut Grove ES",	"White Oak ES",	"Winn Holt ES",	"Woodward Mill ES"
                                                                              
                              ), multiple=TRUE, options = list(maxItems=1)),
                                    h2("Understanding Report"),
                              h6("Total Possible Cases: This chart reflects total active cases by school based on the expected return dates of those affected. It does not reflect those who are cleared medically to return to school/work before that expected date, or those who are out beyond their expected return date. Information about when an individual actually returns is tracked manually at the school or workplace and is not captured in this report."),
                              h6("Suspected Case: A person who displays any of the common symptoms of COVID-19 but is waiting to be tested or to receive the results of a test already administered. These common symptoms include a fever of 100.4 degrees or higher, unusual fatigue, muscle pain, coughing, sudden loss of taste or smell, difficulty breathing, prolonged headache, and sore throat, that do not appear to be caused by another illness. A person who is a suspected case displays symptoms but has not tested positive for COVID-19."),
                              h6("Close Contact: A person who is not ill but who has been exposed to a positive COVID-19 case in a manner that could make the person susceptible to contracting the virus"),
                              h6("Positive Case: A person who receives a lab-confirmed positive COVID-19 test result, or a physician's diagnosis of COVID-19."),
                              ),
                              mainPanel(
                                  h2("Gwinnett County Public Schools Covid-19 Cases by School"),
                                  plotlyOutput("plot1")
                              )
                              )
                ),
                tabPanel("Forecasting",
                         sidebarLayout(
                             sidebarPanel(
                                 h2("Understanding the Forecast"),
                                 h6("The model forecasts 7 days into the future. The dashed lines for each scenario (Total Possible Cases, Suspected Cases, Close Contact Cases, and Positive Cases) represent the 95% Confidence Interval for the forecast. "),
                                 h2("Methodology Considerations"),
                                 h6("While Epidemiologists may typically apply SEIR (Suspectible, Exposed, Infectious, Removed) modeling to Covid-19, the existing dataset providied by GCPS is limited to Positive Cases, Suspected Cases, and Close Contact Cases with all active cases summed as Total Possible Cases. Without key information regarding individual's infectious rate or recovery rate, assumptions would need to be made that may ultimately cause further model inaccuracies. Instead, an autoregressive integrated moving average model is applied; the model, in essence, leverages observations from previous time lags to forecast. In particular, the auto.arima() function in R/'s forecasting package was used to generate the parameters for the ARIMA function used for forecasting."),
                                 h6(uiOutput("Reference")),
                                 h2("Limitations"),
                                 h6("The existing data set is extremely small, and as more data is included, the model is subject to change. Likewise, the data set is reflective of only the information officially put forth by GCPS. The data and forecasting may not reflect asymptomatic but untested cases or cases unreleased by GCPS.")
                                 
                             ),
                             mainPanel(
                                 h2("Gwinnett County Public Schools Covid-19 Forecast"),
                                 plotlyOutput("plot2")
                             )
                         )
                    
                ),
                tabPanel("TAKE ACTION",
                         fluidRow(
                             box(h2("Sign The Pledge", align="center"),
                                 status = "primary",
                                 solidHeader = F,
                                 collapsible = F,
                                 width = 12,
                                 fluidRow(column(width = 2, align = "center",
                                                 tags$a(img(src="https://image.flaticon.com/icons/svg/2328/2328241.svg", width=100), href="https://docs.google.com/forms/d/e/1FAIpQLSeGYtH2jMpXShl7PtcS1EP2FzHQOC1wl7CAT_71rTJP1V7e3A/viewform", title="Pledge Link")),column(width = 10, htmlOutput("PledgeText")))),
                         ),
                         fluidRow(
                             box(h2("Contact The School Board", align="center"),
                                 status = "primary",
                                 solidHeader = F,
                                 collapsible = F,
                                 width = 12,
                                 fluidRow(column(width = 2, align = "center",
                                                 tags$a(img(src="https://image.flaticon.com/icons/svg/2572/2572197.svg", width=100), href="http://data.gwinnett.k12.ga.us/ReqsforAddrGCPSBOE.nsf/RequestXPage.xsp", title="Speak at School Board")),column(width = 10, htmlOutput("SchoolBoard")))),
                         ),
                         fluidRow(
                             box(h2("Report My School", align="center"),
                                 status = "primary",
                                 solidHeader = F,
                                 collapsible = F,
                                 width = 12,
                                 fluidRow(column(width = 2, align = "center",
                                                 tags$a(img(src="https://image.flaticon.com/icons/svg/545/545684.svg", width=100), href="mailto:reportmyschool@gmail.com", title="Report My School")),column(width = 10, htmlOutput("ReportSchool")))),
                         )
                )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot1 <- renderPlotly({
        if (is.null(input$variable)) {
            p<- ggplot() +
                geom_line(data=GCPS_Total_Cases, aes(x=Date, y=All, color="Total Possible Cases", text = paste("Cases: ", All), group=1))+
                geom_line(data=GCPS_Suspected_Cases, aes(x=Date, y=All, color="Suspected Cases", text = paste("Cases: ", All), group=1))+
                geom_line(data=GCPS_Close_Contact, aes(x=Date, y=All, color="Close Contact Cases", text = paste("Cases: ", All), group=1))+
                geom_line(data=GCPS_Positive_Cases, aes(x=Date, y=All, color="Positive Cases", text = paste("Cases: ", All), group=1))+
                annotate(geom="text", x=as.Date("2020-08-26"), y=max(GCPS_Total_Cases$All),
                         label="GCPS Phase 1") +
                geom_vline(data=GCPS_Total_Cases, aes(xintercept=as.numeric(as.Date("2020-08-26"))),color="black", linetype=6)+
                annotate(geom="text", x=as.Date("2020-09-02"), y=max(GCPS_Total_Cases$All),
                         label="GCPS Phase 2") +
                geom_vline(data=GCPS_Total_Cases, aes(xintercept=as.numeric(as.Date("2020-09-02"))),color="black", linetype=6)+
                xlab("Date") +
                ylab("Cases")+
                scale_colour_manual("", 
                                    breaks = c("Total Possible Cases", "Close Contact Cases", "Suspected Cases", "Positive Cases"),
                                    values = c("red", "green", "blue", "orange"))
            ggplotly(p, tooltip = c("Date","text"))
        }
        else {
        p<- ggplot() +
            geom_line(data=GCPS_Total_Cases, aes(x=Date, y=unlist(GCPS_Total_Cases[input$variable], use.names = FALSE), color="Total Possible Cases", text = paste("Cases: ", unlist(GCPS_Total_Cases[input$variable], use.names = FALSE)), group=1))+
            geom_line(data=GCPS_Suspected_Cases, aes(x=Date, y=unlist(GCPS_Suspected_Cases[input$variable], use.names = FALSE), color="Suspected Cases", text = paste("Cases: ", unlist(GCPS_Suspected_Cases[input$variable], use.names = FALSE)), group=1))+
            geom_line(data=GCPS_Close_Contact, aes(x=Date, y=unlist(GCPS_Close_Contact[input$variable], use.names = FALSE), color="Close Contact Cases", text = paste("Cases: ", unlist(GCPS_Close_Contact[input$variable], use.names = FALSE)), group=1))+
            geom_line(data=GCPS_Positive_Cases, aes(x=Date, y=unlist(GCPS_Positive_Cases[input$variable], use.names = FALSE), color="Positive Cases", text = paste("Cases: ", unlist(GCPS_Positive_Cases[input$variable], use.names = FALSE)), group=1))+
            annotate(geom="text", x=as.Date("2020-08-26"), y=max(unlist(GCPS_Total_Cases[input$variable], use.names = FALSE)),
                     label="GCPS Phase 1") +
            geom_vline(data=GCPS_Total_Cases, aes(xintercept=as.numeric(as.Date("2020-08-26"))),color="black", linetype=6)+
            annotate(geom="text", x=as.Date("2020-09-02"), y=max(unlist(GCPS_Total_Cases[input$variable], use.names = FALSE)),
                     label="GCPS Phase 2") +
            geom_vline(data=GCPS_Total_Cases, aes(xintercept=as.numeric(as.Date("2020-09-02"))),color="black", linetype=6)+
            xlab("Date") +
            ylab("Cases")+
            scale_colour_manual("", 
                                breaks = c("Total Possible Cases", "Close Contact Cases", "Suspected Cases", "Positive Cases"),
                                values = c("red", "green", "blue", "orange"))
        ggplotly(p, tooltip = c("Date", "text"))
        }
        })
    output$plot2 <- renderPlotly({
        GCPS_Total_Cases_Fit<- auto.arima(GCPS_Total_Cases$All)
        GCPS_Suspected_Cases_Fit<- auto.arima(GCPS_Suspected_Cases$All)
        GCPS_Close_Cases_Fit<- auto.arima(GCPS_Close_Contact$All)
        GCPS_Positive_Cases_Fit<- auto.arima(GCPS_Positive_Cases$All)
        
        GCPS_Total_Cases_Forecast<- GCPS_Total_Cases[,c("Date","All")]
        GCPS_Total_Cases_Forecast$low_bound <- GCPS_Total_Cases_Forecast$All
        GCPS_Total_Cases_Forecast$high_bound <- GCPS_Total_Cases_Forecast$All
        
        GCPS_Suspected_Cases_Forecast<- GCPS_Suspected_Cases[,c("Date","All")]
        GCPS_Suspected_Cases_Forecast$low_bound <- GCPS_Suspected_Cases_Forecast$All
        GCPS_Suspected_Cases_Forecast$high_bound <- GCPS_Suspected_Cases_Forecast$All
        
        GCPS_Close_Cases_Forecast<- GCPS_Close_Contact[,c("Date","All")]
        GCPS_Close_Cases_Forecast$low_bound <- GCPS_Close_Cases_Forecast$All
        GCPS_Close_Cases_Forecast$high_bound <- GCPS_Close_Cases_Forecast$All
        
        GCPS_Positive_Cases_Forecast<- GCPS_Positive_Cases[,c("Date","All")]
        GCPS_Positive_Cases_Forecast$low_bound <- GCPS_Positive_Cases_Forecast$All
        GCPS_Positive_Cases_Forecast$high_bound <- GCPS_Positive_Cases_Forecast$All
        
        for (i in 1:7) {
            GCPS_Total_Cases_Forecast <- rbind(GCPS_Total_Cases_Forecast, list(anydate(unlist(GCPS_Total_Cases_Forecast[nrow(GCPS_Total_Cases_Forecast),"Date"]+1)),forecast(GCPS_Total_Cases_Fit, h=7)[[4]][i], forecast(GCPS_Total_Cases_Fit, h=7)[[5]][7+i], forecast(GCPS_Total_Cases_Fit, h=7)[[6]][7+i]))
            GCPS_Suspected_Cases_Forecast <- rbind(GCPS_Suspected_Cases_Forecast, list(anydate(unlist(GCPS_Suspected_Cases_Forecast[nrow(GCPS_Suspected_Cases_Forecast),"Date"]+1)),forecast(GCPS_Suspected_Cases_Fit, h=7)[[4]][i], forecast(GCPS_Suspected_Cases_Fit, h=7)[[5]][7+i], forecast(GCPS_Suspected_Cases_Fit, h=7)[[6]][7+i]))
            GCPS_Close_Cases_Forecast <- rbind(GCPS_Close_Cases_Forecast, list(anydate(unlist(GCPS_Close_Cases_Forecast[nrow(GCPS_Close_Cases_Forecast),"Date"]+1)),forecast(GCPS_Close_Cases_Fit, h=7)[[4]][i], forecast(GCPS_Close_Cases_Fit, h=7)[[5]][7+i], forecast(GCPS_Close_Cases_Fit, h=7)[[6]][7+i]))
            GCPS_Positive_Cases_Forecast <- rbind(GCPS_Positive_Cases_Forecast, list(anydate(unlist(GCPS_Positive_Cases_Forecast[nrow(GCPS_Positive_Cases_Forecast),"Date"]+1)),forecast(GCPS_Positive_Cases_Fit, h=7)[[4]][i], forecast(GCPS_Positive_Cases_Fit, h=7)[[5]][7+i], forecast(GCPS_Positive_Cases_Fit, h=7)[[6]][7+i]))
        }
        
        q <- ggplot() +
            geom_line(data=GCPS_Total_Cases_Forecast, aes(x=Date, y=All, color="Total Possible Cases", text = paste("Cases: ", All), group=1))+
            geom_line(data=GCPS_Total_Cases_Forecast, aes(x=Date, y=low_bound, color="Total Possible Cases", text = paste("Cases: ", All), group=1), linetype=6)+
            geom_line(data=GCPS_Total_Cases_Forecast, aes(x=Date, y=high_bound, color="Total Possible Cases", text = paste("Cases: ", All), group=1), linetype=6)+
            geom_line(data=GCPS_Suspected_Cases_Forecast, aes(x=Date, y=All, color="Suspected Cases", text = paste("Cases: ", All), group=1))+
            geom_line(data=GCPS_Suspected_Cases_Forecast, aes(x=Date, y=low_bound, color="Suspected Cases", text = paste("Cases: ", All), group=1), linetype=6)+
            geom_line(data=GCPS_Suspected_Cases_Forecast, aes(x=Date, y=high_bound, color="Suspected Cases", text = paste("Cases: ", All), group=1), linetype=6)+
            geom_line(data=GCPS_Close_Cases_Forecast, aes(x=Date, y=All, color="Close Contact Cases", text = paste("Cases: ", All), group=1))+
            geom_line(data=GCPS_Close_Cases_Forecast, aes(x=Date, y=low_bound, color="Close Contact Cases", text = paste("Cases: ", All), group=1), linetype=6)+
            geom_line(data=GCPS_Close_Cases_Forecast, aes(x=Date, y=high_bound, color="Close Contact Cases", text = paste("Cases: ", All), group=1), linetype=6)+
            geom_line(data=GCPS_Positive_Cases_Forecast, aes(x=Date, y=All, color="Positive Cases", text = paste("Cases: ", All), group=1))+
            geom_line(data=GCPS_Positive_Cases_Forecast, aes(x=Date, y=low_bound, color="Positive Cases", text = paste("Cases: ", All), group=1), linetype=6)+
            geom_line(data=GCPS_Positive_Cases_Forecast, aes(x=Date, y=high_bound, color="Positive Cases", text = paste("Cases: ", All), group=1), linetype=6)+
            annotate(geom="text", x=as.Date("2020-08-26"), y=max(GCPS_Total_Cases_Forecast$high_bound), label="GCPS Phase 1") +
            geom_vline(data=GCPS_Total_Cases, aes(xintercept=as.numeric(as.Date("2020-08-26"))),color="black", linetype=6)+
            annotate(geom="text", x=as.Date("2020-09-02"), y=max(GCPS_Total_Cases_Forecast$high_bound), label="GCPS Phase 2") +
            geom_vline(data=GCPS_Total_Cases, aes(xintercept=as.numeric(as.Date("2020-09-02"))),color="black", linetype=6)+
            annotate(geom="text", x=Sys.Date(), y=-5, label="Forecast Begins")+
            geom_vline(data=GCPS_Total_Cases, aes(xintercept=as.numeric(Sys.Date())),color="black", linetype=6)+
            xlab("Date") +
            ylab("Cases")+
            scale_colour_manual("", 
                                breaks = c("Total Possible Cases", "Close Contact Cases", "Suspected Cases", "Positive Cases"),
                                values = c("red", "green", "blue", "orange"))
        ggplotly(q, tooltip = c("Date","text"))
    })
    output$PledgeText <- renderUI({
        HTML(paste("Out of an abundance of love for the students, faculty and staff of Gwinnett County Public Schools:", "","I pledge to protect lives and stand in solidarity by voluntarily releasing any positive COVID test results I receive.", ""
                   ,"I also pledge to make a request of my employer, Gwinnett County Public Schools, to release my positive COVID test result immediately to the public in order to facilitate contact tracing and limit community spread of this deadly virus.", sep="<br/>"))
        
    })
    
    output$SchoolBoard <- renderUI({
        HTML(paste("District 1: Carol C. Boyce, Phone: (770) 995-6796", "District 2: Steven B. Knudsen, Phone: (470) 839-5366",
                   "District 3: Dr. Mary Kay Murphy, Phone: (770) 840-9752", "District 4: Everton Blair Jr., Phone: (404) 491-0423",
                   "District 5: Louise Radloff, Phone: (770) 923-4784", "", "Email: MySchoolBoard@gwinnett.k12.ga.us", sep="<br/>"))
    })

    output$ReportSchool <- renderUI({
        HTML(paste("Representative Beth Moore set up a whistleblower email for Georgia students, teachers, and admin
                   to share photos, videos, and testimonials of unsafe conditions at school. She has indicated that 
                   she will provide anonymous cover, especially if you've been threatened with consequences for 
trying to ensure a safe school environment.", "", "Email: reportmyschool@gmail.com", sep="<br/>"))
    })
}


# Run the application 
shinyApp(ui = ui, server = server)