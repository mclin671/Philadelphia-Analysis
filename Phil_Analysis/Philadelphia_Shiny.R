
# Install Libraries -------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(tidyr)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(plotly)
library(zoo)
library(shiny)
library(RColorBrewer)
library(plotly)
library(geojson)
library(geojsonio)
library(maps)
library(sp)
library(maptools)

# Import Data -------------------------------------------------------------
shootings <- read_csv("https://phl.carto.com/api/v2/sql?q=SELECT+*,+ST_Y(the_geom)+AS+lat,+ST_X(the_geom)+AS+lng+FROM+shootings&filename=shootings&format=csv&skipfields=cartodb_id")

arrests <- read_csv("https://media.githubusercontent.com/media/phillydao/phillydao-public-data/main/docs/data/arrest_data_daily_by_district.csv")

school_grad <- read_csv("https://cdn.philasd.org/offices/performance/Open_Data/School_Performance/Graduation_Rates/SDP_Graduation_Rates_School_S_2022-05-23.csv")

collegematric_2020 <- read_csv("https://cdn.philasd.org/offices/performance/Open_Data/School_Performance/College_Matriculation/SDP_College_Matriculation_2019-2020.csv")

collegematric_2021 <- read_csv("https://cdn.philasd.org/offices/performance/Open_Data/School_Performance/College_Matriculation/SDP_College_Matriculation_2020-2021.csv")

phila <- geojson_read("https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson", what = "sp")

schoollocation <- read_csv("https://cdn.philasd.org/offices/performance/Open_Data/School_Information/School_List/2021-2022%20Master%20School%20List%20(20211105).csv")

allschoolgrad <- read_csv("https://cdn.philasd.org/offices/performance/Open_Data/School_Performance/Graduation_Rates/SDP_Graduation_Rates_District_S_2022-05-23.csv")


# Shooting Data Analysis --------------------------------------------------

#Filter data for Shootings by Date plot
shootingdate <- shootings %>% 
  mutate(ymd = ymd(date_),
         Date = as.Date(as.yearmon(ymd))) %>% 
  group_by(Date) %>% 
  mutate(Total = n()) %>% 
  select(Total, Date) %>%
  distinct(.keep_all = TRUE)

#Filter data for shootings fatality plot
totalshootings <- shootings %>% group_by(year, fatal) %>% summarise(total = n())

#Recode fatality variables
totalshootings$fatal <- recode(na_if(totalshootings$fatal, ""), '0' = 'Nonfatal', '1' = 'Fatal', .missing = 'Unknown')

totalshootings$fatal <- factor(totalshootings$fatal, levels = c("Unknown", "Fatal", "Nonfatal"))

#Filter data for shootings race plot
shootings25 <- shootings %>% 
  group_by(year, race, sex) %>% 
  filter(age <=25) %>% 
  summarise(count = n())

#Recode column values for shooting race and gender plots
shootings25$sex <- recode(shootings25$sex, 'M' = 'Male', 'F' = 'Female')

shootings25$race <- recode(shootings25$race, 'A' = 'Asian', 'B' = 'Black', 'W' = 'White', 'U' = 'Unknown')

shootings25$race <- factor(shootings25$race)

# Arrest Data Analysis ----------------------------------------------------
#Extract year from the dispatch date data
arrests <- arrests %>% 
  mutate(year = year(ymd(date_value)),
         ymd = ymd(date_value),
         Date = as.Date(as.yearmon(ymd)))

#Convert data from wide format to long format
arrests_long <- arrests %>% 
  pivot_longer(cols = `Aggravated Assault`:'Violation of Protection Order', names_to = "Type", values_to = "Count")

#Summarize total arrests by month
monthsum <- arrests_long %>%
  group_by(Date) %>%
  mutate(Total = sum(Count)) %>%
  select(Total, Date) %>%
  distinct(.keep_all = TRUE) %>%
  mutate(monthtext = paste(month(Date, label = TRUE), " ", year(Date), "\n", "Arrests: ", Total, sep = ""))

#Sum total number of arrests into a new tibble
yearsum <- arrests_long %>% 
  group_by(year) %>% 
  summarise(total = sum(Count))

#Combine arrest data into generalized groups
arrest_combine <- arrests %>% 
  rowwise() %>% 
  group_by(year) %>% 
  mutate(
    
    `All Violent Offenses` = sum(`Aggravated Assault`, `Arson`, `Carjacking`, `Homicide: Shooting`, `Homicide: Other`, `Non-Fatal Shooting`, `Rape`, `Robbery with a Deadly Weapon`, `Robbery`, `Strangulation`, `Simple Assault`, `Other Assaults`, `Sexual Assault and Other Sex Offenses`, `Other Violent Crimes`, na.rm = T), 
    `All Property Offenses` = sum(`Burglary/Residential`, `Burglary/Commercial`, `Theft`, `Retail Theft`, `Theft from Auto`, `Auto Theft`, `Fraud`, `Trespass`, `Criminal Mischief`, `Other Property Crimes`, na.rm = T),
    `All Drug Offenses` = sum(`Drug Sales with a Firearm`, `Drug Sales`, `Drug Possession`, `Drug Possession in Jails`, na.rm = T),
    `All Firearm Possession` = sum(`Firearm Possession by a Prohibited Person`, `Firearm Possession without a License`, `Altered Firearm Serial Number`, `Other Firearm Offenses`, na.rm = T),
    
    `All Other Offenses` = sum(`Violation of Protection Order`, `DUI`, `Prostitution/Sex Work`, `Patronizing Prostitutes/Sex Workers`, `Promoting Prostitution`, `Victim/Witness Intimidation & Retaliation`, `Threats of Violence`, `Ethnic Intimidation`, `Disorderly Conduct`, `Illegal Dumping/Littering`, na.rm = T),
    
    `Uncategorized Offenses` = sum(`Uncategorized Offenses`, na.rm = T)
  ) %>%
  
  summarise(`All Violent Offenses`, `All Property Offenses`, `All Drug Offenses`, `All Firearm Possession`, `All Other Offenses`, `Uncategorized Offenses`) %>% 
  
  distinct(.keep_all = TRUE)

#Pivot offense category data into long format for plotting

summarized_long <- pivot_longer(data = arrest_combine, cols = `All Violent Offenses`:`Uncategorized Offenses`, names_to = "Offense Category", values_to = "Count")

summarized_long <- rename(summarized_long, "Year" = "year")

# School District Data Analysis -------------------------------------------
#Filter school graduation data. College matriculation data does not include Charter or Alternative Schools, so graduation data on those schools were excluded.
#Data that did not include a graduation score was excluded. 

#Create dataset for gender graduation

grad_gen <- allschoolgrad %>%
  filter(rate_type == "4-Year Graduation Rate" & aggregate_level == "Graduation Rate- Excluding Alternative Schools" & group == "Gender" & score != "s")

grad_gen <- grad_gen %>% 
  mutate(startyr = parse_number(grad_gen$cohort), gradyr = startyr + 4) %>%
  mutate(gentext = paste("Graduation Year: ", gradyr, "\n", "Graduation Rate: ", score, "\n", sep = ""))

#Create data set for race graduation

allschoolgrad$score <- as.numeric(allschoolgrad$score)

grad_race <- allschoolgrad %>% 
  filter(rate_type == "4-Year Graduation Rate" & aggregate_level == "Graduation Rate- Excluding Alternative Schools" & group == "Ethnicity" & score != "s") 

grad_race <- grad_race %>% 
  mutate(startyr = parse_number(grad_race$cohort), gradyr = startyr + 4) %>%
  mutate(racetext = paste("Graduation Year: ", gradyr, "\n", "Graduation Rate: ", score, "\n", "Race: ", subgroup, sep = ""))

#Filter data to four year grad rate, exclude alternative schools, only show graduation rate for all students
fouryr <- school_grad %>% 
  filter(rate_type == "4-Year Graduation Rate" & sector != "Alternative" & group == "All Students")

fouryr <- fouryr %>% 
  mutate(startyr = parse_number(fouryr$cohort), gradyr = startyr + 4)

fouryr <- rename(fouryr, "ULCS Code" = "schoolid_ulcs")

#Rename columns in College Matriculation Data for Left Join
collegematric_2020 <- rename(collegematric_2020, "2020" = "First-Fall Matriculation Rate")

collegematric_2021 <- rename(collegematric_2021, "2021" = "First-Fall Matriculation Rate")

collegematric_all <- left_join(collegematric_2020, collegematric_2021, by = "ULCS Code", suffix = c(".x", ".y")) %>% 
  select(-ends_with(".x"), -ends_with(".y"))

#Pivot data into long format
collegematric_all <- collegematric_all %>% 
  pivot_longer(cols = c('2020', '2021'), names_to = 'gradyr', values_to = 'Matriculation Rate')

collegematric_all$gradyr <- as.numeric(unlist(collegematric_all$gradyr))

#Join college matriculation data with high school graduation data for bar plot
grad_matric <- left_join(collegematric_all, fouryr %>% 
                           select(gradyr, 'ULCS Code', score, schoolname), by = c('gradyr', 'ULCS Code'))

grad_matric <- rename(grad_matric, "Graduation Rate" = "score")

grad_matric$`Graduation Rate` <- as.numeric(grad_matric$`Graduation Rate`)

grad_matric_long <- grad_matric %>% pivot_longer(grad_matric, cols = c('Matriculation Rate', 'Graduation Rate'), names_to = 'Type', values_to = 'Rate')

grad_matric_long <- grad_matric_long %>% mutate(Type = factor(Type, unique(Type)))

grad_matric_long <- grad_matric_long %>% mutate(gradmtext = paste(schoolname, "\n", grad_matric_long$Type, ": ", grad_matric_long$Rate, sep = ""))

#Join college matriculation data with GPS data for mapping
matric_map <- left_join(collegematric_all, schoollocation %>% 
                          select('ULCS Code', 'GPS Location', 'Publication Name'), by = 'ULCS Code')

matric_map <- matric_map %>% 
  separate('GPS Location', c('lat', 'long'), sep = ",") %>% 
  mutate_at(c('lat', 'long'), as.numeric)

matric_map <- matric_map %>% mutate(mytext = paste("School: ", matric_map$'Publication Name', "\n", "Graduation Rate: ", grad_matric$`Graduation Rate`, "\n", "Matriculation Rate: ", grad_matric$'Matriculation Rate', sep = ""))



# Define UI for Application -----------------------------------------------

ui <- navbarPage("My Application", 
                 tabPanel("Shootings", 
                          titlePanel(
                            h1("Philadelphia Shooting Victims - 2015 to Present", align = "center")
                          ),
                          fluidRow(
                            column(12,
                                   offeset = 0.7,
                                   p("The data below shows the total of reported shooting victims in Philadelphia. Victims of shooting violence are disproportionately male. With the onset of COVID-19, the number of shooting victims for both men and women increased. Individuals under the age of 26 may be especially susceptible to an increase in gun violence due to the transition from in-peron to virtual schooling.", size = 14))
                          ),
                          fluidRow(
                            column(6, 
                                   plotlyOutput("dateplot")),
                            column(6, 
                                   plotOutput("fatalityplot"))
                          ),
                          fluidRow(
                            column(12,
                                   plotlyOutput("raceshooting"))
                          ),
                          fluidRow(
                            column(12, 
                                   offset = 0.7,
                                   h4("Data Source: Philadelphia Police Department. Year to Date Through 22 August 2022", align = "left"))
                          )
                 ),
                 
                 tabPanel("Arrests",
                          titlePanel(
                            h1("Philadelphia Arrests - 2014 to Present", align = "center")
                          ),
                          fluidRow(
                            column(12,
                                   offeset = 0.7,
                                   p("The data below shows Philadelphia totals for people arrested by local and state law enforcement agencies for different offense types. With the onset of COVID-19, government function changed due to: 1) isolation, closed schools, decreased community interaction, 2) to minimize human contact, police made fewer arrests overall, especially for minor infractions, 3) court operations were delayed. Current District Attorney of Philadelphia, Larry Krasner, entered office on January 1, 2018, and will remain DA until January 5, 2026.", size = 14))
                          ),
                          fluidRow(
                            column(6,
                                   plotlyOutput("monthplot")),
                            column(6,
                                   plotOutput("yearsum"))
                          ),
                          fluidRow(
                            column(12,
                                   plotlyOutput("offenseplot"))
                          ),
                          fluidRow(
                            column(12,
                                   offset = 0.7,
                                   h4("Data Source: Philadelphia District Attorney's Office. Year to Date Through 22 August 2022.", align = "left"))
                          )
                 ),
                 tabPanel("School Districts",
                          titlePanel(
                            h1("Philadelphia School District Performance - 2014 to 2021", align = "center")
                          ),
                          fluidRow(
                            column(12,
                                   offset = 0.7,
                                   p("Graduation rates have trended higher from 2014 to 2021. However, there is a large difference between high school graduation rates and college matriculation rates for the fall semester after graduating high school. Lower matriculation rates vary across Philadelphia, however, there is a correlation betwen lower-income neighborhoods, such as the North, West, Southwest, and the Northeast sections of the city, with lower college matriculation rates.", size = 14))
                          ),
                          tabsetPanel(
                            tabPanel("Tab 1",
                                     fluidRow(column(12,
                                                     plotlyOutput("mapplot", height = 650))),
                                     fluidRow(column(7,
                                                     plotlyOutput("raceschool")),
                                              column(5,
                                                     plotlyOutput("genderplot"))),
                                     fluidRow(column(12,
                                                     offset = 0.7,
                                                     h4("Data Source: Philadelphia School District. Year to Date Through 22 August 2022.")))
                            ),
                            tabPanel("Tab 2",
                                     sidebarLayout(
                                       sidebarPanel(
                                         checkboxGroupInput("schools", 
                                                            label = h3("Select Philadelphia High Schools"), 
                                                            choices = sort(unique(grad_matric_long$schoolname)),
                                                            selected = grad_matric_long$schoolname[1],
                                         ),
                                       ),
                                       mainPanel(
                                         plotlyOutput("gradmatric", height = 800, width = 800)
                                       )
                                     )
                            )
                          )
                 )
)


# Server Function ---------------------------------------------------------
server <- function(input, output, session) {
  
  #Make base plot for Shooting Date Plot
  dateplot <- reactive({
    datebase <- ggplot(data = shootingdate, aes(x = Date, y = Total)) + 
      geom_area(fill = "#69b3a2", alpha = 0.5) + 
      geom_line(color = "#69b3a2") + 
      scale_x_date(limits = as.Date(c("2015-01-01", "2023-01-01")), 
                   date_breaks = "1 year", 
                   date_labels ="%Y") +
      ylab("Number of Shootings") + 
      ggtitle("Number of Shooting Victims by Date")
    #Make Shooting Date plot interactive
    ggplotly(datebase)
  })
  
  #Output Dateplot
  output$dateplot <- renderPlotly(
    dateplot()
  )
  
  output$fatalityplot <- renderPlot(
    #Plot fatality shootings
    ggplot(data = totalshootings, aes(x = year, y = total, fill = fatal)) + 
      geom_bar(stat = "identity") + 
      geom_text(aes(label = total, group = fatal), position = position_stack(vjust = 0.15), alpha = 0.8, vjust = 0.1, size = 6) + 
      scale_x_continuous(breaks = totalshootings$year) +
      labs(x = "Year", y = "Total", title = "Shooting Victims Per Year") +
      theme_bw() +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) + 
      theme(text = element_text(size = 16))
  )
  
  
  #Plot base for shootings by race
  raceshooting <- reactive({
    rbase <- ggplot(data = shootings25, aes(x = year, y = count, fill = race)) + 
      geom_bar(stat = "identity") +
      facet_wrap(~sex) + 
      scale_x_continuous(breaks = shootings25$year) +
      scale_fill_brewer(palette = "Set1") +
      labs(x = "Year", 
           y = "Number of Victims", 
           fill = "Race")
    #Make raceplot interactive 
    ggplotly(rbase) %>%
      layout(
        title = list(
          text = paste0('Total Number of Shooting Victims Under the Age of 26'
          )),
        margin = list(l = 50, t = 100)
      )
  }) 
  
  #Output race plot for Shootings tab
  output$raceshooting <- renderPlotly(
    raceshooting()
  )
  
  
  #Plot monthly arrest data
  monthplot <- reactive({
    #Make base plot for monthly arrest data
    monthbase <- ggplot(data = monthsum, aes(x = Date, y = Total, )) + 
      geom_jitter(aes(text = monthsum$monthtext), shape = 20, size = 1.5) + 
      geom_smooth(span = 0.5, se = FALSE, color = "#4040ff") + 
      scale_x_date(limits = as.Date(c("2013-12-01", "2023-01-01")), date_breaks = "1 year", date_labels = "%Y") + 
      labs(x = "Year", y = "Number of Arrests", title = "Arrests") + 
      theme_bw() 
    #Make monthly plot interactive
    ggplotly(monthbase, tooltip = "text")
  })
  
  
  #Output month plot for Arrests tab
  output$monthplot <- renderPlotly(
    monthplot()
  )
  
  #Make plot for Total Arrests by Year
  output$yearsum <- renderPlot(
    #Plot total arrests by year
    ggplot(data = yearsum, aes(x = year, y = total)) + 
      geom_bar(stat = "identity") + 
      geom_text(aes(label = total), vjust = -0.25) + 
      scale_x_continuous(breaks = yearsum$year) + 
      labs(x = "Year", 
           y = "Total Arrests", 
           title = "Total Number of Arrests by Year", 
           caption = "Data source: Philadelphia District Attorney Office") + 
      theme_bw() + 
      theme(plot.caption = element_text(face = "italic"),
            text = element_text(size = 14))
  )
  
  #Plot arrest data by generalized groups
  offenseplot <- reactive({
    #Make base plot for arrests by offense category
    offensebase <- ggplot(data = summarized_long, aes(x = Year, y = Count, colour = `Offense Category`)) + 
      geom_line() + 
      scale_x_continuous(breaks = summarized_long$Year) + 
      labs(x = "Year", y = "Number of Arrests", title = "Total Arrests by Offense Category")
    
    #Make offense plot interactive
    ggplotly(offensebase)
  })
  
  #Output offense plot
  output$offenseplot <- renderPlotly(
    offenseplot()
  )
  
  #Plot high school graduation by gender
  genderplot <- reactive({
    #Make base plot for gender
    genbase <- ggplot(grad_gen, aes(x = gradyr, y = score, group = subgroup, colour = subgroup, text = gentext)) + 
      geom_line() + 
      labs(x = "Graduation Year", y = "Graduation Rate", title = "Graduation Rates Across Gender", subtitle = "Data shown from 2014 to 2021", colour = "Gender") + 
      scale_x_continuous(breaks = grad_gen$gradyr) + 
      theme_bw() 
    
    #Make interactive
    ggplotly(genbase, tooltip = "text")
  })
  
  #Output gender plot
  output$genderplot <- renderPlotly(
    genderplot()
  )
  
  #Plot high school graduation by race
  raceschool <- reactive({
    #Make base plot
    raceschoolbase <- ggplot(grad_race, aes(x = gradyr, y = score, group = subgroup, colour = subgroup, text = racetext)) + 
      geom_line() + 
      labs(x = "Graduation Year", y = "Graduation Rate", title = "Graduation Rates Across Race", subtitle = "Data shown from 2014 to 2021", colour = "Race") + 
      scale_x_continuous(breaks = grad_race$gradyr) + 
      theme_bw() 
    
    #Make plot interactive
    ggplotly(raceschoolbase, tooltip = "text")
  })
  
  #Output race plot 
  output$raceschool <- renderPlotly(
    raceschool()
  )
  
  #Plot high school graduation and college matriculation on Philadelphia map
  mapplot <- reactive({
    #Plot base map data
    mybreaks <- c(0, 20, 40, 60, 80, 100)
    
    philamap <- fortify(phila)
    
    mapbase <- ggplot() + 
      geom_polygon(data = philamap, aes(x = long, y = lat, group = group), fill = "black", alpha = 0.3, color = "white") + 
      geom_point(data = matric_map, aes(x = long, y = lat, size = `Matriculation Rate`, colour = `Matriculation Rate`, alpha = `Matriculation Rate`, text = mytext), shape = 20, stroke = FALSE) + 
      scale_size_continuous(name = "Matriculation Rate", breaks = mybreaks, range = c(2, 6)) +
      scale_color_continuous(name = "Matriculation Rate", type = "viridis", breaks = mybreaks) + 
      scale_alpha_continuous(name = "Matriculation Rate", range = c(0.4, 0.9), breaks = mybreaks) + 
      ggtitle("Philadelphia Map Overview of Graduation and Matriculation Rate from 2020 to 2021") +
      theme_bw() +
      facet_wrap(~gradyr)
    
    #Make map interactive
    ggplotly(mapbase, tooltip = "text")
  })
  
  #Output map data 
  output$mapplot <- renderPlotly(
    mapplot()
  )
  
  #Make base bar plot
  gradmatric <- reactive({
    
    gradbase <- filter(grad_matric_long, schoolname %in% input$schools) %>%
      ggplot(
        aes(x = Rate, y = schoolname, group = Type, color = Type, fill = Type, text = gradmtext)) + 
      geom_bar(stat = "identity", position = "dodge") + 
      labs(x = "Rate", y = "School", title = "HS Graduation vs. College Matriculation Rates") + 
      scale_x_continuous(limits = c(0, 100)) + 
      facet_wrap(~gradyr)
    
    #Make bar graph interactive
    gradmatric <- ggplotly(gradbase, tooltip = "text")
  })
  #Make  bar plot of high schools by plot matriculation vs. graduation data
  output$gradmatric <- renderPlotly(
    gradmatric()
  )
}



# Run Application ---------------------------------------------------------

shinyApp(ui = ui, server = server)
