library(shiny)

#all <- read.csv('tmp.csv')
library(readxl)
library(ggplot2)
library(dplyr)

ebola_data <- function(year) {
  data <- read_xls(paste0('data/', year, '.xls'))
  data <- data[-c(1:5, 46:49), 1:5]
  colnames(data) <- c('Type', 'Incidents', 'Offenses', 'Victims', 'KnownOffenders')
  data <- data[data$Type != 'Race/Ethnicity/Ancestry:' &
                 data$Type != 'Religon:' &
                 data$Type != 'Sexual Orientation:' &
                 data$Type != 'Disability:' & 
                 data$Type != 'Gender:' & 
                 data$Type != 'Gender Identity:', ]
  data$Type <- ifelse(data$Type == 'Anti-Black or African American',
                      'Anti-Black',
                      data$Type)
  data$Type <- ifelse(data$Type == 'Anti-American Indian or Alaska Native',
                      'Anti-American Indian/Alaskan Native',
                      data$Type)
  data$Type <- ifelse(data$Type == 'Anti-Asian',
                      'Anti-Asian/Pacific Islander',
                      data$Type)
  data <- data[data$Type == 'Anti-White' |
                 data$Type == 'Anti-Black' |
                 data$Type == 'Anti-American Indian/Alaskan Native' |
                 #data$Type == 'Anti-Asian' | 
                 data$Type == 'Anti-Native Hawaiian or Other Pacific Islander' | 
                 data$Type == 'Anti-Multiple Races, Group' |
                 data$Type == 'Anti-Arab' |
                 data$Type == 'Anti-Asian/Pacific Islander' |
                 data$Type == 'Anti-Hispanic or Latino' |
                 data$Type == 'Anti-Other Race/Ethnicity/Ancestry'
               , ]
  data$Incidents <- as.integer(data$Incidents)
  data$Offenses <- as.integer(data$Offenses)
  data$Victims <- as.integer(data$Victims)
  data$KnownOffenders <- as.integer(data$KnownOffenders)
  data$Year <- year
  return(data)
}

data2018 <- ebola_data(2018)
data2017 <- ebola_data(2017)
data2016 <- ebola_data(2016)
data2015 <- ebola_data(2015)
data2014 <- ebola_data(2014)
data2013 <- ebola_data(2013)
data2012 <- ebola_data(2012)
data2011 <- ebola_data(2011)
data2010 <- ebola_data(2010)
data2009 <- ebola_data(2009)
data2008 <- ebola_data(2008)
data2007 <- ebola_data(2007)
data2006 <- ebola_data(2006)
data2005 <- ebola_data(2005)
data2004 <- ebola_data(2004)
data2003 <- ebola_data(2003)
data2002 <- ebola_data(2002)
data2001 <- ebola_data(2001)
data2000 <- ebola_data(2000)
data1999 <- ebola_data(1999)
data1998 <- ebola_data(1998)
data1997 <- ebola_data(1997)

all <- rbind(data2018,
             data2017,
             data2016,
             data2015,
             data2014, 
             data2013, 
             data2012, 
             data2011, 
             data2010, 
             data2009, 
             data2008, 
             data2007, 
             data2006, 
             data2005, 
             data2004,
             data2003,
             data2002,
             data2001,
             data2000,
             data1999,
             data1998,
             data1997)
all$Event <- rep(NA, nrow(all))
all$Event <- ifelse(all$Year %in% c(2012:2018),
                     'election2016',
                    all$Event)
all$Event <- ifelse(all$Year %in% c(2005:2011),
                    'election2008',
                    all$Event)
all$Event <- ifelse(all$Year %in% c(2009:2015),
                    'election2012',
                    all$Event)
all$Event <- ifelse(all$Year %in% c(2001:2007),
                    'election2004',
                    all$Event)
all$Event <- ifelse(all$Year %in% c(1997:2003),
                    'election2000',
                    all$Event)
all$Event <- ifelse(all$Year %in% c(1997:2003),
                    'election2000',
                    all$Event)


shinyServer(function(input,output){
  
  # category <- c("MA", "CA", "NY", "NY")
  # population <- c(3,8,4, 8)
  # 
  # df <- data.frame(category,population)
  
  df_subset <- reactive({
    a <- subset(all, Event == input$state)
    return(a)
  })
  
  #output$table1 <- renderTable(df_subset()) #Note how df_subset() was used and not df_subset
  #output$incidentPlot <- renderPlot(plot(df_subset()$Incidents))
  output$incidentPlot <- renderPlot({
    ggplot(df_subset(),aes(x=Year,
                                     y=Incidents,
                                     color=Type
    )) +
      geom_line()
  })
  
})