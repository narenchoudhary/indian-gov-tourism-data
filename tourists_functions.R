
# Useful functions for tourists data

ReadTouristsData <- function() {
    if(!exists('table1')) {
        table1 <- read.xls("Data/Table26_1_copy.xls")
        # remove redundant columns
        table1 <- table1[,1:11]
        # rename columns
        names(table1) <- c("Nationality", "y2001", "y2002", "y2003", "y2004", 
                           "y2005", "y2006", "y2007", "y2008", "y2009", "y2010")
        # Keep rows with no null values
        table1 <- table1[!rowSums(is.na(table1)) > 0, ]
    }
    table1
}

GetTouristsByCountryYear <- function(country, year) {
    temp_df <- ReadTouristsData()
    temp_df$Nationality <- trimws(as.character(temp_df$Nationality))
    
    if(missing(country) && missing(year)) {
        stop('No arguments provided.')
    } else if (missing(country)) {
        select_(temp_df, 'Nationality', paste0('y', year))
    }else if (missing(year)) {
        filter(temp_df, Nationality == as.character(country))
    } else {
      select_(temp_df, 'Nationality', paste0('y', year)) %>% 
          filter(Nationality == as.character(country))
    }
}

PlotCountry<- function(country) {
    suppressPackageStartupMessages(library(reshape2))
    suppressPackageStartupMessages(library(ggplot2))
  
    temp_df <- ReadTouristsData()
    temp_df$Nationality <- trimws(as.character(temp_df$Nationality))
    temp_df <- temp_df %>% filter(Nationality == as.character(country))
    temp_df <- melt(temp_df, id.vars = 'Nationality')
    ggplot(temp_df, aes(x = variable , y = value)) + geom_bar(stat = 'identity')
}

CompareCountry <- function(country1, country2) {
    suppressPackageStartupMessages(library(reshape2))
    suppressPackageStartupMessages(library(ggplot2))
    suppressPackageStartupMessages(library(dplyr))
    suppressPackageStartupMessages(library(scales))
  
    temp_df <- ReadTouristsData()
    temp_df$Nationality <- trimws(as.character(temp_df$Nationality))
    temp_df <- temp_df %>% 
        filter(Nationality == as.character(country1) | Nationality == as.character(country2))
    temp_df <- melt(temp_df, id.vars = 'Nationality')
    ggplot(temp_df, aes(x = variable, y = value, fill = Nationality)) + 
        geom_bar(stat = 'identity', position = 'dodge') +
        labs(title = paste0('Comparison of tourist inflow from ',
                            country1, ' and ', country2), x = 'Year', y = 'Tourists') +
        scale_y_continuous(labels = comma)
}

