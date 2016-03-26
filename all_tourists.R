library(gdata)
library(ggplot2)
library(reshape2)
library(scales)

all_tourists <- read.xls('Data/Table26_1.xls')

# cleaning data
all_tourists <- all_tourists[,1:11]
all_tourists$TOURISM <- trimws(as.character(all_tourists$TOURISM))
all_tourists <- all_tourists[all_tourists$TOURISM != 'Total',]
all_tourists <- all_tourists[-c(1:5),]
all_tourists <- all_tourists[-c(nrow(all_tourists) - 1, nrow(all_tourists)),]

View(all_tourists)

#names(all_tourists) <- c('Nationality', 'y2001', 'y2002', 'y2003', 'y2004', 
#                         'y2005', 'y2006', 'y2007', 'y2008', 'y2009', 'y2010')
names(all_tourists) <- c('Nationality', '2001', '2002', '2003', '2004', 
                         '2005', '2006', '2007', '2008', '2009', '2010')


# Sum of all tourists from all the countries
# No country excluded
tourists.sum <- all_tourists[nrow(all_tourists),]
tourists.sum <- unlist(sapply(tourists.sum[,2:ncol(tourists.sum)], function(x) as.integer(levels(x))[x]))
tourists.sum <- data.frame(year = c(2001:2010), tourists = tourists.sum)
rownames(tourists.sum) <- NULL

tourists_gg1 <- ggplot(tourists.sum, aes(year, tourists)) + 
    geom_point() + 
    geom_line() +
    scale_x_continuous(breaks = seq(from = 2001, to = 2011, by = 1)) +
    theme_bw() + 
    labs(title = 'Tourists Inflow from 2001-2010', 
         x = 'year', 
         y = 'Toursists') +
  scale_y_continuous(labels = comma)

