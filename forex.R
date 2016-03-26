library(gdata)
library(reshape2)
library(ggplot2)

# read forex data
forex <- read.xls('Data/Table26_3.xls')
# keep only relevant columns
forex <- forex[4:15,]
# rename the colNames
names(forex) <- c('Months', 'y2001', 'y2002', 'y2003', 'y2004', 'y2005', 
                  'y2006', 'y2007', 'y2008', 'y2009', 'y2010', 'y2011')

# explicity order the Months so that they don't appear
# alphabetically in ggplot
forex$Months <- factor(forex$Months, levels = forex$Months)
# convert factors to inetgers
forex$y2010 <- as.integer(as.character(forex$y2010))
forex$y2011 <- as.integer(as.character(forex$y2011))

# melt the forex dataframe
forex.melt <- melt(forex, id.vars = 'Months', variable.name = 'years')

# bar-plot of yearwise monthly forex earning from 2001 to 2011  
gg1 <- ggplot(forex.melt, aes(x = Months, y = value, fill = years)) 
forex_gg1 <- gg1 + 
    geom_bar(stat = 'identity') + 
    labs(title = 'Yearwise monthly forex earning', 
         x = 'Months', 
         y = 'Forex Earning (in Crore Rupees)')

# bar-plot of month-wise yearly forex earning from 2001 to 2011
gg2 <- ggplot(forex.melt, aes(x = years, y = value, fill = Months))
forex_gg2 <- gg2 + geom_bar(stat='identity') +
  labs(title = 'Month-wise yearly forex earning',
       x = 'Years',
       y = 'Forex Earning (in Crore Rupees)')


# line-plot of yearwise tourists-inflow per country from 2001 to 2010 
gg3 <- ggplot(forex.melt, aes(x = Months, y = value, group = years))
forex_gg3 <- gg3 + geom_line(aes(color = years)) +
    labs(title = 'Yearwise monthly tourists inflow', 
         y = 'Tourists', 
         x = 'Months')


# bar-plot of yearly forex earning from 2001 to 2011
forex.sum <- melt(colSums(forex[2:ncol(forex)]))
forex.sum <- data.frame(year = c(2001:2011), forex = forex.sum$value)
forex.sum$forex <- as.integer(forex.sum$forex)
forex_gg4 <- ggplot(forex.sum, aes(x = year, y = forex)) + 
    geom_bar(stat='identity', aes(fill = year)) +
    scale_x_continuous(breaks = seq(from = 2001, to = 2011, by = 1)) +
    geom_text(aes(label = forex), vjust = -0.25) + 
    labs(title = 'Foreign Exchange Earining from Tourism',
         x = 'Years',
         y = 'Earning(in Crore rupees)') + theme_bw()

# line plot of yearly forex earning from 2001 to 2010 
forex_gg5 <- ggplot(forex.sum[forex.sum$year != 2011, ], aes(x = year, y = forex)) +
    geom_point() + 
    geom_line() + 
    scale_x_continuous(breaks = seq(from = 2001, to = 2010, by = 1)) +
    theme_bw() +
    labs(title = 'Forex Earning from Tourism from 2001-2010', 
         x = 'year', 
         y = 'Earning in Crore Rupees')
