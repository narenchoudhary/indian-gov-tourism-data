library(gdata)
library(ggplot2)
library(reshape2)
library(gganimate)


# "Data/Table26_1_copy.xls" differs from original data file.
# Continents and other fields have been removed to focus on country-wise data.

# read xls 
table1 <- read.xls("Data/Table26_1_copy.xls")
# column names
names(table1)
# remove redundant columns
table1 <- table1[,1:11]
# rename columns
names(table1) <- c("Nationality", "y2001", "y2002", "y2003", "y2004", 
                   "y2005", "y2006", "y2007", "y2008", "y2009", "y2010")
# Keep rows with no null values
table1 <- table1[!rowSums(is.na(table1)) > 0, ]
# Add new columns = Sum of tourists from year 2001 to 2010
table1$sum <- rowSums(table1[,2:11])
# Order all rows by table1$Sum 
table1 <- table1[order(-table1$sum),]
# Subset = top 20 rows
table1.top20 <- head(table1, 20)
# Change the order of factor levels by specifying the order explicitly
table1.top20$Nationality <- factor(table1.top20$Nationality, 
                                      levels = table1.top20$Nationality[order(table1.top20$sum)])

table1_bar <- ggplot(table1.top20, aes(Nationality, sum))
table1_bar <- table1_bar + geom_bar(stat='identity') + 
    geom_text(aes(label=sum), vjust = -0.5) +
    theme(axis.text.x = element_text(angle=90)) + 
    scale_y_continuous(breaks = seq(0,max(table1$sum) + 500, by = 1000000))



# graph with gg_animate
# Take countries with top 20 tourists inflow
# and remove the sum columns
cols <- ncol(table1.top20) - 1
gganimate_data <- table1.top20[,1:cols]
names(gganimate_data) <- c('Nationality', '2001', '2002', '2003', '2004', 
                         '2005', '2006', '2007', '2008', '2009', '2010')
gganimate_data <- melt(gganimate_data, id.vars = 'Nationality')

gganimate_plot <- ggplot(gganimate_data, aes(x=Nationality, y = value, frame = variable)) +
    geom_bar(stat='identity', position = 'identity') + 
    theme(axis.text.x = element_text(angle = 90))