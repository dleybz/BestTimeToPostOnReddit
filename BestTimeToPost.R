#These lines will check if you have the necessary packages installed, installs them if they are not already installed, and opens them
list.of.packages <- c("bigrquery", "ggplot2", "methods", "dplyr", "grid")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#These lines open the libraries referenced in the following code
library(bigrquery)
library(ggplot2)
library(methods)
library(dplyr)
library(grid)

###
subreddit_desired <- "news" #INSERT SUBREDDIT YOU WISH TO ACCESS
project_name <- "serene-boulder-108923" #INSERT PROJECT NAME

sql <- "SELECT
  DAYOFWEEK(SEC_TO_TIMESTAMP(created - 60*60*5)) as day_as_int,
  HOUR(SEC_TO_TIMESTAMP(created - 60*60*5)) as hour_as_int,
  SUM(score) as total_score,
FROM [fh-bigquery:reddit_posts.full_corpus_201509]
WHERE subreddit=\"news\" /*insert subreddit of choice here*/
GROUP BY hour_as_int, day_as_int
ORDER BY day_as_int, hour_as_int"

#This line queries BigQuery and creates a new dataframe called "starting_table". The first time you do this you'll need to enter credentials; simply follow the instructions in the console
starting_table <- tbl_df(query_exec(sql, project=project_name, max_pages=Inf))

#These lines create dataframes which will be used to change the days of the week and hours of the day from their integer identifications to plain-text identifications
day_as_char <- data_frame(day_as_int = 1:7, day_as_char = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
hour_as_char <- data_frame(hour_as_int = 0:23, hour_as_char = c(paste(c(12,1:11),"AM"), paste(c(12,1:11),"PM")))

#This line creates a new dataframe composed of the three old dataframes we just made
ending_table <- starting_table %>% left_join(day_as_char) %>% left_join(hour_as_char)

#These lines level the variables; this is to make sure that our days of the week are in Sunday-Saturday order, instead of in alphabetical order
ending_table$day_as_char <- factor(ending_table$day_as_char, level = rev(day_as_char$day_as_char))
ending_table$hour_as_char <- factor(ending_table$hour_as_char, level = hour_as_char$hour_as_char)

#This line creates the title which will be generated in the image, which includes the best day and time to post
the_title<-paste("The best time to post on /r/",subreddit_desired, "is\n", ending_table$day_as_char[ending_table$total_score == max(ending_table$total_score)], "at", ending_table$hour_as_char[ending_table$total_score == max(ending_table$total_score)])

#These lines will generate the png (saved to your working directory, named after the subreddit selected)  
png(filename=paste(subreddit_desired, ".png"), width=1000)
ggplot(ending_table, aes(x=hour_as_char, y=day_as_char, fill=total_score)) +
  geom_tile() +
  coord_fixed() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6), legend.direction="horizontal", legend.position="top", legend.key.width=unit(5, "cicero"), legend.text = element_text(size=0), legend.margin = unit(0, "cm"), plot.title = element_text(size = rel(2))) + #plot.background = element_rect(fill="alice blue")
  labs(x = "", y = "", title = the_title) +
  scale_fill_continuous(low = "gray86", high = "deepskyblue3", name = "")
dev.off()
