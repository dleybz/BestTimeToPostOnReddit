#These lines will check if you have the necessary packages installed, installs them if they are not already installed, and opens them
list.of.packages <- c("bigrquery", "ggplot2", "methods", "dplyr", "grid")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(bigrquery)
library(ggplot2)
library(methods)
library(dplyr)
library(grid)

###
subreddit_desired <- "news" #INSERT SUBREDDIT YOU WISH TO ACCESS
project_name <- "serene-boulder-108923" #INSERT PROJECT NAME

sql <- "SELECT CASE
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=0 THEN '12AM'
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=1 THEN '1AM'
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=2 THEN '2AM'
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=3 THEN '3AM'
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=4 THEN '4AM'
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=5 THEN '5AM'
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=6 THEN '6AM'
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=7 THEN '7AM'
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=8 THEN '8AM'
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=9 THEN '9AM'
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=10 THEN '10AM'
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=11 THEN '11AM'
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=12 THEN '12PM'
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=13 THEN '1PM'
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=14 THEN '2PM'
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=15 THEN '3PM'
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=16 THEN '4PM'
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=17 THEN '5PM'
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=18 THEN '6PM'
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=19 THEN '7PM'
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=20 THEN '8PM'
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=21 THEN '9PM'
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=22 THEN '10PM'
WHEN HOUR(SEC_TO_TIMESTAMP(created - 60*60*5))=23 THEN '11PM'
END as hour_as_char,
WHEN DAYOFWEEK(SEC_TO_TIMESTAMP(created - 60*60*5))=1 THEN 'Sunday'
WHEN DAYOFWEEK(SEC_TO_TIMESTAMP(created - 60*60*5))=2 THEN 'Monday'
WHEN DAYOFWEEK(SEC_TO_TIMESTAMP(created - 60*60*5))=3 THEN 'Tuesday'
WHEN DAYOFWEEK(SEC_TO_TIMESTAMP(created - 60*60*5))=4 THEN 'Wednesday'
WHEN DAYOFWEEK(SEC_TO_TIMESTAMP(created - 60*60*5))=5 THEN 'Thusday'
WHEN DAYOFWEEK(SEC_TO_TIMESTAMP(created - 60*60*5))=6 THEN 'Friday'
WHEN DAYOFWEEK(SEC_TO_TIMESTAMP(created - 60*60*5))=7 THEN 'Saturday'
END as day_as_char,
HOUR(SEC_TO_TIMESTAMP(created - 60*60*5)) as hour_as_int,
DAYOFWEEK(SEC_TO_TIMESTAMP(created - 60*60*5)) as day_as_int,
SUM(score) as total_score,
FROM [fh-bigquery:reddit_posts.full_corpus_201509]
WHERE subreddit=subreddit_desired
GROUP BY hour_as_int, day_as_int, day_as_char, hour_as_char
ORDER BY day_as_int, hour_as_int, day_as_char, hour_as_char"

#This line queries BigQuery and creates a new dataframe called "table". The first time you do this you'll need to enter credentials; simply follow the instructions in the console
table <- tbl_df(query_exec(sql, project=project_name, max_pages=Inf))

#These lines level the variables; this is to make sure that our days of the week are in Sunday-Saturday order, instead of in alphabetical order
table$day_as_char <- factor(table$day_as_char, level = rev(day_as_char$day_as_char))
table$hour_as_char <- factor(table$hour_as_char, level = hour_as_char$hour_as_char)

#This line creates the title which will be generated in the image, which includes the best day and time to post
the_title<-paste("The best time to post on /r/",subreddit_desired, "is\n", table$day_as_char[table$total_score == max(table$total_score)], "at", table$hour_as_char[table$total_score == max(table$total_score)])

#These lines will generate the png (saved to your working directory, named after the subreddit selected)  
png(filename=paste(subreddit_desired, ".png"), width=1000)
ggplot(table, aes(x=hour_as_char, y=day_as_char, fill=total_score)) +
  geom_tile() +
  coord_fixed() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6), legend.direction="horizontal", legend.position="top", legend.key.width=unit(5, "cicero"), legend.text = element_text(size=0), legend.margin = unit(0, "cm"), plot.title = element_text(size = rel(2))) + #plot.background = element_rect(fill="alice blue")
  labs(x = "", y = "", title = the_title) +
  scale_fill_continuous(low = "gray86", high = "deepskyblue3", name = "")
dev.off()