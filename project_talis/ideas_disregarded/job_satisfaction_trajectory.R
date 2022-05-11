# Load packages
library(dplyr)
library(tidyverse)
library(readxl)

# Load data
votes <- read.csv('data/votes.csv')

# Sort dataframe by employee
votes <- arrange(votes, employee)

# Load other data
churn <- read.csv('data/churn.csv')
interactions <- read.csv('data/commentInteractions.csv')
comments <- read.csv('data/comments_clean_anonimized.csv')
ibm <- read.csv('data/ibm.csv')
acl <- load('data/american_changing_lives.rda')
# Using ibm, group by
# plot JobSatisfaction vs JobInvolvement by department
ggplot(ibm, aes(x=JobInvolvement, y=JobSatisfaction, color=factor(Department) )) +
  geom_point()

# Using votes dataset, group by
# plot vote vs time by department
str(votes)
votes$companyAlias <- factor(votes$companyAlias)
levels(votes$companyAlias) <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","V","W","X","Y","Z","AA","AB","AC","AD","AE","AF","AG","AH","AI","AJ","AK", "AL")
votes$uid <- paste(votes$employee,votes$companyAlias)
# parse dates
# votes$dt <- as.POSIXct((votes$voteDate), origin="1970-01-01")
votes$voteDate<- gsub("CEST", "", votes$voteDate)
votes$voteDate<- gsub("CET", "", votes$voteDate)
votes$voteDate<- gsub("  ", " ", votes$voteDate)
votes$voteDate<- as.Date(votes$voteDate, format = "%a %b %d %H:%M:%S %Y")
ggplot(votes, aes(x=voteDate, y=vote),color=factor(companyAlias)) +
  geom_point()
