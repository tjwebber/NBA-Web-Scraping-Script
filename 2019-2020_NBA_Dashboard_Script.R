# 2019-2020 NBA Dashboard Script
rm(list = ls())
# Load Libraries
library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(stringr)

# Player ID URL
player_ID <- read_html("https://www.basketball-reference.com/leagues/NBA_2020_totals.html")
player_ID <- html_nodes(player_ID, "td") %>% html_attr("data-append-csv")
print(player_ID)
player_ID_vector <- na.omit(player_ID)
player_ID_vector <- as.vector(player_ID_vector)
player_ID_df <- as.data.frame(player_ID_vector) 



# Player Name to Url
player_name <- read_html("https://www.basketball-reference.com/leagues/NBA_2020_totals.html")
player_name <- html_nodes(player_name, "td") %>% html_attr("csk")
player_name_vector <- na.omit(player_name)
player_name_vector <- as.vector(player_name_vector)
player_name_df <- as.data.frame(player_name_vector)
# Separate Name
player_name_df2 <- separate(player_name_df, col = player_name_vector, into = c("Last", "First"),sep = ",")
# Combine Name
player_name_df <- unite(player_name_df2, Full_Name, First, Last, sep = " ")

# Create Data Frame
player_info_2020 <- cbind(player_name_df, player_ID_df)
player_info_2020$letter <- substr(player_info_2020$player_ID_vector,1,1)

player_info_2020 <- distinct(player_info_2020)

# Paste together the URL Links
# Player Pictures Urls
for (i in player_info_2020$player_ID_vector) {
  player_info_2020$player_picture_urls <- paste("https://d2cwpp38twqe55.cloudfront.net/req/201911262/images/players/",player_info_2020$player_ID_vector,".jpg",sep = "")
}

# Game Log Urls Data Frame Column
for (i in player_info_2020$player_ID_vector) {
  player_info_2020$game_log_urls <- paste("https://www.basketball-reference.com/players/",player_info_2020$letter,"/",player_info_2020$player_ID_vector,"/gamelog/2020",sep = "")
}

# Game Log Urls Vector
for (i in player_info_2020$player_ID_vector) {
  game_log_urls <- paste("https://www.basketball-reference.com/players/",player_info_2020$letter,"/",player_info_2020$player_ID_vector,"/gamelog/2020",sep = "")
  game_log_urls <- as.vector(game_log_urls)
}

# GET GAME LOGS BABY
#rm(total_data)

total_data <- data.frame()


for (p in game_log_urls) {
  page <- read_html(p)
  table <- html_table(page, fill = TRUE)
  current_table_value <- table[[8]]
  current_table_value <- current_table_value[,-c(31)]
  

  current_table_value$Name[p] <- html_nodes(page,"title") %>% html_text()
  
  get_measurables <- html_nodes(page, "p")
  current_table_value$pos1[p] <- get_measurables[2] %>% html_text(trim = TRUE)
  current_table_value$pos2[p] <- get_measurables[3] %>% html_text(trim = TRUE)
  current_table_value$pos3[p] <- get_measurables[4] %>% html_text(trim = TRUE)
  current_table_value$pos4[p] <- get_measurables[5] %>% html_text(trim = TRUE)

  
  current_table_value$hw1[p] <- get_measurables[3] %>% html_text(trim = TRUE)
  current_table_value$hw2[p] <- get_measurables[4] %>% html_text(trim = TRUE)
  current_table_value$hw3[p] <- get_measurables[5] %>% html_text(trim = TRUE)
  current_table_value$hw4[p] <- get_measurables[6] %>% html_text(trim = TRUE)
  
  total_data <- rbind(total_data, current_table_value) 
  print(tail(total_data$Name))
}


# Get Playoff Data
rm(playoff_data)
playoff_data <- data.frame()

for (i in game_log_urls) {
  page <- read_html(i)
  playoffs_table <- page %>%
    html_nodes(xpath = '//comment()') %>% 
    html_text() %>%
    paste(collapse = '') %>%
    read_html()
  
  playoffs_gl <- html_table(playoffs_table, header = T, fill = T)
  playoff_table_value <- playoffs_gl[[1]]
  
  playoff_table_value$Name[p] <- html_nodes(page,"title") %>% html_text()

  
  playoff_data <- rbind(playoff_data, playoff_table_value) 
  print(tail(playoff_data$Name))
  
}

# Data Cleaning and transformation

# total_data <- total_data_test
# total_data_test <- total_data

colnames(total_data)[2] <- "GP"
colnames(total_data)[5] <- "Team"
colnames(total_data)[6] <- "Home_Away"
colnames(total_data)[8] <- "Win_Loss"

total_data$`Home_Away`[total_data$`Home_Away`==""] <- "Home"
total_data$`Home_Away`[total_data$`Home_Away`=="@"] <- "Away"


total_data <- total_data[!(total_data$GP == "G" & total_data$Team =="Tm" & total_data$`Win_Loss` == ""),]

total_data <- total_data[!(total_data$GP == ""),]

total_data <- na.omit(total_data)

# Separate Win/Loss Column
Player_Data_2020 <- separate(total_data, col = Win_Loss, into = c("Win_Loss","Game Margin"),sep = " ")

# Separate Name Column
Player_Data_2020 <- separate(Player_Data_2020, col = Name, into = c("First","Last", NA),sep = " ")
# Combine First and Last Name
Player_Data_2020 <- unite(Player_Data_2020, Full_Name, First, Last, sep = " ")

# Separate MP
Player_Data_2020 <- separate(Player_Data_2020, col = MP, into = c("Min","Sec"),sep = ":")
Player_Data_2020$Sec <- as.numeric(as.character(Player_Data_2020$Sec)) / 60

Player_Data_2020$Win_Loss[Player_Data_2020$Win_Loss=="W"] <- "Win"
Player_Data_2020$Win_Loss[Player_Data_2020$Win_Loss=="L"] <- "Loss"

Player_Data_2020$pos1[substr(Player_Data_2020$pos1,0,1)!="P"] <- ""
Player_Data_2020$pos2[substr(Player_Data_2020$pos2,0,1)!="P"] <- ""
Player_Data_2020$pos3[substr(Player_Data_2020$pos3,0,1)!="P"] <- ""
Player_Data_2020$pos4[substr(Player_Data_2020$pos4,0,1)!="P"] <- ""


Player_Data_2020 <- unite(Player_Data_2020,Position, pos1, pos2, pos3, pos4, sep = "")

Player_Data_2020 <- separate(Player_Data_2020, col = Position, into = c("c1", "Position","c3"),sep = ":")
Player_Data_2020$c1 <-NULL
colnames(Player_Data_2020)[35] <- "Shooting_Hand"
Player_Data_2020$Position <- trimws(Player_Data_2020$Position)
Player_Data_2020$Position <- substr(Player_Data_2020$Position,1,14)
Player_Data_2020$Position[substr(Player_Data_2020$Position,0,1)=="C"] <- "Center"
Player_Data_2020$Position[substr(Player_Data_2020$Position,0,4)=="Poin"] <- "Point Guard"
Player_Data_2020$Position[substr(Player_Data_2020$Position,0,4)=="Smal"] <- "Small Forward"
Player_Data_2020$Position[substr(Player_Data_2020$Position,0,4)=="Shot"] <- "Shooting Guard"
Player_Data_2020$Position[substr(Player_Data_2020$Position,0,4)=="Powe"] <- "Power Forward"
Player_Data_2020$Position[substr(Player_Data_2020$Full_Name,0,4)=="Gian"] <- "Power Forward"
Player_Data_2020$Position[substr(Player_Data_2020$Full_Name,0,4)=="Lebr"] <- "Small Forward"

Player_Data_2020$Position <- trimws(Player_Data_2020$Position)


# Starts with P, T, B
# Height Weight 1
Player_Data_2020$hw1[substr(Player_Data_2020$hw1,0,1)=="P"] <- ""
Player_Data_2020$hw1[substr(Player_Data_2020$hw1,0,1)=="T"] <- ""
Player_Data_2020$hw1[substr(Player_Data_2020$hw1,0,1)=="B"] <- ""
Player_Data_2020$hw1[substr(Player_Data_2020$hw1,0,1)=="("] <- ""
# Height Weight 2
Player_Data_2020$hw2[substr(Player_Data_2020$hw2,0,1)=="P"] <- ""
Player_Data_2020$hw2[substr(Player_Data_2020$hw2,0,1)=="T"] <- ""
Player_Data_2020$hw2[substr(Player_Data_2020$hw2,0,1)=="B"] <- ""
Player_Data_2020$hw2[substr(Player_Data_2020$hw2,0,1)=="("] <- ""
# Height Weight 3
Player_Data_2020$hw3[substr(Player_Data_2020$hw3,0,1)=="P"] <- ""
Player_Data_2020$hw3[substr(Player_Data_2020$hw3,0,1)=="T"] <- ""
Player_Data_2020$hw3[substr(Player_Data_2020$hw3,0,1)=="B"] <- ""
Player_Data_2020$hw3[substr(Player_Data_2020$hw3,0,1)=="("] <- ""
# Height Weight 4
Player_Data_2020$hw4[substr(Player_Data_2020$hw4,0,1)=="P"] <- ""
Player_Data_2020$hw4[substr(Player_Data_2020$hw4,0,1)=="T"] <- ""
Player_Data_2020$hw4[substr(Player_Data_2020$hw4,0,1)=="B"] <- ""
Player_Data_2020$hw4[substr(Player_Data_2020$hw4,0,1)=="("] <- ""

Player_Data_2020 <- unite(Player_Data_2020,Hieght_Weight, hw1, hw2, hw3, hw4, sep = "")

Player_Data_2020$Hieght_Weight <- substr(Player_Data_2020$Hieght_Weight,0,11)
Player_Data_2020$Hieght_Weight <- trimws(Player_Data_2020$Hieght_Weight)


# Create Team_Stats Table
# Roll up data through SQL
# install.packages("sqldf")

library(sqldf)

Team_Stats <- Player_Data_2020[,c(0:3,5:9,11:30)]

colnames(Team_Stats)[8] <- "Game_Margin"
colnames(Team_Stats)[14] <- "Three_PT"
colnames(Team_Stats)[15] <- "Three_PTA"


Team_Stats_Final <- sqldf("SELECT 
                            Date, Team, Opp, Home_Away, Win_Loss, Game_Margin,
                            SUM(Min) AS Min,
                            SUM(Sec) AS Sec,
                            (SUM(Min) + SUM(Sec)) AS MP,
                            SUM(FG) AS FG,
                            SUM(FGA) AS FGA,
                            SUM(Three_PT) AS Three_PT,
                            SUM(Three_PTA) AS Three_PTA,
                            SUM(FT) AS FT,
                            SUM(FTA) AS FTA,
                            SUM(ORB) AS ORB,
                            SUM(DRB) AS DRB,
                            SUM(TRB) AS TRB,
                            SUM(AST) AS AST,
                            SUM(STL) AS STL,
                            SUM(BLK) AS BLK,
                            SUM(TOV) AS TOV,
                            SUM(PF) AS PF,
                            SUM(PTS) AS PTS
                          FROM Team_Stats
                          GROUP BY Date, Team, Opp, Home_Away, Win_Loss, Game_Margin
                          ORDER BY Team ASC, Date ASC;")


