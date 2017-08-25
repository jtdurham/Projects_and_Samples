setwd("C:\\Users\\Jordan\\Documents\\Fantasy Football\\Yahoo Content")

library(XML)
library(RCurl)

source("Descriptive Analysis Functions.R")

# Stats Tables

whole_file = htmlParse("Fantasy Player Stats Test.html")
all_data = xpathApply(whole_file, "//div[contains(@class, \"players\")]//tr//td//descendant-or-self::*[last()]", saveXML)
all_data = gsub("%", "", all_data, fixed = TRUE)
tweeners = gregexpr(">.*?<", all_data)

for(i in 1:length(tweeners))
{
  all_data[[i]] = ifelse(tweeners[[i]] >= 0, substr(all_data[[i]], tweeners[[i]] + 1, tweeners[[i]] + attributes(tweeners[[i]])$match.length - 2), "")
}

player_data = list()
#player = 0 #list version

#item = 1 #list version
#modifier = 0 #list version
skip_one = FALSE

item = 0 #vector version
stop_make = FALSE #vector version

for(i in 2:length(all_data))
{
  if((!is.na(as.numeric(all_data[[i-1]])) & is.na(as.numeric(all_data[[i]]))) | i == 2)
  {
    if(i != 2)
    {
      #player_data[[player]][[item + modifier]] = all_data[[i-1]] #list version
      if(!stop_make) #vector version
      {
        player_data[[item]] = character()
      }
      
      player_data[[item]] = c(player_data[[item]], all_data[[i-1]]) #vector version
    }
    
    #player = player + 1 #list version
    #player_data[[player]] = list() #list version
    item = 1
    #modifier = 0 #list version
    
    if(i != 2)
    {
      skip_one = TRUE
      stop_make = TRUE #vector version
      next
    }
  }
  
  if(skip_one)
  {
    skip_one = FALSE
    next
  }
  
  if(!stop_make) #vector version
  {
    player_data[[item]] = character()
  }
  
  #player_data[[player]][[item + modifier]] = all_data[[i-1]] #list version
  player_data[[item]] = c(player_data[[item]], all_data[[i-1]]) #vector version
  
  if((grepl("[@]", all_data[[i-1]]) | grepl(" +(vs) +", all_data[[i-1]])) & nchar(all_data[[i]]) != 0)
  {
    #modifier = modifier + 1 #list version
    item = item + 1 #vector version
    
    if(!stop_make) #vector version
    {
      player_data[[item]] = character()
    }
    
    player_data[[item]] = c(player_data[[item]], "None") #vector version
  }
  
  item = item + 1
}

#player_data = player_data[-length(player_data)] #list version

## Remove unwanted vectors

player_data[c(1, 8)] = NULL

## Parse vectors where needed

split_vectors = list()
sv_ind = 1

split_1 = strsplit(player_data[[3]], " - ", fixed = TRUE)
split_2 = strsplit(player_data[[5]], " ", fixed = TRUE)
player_data[c(3, 5)] = NULL

for(i in 1:length(split_1[[1]]))
{
  split_vectors[[sv_ind]] = sapply(split_1, "[", i)
  sv_ind = sv_ind + 1
}

for(i in 1:length(split_2[[1]]))
{
  split_vectors[[sv_ind]] = sapply(split_2, "[", i)
  sv_ind = sv_ind + 1
}

split_vectors[[5]] = ifelse(split_vectors[[5]] == "pm", 1, 0)
split_vectors[[6]] = ifelse(split_vectors[[6]] == "vs", 1, 0)

yahoo_offense_names = c("Note_Available", "Player_Name", "Injury_Status", 
                        "Environment_Condition", "Owner", "Games_Played",
                        "Bye_Week", "Fantasy_Points", "Owned_Perc",
                        "Proj_Rank", "Act_Rank", "Pass_Yards", "Pass_TD",
                        "Pass_Int", "Rush_Att", "Rush_Yards", "Rush_TD",
                        "Rec_Targ", "Recepts", "Rec_Yards", "Rec_TD",
                        "Two_Pt_Conv", "Fum_Lost")

yahoo_offense_split_names = c("Player_Team", "Position", "Game_Day", "Game_Time",
                              "PM_Time", "Home", "Opponent")

names(player_data) = yahoo_offense_names
names(split_vectors) = yahoo_offense_split_names

player_data = as.data.frame(player_data, stringsAsFactors = FALSE)
split_vectors = as.data.frame(split_vectors, stringsAsFactors = FALSE)
player_data = cbind(player_data, split_vectors)

numeric_vars = c("Games_Played", "Fantasy_Points", "Owned_Perc", "Proj_Rank", "Act_Rank", "Pass_Yards",
                 "Pass_TD", "Pass_Int", "Rush_Att", "Rush_Yards", "Rush_TD", "Rec_Targ",
                 "Recepts", "Rec_Yards", "Rec_TD", "Two_Pt_Conv", "Fum_Lost", "PM_Time", "Home")

for(nm in numeric_vars)
{
  player_data[[nm]] = as.numeric(player_data[[nm]])
}

### ADP Tables

draft_type = "SD"
draft_type_att = "tab"
pos_inc = "ALL"
pos_inc_att = "pos"
sorter = "DA_AP"
sorter_att = "sort"
sort_dir = "1"
sort_dir_att = "sdir"
pages = 0
pages_att = "count"

linky = "https://www.fantasypros.com/nfl/rankings/consensus-cheatsheets.php?partner=yahoo_dropdown"
whole_file = getURL(linky)
#whole_file = htmlParse("ADP_Page_1.html")
adp_data = readHTMLTable(whole_file, header = F, stringsAsFactors = FALSE)$data
adp_data = adp_data[-c(1, which(adp_data[[3]] == "(EDIT)"), which(is.na(adp_data[[2]]))),-c(1,length(adp_data))]
rownames(adp_data) = 1:nrow(adp_data)
adp_names = c("Player", "Position", "Bye", "Best_Rank", "Worst_Rank", "Avg_Rank", "Std_Dev_Rank", "ADP",
              "Experts_vs_ADP")
names(adp_data) = adp_names

play_team = strsplit(adp_data$Player, " ")
spl_lens = lengths(play_team)
team_parse = sapply(play_team, "[", min(spl_lens):max(spl_lens))
teams = character(ncol(team_parse))
for(i in 1:ncol(team_parse))
{
  teams[i] = team_parse[spl_lens[i]-1, i]
}
elong = sort(teams[which(nchar(teams) > 3)], decreasing = F)
shor = sort(unique(teams[-which(nchar(teams) > 3)]), decreasing = F)
shor = shor[c(6, 7, 4, 10, 31, 5, 18, 17, 9, 20, 27, 2, 24, 16, 25, 11, 13, 5, 22, 26, 19, 3, 33, 23, 29, 28, 14, 32, 21)]
e_to_s = data.frame(Full = elong, Short = shor, stringsAsFactors = F)
for(l in elong)
{
  teams[which(teams == l)] = e_to_s[which(e_to_s[["Full"]] == l), "Short"]
}

adp_data$Team = teams

for(sh in unique(teams))
{
  adp_data$Player = gsub(sh, "", adp_data$Player)
}

adp_data$Player = trimws(adp_data$Player, which = "right")
adp_data$Position = gsub("[0-9]+", "", adp_data$Position)

adp_data$Experts_vs_ADP = gsub("\\+", "", adp_data$Experts_vs_ADP)

nums = adp_names[-which(adp_names %in% c("Player", "Team", "Position"))]
for(nm in nums)
{
  adp_data[[nm]] = as.numeric(adp_data[[nm]])
}

adp_data$Year = rep(2016, nrow(adp_data))

## Draft History

whole_file = htmlParse("Draft History 2016.html")

fant_teams = xpathApply(whole_file, "//h3//text()", saveXML)[-1]

hist_data = readHTMLTable(whole_file, header = F, stringsAsFactors = FALSE)
hist_data[[1]] = NULL

for(i in 1:length(hist_data))
{
  hist_data[[i]] = hist_data[[i]][1:2]
  names(hist_data[[i]]) = c("Player_Info", "Draft_Info")
  hist_data[[i]]$Player_Info = gsub("\\).*", "\\)", hist_data[[i]]$Player_Info)
  hist_data[[i]]$Player = trimws(gsub("\\(.*?\\)", "", hist_data[[i]]$Player_Info), which = "right")
  team_pos = substr(hist_data[[i]]$Player_Info, regexpr("\\(.*?\\)", hist_data[[i]]$Player_Info),
                    regexpr("\\(.*?\\)", hist_data[[i]]$Player_Info) + 
                    attributes(regexpr("\\(.*?\\)", hist_data[[i]]$Player_Info))$match.length)
  team_pos = gsub("[()]", "", team_pos)
  team_pos_spl = strsplit(team_pos, " - ")
  hist_data[[i]]$Team = sapply(team_pos_spl, "[", 1)
  hist_data[[i]]$Position = sapply(team_pos_spl, "[", 2)
  hist_data[[i]]$Player_Info = NULL
  
  hist_data[[i]]$Draft_Info = gsub("[^,0-9]", "", hist_data[[i]]$Draft_Info)
  rnd_pick = strsplit(hist_data[[i]]$Draft_Info, ",")
  hist_data[[i]]$Draft_Round = sapply(rnd_pick, "[", 1)
  hist_data[[i]]$Draft_Pick = sapply(rnd_pick, "[", 2)
  hist_data[[i]]$Draft_Info = NULL
  
  numbers = c("Draft_Round", "Draft_Pick")
  for(nm in numbers)
  {
    hist_data[[i]][[nm]] = as.integer(hist_data[[i]][[nm]])
  }
  
  hist_data[[i]]$Year = rep(2015, nrow(hist_data[[i]]))
  hist_data[[i]]$Fant_Team = rep(fant_teams[[i]], nrow(hist_data[[i]]))
  if(i == 1)
  {
    history_data = hist_data[[i]]
  }
  else
  {
    history_data = rbind(history_data, hist_data[[i]])
  }
}

history_data$Draft_Position = (max(history_data$Draft_Pick, na.rm = T) * history_data$Draft_Round) - max(history_data$Draft_Pick, na.rm = T) + history_data$Draft_Pick

write.table(history_data, file="Yahoo_ADP_2016.tsv", sep="\t", row.names = F)
