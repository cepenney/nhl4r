
#### Preamble ####
library("plyr")
library("dplyr")
# library("tidyr")
# library("ggplot2")
# library("grid")
# library("plm")
# library("apsrtable")
# library("car")
# library("gplots")
# library("ivpack")
# library("stargazer")
# library("foreign")
# library("stringr")
# library("scales")
# library("distr")
# library("triangle")
# library("zoo")
# library("fitdistrplus")
# library("fGarch")
# library("rugarch")
# library("xts")
# library("ggrepel")
library("jsonlite")

# setwd("")

dev.off(dev.list()["RStudioGD"]) # clear all plots
rm(list=ls()) # clear memory
options(scipen=999) # get rid of annoying scientific notation

#### # # # Work # # # ####

#### Grabbing Team IDs ####
# api root
root <- "https://statsapi.web.nhl.com/api/v1"
# https://statsapi.web.nhl.com/api/v1/people/ID
# ?stats=gameLog&season=20162017 Provides a game log showing stats for each game of a season

# playerIDlist <- jsonlite::fromJSON(paste0(root,"/people/",8477474))

# https://statsapi.web.nhl.com/api/v1/people/8477474/stats?stats=statsSingleSeason&season=20172018
# so the root is followed by /people/, then player_id, then the query which starts with /stats?stats=...

# need a list of nhl player IDs, don't I?

# and first I need to get the team rosters and extract the player IDs...

# https://statsapi.web.nhl.com/api/v1/teams provides everything we need.

teamList <- fromJSON(paste(root,"/teams/",sep=""))

# Unlist this shit
# teamList <- data.frame(teamList[2]) 
# not necessary, just use $ to access component I need

teamIDs <- teamList$teams$id
rm(teamList)

#### Grabbing Player IDs from Rosters (pulling Rosters) ####

# Now pull Rosters, only need to grab player IDs.
# This one is a bit complicated. I need to go to each team, grab the player IDs, chuck the rest.

# Turns out I can string together team IDs and just pull it all at once? Is that better?
# allTeams <- data.frame(fromJSON(paste(root,"/teams/","?teamId=",paste(teamIDs,collapse=","),sep=""))[2])
# While this works, it seems I'm just accessing the data from before, and it's not giving me the rosters...
# okay then...

# So I need to get player IDs from: https://statsapi.web.nhl.com/api/v1/teams/1/?expand=team.roster

# *** testing
rosterList <- list()
for(i in 1:length(teamIDs)){
  rosterList[i] <- paste(root,"/teams/",as.character(teamIDs[i]),"/",sep="")
}

rosterList
# *** looks good to me!

rm(rosterList)

# * * * This is me dicking around, figuring things out
# so I fucked around with this code to see exactly what part I need:
# testlist <- fromJSON(paste(root,"/teams/",as.character(teamIDs[1]),"/","?expand=team.roster",sep=""))
# and this is what I need:
# testlistRoster <- fromJSON(paste(root,"/teams/",as.character(teamIDs[1]),"/","?expand=team.roster",sep=""))[[2]]$roster$roster[[1]]
# fuck that took forever. There has to be a better way?
#
# So, based on the above, I need to grab the person ID of this monstrosity:
# (paste(root,"/teams/",as.character(teamIDs[1]),"/","?expand=team.roster",sep=""))[[2]]$roster$roster[[1]]
# I guess just add "person.id" to the end of that madness, like:
# (paste(root,"/teams/",as.character(teamIDs[1]),"/","?expand=team.roster",sep=""))[[2]]$roster$roster[[1]]$person.id
# Let's try it:
# well apparently I had to dig a bit deeper to get it, but this is how it works:
# fromJSON(paste(root,"/teams/",as.character(teamIDs[1]),"/","?expand=team.roster",sep=""))[[2]]$roster$roster[[1]][[1]][1]
#
# * * *

# * * * This is me cleaning things up
#
# fromJSON(paste(root,"/teams/",as.character(teamIDs[1]),"/","?expand=team.roster",sep=""))[[2]][12]
# ^ accesses roster, now need "roster" inside of it
# fromJSON(paste(root,"/teams/",as.character(teamIDs[1]),"/","?expand=team.roster",sep=""))[[2]][12][[1]]
# ^ still need to unpack a few levels...
# fromJSON(paste(root,"/teams/",as.character(teamIDs[1]),"/","?expand=team.roster",sep=""))[[2]][12][[1]][[1]][[1]][[1]][[1]]
# ^ there it is! how fun.
#
# * * *

# * * * My first crack at grabbing player IDs
# So here's what we use to grab the player list from each team:
# testTester <- fromJSON(paste(root,"/teams/",as.character(teamIDs[1]),"/","?expand=team.roster",sep=""))[[2]][12][[1]][[1]][[1]][[1]][[1]]
# 
# # and now to load all these fuckers in a data.frame:
# rosterList <- list()
# for(i in 1:length(teamIDs)){
#   rosterList[[i]] <- fromJSON(paste(root,"/teams/",as.character(teamIDs[i]),"/","?expand=team.roster",sep=""))[[2]][12][[1]][[1]][[1]][[1]][[1]]
# }
# 
# # now put it into one big ol' ballsack of player IDs, instead of 31 lists.
# rosterList <- unlist(rosterList)
#
# * * *

# * * * Realizing that I need more than just player IDs, and solving the problem
# Okay, so the above code works - but I need to separate out goalies from the mix! 
# so, just grab up all the info we need from the roster stage first.
# note, we can grab handedness and other important variables here too if necessary.

# EDIT: adding player name, team name for starters...
rosterFrame <- data.frame(playerID=numeric(),pos=character())

for(i in 1:length(teamIDs)){
  load <- fromJSON(paste(root,"/teams/",as.character(teamIDs[i]),"/","?expand=team.roster",sep=""))[[2]][12][[1]][[1]][[1]]
  load2 <- data.frame(playerID=as.numeric(load$person$id),pos=as.character(load$position$code))
  rosterFrame <- rbind(rosterFrame,load2)
}
rm(load,load2,i)

# Now we have a nice data frame with player IDs and positions.
# Just need to strip out goalies into their own data frame; they're special and shall
# be treated as such.

# * * * Separating dirty goalies from the bourgeois skaters
skaterFrame <- rosterFrame[rosterFrame$pos!="G",]
goalieFrame <- rosterFrame[rosterFrame$pos=="G",]

# That's right, outta here you rubber-loving monsters.
rm(rosterFrame)

#### Grabbing Game Logs ####

# So, to grab the game log for a given player for a season, we need to hit up:
# https://statsapi.web.nhl.com/api/v1/people/ID/stats?stats=gameLog&season=20162017
# so try: https://statsapi.web.nhl.com/api/v1/people/8471233/stats?stats=gameLog&season=20162017 
# This gives us Travis Zajac's 2016-2017 game by game stats. Lots of info to parse here...
# What we want to end up with is a single data frame with each row representing a game and
# columns representing the different stats categories. 
# We want to retain important information such as Player Name, Team, Team being played, Date, Game ID, Home v Away...

aplayersstats <- fromJSON(paste(root,"/people/",as.character(skaterFrame[1,1]),"/","/stats?stats=gameLog&season=","20182019",sep=""))[[2]][[2]][[1]]

# Okay, what do we need out of this?
