# load libraries (if necessary)
library(data.table)

# api url
prefix <- "https://statsapi.web.nhl.com/api/v1"

# franchise data
teams <- jsonlite::fromJSON(paste0(prefix, "/teams"))
class(teams) # list object
length(teams) # length 2...
names(teams) # list has 2 objects, "copyright" and "teams"
teams <- teams[["teams"]] # we don't care about "copyrights", select "teams"
class(teams) # data.frame 
head(teams) # wow so much info, time zone, venue id, locationName (city), division, conference name etc...

# pulling all draft data ----
# problem: get data for ALL drafts available in the API
# problem: API doesn't tell us how far data goes, find a way to go all the way to the last 
# possible year without having to manually insert the year. It has to be done progammatically.

# 1st action, go year by year until it satisfies some condition that will stop the process
  # 1st hurdle: what is the most recent year?
draft <- jsonlite::fromJSON(paste0(prefix, "/draft")) # prefix + /draft gives most recent
i <- draft[["drafts"]][["draftYear"]] # draft year

  # what is the condition then? Look at an year that you know there is no draft
  # and see how the api responds (throws and error, returns an empty data etc)
noDraft <- jsonlite::fromJSON(paste0(prefix, "/draft/1900"))

  # we found our condition! the length has to be greater than 1...
length(draft[["drafts"]]) != length(noDraft[["drafts"]])
rm(noDraft)
# create an empty list to house the drafts data
allDrafts <- vector("list", length = 0L)

j <- 1 # create list index to increase in the while loop

while(length(draft[["drafts"]]) > 1){
  allDrafts[[j]] <- draft[["drafts"]]
  i <- i - 1 # decrease year by 1...
  draft <- jsonlite::fromJSON(paste0(prefix, "/draft/", i)) # request data via api
  j <- j+1 # increase list index by 1...
}

# did it work? Use lapply() to iterate through list objects, check for length
sum(unlist(lapply(allDrafts, length)) == 2) # it worked! 57 same no as draft size

# 2nd action: now that we have all drafts data, we need to transform the objects inside each
# of the 57 elements 

# study the first object of this list...
# View(allDrafts[[1]])

# use lapply (oen of your best friends to bind stack these lists)
# approach: go to each of the 57 elements, stack all the rounds in one data.frame
# this will create another list with 57 components. Then we stack these 57 data.frames to 
# create final historical draft
cols <- names(do.call(cbind, lapply(allDrafts[[1]][["rounds"]][[1]][[3]][[1]], function(x) x)))

res <- do.call(rbind, 
        lapply(allDrafts, function(x){
          do.call(rbind, 
                  lapply(x[["rounds"]][[1]][[3]], function(x){
                    d1 <- do.call(cbind, lapply(x, function(x) x))
                    out <- .fillCols(d1, cols)
                    return(out)
                    })
                  )
          })
)

data.table::setDT(res)

hist(res[team.name == "Montréal Canadiens", pickOverall])

