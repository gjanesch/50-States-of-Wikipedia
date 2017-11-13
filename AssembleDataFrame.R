#library(data.table)
library(lubridate)

find.revisions <- function(state.df, lag = 2, max.minutes.diff = 60){
    
    n <- nrow(state.df)
    
    # Get the difference in minutes between the first and last edits in the range
    time.differences <- lapply(1:lag, function(x){state.df[x:(n-(lag-x)),'Edit.Date']})
    time.differences <- matrix(unlist(time.differences, use.names = FALSE), byrow = FALSE, ncol = lag)
    minute.difference <- (time.differences[,1] - time.differences[,lag])/60
  
    # Get total edit length change
    edit.length.change <- lapply(1:lag, function(x){state.df[x:(n-(lag-x)),'Size.Change']})
    edit.length.change <- matrix(unlist(edit.length.change, use.names = FALSE), byrow = FALSE, ncol = lag)
    total.length.change <- rowSums(edit.length.change)
    
    # Determine which rows violate at least one criterion
    close.enough <- minute.difference < 60
    does.length.change <- total.length.change == 0
    not.all.zeros <- rowSums(edit.length.change != 0) != 0
    
    reverted.edits <- close.enough & does.length.change & not.all.zeros
    entries.to.remove <- lapply(1:lag, function(x){which(reverted.edits) + (x - 1)})
    entries.to.remove <- unlist(entries.to.remove)
    
    state.df <- state.df[-entries.to.remove,]
  
    return(state.df)
  
}

format.state.df <- function(state.name){
    
    state.df <- read.csv(paste0("./Individual State Data/", state.name, ".csv"), stringsAsFactors = FALSE)
    
    #Skim out the NAs
    state.df <- state.df[rowSums(is.na(state.df)) == 0,]
    
    # Split the date and time
    state.df$Edit.Date <- as.POSIXct(strptime(state.df$Edit.Date, format = "%H:%M, %d %B %Y", tz = "GMT"))
    
    state.df <- cbind(State.Name = rep(state.name, nrow(state.df)), state.df)
    state.df$Size.Change <- c(-1*diff(state.df$History.Size), state.df[nrow(state.df), 'History.Size'])
    return(state.df)
}

too.big.edits <- function(state.df, low.threshold = 0.5, high.threshold = 1.5){
    n <- nrow(state.df)
    hs <- state.df$History.Size
    
    previous.ratio <- hs[2:(n-1)] / hs[1:(n-2)]
    forward.ratio <- hs[2:(n-1)] / hs[3:n]
    
    point.too.small <- (previous.ratio < low.threshold) & (forward.ratio < low.threshold)
    point.too.big <- (previous.ratio > high.threshold) & (forward.ratio > high.threshold)
    
    points.to.exclude <- which(c(FALSE, (point.too.small | point.too.big), FALSE))
    state.df <- state.df[-points.to.exclude,]
    
    state.df$Size.Change <- c(-1*diff(hs), tail(hs, n=1))
    return(state.df)
    
}

MAX_MIN_BETWEEN_EDITS = 60

state.names <- readLines("state_list.txt")
state.dfs <- lapply(state.names, format.state.df)
orig.state.dfs <- do.call("rbind", state.dfs)
full.lengths <- unlist(lapply(state.dfs, nrow))
for(l in 7:2){
    state.dfs <- lapply(state.dfs, function(x){find.revisions(x,lag = l, max.minutes.diff = MAX_MIN_BETWEEN_EDITS)})
}
minus.reversions <- unlist(lapply(state.dfs, nrow))

all.states <- do.call("rbind", state.dfs)

state.names <- gsub("_\\(.*\\)","", state.names)
state.names <- gsub("_"," ", state.names)
edit.comparison <- data.frame(State.Names = state.names, All.Edits = full.lengths)

# Change the state names to be normal English
all.states$State.Name <- gsub("_\\(.*\\)","", all.states$State.Name)
all.states$State.Name <- gsub("_"," ", all.states$State.Name)

# Remove everything after June 31, 2017; since the entries were collected on different days, the
# ones scraped later probably have more edits to them
cutoff.date <- as.POSIXct("2017-06-30 23:59:59", format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
all.states <- all.states[all.states$Edit.Date < cutoff.date,]

write.csv(all.states, "all_states.csv", row.names = FALSE)