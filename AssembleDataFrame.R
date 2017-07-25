library(data.table)
library(lubridate)

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

state.names <- readLines("state_list.txt")
state.dfs <- lapply(state.names, format.state.df)
all.states <- do.call("rbind", state.dfs)

# Change the state names to be normal English
all.states$State.Name <- gsub("_\\(.*\\)","", all.states$State.Name)
all.states$State.Name <- gsub("_"," ", all.states$State.Name)

# Remove everything after June 31, 2017; since the entries were collected on different days, the
# ones scraped later probably have more edits to them
cutoff.date <- as.POSIXct("2017-06-30 23:59:59", format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
all.states <- all.states[all.states$Edit.Date < cutoff.time,]

write.csv(all.states, "all_states.csv", row.names = FALSE)