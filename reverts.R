
find.revisions <- function(state.df, lag = 2, max.minutes.difference = 60){
    
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


