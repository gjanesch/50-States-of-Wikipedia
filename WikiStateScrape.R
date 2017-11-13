library(rvest)
library(magrittr)
library(data.table)

# Strip out the number from an HTML element
number_from_element <- function(a.node){
    return(html_text(a.node) %>% gsub(pattern = "[^0-9+-]", replacement = "") %>% as.numeric())
}

# Get the 
get_row_info <- function(a.row){
    edit.date <- html_node(a.row, "a.mw-changeslist-date") %>% html_text()
    history.size <- html_node(a.row, "span.history-size") %>% number_from_element()
    return(data.frame(Edit.Date = edit.date, History.Size = history.size))
}

# Gather the rows from the Wikipedia pages, transform
get_all_rows <- function(state.name, base.url, edits.per.page){
    next.page.exists <- TRUE
    list.of.rows <- NULL
    wiki.link <- paste0(BASE.URL,"/w/index.php?title=",state.name,"&offset=&limit=",edits.per.page,"&action=history")
    
    while(next.page.exists){
        page.html <- read_html(wiki.link)
        list.node <- html_node(page.html, css = "ul#pagehistory")
        new.rows <- html_nodes(list.node, "li")
        print(wiki.link)
        list.of.rows <- c(list.of.rows, new.rows)
        
        next.page.link <- html_node(page.html, css = "a.mw-nextlink")
        if(length(next.page.link) > 0){
            wiki.link <- paste0(base.url, html_attr(next.page.link, "href"))
        } else {
            next.page.exists <- FALSE
        }
    }
    
    edit.df <- lapply(list.of.rows, get_row_info)
    edit.df <- rbindlist(edit.df)
    
    write.csv(edit.df, paste0(state.name, ".csv"), row.names = FALSE)
}


BASE.URL = "https://en.wikipedia.org"
EDITS.PER.PAGE = 2000

state.names <- readLines("state_list.txt")

list.of.rows <- lapply(state.names, function(x){get_all_rows(x, BASE.URL, EDITS.PER.PAGE)})