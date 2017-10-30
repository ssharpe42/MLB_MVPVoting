library(rvest)
library(dplyr)
library(tidyr)


get_top_mvp = function(season, top = 10){
    url <- sprintf("https://www.baseball-reference.com/awards/awards_%d.shtml", season)
    
    #Read AL vote table
    AL_table = url %>%
        read_html() %>%
        html_nodes(xpath='//*[@id="AL_MVP_voting"]') %>%
        html_table()
    
    #Read NL vote table
    NL_table = url %>%
        read_html() %>%
        html_nodes(xpath='//*[@id="NL_MVP_voting"]') %>%
        html_table()
    
    process_table = function(df){
        col = as.character(df[[1]][1,])
        df = df[[1]][-1, ]
        names(df) = col
        df = df[,1:7]
        #get rid of percentages
        df = mutate(df, Share = as.numeric(gsub('%','',Share))/100)
        #change column types
        df[,c(1, 4:7)] = sapply(df[,c(1, 4:7)], as.numeric)
        #change WAR to bWAR
        names(df)[7] = 'bWAR'
        return(df)
    }
    #Return cleaned tables with top n
    bind_rows(process_table(AL_table) %>%
        mutate(League = 'AL') %>%
        slice(1:top),
    process_table(NL_table) %>%
        mutate(League = 'NL') %>%
        slice(1:top)) %>%
    mutate(Season  = season)
}

MVPVotes = do.call(rbind, lapply(1974:2016,get_top_mvp ))
saveRDS(MVPVotes,'data/MVPVotes.RDS')
