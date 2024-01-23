

# Get title from website for multiple URLs using URL
# https://stackoverflow.com/questions/39691892/get-title-from-website-for-multiple-urls-using-url


library(rvest)

urls <- allEmailStats$url_1

get_title <- function(url) {
  webpage <- read_html(url)
  title <- html_text(html_nodes(webpage, "title"))
  return(title)
}

titles <- sapply(urls, get_title)

df <- cbind(titles, urls) %>%
  as.data.frame(index = FALSE)

df <- allEmailStats %>%
  mutate(url_drop = sapply(url_1, get_title))













getCampaignEmails <- function(pageURLs){
  
  #' filter each of the 3 "story_url" columns for campaign web page URLs
  df1 <- allEmailStats %>% select(c('id':'COR_S1')) %>% 
    rename(story_url = url_1,
           story_title = title_1,
           story_clicks = clicks_1, 
           story_COR = COR_S1)
  
  df2 <- allEmailStats %>% select(c('id':'plaintext_rate', 'url_2':'COR_S2'))
  names(df2) <- names(df1)
  
  df3 <- allEmailStats %>% select(c('id':'plaintext_rate', 'url_3':'COR_S3'))
  names(df3) <- names(df1)
  
  
  pageurl2 <- campaignKey$page
  
  
  #' bind matches
  allStoryStats <- df1 %>% 
    rbind(df2) %>% 
    rbind(df3) %>% 
    filter(grepl(paste(pageurl2, collapse = '|'), story_url)) %>% 
    mutate(date = as.Date(date),
           story_title = gsub(' - RMI', '', story_title))
  
  #' order by date and keep relevant columns
  allStoryStats <- allStoryStats[rev(order(allStoryStats$date)),] %>% 
    select(id, name, date, delivered_ = delivered, unique_opens, open_rate, unique_clicks,
           unique_CTR, UCTRvsAvg, story_url, story_title, story_clicks, story_COR)
  
  return(allStoryStats)
}








url <- paste0('https://pi.pardot.com/api/email/version/4/do/stats/id/1549394450?format=json')
getStats <- get(url,  header4)
stats <- as.data.frame(getStats[["stats"]]) %>% 
  mutate(id = paste(newsletter[i, 'id']),
         name = paste(newsletter[i, 'name']),
         subject = paste(newsletter[i, 'subject']))
allEmailStats <- allEmailStats %>% rbind(stats)











allNLs2 <- allNLs %>%
  filter(sentAt > '2023-09-01') %>%
  filter(grepl('Upcoming Events', name) )

allNL_clicks <- getLinkClicks(newsletter = allNLs2)

for(i in 1:nrow(allNLs2)){
  
    NL <- sparkClicks(i, allNLs2) %>% 
      mutate(story = '') %>% 
      filter(grepl("https://rmi.org", url)) %>% 
      arrange(-clicks) 
    
}

getLinkClicks <- function(newsletter) {
  allCleaned <- data.frame(email = '', url_1 = '', clicks_1 = '', url_2 = '', clicks_2 = '', url_3 = '', clicks_3 = '', 
                           url_chart = '', clicks_chart = '', url_question = '', clicks_question = '', url_events = '', clicks_events = '',
                           plainTextPCT = '')[0,]
  
  for(i in 1:nrow(newsletter)){
    
    if(deparse(substitute(newsletter)) == 'sparkNLs'){
      NL <- sparkClicks(i, newsletter) %>% 
        mutate(story = '') %>% 
        filter(grepl("https://rmi.org", url)) %>% 
        arrange(-clicks) 
      
      NL[, 'story'] <- paste0(rownames(NL))
      chart <- NL %>% filter(grepl('Chart', url)) 
      question <- NL %>% filter(grepl('Question', url)) 
      events <- NL %>% filter(grepl('Events', url))
      
      if(nrow(chart) == 0){ chart <- NL[1,] %>% mutate(url = 'Chart - NA', clicks = 0, story = 0) }
      if(nrow(question) == 0){ question <- NL[1,] %>% mutate(url = 'Question - NA', clicks = 0, story = 0) }
      if(nrow(events) == 0){ events <- NL[1,] %>% mutate(url = 'Events - NA', clicks = 0, story = 0) }
      
      top_wider <- NL %>% 
        filter(grepl("^https://rmi.org", url)) 
      
      top_wider <- top_wider[c(1:3),] %>% 
        rbind(chart) %>% rbind(question) %>% rbind(events)
      
      top_wider[, 'story'] <- paste0(rownames(top_wider))
      
      for(j in 1:nrow(top_wider)){
        if(grepl('Question', top_wider[j, 'url'])) { top_wider[j, 'story'] <- paste0('question') }  
        if(grepl('Chart', top_wider[j, 'url'])) { top_wider[j, 'story'] <- paste0('chart') }  
        if(grepl('Events', top_wider[j, 'url'])) { top_wider[j, 'story'] <- paste0('events') }  
      }
      
      top_wider <- top_wider %>% 
        pivot_wider(names_from = story, values_from = c(url, clicks), names_vary = "slowest") 
      
      print(paste0(i, ': ', top_wider$email))
      allCleaned <- allCleaned %>% rbind(top_wider)
      
    } else if(deparse(substitute(newsletter)) == 'marketCatalystNLs'){
      NL <- sparkClicks(i, newsletter) %>% 
        mutate(story = '') %>% 
        filter(grepl("https://rmi.org", url)) %>% 
        arrange(-clicks) 
      
      NL[, 'story'] <- paste0(rownames(NL))
      
      top_wider <- NL %>% 
        filter(grepl("^https://rmi.org", url)) 
      
      top_wider <- top_wider[c(1:3),] 
      
      top_wider[, 'story'] <- paste0(rownames(top_wider))
      
      top_wider <- top_wider %>% 
        pivot_wider(names_from = story, values_from = c(url, clicks), names_vary = "slowest") 
      
      print(paste0(i, ': ', top_wider$email))
      allCleaned <- allCleaned %>% rbind(top_wider)
    }
  }
  return(allCleaned)

}






getBatchClicks <- function(emailId, df){
  allClicks <- data.frame(id = '', prospect_id = '', url = '', list_email_id = '', email_template_id = '', created_at = '')[0,]
  
  for(i in 1:25){
    if(i == 1){
      # write query using next page token to bypass API call limit
      email_clicks <- GET(paste0("https://pi.pardot.com/api/emailClick/version/4/do/query?format=json&list_email_id=", allNLs2[emailId, 'id']),
                          add_headers(.headers = header4))
      getClicks <- jsonlite::fromJSON(content(email_clicks, as = "text", encoding = "UTF-8"))
      clicksQuery <- getClicks[["result"]][["emailClick"]]
      
      digit <- sub('(.*) ', '', str_match(clicksQuery[200, 'created_at'], " *(.*?)\\s*(:)")[,2])
      if(grepl('^0', digit)){ hour <- '%2' } else { hour <- '%20' }
      nextDate <- gsub(' ', hour, clicksQuery[200, 'created_at'])
      
      allClicks <- allClicks %>% rbind(clicksQuery)
    } else {
      email_clicks <- GET(paste0("https://pi.pardot.com/api/emailClick/version/4/do/query?format=json&created_after=", nextDate, "&list_email_id=", allNLs2[emailId, 'id']),
                          add_headers(.headers = header5))
      getClicks <- jsonlite::fromJSON(content(email_clicks, as = "text", encoding = "UTF-8"))
      clicksQuery <- as.data.frame(getClicks[["result"]][["emailClick"]])
      
      allClicks <- allClicks %>% rbind(clicksQuery)
      if (nrow(clicksQuery) < 200) break
      
      digit <- sub('(.*) ', '', str_match(clicksQuery[200, 'created_at'], " *(.*?)\\s*(:)")[,2])
      if(grepl('^0', digit)){ hour <- '%2' } else { hour <- '%20' }
      nextDate <- gsub(' ', hour, clicksQuery[200, 'created_at'])
      
    }
  }
  return(allClicks)
}






