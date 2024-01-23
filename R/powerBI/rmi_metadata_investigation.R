
url <- 'https://rmi.org/from-trees-to-tech-and-beyond-carbon-dioxide-removal-cdr-in-all-its-variations/'

url_tb <- url %>%
  read_html() %>% 
  html_nodes('script') %>% 
  html_text() %>% 
  as.data.frame() %>%
  rename(node = 1) %>% 
  filter(grepl('schema.org', node)) %>% 
  mutate(program = str_extract(node, 'articleSection\\":\\"([^"]+)\\"'),
         program = gsub('articleSection":"',"",program),
         program = gsub('"', "", program)) %>%
  mutate(keywords = sub('.*keywords\\"\\:\\[', "", node),
         keywords = gsub('\\].*', "", keywords))

df[i, 'metadata'] <- url_tb[2, 'keywords']
df[i, 'program'] <- url_tb[2, 'program']






# This needs to be added into to get the program
program <- url %>%
  read_html() %>% 
  html_nodes('script') %>% 
  html_text() %>% 
  as.data.frame() %>%
  rename(node = 1) %>% 
  filter(grepl('schema.org', node)) %>% 
  mutate(program = str_extract(node, 'articleSection\\":\\"([^"]+)\\"'),
         program = gsub('articleSection":"',"",program),
         program = gsub('"', "", program))






