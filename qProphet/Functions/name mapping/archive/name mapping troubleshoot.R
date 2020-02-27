
historyDF %>% mutate(year = year(ds)) %>% 
  mutate(status = ifelse(is.na(worldrank),'missing','good')) %>% 
  group_by(year,status) %>% summarise(n=n()) %>% spread(status,n)


historyDF %>% mutate(year = year(ds)) %>% 
  filter(year==2020) %>% 
  mutate(status = ifelse(is.na(worldrank),'missing','good')) %>% 
  filter(status=='missing') %>% 
  group_by(name) %>% summarise(n=n()) %>% arrange(desc(n))

historyDF %>% mutate(year = year(ds)) %>% filter(year==2020) %>% filter(is.na(worldrank)) %>% select(name) %>% unique() 

historyDF %>% mutate(year = year(ds)) %>% 
  filter(year==2020) %>% 
  mutate(status = ifelse(is.na(worldrank),'missing','good')) %>% 
  filter(status=='missing') %>% 
  group_by(ds,salary,name) %>% summarise(n=n()) %>% arrange(desc(n)) %>% ungroup() %>% select(ds,salary,name) %>% mutate(name = as.character(name)) %>% filter(!name %in% unique(worldrank$name))

