#adding in links

onsemi <- read_csv('https://raw.githubusercontent.com/bridg3r/Onsemi/main/OnsemiFootprint/onsemi2.csv')

onsemi <- onsemi %>% mutate(`Advanced Solutions Group` = case_when(cgroup == 'ASG'~1), `Power Solutions Group` = 0, `Intelligent Solutions Group` = 0)

groupLinks <- tibble(cgroup = c('Advanced Solutions Group', 'Power Solutions Group', 'Intelligent Solutions Group')) %>% 
  mutate(link = case_when(cgroup == 'Advanced Solutions Group'~'https://onsemi.sharepoint.com/sites/asg', 
                          cgroup == 'Power Solutions Group'~'https://onsemi.sharepoint.com/sites/psg', cgroup =='Intelligent Solutions Group'~'https://onsemi.sharepoint.com/sites/isg')
  )

linkedLocations <- c("Pocatello", "Gresham" ,'Suzhou', "Nampa", "Mountain Top",
                     "Rožnov", "Carmona", "Cebu", "Seremban", "Bình Dương", 
                     "Tarlac", "Bucheon", "Aizu")

onsemi <- onsemi %>% mutate(link = case_when(
  city == "Pocatello" ~ 'https://onsemi.sharepoint.com/sites/mfg-man-pocatello',         
  city =="Gresham" ~ 'https://onsemi.sharepoint.com/sites/mfg-gresham',
  city =='Suzhou'~'https://onsemi.sharepoint.com/sites/mfg-Suzhou',
  city =="Nampa" ~ 'https://onsemi.sharepoint.com/sites/mfg-Nampa',
  city =="Mountain Top"~ 'https://onsemi.sharepoint.com/sites/mfg-MountainTop',
  city =="Rožnov" ~ 'https://onsemi.sharepoint.com/sites/mfg-CZ4',
  city =="Carmona"~ 'https://onsemi.sharepoint.com/sites/mfg-ospi',         
  city =="Cebu" ~ 'https://onsemi.sharepoint.com/sites/mfg-Cebu',                
  city =="Seremban" ~ 'https://onsemi.sharepoint.com/sites/mfg-ismffab',              
  city =="Bình Dương" ~ 'https://onsemi.sharepoint.com/sites/mfg-vn-osbd',       
  city =="Tarlac" ~ 'https://onsemi.sharepoint.com/sites/mfg-SSMP',
  city =="Bucheon" ~ 'https://onsemi.sharepoint.com/sites/mfg-bucheon',
  city =="Aizu"~ 'https://onsemi.sharepoint.com/sites/mfg-AFSM/SitePages/Home.aspx'
))

write_csv(onsemi, "~/Documents/GitHub/Onsemi/OnsemiFootprint/onsemi3.csv")

