#adding in links

onsemi <- read_csv('/Users/bridg3r/Documents/GitHub/Onsemi/OnsemiFootprint/onsemi4.csv')

onsemi <- onsemi %>% select('Longitude', 'Latitude', 'Number_of_reviews', 'Review_Points', 'Address', 'city', 'country', 'site_type', 'cgroup')
#Fixing the cities that got pulled out incorrectly, this could maybe be done with simpler 
#code by making a list of the Asian countries and then pulling then splititng the address by commas
# then if the country is an asian one, pulling the 3rd to last element in the split for city name
onsemi <- onsemi %>% 
  mutate(cgroup = ifelse(site_type != 'other', 'ASG', 'non-ASG'), 
         city = case_when(Address=='Einsteinring 28, 85609 Aschheim, Germany'~'Aschheim',
                          city== "#145"~ "Austin",
                          Address =='Isafjordsgatan 32 C, 6tr, 164 40 Kista, Sweden'~'Kista',
                          Address == 'Av. des Champs-Montants 12A, 2074 Marin-Epagnier, Switzerland'~'Marin-Epagnier',
                          Address == 'Via Brescia, 26, 20063 Cernusco sul Naviglio MI, Italy'~'Cernusco sul Naviglio',
                          Address == '2nd Floor, Greenwood House, London Rd, Bracknell RG12 2AA, United Kingdom'~'Bracknell',
                          Address == "The Loughmore Centre, Loughmore Avenue, Raheen Business Park, Limerick, Ireland" ~'Limerick',
                          Address =='Bulevardul Iuliu Maniu 6Q, București 061103, Romania'~'București',
                          Address =='B-04-25, Krystal Point, 303, Jalan Sultan Azlan Shah, Krystal Point Corporate Park, 11900 Bayan Lepas, Pulau Pinang, Malaysia'~'Bayan Lepas',
                          Address =='2 Commerce Ave, Alabang, Muntinlupa, Metro Manila, Philippines'~'Muntinlupa',
                          Address =='Lorong Senawang 2/2, Kawasan Perusahaan Senawang, 70450 Senawang, Negeri Sembilan, Malaysia'~'Senawang',
                          Address =='Lot 1.02 Level One, KPMG Tower, 8, First Ave, Bandar Utama, 47800 Petaling Jaya, Selangor, Malaysia'~'Petaling Jaya',
                          Address =='2 Commerce Ave, Alabang, Muntinlupa, Metro Manila, Philippines'~'Muntinlupa',
                          city == 'Ho'~'Hong Kong',
                          city == 'Tarlac' ~ 'Tarlac City',
                          Address == 'Số 10 Đường Số 17A, Bình Trị Đông B, Bình Tân, Thành phố Hồ Chí Minh, Vietnam'~'Ho Chi Minh City',
                          city == 'Mala'~'Senawang',
                          city == 'Pen'~'Singapore',
                          TRUE ~ city)) 

onsemi <- onsemi %>% mutate(`Advanced Solutions Group` = case_when(cgroup == 'ASG'~1, cgroup != 'ASG'~0), `Power Solutions Group` = 0, `Intelligent Solutions Group` = 0)

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


write_csv(onsemi, "/Users/bridg3r/Documents/GitHub/Onsemi/OnsemiFootprint/FootprintApp/www/onsemi5.csv")

