##
install.packages("devtools")
devtools::install_github("sfirke/janitor")

library(readxl); 
library(janitor); 
library(dplyr); 
library(here);


roster_raw <- read_excel(here("../PPARII/24 subjects for study at CHI.xlsx"))
roster_raw <- read_excel(here("dirty_data.xlsx")) # available at http://github.com/sfirke/janitor
glimpse(roster_raw)
roster<-roster_raw %>% clean_names()
roster %>% remove_empty(c("rows", "cols"))

roster %>% get_dupes(contains("name"))


roster %>%
  tabyl(subject)

roster %>%
  tabyl(subject , employee_status)

##ADD Dataedit        https://cran.r-project.org/web/packages/DataEditR/vignettes/DataEditR.html

##ADD PATHCHWORK      https://patchwork.data-imaginist.com/index.html




