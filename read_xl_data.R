library(dplyr)
library(readxl)
library(furrr)
#library(purrr)

path <- "./SCTP monthly data/Excel/"

current_files <- paste0(path,list.files(path = path, pattern=NULL))

tictoc::tic()

data <- furrr::future_map_dfr(current_files,
                       ~read_excel(.,sheet = "Opportunities",skip = 1) %>% 
                         janitor::clean_names() %>% 
                         mutate(referral_count = as.integer(referral_count)),
                       .options = furrr_options(seed = TRUE))

tictoc::toc()
###
library(odbc)
library(DBI)

table_id <- Id(schema = "svu", table = "svu_quotes_combined_R")
my_tbl <- data %>%
  rename(steadfast_category = category,
         steadfast_inception_date = inception_date,
         steadfast_policy_number = policy_number) #tbl(con, in_schema('svu', 'svu_analysis_tableau'))  

### write table 
nrow(my_tbl)
tictoc::tic()

memory.limit(99999)
con <- dbConnect(odbc(), Driver = "SQL Server", Server = "sydawsdb2019", Database = "CalibreSSiSdev", Trusted_Connection = "True")
dbWriteTable(conn = con, name = table_id, value = my_tbl %>% 
               slice(1:floor(1*(nrow(my_tbl)/4)+.01)), overwrite = T)  ## x is any data frame
dbWriteTable(conn = con, name = table_id, value = my_tbl %>% 
               slice(ceiling(1*(nrow(my_tbl)/4)+.01):floor(2*(nrow(my_tbl)/4)+.01)), append = TRUE, overwrite = FALSE)  ## x is any data frame
dbWriteTable(conn = con, name = table_id, value = my_tbl %>% 
               slice(ceiling(2*(nrow(my_tbl)/4)+.01):floor(3*(nrow(my_tbl)/4)+.01)), append = TRUE, overwrite = FALSE)  ## x is any data frame
dbWriteTable(conn = con, name = table_id, value = my_tbl %>% 
               slice(ceiling(3*(nrow(my_tbl)/4)+.01):nrow(my_tbl)), append = TRUE, overwrite = FALSE)
tictoc::toc()
