library(dplyr)
library(stringr)
library(lubridate)

path <- "./SCTP monthly data/Excel/"

current_files <- list.files(path = path,pattern=NULL)

### delete weird files
delete_files <- 
  current_files %>% 
  as_tibble() %>%
  filter(str_detect(value,"-NA")|str_detect(value,"Messages"))

file.remove(delete_files %>% pull())

### current files
current_files <- list.files(path = path , pattern=NULL)

current_names <- gsub("~\\$", "", current_files)
current_names <- gsub(" ", ".", current_names)
current_names <- gsub("SCTP.", "", current_names)

type <- vector()
insurer <- vector()
month <- vector()
file <- vector()
for (i in 1:length(current_names)) {

  type[i] <- "InsurerMonthlyData" #strsplit(current_names,"[-]")[i][[1]][1] 
  insurer[i] <- "CALLIDEN" #strsplit(current_names,"[-]")[i][[1]][2] 
  month[i] <- paste0(unlist(strsplit(current_names,split="[-+*/)(. ]")[i])[3],"-",
                     unlist(strsplit(current_names,split="[-+*/)(. ]")[i])[4],"-",
                     unlist(strsplit(current_names,split="[-+*/)(. ]")[i])[5]
                     )
  file[i] <- "xlsx"  #strsplit(current_names,"[-]")[i][[1]][4] 
  
  if (length(unlist(strsplit(current_names,split="[-+*/)(. ]")[i]))==4) {
    type[i] <- "InsurerMonthlyData" #strsplit(current_names,"[-]")[i][[1]][1] 
    insurer[i] <- "CALLIDEN" #strsplit(current_names,"[-]")[i][[1]][2] 
    month[i] <- unlist(strsplit(current_names,split="[-+*/)(. ]")[i])[3]
    file[i] <- "xlsx"  #strsplit(current_names,"[-]")[i][[1]][4] 
  }
}

new_months <- vector()
for (i in 1:length(month)) {
 new_months[i] <- as_date(ifelse(nchar(month[i])==7, myd(month[i],truncated = 1),month[i])) 
}
new_months <- as_date(new_months)


current_files <- paste0(path,current_files)

new_files <- paste0(path,type,"-",insurer,"-",new_months,".",file)
new_files #%>% tibble::view()

file.rename(current_files,new_files)
