library("rJava")
library("tabulizer")
library("tidyverse")
library("lubridate")

get_pdf_data <- function(type, filedate) {
  type <- paste(type) # BUSP or CM
  filedate <- paste(filedate) # in the format 'yyyy-mm'
  
  path <- "./SCTP monthly data/Excel/"

  current_files <- paste0(path,list.files(path = path , pattern = filedate)) # path = paste0("./SCTP monthly data/PDF/",type),
  file_use <- current_files#[file]
  date <- ymd(substr(strsplit(file_use, "[.]")[[1]][2], nchar(strsplit(file_use, "[.]")[[1]][2]) - 10, nchar(strsplit(file_use, "[.]")[[1]][2])), truncated = 1)


  areaz <- list(c(481.4733, -0.6059336, 763.7703, 574.9663305))

  if (date < "2017-01-01") {
    ndf <- tibble()
    }
  if (date >= "2017-01-01" & date < "2019-07-01") {
    ndf <- tibble()
    }
  if (date >= "2019-07-01" & date < "2020-06-01") {
    ndf <- extract_tables(file_use, pages = 7, area = areaz, guess = FALSE, method = "stream")
  }
  if (date >= "2020-06-01") {
    ndf <- extract_tables(file_use, pages = 8, area = areaz, guess = FALSE, method = "stream")
  }

  ndf2 <- data.frame(unlist(ndf[[1]])) %>%
    janitor::clean_names() %>%
    slice_tail(n = 6)

  no_insurers <- ncol(ndf2) - 1

  no_insurers2 <- length(which(substr(unlist(strsplit(unlist(ndf), " ")), 1, 7) == "Insurer")) + 1

  if (no_insurers != no_insurers2) {
    ndf_final <-
      ndf2 %>% unite(x2,-x1, sep = " ", remove=TRUE) %>%
      # separate(x1, into = c("a","b"),sep = " ") %>%
      separate(x2, into = paste0("Insurer", 1:no_insurers2), sep = " ")
  }
  if (no_insurers == no_insurers2) {
    ndf_final <- ndf2
    colnames(ndf_final) <- c("x1", paste0("Insurer", 1:no_insurers2))
  }

  final_df <- ndf_final %>%
    pivot_longer(-x1) %>%
    pivot_wider(names_from = x1, values_from = value) %>%
    janitor::clean_names() %>% 
    mutate(opportunity_date = date)
  return(final_df)
}
#########################################################
###### PDF text
# pdf.text <- pdftools::pdf_text(current_files[1])
# cat(pdf.text[[1]])
# pdf_info(current_files[1])


#### Tabulizer tutorial
# f2 <- "https://github.com/leeper/tabulizer/raw/master/inst/examples/data.pdf"
# extract_tables(f2, pages = 2)

# ##### extract tables
# out1 <- extract_tables(current_files[length(current_files)],pages = 8,method = "stream") # page 8 most important, 13 less so
# out1
#
# df <- bind_cols(out1[[1]][,1],out1[[1]][,2],out1[[1]][,3],out1[[1]][,4]) %>% janitor::clean_names() #%>% view()
#
# df2 <- df %>% slice_tail(n=6) #%>% janitor::clean_names()
# df_final <-
#   df2 %>%
#   separate(x1, into = c("a","b","Insurer1","Insurer2","Insurer3"),sep = " ") %>%
#   separate(x2, into = c("Insurer4","Insurer5","Insurer6"),sep = " ") %>%
#   rename(Insurer7 = x3) %>%
#   separate(x4, into = c("Insurer8","Insurer9"),sep = " ") %>%
#   unite(metric, c(a, b),sep = " ")
#
# df_final
#
# no_insurers <- df %>% filter(substr(x1,1,7)=="Insurer") %>% separate(x1,into = c("insurer","number"),sep = " ") %>% summarise(max(number)) %>% pull() %>% as.numeric()+1
# hollard <- which(substr(df$x1[1:no_insurers],1,1) == "H")
# no_insurers
# hollard

#################################################################################################
# extract_areas(current_files[length(current_files)],pages = 8)
# locate_areas(current_files[length(current_files)],pages = 8)
#
# # areaz <- list(c(481.4733,11.9406,763.7703,568.6931))
# areaz <- list(c(481.4733 , -0.6059336, 763.7703, 574.9663305))
# ndf <- extract_tables(current_files[length(current_files)],pages = 8,area = areaz,guess = FALSE, method = "stream")
# ndf2 <-  data.frame(unlist(ndf[[1]])) %>% janitor::clean_names() %>% slice_tail(n=6)
#
# no_insurers <- ncol(ndf2)-1
#
# no_insurers2 <- length(which(substr(unlist(strsplit(unlist(ndf)," ")) ,1,7)=="Insurer"))+1
#
# if(no_insurers != no_insurers2){
# ndf_final <-
#   ndf2 %>%
#   #separate(x1, into = c("a","b"),sep = " ") %>%
#   separate(x2, into = paste0("Insurer",1:no_insurers2),sep = " ")
# }
# if(no_insurers == no_insurers2){
#  ndf_final <- ndf2
# }
#
# ndf_final
