


# Load một số package cần thiết:

rm(list = ls())

library(rvest)
library(tidyverse)
library(stringr)
library(magrittr)


#=========================================
#         Viết một số hàm hỗ trợ
#=========================================


job_title <- function(link) {
  # Đọc HTML codes: 
  m <- link %>% 
    read_html()
  # Lấy ra thông tin về Job title: 
  m %>% 
    html_nodes('.title a') %>% 
    html_text() %>% 
    return()
}


organization_name <- function(link) {
  m <- link %>% 
    read_html()

  m %>% 
    html_nodes('.field-field-jorg .odd') %>% 
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim() %>% 
    return()
  
}


location <- function(link) {
  m <- link %>% 
    read_html()
  
  m %>% 
    html_nodes('.field-field-jaddress .odd') %>% 
    html_text() %>% 
    str_replace_all("\n", "") %>%
    str_trim() %>%
    return()
  
}



deadline_date <- function(link) {
  m <- link %>% 
    read_html()
  
  m %>% 
    html_nodes('.field-item .date-display-single') %>% 
    html_text() %>% 
    str_replace_all("\n", "") %>%
    str_trim() %>%
    return()
  
}

#==============================
#     Lấy toàn bộ dữ liệu
#==============================

base_url <- "http://www.ngocentre.org.vn/jobs?page="
link <- paste0(base_url, 0:1371)

job_information <- function(link) {
  
  loc_name <- link %>% location()
  job_name <- link %>% job_title()
  org_name <- link %>% organization_name()
  deadline <- link %>% deadline_date()
  
  if (length(loc_name) != 10) {
    loc_name <- c(loc_name, rep(NA, 10 - length(loc_name)))
  }
  
  if (length(job_name) != 10) {
    job_name <- c(job_name, rep(NA, 10 - length(job_name)))
  }
  
  if (length(org_name) != 10) {
    org_name <- c(org_name, rep(NA, 10 - length(org_name)))
  }
  
  if (length(deadline) != 10) {
    deadline <- c(deadline, rep(NA, 10 - length(deadline)))
  }
  
  df <- data_frame(job_name, org_name, deadline, loc_name)
  return(df)
}


#  Test hàm với 5 pages: 
all_df <- lapply(link[1:10], job_information)


# Dùng cách này cho an toàn: 
all_df_c2 <- vector("list", length = length(link))

for (i in seq_along(link)) {
  all_df_c2[[i]] <- job_information(link[i])
  Sys.sleep(3)
}

# Lưu lại dữ liệu có được:   
job_data <- do.call("bind_rows", all_df_c2)
write.csv(job_data, "job_data.csv", row.names = FALSE)

 

