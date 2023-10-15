library(rvest)
library(stringr)

url <- "https://cran.r-project.org/src/contrib/"

cran_new <- read_html(x = url)

cran <- cran_new %>% html_elements("tr") %>% 
  html_elements("a") %>% 
  html_text()
# cran[1:20]

pkgs <- cran[!cran %in% c("Name","Last modified","Size","Description","Parent Directory")]
pkgs <- pkgs[!str_detect(pkgs, pattern = ".*\\/$")] %>%  # 去除文件夹
  str_remove("\\.tar\\.gz") %>% # 保留R包名称与版本
  str_split(pattern = "_")
pkgs[1:10]
df <- do.call(rbind,pkgs)
colnames(df) <- c("name","version")


