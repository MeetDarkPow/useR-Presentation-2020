######################
# Base R
######################

## 1. current R version
version$version.string

## 2. Chart for Yearly download count of Base R
library(cranlogs)
library(data.table)
library(lubridate)
library(ggplot2)
library(magrittr)

### assigned a certain time period
total_R <- cran_downloads("R", from = "2005-01-01", to = Sys.Date())
setDT(total_R)
total_R[, `:=`(
  round_year_r = floor_date(date, "year" )) ]
### Plotting R downloads
total_R[, .(count = sum(count)), year(round_year_r)] %>%
  ggplot(aes(year, count)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  geom_text(aes(label=count), hjust=-0.1, color="red", size=4) +
  labs(
    title = "R downloads by Year on RStudio CRAN mirror", 
    subtitle = "data via {cranlogs}", 
    x = "Year classification", y="Number of downloads"
  ) + theme_minimal() + coord_flip()

### Plotting number of R downloads Version-wise
total_R[, .(count = sum(count)), version][order(count, decreasing = TRUE)] %>%
  head(10) %>% 
  ggplot(aes(reorder(version, count), count)) + coord_flip() +
  geom_bar(stat="identity", fill="red") + 
  geom_text(aes(label=count), hjust=-0.1, color="red", size=4) +
  labs(
    title = "10 most downloaded R versions in 2019 on RStudio CRAN mirror", 
    subtitle = "data via {cranlogs}", 
    x = "version", y="Number of downloads"
  ) + theme_minimal()

######################
# CRAN Packages
######################

## 1. Total R packages on CRAN
nrow(available.packages())

## 2. Total Archived Packages
library(packageRank)
length(archivePackages())

## 3. Chart of Cumulative count downloads of R packages over time
library(jsonlite)
library(ggplot2)
dailyPKG = fromJSON("https://cranlogs.r-pkg.org/downloads/daily/2012-01-01:2020-12-31")
downloads <- dailyPKG$downloads[[1]]
dailypkg_dwnld <- data.frame(Date=as.Date(downloads$day), Count=downloads$downloads)
ggplot(data=dailypkg_dwnld, aes(x=Date, y=Count, group=1)) +
  geom_point() +
  labs(
    title = "Chart of Cumulative count downloads of R packages over time",
    x = "Timeline from 2012-10-01 till 2020-(Present)", y="Number of downloads"
  )

## 4. Unique number of package 'Maintainers'.
library(tools)
pdb <- CRAN_package_db()
length(unique(pdb$Maintainer))

## 5. Total Download Count of all R Packages
sum(dailypkg_dwnld$Count)

## 6. Yearly Package download count chart
library(data.table)
library(lubridate)
library(ggplot2)
library(magrittr)

setDT(dailypkg_dwnld)
dailypkg_dwnld[, `:=`(
  round_year = floor_date(Date, "year" )) ]

dailypkg_dwnld[, .(count = sum(Count)), year(round_year)] %>%
  ggplot(aes(year, count)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  geom_text(aes(label=count), hjust=-0.1, color="red", size=4) +
  labs(
    title = "Yearly Package download count chart", 
    x = "Year classification", y="Number of downloads"
  ) + theme_minimal() + coord_flip()

## 7. Chart of Top 20 Keywords in CRAN Packages
library(xml2)
library(rvest)
library(tm)

CRAN_package_wbpg <- read_html("https://cran.r-project.org/web/packages/available_packages_by_name.html")
package_summary <- CRAN_package_wbpg %>%
  html_nodes("td+ td") %>%
  html_text()
package_summary <- gsub("[\r\n]", "", package_summary)

### converting data to string
data <- toString(package_summary)
data <- gsub('[[:punct:] ]+',' ',data)

### cleaning data
docs <- VCorpus(VectorSource(data))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)

### creating a matrix for tabulation of word with respective frequency 
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)

## 8. Get the unique count of licenses and Top 5 most popular license
library(tools)
pdb <- CRAN_package_db()
unique_count_license <- length(unique(pdb$License))
uniq_license <- unique(pdb$License)
No_pkgs_attached <- c()
for(i in 1:unique_count_license){
  No_pkgs_attached[i] <- length(which(pdb$License==uniq_license[i]))
}
license_df <- data.frame(License=uniq_license, Pkgs_attached=No_pkgs_attached)
popular_license <- license_df[with(license_df, order(-Pkgs_attached)),]
head(popular_license, 5)

######################
# GitHub
######################

## 1. Total count of R repos
library(httr)
library(jsonlite)
library(lubridate)

options(stringsAsFactors = FALSE)

url <- "https://api.github.com"
path <- "search/repositories?q=language:R&per_page=100&sort=stars&order=desc"
raw.result <- GET(url = url, path = path)
raw.result$status_code
this.raw.content <- rawToChar(raw.result$content)
this.content <- fromJSON(this.raw.content)
this.content$total_count

## 2. Yearly chart of R repos
library(httr)
library(jsonlite)
library(lubridate)
library(ggplot2)

year_repo_count <- c()
timeline <- 2008:year(Sys.Date())
for(i in 1:length(timeline)){
  url <- "https://api.github.com"
  path <- paste0("search/repositories?q=language:R+created:",as.character(timeline[i]),"&per_page=100&sort=stars&order=desc")
  raw.result <- GET(url = url, path = path)
  this.raw.content <- rawToChar(raw.result$content)
  this.content <- fromJSON(this.raw.content)
  year_repo_count[i] <- this.content$total_count
  Sys.sleep(5)
}
yearly_repo_df <- data.frame(Year = 2008 : year(Sys.Date()),
                             RepoCount = year_repo_count)
ggplot(data=yearly_repo_df, aes(x=Year, y=RepoCount)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=RepoCount), vjust=-0.3, color="black", size=3.5)+
  labs(
    title = "Count of R Repositories on GitHub - Yearly Basis", 
    x = "Year", y="Number of Repositories"
  ) + theme_minimal()

## 3. Top 20 Most starred repos
library(httr)
library(jsonlite)
library(ggplot2)

url <- "https://api.github.com"
path <- "search/repositories?q=language:R&per_page=100&sort=stars&order=desc"
raw.result <- GET(url = url, path = path)
raw.result$status_code
this.raw.content <- rawToChar(raw.result$content)
this.content <- fromJSON(this.raw.content)
repos_names <- this.content[["items"]][["name"]]
star_count <- this.content[["items"]][["watchers"]]
star_repos_df <- data.frame(Repo_Name=head(this.content[["items"]][["name"]],20), 
                            Star_Count=head(this.content[["items"]][["watchers"]],20))
ggplot(data=star_repos_df, aes(x=Repo_Name, y=Star_Count)) +
  geom_bar(stat="identity", fill="orange")+
  geom_text(aes(label=Star_Count), vjust=-0.3, color="black", size=3.5)+
  labs(
    title = "Top 20 most starred/popular repositories on GitHub", 
    x = "Repositories Name", y="Number of Star Count"
  ) + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

######################
# BioConductor
######################

## 1. Number of Packages
library(BiocManager)
length(available())

## 2. Total Downloads for all time
library(packageRank)
bioc_dwnld <- bioconductorDownloads()
sum(bioc_dwnld[["data"]][[1]][["Nb_of_downloads"]])

## 3. Chart of total yearly downloads
library(data.table)
library(packageRank)
library(magrittr)
library(ggplot2)

bioc_dwnld_df <- bioconductorDownloads()
bioc_dwnld_df <- bioc_dwnld_df[["data"]][[1]]
setDT(bioc_dwnld_df)
bioc_dwnld_df[, .(count = sum(Nb_of_downloads)), Year] %>%
  ggplot(aes(Year, count)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  geom_text(aes(label=count), hjust=-0.1, color="red", size=4) +
  labs(
    title = "R downloads by Year on RStudio CRAN mirror", 
    subtitle = "data via {cranlogs}", 
    x = "Year classification", y="Number of downloads"
  ) + theme_minimal() + coord_flip()

######################
# CRAN Task Views
######################

## 1. Total Number of CRAN Task Views
length(available.views())

## 2. Chart of number of packages per task view
taskview_list <- available.views()
taskview_name <- c()
for(i in 1:length(taskview_list)){
  taskview_name[i]=taskview_list[[i]][["name"]]
}
taskview_pkg_count <- c()
for(i in 1:length(taskview_list)){
  taskview_pkg_count[i]=length(taskview_list[[i]][["packagelist"]][["name"]])
}
taskview_df <- data.frame(TaskViews = taskview_name, NumberOfPackages = taskview_pkg_count)
