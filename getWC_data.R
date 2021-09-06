
##########################################################
#GETTING CLIMATE DATA temp, prec
##########################################################

#The following function downloads data from worldclim.org 
# inputs: category , product, product_resolution, period, SSP, GCM
#input Details: 
# category: 
# 1. HCD (historical climate data) downloads 12 geotifs 1 for each month as.zip
#   product types: tmin, tmax,tavg, prec,srad,wind,vapr
#   requires the user to define resolution
#   possibilities are 10m, 5m,2.5m and 30s
# 2. HMWD (historical monthly climate data) data since 1960-2018(min/max temperature and precipitation)
#    product types available: tmin, tmax,prec.
#    requires user to define period of interest as a list ( ie c("1960-1969","1970-1979", "1980-1989", ..)). 
#    available periods are: 1960-1969, 1970-1979,1980-1989, 1990-1999, 2000-2009, 2010-2018
# 3. FCD (future climate data) download downscaled monthly future climate data from CMIP6 for 9 GCMs and 4 SSPs
#    available GCMs: BCC-CSM2-MR, CNRM-CM6-1, CNRM-ESM2-1, CanESM5, GFDL-ESM4, IPSL-CM6A-LR, MIROC-ES2L, MIROC6, MRI-ESM2-0
#    available SSPs: 126, 245, 370 and 585.
#    available periods: (2021-2040, 241-2060, 2061-2080, 2081-2100)
#    available resolution: 10 minutes, 5 minutes, 2.5 minutes.


library(stringr)
library(dplyr)

getWC_data <- function(category=NULL, product=NULL, Product_resolution=NULL, period=NULL, SSP=NULL, GCM=NULL ){
  worldclimlink <- "https://www.worldclim.org"
  page <- read_html(worldclimlink)
  #get data page url
  dataPageUrl <- page %>%
    html_nodes("a") %>%
    html_attr("href")
  dataPageUrl2 <-  str_split(string = dataPageUrl, pattern = " ")[[1]] %>% 
    str_sub(start = 2, end = -1) %>% 
    str_c(paste0(worldclimlink), .)
  #navigate to the page
  dataPage <- read_html(dataPageUrl2) %>% 
    html_nodes("a") %>%
    html_attr("href")
  #extract importantlinks
  patternList <- c("worldclim21", "monthlywth", "cmip6climate")
  # HC_data <- grep(pattern = patternList[1], x = dataPage, value = T)[[1]]
  # gsub(pattern = "index", HC_data,dataPageUrl2)
  
  urls <- lapply(patternList, function(eachPattern){
    gsub(pattern = "index", replacement = eachPattern,dataPageUrl2)
  })
  list_of_urls <- unlist(urls) 
  if(category=="HCD"){
    pattern <- paste0("_",Product_resolution,"m","_",product)
    download_list <- read_html(list_of_urls[[1]]) %>% 
      html_nodes("a") %>%
      html_attr("href") %>%
      str_subset(paste0(pattern, ".zip")) #%>%
    lapply(download_list,function(each){
      options(timeout=3600)
      download.file(url = each, destfile = paste0(getwd(),"/", pattern, ".zip"))
    })
    #return(download_list)
  }else {
    if(category=="HMWD"){
      for(i in 1:length(period)){
        pattern <- paste0("_",product,"_", period[[i]])
        download_list <- read_html(list_of_urls[[2]]) %>% 
          html_nodes("a") %>%
          html_attr("href") %>%
          str_subset(paste0(pattern, ".zip")) #%>%
        lapply(download_list,function(each){
          options(timeout=43200)
          download.file(url = each, destfile = paste0(getwd(),"/", pattern,".zip"), method = "auto", mode = "wb")
        })
      }
     
    }else{
      pattern <- paste0(product,"_", GCM,"_",SSP,"_",period)
      download_list <- read_html(gsub(pattern ="climate",replacement = paste0("/cmip6_clim",Product_resolution,"m"),x = list_of_urls[[3]]))%>% 
        html_nodes("a") %>%
        html_attr("href") %>%
        str_subset(paste0(pattern, ".zip")) #%>%
      lapply(download_list,function(each){
        options(timeout=36000)
        download.file(url = each, destfile = paste0(getwd(),"/",pattern, ".zip", ))
      })
    }
  }
}


getWC_data(category = "HMWD", product = "tmin", period = c("1960-1969","1970-1979", "1980-1989"))
