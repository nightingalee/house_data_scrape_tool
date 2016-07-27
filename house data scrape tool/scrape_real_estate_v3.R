# install.packages("tidyr")
# install.packages("xml2")
# install.packages("ggmap")
# install.packages("data.table")
# install.packages("xlsx")
# install.packages("XLConnect")

# load packages
library(RCurl)
library(XML)
library(xml2)
library(rvest)
library(data.table)
library(xlsx)
library(ggmap)
library(XLConnect)

list_suburbs <- c("Mitchelton","Rochedale South","Acacia Ridge","Springwood")

list_postcodes <- c(4053,4123,4110,4127)

#test Algester

#list_suburbs <- c("Algester")
#list_postcodes <- c(4115)


minBeds <- 2
maxBeds <- "any"
minPrice <- 0
maxPrice <- 800000

#for Mac
#working_directory <- "~/Dropbox/House data scrape tool/Daily data update"

#for windows
working_directory <- "C:/Users/xie014/Dropbox/House data scrape tool/Daily data update"

setwd(working_directory)
filename <- "House_Search_Summary.xlsx"

for(k in 1:length(list_suburbs)){
  subName <- list_suburbs[k]
  subCode <- list_postcodes[k]
  
  text <- gsub(" ", "+", tolower(subName))
  dlist<-list()
  #ThreeB <- c("Rochedale South")
  for(i in 1:3){
    #url <- paste("http://www.realestate.com.au/buy/property-house-with-", minBeds, "-bedrooms-between-", 
                # minPrice, "-", maxPrice, "-in-", text, "%2c+qld+", subCode, "/list-",i,"?activeSort=list-date&maxBeds=", maxBeds, sep="")
    if (subName=='Algester'){
      url <- paste("http://www.realestate.com.au/buy/property-house-with-2-bedrooms-between-0-800000-in-algester%2c+qld+4115/list-",
                   i,"?maxBeds=any&includeSurrounding=false&persistIncludeSurrounding=true&misc=ex-under-contract&source=location-search",sep="")
      }
    else{
      url <- paste("http://www.realestate.com.au/buy/property-house-with-", minBeds, "-bedrooms-between-", 
                   minPrice, "-", maxPrice, "-in-", text, "%2c+qld+", subCode,"/list-",i,"?maxBeds=", maxBeds,
                   "&includeSurrounding=false&misc=ex-under-contract&activeSort=list-date&source=location-search",sep="")
    }
      

    prod<-read_html(url,user_agent="myagent")%>%
      html_nodes(".listingInfo")
    
    d<-do.call(rbind, lapply(prod, function(x) {
      price <- tryCatch(xml_text(xml_node(x, ".priceText")),
                        error=function(err) {NA})
      bedrooms <- tryCatch(xml_text(xml_node(x, "dd:nth-child(2)")),
                           error=function(err) {NA})
      bathrooms <- tryCatch(xml_text(xml_node(x, "dd:nth-child(4)")),
                            error=function(err) {NA})
      car <- tryCatch(xml_text(xml_node(x, "dd:nth-child(6)")),
                      error=function(err) {NA})
      address <- tryCatch(xml_text(xml_node(x, ".name")),
                          error=function(err) {NA})
      str_list <- unlist(strsplit(address, ", "))
      suburb <- str_list[length(str_list)-1]
      link <- unlist(xml_attrs(xml_node(x, ".name")))[1]
      link <- paste("http://www.realestate.com.au", link, sep="")
      str_list <- unlist(strsplit(link, "-"))
      id <- as.numeric(str_list[length(str_list)])
      prod2 <- read_html(link,user_agent="myagent")%>%html_nodes(".featureList")
      land <- xml_text(xml_find_all(prod2, ".//span"))[4]
      land <- as.numeric(unlist(strsplit(land, " "))[1])
      data.frame(id, price,bedrooms,bathrooms,car,suburb, address,land, link,stringsAsFactors=FALSE)
    }))
    dlist[[i]]<-d
    
    #if(subName != d$suburb[nrow(d)]){
      #break
    #}
  }
  ddlist<-rbindlist(dlist)
  ddlist <- ddlist[which(ddlist$suburb == subName), ]
  ddlist<-ddlist[order(ddlist$id),]
  
  price<-ddlist$price
  price<-substr(price,regexpr(pattern="[$]",price),regexpr(pattern="[$]",price)+8)
  price<-gsub("\\$","",price)
  price<-gsub(",","",price)
  price<-gsub(" ","",price)
  price<-substr(price,1,6)
  price<-as.numeric(price)
  
  # ddlist$price<-price
  #Do some maps of the addresses
  ddlist$cprice<-cut(price,breaks=c(0,400000,450000,500000,550000,600000,650000,700000,Inf))
  ddlist$cland <-cut(ddlist$land,breaks=c(0,400,700,808.999,Inf))
  
  lonlat<-geocode(ddlist$address)
  
  ddlist$lon<-lonlat$lon
  ddlist$lat<-lonlat$lat
  
  if (file.exists(filename)){
    wb <- loadWorkbook(filename)
    index <- which(getSheets(wb) == subName)
    if (length(index) > 0){
      old_ddlist <- read.xlsx(filename, sheetName=subName)
      # Remove the items in old_ddlist
      for(pid in old_ddlist$id){
        index <- which(ddlist$id == as.numeric(pid))
        ddlist <- ddlist[-index, ]
      }
      if (nrow(ddlist) == 0){
        ddlist <- old_ddlist
      }
      else{
        ddlist <- merge(old_ddlist, ddlist)
      }
    }
  }
  
  area<-paste(subName,",Brisbane",sep="")
  
  bris<-get_map(location="brisbane")
  zillmap<-qmap(area,zoom=14)
  
  # For MAC
  #quartz(20,18)
  # For Windows
  x11(35,35)
  zillmap+geom_point(aes(x=lon,y=lat,colour=cprice,size=cland),data=ddlist)
  outfile <- paste("Map_House_Search_in_", subName, ".png", sep="")
  ggsave(outfile, width = 18, height = 14, units = "cm")
  dev.off()
  
  if(k > 1){
    write.xlsx(ddlist, filename, sheetName=subName, col.names=TRUE, row.names=TRUE, append=TRUE)
  }
  else{
    write.xlsx(ddlist, filename, sheetName=subName, col.names=TRUE, row.names=TRUE, append=FALSE)
  }
}

# summary(lm(price~land+as.factor(bedrooms)+as.factor(bathrooms)+lat+lon,data=ddlist))

