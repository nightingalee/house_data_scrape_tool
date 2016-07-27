#install.packages("tidyr")
#install.packages("xml2")
#install.packages("ggmap")
#install.packages("data.table")
#install.packages("xlsx")

library(tidyr)
library(xml2)
library(rvest)
library(data.table)
library(ggmap)
#library(XLConnect)

strhead<-"http://www.realestate.com.au"
dlist<-list()

for(i in 1:3){
  
  #Geebung
  #url<-paste("http://www.realestate.com.au/buy/property-house-with-2-bedrooms-in-geebung%2c+qld+4034%3b+/list-",i
             #,"?maxBeds=5&includeSurrounding=false&misc=ex-under-contract&source=location-search",sep="")
  
  #Zilmere
  #url <-paste("http://www.realestate.com.au/buy/property-house-with-2-bedrooms-in-zilmere/list-",i,
              #"?maxBeds=5&includeSurrounding=false&persistIncludeSurrounding=true&misc=ex-under-contract&source=location-search",sep="")
  
  #Mitchelton
  #url<-paste("http://www.realestate.com.au/buy/property-house-with-2-bedrooms-in-mitchelton%2c+qld+4053%3b+/list-",i
             #,"?maxBeds=5&includeSurrounding=false&misc=ex-under-contract&source=location-search",sep="")
  
  #Wynnum West
  # url<-paste("http://www.realestate.com.au/buy/property-house-with-2-bedrooms-in-wynnum+west%2c+qld+4178%3b+/list-",
  #,"?maxBeds=5&includeSurrounding=false&misc=ex-under-contract&source=location-search",sep="")

  #Rochedale South
  #url<-paste("http://www.realestate.com.au/buy/property-house-with-2-bedrooms-in-rochedale+south%2c+qld+4123%3b+/list-",i
             #,"?maxBeds=5&includeSurrounding=false&misc=ex-under-contract&source=location-search",sep="")
  
  #Acacia Ridge
  #url<-paste("http://www.realestate.com.au/buy/property-house-with-2-bedrooms-in-acacia+ridge%2c+qld+4110/list-",
             #i,"?maxBeds=5&includeSurrounding=false&persistIncludeSurrounding=true&misc=ex-under-contract&source=location-search",sep="")
  
  #Algester
  #url<-paste("http://www.realestate.com.au/buy/property-house-with-2-bedrooms-in-algester%2c+qld+4115/list-",
             #i,"?maxBeds=5&includeSurrounding=false&misc=ex-under-contract&source=location-search",sep="")
  
  #Parkinson
  #url<-paste("http://www.realestate.com.au/buy/property-house-with-2-bedrooms-in-parkinson%2c+qld+4115/list-",
             #i,"?maxBeds=5&includeSurrounding=false&misc=ex-under-contract&source=location-search",sep="")
  
  #Springwood
  #url<- paste("http://www.realestate.com.au/buy/property-house-with-2-bedrooms-between-0-600000-in-springwood%2c+qld+4127/list-",
  #i,"?maxBeds=5&includeSurrounding=false&misc=ex-under-contract&source=location-search",sep="")
  
  #Runcorn
  #url<-paste("http://www.realestate.com.au/buy/property-house-with-2-bedrooms-in-runcorn%2c+qld+4113%3b+/list-",i,"?maxBeds=5&includeSurrounding=false&misc=ex-under-contract&source=location-search",sep="")
  
  #Slacks Creek
  url<-paste("http://www.realestate.com.au/buy/property-house-with-2-bedrooms-in-slacks+creek%2c+qld+4127%3b+/list-",i,"?maxBeds=5&includeSurrounding=false&misc=ex-under-contract&source=location-search",sep="")
  
  
  
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
    links <- tryCatch(html_attr(xml_node(x, ".name"),"href"),
                      error=function(err) {NA})
    data.frame(price,bedrooms,bathrooms,car,address,links,stringsAsFactors=FALSE)
  }))
  
  
  dlist[[i]]<-d
}

ddlist<-rbindlist(dlist)

price<-ddlist$price


price<-substr(price,regexpr(pattern="[$]",price),regexpr(pattern="[$]",price)+8)
price<-gsub("\\$","",price)
price<-gsub(",","",price)
price<-gsub(" ","",price)
price<-substr(price,1,6)
price<-as.numeric(price)


ddlist$price<-price
#Do some maps of the addresses
ddlist$cprice<-cut(ddlist$price,breaks=c(0,350000,400000,450000,500000,550000,Inf))



dims<-dim(ddlist)
size<-dims[1]
LandSize<-numeric()
for (j in 1:size) {
  sub_url<-paste(strhead,ddlist$links[j],sep="")
  
  main.page <- read_html(x=sub_url)
  landsize <- main.page %>% # feed `main.page` to the next step
  html_nodes(".featureList li:nth-child(5)") %>% # get the CSS nodes
  html_text() # extract the link text
  landsize<-sub(".*:", "", landsize)
  landsize<-sub("m.*", "", landsize)
  landsize<-sub(" .*", "", landsize)
  landsize<-as.numeric(landsize)
  if  (length(landsize)==0){
    landsize<-NA
  }
  LandSize[j]<-landsize
  
}


ddlist=cbind(ddlist,LandSize)

ddlist$cland<-cut(ddlist$LandSize,breaks=c(0,808.999,Inf))

library(ggmap)

lonlat<-geocode(ddlist$address)

ddlist$lon<-lonlat$lon
ddlist$lat<-lonlat$lat

bris<-get_map(location="brisbane")
zillmap<-qmap("slacks creek,QLD",zoom=13)

x11(35,35)
zillmap+geom_point(aes(x=lon,y=lat,size=cland,colour=cprice),data=ddlist)

datenow=Sys.Date()
filename=paste("Slacks_Creek_",datenow,".csv",sep="")

write.csv(ddlist, file=filename)



#summary(lm(price~as.factor(bedrooms)+as.factor(bathrooms)+lat+lon,data=ddlist))




#How to find all the classes in a web page
#page <- read_html(doc_url)

#page %>% 
#html_nodes("*") %>% 
#html_attr("class") %>% 
#unique()

