#Quote compiler
library(tidyverse)
library(rvest)
quotes<-data.frame(quote=NA,author=NA)

returnRows<-function(x,cond_func,manip){
  if(cond_func(x)){
    if(!missing(manip)){
      x2<-manip(x)
    }
    return(x)
  }else{
    return(NULL)
  }
}

q1<-read_html("https://www.briantracy.com/blog/personal-success/26-motivational-quotes-for-success/") %>% 
  html_nodes("h3")%>%
  html_text 

q1_df<-q1 %>% 
  map(~returnRows(.x,
                      function(x){grepl("^(\\d+))",x)},
                      function(x){
                        x<-gsub("((\\d+))\\s*)|(")|(")|(Inspirational Quote by)","",x)
                        if(grepl(" - ",x)){
                          x<-strsplit(x,split = " - ",perl = TRUE)[[1]]
                        }else{
                          x[2]<-NA
                        }
                        x<-trimws(x)
                        x<-data.frame(Quote=x[1], Author=x[2])
                        return(x)
                        }))%>%
  bind_rows


q2<- read_html("https://www.forbes.com/sites/kevinkruse/2013/05/28/inspirational-quotes/#59f224566c7a")%>%
  html_nodes(".article-container") %>% 
  html_nodes("p") %>% 
  html_text()

q2_df<-q2 %>% 
  map(~returnRows(.x,
                  function(x){grepl("^(\\d+)[.]",x)},
                  function(x){
                    x<-gsub("((\\d+)[.]\\s*)","",x)
                    if(grepl(" -",x)){
                      x<-strsplit(x,split = " -",perl = TRUE)[[1]]
                    }else{
                      x[2]<-NA
                    }
                    x<-trimws(x)
                    x<-data.frame(Quote=x[1], Author=x[2])
                    return(x)
                  }))%>%
  bind_rows


q3<- read_html("http://billcrosby.com/socialmedia/top-100-motivational-quotes-of-all-time/")%>%
  html_nodes(".entry-content") %>% 
  html_nodes("p") %>% 
  html_text()

q3_df<-q3 %>% 
  map(~returnRows(.x,
                  function(x){grepl("^(\\d+)[.]",x)},
                  function(x){
                    x<-gsub("((\\d+)[.]\\s*)","",x)
                    if(grepl("\n-",x)){
                      x<-strsplit(x,split = "\n-",perl = TRUE)[[1]]
                      x[2]<-strsplit(x[2],split = "(\\()|(,)")[[1]][1]
                    }else{
                      x[2]<-NA
                    }
                    x<-trimws(x)
                    x<-data.frame(Quote=x[1], Author=x[2])
                    return(x)
                  }))%>%
  bind_rows

q4<-read_html("https://motivationping.com/quotes/")%>%
  html_nodes("p") %>% 
  html_nodes("strong") %>% 
  html_text

q4_df<-q4 %>% 
  map(~returnRows(.x,
                  function(x){grepl("^(\\d+)[.]",x)},
                  function(x){
                    x<-gsub("((\\d+)[.]\\s*)","",x)
                    if(grepl(" - ",x)){
                      x<-strsplit(x,split = " - ",perl = TRUE)[[1]]
                    }else{
                      x[2]<-NA
                    }
                    x<-trimws(x)
                    x<-data.frame(Quote=x[1], Author=x[2])
                    return(x)
                  }))%>%
  bind_rows



q5<-read_html("https://www.huffingtonpost.com/lolly-daskal-/100-motivational-quotes-t_b_4505356.html")%>%
  html_nodes(".content-list-component") %>% 
  html_nodes("p") %>% 
  html_text

q5_df<-q5 %>% 
  map(~returnRows(.x,
                  function(x){grepl("^(\\d+)[.]",x)},
                  function(x){
                    x<-gsub("((\\d+)[.]\\s*)","",x)
                    if(grepl(" ~",x)){
                      x<-strsplit(x,split = " ~",perl = TRUE)[[1]]
                      x[2]<-strsplit(x[2],split = "\nAs")[[1]][1]
                      
                    }else{
                      x[2]<-NA
                    }
                    x<-trimws(x)
                    x<-data.frame(Quote=x[1], Author=x[2])
                    return(x)
                  }))%>%
  bind_rows



q6<-read_html("https://blog.hubspot.com/sales/18-motivational-quotes-to-start-your-day-list")%>%
  html_nodes(".hs_cos_wrapper") %>% 
  html_nodes("p") %>% 
  html_text

q6_df<-q6 %>% 
  map(~returnRows(.x,
                  function(x){grepl("^(\\d+)[.]",x)},
                  function(x){
                    x<-gsub("((\\d+)[.]\\s*)","",x)
                    x<-gsub("(\\\")","",x)
                    
                    if(grepl("[.?] -",x)){
                      x<-strsplit(x,split = "[.?] -")[[1]]
                    }else{
                      x[2]<-NA
                    }
                    x<-trimws(x)
                    x<-data.frame(Quote=x[1], Author=x[2])
                    return(x)
                  }))%>%
  bind_rows



quote_list<-eval(parse(text=paste("bind_rows(list(",paste(ls()[grep("_df",ls())],collapse=", "),"))")))

#Number of distinct quotes
table(!duplicated(quote_list$Quote))

table(!duplicated(tolower(quote_list$Author[!(is.na(quote_list$Author) | grepl("unknown",quote_list$Author))])))



