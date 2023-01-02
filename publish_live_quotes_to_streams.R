Sys.setenv(TZ='America/New_York')

library(httr)
library(data.table)
library(quantmod)

yarx_securities_raw = jsonlite::fromJSON("https://raw.githubusercontent.com/microprediction/microprediction/master/microprediction/live/xraytickers.json")
yarx_securities = toupper(as.character(unlist(yarx_securities_raw)))

rdps_securities_raw = jsonlite::fromJSON("https://raw.githubusercontent.com/microprediction/microprediction/master/microprediction/live/rdpstickers.json")
rdps_securities = toupper(as.character(unlist(rdps_securities_raw)))

combined_securities = c(yarx_securities, rdps_securities)

sponsor_write_key = "6a1248f2cf43d78cebb54861fea2401c" 

repeat{
  start.time = Sys.time()
  weekend = any(weekdays(Sys.time())%in%c("Saturday", "Sunday"))
  holiday = any(Sys.Date()%in%c("2023-01-02", "2023-01-16", "2023-02-20", "2023-04-07", "2023-05-29", "2023-06-19", "2023-09-04", "2023-12-25"))
  if(!weekend && !holiday){
    if((data.table::hour(start.time)==9 && data.table::minute(start.time)>=30) ||  (data.table::hour(start.time)>=10 && data.table::hour(start.time)<16)){ 
      # Check for early close / holiday with active index
      if(as.numeric(start.time - quantmod::getQuote("^GSPC", what = yahooQF("marketstate")), units = "secs")<60){    
        combined_quotes = data.table::data.table(quantmod::getQuote(combined_securities, what = yahooQF("Last Trade (Price Only)")))
        combined_quotes$ID = combined_securities
        print(combined_quotes)
        
        # To publish streams with .../AAPL_prices.json format for `sponsor_write_key` 
        for(i in 1:length(combined_securities)){
          httr::PUT(url = paste0("https://api.microprediction.org/live/", combined_securities[i], "_prices.json"),
                    body = list(write_key = sponsor_write_key, budget = 1, value = combined_quotes$Last[i]))
        }
        
        eclipsed = as.numeric(Sys.time()-start.time, units="secs")
        
        print(paste("Resting for: ", max(0, 60-eclipsed)))
        
        Sys.sleep(max(0, 60-eclipsed))
      } else {
        print(paste0("Market closed: ", start.time))
        Sys.sleep(60)
      }
    } else {
      if(data.table::minute(start.time)>=59){  
        yarx_securities_raw = jsonlite::fromJSON("https://raw.githubusercontent.com/microprediction/microprediction/master/microprediction/live/xraytickers.json")
        yarx_securities = toupper(as.character(unlist(yarx_securities_raw)))
        
        rdps_securities_raw = jsonlite::fromJSON("https://raw.githubusercontent.com/microprediction/microprediction/master/microprediction/live/rdpstickers.json")
        rdps_securities = toupper(as.character(unlist(rdps_securities_raw)))
        
        combined_securities = c(yarx_securities, rdps_securities)
        
        print(paste0("Market closed: ", start.time))
        Sys.sleep(1770)
      }
    }
  }  else { 
    if(weekend) print(paste0("Weekend: ", start.time))
    if(holiday) print(paste0("Holiday: ", start.time))
    Sys.sleep(60*60*6)
  }
}


