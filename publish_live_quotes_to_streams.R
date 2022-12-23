Sys.setenv(TZ='America/New_York')

yarx_securities_raw = jsonlite::fromJSON("https://raw.githubusercontent.com/microprediction/microprediction/master/microprediction/live/xraytickers.json")
yarx_securities = toupper(as.character(unlist(yarx_securities_raw)))

rdps_securities_raw = jsonlite::fromJSON("https://raw.githubusercontent.com/microprediction/microprediction/master/microprediction/live/rdpstickers.json")
rdps_securities = toupper(as.character(unlist(rdps_securities_raw)))

combined_securities = c(yarx_securities, rdps_securities)

sponsor_write_key = "6a1248f2cf43d78cebb54861fea2401c" 

repeat{
  start.time = Sys.time()
  weekend = any(weekdays(Sys.time())%in%c("Saturday", "Sunday"))
  if(!weekend){
    if((data.table::hour(start.time)==9 && data.table::minute(start.time)>=30) ||  
       (data.table::hour(start.time)>=10 && data.table::hour(start.time)<16 )){    
      combined_quotes = data.table::data.table(quantmod::getQuote(combined_securities, what = yahooQF("Last Trade (Price Only)")))
      combined_quotes$ID = combined_securities
      print(combined_quotes)
      
      # Uncomment to publish streams with .../AAPL_prices.json format for `sponsor_write_key` 
      for(i in 1:length(combined_securities)){
        httr::PUT(url = paste0("https://api.microprediction.org/live/", combined_securities[i], "_prices.json"),
                  body = list(write_key = sponsor_write_key, budget = 1, value = combined_quotes$Last[i]))
      }
      
      eclipsed = as.numeric(Sys.time()-start.time, units="secs")
      
      print(paste("Resting for: ", max(0, 60-eclipsed)))
      
      Sys.sleep(max(0, 60-eclipsed))
    } else {
      if(data.table::minute(start.time)>=59){  
        yarx_securities_raw = jsonlite::fromJSON("https://raw.githubusercontent.com/microprediction/microprediction/master/microprediction/live/xraytickers.json")
        yarx_securities = toupper(as.character(unlist(yarx_securities_raw)))
        
        rdps_securities_raw = jsonlite::fromJSON("https://raw.githubusercontent.com/microprediction/microprediction/master/microprediction/live/rdpstickers.json")
        rdps_securities = toupper(as.character(unlist(rdps_securities_raw)))
        
        combined_securities = c(yarx_securities, rdps_securities)
        
        print(paste0("Market closed: ", start.time))
        Sys.sleep(60)
      }
    }
  }      
}
