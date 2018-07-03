


library(tidyverse)
library(lubridate)
#library(chron)

# list of time zones
#https://en.wikipedia.org/wiki/List_of_tz_database_time_zones


getDate = function(firstChunk){
  #firstChunk = "03/03/2018"
  
  if(nchar(firstChunk) == 10){
    tryTheseFormats = c('%m-%d-%Y',
                        '%m/%d/%Y', 
                        '%Y-%m-%d', 
                        '%Y/%m/%d')
  } else{
    tryTheseFormats = c('%m/%d/%y',
                        '%m-%d-%y',
                        '%y/%m/%d',
                        '%y-%m-%d')
  }
  
  match=F
  i=0
  while(!match && i<length(tryTheseFormats)){
    i = i+1
    date = as.Date(firstChunk, format=tryTheseFormats[i])
    match = !is.na(date)
  }
  
  return(date)
}

spaceDelim = function(lines){
  dataLines = sub("\t", " ", lines) %>%
    str_squish() %>%
    str_split(' ')
  return(dataLines)
}


makeORFIDtagDF = function(tagDataDF){
  date = as.Date(tagDataDF[,2])
  time = as.character(tagDataDF[,3])
  fracsec = round(as.numeric(str_sub(time, 9, 11)), 2)
  datetime = strptime(paste(date, time),format='%Y-%m-%d %H:%M:%S', tz=tz)
  duration = period_to_seconds(hms(tagDataDF[,4]))
  tagtype = as.character(tagDataDF[,5])
  tagid = as.character(tagDataDF[,6])
  antnum = NA
  consdetc = as.numeric(tagDataDF[,7])
  arrint = as.character(tagDataDF[,8])
  arrint[arrint == '.'] = '65001'
  arrint = as.numeric(arrint)
  
  return(data.frame(date, time, fracsec, datetime, duration, tagtype, tagid, antnum, consdetc, arrint, stringsAsFactors = F))
}

makeBiomarkDF = function(tagDataDF){
  date = as.Date(do.call("c", lapply(as.character(tagDataDF[,4]), getDate)))
  time = as.character(str_sub(tagDataDF[,5], 1, 11))
  fracsec = round(as.numeric(str_sub(time, 9, 11)), 2)
  datetime = strptime(paste(date, time),format='%Y-%m-%d %H:%M:%S', tz=tz)
  duration = NA
  tagtype = NA
  tagid = as.character(str_replace(tagDataDF[,6], '[.]', '_')) #str_replace(tagDataDF[,6], '[.]', '_')
  antnum = NA
  consdetc = NA
  arrint = NA
  return(data.frame(date, time, fracsec, datetime, duration, tagtype, tagid, antnum, consdetc, arrint, stringsAsFactors = F))
}


parseORFIDmsg = function(line){
  date = line[2]
  time = line[3]
  # skipping duration 
  msg = str_c(line[5:length(line)], collapse=' ')
  return(data.frame(date, time, msg))
}

parseBiomarkMsg = function(line){
  date = line[4]
  time = line[5]
  # skipping duration 
  msg = str_c(line, collapse=' ')
  return(data.frame(date, time, msg))
}


addInfo = function(df, lineNumbers, archiveFile, site, reader){
  df$site = site
  df$reader = reader
  df$fname = archiveFile
  df$line = lineNumbers
  df$dateadded = Sys.Date()
  return(df)
}


###############################################################################################################
###############################################################################################################
###############################################################################################################

dataDir = "C:\\Users\\braatenj\\Documents\\GitHub\\pit-tag-data-compile\\example"
tagDBfile = "C:\\Users\\braatenj\\Documents\\GitHub\\pit-tag-data-compile\\example\\tagDB.csv"
msgDBfile = "C:\\Users\\braatenj\\Documents\\GitHub\\pit-tag-data-compile\\example\\msgDB.csv"
failDBfile = "C:\\Users\\braatenj\\Documents\\GitHub\\pit-tag-data-compile\\example\\failDB.csv"
timeZone = "America/Los_Angeles"

siteDirs = normalizePath(list.dirs(dataDir, recursive = F))
tz = timeZone

###############################################################################################################
###############################################################################################################
###############################################################################################################



for(dir in siteDirs){
  dir = siteDirs[2]
  print(str_glue(dir))
  downloadDir = normalizePath(file.path(dir,"downloads"))
  archiveDir = normalizePath(file.path(dir,"archive"))
  logFiles = normalizePath(list.files(downloadDir, '*', full.names = T))
  
  # if log files then parse it, else next
  if(length(logFiles) != 0){
    for(logFile in logFiles){
      logFile = logFiles[1]
      bname = basename(logFile)
      archiveFile = suppressWarnings(normalizePath(file.path(archiveDir,str_glue(as.character(Sys.Date()),'_',bname))))
      
      lines = read_lines(logFile)
      

      
      # is this file orfid or biomark - need to get site name
      lineLen = length(lines)
      end = ifelse(lineLen < 100, lineLen, 100)
      if(length(which(str_detect(lines[1:end], 'Oregon RFID Datalogger') == TRUE)) != 0){
        site = unlist(str_split(basename(logFile),'_'))[1]
        reader = 'ORFID'
        lineStart = substr(lines, 1, 2)
        
        
        ########## DEAL WITH THE TAG DATA (D CODE)
        dataMaybe = which(lineStart == 'D ')
        dataMaybeLength = length(dataMaybe)
        if(dataMaybeLength > 0){
          dataLines = spaceDelim(lines[dataMaybe])
          
          #...do a check on the date to make sure that its a proper date
          lens = lengths(dataLines)
          date = unlist(lapply(dataLines, function(l) {unlist(l[2])}))
          dateCheck = do.call("c", lapply(date, getDate)) # need to use do.call('c') here to unlist because unlist reformats the date
          
          #... for dates that are good and the number of columns is correct, assume they are tags and put them in a DF
          tagDataLines = dataMaybe[which(lens == 8 & !is.na(dateCheck))]
          tagDataLinesLength = length(tagDataLines)
          if(tagDataLinesLength > 0){
            tagDataList = spaceDelim(lines[tagDataLines])
            tagDataMatrix = do.call(rbind, tagDataList)
            tagDataDF = as.data.frame(tagDataMatrix) %>%  #cbind(tagDataMatrix, tagDataLines)
              makeORFIDtagDF() %>%
              addInfo(tagDataLines, archiveFile, site, reader)
          }
          
          #... for dates that are good but the number of columns is incorrect, assume they are failed reads and put them in a separate DF
          tagDataFailLines = dataMaybe[which(lens != 8 & !is.na(dateCheck))]
          tagDataFailLinesLength = length(tagDataFailLines)
          if(tagDataFailLinesLength > 0){
            tagDataFailList = spaceDelim(lines[tagDataFailLines])
            tagDataFailDF = do.call("rbind", lapply(tagDataFailList, parseORFIDmsg)) %>%
              addInfo(tagDataFailLines, archiveFile, site, reader)
          }
          
          #... for D codes that have a bad date, put them in a separate DF  ---- NEED TO UNMOCK THIS
          tagDataJunkLines = dataMaybe[which(is.na(dateCheck))]
          tagDataJunkLinesLength = length(tagDataJunkLines)
          if(tagDataJunkLinesLength > 0){
            tagDataJunkVector = lines[tagDataJunkLines]
            tagDataJunkDF = data.frame(msg = tagDataJunkVector) %>%
              addInfo(tagDataJunkLines, archiveFile, site, reader)
          }
        }
        

        ##########  DEAL WITH THE MESSAGE DATA (E AND B CODES)
        msgMaybe = which(lineStart == 'E ' | lineStart == 'B ')
        msgMaybeLength = length(msgMaybe)
        if(msgMaybeLength > 0){
          msgLines = spaceDelim(lines[msgMaybe])
          
          #...do a check on the date to make sure that its a proper date
          date = unlist(lapply(msgLines, function(l) {unlist(l[2])}))
          dateCheck = do.call("c", lapply(date, getDate)) # need to use do.call('c') here to unlist because unlist reformats the date
          
          #... for dates that are good, assume they are messages and put them in a DF
          msgDataLines = msgMaybe[which(!is.na(dateCheck))]
          msgDataLinesLength = length(msgDataLines)
          if(msgDataLinesLength > 0){
            msgDataList = spaceDelim(lines[msgDataLines])
            msgDataDF = do.call("rbind", lapply(msgDataList, parseORFIDmsg)) %>%
              addInfo(msgDataLines, archiveFile, site, reader)
          }
          
          #... for E and B codes that have a bad date, put them in a separate DF
          msgDataJunkLines = msgMaybe[which(is.na(dateCheck))]
          msgDataJunkLinesLength = length(msgDataJunkLines)
          if(msgDataJunkLinesLength > 0){
            msgDataJunkVector = lines[msgDataJunkLines]
            msgDataJunkDF = data.frame(msg = tagDataJunkVector) %>%
              addInfo(msgDataJunkLines, archiveFile, site, reader)
          }
        }
        

        ##########  DEAL WITH OTHER
        otherLines = which(lineStart != 'D ' & lineStart != 'E ' & lineStart != 'B ')
        otherLinesLength = length(otherLines)
        if(otherLinesLength > 0){
          otherVector = lines[otherLines] %>%
            sub("\t", " ", .) %>%
            str_squish()
          otherDF = data.frame(msg = otherVector) %>%
            addInfo(otherLines, archiveFile, site, reader)
        }
        
        
      } else {
        site = basename(dirname(dirname(logFile)))
        reader = 'Biomark'
        lineStart = substr(lines, 1, 4)
        #checkCode = substr(lines, 4, 5)
        
        
        ########## DEAL WITH THE TAG DATA (TAG: CODE)
        dataMaybe = which(lineStart == 'TAG:')
        dataMaybeLength = length(dataMaybe)
        if(dataMaybeLength > 0){
          dataLines = spaceDelim(lines[dataMaybe])
          
          #...do a check on the date to make sure that its a proper date
          lens = lengths(dataLines)
          date = unlist(lapply(dataLines, function(l) {unlist(l[4])}))
          dateCheck = do.call("c", lapply(date, getDate)) # need to use do.call('c') here to unlist because unlist reformats the date
          
          #... for dates that are good and the number of columns is correct, assume they are tags and put them in a DF
          tagDataLines = dataMaybe[which(lens == 6 & !is.na(dateCheck))]
          tagDataLinesLength = length(tagDataLines)
          if(tagDataLinesLength > 0){
            tagDataList = spaceDelim(lines[tagDataLines])
            tagDataMatrix = do.call(rbind, tagDataList)
            
            #tagDataDF = head(as.data.frame(tagDataMatrix))
            tagDataDF = as.data.frame(tagDataMatrix) %>%  #cbind(tagDataMatrix, tagDataLines)
              makeBiomarkDF() %>%
              addInfo(tagDataLines, archiveFile, site, reader)
          }
          
          #... for dates that are good but the number of columns is incorrect, assume they are failed reads and put them in a separate DF
          tagDataFailLines = dataMaybe[which(lens != 6 & !is.na(dateCheck))]
          tagDataFailLinesLength = length(tagDataFailLines)
          if(tagDataFailLinesLength > 0){
            tagDataFailList = spaceDelim(lines[tagDataFailLines])
            tagDataFailDF = do.call("rbind", lapply(tagDataFailList, parseBiomarkMsg)) %>% 
              addInfo(tagDataFailLines, archiveFile, site, reader)
          }
          
          #... for TAG: codes that have a bad date, put them in a separate DF
          tagDataJunkLines = dataMaybe[which(is.na(dateCheck))]
          tagDataJunkLinesLength = length(tagDataJunkLines)
          if(tagDataJunkLinesLength > 0){
            tagDataJunkVector = lines[tagDataJunkLines]
            tagDataJunkDF = data.frame(msg = tagDataJunkVector) %>%
              addInfo(tagDataJunkLines, archiveFile, site, reader)
          }
          
          
          
          
          
        }
        
        
        
        
        
        

        
        
        
        
        
        
        
        
        
        
      }
      
      # write out the files here
      

      
      
      
      
    }
  
  } else {
    print(str_glue('...skip, no log files'))
    next
  }
  
  
  
}





