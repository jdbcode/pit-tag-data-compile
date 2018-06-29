


library(tidyverse)
library(lubridate)
#library(chron)

# list of time zones
#https://en.wikipedia.org/wiki/List_of_tz_database_time_zones



failedLine = function(site, reader, archiveFile, i, line){
  line = str_glue(
    site,',',
    reader,',',
    as.character(Sys.Date()),',',
    archiveFile,',',
    i,',',
    line
  )
  return(line)
}


getEmptyHolders = function(tz){
  # setup the msg dataframe 
  msgDF = data.frame(
    site = '',
    date=as.Date('2000-01-01'), 
    time='00:00:00.00', 
    fracsec = 0.0, 
    datetime = strptime('2000-01-01 00:00:00',format='%Y-%m-%d %H:%M:%S', tz=tz),
    message = '',
    fname = '',
    line = 0,
    reader = ''
  )
  
  # setup the tag dataframe 
  tagDF = data.frame(
    site = '',
    date = as.Date('2000-01-01'), 
    time = '00:00:00.00', 
    fracsec = 0.0, 
    datetime = strptime('2000-01-01 00:00:00',format='%Y-%m-%d %H:%M:%S', tz=tz),
    duration = 0.0,
    tagtype = '',
    tagid = '',
    antnum = '',
    consdetc = 0,
    arrint = 0,
    fname = '',
    line = 0,
    reader = ''
  )
  
  
  # make a bad line holder
  badLines = vector()
  return(list(msgDF=msgDF, tagDF=tagDF, badLines=badLines))
}

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

makeORFIDmsgDF = function(allLines, theseLines){
  linesVector = allLines[theseLines]
  return(data.frame(linesVector, theseLines, stringsAsFactors = F))
}




dataDir = "C:\\Users\\braatenj\\Documents\\GitHub\\pit-tag-data-compile\\example"
tagDBfile = "C:\\Users\\braatenj\\Documents\\GitHub\\pit-tag-data-compile\\example\\tagDB.csv"
msgDBfile = "C:\\Users\\braatenj\\Documents\\GitHub\\pit-tag-data-compile\\example\\msgDB.csv"
failDBfile = "C:\\Users\\braatenj\\Documents\\GitHub\\pit-tag-data-compile\\example\\failDB.csv"
timeZone = "America/Los_Angeles"

siteDirs = normalizePath(list.dirs(dataDir, recursive = F))
tz = timeZone





for(dir in siteDirs){
  dir = siteDirs[1]
  print(str_glue(dir))
  downloadDir = normalizePath(file.path(dir,"downloads"))
  archiveDir = normalizePath(file.path(dir,"archive"))
  logFiles = normalizePath(list.files(downloadDir, '*', full.names = T))
  
  # if log files then parse it, else next
  if(length(logFiles) != 0){
    for(logFile in logFiles){
      logFile = logFiles[1]
      lines = read_lines(logFile)
      
      
      
      # is this file orfid or biomark - need to get site name
      lineLen = length(lines)
      end = ifelse(lineLen < 100, lineLen, 100)
      if(length(which(str_detect(lines[1:end], 'Oregon RFID Datalogger') == TRUE)) != 0){
        site = unlist(str_split(basename(logFile),'_'))[1]
        reader = 'ORFID'
      
        lineStart = substr(lines, 1, 2)
        
        # deal with the tag data
        dataMaybe = which(lineStart == 'D ')
        dataLines = spaceDelim(lines[dataMaybe])
        lens = lengths(dataLines)
        date = unlist(lapply(dataLines, function(l) {unlist(l[2])}))
        dateCheck = do.call("c", lapply(date, getDate)) # need to use do.call('c') here to unlist because unlist reformats the date
        
        tagDataLines = dataMaybe[which(lens == 8 & !is.na(dateCheck))]
        tagDataList = sepSpace(lines[tagDataLines])
        tagDataMatrix = do.call(rbind, tagDataList)
        tagDataDF = as.data.frame(cbind(tagDataMatrix, tagDataLines)) %>%
          makeORFIDtagDF()
        
        tagDataFailDF = makeORFIDmsgDF(lines, dataMaybe[which(lens != 8 & !is.na(dateCheck))])
        other1DF = makeORFIDmsgDF(lines, dataMaybe[which(is.na(dateCheck))])
        

        # deal with the e data
        msgMaybe = which(lineStart == 'E ')
        msgLines = spaceDelim(lines[msgMaybe])

        parseORFIDmsg = function(line){
          date = line[2]
          time = line[3]
          msg = str_c(line[4:length(line)], collapse=' ')
          return(data.frame(date, time, msg))
        }
        
        msgTest = do.call("rbind", lapply(msgLines, parseMSG))
        
                

        
        
        other2DF = makeORFIDmsgDF(lines, dataMaybe[which(is.na(dateCheck))])
        
        msgData = lines[msgMaybe[!is.na(dateCheck)]]
        other2 = lines[msgMaybe[is.na(dateCheck)]]
        
        # deal with the b data
        msgMaybe = which(lineStart == 'E ' | lineStart == 'B ' )
        msgLines = sub("\t", " ", lines[msgMaybe]) %>%
          str_squish() %>%
          str_split(' ')
        
        lens = lengths(msgLines)
        date = unlist(lapply(dataLines, function(l) {unlist(l[2])}))
        dateCheck = do.call("c", lapply(date, getDate))
        msgData = lines[msgMaybe[!is.na(dateCheck)]]
        other2 = lines[msgMaybe[is.na(dateCheck)]]

        # deal with other
        other3 = lines[which(lineStart != 'D ' & lineStart != 'E ' & lineStart != 'B ')]

        
        
        
        
        
        
        
      } else {
        site = basename(dirname(dirname(logFile)))
        reader = 'Biomark'
      }
      
      
      
      bname = basename(logFile)
      archiveFile = suppressWarnings(normalizePath(file.path(archiveDir,str_glue(as.character(Sys.Date()),'_',bname))))
      
      
      
      
    }
  
  } else {
    print(str_glue('...skip, no log files'))
    next
  }
  
  
  
}





