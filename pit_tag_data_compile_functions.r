

library(tidyverse)
library(lubridate)
#library(chron)

# list of time zones
#https://en.wikipedia.org/wiki/List_of_tz_database_time_zones

# define function

# junk coulkd always be: \001\006  


isRecInd = function(firstChunk){
  return(firstChunk %in% c('E','D'))
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


getLineChunks = function(line){
  Chunks = str_replace(line, '\t', ' ') %>%
    str_squish() %>%
    str_split(' ') %>%
    unlist()
  return(Chunks)
}


# fixMissingAntNum = function(lineChunks){
#   len = length(lineChunks)
#   if(len == 7){
#     lineChunks = c(lineChunks[1:5], NA, lineChunks[6:7])
#   } else if(!(len %in% c(7, 8))){
#     stop('The number of chunks in this line does not match 7 or 8')
#   }
#   return(lineChunks)
# }


parseMsg = function(lineChunks, tz){
  # get rid of first chunk if it is a detection indicator 
  if(isRecInd(lineChunks[1])){
    lineChunks = lineChunks[2:length(lineChunks)]
  }
  # parse the chunks
  date = getDate(lineChunks[1])
  time = lineChunks[2]
  fracsec = round(as.numeric(str_sub(time, 9, 11)), 2)
  datetime = strptime(paste(date, time),format='%Y-%m-%d %H:%M:%S', tz=tz)
  message = str_flatten(lineChunks[3:length(lineChunks)], ' ')
  return(data.frame(date, time, fracsec, datetime, message))
}


parseORFIDtag = function(lineChunks, tz){
  # get rid of first chunk if it is a detection indicator 
  if(isRecInd(lineChunks[1])){
    lineChunks = lineChunks[2:length(lineChunks)]
  }
  len = length(lineChunks)
  if(len != 7){
    print('fail')
    return('fail')
  }
  
  # fix missing antenna number
  # lineChunks = fixMissingAntNum(lineChunks)
  
  # parse the chunks
  date = getDate(lineChunks[1]) #TODO if date returned is NA return as failed line
  time = lineChunks[2]
  fracsec = round(as.numeric(str_sub(time, 9, 11)), 2)
  datetime = strptime(paste(date, time),format='%Y-%m-%d %H:%M:%S', tz=tz)
  duration = period_to_seconds(hms(lineChunks[3]))
  if(str_length(lineChunks[4]) == 2){
    tagtype = str_sub(lineChunks[4],2,2)
  } else {
    tagtype = lineChunks[4]
  }
  tagid = lineChunks[5]
  antnum = NA #lineChunks[6]
  consdetc = as.numeric(lineChunks[6])
  arrint = lineChunks[7]
  if(arrint == '.'){
    arrint = 65001
  } else{
    arrint = as.numeric(arrint)
  }
  return(data.frame(date, time, fracsec, datetime, duration, tagtype, tagid, antnum, consdetc, arrint))
}


parseMTStag = function(lineChunks, tz){
  len = length(lineChunks)
  if(len != 6){
    return('fail')
  }
  date = getDate(lineChunks[4])
  time = str_sub(lineChunks[5], 1, 11)
  fracsec = round(as.numeric(str_sub(time, 9, 11)), 2)
  datetime = strptime(paste(date, time),format='%Y-%m-%d %H:%M:%S', tz=tz)
  duration = NA
  tagtype = NA
  tagid = str_replace(lineChunks[6], '[.]', '_')
  antnum = NA
  consdetc = NA
  arrint = NA
  return(data.frame(date, time, fracsec, datetime, duration, tagtype, tagid, antnum, consdetc, arrint))
}


parseMTSmsg = function(lineChunks, tz){
  len = length(lineChunks)
  if(len < 6){
    return('fail')
  }
  date = getDate(lineChunks[4])
  time = str_sub(lineChunks[5], 1, 11)
  fracsec = round(as.numeric(str_sub(time, 9, 11)), 2)
  datetime = strptime(paste(date, time),format='%Y-%m-%d %H:%M:%S', tz=tz)
  message = str_flatten(lineChunks[6:length(lineChunks)], ' ')
  return(data.frame(date, time, fracsec, datetime, message))
}


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



##############################################################################################
##### THESE ARE FOR TESTING #####
##############################################################################################

# give the paths
# fn = "D:\\work\\proj\\pittag\\all_pit_data\\RSC\\Downloads\\RSC_2018_04_06_External_Memory\\01_00089.log" # BioMark
# fn = "D:\\work\\proj\\pittag\\all_pit_data\\Sleepy\\Downloads\\sh1-downstream\\blueTerm_20180315_101235.log" # blueterm, . gap, bad records
# fn = "D:\\work\\proj\\pittag\\all_pit_data\\CAWD\\Downloads\\CAWD_2018_03_12.log" # no indicator and missing ant number
# fn = "D:\\work\\proj\\pittag\\all_pit_data\\CAWD\\Downloads\\CAWD_2017_01_06.TXT" # includes e and d at beginning

# fns = c("D:\\work\\proj\\pittag\\all_pit_data\\RSC\\Downloads\\RSC_2018_04_06_External_Memory\\01_00089.log",
#         "D:\\work\\proj\\pittag\\all_pit_data\\Sleepy\\Downloads\\sh1-downstream\\blueTerm_20180315_101235.log",
#         "D:\\work\\proj\\pittag\\all_pit_data\\CAWD\\Downloads\\CAWD_2018_03_12.log",
#         "D:\\work\\proj\\pittag\\all_pit_data\\CAWD\\Downloads\\CAWD_2017_01_06.TXT")
##############################################################################################



dataDir = "C:\\Users\\braatenj\\Documents\\GitHub\\pit-tag-data-compile\\example"
tagDBfile = "C:\\Users\\braatenj\\Documents\\GitHub\\pit-tag-data-compile\\example\\tagDB.csv"
msgDBfile = "C:\\Users\\braatenj\\Documents\\GitHub\\pit-tag-data-compile\\example\\msgDB.csv"
failDBfile = "C:\\Users\\braatenj\\Documents\\GitHub\\pit-tag-data-compile\\example\\failDB.csv"
timeZone = "America/Los_Angeles"

siteDirs = normalizePath(list.dirs(dataDir, recursive = F))
tz = timeZone




for(dir in siteDirs){
  print(str_glue(dir))
  downloadDir = normalizePath(file.path(dir,"downloads"))
  archiveDir = normalizePath(file.path(dir,"archive"))
  logFiles = normalizePath(list.files(downloadDir, '*', full.names = T))
  
  # if log files then parse it, else next
  if(length(logFiles) != 0){
      for(logFile in logFiles){
      lines = read_lines(logFile)
      
      # is this file orfid or biomark - need to get site name
      lineLen = length(lines)
      end = ifelse(lineLen < 100, lineLen, 100)
      if(length(which(str_detect(lines[1:end], 'Oregon RFID Datalogger') == TRUE)) != 0){
        site = unlist(str_split(basename(logFile),'_'))[1]
        reader = 'ORFID'
      } else {
        site = basename(dirname(dirname(logFile)))
        reader = 'Biomark'
      }

      bname = basename(logFile)
      archiveFile = suppressWarnings(normalizePath(file.path(archiveDir,str_glue(as.character(Sys.Date()),'_',bname))))
      
      # set up holders
      holders = getEmptyHolders(tz)
      msgDF = holders$msgDF
      tagDF = holders$tagDF
      badLines = holders$badLines
      
      
      
      # loop through the lines
      for(i in 1:length(lines)){
        # break up the line
        print(str_glue(bname,i, .sep = " "))
        line = lines[i]
        lineChunks = getLineChunks(line)
        firstChunk = lineChunks[1]
        
        # figure out what we're dealing with
        if(isRecInd(firstChunk)){ # ORFID 'D' or 'E'
          if(firstChunk %in% c('E','B')){
            df = parseMsg(lineChunks, tz)
            df$site = site
            df$fname = archiveFile
            df$line = i
            df$reader = reader
            msgDF = rbind(msgDF, df)
          } else if(firstChunk == 'D'){
            df = parseORFIDtag(lineChunks, tz)
            if(class(df) == 'character'){
              badLine = failedLine(site, reader, archiveFile, i, line)
              badLines = c(badLines, badLine)
              next()
            }
            df$site = site
            df$fname = archiveFile
            df$line = i
            df$reader = reader
            tagDF = rbind(tagDF, df)
          }
        } else if(!is.na(getDate(firstChunk))){   # ORFID message
          isMsg = is.na(suppressWarnings(as.numeric(str_sub(lineChunks[3],1,1))))
          if(isMsg){
            df = parseMsg(lineChunks, tz)
            df$site = site
            df$fname = archiveFile
            df$line = i
            df$reader = reader
            msgDF = rbind(msgDF, df)
          } else {
            df = parseORFIDtag(lineChunks, tz)
            if(class(df) == 'character'){
              badLine = failedLine(site, reader, archiveFile, i, line)
              badLines = c(badLines, badLine)
              next()
            }
            df$site = site
            df$fname = archiveFile
            df$line = i
            df$reader = reader
            tagDF = rbind(tagDF, df)
          }
        } else if(firstChunk %in% c('MSG:', 'ALM', 'NRP', 'SRP') ){
          df = parseMTSmsg(lineChunks, tz)
          if(class(df) == 'character'){
            badLine = failedLine(site, reader, archiveFile, i, line)
            badLines = c(badLines, badLine)
            next()
          }
          df$site = site
          df$fname = archiveFile
          df$line = i
          df$reader = reader
          msgDF = rbind(msgDF, df)
        } else if(firstChunk == 'TAG:'){
          df = parseMTStag(lineChunks, tz)
          if(class(df) == 'character'){
            badLine = failedLine(site, reader, archiveFile, i, line)
            badLines = c(badLines, badLine)
            next()
          }
          df$site = site
          df$fname = archiveFile
          df$line = i
          df$reader = reader
          tagDF = rbind(tagDF, df)
        }
      }
      
      
      # remove any dup data and append to the tag DB
      tagDF = tagDF[!duplicated(tagDF),]
      tagDF = tagDF[2:nrow(tagDF),]
      
      # tagDF = tagDF[!is.na(tagDF$date),]
      # tagDF = tagDF[!is.na(tagDF$time),]
      # tagDF = tagDF[!is.na(tagDF$fracsec),]
      # tagDF = tagDF[!is.na(tagDF$datetime),]
      # tagDF = tagDF[!is.na(tagDF$tagid),]
      tagDF$added = Sys.Date()
      
      
      
      # remove any dup data and append to the msg DB
      msgDF = msgDF[!duplicated(msgDF),]
      msgDF = msgDF[2:nrow(msgDF),]
      
      # msgDF = msgDF[!is.na(msgDF$date),]
      # msgDF = msgDF[!is.na(msgDF$time),]
      # msgDF = msgDF[!is.na(msgDF$fracsec),]
      # msgDF = msgDF[!is.na(msgDF$datetime),]
      # msgDF = msgDF[!is.na(msgDF$tagid),]
      msgDF$added = Sys.Date()
      
      
      write_csv(tagDF, tagDBfile, append=T)
      write_csv(msgDF, msgDBfile, append=T)
      write(badLines,failDBfile,ncolumns=1,append=T)
      file.rename(logFile, archiveFile)
    
    }
    
  } else {
    print(str_glue('...skip, no log files'))
    next
  }
  
}


# read in the date
col_types = cols(
  X1 = col_character(),
  X2 = col_character(),
  X3 = col_date(format = ""),
  X4 = col_time(format = ""),
  X5 = col_double(),
  X6 = col_datetime(format = ""),
  X7 = col_double(),
  X8 = col_character(),
  X9 = col_character(),
  X10 = col_character(),
  X11 = col_integer(),
  X12 = col_integer(),
  X13 = col_character(),
  X14 = col_integer(),
  X15 = col_date(format = "")
)

tagDF = read_csv(tagDBfile, col_names = F, col_types = col_types)


# remove any duplicates
tagDF = tagDF[!duplicated(tagDF),]

# write out the clean data compilation
write_csv(tagDF, tagDBfile, col_names = F)



