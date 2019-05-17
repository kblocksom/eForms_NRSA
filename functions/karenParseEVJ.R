


karenOrganizationShiny <- function(pathlist,filelist){
  finalOut <- list() #vector("list", length(filelist)) # preallocate list length to speed processing
  
  # Remove tracking information from parsing exercise
  # filelist <- subset(filelist,!str_detect(Name,'TRACKING'))
  
  for(i in 1:length(filelist)){
    # fileName <- paste(path,filelist[i],sep='/')
    fileName <- filelist[i]
    filePath <- pathlist[i]
    
    print(fileName)

    
    # This step parses data and then organizes data in each file
    if(str_detect(fileName,'TRACKING',negate=TRUE)){
    finalOut[[fileName %>% 
                str_replace("[:alnum:]+\\_[:alpha:]+\\_[:alnum:]+\\_[:alnum:]\\_",'') %>%
                str_replace('.json*','') %>% 
                str_replace('.*/','') ]] <- eFormsParseJSON(filePath) %>%
      eFormsOrganize_byTable()  }
    }
  
    return(finalOut)
  }
    


karenWriteShiny <- function(filelist, finalList){
  # Create the first part of the filename for writing to a .csv file, based on visit info and sample type
#  subName.out <- str_extract(paste(path,filelist[1],sep='/'),"[:alnum:]+\\_[:alpha:]+\\_[:alnum:]+\\_[:alnum:]\\_")
  subName.out <- str_extract(filelist[1],"[:alnum:]+\\_[:alpha:]+\\_[:alnum:]+\\_[:alnum:]\\_")
  print(subName.out)
  #if( fileFormat == '.xlsx'){
    objLen <- map(finalList, length)
    specialCases <- names(objLen[objLen>2]) # deal with list objects with > 2 separately
    
    others <- finalList[!(names(finalList) %in% specialCases)]
    phab_channel <- finalList[specialCases] %>%
      map_df('channel') 
    phab_chanrip <- finalList[specialCases] %>%
      map_df('chanrip')
    phab_chanxsec <- finalList[specialCases] %>%
      map_df('chanxsec') 
    phab_littoral <- finalList[specialCases] %>%
      map_df('littoral')
    phab_thalweg <- finalList[specialCases] %>%
      map_df('thalweg') 
    phab <- list(PHAB_channel = phab_channel, PHAB_chanrip = phab_chanrip, PHAB_chanxsec = phab_chanxsec, PHAB_littoral = phab_littoral, PHAB_thalweg = phab_thalweg)
    meta <- list(Metadata = metadata)
    
    return(c(map(others,1),phab,meta))

}


