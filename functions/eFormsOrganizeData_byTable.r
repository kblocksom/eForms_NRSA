# eFormsOrganizeData_byTable.r
# Purpose: For each type of data, organize into data frames
# First figure out the type of data by sample type
#
# Created 2/27/2019 by Karen Blocksom
###############################################################

eFormsOrganize_byTable <- function(rawData){
  # Extract visit info
  visitinfo <- as.data.frame(rawData[1:7],stringsAsFactors=F)
  # Extract sample type from 8th element in each file
  sampletype <- names(rawData)[8]
  # PHAB sample types are special
  sampletype <- ifelse(substring(sampletype,1,5) %in% c('PHABW','PHABB'),substring(sampletype,1,5),
                       sampletype)
   
  # Create data frame of parsed data to start with, making them all character variables 
  parsedData <- as.data.frame(rawData[8])
  parsedData[,names(parsedData)] <- lapply(parsedData[,names(parsedData)], as.character)
  
  # run parsed data through organizing function, based on sample type 
  switch(sampletype,
    FISH = {rr <- organizeFish(parsedData)},
    FISHGEAR = {rr <- organizeFishGear(parsedData)},
    BENTHIC = {rr = organizeBenthic(parsedData)},
    VERIFICATION = {rr = organizeVerification(parsedData)},
    FIELD = {rr = organizeField(parsedData)},
    SAMPLES = {rr = organizeSamples(parsedData)},
    PHABW = {rr = organizePhab_W(parsedData)},
    PHABB = {rr = organizePhab_B(parsedData)},
    ASSESSMENT = {rr = organizeAssessment(parsedData)},
    CONSTRAINT = {rr = organizeConstraint(parsedData)},
    DISCHARGE = {rr = organizeDischarge(parsedData)},
    SLOPE = {rr = organizeSlope(parsedData)},
    TORRENT = {rr = organizeTorrent(parsedData)}
  )
  
  # PHAB sample types create lists of data frames
  if(sampletype %in% c('PHABW','PHABB')){
    ss <- rr
    # Go through each data frame in this list and bind with visit info
    for(k in 1:length(rr)){
      if(nrow(ss[[k]])>0){
        ss[[k]] <- cbind(visitinfo, ss[[k]])
      }
    }
  }else{ 
    # For other sample types, bind with visit info and create list 
    # in process for later use
    ss <- list(cbind(visitinfo, rr))
  }
  
  # Add new object to list with sample type name
  ss[["SAMPLE_TYPE"]] <- sampletype
  return(ss)
}

#############################################################################################################
# This begins the section which organizes the parsed data by sample type
organizeFish <- function(parsedIn){
  # Start by separating data that describe samples and those that describe species
  # aa pulls out sample information by SAMPLE_TYPE and sets LINE=0
  aa <- subset(parsedIn, select=str_detect(names(parsedIn), 'FISH\\.[:alpha:]')) %>%
    mutate(PAGE='1', LINE='0') %>%
    melt(id.vars=c('PAGE','LINE'), variable.name='PARAMETER', value.name='RESULT') %>%
    mutate(SAMPLE_TYPE=substring(PARAMETER,6,9), 
           PARAMETER=gsub('FISH\\.VERT\\_|FISH\\.FTIS\\_|FISH\\.FPLG\\_|FISH\\.FISH_', '', PARAMETER)) %>%
    select(SAMPLE_TYPE, PAGE, LINE, PARAMETER, RESULT)
  # bb pulls out and formats species by line number and sample type
  bb <- subset(parsedIn, select=str_detect(names(parsedIn), 'FISH\\.[:digit:]')) %>%
    mutate(PAGE='1') %>%
    melt(id.vars='PAGE',value.name='RESULT') %>%
    mutate(variable = gsub('FISH\\.','',variable),
           LINE=str_extract(variable, '[:digit:]+'),
           SAMPLE_TYPE=ifelse(str_detect(variable,'FTIS'),'FTIS',
                              ifelse(str_detect(variable,'FPLG'),'FPLG','FISH'))) %>%
    mutate(PARAMETER=ifelse(SAMPLE_TYPE %in% c('FTIS','FPLG'),str_replace(variable,'[:digit:]+\\_FTIS\\_|[:digit:]+\\_FPLG\\_', ''),
                            str_replace(variable, '[:digit:]+\\_', ''))) %>%
    select(SAMPLE_TYPE, PAGE, LINE, PARAMETER, RESULT)
  # stack aa and bb on top of one another
  cc <- rbind(aa, bb) 
  
  return(cc)
}

organizeFishGear <- function(parsedIn){
  # Simply melt these data and clean up parameters
  aa <- mutate(parsedIn, SAMPLE_TYPE='FISH') %>%
    melt(id.vars=c('SAMPLE_TYPE'), value.name='RESULT') %>%
    mutate(PARAMETER=gsub('FISHGEAR\\.', '', variable)) %>%
    select(SAMPLE_TYPE, PARAMETER, RESULT)
  
  return(aa)
}

organizeBenthic <- function(parsedIn){
  # Pull out sample information for each sample type and set TRANSECT='ALL'
  aa <- subset(parsedIn, select=str_detect(names(parsedIn), 'BENTHIC\\.BERW|BENTHIC\\.BETB|BENTHIC\\.BENTHIC')) %>%
    mutate(TRANSECT='ALL') %>%
    melt(id.vars=c('TRANSECT'), variable.name='PARAMETER', value.name='RESULT') %>%
    mutate(SAMPLE_TYPE=substring(PARAMETER,9,12), 
           PARAMETER=gsub('BENTHIC\\.BETB\\_|BENTHIC\\.BERW\\_|BENTHIC\\.BENTHIC\\_', '', PARAMETER)) %>%
    select(SAMPLE_TYPE, TRANSECT, PARAMETER, RESULT)
  # Pull out info on substrate for each transect subsample, extract TRANSECT from variable name
  bb <- subset(parsedIn, select=str_detect(names(parsedIn), 'BENTHIC\\.[:alpha:]\\_')) %>%
    mutate(SAMPLE_TYPE='BENTHIC') %>%
    melt(id.vars='SAMPLE_TYPE',value.name='RESULT') %>%
    mutate(variable = gsub('BENTHIC\\.','',variable),
           TRANSECT=substring(variable, 1, 1),
           SAMPLE_TYPE=substring(variable,3,6)) %>%
    mutate(PARAMETER=substring(variable,8,nchar(variable))) %>%
    select(SAMPLE_TYPE, TRANSECT, PARAMETER, RESULT)
  # Stack aa and bb data frames
  cc <- rbind(aa, bb) 
  
  return(cc)
}

organizeVerification <- function(parsedIn){
# Simply melt these data and clean up parameter names
  aa <- mutate(parsedIn, SAMPLE_TYPE='VERIF') %>%
    melt(id.vars=c('SAMPLE_TYPE'), variable.name='PARAMETER', value.name='RESULT') %>%
    mutate(PARAMETER=gsub('VERIFICATION\\.', '', PARAMETER)) %>%
    select(SAMPLE_TYPE, PARAMETER, RESULT)
  
  return(aa)
}

organizeField <- function(parsedIn){
  # Simply melt data, use parameters to assign sample type, and clean up parameter names
  aa <- mutate(parsedIn, SAMPLE_TYPE='FIELDMEAS') %>%
    melt(id.vars=c('SAMPLE_TYPE'), variable.name='PARAMETER', value.name='RESULT') %>%
    mutate(PARAMETER=gsub('FIELD\\.', '', PARAMETER)) %>%
    mutate(SAMPLE_TYPE=ifelse(PARAMETER %in% c('DO','CONDUCTIVITY','TEMPERATURE','PH','CORRECTED','TIME','LOCATION','OTH_LOCATION','FIELD_MEASUREMENT_REVIEW')|str_detect(PARAMETER,'MEASUREMENT'),'FIELDMEAS','CALIB')) %>%
    select(SAMPLE_TYPE, PARAMETER, RESULT) 
  
  return(aa)
}

organizeSamples <- function(parsedIn){
  # Simply melt these data by SAMPLE_TYPE and clean up parameter names
  aa <- mutate(parsedIn, SAMPLE_TYPE='SAMPLES') %>%
    melt(id.vars=c('SAMPLE_TYPE'), value.name='RESULT') %>%
    mutate(SAMPLE_TYPE=substring(as.character(variable),9,12), 
           PARAMETER=substring(as.character(variable),14,nchar(as.character(variable)))) %>%
    select(SAMPLE_TYPE, PARAMETER, RESULT)
  
  return(aa)
}


organizePhab_W <- function(parsedIn){
  # This breaks into numerous parts, depending on whether boatable or wadeable
  # Cross-section data
  parsedIn.long <- mutate(parsedIn, PROTOCOL='W') %>%
    melt(id.vars='PROTOCOL',value.name='RESULT') %>%
    mutate(variable=as.character(variable), TRANSECT=substring(variable,7,7),
           variable.1=str_replace(variable,'PHABW\\_[:alpha:]\\.','')) 
  # from tblCHANCROSSEC
  xc <- filter(parsedIn.long, str_detect(variable.1,'CROSSSEC_COMMENT')|(variable.1 %in% c("LF_DIST_LB","LC_DIST_LB","CT_DIST_LB",
                                                "RC_DIST_LB","RT_DIST_LB","LF_DEPTH",
                                                "LC_DEPTH","CT_DEPTH","RC_DEPTH","RT_DEPTH",
                                                "LF_SIZE_CLS","LC_SIZE_CLS","CT_SIZE_CLS",
                                                "RC_SIZE_CLS","RT_SIZE_CLS","LC_EMBED","RT_EMBED"))) %>%
    mutate(SAMPLE_TYPE='CROSSSECW',TRANSDIR=substring(variable.1,1,2),
           PARAMETER=substring(variable.1,4,nchar(variable.1))) %>%
    select(SAMPLE_TYPE,PARAMETER,TRANSECT,TRANSDIR,RESULT)
  
 # from tblCHANNEL 
  # fish cover
  fishc <- filter(parsedIn.long, str_detect(variable.1,("ALGAE|MACPHY|WOODY|BRUSH|LVTREE|OVRHNG|UNDCUT|BOULDR|STRUCT"))) %>%
    mutate(SAMPLE_TYPE='FISHCOVW',PARAMETER=variable.1) %>%
    select(SAMPLE_TYPE,PARAMETER,TRANSECT,RESULT)
  
  # bank measurements
  bank <- filter(parsedIn.long, str_detect(variable.1,"WETWID|BARWID|BANKWID|BANKHT|INCISED") & variable.1 %nin% c('0_WETWIDTH','5_WETWIDTH','7_WETWIDTH','0_BARWIDTH','5_BARWIDTH','7_BARWIDTH')) %>%
    mutate(SAMPLE_TYPE='BANKW',PARAMETER=variable.1) %>%
    select(SAMPLE_TYPE,PARAMETER,TRANSECT,RESULT)
  
  
  # from tblCHANRIP
  # bank angle
  angle <- filter(parsedIn.long, str_detect(variable.1,"ANGLE_UNDERCUT_COMMENT")|(variable.1 %in% c("LF_ANGLE","RT_ANGLE","LF_UNDERCUT","RT_UNDERCUT"))) %>%
    mutate(SAMPLE_TYPE='BANKW',BANK=substring(variable.1,1,2),
           PARAMETER=substring(variable.1,4,nchar(variable.1))) %>%
    select(SAMPLE_TYPE,PARAMETER,TRANSECT,BANK,RESULT)
  # canopy cover
  canopy <- filter(parsedIn.long, str_detect(variable.1,'DENSIOM')) %>%
    mutate(SAMPLE_TYPE='CANCOVERW',BANK=substring(variable.1,1,2),
           PARAMETER=substring(variable.1,4,nchar(variable.1))) %>%
    select(SAMPLE_TYPE,PARAMETER,TRANSECT,BANK,RESULT)
  
  # visual riparian
  visrip <- filter(parsedIn.long, str_detect(variable.1,"CANVEG|CANBTRE|CANSTRE|UNDERVEG|UNDWDY|UNDNWDY|GCWDY|GCNWDY|BARE")) %>%
    mutate(SAMPLE_TYPE='VISRIPW',BANK=substring(variable.1,1,2),
           PARAMETER=substring(variable.1,4,nchar(variable.1))) %>%
    select(SAMPLE_TYPE,PARAMETER,TRANSECT,BANK,RESULT)
  # human influence
  human <- filter(parsedIn.long, str_detect(variable.1,"WALL|BUILD|PAVE|ROAD|PIPES|TRASH|PARK|ROW|PAST|LOG|MINE")) %>%
    mutate(SAMPLE_TYPE='HUMINFLUW',BANK=substring(variable.1,1,2),
           PARAMETER=substring(variable.1,4,nchar(variable.1))) %>%
    select(SAMPLE_TYPE,PARAMETER,TRANSECT,BANK,RESULT)
  
  # tblTHALWEG
  thalweg <- filter(parsedIn.long, str_detect(variable.1,'THALWEG_COMMENT|WETWIDTH|SEDIMENT|CHANUNCD|INCREMENT|REACHLENGTH|BAR_PRES|BACKWATER|BARWIDTH|SIDCHN')|(str_detect(variable.1,"[:digit:]+\\_DEPTH"))) %>%
    mutate(SAMPLE_TYPE='THALW',
           STATION=ifelse(variable.1 %in% c('INCREMENT','REACHLENGTH'),'ALL',
                                              str_extract(variable.1,"[:digit:]+"))) %>%
    mutate(PARAMETER=str_replace(variable.1,"[:digit:]+\\_|[:digit:]+\\.",'')) %>%
    select(SAMPLE_TYPE,PARAMETER,TRANSECT,STATION,RESULT)
  
  # tblCHANCROSSSEC
  # Extra substrate measurements
  substrate <- filter(parsedIn.long, str_detect(variable.1,'XSIZE_CLS|SUBSTRATE_COMMENT')) %>%
    mutate(SAMPLE_TYPE='CROSSSECW',TRANSDIR=ifelse(variable.1=='SUBSTRATE_COMMENT','ALL',substring(variable.1,1,2)),
           PARAMETER=ifelse(variable.1=='SUBSTRATE_COMMENT',variable.1,substring(variable.1,4,nchar(variable.1)))) %>%
    select(SAMPLE_TYPE,PARAMETER,TRANSECT,TRANSDIR,RESULT)
  
  # tblCHANNEL
  # Large woody debris
  lwd <- filter(parsedIn.long, variable.1 %in% c("LWD_COMMENT","WSDSL","WSDML","WSDLL",
                                                 "DSDSL","DSDML","DSDLL","WMDSL","WMDML",
                                                 "WMDLL","DMDSL","DMDML","DMDLL","WLDSL",
                                                 "WLDML","WLDLL","DLDSL","DLDML","DLDLL",
                                                 "WXDSL","WXDML","WXDLL","DXDSL","DXDML",
                                                 "DXDLL")) %>%
    mutate(SAMPLE_TYPE='LWDW',PARAMETER=variable.1) %>%
    select(SAMPLE_TYPE,PARAMETER,TRANSECT,RESULT)
  
  # stack various data types into like data frames
  chanxsec <- rbind(xc, substrate) 
  channel <- rbind(fishc, bank, lwd)
  chanrip <- rbind(angle, canopy, visrip, human)
  # Create a list object with 4 data frames in it
  outdf <- list(chanxsec, channel, chanrip, thalweg)
  # Assign names to each object (data frame) in list
  names(outdf) <- c('chanxsec','channel','chanrip','thalweg') 
  
  return(outdf)
}

organizeAssessment <- function(parsedIn){
  
  # Simply melt data and clean up parameter names
  aa <- mutate(parsedIn, SAMPLE_TYPE='ASSESS') %>%
    melt(id.vars=c('SAMPLE_TYPE'), variable.name='PARAMETER', value.name='RESULT') %>%
    mutate(PARAMETER=gsub('ASSESSMENT\\.', '', PARAMETER)) %>%
    select(SAMPLE_TYPE, PARAMETER, RESULT)
  
  return(aa)
  
}

organizeConstraint <- function(parsedIn){
  # Simply melt data and clean up parameter names
  aa <- mutate(parsedIn, SAMPLE_TYPE='CONSTRAINT') %>%
    melt(id.vars=c('SAMPLE_TYPE'), variable.name='PARAMETER', value.name='RESULT') %>%
    mutate(PARAMETER=gsub('CONSTRAINT\\.', '', PARAMETER)) %>%
    select(SAMPLE_TYPE, PARAMETER, RESULT)
  
  return(aa)
  
}

organizeDischarge <- function(parsedIn){
  # Melt data, extract REP, and clean up parameters names - does not identify method used because database does not either
  aa <- mutate(parsedIn, SAMPLE_TYPE='FLOW') %>%
    melt(id.vars=c('SAMPLE_TYPE'), value.name='RESULT') %>%
    mutate(REP=ifelse(str_detect(variable, '[:digit:]'), str_extract(variable, "[:digit:]+"), '0'),
                      variable.1=str_replace(variable, 'DISCHARGE\\.', '')) %>%
    mutate(PARAMETER=str_replace(variable.1, "[:digit:]+\\_",'')) %>%
    select(SAMPLE_TYPE, PARAMETER, REP, RESULT)
  
  return(aa)
  
}

organizeSlope <- function(parsedIn){
  # Melt data, extract REP and TRANSECT, and clean up parameter names
  aa <- mutate(parsedIn, SAMPLE_TYPE='SLOPEW') %>%
    melt(id.vars=c('SAMPLE_TYPE'), value.name='RESULT') %>%
    mutate(REP=ifelse(str_detect(variable, '[:digit:]'), str_extract(variable, "[:digit:]+"), '0'),
           TRANSECT=ifelse(str_detect(variable,'SLOPE_REVIEW'), 'ALL', substring(variable,7,7)),
           variable.1=str_replace(variable, 'SLOPE\\.', '')) %>%
    mutate(PARAMETER=str_replace(variable.1, "[:alpha:]\\_[:digit:]+\\_",'')) %>%
    select(SAMPLE_TYPE, PARAMETER, TRANSECT, REP, RESULT)
  
  return(aa)
  
}

organizeTorrent <- function(parsedIn){
  # Melt data and clean up parameter names
  aa <- mutate(parsedIn, SAMPLE_TYPE='TORR') %>%
    melt(id.vars=c('SAMPLE_TYPE'), value.name='RESULT') %>%
    mutate(PARAMETER=str_replace(variable, 'TORRENT\\.', '')) %>%
    select(SAMPLE_TYPE, PARAMETER, RESULT)
  
  return(aa)
  
}

organizePhab_B <- function(parsedIn){
  # This breaks into numerous parts, depending on whether boatable or wadeable
  # Cross-section data
  parsedIn.long <- mutate(parsedIn, PROTOCOL='B') %>%
    melt(id.vars='PROTOCOL',value.name='RESULT') %>%
    mutate(variable=as.character(variable), TRANSECT=substring(variable,7,7),
           variable.1=str_replace(variable,'PHABB\\_[:alpha:]\\.','')) 
  
  # from tblCHANNEL 
  # fish cover
  fishc <- filter(parsedIn.long, str_detect(variable.1,("ALGAE|MACPHY|WOODY|BRUSH|LVTREE|OVRHNG|UNDCUT|BOULDR|STRUCT"))) %>%
    mutate(SAMPLE_TYPE='FISHCOVB',PARAMETER=variable.1) %>%
    select(SAMPLE_TYPE,PARAMETER,TRANSECT,RESULT)
  
  # bank measurements
  bank <- filter(parsedIn.long, str_detect(variable.1,"WETWID|BARWID|BANKWID|BANKHT|INCISED|CHOSENBANK|ACTRANSP|BKANGLE|INTDTRAN")) %>%
    mutate(SAMPLE_TYPE='BANKB',PARAMETER=variable.1) %>%
    select(SAMPLE_TYPE,PARAMETER,TRANSECT,RESULT)
  
  
  # from tblCHANRIP
  # canopy cover
  canopy <- filter(parsedIn.long, str_detect(variable.1,'DENSIOM')) %>%
    mutate(SAMPLE_TYPE='CANCOVERB',BANK=substring(variable.1,1,2),
           PARAMETER=substring(variable.1,4,nchar(variable.1))) %>%
    select(SAMPLE_TYPE,PARAMETER,TRANSECT,BANK,RESULT)
  
  # visual riparian
  visrip <- filter(parsedIn.long, str_detect(variable.1,"CANVEG|CANBTRE|CANSTRE|UNDERVEG|UNDWDY|UNDNWDY|GCWDY|GCNWDY|BARE")) %>%
    mutate(SAMPLE_TYPE='VISRIPB',BANK=substring(variable.1,1,2),
           PARAMETER=substring(variable.1,4,nchar(variable.1))) %>%
    select(SAMPLE_TYPE,PARAMETER,TRANSECT,BANK,RESULT)
  # human influence
  human <- filter(parsedIn.long, str_detect(variable.1,"WALL|BUILD|PAVE|ROAD|PIPES|TRASH|PARK|ROW|PAST|LOG|MINE")) %>%
    mutate(SAMPLE_TYPE='HUMINFLUB',BANK=substring(variable.1,1,2),
           PARAMETER=substring(variable.1,4,nchar(variable.1))) %>%
    select(SAMPLE_TYPE,PARAMETER,TRANSECT,BANK,RESULT)
  # littoral
  littoral <- filter(parsedIn.long, str_detect(variable.1,"BOTTOM|SHORE|SUBOBS")) %>%
    mutate(SAMPLE_TYPE='LITTORALB',
           PARAMETER=variable.1) %>%
    select(SAMPLE_TYPE,PARAMETER,TRANSECT,RESULT)
  
  # tblTHALWEG
  thalweg <- filter(parsedIn.long, (str_detect(variable.1,'THALB_COMMENT|SONAR|SNAG|SIZE_CLS|POLE|OFF_CHAN|DEPTH_UNITS|CHANUNCD')|(str_detect(variable.1,"[:digit:]+\\_DEPTH"))) & str_detect(variable.1,'\\_PB|CHANDEPTHB')==FALSE) %>%
    mutate(SAMPLE_TYPE='THALB',
           STATION=ifelse(variable.1 %in% c('INCREMENT','REACHLENGTH'),'ALL',
                          str_extract(variable.1,"[:digit:]+"))) %>%
    mutate(PARAMETER=str_replace(variable.1,"[:digit:]+\\_|[:digit:]+\\.",'')) %>%
    select(SAMPLE_TYPE,PARAMETER,TRANSECT,STATION,RESULT)
  
  # tblLITTORAL 
  littdepth <- filter(parsedIn.long, str_detect(variable.1, 'POLE_PB|SONAR_PB|DEPTH_PB|CHANDEPTHB_DEPTH_UNITS')) %>%
    mutate(SAMPLE_TYPE='CHANDEPTHB',LINE=str_extract(variable.1,"[:digit:]+")) %>%
    mutate(PARAMETER=str_replace(variable.1,"[:digit:]+\\_|[:digit:]+\\.",'')) %>%
    select(SAMPLE_TYPE,PARAMETER,TRANSECT,LINE,RESULT)
  
  # tblCHANNEL - constraint
  constraint <- filter(parsedIn.long, str_detect(variable.1,"CONSTRT|SEEOVRBK|SHOR2RIP")) %>%
    mutate(SAMPLE_TYPE='CONSTB',PARAMETER=variable.1) %>%
    select(SAMPLE_TYPE,PARAMETER,TRANSECT,RESULT)
  
  # tblCHANNEL
  # Large woody debris
  lwd <- filter(parsedIn.long, variable.1 %in% c("LWD_COMMENT","WSDSL","WSDML","WSDLL",
                                                 "DSDSL","DSDML","DSDLL","WMDSL","WMDML",
                                                 "WMDLL","DMDSL","DMDML","DMDLL","WLDSL",
                                                 "WLDML","WLDLL","DLDSL","DLDML","DLDLL",
                                                 "WXDSL","WXDML","WXDLL","DXDSL","DXDML",
                                                 "DXDLL")) %>%
    mutate(SAMPLE_TYPE='LWDB',PARAMETER=variable.1) %>%
    select(SAMPLE_TYPE,PARAMETER,TRANSECT,RESULT)
  
  # Combine data types by database table
  channel <- rbind(fishc, bank, lwd, constraint,littoral)
  chanrip <- rbind(canopy, visrip, human)
  # Create list of data frames
  outdf <- list(channel, chanrip, littdepth, thalweg)
  # Assign names to each object (data frame) in list
  names(outdf) <- c('channel','chanrip','littoral','thalweg') 
  
  return(outdf)
}