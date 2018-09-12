library(dplyr)
library(tidyr)
library(readr)

# Function to deduce site properties from the cell name
getPhySite <- function(cellname) {
  if (substr(cellname,1,1)=="L") {
    s=strsplit(cellname,"_",fixed=TRUE)
    if (length(s[[1]])==3) {
      pType=s[[1]][1]
      if (pType=="L18") {
        Type="LTE1800"
      } else if (pType=="L21") {
        Type="LTE2100"
      } else {
        Type="Invalid"
      }
      pSite=s[[1]][2]
      if ((nchar(pSite)==5 | nchar(pSite)==6) & substr(pSite,1,1)=="L") {
        PhySite <- substring(pSite,2)
      } else {
        PhySite="Invalid"
      }
    }
    else {
      PhySite <- "Invalid"
      Type <- "Invalid"
    }
  } else {
    chrLoc <- regexpr("[0-9]",cellname)[1]
    chrPrefix <- substring(cellname,1,chrLoc-1)
    numSuffix <- substring(cellname,chrLoc)
    if (nchar(chrPrefix)>2 | nchar(numSuffix)!=5) {
      PhySite="Invalid"
      Type="Invalid"
    } else {
      prefix <- substr(chrPrefix,1,1)
      suffix <- substr(numSuffix,1,4)
      if (prefix=="C") {
        PhySite <- paste0(substring(chrPrefix,2),suffix)
        Type <- "DCS1800"
      } else if (prefix=="G") {
        PhySite <- paste0(substring(chrPrefix,2),suffix)
        Type <- "UMTS2100"
      } else if (prefix=="M") {
        PhySite <- paste0(substring(chrPrefix,2),suffix)
        Type <- "UMTS900"
      } else {
        if (nchar(chrPrefix)<2) {
          PhySite <- paste0(chrPrefix,suffix)
          Type <- "GSM900" 
        } else {
          PhySite <- "Invalid"
          Type <- "Invalid"
        }
      }
    }     
  }
  if (PhySite!="Invalid") {
    a <- substr(cellname,nchar(cellname),nchar(cellname))
    Sector <- as.character(cellMap[a,2])
    if (Type=="UMTS2100") {Carrier <- paste0("G",as.character(cellMap[a,3]))}
    else if (Type=="UMTS900") {Carrier <- paste0("M",as.character(cellMap[a,3]))}
    else {Carrier <- "NA"}
  } else {
    Sector="Invalid"
    Carrier="Invalid"
  }
  return(paste(PhySite,Type,Sector,Carrier,sep=","))
}


cellMap <- read.delim(paste0(dataDir,"sectorMapping.txt"))
cellMap$Carrier<-as.factor(cellMap$Carrier)
rownames(cellMap)<-cellMap$Suffix



RbsDfAll <- Hs %>%
  full_join(HsPDF) %>%
  full_join(Eul) %>%
  full_join(Pwr)

RbsDfAll <- inner_join(RbsDfAll,uCellMap) %>%
  select(-RNC,-Status)

allData <- inner_join(UCell,RbsDfAll) %>%
  select(-RBS,-Sector,-Carrier)

allDataGr <- allData %>%
  select(-DateID) %>%
  group_by(HOUR_ID,UCellName,BbType,RNC) %>%
  summarize_all(funs(mean(.,na.rm=TRUE)))

allDataGr <- allDataGr %>%
  select(-pmSumNonEmptyUserBuffersPqSpiAll,-pmSumAckedBitsPqSpiAll,-pmCapacityAllocAttHsDschUsers)

allDataGr <- allDataGr %>%
  rowwise %>%
  mutate(siteData=getPhySite(UCellName)) %>%
  separate(siteData,into=c("Site","Type","Sector","Carrier"),sep=",") %>%    
  filter(Site!="Invalid") %>%
  ungroup()

allDataGr <- inner_join(allDataGr,sitesDf,on="Site")
allDataGr <- filter(allDataGr,!is.na(HsCellThp))
allDataGr <- allDataGr %>%
  filter(Region=="Delta") %>%
  select(-Region,-SheakhaAr,-SheakhaEn,-QismEn,-Vendor)
allDataGr <- mutate(allDataGr,eulThp=UlThrTti10ms*(1-Eul2msShare)+UlThrTti2ms*Eul2msShare)

#allDataCom <-  allDataGr %>%
#  drop_na()

write_csv(allDataGr,"allData.csv")

# Remove all intermediate data
rm(Hs,HsPDF,Eul,Pwr,PDelay,RbsDfAll,UCell,allData,uCellMap)
