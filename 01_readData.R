library(readr)
library(dplyr)

dataDir <- "D:/Optimisation/~InProgress/201809_HsThroughput/Queries/"

# read the RBS tables
RbsTables <- c("Hs","HsPDF","Eul","Pwr","PDelay")
#RbsTables <- c("Hs","Eul","PDelay","Pwr")
RbsTypes <- c("DUW","G2")
for (r in RbsTables) {
  for (t in RbsTypes) {
    fileName <- paste0(dataDir,r,"_",t,".txt")
    dfName <- paste0(r,".",t)
    rbsDf <- read_tsv(fileName,
                      col_types = cols(DateID = col_date(format = "%Y-%m-%d"),
                                       HOUR_ID = col_factor(levels = c(0,3,21)),
                                       BbType = col_factor(levels = c("G1","G2")),
                                       RBS = col_character(),
                                       Sector = col_factor(levels = c("1","2", "3", "4", "5", "6","7","8","9","10","11")),
                                       Carrier = col_factor(levels = c("1","2", "3")),
                                       RNC=col_character(),
                                       AverageRssi=col_double(),
                                       HsUsersPdf=col_double(),
                                       UsedNonHsPower_Avg=col_double(),
                                       HsDataRlim=col_double(),
                                       HsThpRlim=col_double()),
                      na = c("empty","NA","#DIV/0","null","","#SYNTAX"),
                      trim_ws = TRUE)
    assign(dfName,rbsDf)
    rm(rbsDf)
  }
  assign(r,union(get(paste0(r,".DUW")),get(paste0(r,".G2"))))
}

rm(dfName,fileName,r,t)
rm(Hs.G2,Hs.DUW,HsPDF.DUW,HsPDF.G2,Pwr.DUW,Pwr.G2,Eul.DUW,Eul.G2,PDelay.DUW,PDelay.G2)



# read the UtranCell tables
r <- "UCell"
UCellHours <- c("H00","H03","H21")
for (t in UCellHours) {
  fileName <- paste0(dataDir,r,"_",t,".txt")
  dfName <- paste0(r,".",t)
  uCellDf <- read_tsv(fileName,
              col_types = cols(DateID = col_date(format = "%Y-%m-%d"),
                               HOUR_ID = col_factor(levels = c(0,3,21)),
                               UCellName = col_character(),
                               RNC = col_character(),
                               Availability = col_double(),
                               SpTotTraffic = col_double(),
                               SpBestTraffic = col_double(),
                               SpRadCssr = col_double(),
                               SpRabDclr = col_double(),
                               DataVolDlHs = col_double(),
                               DataVolUlEul = col_double(),
                               HsRabDclr = col_double(),
                               HsRadCssr = col_double(),
                               NoCmUsers = col_double(),
                               NoEulRab = col_double(),
                               NoFacchRabs = col_double(),
                               NoHsRab = col_double(),
                               NoTotalSrb = col_double(),
                               pmTotNoRrcConnectReq = col_integer(),
                               pmTotNoRrcConnectReqSuccess = col_integer(),
                               pmTotNoRrcConnectReqcs = col_integer(),
                               pmTotNoRrcconnectReqcssucc = col_integer(),
                               pmTotNoRrcConnectReqPs = col_integer(),
                               pmTotNoRrcConnectReqPsSucc = col_integer(),
                               Rtwp = col_double(),
                               SohoOverhead = col_double()),
                    na = c("empty","NA","#DIV/0","null","","#SYNTAX"),
                    trim_ws = TRUE)
  assign(dfName,uCellDf)
  rm(uCellDf)
}
UCell <- union(UCell.H00,UCell.H03)
UCell <- union(UCell,UCell.H21)
rm(UCell.H00,UCell.H03,UCell.H21)
rm(dfName,fileName,r,t)


# Read in the UCell mapping
uCellMap <- read_tsv(paste0(dataDir,"UCellMapping.txt"),
                col_types = cols(
                  UCellName = col_character(),
                  RBS = col_character(),
                  Sector = col_factor(levels = c("1","2", "3", "4", "5", "6","7","8","9","10","11")),
                  Carrier = col_factor(levels = c("1","2", "3")),
                  RNC = col_character(),
                  Status = col_factor(NULL)
                ))

# Read the sites data
sitesDf <- read_csv(paste0(dataDir,"../vodaSites.txt"),
              col_types = cols(
                Site = col_character(),
                Vendor = col_factor(NULL),
                noCells = col_integer(),
                noSecs = col_integer(),
                Clutter = col_factor(NULL),
                Region = col_factor(NULL),
                SubRegion = col_factor(NULL),
                SheakhaAr = col_factor(NULL),
                SheakhaEn = col_factor(NULL),
                QismEn = col_factor(NULL),
                GovernorateEn = col_factor(NULL),
                Long = col_skip(),
                Lat = col_skip(),
                minDist = col_double(),
                maxDist = col_skip(),
                meanDist = col_double(),
                sdDist = col_double(),
                normSd = col_skip(),
                Cluster = col_factor(NULL)
              ))
