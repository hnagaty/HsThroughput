library(dplyr)
library(ggplot2)
library(tidyr)

threshold <- 0.01

HsThresholds <- allDataGr %>%
  filter(HOUR_ID==0) %>%
  group_by(Cluster,Type) %>%
  summarise(HsCellThpCutoff=quantile(HsCellThp,probs=threshold,names=FALSE,na.rm=TRUE),
            MeanHsCellThp=mean(HsCellThp))
EulThresholds <- allDataGr %>%
  filter(HOUR_ID==0) %>%
  group_by(Cluster,Type) %>%
  summarise(EulThpCutoff=quantile(eulThp,probs=threshold,names=FALSE,na.rm=TRUE),
            MeanEulThp=mean(eulThp,na.rm=TRUE))


worstCells <- allDataGr %>%
  filter(HOUR_ID==0) %>%
  select(UCellName,Type,Cluster,
         HsCellThp,eulThp,
         Eul2msShare,UlThrTti10ms,UlThrTti2ms) %>%
  inner_join(HsThresholds,by=c("Type","Cluster")) %>%
  inner_join(EulThresholds,by=c("Type","Cluster")) %>%
  select(-Cluster,-Type) %>%
  filter(HsCellThp<HsCellThpCutoff & eulThp<EulThpCutoff) %>%
  rowwise %>%
  mutate(siteData=getPhySite(UCellName)) %>%
  separate(siteData,into=c("Site","Type","Sector","Carrier"),sep=",") %>%
  inner_join(sitesDf,by="Site") %>%
  select(Site,UCellName,Sector,Carrier,Type,Vendor,Cluster,
         SubRegion,GovernorateEn,SheakhaEn,
         HsCellThp,HsCellThpCutoff,MeanHsCellThp,
         eulThp,Eul2msShare,UlThrTti10ms,UlThrTti2ms,EulThpCutoff,MeanEulThp) %>%
  arrange(SubRegion,Site,Sector,Carrier)

write_csv(worstCells,"worstCells.csv")


ggplot(Thresholds,aes(x=Cluster,y=HsCellThpQ01,fill=Type,alpha=Cnt)) +
  geom_col(position="dodge") +
  geom_text(aes(label=Cnt))

