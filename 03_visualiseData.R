library(ggplot2)
library(dplyr)

allDataGr$noSecs <- as.factor(allDataGr$noSecs)
allDataGr$noCells <- as.factor(allDataGr$noCells)
allDataGr <- allDataGr %>%
  filter(UCellName != "G55216")

ggplot(allDataGr,aes(x=Cluster,y=HsCellThp)) + geom_boxplot()

summary(allDataGr$HsCellThp)

allDataGr %>% filter(Cluster != "Road",Carrier!="GNA") %>%
  ggplot(aes(x=HsCellThp,fill=Type)) +
    geom_histogram() +
    geom_rug() +
    facet_grid(HOUR_ID~Cluster) +
    scale_x_continuous(limits=c(0,4000)) +
    labs(title="HS Cell Throughput Histogram",
         subtitle="Per Hour & Site Class",
         x="HS Cell Throughput",y="No. of Cells",fill="Site Band")

allDataGr %>% filter(Cluster != "Road",Carrier!="GNA") %>%
  ggplot(aes(x=PwrUtilisation,y=HsCellThp,col=Cluster)) +
  facet_grid(HOUR_ID~Type) +
  stat_smooth(method=lm) +
  ggtitle("The Cell Throughput") +
  theme_light()

allDataGr %>% filter(Cluster != "Road",Carrier!="GNA") %>%
  ggplot(aes(x=HsCellThp,col=Cluster)) +
  #geom_histogram() +
  stat_ecdf() +
  geom_rug() +
  facet_grid(HOUR_ID~Type) +
  scale_x_continuous(limits=c(0,4000)) +
  labs(title="HS Cell Throughput CDF",
       x="HS Cell Throughput",y="% of Cells",fill="Site Band")
