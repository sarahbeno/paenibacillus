# ggplot2 provides the plotting functions used in this script
library(ggplot2)
# dplyr provides the pipe (%>%), mutate(), filter(), and select() functions
library(dplyr)
# tidyr provides the gather() and spread() functions
library(tidyr)
# flexclust provides the qtclust() function
library(flexclust)

# (17 - 21) Read in data. Create "curve" factor with one level for each
# condition/day combination, e.g. BHI at 10°C, day 21. Perform
# log10-transformation on count data; since the qtclust algorithm can't account
# for below detection limit observations, we add one to the counts before the
# transform.
# (22 - 33) Normalize each curve to day 0
# (34 - 46) Average by isolate
read.csv("count_data.csv") %>%
  mutate(curve=paste(Media,paste(Temp,"C",sep=""),Day,sep="_"),
         logCount=log10(Count+1)) %>%
  select(Isolate.ID,Rep,logCount,curve) %>%
  spread(curve,logCount) %>%
  mutate(BHI_10C_21=BHI_10C_21 - BHI_10C_0,
         BHI_10C_14=BHI_10C_14 - BHI_10C_0,
         BHI_10C_0=0,
         BHI_6C_21=BHI_6C_21 - BHI_6C_0,
         BHI_6C_14=BHI_6C_14 - BHI_6C_0,
         BHI_6C_0=0,
         SMB_10C_21=SMB_10C_21 - SMB_10C_0,
         SMB_10C_14=SMB_10C_14 - SMB_10C_0,
         SMB_10C_0=0,
         SMB_6C_21=SMB_6C_21 - SMB_6C_0,
         SMB_6C_14=SMB_6C_14 - SMB_6C_0,
         SMB_6C_0=0) %>%
  group_by(Isolate.ID) %>%
  summarize(BHI_10C_0=mean(BHI_10C_0),
            BHI_10C_14=mean(BHI_10C_14),
            BHI_10C_21=mean(BHI_10C_21),
            BHI_6C_0=mean(BHI_6C_0),
            BHI_6C_14=mean(BHI_6C_14),
            BHI_6C_21=mean(BHI_6C_21),
            SMB_10C_0=mean(SMB_10C_0),
            SMB_10C_14=mean(SMB_10C_14),
            SMB_10C_21=mean(SMB_10C_21),
            SMB_6C_0=mean(SMB_6C_0),
            SMB_6C_14=mean(SMB_6C_14),
            SMB_6C_21=mean(SMB_6C_21)) -> data

# Cluster the isolates by their normalized day 14 and day 21 counts in SMB at
# 6°C, and add the clusters back to the dataframe
set.seed(200)
clust <- qtclust(x = data[,c("SMB_6C_14","SMB_6C_21")],radius = 1.5)
data$clusters <- predict(clust)

# Reshape data into long form for plotting
data %>%
  gather(conditions,logCount,BHI_10C_0:SMB_6C_21) %>%
  group_by(conditions) %>%
  mutate(Media=strsplit(conditions,"_")[[1]][1],
         Temp=as.numeric(
           substr(
             strsplit(conditions,"_")[[1]][2],
             1,
             nchar(strsplit(conditions,"_")[[1]][2])-1)),
         Day=as.numeric(strsplit(conditions,"_")[[1]][3])) -> long_data

# Plot each isolate's normalized growth curves, separated by cluster
long_data %>%
  ggplot(aes(x=Day,
             y=logCount,
             group=interaction(Isolate.ID),
             color=factor(clusters))) +
  theme_bw() +
  labs(x="Day", y="log CFU/mL", color="Cluster") +
  geom_line() +
  ggtitle("Radius 1.5 / Just SMB at 6C, averaged by Isolate") +
  scale_color_manual(values = hcl(h = seq(15,375, length = 5),
                                  l = 65, c = 100)[c(3,1,4,2)],
                     na.value="#BBBBBB") +
  facet_grid(clusters~Media+Temp)

# Plot red and green regions to represent notion of regions of no-growth and
# growth, respectively. Plot each isolate based on their normalized day 14 and
# day 21 log counts.
ggplot() +
  theme_bw() +
  geom_rect(aes(xmin=-1,xmax=Inf,ymin=1,ymax=Inf),fill="green",alpha=0.3) +
  geom_rect(aes(xmin=1,xmax=Inf,ymin=-1,ymax=1),fill="green",alpha=0.3) +
  geom_rect(aes(xmin=-1,xmax=-Inf,ymin=1,ymax=-Inf),fill="red",alpha=0.3) +
  geom_rect(aes(xmin=-1,xmax=1,ymin=-1,ymax=-Inf),fill="red",alpha=0.3) +
  geom_rect(aes(xmin=-1,xmax=1,ymin=-1,ymax=1),fill="black",alpha=0.2) +
  geom_point(data=data %>%
               mutate(clusters=ifelse(is.na(clusters), "Not clustered", clusters)),
             aes(x = SMB_6C_14,
                 y=SMB_6C_21,
                 shape=factor(clusters)),
             size=3) +
  labs(title="D21 vs. D14 w/ clusters",x="D14",y="D21",shape="Cluster")