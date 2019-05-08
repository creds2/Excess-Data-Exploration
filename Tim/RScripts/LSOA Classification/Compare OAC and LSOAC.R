library(data.table)
library(ggplot2)

lsoac<-fread("data-prepared/LSOAC_2011.csv")
census <- readRDS("data-prepared/census_lsoa.Rds")
seg<-fread( "N:\\MOT.R\\Processed Data\\DfT Segmentation\\output-area-dft-segmentation-dataset.csv")
clusters<-fread( "N:\\MOT.R\\Processed Data\\MOT Clusters\\MOTclustersMSOA240217.csv")
oac<-fread("N:\\MOT.R\\AP2016\\data\\oac_lsoa.csv")

o2l<-merge(lsoac,oac,by="lsoa",all.y=TRUE)
#Small g = lsoa classification big G = OAC classification (estimated by Mode)

supgroupmatch<-o2l[,.N,by=.(supergroupName,upergroupName)]

groupmatch[,.N,by=.(groupName,GroupName)]
supgroups<-lsoac[order(supergroupCode),.N,by=.(supergroupCode,supergroupName)]
groups<-lsoac[order(groupCode),.N,by=.(groupCode,groupName)]

#Go through and do plot one by one for groups

pn<-1
for (g in supgroups$supergroupName){
 #g<-"Cosmopolitan student neighbourhoods" 
  data<-supgroupmatch[supergroupName==g,][order(-N)]
p<-ggplot(data,aes(SupergroupName,N))+geom_bar(stat="identity")+coord_flip()+
  labs(title=g,x="OAC")
#+  scale_x_discrete(limits = rev(levels(SupergroupName)))

assign(paste0("p",pn),p)  
pn=pn+1  
}

multiplot(p1,p2,p3,p4,p5,p6,p7,p8,cols=2)




##############################################################
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
