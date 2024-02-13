#' @filter cors
cors <- function(req, res) {
  
  res$setHeader("Access-Control-Allow-Origin", "*")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods","*")
    res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200 
    return(list())
  } else {
    plumber::forward()
  }
  
}
#* @post /
#* @param datasetId
#* @serializer html
function(datasetId) {
	#library(png)
	library(ggplot2)
	library(reshape2)
  	datasetID <- unlist(strsplit(datasetId,","))
	#library(lipdR)
	#library(jsonlite)
	#numIds <- length(datasetID)
	queryTable <- readRDS("datasetQT.rds")
	df1 <- queryTable[queryTable$datasetId %in% datasetID,]
	
	min1 <- floor(min(df1$minAge,na.rm=TRUE)/100)*100
	max1 <- ceiling(max(df1$maxAge,na.rm=TRUE)/100)*100
	df2 <- data.frame(Age = seq(min1, max1, by=100))
	for (ii in 1:length(unique(df1$archiveType))){
	  df2 <- cbind.data.frame(df2,data.frame(rep(NA,length(seq(min1, max1, by=100)))))
	  names(df2)[(ii+1)] <- unique(df1$archiveType)[ii]
	}

	for (i in seq(min1, max1, by=100)){
	  tsArchives <- df1$archiveType[df1$minAge<i & df1$maxAge>i]
	  for (k in names(df2)[names(df2)!="Age"]){
	    df2[df2$Age==i,names(df2)==k] <- sum(tsArchives == k)
	  }
	  
	}

cutoff1 <- max(rowSums(df2[,-1]))*.05
df2 <- df2[rowSums(df2[,-1])>cutoff1,]

addNote <- ""
if(nrow(df2) < length(seq(min1, max1, by=100))){
  addNote <- paste0("Plot truncated\n Max age = ", max1)
}


df3 <- reshape2::melt(df2, id.vars="Age", value.name = "Count")

names(df3)[2] <- "archiveType"

p1 <- ggplot(data = df3, mapping = aes(x=Age, y=Count, fill=archiveType)) + geom_area() +
  annotate("text", x=max(df3$Age)*.75, y=max(df3$Count)*.5, label=addNote, size=1) +
  xlab("Age (yr BP)") +
  theme_classic() +
  theme(
    legend.title=element_text(size=4),
    legend.text=element_text(size=4),
    axis.title=element_text(size=4),
    axis.text=element_text(size=4),
    legend.key.size = unit(0.2,"line"),
    axis.ticks = element_line(size = 0.1),
    axis.line = element_line(size = 0.1)
  )

path1 <- tempfile()

ggsave(filename = paste0(path1,".png"),plot = p1,width = 800,height=300,device = "png",units = "px")

base64::img(paste0(path1,".png"))

}
