source("~/swissinfo/_helpers/helpers.R")
library(slopegraph)
library(swiTheme)


############################################################################################
###		SETTINGS
############################################################################################

infile <- read.csv("GENDER_EMP_Data_3c63b2eb-27b1-4012-bd9a-0a24a42be58f.csv", stringsAsFactors = F)

countries.sub <- c('United States', 'OECD - Average', 'Switzerland', 'United Kingdom', 'Japan', 'France',
	'Germany', 'Italy', 'Sweden', 'Spain', 'Portugal', 'France')

data <- infile[infile$Country %in% countries.sub,]
data <- data[,c('Country', 'Time', 'Value')]

data2 <- dplyr::summarise(group_by(data, Country), start = Value[1], end = Value[length(Value)])

data3 <- as.data.frame(data2[,2:3])
rownames(data3) <- unlist(data2[,1])


slopegraph(data3, rescaleByColumn = F)

data3 <- round(data3, 1)
colnames(data3) <- c("2000", "2012")

pdf("genderGap_slopgraph.pdf", family = font, width = 10, height = 13)
slopegraph(data3, rescaleByColumn = F, col.line = swi_rpal, col.lab = swi_rpal, , cex.lab = 1, cex.num = 0.9, offset.x = 0.05, lab.sep = 0.2)
dev.off()