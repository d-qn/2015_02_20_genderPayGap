library("swiTheme")
library("swiRcharts")

############################################################################################
###		Get data and translations
############################################################################################

data <- read.csv("data/salaireHommeFemme_2014.csv", check.names = FALSE, stringsAsFactors = F)
trad <- read.csv("data/Gender pay gap - Sheet1.csv", row.names = 1, stringsAsFactors = F)

cidx <- unique(which(trad =="" | is.na(trad), T)[,2])
if(length(cidx > 0)) {
  warning(paste(colnames(trad)[cidx], collapse = "\t"), " languages will be discarded!", "\n")
  trad <- trad[,-cidx, drop = F]
}

rown.num <- suppressWarnings(as.numeric(rownames(trad)))
names.short <- 1: (which.max(rown.num) / 2)
names.long <- ((which.max(rown.num) / 2)+1): which.max(rown.num)

if(nrow(data) != length(names.short) || length(names.short) != length(names.long)) {
	stop("data is not the same length as translation file")
}

############################################################################################
###		highcharts bubble chart: %
############################################################################################

# keep only 1 digit of % median salary ratios
data[,'% du salaire hommes'] <- round(data[,'% du salaire hommes'], 1)

for (i in 1:ncol(trad)) {

	lang <- colnames(trad)[i]
	output.html <- paste("01_manWomanSalary_bubble_", lang, ".html", sep ="")

	dd <- data

	dd[,'nom court'] <- trad[names.short,lang]
	dd[,'Nom'] <- trad[names.long,lang]

	# define tooltip
	dd$name <- paste0(
	  '<table cellpadding="1" style="line-height:1.1"', ifelse (lang == 'ar', ' dir="rtl" align="right">', '>'),
	  '<div class=\"tooltip\" style=\"color:#686868;font-size:0.8em\">',
	  '<tr><td colspan="3"><strong>', dd$Nom, '</strong></td></tr>',
	  '<tr><td colspan="3">&nbsp;</td></tr>',
	  '<tr><td colspan="2">', trad['ylab',lang], '</td><td><strong>', dd$Hommes, '</strong></td></tr>',
	  '<tr><td colspan="2">', trad['wtool',lang], '</td><td><strong>', dd$Femmes, '</strong></td></tr>',
	  '<tr><td colspan="2">', trad['xlab',lang], '</td><td><strong>', dd[,'% du salaire hommes'], '</strong></td></tr>',
	  '<tr><td colspan="3">&nbsp;</td></tr>',
	  '<tr><td colspan="2">', trad['ztool',lang], '</td><td>', dd[,'total job'], '</td></tr>',
	  '<tr><td colspan="2">', trad['ftool',lang], '</td><td><strong>', dd[,'ratio femmes/hommes'], '%</strong></td></tr>',
	  '</div></table>'
	)

	a <- rCharts::Highcharts$new()
	h2 <- hSeries2(data.frame(x = dd[,'% du salaire hommes'], y = dd[,'Hommes'], z = dd[,'total job'],
	  name = dd$name, series = dd[,'nom court']), "series")
	a$series(h2)

	a$colors(swi_rpal[1:nrow(dd)])
	a$chart(zoomType = "xy", type = "bubble", height = 400)
	a$plotOptions(bubble = list(dataLabels = list(
	  enabled = T,
	  verticalAlign = "middle",
	  style = list(textShadow = 'none', fontSize = "0.6em"),
		color = 'black', useHTML = T, formatter = "#! function() { return this.series.name; } !#"),
		minSize = 20, maxSize = 84))

	a$tooltip(formatter = "#! function() { return this.point.name; } !#",
	          useHTML = T, borderWidth = 1, style = list(padding = 2))

	a$yAxis(title = list(text = trad['ylab',lang], style = list(fontWeight = "bold")), labels = list(format = '{value}'), floor = 3500, ceiling = 11000)
	a$xAxis(title = list(text = trad['xlab',lang], style = list(fontWeight = "bold")), labels = list(format = '{value}%'), min = 67, max = 100,
		lineColor = list ('#FF0000'))
	a$legend(enabled = F)
	#a

	hChart.html <- tempfile("hchart_labelledBubble.html")
	a$save(hChart.html)

	hChart2responsiveHTML(
	 hChart.html,
	 output.html = output.html,
	 h2 = trad['title',lang],
	 descr = trad['descr',lang],
	 source = paste0(trad['footnote', lang], " | ", trad['source',lang]),
	 h3 = "", author = " swissinfo.ch"
	)

}


