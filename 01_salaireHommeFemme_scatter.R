library("swiTheme")
library("swiRcharts")

############################################################################################
###		Get data and translations
############################################################################################

data <- read.csv("data/salaireHommeFemme_2014.csv", check.names = FALSE, stringsAsFactors = F)
trad <- read.csv("data/gender wage gap by economical sector - Sheet1.csv", row.names = 1, stringsAsFactors = F)

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
	dd$color <- swi_rpal[1:nrow(dd)]

	dd[,'nom court'] <- trad[names.short,lang]
	dd[,'Nom'] <- trad[names.long,lang]

	a <- rCharts::Highcharts$new()
	a$series(hSeries( x = dd[,'% du salaire hommes'], y = dd[,'Hommes'], z = dd[,'total job'],
		name = dd[,'nom court'], color = dd$color, series = dd$Nom))

	a$chart(zoomType = "xy", type = "bubble", height = 400)
	a$plotOptions(bubble = list(dataLabels = list(enabled = T, verticalAlign = "middle", style = list(textShadow = 'none', fontSize = "0.6em"),
		color = 'black', useHTML = T, formatter = "#! function() { return this.point.name; } !#"),
		minSize = 15, maxSize = 75))

	x.tooltip <- gsub("'", " ", trad['xlab',lang])
	y.tooltip <- gsub("'", " ", trad['ylab',lang])
	z.tooltip <- gsub("'", " ", trad['ztool',lang])

	formatter <- paste("#! function() {", "return '<strong>' + this.series.name + ",
		 "'</strong><div class=\"tooltip\" style=\"color:#686868;font-size:0.8em\"><br/>", y.tooltip, ": <b>' + this.y + '", "</b><br/>",  x.tooltip, ": <b>' + this.x + '",
		 "</b><br/>", z.tooltip, ": ' + this.point.z +","'</div>';",
		"} !#", sep = "")

	a$tooltip( useHTML = T, formatter = formatter)

	a$yAxis(title = list(text = trad['ylab',lang], style = list(fontWeight = "bold")), labels = list(format = '{value}'), floor = 3500, ceiling = 11000)
	a$xAxis(title = list(text = trad['xlab',lang], style = list(fontWeight = "bold")), labels = list(format = '{value}%'), min = 65, max = 100,
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
	 h3 = "", author = " | swissinfo.ch"
	)

}





