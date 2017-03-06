library("swiTheme")
library("swiRcharts")

############################################################################################
###		Get data and translations
############################################################################################

data <- read.csv("salaireHommeFemme_slim.csv", check.names = FALSE, stringsAsFactors = F)

trad <- read.csv("01_trad.csv", row.names = 1, stringsAsFactors = F)

rown.num <- suppressWarnings(as.numeric(rownames(trad)))
names.short <- 1: (which.max(rown.num) / 2)
names.long <- ((which.max(rown.num) / 2)+1): which.max(rown.num)

if(nrow(data) != length(names.short) || length(names.short) != length(names.long)) {
	stop("data is not the same length as translation file")
}

############################################################################################
###		highcharts bubble chart: %
############################################################################################

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
	a$plotOptions(bubble = list(dataLabels = list(enabled = T, zIndex = 1, verticalAlign = "middle", style = list(textShadow = 'none', fontSize = "0.6em"),
		color = 'black', useHTML = T, formatter = "#! function() { return this.point.name; } !#"),
		minSize = 15, maxSize = 75))

	#formatter <- "#! function() { return '<b>' + this.series.name + '</b><br/><br/>' + this.x + '%  -  ' + this.y + '%';} !#"

	x.tooltip <- gsub("'", " ", trad['xlab',lang])
	y.tooltip <- gsub("'", " ", trad['ylab',lang])
	z.tooltip <- gsub("'", " ", trad['ztool',lang])

	formatter <- paste("#! function() {", "return '<h5>' + this.series.name + ",
		 "'</h5><div class=\"tooltop\" style=\"color:#686868;font-size:0.8em\">", y.tooltip, ": <b>' + this.y + '", "</b><br/>",  x.tooltip, ": <b>' + this.x + '",
		 "</b><br/>", z.tooltip, ": ' + this.point.z +","'</div>';",
		"} !#", sep = "")

	a$tooltip(shadow = F, useHTML = T, backgroundColor = "rgba(255,255,255,1)", formatter = formatter)

	a$yAxis(title = list(text = trad['ylab',lang], style = list(fontWeight = "bold")), labels = list(format = '{value}'), floor = 3500, ceiling = 11000)
	a$xAxis(title = list(text = trad['xlab',lang], style = list(fontWeight = "bold")), labels = list(format = '{value}%'), min = 65, max = 100,
		lineColor = list ('#FF0000'))
	a$legend(enabled = F)
	#a

	hChart.html <- tempfile("hchart_labelledBubble.html")
	a$save(hChart.html)

	# Example of converting a highcharts-rCharts html chart into a responsive one

	hChart2responsiveHTML(hChart.html, output.html = output.html, h2 = trad['title',lang], descr = trad['descr',lang],
		source = trad['source',lang], h3 = "")

}





