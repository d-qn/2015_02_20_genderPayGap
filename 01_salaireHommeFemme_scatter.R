library("swiTheme")
library("swiRcharts")

############################################################################################
###		Get data and translations
############################################################################################

data <- read.csv("data/salaireHommeFemme_slim.csv", check.names = FALSE, stringsAsFactors = F)

trad <- read.csv("01_trad.csv", row.names = 1, stringsAsFactors = F)

rown.num <- suppressWarnings(as.numeric(rownames(trad)))
names.short <- 1: (which.max(rown.num) / 2)
names.long <- ((which.max(rown.num) / 2)+1): which.max(rown.num)

############################################################################################
###		highcharts bubble chart: %
############################################################################################

for (i in 1:ncol(trad)) {

	lang <- colnames(trad)[i]
	output.html <- paste("01_manWomanSalary_bubble_", lang, ".html", sep ="")

	dd <- data
	dd$color <- swi_rpal[1:nrow(dd)]

	a <- rCharts::Highcharts$new()
	a$series(hSeries( x = dd[,'% du salaire hommes'], y = dd[,'Hommes'], z = dd[,'total job'],
		name = dd[,'nom court'], color = dd$color, series = dd$Nom))

	a$chart(zoomType = "xy", type = "bubble", height = 400)
	a$plotOptions(bubble = list(dataLabels = list(enabled = T, verticalAlign = "middle", style = list(textShadow = 'none', fontSize = "0.3em"),
		color = 'black', useHTML = T, formatter = "#! function() { return this.point.name; } !#"),
		minSize = 20, maxSize = 100))

	#formatter <- "#! function() { return '<b>' + this.series.name + '</b><br/><br/>' + this.x + '%  -  ' + this.y + '%';} !#"

	x.tooltip <- gsub("'", " ", trad['xlab',lang])
	y.tooltip <- gsub("'", " ", trad['ylab',lang])
	z.tooltip <- gsub("'", " ", trad['ztool',lang])

	formatter <- paste("#! function() {", "return '<b>' + this.series.name + ",
		 "'</b><div style=\"color:#686868;font-size:0.8em\"><br/>", y.tooltip, ": ' + this.y + '", "<br/>",  x.tooltip, ": ' + this.x + '",
		 "<br/>", z.tooltip, ": ' + this.point.z +","'</div>';",
		"} !#", sep = "")

		# formatter <- paste("#! function() {", "return '<b>' + this.series.name + ",
		# 	 "'</b><div style=\"color:#686868;font-size:0.8em\"><br/>", y.tooltip, ": ' + this.y + '", "<br/>",  x.tooltip, ": ' + this.x + '",
		# 	 "<br/>'", "+ this.point.z +","'</div>';",
		# 	"} !#", sep = "")
	# formatter <- paste("#! function() {", "return '<b>' + this.series.name + ",
	# 	 "'</b><div style=\"color:#686868;font-size:0.8em\">' + this.x + '", x.tooltip, "<br/>'", " + this.y + '",  y.tooltip, "<br/>'","+ this.point.z +",
	# 	 "'</div>';",
	# 	"} !#", sep = "")

	a$tooltip(formatter = formatter, useHTML = T)

	a$yAxis(title = list(text = trad['ylab',lang], style = list(fontWeight = "bold")), floor = 0, ceiling = 70)
	a$xAxis(title = list(text = trad['xlab',lang], style = list(fontWeight = "bold")), min = 0, max = 41,
		lineColor = list ('#FF0000'))
	a$legend(enabled = F)
	#a

	hChart.html <- tempfile("hchart_labelledBubble.html")
	a$save(hChart.html)

	# Example of converting a highcharts-rCharts html chart into a responsive one

	hChart2responsiveHTML(hChart.html, output.html = output.html, h2 = trad['title',lang], descr = trad['descr',lang],
		h3 = "", source = trad['source',lang], author = " swissinfo.ch")

}





