#### ESTE SCRIPT SOH DEVE SER RODADO EM UMA DAS MAQUINAS:
if (system('uname -n', intern=TRUE) != "abacus0015") quit("no");

#### O output de gráficos deve ser feito no seguinte diretório:
setwd("/var/www/stats")

# Load libraries
library(chron)

##### CONFIGURATION
TOL = 5 / (60 * 24) # How many minutes with no activity is considered offline
DAYS = 2 # How many days should be plotted?
MAXNET = 1e7 # Max internet speed (bytes/sec)
SIZE = 720 # Pixels for images
FG = "#202020" # Foreground color
BG = "#ddeedd" # Background color

### Helper functions here
my.as.chron <- function(data, format=c('y-m-d', 'h:m:s')) {
    if(length(data) == 0) return (chron())
    if("POSIXt" %in% class(data))
        data = as.character(data)
    thedates = t(as.data.frame(strsplit(data,' ')))
    row.names(thedates) = NULL
    thetimes = chron(dates=thedates[,1],
                     times=thedates[,2],
                     format=format) 
    thetimes
}

# Date and time settings
now = my.as.chron(Sys.time())
start = now - DAYS

onePlot = function(x, which) { # Uses global "start, now"
	horas = c(start, x$Hora, now)
	breaks = which(diff(horas) > TOL)
    # Plot Structure
    plot(x$Hora, x[[which]], type='n', las=1, bty = 'n',
         xlim= c(start, now), xlab = "Data/Hora",
         ylim= c(0, 100), ylab = "", main=which)
    # Gaps (ok for no breaks)
    for (i in 1:length(breaks)) {
        idx = breaks[i]
        lines(c(horas[idx], horas[idx+1]),y=c(0,0),  
              lty=2, col='darkred')
    }
    # Lines
    for (i in 1:(length(breaks)+1)) {
        idx = breaks[i]
        # CASE no breaks
        if (length(breaks)==0) {
            lines(x$Hora, x[[which]], col='cornflowerblue')
            break
        }
        # There ARE breaks
        if(i == 1) {
            from = 1
        } else {
            from = breaks[i-1]
        }
        if (i == length(breaks)+1) {
            to = length(x[[which]]) 
        } else {
            to = breaks[i]-1
        }
        lines(horas[from:to +1], x[[which]][from:to], 
              col='cornflowerblue')
    }
}
## Read data file 
fourPlots = function(hostname) {
    # Reads the last log
    lastw = tryCatch( {
    	read.csv(paste0("/var/log/",hostname,"/stat.log.1"), header=FALSE, sep=";", as.is=TRUE)
      }, error = function(e) return (
          data.frame(Hora=character(0),CPU=numeric(0),Memory=numeric(0),Download=numeric(0), Upload=numeric(0), stringsAsFactors=FALSE) 
    ))
    # Reads the current log
    curw = tryCatch( {
    	read.csv(paste0("/var/log/",hostname,"/stat.log"), header=FALSE, sep=";", as.is=TRUE)
      }, error = function(e) return (
          data.frame(Hora=character(0),CPU=numeric(0),Memory=numeric(0),Download=numeric(0), Upload=numeric(0), stringsAsFactors=FALSE) 
    ))
	x = rbind(lastw, curw)
	names(x) = c("Hora", "CPU", "Memory", "Download", "Upload")
	x$Hora = my.as.chron(x$Hora)
	x = subset(x, Hora > start)
	MAXNET = max(MAXNET, max(x$Download, na.rm=TRUE))
	x$Download = x$Download / MAXNET * 100
	x$Upload = x$Upload / MAXNET * 100
	# Do the actual plotting
	png(paste0(hostname,".png"), SIZE, SIZE)
	par(mfrow=c(2,2), bg=BG, fg=FG, col.axis=FG, col.lab=FG, col.main=FG)
	onePlot(x, "CPU")
	onePlot(x, "Memory")
	onePlot(x, "Download")
	onePlot(x, "Upload")
	dev.off()
}

for (i in 0:23) {
	name = paste0("abacus",sprintf("%04d", i))
	print(name)
	fourPlots(name)
}
