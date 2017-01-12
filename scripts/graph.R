# Load libraries
library(chron)

##### CONFIGURATION
TOL = 5 / (60 * 24) # How many minutes with no activity is considered offline
DAYS = 0.05 # How many days should be plotted?
MAXNET = 1e7 # Max internet speed (bytes/sec)

### Helper functions here
as.chron <- function(data, format=c('y-m-d', 'h:m:s')) {
    if("POSIXt" %in% class(data))
        data = as.character(data)
    thedates = t(as.data.frame(strsplit(data,' ')))
    row.names(thedates) = NULL
    thetimes = chron(dates=thedates[,1],
                     times=thedates[,2],
                     format=format) 
    thetimes
}
onePlot = function(x, which) { # Uses global "breaks, start, now"
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
x = read.csv("stat.log", header=FALSE, sep=";", as.is=TRUE)
names(x) = c("Hora", "CPU", "Memory", "Download", "Upload")
x$Hora = as.chron(x$Hora)
now = as.chron(Sys.time())
start = now - DAYS
x = subset(x, Hora > start)
MAXNET = max(MAXNET, max(x$Download))
x$Download = x$Download / MAXNET * 100
x$Upload = x$Upload / MAXNET * 100
horas = c(start, x$Hora, now)
breaks = which(diff(horas) > TOL)

# Do the actual plotting
par(mfrow=c(2,2))
onePlot(x, "CPU")
onePlot(x, "Memory")
onePlot(x, "Download")
onePlot(x, "Upload")
