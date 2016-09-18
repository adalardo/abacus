

#' Machinefile
#' copied from https://github.com/andrechalom/pse/blob/master/R/cluster.R
#' Convenience for Clustering
#'
#'	Provides a convenience interface for using MPD-style hostfiles to generate cluster objetcs.
#'	The hostfile should be written as a text file using the MPD style:
#'	one line for each host, which can be followed by a colon and a number
#'	indicating the number of processes to be started on that host. An example
#'	hostfile for starting three processes on two hosts named avalon and
#'	glastonbury would be:
#'
#'	avalon
#'	glastonbury:2
#'
#' @param name Filename of the hostfile.
#' @examples
#'	\dontrun{
#'		library(parallel)
#'		cl = makePSOCKcluster(machinefile("mpd.hosts"))
#'		stopCluster(cl)
#'	}
#' @export
machinefile <- function(name) {
	x <- utils::read.table(name, sep=":", header=FALSE, stringsAsFactors=FALSE, fill=TRUE)
	ret <- c()
	for (i in 1:(dim(x)[1])) { 
		if (is.null(x[i,2]) | is.na (x[i,2])) x[i,2] <- 1
		ret <-c(ret, rep(x[i,1], as.numeric(x[i,2])))
	}
	return(ret)
}


library(parallel)
cl = makePSOCKcluster(machinefile("mpd.hosts"))

ret1 <- clusterCall(cl,rnorm,100,0,1)
parSapply(cl,ret1,summary)

n<- clusterCall(cl,set.seed,100)
ret2 <- clusterCall(cl,rnorm,100,0,1)
parSapply(cl,ret2,summary)

xx<- seq(0,100,.1)
clusterExport(cl,"xx")
ret3<-clusterCall(cl, function(y) xx + y, 10)
parSapply(cl,ret3,summary)

stopCluster(cl)

