####
# this script contains some very basic functions for plotting and stats
# written by nico schuck, 2013-2021
####

se_bars = function(x, y, se, col=1, wf=0, bi=TRUE, lwd = 2, horiz = FALSE) {
	# adds se bars to data x
	n=length(x)

	w=0.02*max(x)*wf
	y = as.numeric(y)
	se = as.numeric(se)
  col = rep(col, ceiling(n/length(col)))
  if (horiz == FALSE) {
	  for (i in 1:n) {
		  segments(x[i]+w, y[i]+se[i], x[i]-w, y[i]+se[i], col=col[i], lwd = lwd)
      if (bi==TRUE) {
    	   segments(x[i], y[i]-se[i], x[i], y[i]+se[i], col=col[i], lwd = lwd)
    	   segments(x[i]+w, y[i]-se[i], x[i]-w, y[i]-se[i], col=col[i], lwd = lwd)
    	 }	else {
    		 segments(x[i], y[i], x[i], y[i]+se[i], col=col[i], lwd = lwd)
    	 }
		  }
    } else {
    for (i in 1:n) {
      segments(x[i]+se[i], y[i]+w, x[i]+se[i], y[i]+w, col=col[i], lwd = lwd)
      if (bi==TRUE) {
         segments(x[i]-se[i], y[i], x[i]+se[i], y[i], col=col[i], lwd = lwd)
         segments(x[i]-se[i], y[i]+w, x[i]-se[i], y[i]-w, col=col[i], lwd = lwd)
        }  else {
         segments(x[i], y[i], x[i]+se[i], y[i], col=col[i], lwd = lwd)
         }

      }
    }
  }

se_shadows = function(x, y, se, ccol='#66666666', border = NA) {
	# adds se shadows to data x
	polygon(c(x, rev(x)), c(y + se, rev(y - se)), col = ccol, border = border)
}

count = function(x) {
	return(length(na.omit(x)))
}

std.error = function(X, na.rm = TRUE, within = FALSE ) {
  if (within == FALSE) {
    sd(X, na.rm = TRUE)/sqrt(count(X))
  } else {
    # assumes
    Y = X - rowMeans(X) + mean(X)
    sd(X, na.rm = TRUE)/sqrt(count(X))
  }
}

runmean = function(data, length) {
	n=length(data-length)
	y=rep(NA, n)
	for (i in 1:n) {
		end<-i+length
		y[i]<-mean(data[i:end])
	}
	return(y)
}
