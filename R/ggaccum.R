##----------------------------------------------------------------------
## Accumulation curves using ggplot2
## Author:  German Gonzalez
##----------------------------------------------------------------------

require(ggrepel)
require(dplyr)
require(tibble)

`ggaccum` <-
    function(spo, add = FALSE, random = FALSE, ci = 2,
             ci.type = c("bar","line","polygon"), col = par("fg"), lty = 1,
             ci.col = col, ci.lty = "solid", ci.length = 0,  xlab, ylab = spo$method,
             ylim, alpha = 0.8, xvar = c("sites", "individuals", "effort"), ...)
{
    if(random && !(spo$method %in% c("random", "collector")))
        stop("random = TRUE can be used only with method='random'")
    if(spo$method == "collector")
        random <- TRUE
    xvar <- match.arg(xvar)
    
    ## adjust weights to number of sites
    if (random && !is.null(spo$weights) && xvar == "sites") {
        n <- length(spo$effort)
        adj <- n/spo$effort[n]
    } else {
        adj <- 1
    }
    xaxvar <- spo[[xvar]]
    
    if (missing(xlab))
        xlab <- paste(toupper(substring(xvar, 1, 1)),
                              substring(xvar, 2), sep="")
    if (random)
        ci <- FALSE
        
    ci.type <- match.arg(ci.type)
    
    if (!add) {
        if (missing(ylim))
            if (random)
                ylim <- c(1, max(spo$perm, na.rm = TRUE))
            else
                ylim <- c(1, max(spo$richness, spo$richness + ci*spo$sd, na.rm = TRUE))
              
        p <- ggplot(data = data.frame(x=xaxvar, y=spo$richness), aes(x = x, y = y)) +
            geom_line(linetype = "dashed") +
            xlab(xlab) + ylab(ylab) + coord_cartesian(ylim=ylim)
    }
    if (!is.null(spo$sd) && ci)
        switch(ci.type,
               bar = {
                p <- p + geom_segment(aes(x = xaxvar, y = (spo$richness - ci*spo$sd), 
                            xend =  xaxvar, yend = (spo$richness + ci*spo$sd), 
                            colour = ci.col, linetype = ci.lty )) +
                            theme(legend.position="none")
                      },
               line = {
                p <- p + geom_path(data = data.frame(x = c(xaxvar, rev(xaxvar)), 
                    y = c(spo$richness - ci*spo$sd, rev(spo$richness + ci*spo$sd))),
                    aes(x = x, y = y))
               },
               polygon = {
                p <- p + geom_polygon(data = data.frame(x = c(xaxvar, rev(xaxvar)), 
                    y = c(spo$richness - ci*spo$sd, rev(spo$richness + ci*spo$sd))),
                    aes(x = x, y = y), alpha = alpha, fill = ci.col)
               })
           
    if (random) {
        if (is.null(spo$weights)) {
            for(i in seq_len(NCOL(spo$perm)))
                lines(xaxvar, spo$perm[,i], col=col, ...)
        } else {
            for(i in seq_len(NCOL(spo$perm)))
                lines(spo$weights[,i]*adj, spo$perm[,i], col=col, ...)
        }
    } else
        lines(xaxvar, x$richness, col=col, lty = lty, ...)
    
    p
}

`lines.specaccum` <-
    function(x, ...)
{
    plot(x, add = TRUE, ...)
}

# 
# ## Curvas de rarefaccion por sitio
# sarf1 <- specaccum(datos1, "random")
# 
# plot(sarf1, ci.type="polygon", xlab = "Sitios", ylab = "Rarefacción", col = "grey50", 
# lwd = 2, lty= 3, main="Sitio 1", ylim=c(0, max(estriq$Species)))

