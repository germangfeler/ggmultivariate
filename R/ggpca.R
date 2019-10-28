##----------------------------------------------------------------------
## Biplot for PCA results using ggplot2
## Author:  German Gonzalez
##----------------------------------------------------------------------

PCbiplot <- function(PC, x="PC1", y="PC2", cl="black") {

  # PC being a prcomp object
  data <- data.frame(obsnames=row.names(PC$x), PC$x)
  PoV <- round(100*PC$sdev^2/sum(PC$sdev^2),2)
  names(PoV) <- colnames(PC$x)

  datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )

  ## Plot
  p <- ggplot(data, aes_string(x=x, y=y)) + 
        geom_point(aes(color=cl), size=4) + 
        
        # Ejes
        geom_hline(aes(yintercept=0), size=.2) + 
        geom_vline(aes(xintercept=0), size=.2) +
        coord_equal() + 
       
        # Vectores
        geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), 
           arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="red") +
           
        # Labels
        xlab(paste(x, " (", PoV[x], "%)")) + 
        ylab(paste(y, " (", PoV[y], "%)")) +
   
  if(label) {
    
     p <- p + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), 
         size = 4, vjust=1, color="red")
  
  }
 
  p
}
