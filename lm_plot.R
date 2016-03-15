lm_plot <- function(x,orig,title,p_max=0.01,text=NULL,offsets=NULL,flip=NULL,
                    width=15) {
  # x must be the output from a multiple regression on MICE output
  # orig must be the original, unimputed datasetas
  # p_max is the maximum p-value that will be included in the viz
  # text is a 2-column data frame containing variable names and
  #   the text with which they should be replaced
  # offsets is a 3-column data frame (var,x,y) detailing how 
  #   labels should be offset for readability.
  # flip is a list of variables whose directionality should be 
  #       flipped for better comprehension                      
  # width is the width (in characters) to be used for wrapping of long
  #   labels
  s <- summary(pool(x))
  plotme <- data.frame(s[-1,c(1,5)])
  plotme$log_p <- -log10(plotme[,2])
  m <- max(plotme$log_p[plotme$log_p < Inf])
  plotme$log_p[plotme$log_p > m] <- 1.1*m
  plotme$scale <- sapply(rownames(plotme),
                         function(x) max(orig[,x],na.rm=TRUE) - min(orig[,x],na.rm=TRUE))
  plotme$scale[plotme$scale %% 1 > 0] <- 1 # don't rescale composite indices
  plotme$est <- plotme$est * plotme$scale
  plotme$label <- rownames(plotme)
  # get rid of non-significant points
  plotme <- plotme[plotme$log_p > -log10(p_max),]
  if (nrow(plotme) == 0) {
    print('ERROR: nothing to plot.')
    print('Try using a higher value for p_max!')
  }
  # flip as needed
  if (!is.null(flip)) {
    plotme[plotme$label %in% flip,'est'] <- -1*plotme[plotme$label %in% flip,'est']
  }
  # move labels if needed
  plotme$label_x <- plotme$log_p
  plotme$label_y <- plotme$est
  if (!is.null(offsets)) {
    j <- join(plotme,offsets,by='label')
    plotme[!is.na(j$x),'label_x'] <- plotme[!is.na(j$x),'label_x'] + 
      j[!is.na(j$x),'x']
    plotme[!is.na(j$y),'label_y'] <- plotme[!is.na(j$y),'label_y'] + 
      j[!is.na(j$y),'y']
  }
  # replace labels
  if (!is.null(text)) {
    j <- join(plotme,text,by='label')
    plotme[!is.na(j$long),'label'] <- str_wrap(j[!is.na(j$long),'long'],width)
  }
  # now build the plot
  x1 = min(plotme$log_p)
  x2 = max(plotme$log_p)
  y1 = min(plotme$est)
  y2 = max(plotme$est)
  p <- ggplot(data=plotme,aes(x=log_p,y=est,color=est,label=label)) +
    #geom_point(size=20,alpha=0.2) +
    geom_point(alpha=0.2,aes(size=log_p)) +
    scale_size_continuous(range=c(15,50)) +
    geom_text(color='black',lineheight=0.8,aes(x=label_x,y=label_y)) +
    scale_color_gradientn(colours=c('cornflowerblue','seashell3',
                                    'firebrick1')) +
    geom_hline(yintercept=0,color='seashell3') +
    scale_x_continuous(breaks=c(x1+0.1*(x2-x1),x1+0.9*(x2-x1)),
                       labels=c('Lower confidence','Higher confidence'),
                       limits=c(x1,1.05*x2)) +
    scale_y_continuous(breaks=c(y1+0.1*(y2-y1),y1+0.9*(y2-y1)),
                       labels=c(str_wrap('Less',10),
                                str_wrap('More',10)),
                       limits=c(1.1*y1,1.1*y2)) +
    theme_classic() +
    theme(legend.position='none',
          text=element_text(size=20),
          axis.ticks = element_blank(), 
          axis.title = element_blank()) +
    ggtitle(title)
  if (p_max > 0.01 & max(plotme[,2]) > 0.01) {
    p <- p + geom_segment(x=-log10(0.01),xend=-log10(0.01),
                          y=1.2*min(plotme$est),yend=0.95*max(plotme$est),color='red') +
      annotate("text",x=-log10(0.01)+0.2,y=max(plotme$est),label='p=0.01',
               color='red') 
  }
  if (p_max > 0.05 & max(plotme[,2]) > 0.05) {
    p <- p + geom_segment(x=-log10(0.05),xend=-log10(0.05),
                          y=1.2*min(plotme$est),yend=0.95*max(plotme$est),color='blue') +
       annotate("text",x=-log10(0.05)-0.2,y=max(plotme$est),label='p=0.05',
                color='blue') 
  }
  print(p)
  plotme # return this so I can re-use it below
}
