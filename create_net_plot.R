# Code taken and adjusted from ricardo-bion.
# https://github.com/ricardo-bion/ggradar/blob/master/R/ggradar.R

library(ggplot2)

CreateRadialPlot = function(
  plot.data,
  axis.labels = colnames(plot.data)[-1],                             
  grid.min=1,
  grid.mid=3,
  grid.max=5,
  centre.y=grid.min - ((1/9)*(grid.max-grid.min)),
  plot.extent.x.sf=1.2,
  plot.extent.y.sf=1.2,
  x.centre.range=0.02*(grid.max-centre.y),
  label.centre.y=FALSE,
  grid.line.width=0.5,
  gridline.min.linetype="longdash",
  gridline.mid.linetype="longdash",
  gridline.max.linetype="solid",
  gridline.min.colour="grey",
  gridline.mid.colour="blue",
  gridline.max.colour="grey",
  grid.label.size=3,
  gridline.label.offset=-0.02*(grid.max-centre.y),
  label.gridline.min=TRUE,
  axis.label.offset=1.15,
  axis.label.size=3,
  axis.line.colour="grey",
  group.line.width=1,
  group.point.size=3,
  background.circle.colour="white",
  background.circle.transparency=0.2,
  plot_legend = if (nrow(plot.data)>1) TRUE else FALSE,
  legend.text.size=grid.label.size,
  plot.color = c("seagreen", "springgreen3"),
  left.labels.hjust = "inward",
  center.labels.vjust = "inward",
  right.labels.hjust = "inward",
  legend.title = "Interests") {
  
  var.names = colnames(plot.data)[-1]
  grp = plot.data$group
  grp1 = grp[2]
  grp2 = grp[1]
  
  #caclulate total plot extent as radius of outer circle x a user-specifiable scaling factor
  plot.extent.x=(grid.max+abs(centre.y))*plot.extent.x.sf
  plot.extent.y=(grid.max+abs(centre.y))*plot.extent.y.sf
  
  #Check supplied data makes sense
  if (length(axis.labels) != ncol(plot.data)-1) 
    return("Error: 'axis.labels' contains the wrong number of axis labels") 
  if(min(plot.data[,-1])<centre.y)
    return("Error: plot.data' contains value(s) < centre.y")
  if(max(plot.data[,-1])>grid.max)
    return("Error: 'plot.data' contains value(s) > grid.max")
  
  #Declare required internal functions
  
  CalculateGroupPath = function(df) {
    #Converts variable values into a set of radial x-y coordinates
    #Code adapted from a solution posted by Tony M to
    #http://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-
    #star-plot-spider-plot-using-ggplot2-in-r
    
    #Args:
    #  df: Col 1 -  group ('unique' cluster / group ID of entity)
    #      Col 2-n:  v1.value to vn.value - values (e.g. group/cluser mean or median) of variables v1 to v.n
    
    path = as.factor(as.character(df[,1]))
    
    ##find increment
    angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1))
    
    ##create graph data frame
    graphData= data.frame(seg="", x=0,y=0)
    graphData=graphData[-1,]
    
    for(i in levels(path)){
      pathData = subset(df, df[,1]==i)
      for(j in c(2:ncol(df))){
        graphData=rbind(graphData, data.frame(group=i, 
          x=pathData[,j]*sin(angles[j-1]),
          y=pathData[,j]*cos(angles[j-1])))
      }
      ##complete the path by repeating first pair of coords in the path
      graphData=rbind(graphData, data.frame(group=i, 
        x=pathData[,2]*sin(angles[1]),
        y=pathData[,2]*cos(angles[1])))
    }
    #Make sure that name of first column matches that of input data (in case !="group")
    colnames(graphData)[1] = colnames(df)[1]
    graphData #data frame returned by function
  }
  
  CaclulateAxisPath = function(var.names,min,max) {
    #Caculates x-y coordinates for a set of radial axes (one per variable being plotted in radar plot)
    
    #Args:
    #var.names - list of variables to be plotted on radar plot
    #min - MININUM value required for the plotted axes (same value will be applied to all axes)
    #max - MAXIMUM value required for the plotted axes (same value will be applied to all axes)
    
    #var.names = c("v1","v2","v3","v4","v5")
    n.vars = length(var.names) # number of vars (axes) required
    #Cacluate required number of angles (in radians)
    angles = seq(from=0, to=2*pi, by=(2*pi)/n.vars)
    #calculate vectors of min and max x+y coords
    min.x = min*sin(angles)
    min.y = min*cos(angles)
    max.x = max*sin(angles)
    max.y = max*cos(angles)
    
    #Combine into a set of uniquely numbered paths (one per variable)
    axisData = NULL
    for (i in 1:n.vars) {
      a = c(i,min.x[i],min.y[i])
      b = c(i,max.x[i],max.y[i])
      axisData = rbind(axisData,a,b)
    }
    
    #Add column names + set row names = row no. to allow conversion into a data frame
    colnames(axisData) = c("axis.no","x","y")
    rownames(axisData) = seq(1:nrow(axisData))
    
    #Return calculated axis paths
    as.data.frame(axisData)
  }
  
  
  funcCircleCoords = function(center = c(0,0), r = 1, npoints = 100){
    #Adapted from Joran's response to http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
    tt = seq(0,2*pi,length.out = npoints)
    xx = center[1] + r * cos(tt)
    yy = center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  ### Convert supplied data into plottable format
  
  # (a) add abs(centre.y) to supplied plot data 
  # [creates plot centroid of 0,0 for internal use, regardless of min. value of y
  #  in user-supplied data]
  plot.data.offset = plot.data
  plot.data.offset[,2:ncol(plot.data)]= plot.data[,2:ncol(plot.data)]+abs(centre.y)
  
  # (b) convert into radial coords
  group =NULL
  group$path = CalculateGroupPath(plot.data.offset)
  
  # (c) Calculate coordinates required to plot radial variable axes
  axis = NULL
  axis$path = CaclulateAxisPath(var.names,grid.min+abs(centre.y),grid.max+abs(centre.y))
  
  # (d) Create file containing axis labels + associated plotting coordinates
  
  #Labels
  axis$label = data.frame(
    text=axis.labels,
    x=NA,
    y=NA )
  
  #axis label coordinates
  n.vars = length(var.names)
  angles = seq(from=0, to=2*pi, by=(2*pi)/n.vars)
  axis$label$x = sapply(1:n.vars, function(i, x) {
    ((grid.max+abs(centre.y))*axis.label.offset)*sin(angles[i])
  })
  axis$label$y = sapply(1:n.vars, function(i, x) {
    ((grid.max+abs(centre.y))*axis.label.offset)*cos(angles[i])
  })
  
  # (e) Create Circular grid-lines + labels
  #caclulate the cooridinates required to plot circular grid-lines for three user-specified
  #y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
  gridline = NULL
  gridline$min$path = funcCircleCoords(c(0,0),grid.min+abs(centre.y),npoints = 360)
  gridline$mid$path = funcCircleCoords(c(0,0),grid.mid+abs(centre.y),npoints = 360)
  gridline$max$path = funcCircleCoords(c(0,0),grid.max+abs(centre.y),npoints = 360)
  
  #gridline labels
  gridline$min$label = data.frame(x=gridline.label.offset,y=grid.min+abs(centre.y),
    text=as.character(grid.min))
  gridline$max$label = data.frame(x=gridline.label.offset,y=grid.max+abs(centre.y),
    text=as.character(grid.max))
  gridline$mid$label = data.frame(x=gridline.label.offset,y=grid.mid+abs(centre.y),
    text=as.character(grid.mid))
  
  ### Start building up the radar plot
  
  # Delcare 'theme_clear', with or without a plot legend as required by user
  #[default = no legend if only 1 group [path] being plotted]
  theme_clear = theme_bw() + 
    theme(axis.text.y=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.border=element_blank(),
      legend.key=element_rect(linetype="blank"))
  
  if (plot_legend==FALSE) theme_clear = theme_clear + theme(legend.position="none")
  
  #Base-layer = axis labels + plot extent
  # [need to declare plot extent as well, since the axis labels don't always
  # fit within the plot area automatically calculated by ggplot, even if all
  # included in first plot; and in any case the strategy followed here is to first
  # plot right-justified labels for axis labels to left of Y axis for x< (-x.centre.range)], 
  # then centred labels for axis labels almost immediately above/below x= 0 
  # [abs(x) < x.centre.range]; then left-justified axis labels to right of Y axis [x>0].
  # This building up the plot in layers doesn't allow ggplot to correctly 
  # identify plot extent when plotting first (base) layer]
  
  #base layer = axis labels for axes to left of central y-axis [x< -(x.centre.range)]
  axis$label$text = gsub(".", "\n", fixed = TRUE, x = axis$label$text)
  base = ggplot(axis$label) + xlab(NULL) + ylab(NULL) + coord_equal() +
    geom_text(data=subset(axis$label,axis$label$x < (-x.centre.range)),
      aes(x=x,y=y,label=text),size=axis.label.size,vjust="inward",hjust=left.labels.hjust) +
    scale_x_continuous(limits=c(-plot.extent.x,plot.extent.x)) + 
    scale_y_continuous(limits=c(-plot.extent.y,plot.extent.y))
  
  # + axis labels for any vertical axes [abs(x)<=x.centre.range]
  base = base + geom_text(data=subset(axis$label, abs(axis$label$x) <= x.centre.range),
    aes(x=x,y=y,label=text),size=axis.label.size, hjust="inward", vjust=center.labels.vjust)
  
  # + axis labels for any vertical axes [x>x.centre.range]
  base = base + geom_text(data=subset(axis$label,axis$label$x>x.centre.range),
    aes(x=x,y=y,label=text),size=axis.label.size,
    vjust="inward", hjust=right.labels.hjust)
  
  # + theme_clear [to remove grey plot background, grid lines, axis tick marks and axis text]
  base = base + theme_clear
  
  #  + background circle against which to plot radar data
  base = base + geom_path(data=gridline$min$path,aes(x,y),
    alpha=background.circle.transparency,
    lty = "dashed")
  
  base = base + geom_path(data=gridline$mid$path,aes(x,y),
    alpha=background.circle.transparency,
    lty = "dashed")
  
  # + radial axes
  base = base + geom_path(data=axis$path,aes(x=x,y=y,group=axis.no),
    colour=axis.line.colour)
  base = base + annotate(geom = "point", x = 0, y = 0, col = "darkgray")
  
  # ... + group (cluster) 'paths'
  base = base + geom_polygon(data = group$path[group$path$group == grp2, ],
    aes(x = x, y = y), colour = plot.color[1], fill = plot.color[1],
    size = 1, alpha = 0.8)
  
  base = base + geom_polygon(data = group$path[group$path$group == grp1, ],
    aes(x = x, y = y), colour = plot.color[2], fill = plot.color[2],
    size = 1, alpha = 0.4)
  
  
  # ... + group points (cluster data)
  base = base + geom_point(data = group$path,
    aes(x = x, y = y, col = group),
    size = group.point.size)
  
  
  #... + amend Legend title
  base  = base + scale_colour_manual(name = legend.title,
    values = plot.color, labels = grp)
  
  if (!plot_legend) {
    guides(color=FALSE)
  }
  
  
  # ... + circular grid-lines at 'min', 'mid' and 'max' y-axis values
  base = base +  geom_path(data=gridline$max$path,aes(x=x,y=y),
    lty=gridline.max.linetype,
    colour=gridline.max.colour,size=grid.line.width)
  
  return(base)
  
}
