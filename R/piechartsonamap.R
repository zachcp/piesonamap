#' Createa PieChart from a Dataframe
#'
#' @param df, dataframe. A five column dataframe where ID/Lat/Lon/Variable/Value
#' @importFrom ggtree theme_tree theme_transparent
#' @export
#'
makepie <- function(df, trim=TRUE){
  id   <- unique(df[[1]])
  lat  <- unique(df[[2]])
  lon  <- unique(df[[3]])

  if (trim == TRUE) {
    plt <- ggplot(df, aes(x=1, y=value, fill=variable)) +
      geom_bar(stat="identity",width=1) +
      coord_polar(theta="y" )+
      theme_tree() +
      xlab(NULL) +
      ylab(NULL) +
      theme_transparent() +
      theme(plot.margin=unit(c(0,0,0,0),"mm"))
  } else {
    plt <- ggplot(df, aes(x=1, y=value, fill=variable)) +
      geom_bar(stat="identity",width=1) +
      coord_polar(theta="y")
  }

  return(list(plot = plt,lat=lat,lon=lon,id=id ))
}

#' Draw Pies on a Map
#'
#' http://stackoverflow.com/questions/10368180/plotting-pie-graphs-on-map-in-ggplot
#'
#' @importFrom gtable gtable_filter
#' @importFrom ggtree subview
#' @export
#'
piesonmap <- function(map, data, piedata, locationdata,circlesize = 0.1,  legend=TRUE, legendloc = "TL" ){
  gg <- ggplot(data=data,
               aes_string(x="long",y="lat",map_id="region")) +
    geom_map(data=data,map=map,
             aes_string(x="long",y="lat",map_id="region")) +
    theme_bw()

  #add the pies
  pies <- by(piedata, piedata[[1]], makepie)  #first column is IDs
  for (pie in pies) {
    gg <- subview(gg, pie$plot, x=pie$lon, y=pie$lat, width = circlesize, height=circlesize)
  }

  #create the legend by creating a plot without stripping the legend
  # ake the legend and cusotom place it on the plot
  if (legend == TRUE) {
    xminmax <- layer_scales(gg)$y$range$range
    yminmax <- layer_scales(gg)$x$range$range

    if (legendloc == "TL"){
      #top left
      xloc <- xminmax[[1]] + (xminmax[[2]] - xminmax[[1]] ) / 10
      yloc <- yminmax[[2]] - (yminmax[[2]] - yminmax[[1]] ) / 10
    } else if(legendloc == "TR") {
      #top right
      xloc <- xminmax[[2]] - (xminmax[[2]] - xminmax[[1]] ) / 10
      yloc <- yminmax[[2]] - (yminmax[[2]] - yminmax[[1]] ) / 10
    }

    firstid <- first(piedata[[1]])
    baseplot <- makepie(piedata[piedata[[1]] == firstid, ] ,trim = FALSE)
    leg1 <- gtable_filter(ggplot_gtable(ggplot_build(baseplot$plot)), "guide-box")
    gg <- gg + annotation_custom(grob = leg1,
                                 xmin = -77.5,
                                 xmax = -78.5,
                                 ymin = 44,
                                 ymax = 55)

  }
  return(gg)

}

#' Draw Pies on a Map
#'
#' http://stackoverflow.com/questions/10368180/plotting-pie-graphs-on-map-in-ggplot
#'
#' @importFrom gtable gtable_filter
#' @importFrom ggtree subview
#' @export
addpies <- function(ggmap, piedata, circlesize = 0.1,  legend=TRUE, legendloc = "TL" ){
  #add the pies
  pies <- by(piedata, piedata[[1]], makepie)  #first column is IDs
  for (pie in pies) {
    ggmap <- subview(ggmap, pie$plot, pie$lat, pie$lon, width = circlesize, height=circlesize)
  }

  #create the legend by creating a plot without stripping the legend
  # ake the legend and cusotom place it on the plot
  if (legend == TRUE) {
    xminmax <- layer_scales(ggmap)$y$range$range
    yminmax <- layer_scales(ggmap)$x$range$range
    xmin <- xminmax[[1]]
    xmax <- xminmax[[2]]
    ymin <- yminmax[[1]]
    ymax <- yminmax[[2]]
    deltax =  (xmax - xmin) / 10
    deltay =  (ymax - ymin) / 10

    if (legendloc == "TL"){
      #top left

      xloc1 <- xmin + deltax
      xloc2 <- xmin + (2 * deltax)
      yloc1 <- ymax - deltay
      yloc2 <- ymax - (2* deltay)

    } else if(legendloc == "TR") {
      #top right
      xloc1 <- xmax - (2 * deltax)
      xloc2 <- xmax - deltax
      yloc1 <- ymax - deltay
      yloc2 <- ymax - (2* deltay)
    }

    firstid <- first(piedata[[1]])
    baseplot <- makepie(piedata[piedata[[1]] == firstid, ] ,trim = FALSE)
    leg1 <- gtable_filter(ggplot_gtable(ggplot_build(baseplot$plot)), "guide-box")

    ggmap <- ggmap + annotation_custom(grob = leg1,
                                 xmin = xloc1,
                                 xmax = xloc2,
                                 ymin = yloc1,
                                 ymax = yloc2)
  }
  return(ggmap)

}


