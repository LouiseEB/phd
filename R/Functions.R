#functions

rMaster <- function(data = data, ep= "mace"){
  data <- dplyr::filter(data, endpoint == ep, study == "Fixed effects model" ) %>%
    dplyr::select(row, ci_l, ci_u)

  yStart=data$row-0.2
  yEnd=data$row+0.2
  xStart=data$ci_l
  xEnd=data$ci_u

  xMiddel <- xStart + ((xEnd - xStart) / 2)
  yMiddel <- yStart + ((yEnd - yStart) / 2)
  data.frame(
    y = c(yStart, yMiddel, yEnd, yStart, yMiddel, yEnd),
    x = c(xMiddel, xEnd, xMiddel, xMiddel, xStart, xMiddel)
  )
}

rMaster2 <- function(data = data, ep= "mace", stu = "DPP-4 inhibitors" ){
  data <- dplyr::filter(data, endpoint == ep, study == stu ) %>%
    dplyr::select(row, ci_l, ci_u)

  yStart=data$row-0.2
  yEnd=data$row+0.2
  xStart=data$ci_l
  xEnd=data$ci_u

  xMiddel <- xStart + ((xEnd - xStart) / 2)
  yMiddel <- yStart + ((yEnd - yStart) / 2)
  data.frame(
    y = c(yStart, yMiddel, yEnd, yStart, yMiddel, yEnd),
    x = c(xMiddel, xEnd, xMiddel, xMiddel, xStart, xMiddel)
  )
}


