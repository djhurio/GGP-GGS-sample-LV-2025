# TSP sort and plot functions

sort.DT <- function(
  DT,
  coords = c("koord_x", "koord_y"),
  method = "arbitrary_insertion"
) {
  tour <- solve_TSP(
    ETSP(
      DT[, .SD, .SDcols = coords]
    ),
    method = method
  )
  dat <- copy(DT[tour])
  dat[, i := .I]
  return(dat[])
}

sort.DT.by <- function(
  DT,
  by,
  coords = c("koord_x", "koord_y"),
  method = "arbitrary_insertion"
) {
  by.values <- unique(DT[, get(by)])
  return(rbindlist(purrr::map(
    by.values,
    function(x) {
      sort.DT(
        DT = DT[get(by) == x],
        coords = coords,
        method = method
      )
    }
  )))
}

plot.DT <- function(
  DT,
  coords = c("koord_x", "koord_y"),
  group = NULL,
  colour = NULL,
  size = NULL,
  title = NULL,
  subtitle = NULL
) {
  ggplot(DT, aes_string(x = coords[1], y = coords[2], group = group)) +
    geom_point(aes_string(colour = colour, size = size)) +
    geom_path(linetype = "dashed") +
    coord_fixed() +
    ggtitle(label = title, subtitle = subtitle) +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
}

plot.DT.by <- function(
  DT,
  by,
  coords = c("koord_x", "koord_y"),
  group = NULL,
  colour = NULL,
  size = NULL,
  title = NULL
) {
  by.values <- unique(DT[, get(by)])
  purrr::map(
    by.values,
    function(x) {
      plot.DT(
        DT = DT[get(by) == x],
        coords = coords,
        group = group,
        colour = colour,
        size = size,
        title = title,
        subtitle = paste(by, x, sep = ": ")
      )
    }
  )
}
