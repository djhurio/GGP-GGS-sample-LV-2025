# TSP sort and plot functions

sort.DT <- function(
  DT,
  coords = c("koord_y", "koord_x"),
  method = "arbitrary_insertion",
  ord.name = "i"
) {
  tour <- solve_TSP(
    ETSP(
      DT[, .SD, .SDcols = coords]
    ),
    method = method
  )
  dat <- copy(DT[tour])
  dat[, c(ord.name) := .(.I)]
  return(dat[])
}

sort.DT.by <- function(
  DT,
  by,
  coords = c("koord_y", "koord_x"),
  method = "arbitrary_insertion",
  ord.name = "i"
) {
  by.values <- unique(DT[, get(by)])
  dat <- rbindlist(purrr::map(
    by.values,
    function(x) {
      sort.DT(
        DT = DT[get(by) == x],
        coords = coords,
        method = method,
        ord.name = ord.name
      )
    }
  ))
  dat[, c(ord.name) := .(.I)]
  return(dat[])
}

plot.DT <- function(
  DT,
  coords = c("koord_y", "koord_x"),
  group = NULL,
  colour = NULL,
  size = NULL,
  title = NULL,
  subtitle = NULL
) {
  # Convert character column names to symbols
  # x <- sym(coords[1])
  # y <- sym(coords[2])

  aes_mapping <- aes(x = !!sym(coords[1]), y = !!sym(coords[2]))

  if (!is.null(group)) {
    aes_mapping$group <- sym(group)
  }

  point_aes <- list()
  if (!is.null(colour)) {
    point_aes$colour <- sym(colour)
  }
  if (!is.null(size)) {
    point_aes$size <- sym(size)
  }

  p <- ggplot(DT, aes_mapping) +
    geom_point(do.call(aes, point_aes)) +
    geom_polygon(linetype = "dashed", fill = NA, colour = "black") +
    coord_fixed() +
    ggtitle(label = title, subtitle = subtitle) +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )

  return(p)
}

plot.DT.by <- function(
  DT,
  by,
  coords = c("koord_y", "koord_x"),
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
