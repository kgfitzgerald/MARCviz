# look at ggdist for how constructed ggplot & adpatable geoms

plot_marc <- function(x, ...) {
  if (!inherits(x, c("rma", "data.frame"))) {
    cli::cli_abort(
      message = "{.arg x} must be one of {.cls rma} or {.cls data.frame}, not {.cls {class(x)}}.",
      class = "vizmarc_x_not_inherit"
    )
  }

  meta_df <- get_meta(x, ...)

  base <- ggplot2::ggplot(meta_df) +
    ggplot2::theme_light(base_line_size = .1) +
    ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 12),
      panel.grid.minor.y = ggplot2::element_blank(),
      legend.position = "none",
      plot.caption = ggplot2::element_text(
        size = 10,
        face = "italic",
        color = "grey",
        hjust = 0
      )
    ) +

    # Left/Right Shaded Regions
    ggplot2::geom_vline(xintercept = 0, alpha = 0.3, linewidth = 2) +
    ggplot2::annotate(
      "rect",
      xmin = -Inf,
      xmax = 0,
      ymin = -Inf,
      ymax = Inf,
      alpha = .1,
      fill = "red"
    ) +
    ggplot2::annotate(
      "rect",
      xmin = 0,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      alpha = .1,
      fill = "blue"
    )

  return(base)
}

get_meta <- function(x, ...) {
  UseMethod("get_meta")
}

get_meta.data.frame <- function(
  x,
  d_j,
  se_j,
  method = c("FE", "REML"),
  ...
) {
  rlang::check_dots_used()

  if (missing(d_j)) {
    cli::cli_abort(
      message = "{.arg d_j} is missing, with no default.",
      class = "vizmarc_d_j_missing"
    )
  }

  if (missing(se_j)) {
    cli::cli_abort(
      message = "{.arg se_j} is missing, with no default.",
      class = "vizmarc_se_j_missing"
    )
  }

  rlang::arg_match(method)

  d_j <- dplyr::pull(x, {{ d_j }})
  se_j <- dplyr::pull(x, {{ se_j }})

  if (!is.numeric(d_j)) {
    cli::cli_abort(
      message = "{.arg d_j} must be <numeric>, not {.cls {class(d_j)}}.",
      class = "vizmarc_d_j_not_numeric"
    )
  }

  if (!is.numeric(se_j)) {
    cli::cli_abort(
      message = "{.arg se_j} must be <numeric>, not {.cls {class(se_j)}}.",
      class = "vizmarc_se_j_not_numeric"
    )
  }

  if (any(se_j < 0)) {
    cli::cli_abort(
      message = "{.arg se_j} must not contain negative values.",
      class = "vizmarc_se_j_negative"
    )
  }

  if (any(is.na(d_j))) {
    cli::cli_abort(
      message = "{.arg d_j} must not contain {.val NA} values.",
      class = "vizmarc_d_j_na"
    )
  }

  if (any(is.na(se_j))) {
    cli::cli_abort(
      message = "{.arg se_j} must not contain {.val NA} values.",
      class = "vizmarc_se_j_na"
    )
  }

  model <- metafor::rma.uni(
    yi = d_j,
    sei = se_j,
    method = method
  )

  # save study labels for after meeting. if we restrict to rma.uni
  # do we need to worry about them for data.frame method? maybe have
  # to require/figure out a way for rma method
  x |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      w_j = stats::weights(model),
      w_j_percent = w_j / sum(w_j),
      summary_d = as.numeric(model$b),
      summary_se = as.numeric(model$se)
    )
}

get_meta.rma <- function(x, ...) {
  rlang::check_dots_used()

  # Q: what extra checks should be made?
  # Q: how should we handle study labels? if ppl supply rma, will they
  # always have a study label through `slab`? what about data.frame method?

  tibble::tibble(
    # study = x$slab,
    d_j = x$yi,
    w_j = stats::weights(x),
    w_j_percent = w_j / sum(w_j),
    summary_d = as.numeric(x$b),
    summary_se = as.numeric(x$se)
  )
}

dat <- escalc(
  measure = "RR",
  ai = tpos,
  bi = tneg,
  ci = cpos,
  di = cneg,
  data = dat.bcg,
  slab = paste(author, year, sep = ", ")
)
res <- rma(yi, vi, data = dat, test = "knha")

viz_MARC(res)

data(viz_MA_data)
test_df <- data.frame(
  d_j = viz_MA_data |> filter(k == 100) |> pull(d_j),
  se_j = viz_MA_data |> filter(k == 100) |> pull(se_j)
)

plot_marc(res)
plot_marc(test_df, d_j, se_j)
viz_MARC(test_df$d_j, test_df$se_j)
