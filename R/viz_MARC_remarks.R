# RECOMENDATION:
# 1. Split viz_marc into two functions: cleaning and plotting
# 2. Cleaning function should be an S3 generic, with different
#    methods corresponding to different rma.* or even meta package.
#    Should also only take model as an input, not d_j, se_j, etc.
# 3. Depending on how complex cleaning is, utilize Next_Method
#    for each rma.* and use rma as default method.
# 4. Functionalize any cleaning/checks that are consistent
#    between rma.*'s
# 3. Call cleaning function inside plotting function
# 4. Keep plotting function as consistent as possible
#    between different models
# 5. Return a ggplot object to cut down on number of arguments
#    and allow end-users to adjust the output to their liking

# meeting notes:
# see how metaviz package sets up functionality
# font sizing is clunky

viz_MARC <- function(
  d_j = NULL,
  se_j = NULL,
  w_j = NULL,
  method_obj = NULL,
  summary_es = NULL,
  summary_se = NULL,
  confidence_level = 0.95,
  summary_only = FALSE,
  study_labels = NULL,
  show_study_labels = FALSE,
  show_dot_annotation = TRUE,
  seed = NULL,
  x_limits = NULL,
  y_limits = NULL,
  y_limits_rect = c(0.8, 1.2),
  width_in = 7,
  height_in = 5,
  font_sizes = c(14, 10, 8, 9, 8, 7, 10, 16, 16),
  textbox_width = 4,
  digits = 2,
  xinc = .2,
  max_dot_size = 10,
  dot_color = "navyblue",
  dot_trans = .5,
  font_type = ""
) {
  # Q1: Whats the logic behind setting the method_obj to one of the
  # first three inputs? In EXP3, code calls the columns individually.
  # Believe its programmed to accept either a metafor object or data.
  # If data, then does the metafor analysis for you.
  #
  # Is this the best approach? Cuts down on code, inputs, debug if
  # choose one at expense of usability. Nonetheless, separate into
  # function. Might have clarity about process based on multi meta
  # points being added to plot. If need both, make it mutually
  # exclusive like some dplyr functions or tidycensus get_* functions
  #
  # Q2: Are all these classes needed? Seems that "rma" is applied to
  # all these cases like "data.frame" for tibble.

  # If metafor object is passed through first:
  if (
    inherits(
      d_j,
      c("rma", "rma.uni", "rma.mh", "rma.peto", "rma.glmm", "rma.mv")
    )
  ) {
    method_obj <- d_j
    d_j <- NULL
  }

  # If metafor object is passed through second:
  if (
    inherits(
      se_j,
      c("rma", "rma.uni", "rma.mh", "rma.peto", "rma.glmm", "rma.mv")
    )
  ) {
    method_obj <- se_j
    se_j <- NULL
  }

  # If metafor object is passed through third:
  if (
    inherits(
      w_j,
      c("rma", "rma.uni", "rma.mh", "rma.peto", "rma.glmm", "rma.mv")
    )
  ) {
    method_obj <- w_j
    w_j <- NULL
  }

  if (!is.null(method_obj)) {
    if (
      !inherits(
        method_obj,
        c("rma", "rma.uni", "rma.mh", "rma.peto", "rma.glmm", "rma.mv")
      )
    ) {
      stop("Method_obj must be a metafor object.")
    }
  }

  #---------------- Error messages -----------------------------

  # Check that either effect sizes and standard errors are provided or a metafor object
  if (is.null(method_obj) & (is.null(d_j))) {
    stop(
      "Missing Effect sizes or metafor object.
       Effect sizes or metafor object must be provided."
    )
  }

  # If metafor object is not provided, check the following
  if (is.null(method_obj)) {
    #if summaries are not provided, check the following
    if (is.null(summary_es) || is.null(summary_se) || is.null(w_j)) {
      #Check for provided standard errors
      if (is.null(se_j)) {
        stop("No provided standard errors. Standard errors must be provided.")
      }
      # Check for negative standard errors
      if (any(se_j < 0)) {
        stop("Negative values found in se_j. Standard errors must be positive.")
      }
      # Check if whether d_j or se_j are numeric vectors
      if (!is.numeric(d_j) || !is.numeric(se_j)) {
        stop("Both d_j and se_j must be numeric vectors.")
      }
      # Check that d_j and se_j are same length
      if (length(d_j) != length(se_j)) {
        stop("d_j and se_j must be the same length.")
      }
      # Check for missing values
      if (any(is.na(d_j)) || any(is.na(se_j))) {
        stop(
          "Missing values detected in d_j or se_j. Please remove or impute missing values before plotting."
        )
      }
    }
  }

  #Check if the summary effect sizes, summary standard errors, and weights are numbers
  if (!is.null(summary_es) & !is.null(summary_se)) {
    if (!is.numeric(summary_es) || !is.numeric(summary_se)) {
      stop(
        "Both the summary effect size and summary standard error must be numeric."
      )
    }
    if (!is.null(w_j)) {
      #check that w_j is a numeric vector or matrix
      if (!is.numeric(w_j)) {
        stop("Weights must be a numeric vector or matrix.")
      }
    }
  }

  # Check confidence_level is between 0 and 1
  if (
    !is.numeric(confidence_level) ||
      confidence_level <= 0 ||
      confidence_level >= 1
  ) {
    stop("confidence_level must be a numeric value between 0 and 1.")
  }

  #Check that study_labels is equal to the len of the effect sizes or method_object
  if (!is.null(study_labels)) {
    if (!is.null(d_j)) {
      if (length(d_j) != length(study_labels)) {
        stop("d_j and study_labels must be the same length.")
      }
    } else if (!is.null(method_obj)) {
      if (length(method_obj$yi) != length(study_labels)) {
        stop(
          "method_object effect sizes and study_labels must be the same length."
        )
      }
    }

    # TODO: make a warning instead
    if (length(study_labels) > 10) {
      stop(
        "Too many studies to include study labels.
            Use viz_MARC_interactive or create subplots with categories of studies.
            Plot is not intended for data exploration but for conveying findings to a greater audience.
           "
      )
    }
  }

  # TODO: move up the chain
  #Check that summary_only is True of False
  if (!summary_only %in% c(TRUE, FALSE)) {
    stop("The 'summary_only' argument must be either 'TRUE' or 'FALSE'.")
  }

  #Check that show_study_labels is True of False
  if (!show_study_labels %in% c(TRUE, FALSE)) {
    stop("The 'show_study_labels' argument must be either 'TRUE' or 'FALSE'.")
  }

  # TODO: likely can rely on ggplot to handle this
  #Check x_limits, y_limits, and y_limits_rect are a numerica vector of length 2
  lim <- list(x_limits, y_limits, y_limits_rect)
  lim_name <- c("x_limits", "y_limits", "y_limits_rect")
  i <- 1
  for (l in lim) {
    if (!is.null(l)) {
      if (length(l) != 2) {
        stop("The ", lim_name[i], " arguments must be a vector with length 2.")
      }
      if (!is.numeric(l)) {
        stop("The ", lim_name[i], " arguments must be numeric.")
      }
    }
    i <- i + 1
  }

  # TODO: likely can rely on ggplot to handle
  #Check that max_dot_size, width_in, height_in, textbox_width, digits, and dot_trans are all a single numeric value
  lim <- list(
    max_dot_size,
    width_in,
    height_in,
    textbox_width,
    digits,
    xinc,
    dot_trans
  )
  lim_name <- c(
    "max_dot_size",
    "width_in",
    "height_in",
    "textbox_width",
    "digits",
    "xinc",
    "dot_trans"
  )
  i <- 1
  for (l in lim) {
    if (length(l) != 1 || !is.numeric(l)) {
      stop("The ", lim_name[i], " must be a single numeric value.")
    }
    i <- i + 1
  }

  if (length(font_sizes) != 9 || !is.numeric(font_sizes)) {
    stop("Font sizes must be a numeric vector with 9 sizes.")
  }

  # TODO: likely can rely on ggplot to handle

  # Check that the dot_color passed through and font_type are string types

  if (!is.character(dot_color)) {
    stop("Dot color must be a string.")
  }
  if (!is.character(font_type)) {
    stop(
      "font_type must be in the format of a string with
       the font type's given name"
    )
  }

  #------ create MA_data from d_j inputs -------
  # create MA_data from d_j inputs if provided
  if (!is.null(d_j)) {
    MA_data <- data.frame(d_j)
    #if not method is specified, set method object to the default
    if (is.null(summary_es) || is.null(summary_se) || is.null(w_j)) {
      if (!is.null(se_j) & is.null(method_obj)) {
        if (is.null(w_j)) {
          MA_data$se_j <- se_j
          method_obj <- rma.uni(
            yi = MA_data$d_j,
            sei = MA_data$se_j,
            method = "FE"
          )
        } else {
          if (is.vector(w_j)) {
            MA_data$se_j <- se_j
            method_obj <- rma.uni(
              yi = MA_data$d_j,
              sei = MA_data$se_j,
              weights = w_j,
              method = "FE"
            )
          } else if (is.matrix(w_j)) {
            MA_data$se_j <- se_j
            V <- se_j^2
            method_obj <- rma.mv(yi = MA_data$d_j, V = V, W = w_j)
          }
        }
      } else if (is.null(se_j) & is.null(method_obj)) {
        stop("Standard errors or metafor object are not provided")
      }
    }
  } else if (is.null(d_j) & !is.null(method_obj)) {
    if (!is.null(method_obj$yi)) {
      d_j <- as.numeric(method_obj$yi)
      MA_data <- data.frame(d_j)
    } else {
      stop("Metfor object does not include effect sizes/outcome")
    }
  } else {
    stop(
      "No effect sizes and standard errors or summary effecs and standard errors
       or metafor object provided."
    )
  }

  #specify # of studies k
  #NOTE TO SELF: For future CRAN package, will need to update this to be flexible for
  #studies w/ multiple effect sizes
  if (is.null(study_labels)) {
    if (inherits(method_obj, "rma.mv")) {
      stop(
        "It appears your meta-analysis may have dependecies but no study labels are provided.
         Add study labels or adjust your meta-analysis method."
      )
    }
    k <- dim(MA_data)[1]
  } else {
    k <- length(unique(study_labels))
  }

  stopifnot(k > 1)

  if (k < 2) {
    stop("At least two studies are required to produce a meta-analytic plot.")
  }

  #compute meta-analytic weights
  if (is.null(w_j)) {
    MA_data$w_j <- weights(method_obj)
  } else {
    if (is.vector(w_j)) {
      MA_data$w_j <- w_j
    } else if (is.matrix(w_j)) {
      MA_data$w_j <- diag(w_j)
    } else {
      stop(
        "Weights is neither a vector or matrix. Weights must be in vector or matrix form"
      )
    }
  }

  MA_data <- MA_data |>
    mutate(w_j_perc = w_j / sum(w_j))

  #store maximum (absolute) effect size
  #to be used for plot scaling purposes
  max_abs_es <- max(abs(MA_data$d_j))

  #compute summary effect size - fixed effects model
  if (is.null(summary_es)) {
    summary_es <- as.numeric(method_obj$b)
  } else {
    summary_es <- summary_es
  }
  #compute summary standard error - fixed effects model
  if (is.null(summary_se)) {
    summary_se <- as.numeric(method_obj$se)
  } else {
    summary_se <- summary_se
  }

  # compute lower and upper bounds of confidence interval at specified level
  critical_value <- qnorm((1 - confidence_level) / 2, lower.tail = FALSE)
  CIlb <- summary_es - critical_value * summary_se
  CIub <- summary_es + critical_value * summary_se

  # set study labels to be integers 1:k if not provided by user
  if (is.null(study_labels)) {
    study_labels <- c(seq(1:k))
  } else {
    study_labels <- c(study_labels)
  }

  if (!is.null(study_labels) && length(study_labels) != length(MA_data$d_j)) {
    stop("Length of study_labels does not match length of d_j.")
  }

  MA_data <- MA_data |>
    #create ID variable to be used as y-axis labels
    mutate(ID = factor(study_labels, ordered = TRUE))

  #randomly sort rows
  #useful in experimental setting so studies
  #don't appear in the same order for every vis
  #moved this from earlier on to later
  if (!is.null(seed)) {
    set.seed(seed)
    MA_data <- MA_data |>
      sample_n(size = k)
  }

  # set max x value for plotting purposes
  if (is.null(x_limits)) {
    #round max (abs) d value up to next tenth, then add .2 buffer (.1 for both sides)
    xmax <- (ceiling(max_abs_es * 10)) / 10 + .2
    #so can fit nicely with breaks of 0.2 increments
    xmax <- if_else((xmax * 10) %% 2 == 0, xmax, xmax + 0.1)
    xmin <- -xmax
    x_limits <- c(xmin, xmax)
  } else {
    xmin <- x_limits[1]
    xmax <- x_limits[2]
  }

  # set max y value for plotting purposes
  if (is.null(y_limits)) {
    #if max weight is above 0.4, then allow y-axis labels to increment by 0.15
    ymax <- if_else(
      max(MA_data$w_j_perc) > .4,
      ceiling(1.15 * max(MA_data$w_j_perc) * 100) / 100,
      #otherwise allow them to increment by 0.10
      ceiling(1.1 * max(MA_data$w_j_perc) * 100) / 100
    )
    #max weight rounded up to nearest hundredth
    ymax <- if_else((ymax * 100) %% 2 == 0, ymax, ymax + 0.01)
    ymin <- 0 - ymax
  } else {
    ymin <- y_limits[1]
    ymax <- y_limits[2]
  }

  # to enable shared layout between Full and Summary plots
  summary_text_size <- font_sizes[3] * 5 / 14
  label_text_size <- font_sizes[4] * 5 / 14
  top_margin <- 20
  right_margin <- 69
  bottom_margin <- 20
  left_margin <- 57

  # Validate that max_dot_size is positive
  if (max_dot_size <= 0) {
    stop("max_dot_size must be a positive number.")
  }

  if (textbox_width <= 0) {
    stop("textbox_width must be a positive number.")
  }

  summary_data <- data.frame(d_j = summary_es, se_j = summary_se)
  ##### CREATE PLOT BASE ######
  base <- ggplot2::ggplot(MA_data) +
    # remove axes and superfluous grids
    ggplot2::theme_light(base_family = font_type, base_line_size = .1) +
    ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = font_sizes[8]),
      axis.text.y = ggplot2::element_text(size = font_sizes[9]),
      axis.title = ggplot2::element_text(size = font_sizes[8]),
      panel.grid.minor.y = ggplot2::element_blank(),
      legend.position = "none",
      plot.caption = ggplot2::element_text(
        size = 10,
        face = "italic",
        color = "grey",
        hjust = 0
      )
    ) +
    # create red/blue shading to distinguish negative/positive SMD regions
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

  ##### CREATES "bottom" PLOT###### (that displays effect sizes)
  bottom <- base +
    # create labels to aid in interpretation of x-axis (SMD)
    ggplot2::annotate(
      "label",
      label = "Decreased scores (SMD < 0)",
      x = xmin * 0.5,
      y = -ymax * .05,
      size = font_sizes[4] * 5 / 14,
      family = font_type
    ) +
    ggplot2::annotate(
      "label",
      label = "Increased scores (SMD > 0)",
      x = xmax * .5,
      y = -ymax * 0.05,
      size = font_sizes[4] * 5 / 14,
      family = font_type
    ) +
    # add annotation to aid in interpretation of y-axis (meta-analytic weight)
    ggplot2::annotate(
      "text",
      label = "More certain",
      x = xmin * .85,
      y = ymax * .65,
      alpha = 0.5,
      size = font_sizes[5] * 5 / 14,
      family = font_type
    ) +
    ggplot2::annotate(
      "text",
      label = "Less certain",
      x = xmin * .85,
      y = ymax * .35,
      alpha = 0.5,
      size = font_sizes[5] * 5 / 14,
      family = font_type
    ) +
    ggplot2::annotate(
      "segment",
      x = xmin * .85,
      xend = xmin * .85,
      y = ymax * .6,
      yend = ymax * .4,
      alpha = 0.4,
      arrow = ggplot2::arrow(
        length = grid::unit(2, "mm"),
        type = "closed",
        ends = "both"
      )
    ) +
    # add effect size dots
    ggplot2::geom_point(
      data = MA_data,
      ggplot2::aes(x = d_j, y = w_j_perc, size = w_j_perc),
      color = dot_color,
      alpha = dot_trans
    ) +
    # horizontal line to make x-axis more prominent
    ggplot2::geom_hline(yintercept = 0) +
    # set maximum dot size for effect size dots
    ggplot2::scale_size_area(
      name = "Weight",
      max_size = max_dot_size,
      guide = ggplot2::guide_legend(reverse = TRUE)
    ) +
    # set y-axis breaks to have five 2-decimal numbers labeled from 0 to ~ymax
    ggplot2::scale_y_continuous(
      "Weight",
      limits = c(-ymax * .1, ymax),
      breaks = seq(
        0,
        ymax,
        if_else(
          ceiling(ymax / 5 * 100) %% 2 == 0,
          ceiling(ymax / 5 * 100) / 100,
          (ceiling(ymax / 5 * 100) + 1) / 100
        )
      )
    ) +
    # set x-axis breaks to have 1-decimal numbers in 0.2 increments from xmin to xmax
    ggplot2::scale_x_continuous(
      "Standardized Mean Difference (SMD)",
      limits = c(xmin, xmax),
      breaks = round(seq(xmin, xmax, xinc), 1)
    ) +
    # set x-axis label
    ggplot2::xlab("Standardized Mean Difference (SMD)")

  # ggplot2::xlab("Standardized Mean Difference (SMD)")
  # ggplot2::xlab("Standardized Mean Difference (SMD)")
  # ggplot2::xlab("Standardized Mean Difference (SMD)")

  #### IF ADDING STUDY DOT ANNOTATION
  if (show_dot_annotation == TRUE) {
    max_point <- MA_data[which.max(MA_data$w_j_perc), ]
    bottom <- bottom +
      ggplot2::annotate(
        "text",
        x = max_point$d_j + 0.05,
        y = max_point$w_j_perc - 0.02,
        label = "Each dot summarizes evidence \nfrom 1 research study",
        size = font_sizes[3] * 5 / 14,
        hjust = 0
      ) +
      ggplot2::annotate(
        "curve",
        x = max_point$d_j,
        y = max_point$w_j_perc,
        xend = max_point$d_j + 0.05,
        yend = max_point$w_j_perc - 0.02,
        curvature = 0.3,
        color = "grey80",
        arrow = grid::arrow(length = grid::unit(0.1, "inches"))
      )
  }

  #### IF DISPLAYING STUDY LABELS: DODGE BY DOT RADIUS ###
  if (show_study_labels == TRUE) {
    # Extract meta-data from "bottom" ggplot object
    # to get value of size aesthetic for each dot
    pb <- ggplot_build(bottom)
    point_data <- pb$data[[9]]
    # Get panel scales to extract x and y ranges
    panel_params <- pb$layout$panel_params[[1]]
    # x and y ranges
    x_range <- diff(panel_params$x.range)
    y_range <- diff(panel_params$y.range)
    # convert plot dimensions from inches to mm
    plot_width_mm <- width_in * 25.4
    plot_height_mm <- height_in * 25.4
    # compute data units per mm
    x_units_per_mm <- x_range / plot_width_mm
    y_units_per_mm <- y_range / plot_height_mm

    MA_data <- MA_data |>
      mutate(
        diameter = point_data$size, #in mm
        radius = diameter / 2, #in mm
        # convert radius from mm to data units
        radius_x_units = radius * x_units_per_mm,
        radius_y_units = radius * y_units_per_mm
      )

    # Add study labels, offset in x direction by radius of dot
    # then repelled in y-direction only to avoid overlap
    bottom <- bottom +
      geom_text_repel(
        data = MA_data,
        ggplot2::aes(
          x = d_j + radius_x_units,
          y = w_j_perc,
          label = paste("Study ", ID)
        ),
        direction = "y",
        size = 2
      )
  }

  ##### CREATE "top" PLOT (that displays summary) ######
  if (summary_only == FALSE) {
    top <- base +
      # boundaries of white rectangle for summary display
      ggplot2::annotate(
        "rect",
        xmin = x_limits[1],
        xmax = x_limits[2],
        ymin = y_limits_rect[1],
        ymax = y_limits_rect[2],
        alpha = .9,
        fill = "white"
      ) +
      # add annotation for Average SMD and # of studies
      ggplot2::annotate(
        "text",
        x = xmin * .95,
        y = 1,
        label = paste(
          "Average SMD: ",
          round(summary_es, digits),
          "\n# of Studies: ",
          k
        ),
        hjust = 0,
        size = font_sizes[2] * 5 / 14,
        family = font_type
      ) +
      # add SUMMARY OF THE EVIDENCE annotation
      ggplot2::annotate(
        "text",
        x = x_limits[1],
        y = y_limits_rect[2] * 1.1,
        hjust = 0,
        label = paste0("SUMMARY OF THE EVIDENCE:"),
        size = font_sizes[1] * 5 / 14,
        family = font_type
      ) +

      ggdist::stat_dots(
        data = tibble::tibble(
          .dist = distributional::dist_normal(summary_es, summary_se)
        ),
        ggplot2::aes(y = 1, xdist = .dist),
        side = "both",
        scale = 0.4
      ) +
      # add the navy blue summary dot
      ggplot2::geom_point(
        data = summary_data,
        ggplot2::aes(x = d_j, y = 1),
        size = 5,
        color = dot_color
      ) +
      # add the explanatory annotation for interpreting the summary
      geom_textbox(
        x = summary_data$d_j,
        y = y_limits_rect[1] * .96,
        label = paste0(
          "The center blue dot represents our best estimate
                      of the true SMD for this curriculum, based on existing evidence from ",
          k,
          " studies. The grey dots represent our uncertainty in that estimate;
                      95 times out of 100, the SMD for this curriculum is between ",
          round(CIlb, 2),
          " and ",
          round(CIub, 2),
          "."
        ),
        alpha = 0.5,
        size = font_sizes[3] * 5 / 14,
        family = font_type,
        vjust = 1,
        box.color = NA,
        fill = "white",
        width = ggplot2::unit(textbox_width, "inches")
      ) +
      ggplot2::theme_void() +
      ggplot2::guides(size = "none") +
      ggplot2::xlim(xmin, xmax)
    ### COMBINE TOP AND BOTTOM PLOTS
    p <- cowplot::plot_grid(
      top,
      bottom,
      ncol = 1,
      align = "v",
      axis = "lr",
      rel_heights = c(0.4, 0.6)
    )
  } else {
    top <- ggplot(MA_data) +
      # remove axes and superfluous grids
      theme_light(base_line_size = .1, base_family = font_type) +
      #create red/blue shading to distinguish negative/positive SMD regions
      geom_vline(xintercept = 0, alpha = 0.3) +
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
      ) +
      # add annotation for Average SMD and # of studies
      ggplot2::annotate(
        "text",
        x = xmin * .95,
        y = 1,
        label = paste(
          "Average SMD: ",
          round(summary_es, digits),
          "\n# of Studies: ",
          k
        ),
        hjust = 0,
        size = font_sizes[2] * 5 / 14,
        family = font_type
      ) +
      labs(title = "SUMMARY OF THE EVIDENCE") +
      # add the cloud dots
      ggdist::stat_dots(
        data = tibble::tibble(
          .dist = distributional::dist_normal(summary_es, summary_se)
        ),
        ggplot2::aes(y = 1, xdist = .dist),
        side = "both",
        scale = 0.4
      ) +
      # add the navy blue summary dot
      ggplot2::geom_point(
        data = summary_data,
        ggplot2::aes(x = d_j, y = 1),
        size = 5,
        color = dot_color
      ) +
      # add the explanatory annotation for interpreting the summary
      geom_textbox(
        x = summary_data$d_j,
        y = y_limits_rect[2] * 1.25,
        label = paste0(
          "The center blue dot represents our best estimate
                      of the true SMD for this curriculum, based on existing evidence from ",
          k,
          " studies. The grey dots represent our uncertainty in that estimate;
                      95 times out of 100, the SMD for this curriculum is between ",
          round(CIlb, 2),
          " and ",
          round(CIub, 2),
          "."
        ),
        alpha = 0.5,
        size = font_sizes[3] * 5 / 14,
        vjust = 1,
        box.color = NA,
        fill = "white",
        width = ggplot2::unit(textbox_width, "inches")
      ) +
      scale_x_continuous(
        "Standardized Mean Difference (SMD)",
        limits = c(xmin, xmax),
        breaks = round(seq(xmin, xmax, 0.2), 1)
      ) +
      # set x-axis label
      xlab("Standardized Mean Difference (SMD)") +
      # create labels to aid in interpretation of x-axis (SMD)
      ggplot2::annotate(
        "label",
        label = "Decreased scores (SMD < 0)",
        vjust = 1,
        hjust = 1,
        label.size = 0,
        x = -0.1,
        y = 0.6,
        size = font_sizes[4] * 5 / 14
      ) +
      ggplot2::annotate(
        "label",
        label = "Increased scores (SMD > 0)",
        vjust = 1,
        hjust = 0,
        label.size = 0,
        x = 0.1,
        y = 0.6,
        size = font_sizes[4] * 5 / 14
      ) +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.margin = margin(
          t = top_margin,
          r = right_margin,
          b = bottom_margin,
          l = left_margin
        )
      )
    p <- top
  }

  return(p)
}
