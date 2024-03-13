# This file is part of MARCviz, which is a free R package: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
#' @title viz_MARC
#'
#' @description Produces an interactive Meta-Analytic Rain Cloud (MARC) plot
#'
#' @param d_j vector of effect size estimates
#' @param se_j vector of standard errors of effect size estimates
#' @param type type of graph, "interactive" (built by plotly) or "static" (built by ggplot)
#' @param confidence_level confidence level for interval written in plot annotation (default = 0.95)
#' @param study_labels vector of study labels (optional)
#' @param seed integer used to set random seed before randomizing rows (optional)
#' @param x_limits vector of x-axis limits c(xmin, xmax) to be used in plotting (optional)
#' @param y_limits vector of y-axis limits c(ymin, ymax) to be used in plotting (optional)
#' @param noise_height controls how much the cloud of points varies vertically above/below y = 1 (default = 0.18, to stay within rectangle of height -/+0.2)
#' @param n_dots_in_cloud number of random dots to generate for the cloud of points at top (default = 250)
#' @param legend_data 3x3 data.frame of vectors y, x, w_j_perc that give y-coordinates, x-coordinates, and weights to be plotted manually in the legend. Default is legend_data <- data.frame(y = c(ymax*.55, ymax*.45, ymax*.35), x = xmin*0.65, w_j_perc = c(.1, .01, .001))
#' @param sizeref controls plotly argument that scales dots. (default set by sizeref <- 2.0 / (max_marker_size**2) as recommended by https://plotly.com/r/bubble-charts/)
#' @param max_marker_size maximum marker size, used in calculating sizeref if not specified separately (default = 50)
#' @param width controls figure width, passed to plot_ly() argument (default = 650)
#' @param height controls figure height, passed to plot_ly() argument (default = 425)
#' @param font_sizes vector of 7 font sizes for annotations, in order: SUMMARY OF EVIDENCE, Average SMD & Weight, CI annotation, Increased/Decreased Scores, More/Less certain, Legend number labels, Legend Weight title (default = c(14, 10, 8, 9, 8, 7, 10))
#' @param digits number of digits to round summary effect size and confidence interval bounds to (default = 2)
#'
#' @return a plotly object (if type = "interactive") or ggplot object (if type = "static")
#' @examples
#' library(tidyverse)
#' data(viz_MA_data)
#' d_j <- viz_MA_data %>% filter(k == 100) %>% pull(d_j)
#' se_j <- viz_MA_data %>% filter(k == 100) %>% pull(se_j)
#' viz_MARC(d_j, se_j, seed = 437)
#' viz_MARC(d_j, se_j, seed = 437, type = "static")
#' @export
#' @importFrom dplyr "%>%"
#' @importFrom dplyr "sample_n"
#' @importFrom dplyr "mutate"
#' @importFrom metafor "rma.uni"
#' @importFrom stats "qnorm"
#' @importFrom stats "rnorm"
#' @importFrom stats "dnorm"
#' @importFrom tibble "add_row"
#' @importFrom dplyr "case_when"
#' @importFrom dplyr "filter"
#' @importFrom dplyr "if_else"
#' @importFrom plotly "plot_ly"
#' @importFrom plotly "add_annotations"
#' @importFrom plotly "add_trace"
#' @importFrom plotly "layout"
#' @importFrom plotly "subplot"
#' @importFrom plotly "config"
#' @importFrom dplyr "pull"
#' @importFrom dplyr "slice"
#' @importFrom cowplot "plot_grid"
#' @importFrom ggplot2 "ggplot"
#' @importFrom ggplot2 "theme_light"
#' @importFrom ggplot2 "theme"
#' @importFrom ggplot2 "theme_void"
#' @importFrom ggplot2 "geom_vline"
#' @importFrom ggplot2 "geom_hline"
#' @importFrom ggplot2 "geom_point"
#' @importFrom ggplot2 "scale_size_area"
#' @importFrom ggplot2 "scale_y_continuous"
#' @importFrom ggplot2 "scale_x_continuous"
#' @importFrom ggplot2 "scale_fill_gradient"
#' @importFrom ggplot2 "geom_curve"
#' @importFrom ggplot2 "guides"
#' @importFrom ggplot2 "xlim"
#' @importFrom ggplot2 "ylim"

viz_MARC <- function(d_j = NULL,
                     se_j = NULL,
                     type = "interactive",
                     confidence_level = 0.95,
                     study_labels = NULL,
                     seed = NULL,
                     x_limits = NULL,
                     y_limits = NULL,
                     noise_height = 0.18,
                     n_dots_in_cloud = 250,
                     legend_data = NULL,
                     sizeref = NULL,
                     width = 650,
                     height = 425,
                     font_sizes = c(14, 10, 8, 9, 8, 7, 10),
                     digits = 2,
                     max_marker_size = 50
                     ){

  # create MA_data from d_j and se_j inputs
  MA_data <- data.frame(d_j, se_j)
  #specify # of studies k
  #NOTE TO SELF: For future CRAN package, will need to update this to be flexible for
  #studies w/ multiple effect sizes
  k <- dim(MA_data)[1]
  stopifnot(k > 1)

  #randomly sort rows
  #useful in experimental setting so studies
  #don't appear in the same order for every vis
  if(!is.null(seed)){
    set.seed(seed)
    MA_data <- MA_data %>%
      sample_n(size = k)
  }

  #compute fixed-effects meta-analytic weights
  MA_data <- MA_data %>%
    mutate(w_j = 1/(se_j^2),
           w_j_perc = w_j/sum(w_j))

  #store maximum (absolute) effect size
  #to be used for plot scaling purposes
  max_abs_es <- max(abs(MA_data$d_j))

  #compute summary effect size - fixed effects model
  summary_es <- as.numeric(rma.uni(yi = MA_data$d_j,
                                   sei = MA_data$se_j,
                                   method = "FE")$b)
  summary_se <- as.numeric(rma.uni(yi = MA_data$d_j,
                                   sei = MA_data$se_j,
                                   method = "FE")$se)

  # compute lower and upper bounds of confidence interval at specified level
  critical_value <- qnorm((1 - confidence_level)/2, lower.tail = FALSE)
  CIlb <- summary_es - critical_value*summary_se
  CIub <- summary_es + critical_value*summary_se



  if(is.null(study_labels)){
    study_labels <- c(seq(1:k))
  } else{
    study_labels <- c(study_labels)
  }
  MA_data <- MA_data %>%
    #create ID variable to be used as y-axis labels
    mutate(ID = factor(study_labels, ordered = TRUE))

  # generate data for cloud
  if(!is.null(seed)){
    set.seed(seed)
  }
  cloud_data <- data.frame(d = rnorm(n_dots_in_cloud, summary_es, summary_se))

  #rectangle extends 0.2 above and below y = 1
  noise_height <- noise_height
  cloud_data <- cloud_data %>%
    mutate(density = dnorm(d, summary_es, summary_se),
           #rectangle extends 0.2 above and below y = 1
           #scale density to set as s.d. of noise, ensures
           #noise that falls 3 sds away from 0 will still fall in rectangle
           density_scaled = density*noise_height/(3*max(density)),
           noise = rnorm(n_dots_in_cloud, 0, density_scaled))
  cloud_sample <- cloud_data %>%
    # truncate noise for aesthetic purposes so it fits within summary bar
    mutate(noise = case_when(noise > noise_height ~ noise_height,
                             noise < -noise_height ~ -noise_height,
                             TRUE ~ noise))

  # choose dot with minimum y value to add CI annotation to
  min <- cloud_sample %>%
    filter(d < summary_es) %>%
    filter(noise == min(noise))

  # set max x value for plotting purposes
  if(is.null(x_limits)){
    #round max (abs) d value up to next tenth, then add .2 buffer (.1 for both sides)
    xmax <- (ceiling(max(abs(cloud_sample$d), max_abs_es)*10))/10 + .2
    #so can fit nicely with breaks of 0.2 increments
    xmax <- if_else((xmax*10) %% 2 == 0, xmax, xmax + 0.1)
    xmin <- -xmax
  } else{
    xmin <- x_limits[1]
    xmax <- x_limits[2]
  }

  # set max y value for plotting purposes
  if(is.null(y_limits)){
    ymax <- if_else(max(MA_data$w_j_perc) > .4,
                  ceiling(1.15*max(MA_data$w_j_perc)*100)/100,
                  ceiling(1.1*max(MA_data$w_j_perc)*100)/100)
    #max weight rounded up to nearest hundredth
    ymax <- if_else((ymax*100) %% 2 == 0, ymax, ymax + 0.01)
    ymin <- 0 - ymax
  } else{
    ymin <- y_limits[1]
    ymax <- y_limits[2]
  }

  ################# INTERACTIVE (PLOTLY) VERSION ###################
  if(type == "interactive"){
    #create coordinates for legend (added as manual annotations)
    if(is.null(legend_data)){
      #find largest weight to place on legend based on ymax
      max_w_j_legend <- data.frame(w_j_perc = c(0.1, 0.01, 0.001, 0.0001)) %>%
        mutate(keep = ymax >= w_j_perc) %>%
        filter(keep == TRUE) %>%
        slice(1) %>%
        pull(w_j_perc)
      #create coordinates and dot size for 3 dots in legend
      #y-coordinates placed at 55%, 45%, and 35% of ymax value
      #x-coordinates plaed at 65% of xmin value
      legend_data <- data.frame(y = c(ymax*.55, ymax*.45, ymax*.35),
                                x = xmin*0.65,
                                w_j_perc = c(max_w_j_legend,
                                             max_w_j_legend/10,
                                             max_w_j_legend/100))
    } else{
      legend_data <- legend_data
    }

    #recommendation from https://plotly.com/r/bubble-charts/
    if(is.null(sizeref)){
      #sizeref <- 2.0 * max(MA_data$w_j_perc) / (max_marker_size**2)
      #to size with summary effect, max w_j_perc is 1
      sizeref <- 2.0 / (max_marker_size**2)
    } else{
      sizeref <- sizeref
    }

    ## TOP PANE OF VISUALIZATION (w/ Summary evidence + annotations)
    viz_top <- plot_ly(type = "scatter", mode = "markers", fill = "",
                       width = width, height = height)  %>%
      #create annotations for top panel of summary evidence
      # Summary of evidence annotation
      add_annotations(text = "SUMMARY OF THE EVIDENCE:",
                      x = xmin*.95 + abs(min(cloud_sample$d) - xmin)/2, y = 1,
                      showarrow = FALSE, xanchor = "center",
                      font = list(size = font_sizes[1]),
                      layer = 'above') %>%
      # Average SMD annotation
      add_annotations(text = paste("Average SMD: ", round(summary_es, digits),
                                   "\nWeight: 1.0",
                                   "\n# of Studies: ", k),
                      x = max(cloud_sample$d) + 0.05, y = 1,
                      showarrow = FALSE, xanchor = "left", align = "left",
                      font = list(size = font_sizes[2]),
                      layer = 'above') %>%
      # Interpretation of CI annotation
      add_annotations(text = paste("One out of 10,000+ possible values for the true SMD, \nbased on the existing evidence.\n95% of values fall between ",
                                   round(CIlb,  digits), " and ", round(CIub, digits),
                                   "\nwith an average value of ",  round(summary_es, digits), ".", sep = ""),
                      #x = .45, y = 0.85,
                      x = summary_es, y = 0.75,
                      showarrow = FALSE, xanchor = "left", yanchor = "top",
                      font = list(size = font_sizes[3]),
                      layer = 'above', align = "left") %>%
      # arrow from individual dot to annotation text
      add_annotations(text = "",
                      ax = min$d, ay = (1 + min$noise),
                      x = summary_es, y = 0.75,
                      xref = 'x', yref = 'y', axref = 'x', ayref = 'y',
                      showarrow = TRUE, arrowhead = 3, arrowsize = 1,
                      arrowwidth = 1, arrowcolor = 'lightblue') %>%
      # summary dot
      add_trace(type = "scatter", x = summary_es, y = 1,
                marker = list(sizeref = .1, sizemode = 'area', size = max_marker_size,
                              color = 'navy',
                              line = list(color = "navy")),
                showlegend = FALSE,
                hovertemplate = paste('<b>SMD</b>: %{x}<br>',
                                      '<b>Weight </b>: %{y}</b>',
                                      '<extra></extra>')) %>%
      # add cloud of points
      add_trace(type = "scatter",
                mode = "markers", data = cloud_sample, x = ~d,
                y = (1 + cloud_sample$noise),
                hoverinfo = "none",
                marker = list(sizemode = "area", color = 'white',
                              line = list(color = "lightblue", width = 0.5),
                              size = 2.5),
                showlegend = FALSE) %>%
      # red rectangle for negative SMD, white for outline of Summary of evidence
      layout(plot_bgcolor = "#e5ecf6",
             shapes = list(list(type = "rect",
                                fillcolor = "red",
                                line = list(color = "red"),
                                opacity = 0.2,
                                y0 = 0.4, y1 = 1.3, #y1 = 1.15,
                                x0 = xmin, x1 = 0,
                                layer = 'below'),
                           list(type = "rect",
                                fillcolor = "white",
                                line = list(color = "white"),
                                opacity = 1,
                                y0 = .8, y1 = 1.2,
                                x0 = xmin*.9, x1 = xmax*.9,
                                layer = 'below')),
             xaxis = list(title = "",
                          range = c(xmin, xmax),
                          dtick = 0.2,
                          tick0 = -1,
                          tickmode = "linear",
                          showgrid = FALSE,
                          showticklabels = FALSE,
                          zeroline = F),
             yaxis = list(title = "", #range = c(0.85,1.15),
                          dtick = 0.1,
                          tick0 = 1,
                          tickmode = "linear",
                          showgrid = FALSE,
                          showticklabels = FALSE,
                          zeroline = F))

    ## BOTTOM PANE OF VISUALIZATION (with study data)
    viz_bottom <- plot_ly(type = "scatter", mode = "markers", fill = "",
                          width = 650, height = 425) %>%
      # Increase / Decrease annotations
      add_annotations(text = "Decreased Scores (SMD < 0)",
                      x = xmin*0.5, y = -ymax*.05,
                      showarrow = FALSE, xanchor = "center",
                      font = list(size = font_sizes[4]),
                      layer = 'above') %>%
      add_annotations(text = "Increased Scores (SMD > 0)",
                      #x = 0.65, y = 0.1,
                      x = xmax*0.5, y = -ymax*.05,
                      showarrow = FALSE, xanchor = "center",
                      font = list(size = font_sizes[4]),
                      layer = 'above') %>%

      # More / Less certain annotations
      add_annotations(text = "More certain",
                      x = xmin*.85, y = ymax*.65,
                      yref = "y2",
                      showarrow = FALSE, xanchor = "center",
                      font = list(size = font_sizes[5]),
                      layer = 'below') %>%
      add_annotations(text = "Less certain",
                      #x = -0.5, y = 0.4,
                      x = xmin*0.85, y = ymax*.35,
                      yref = "y2",
                      showarrow = FALSE, xanchor = "center",
                      font = list(size = font_sizes[5]),
                      layer = 'above') %>%
      # arrows between More / Less Certain annotations
      add_annotations(text = "",
                      x = xmin*0.85, y = ymax*0.37,
                      ax = xmin*0.85, ay = ymax/2,
                      xref = 'x2', axref = 'x2',
                      yref = 'y2', ayref = 'y2',
                      showarrow = TRUE, arrowhead = 3, arrowsize = 1,
                      arrowwidth = 1, arrowcolor = 'black') %>%
      add_annotations(text = "",
                      x = xmin*0.85, y = ymax*.63,
                      ax = xmin*0.85, ay = ymax/2,
                      xref = 'x2', axref = 'x2',
                      yref = 'y2', ayref = 'y2',
                      showarrow = TRUE, arrowhead = 3, arrowsize = 1,
                      arrowwidth = 1, arrowcolor = 'black') %>%
      # study data
      add_trace(data = MA_data,
                x = ~d_j, y = ~w_j_perc, text = ~ID, size = ~w_j_perc,
                marker = list(size = ~w_j_perc,
                              sizemode = "area", color = 'navy',
                              line = list(color = "navy"),
                              sizeref = sizeref),
                hovertemplate = paste('<i>Study ID</i>: %{text}',
                                      '<br><b>SMD</b>: %{x}<br>',
                                      '<b>Weight </b>: %{y}</b>',
                                      '<extra></extra>'),
                showlegend = FALSE#,
      ) %>%
      # legend data - dots
      add_trace(data = legend_data,
                x = ~x, y = ~y, size = ~w_j_perc,
                marker = list(size = ~w_j_perc,
                              sizemode = "area", color = 'navy',
                              line = list(color = "navy"),
                              sizeref = sizeref),
                showlegend = FALSE,
                hoverinfo = "none"
      ) %>%
      # legend data - text
      add_annotations(data = legend_data, text = ~w_j_perc,
                      x = ~x + .05, y = ~y,
                      showarrow = FALSE, xanchor = "left",
                      yref = "y2", xref = "x2",
                      font = list(size = font_sizes[6]),
                      layer = 'above') %>%
      add_annotations(text = "Weight", y = ymax*0.65, x = xmin*0.65,
                      font = list(size = font_sizes[7]), yref = "y2", xref = "x2",
                      showarrow = FALSE, xanchor = "center",
                      layer = "above") %>%
      # red / white rectangles
      layout(plot_bgcolor = "#e5ecf6",
             shapes = list(
               # red for negative SMD region
               list(type = "rect",
                    fillcolor = "red",
                    line = list(color = "red"),
                    opacity = 0.2,
                    y0 = -ymax*.1, y1 = ymax, #y1 = 1.15,
                    x0 = xmin, x1 = 0,
                    layer = 'below'),
               # small white rectangles for Increase / Decrease annotations
               list(type = "rect",
                    fillcolor = "white",
                    line = list(color = "black", width = 0.5),
                    opacity = 1,
                    y0 = -ymax*0.08, y1 = -ymax*0.02,
                    x0 = xmin*.75, x1 = xmin*.25,
                    layer = 'below'),
               list(type = "rect",
                    fillcolor = "white",
                    line = list(color = "black", width = .5),
                    opacity = 1,
                    y0 = -ymax*0.08, y1 = -ymax*0.02,
                    x0 = xmax*0.25, x1 = xmax*0.75,
                    layer = 'below'),
               list(type = "rect",
                    fillcolor = "white",
                    line = list(color = "black", width = 0.5),
                    opacity = 0.5,
                    y0 = ymax*0.3, y1 = ymax*.7,
                    x0 = xmin*0.95, x1 = xmin*0.5,
                    layer = 'below')
             ),
             # xaxis specifications
             xaxis = list(title = "Standardized Mean Difference (SMD)",
                          range = c(xmin, xmax),
                          dtick = 0.2,
                          tick0 = -1,
                          tickmode = "linear",
                          title = "Standardized Mean Difference (SMD)",
                          showgrid = FALSE,
                          gridcolor = '#ececec',
                          gridwidth = .10,
                          hoverformat = '.3f'
             ),
             # yaxis specifications
             yaxis = list(title = "Weight",
                          range = c(-ymax*.1, ymax),
                          dtick = if_else(ceiling(ymax/5*100) %% 2 == 0,
                                          ceiling(ymax/5*100)/100,
                                          (ceiling(ymax/5*100) + 1)/100),
                          tick0 = 0,
                          tickmode = "linear",
                          gridcolor = '#ececec',
                          gridwidth = .10,
                          showgrid = FALSE,
                          hoverformat = '.4f'))

    #combine top and bottom panes
    plotly_viz <- subplot(viz_top, viz_bottom, nrows = 2,
                          titleX = TRUE, titleY = TRUE, shareX = FALSE,
                          heights = c(0.4,0.6),
                          which_layout = 2)
    p <- plotly_viz %>% config(displayModeBar = FALSE)
  }

  ################# STATIC (GGPLOT)) VERSION ###################
  if(type == "static"){
    summary_data <- data.frame(d_j = summary_es,
                               se_j = summary_se)
    # create plot base
    base <- ggplot(MA_data) +
      # remove axes and superfluous grids
      theme_light(base_line_size = .1) +
      theme(axis.ticks.y = element_blank(),
            axis.line = element_blank(),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16),
            axis.title=element_text(size = 16),
            panel.grid.minor.y = element_blank(),
            legend.position = c(),
            plot.caption = element_text(size = 10, face = "italic", color = "grey", hjust = 0)) +
      #create shading to distinguish positive and negative SMD regions
      geom_vline(xintercept = 0, alpha = 0.3, linewidth = 2) +
      annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
               alpha = .1, fill = "red") +
      annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
               alpha = .1, fill = "blue")
      bottom <- base +
        # create labels to aid in interpretation of x-axis (SMD)
        annotate("label", label = "Decreased scores (SMD < 0)",
                 x = xmin*0.5, y = -ymax*.05, size = 3) +
        annotate("label", label = "Increased scores (SMD > 0)",
               x = xmax*.5, y = -ymax*0.05, size = 3) +

       # add annotation to aid in interpretation of y-axis (meta-analytic weight)
        annotate("text", label = "More certain",
                 x = xmin*.85, y = ymax*.65, alpha = 0.5, size = 3) +
        annotate("text", label = "Less certain",
                 x = xmin*.85, y = ymax*.35, alpha = 0.5, size = 3) +
        annotate("segment", x = xmin*.85, xend = xmin*.85,
                y = ymax*.6, yend = ymax*.4, alpha = 0.4,
                arrow = arrow(length = unit(2, "mm"),
                             type = "closed", ends = "both")) +
        geom_point(data = MA_data, aes(x = d_j, y = w_j_perc, size = w_j_perc),
                   color = "navyblue") +
        geom_hline(yintercept = 0) +
        scale_size_area(name = "Weight", max_size = 10,
                        guide = guide_legend(reverse = TRUE)) +
        scale_y_continuous("Weight", limits = c(-ymax*.1, ymax),
                           breaks = seq(0,
                                              ymax,
                                              if_else(ceiling(ymax/5*100) %% 2 == 0,
                                                      ceiling(ymax/5*100)/100,
                                                      (ceiling(ymax/5*100) + 1)/100)
                                              )
                           ) +
        scale_x_continuous("Standardized Mean Difference (SMD)",
                         limits = c(xmin, xmax), breaks = round(seq(xmin, xmax, 0.2),1)) +
        scale_fill_gradient(guide = "none", low = "navyblue",
                          high = "white") +
        xlab("Standardized Mean Difference (SMD)")
    top <- base +
      annotate("rect", xmin = xmin*.9, xmax = xmax*.9,
               ymin = 0.8, ymax = 1.2,
               alpha = .9, fill = "white") +
      geom_point(data = cloud_sample,
                 aes(x = d, y = 1 + noise),
                 alpha = 0.2, size = 1,
                 color = "navyblue", shape = 21) +
      geom_point(data = summary_data, aes(x = d_j, y = 1, size = 1),
                 color = "navyblue", alpha = 0.8) +
      scale_size_area(name = "Weight", max_size = 10) +
      annotate("text", x = max(cloud_sample$d) + 0.05, y = 1,
               label = paste("Average SMD: ", round(summary_es, digits),
                             "\nWeight: 1.0",
                             "\n# of Studies: ", k),
               hjust = 0, size = 3) +
      annotate("text", x = summary_es, y = .75, size = 2.5, hjust = 0, vjust = 1,
               label = paste0("1 of 10,000+ possible values of the true SMD, \nbased on the existing evidence.\n95% of values fall between ", round(CIlb, 2), " and ", round(CIub, 2),",\nwith an average value of ", round(summary_es, 2), ".")) +
      annotate("text", x = xmin*.95 + abs(min(cloud_sample$d) - xmin)/2, y = 1,
               label = paste0("SUMMARY OF THE EVIDENCE:"),
               size = 4) +
      annotate("curve", linewidth = .1,
               x = min$d, y = 1 + min$noise,
               xend = summary_es, yend = .75,
                 arrow = arrow(length = unit(0.01, "npc"))) +
      theme_void() +
      guides(size = "none") +
      xlim(xmin, xmax) +
      ylim(.6, 1.2)

    p <- cowplot::plot_grid(top, bottom, ncol = 1, align = "v",
                   axis = "lr",
                   rel_heights = c(0.4,0.6))

  }
  return(p)
}

