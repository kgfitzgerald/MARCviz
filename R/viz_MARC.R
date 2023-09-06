#' @title viz_MARC
#'
#' @description Produces an interactive Meta-Analytic Rain Cloud (MARC) plot
#'
#' @param d_j vector of effect size estimates
#' @param se_j vector of standard errors of effect size estimates
#' @param study_labels vector of study labels (optional)
#' @param seed integer used to set random seed before randomizing rows (optional)
#' @param x_limits vector of x-axis limits c(xmin, xmax) to be used in plotting (optional)
#' @param confidence_level confidence level for interval written in plot annotation (default = 0.95)
#'
#' @return a plotly object
#' @examples
#' data(viz_MA_data)
#' d_j <- viz_MA_data %>% filter(k == 100) %>% pull(d_j)
#' se_j <- viz_MA_data %>% filter(k == 100) %>% pull(se_j)
#' viz_MARC(d_j, se_j, seed = 437)
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

viz_MARC <- function(d_j = NULL, se_j = NULL, study_labels = NULL,
                     seed = NULL, x_limits = NULL,
                     confidence_level = 0.95){

  # create MA_data from d_j and se_j inputs
  MA_data <- data.frame(d_j, se_j)
  #specify # of studies k
  #NOTE TO SELF: For package, will need to update this to be flexible for
  #studies w/ multiple effect sizes
  k <- dim(MA_data)[1]

  #randomly sort rows
  #useful in experimental setting so studies
  #don't appear in the same order for every vis
  if(!is.null(seed)){
    set.seed(seed)
    MA_data <- MA_data %>%
      sample_n(size = k)
  }

  #compute meta-analytic weights
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

  # add row for summary data
  # set w_j_perc = 1 for summary since V(summary) = 1/sum(weights)
  MA_data <- MA_data %>%
    add_row(w_j_perc = 1,
            d_j = summary_es,
            w_j = sum(MA_data$w_j))

  if(is.null(study_labels)){
    #NOTE TO SELF: another place to make flexible in package for when # of es > k
    study_labels <- c(seq(1:k), "SUMMARY")
  } else{
    study_labels <- c(study_labels, "SUMMARY")
  }
  MA_data <- MA_data %>%
    #create ID variable to be used as y-axis labels
    mutate(ID = factor(study_labels, ordered = TRUE))

  # generate data for cloud
  if(!is.null(seed)){
    set.seed(seed)
  }
  cloud_data <- data.frame(d = rnorm(250, summary_es, summary_se))

  noise_height <- 0.18
  cloud_data <- cloud_data %>%
    mutate(density = dnorm(d, summary_es, summary_se),
           #rectangle extends 0.2 above and below y = 1
           #scale density to set as s.d. of noise, ensures
           #noise that falls 3 sds away from 0 will still fall in rectangle
           density_scaled = density*noise_height/(3*max(density)),
           noise = rnorm(250, 0, density_scaled))
  cloud_sample <- cloud_data %>%
    # truncate noise for aesthetic purposes so it fits within summary bar
    mutate(noise = case_when(noise > noise_height ~ noise_height,
                             noise < -noise_height ~ -noise_height,
                             TRUE ~ noise))

  # identify dot annotation should be added to
  min <- cloud_sample %>%
    filter(d < summary_es) %>%
    filter(noise == min(noise))

  # set min and max x values for plotting purposes
  if(is.null(x_limits)){
    #round max (abs) d value up to next tenth, then add .2 buffer (.1 for both sides)
    xmax <- (ceiling(max(abs(cloud_sample$d), max_abs_es)*10))/10 + .2
    #so can fit nicely with breaks of 0.2 increments
    xmax <- if_else((xmax*10) %% 2 == 0, xmax, xmax + 0.1)
  } else{
    xmax <- x_limits[2]
  }

  xmin <- -xmax


  ##### PLOTLY VERSION ######
  MA_data_plotly <- MA_data %>%
    filter(ID != "SUMMARY")

  ymax <- if_else(max(MA_data_plotly$w_j_perc) > .4,
                  ceiling(1.15*max(MA_data_plotly$w_j_perc)*100)/100,
                  ceiling(1.1*max(MA_data_plotly$w_j_perc)*100)/100)
  #max weight rounded up to nearest hundredth
  ymax <- if_else((ymax*100) %% 2 == 0, ymax, ymax + 0.01)
  ymin <- 0 - ymax

  legend_data <- data.frame(y = c(ymax*.55, ymax*.45, ymax*.35),
                            x = xmin*0.65,
                            w_j_perc = c(.1, .01, .001))

  sizeref <- 2.0 * max(MA_data$w_j_perc) / (50**2)

  viz_top <- plot_ly(type = "scatter", mode = "markers", fill = "",
                     width = 650, height = 425)  %>%
    add_annotations(text = "SUMMARY OF THE EVIDENCE:",
                    x = xmin*.95 + abs(min(cloud_sample$d) - xmin)/2, y = 1,
                    showarrow = FALSE, xanchor = "center",
                    font = list(size = 14),
                    layer = 'above') %>%
    add_annotations(text = paste("Average SMD: ", round(MA_data$d_j[k+1],2),
                                 "\nWeight: 1.0"),
                    x = max(cloud_sample$d) + 0.05, y = 1,
                    showarrow = FALSE, xanchor = "left", align = "left",
                    font = list(size = 10),
                    layer = 'above') %>%
    add_annotations(text = paste("One out of 10,000+ possible values for the true SMD, \nbased on the existing evidence.\n95% of values fall between ",
                                 round(CIlb, 2), " and ", round(CIub,2),
                                 "\nwith an average value of ",  round(summary_es,2), ".", sep = ""),
                    #x = .45, y = 0.85,
                    x = summary_es, y = 0.75,
                    showarrow = FALSE, xanchor = "left", yanchor = "top",
                    font = list(size = 8),
                    layer = 'above', align = "left") %>%
    add_annotations(text = "",
                    ax = min$d, ay = (1 + min$noise),
                    x = summary_es, y = 0.75,
                    xref = 'x', yref = 'y', axref = 'x', ayref = 'y',
                    showarrow = TRUE, arrowhead = 3, arrowsize = 1,
                    arrowwidth = 1, arrowcolor = 'lightblue') %>%
    add_trace(type = "scatter", x = summary_es, y = 1,
              marker = list(sizeref = .1, sizemode = 'area', size = 50,
                            color = 'navy',
                            line = list(color = "navy")),
              showlegend = FALSE,
              hovertemplate = paste('<b>SMD</b>: %{x}<br>',
                                    '<b>Weight </b>: %{y}</b>',
                                    '<extra></extra>')) %>%
    #trace 2
    add_trace(type = "scatter",
              mode = "markers", data = cloud_sample, x = ~d,
              y = (1 + cloud_sample$noise),
              hoverinfo = "none",
              marker = list(sizemode = "area", color = 'white',
                            line = list(color = "lightblue", width = 0.5),
                            size = 2.5),
              showlegend = FALSE) %>%
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
  #viz_top

  viz_bottom <- plot_ly(type = "scatter", mode = "markers", fill = "",
                        width = 650, height = 425) %>%
    add_annotations(text = "Decreased Scores (SMD < 0)",
                    x = xmin*0.5, y = -ymax*.05,
                    showarrow = FALSE, xanchor = "center",
                    font = list(size = 9),
                    layer = 'above') %>%
    add_annotations(text = "Increased Scores (SMD > 0)",
                    #x = 0.65, y = 0.1,
                    x = xmax*0.5, y = -ymax*.05,
                    showarrow = FALSE, xanchor = "center",
                    font = list(size = 9),
                    layer = 'above') %>%
    add_annotations(text = "More certain",
                    x = xmin*.85, y = ymax*.65,
                    yref = "y2",
                    showarrow = FALSE, xanchor = "center",
                    font = list(size = 8),
                    layer = 'below') %>%
    add_annotations(text = "Less certain",
                    #x = -0.5, y = 0.4,
                    x = xmin*0.85, y = ymax*.35,
                    yref = "y2",
                    showarrow = FALSE, xanchor = "center",
                    font = list(size = 8),
                    layer = 'above') %>%
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
    add_trace(data = MA_data_plotly,
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
    add_trace(data = legend_data,
              x = ~x, y = ~y, size = ~w_j_perc,
              marker = list(size = ~w_j_perc,
                            sizemode = "area", color = 'navy',
                            line = list(color = "navy"),
                            sizeref = sizeref),
              showlegend = FALSE,
              hoverinfo = "none"
    ) %>%
    add_annotations(data = legend_data, text = ~w_j_perc,
                    x = ~x + .05, y = ~y,
                    showarrow = FALSE, xanchor = "left",
                    yref = "y2", xref = "x2",
                    font = list(size = 7),
                    layer = 'above') %>%
    add_annotations(text = "Weight", y = ymax*0.65, x = xmin*0.65,
                    font = list(size = 10), yref = "y2", xref = "x2",
                    showarrow = FALSE, xanchor = "center",
                    layer = "above") %>%
    layout(plot_bgcolor = "#e5ecf6",
           shapes = list(
             list(type = "rect",
                  fillcolor = "red",
                  line = list(color = "red"),
                  opacity = 0.2,
                  y0 = -ymax*.1, y1 = ymax, #y1 = 1.15,
                  x0 = xmin, x1 = 0,
                  layer = 'below'),
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

  plotly_viz <- subplot(viz_top, viz_bottom, nrows = 2,
                        titleX = TRUE, titleY = TRUE, shareX = FALSE,
                        heights = c(0.4,0.6),
                        which_layout = 2)
  plotly_viz %>% config(displayModeBar = FALSE)

}
