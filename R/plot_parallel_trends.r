#' Plots parallel trends figures.
#'
#' The `plot_parallel_trends` function combines the trends_data.csv's
#' and plots parallel trends figures.
#'
#' @details All treatment and all control groups can be combined so that
#' there is one control line and one treatment line by setting combine = `TRUE`
#'
#' @param dir_path A string filepath to the folder containing all
#' of the trends_data.csv's
#' @param save_csv A logical value (defaults to `FALSE`)
#' indicating whether or not to save the combined_trends_data.csv
#' @param combine A logical value (defaults to `FALSE`) indicating whether
#' to plot each silo separately or to combine silos based on treatment status
#' @param covariates A logical value (defaults to `FALSE`)
#' indicating whether or not to consider covariates
#' @param pch An integer (from 0 to 25, defaults to `NA`)
#' or vector of integers (from 0 to 25) determines the points used on the plot
#' @param pch_control An integer (from 0 to 25, defaults to `NULL`)
#' or vector of integers (from 0 to 25) determines the points used on the plot
#' for control silos. Takes value of pch if NULL
#' @param pch_treated An integer (from 0 to 25, defaults to `NULL`) or vector
#' of integers (from 0 to 25) determines the points used on the plot for treated
#' silos. Takes value of pch if NULL
#' @param control_colour A vector of string colours
#' (defaults to `c("darkgrey", "lightgrey")`) option for the control silos.
#' If combine = TRUE, takes the 1st value to determine colour of control line
#' @param control_color Overrides `control_colour` if used
#' @param treatment_colour A vector of string colours
#' (defaults to `c("darkred", "lightcoral")`) for the treatment silos.
#' If combine = TRUE, takes the 1st value to determine colour of control line
#' @param treatment_color Overrides `treatment_colour` if used
#' @param lwd An integer (defaults to `2`) for selecting the line widths
#' @param xlab A string value for the x-axis label (defaults to `NA`)
#' @param ylab A string value for the y-axis label (defaults to `NA`)
#' @param title A string value for the title of the plot (defaults to `NA`)
#' @param xticks An integer value denoting how many ticks to display
#' on the x-axis (defaults to `4`)
#' @param xdates Takes in a vector of dates to be used at the dates shown
#' along the x-axis (defaults to `NULL`)
#' @param date_format A string value denoting the format with which to display
#' the dates along the x-axis (defaults to `"%Y"`)
#' @param xaxlabsz A double indicating the x-axis label sizes
#' (defaults to `0.8`)
#' @param save_png A logical value indicating whether or not to save the plot
#' as a png file (defaults to `FALSE`)
#' @param width An integer denoting the width of the saved .png file
#' @param height An integer denoting the height of the saved .png file
#' @param ylim A vector of two doubles defining the min and max range of the
#' values on the y-axis. Defaults to the min and max values in
#' the y column of trends_data
#' @param yaxlabsz A double for specifying the y-axis label sizes
#' (defaults to `0.8`)
#' @param ylabels A vector of values that you would like
#' to appear on the y-axis (defaults to `NULL`)
#' @param yticks An integer denoting how many values to display
#' along the y-axis (defaults to `4`)
#' @param ydecimal An integer value denoting to which decimal point
#' the values along the y-axis are rounded to
#' @param legend_location A string value for determining the location
#' of the legend (defaults to `topright`)
#' @param simplify_legend A logical value which if set to `TRUE` shows one
#' colour for the treatment silos in the legend and one colour for the control.
#' Defaults to `TRUE`
#' @param legend_text An double for setting the size of the text in the legend.
#' Defaults to `0.7`
#' @param legend_on A logical value for turning the legend on or off
#' (defaults to `TRUE`)
#' @param treatment_indicator_col A string value for determining the colour of
#' the dashed vertical line showing when treatment times were
#' (defaults to `"grey"`)
#' @param treatment_indicator_alpha A double for for determining the
#' transparency level of the dashed vertical lines showing the treatment
#' times (defaults to `0.5`)
#' @param treatment_indicator_lwd A double for for selecting the line width
#' of the treatment indicator lines (defaults to `2`)
#' @param treatment_indicator_lty An integer for the selecting the lty option
#' for the treatment_indicator lines (defaults to `2`)
#' @param interpolate A logical value (either `TRUE` or `FALSE`) which
#' determines if interpolation should be used to fill missing trends data.
#' Defaults to `FALSE`.
#' 
#' @examples
#' NULL
#'
#' @importFrom graphics axis legend lines abline
#' @export
#'
plot_parallel_trends <- function(dir_path, covariates = FALSE, save_csv = FALSE,
                                 combine = FALSE, pch = NA, pch_control = NULL,
                                 pch_treated = NULL,
                                 control_colour = c("darkgrey", "lightgrey"),
                                 control_color = NULL,
                                 treatment_colour = c("darkred", "lightcoral"),
                                 treatment_color = NULL,
                                 lwd = 2, xlab = NA, ylab = NA, title = NA,
                                 xticks = 4, date_format = "%Y", xdates = NULL,
                                 xaxlabsz = 0.8, save_png = FALSE, width = 800,
                                 height = 600, ylim = NULL, yaxlabsz = 0.8,
                                 ylabels = NULL, yticks = 4, ydecimal = 2,
                                 legend_location = "topright",
                                 simplify_legend = TRUE,
                                 legend_text = 0.7, legend_on = TRUE,
                                 treatment_indicator_col = "grey",
                                 treatment_indicator_alpha = 0.5,
                                 treatment_indicator_lwd = 2,
                                 treatment_indicator_lty = 2,
                                 interpolate = FALSE) {

  # First read in the trends data
  trends_data <- .combine_trends_data(dir_path, covariates, interpolate)


  # Set pch_control and pch_treated to pch if not selected
  if (is.null(pch_control)) {
    pch_control <- pch
  }
  if (is.null(pch_treated)) {
    pch_treated <- pch
  }

  # Override colours if color is used instead
  if (!is.null(control_color)) {
    control_colour <- control_color
  }
  if (!is.null(treatment_color)) {
    treatment_colour <- treatment_color
  }

  # Set xlabels as specified dates if xdates is specified. Otherwise use xticks
  if (!is.null(xdates)) {
    xdates <- as.Date(xdates)
    xlabels <- xdates
  } else {
    xlabels <- sort(unique(trends_data$time))
    xlabels <- xlabels[seq(1, length(xlabels), length.out = xticks)]
  }

  if (combine == TRUE) {
    if (any(is.na(trends_data$y))) {
      warning("`NA` values found, consider setting `interpolate = TRUE`.")
    }
    time <- c()
    silo_name <- c()
    y_vec <- c()
    treatment_times <- c()

    trends_data$string_date <- .parse_date_to_string(as.Date(trends_data$time),
                                                     trends_data$date_format[1])
    dates <- unique(trends_data$string_date)

    for (date in dates) {
      time <- c(time,
                rep(.parse_string_to_date(date, trends_data$date_format[1]), 2))

      silo_name <- c(silo_name, "Treatment")
      y_vec <- c(y_vec,
                 mean(unlist(trends_data[trends_data$string_date == date &
                                           trends_data$treatment_time !=
                                             "control", "y"]), na.rm = TRUE))

      silo_name <- c(silo_name, "Control")
      y_vec <- c(y_vec,
                 mean(unlist(trends_data[trends_data$string_date == date &
                                           trends_data$treatment_time ==
                                             "control", "y"]), na.rm = TRUE))

      if (nrow(trends_data[trends_data$treatment_time == date, ]) > 0) {
        treatment_times <- c(treatment_times,
                             rep(trends_data[trends_data$treatment_time == date,
                                             "treatment_time"][1], 2))
      } else {
        treatment_times <- c(treatment_times, rep(NA, 2))
      }
    }
    loc_date <- trends_data$date_format[1]
    trends_data <- data.frame(time = as.Date(time),
                              silo_name = silo_name, y = y_vec,
                              treatment_time = treatment_times,
                              date_format = loc_date)

    # Set ylabels if not already specified
    yrange <- range(trends_data$y, na.rm = TRUE)
    if (is.null(ylabels)){
      ylabels <- seq(from = yrange[1], to = yrange[2], length.out = yticks)
      ylabels <- round(ylabels, ydecimal)
    }

    # Save png (optional)
    if (identical(save_png, TRUE)) {
      png("undid_plot.png", width = width, height = height)
    } else if (!identical(save_png, FALSE)) {
      stop("`save_png` must be set to either `TRUE` or `FALSE`.")
    }

    trends_data$time <- as.Date(trends_data$time)

    plot(trends_data$time, trends_data$y, type = "n", xlab = xlab, ylab = ylab,
         main = title, ylim = ylim, xaxt = "n",
         yaxt = "n")
    axis(1, at = xlabels, labels = format(as.Date(xlabels), date_format),
         cex.axis = xaxlabsz)
    axis(2, at = ylabels, labels = ylabels, cex.axis = yaxlabsz)
    col_palette <- c(treatment_colour[1], control_colour[1])
    pch_palette <- c(pch_treated[1], pch_control[1])
    treatment_data <- trends_data[trends_data$silo_name == "Treatment",]
    control_data <- trends_data[trends_data$silo_name == "Control",]
    lines(treatment_data$time, treatment_data$y, col = col_palette[1],
          type = "o", lwd = lwd, pch = pch_palette[1])
    lines(control_data$time, control_data$y, col = col_palette[2],
          type = "o", lwd = lwd, pch = pch_palette[2])

    # Plot the dashed lines for indicating treatment times
    treatment_lines <- unique(trends_data$treatment_time)
    treatment_lines <- treatment_lines[treatment_lines != "control"]
    treatment_lines <- treatment_lines[treatment_lines != "missing"]
    treatment_lines <- treatment_lines[!is.na(treatment_lines)]
    treatment_lines <- vapply(
      as.character(treatment_lines),
      .parse_string_to_date,
      FUN.VALUE = as.Date(NA),
      date_format = as.character(trends_data$date_format[1])
    )

    for (treatment_line in treatment_lines) {
      abline(v = treatment_line,
             col = adjustcolor(treatment_indicator_col,
                               alpha.f = treatment_indicator_alpha),
             lty = treatment_indicator_lty, lwd = treatment_indicator_lwd)
    }

    # Plot legend
    if (identical(legend_on, TRUE)) {
      legend(legend_location,
             legend = c("Treatment", "Control"),
             col = col_palette,
             pch = pch_palette,
             lty = 1,
             lwd = lwd,
             cex = legend_text)
    } else if (!(identical(legend_on, FALSE))) {
      stop("`legend_on` must be set to either `TRUE` or `FALSE`.")
    }
  } else if (combine == FALSE) {

    trends_data$time <- as.Date(trends_data$time)

    # Set ylabels if not already specified
    yrange <- range(trends_data$y, na.rm = TRUE)
    if (is.null(ylabels)){
      ylabels <- seq(from = yrange[1], to = yrange[2], length.out = yticks)
      ylabels <- round(ylabels, ydecimal)
    }

    # Determine the unique control and unique treatment silos
    unique_control <- unique(trends_data[trends_data$treatment_time ==
                                           "control", "silo_name"])
    unique_treated <- unique(trends_data[trends_data$treatment_time !=
                                           "control", "silo_name"])

    # Set the colour palette for control silos
    if (length(control_colour) == 2) {
      control_palette <- colorRampPalette(control_colour)(length(unique_control))
    } else {
      control_palette <- rep(control_colour,
                             length.out = length(unique_control))
    }

    # Set the colour palette for treatment silos
    if (length(control_colour) == 2) {
      treatment_palette <- colorRampPalette(treatment_colour)(length(unique_treated))
    } else {
      treatment_palette <- rep(treatment_colour,
                               length.out = length(unique_treated))
    }

    # Set the points used in the plot
    pchs_treated <- rep(pch_treated, length.out = length(unique_treated))
    pchs_control <- rep(pch_control, length.out = length(unique_control))

    # Define the ylim
    if (is.null(ylim)) {
      ylim <- range(trends_data$y, na.rm = TRUE)
    }

    # Initiate plot
    trends_data$time <- as.Date(trends_data$time)

    if (save_png == TRUE) {
      png("undid_plot.png", width = width, height = height)
    }
    plot(trends_data$time, trends_data$y, type = "n", xlab = xlab, ylab = ylab,
         main = title, ylim = ylim, xaxt = "n",
         yaxt = "n")
    axis(1, at = xlabels, labels = format(as.Date(xlabels), date_format),
         cex.axis = xaxlabsz)
    axis(2, at = ylabels, labels = ylabels,
         cex.axis = yaxlabsz)
    # Plot control silos
    for (i in seq_along(unique_control)){
      silo_data <- trends_data[trends_data$silo_name == unique_control[i], ]
      lines(silo_data$time, silo_data$y, col = control_palette[i], type = "o",
            lwd = lwd, pch = pchs_control[i])
    }

    # Plot treated silos
    for (i in seq_along(unique_treated)){
      silo_data <- trends_data[trends_data$silo_name == unique_treated[i], ]
      lines(silo_data$time, silo_data$y, col = treatment_palette[i], type = "o",
            lwd = lwd, pch = pchs_treated[i])
    }

    # Plot the dashed lines for indicating treatment times
    treatment_lines <- unique(trends_data$treatment_time)
    treatment_lines <- treatment_lines[treatment_lines != "control"]
    treatment_lines <- na.omit(treatment_lines)
    treatment_lines <- mapply(
      .parse_string_to_date,
      treatment_lines,
      rep(trends_data$date_format[1], length(treatment_lines))
    )


    for (treatment_line in treatment_lines){
      abline(v = treatment_line,
             col = adjustcolor(treatment_indicator_col,
                               alpha.f = treatment_indicator_alpha),
             lty = treatment_indicator_lty, lwd = treatment_indicator_lwd)
    }

    # Plot legend
    if (identical(legend_on, TRUE)) {
      if (identical(simplify_legend, TRUE)) {
        silo_names <- c("Control", "Treatment")
        if (length(control_palette) %% 2 == 0) {
          darkcolr <- control_palette[length(control_palette) / 2]
          lightcolr <- control_palette[(length(control_palette) / 2) + 1]
          ctrl_clr <- colorRampPalette(c(darkcolr, lightcolr))(3)[2]
        } else {
          ctrl_clr <- control_palette[ceiling(length(control_palette)/2)]
        }

        if (length(treatment_palette) %% 2 == 0){
          darkcolr <- treatment_palette[length(treatment_palette)/  2]
          lightcolr <- treatment_palette[(length(treatment_palette) / 2) + 1]
          treat_clr <- colorRampPalette(c(darkcolr, lightcolr))(3)[2]
        } else {
          treat_clr <- treatment_palette[ceiling(length(treatment_palette) / 2)]
        }
        silo_colors <- c(ctrl_clr, treat_clr)

        legend(legend_location,
               legend = silo_names,
               col = silo_colors,
               pch = NA,
               lty = 1,
               lwd = lwd,
               cex = legend_text)
      } else {
        silo_names <- c(unique_control, unique_treated)
        silo_colors <- c(control_palette, treatment_palette)
        silo_pchs <- c(pchs_control, pchs_treated)

        legend(legend_location,
               legend = silo_names,
               col = silo_colors,
               pch = silo_pchs,
               lty = 1,
               lwd = lwd,
               cex = legend_text)
      }
    } else if (!(identical(legend_on, FALSE))) {
      stop("Set `legend_on`` to `TRUE` or `FALSE`.")
    }

  } else {
    stop("Set `combine` to `TRUE` or `FALSE`.")
  }

  if (save_png == TRUE) {
    dev.off()
  }

  return(trends_data)

}


#' @keywords internal
# Combines trends data
.combine_trends_data <- function(dir_path, covariates, interpolate) {

  files <- list.files(dir_path,
                      pattern = "^trends_data_.*\\.csv$", full.names = TRUE)
  if (length(files) == 0) stop(paste("No trends_data csv files found in:",
                                     dir_path))
  trends_data <- do.call(rbind, lapply(files, read.csv))
  trends_data$silo_name <- as.character(trends_data$silo_name)
  trends_data$treatment_time <- as.character(trends_data$treatment_time)
  trends_data$time <- mapply(.parse_string_to_date,
                             as.character(trends_data$time),
                             trends_data$date_format)


  if (identical(covariates, TRUE)) {
    trends_data$y <- trends_data$mean_outcome_residualized
  } else if (identical(covariates, FALSE)) {
    trends_data$y <- trends_data$mean_outcome
  } else {
    stop("Covariates must be set to ")
  }

  if (any(is.na(trends_data$y))) {
    if (identical(interpolate, TRUE)) {
      missing_silos <- unique(trends_data[is.na(trends_data$y), "silo_name"])
      for (silo in missing_silos) {
        trends_data[is.na(trends_data$y) &
                      trends_data$silo_name ==
                        silo, "y"] <- .piecewise_linear_function(
          trends_data[trends_data$silo_name == silo, "y"]
        )
      }
    } else if (!(identical(interpolate, FALSE))) {
      stop("`interpolate` must be set to `TRUE` or `FALSE`.")
    }
  }

  return(trends_data)

}