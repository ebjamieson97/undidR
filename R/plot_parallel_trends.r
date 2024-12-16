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
#' @param legend_cex An double for adjusting the size of the text in the legend.
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
#' @param filepath Filepath to save the .csv file. Defaults to `tempdir()`.
#' @param filenamecsv A string filename for the combined trends data
#' Defaults to `"combined_trends_data.csv"`.
#' @param filenamepng A string filename for the .png file output.
#' Defaults to `"undid_plot.png"`.
#'
#' @examples
#' NULL
#'
#' @importFrom graphics axis legend lines abline
#' @export
#'
plot_parallel_trends <- function(dir_path, covariates = FALSE, save_csv = FALSE,
                                 combine = FALSE, pch = NA, pch_control = NA,
                                 pch_treated = NA,
                                 control_colour = c("darkgrey", "lightgrey"),
                                 control_color = NULL,
                                 treatment_colour = c("darkred", "lightcoral"),
                                 treatment_color = NULL,
                                 lwd = 2, xlab = NA, ylab = NA, title = NA,
                                 xticks = 4, date_format = "%Y-%m-%d",
                                 xdates = NULL,
                                 xaxlabsz = 0.8, save_png = FALSE, width = 800,
                                 height = 600, ylim = NULL, yaxlabsz = 0.8,
                                 ylabels = NULL, yticks = 4, ydecimal = 2,
                                 legend_location = "topright",
                                 simplify_legend = TRUE,
                                 legend_cex = 0.7, legend_on = TRUE,
                                 treatment_indicator_col = "grey",
                                 treatment_indicator_alpha = 0.5,
                                 treatment_indicator_lwd = 2,
                                 treatment_indicator_lty = 2,
                                 interpolate = FALSE, filepath = tempdir(),
                                 filenamecsv = "combined_trends_data.csv",
                                 filenamepng = "undid_plot.png") {

  # Run argument validations
  .validate_plotting_args(covariates, save_csv, combine, save_png,
                          simplify_legend, legend_on, interpolate,
                          dir_path, pch, pch_control, pch_treated,
                          legend_location, control_color,
                          control_colour, treatment_color,
                          treatment_colour, treatment_indicator_col,
                          lwd, xticks, yticks, xaxlabsz, yaxlabsz,
                          width, height, ydecimal, legend_cex,
                          treatment_indicator_alpha,
                          treatment_indicator_lwd,
                          treatment_indicator_lty, ylim,
                          ylabels, xdates, xlab, ylab, title,
                          date_format)

  # Read in trends data (optionally, save csv of combined data)
  trends_data <- .combine_trends_data(dir_path, covariates, interpolate)
  if (identical(save_csv, TRUE)) {
    filepath <- .filename_filepath_check(filenamecsv, filepath)
    full_path <- file.path(filepath, filenamecsv)
    write.csv(trends_data, full_path, row.names = FALSE, quote = FALSE,
              fileEncoding = "UTF-8")
    message(filenamecsv, " saved to: ", full_path)
  }


  # Set pch_control and pch_treated to pch if not selected
  if (is.na(pch_control)) {
    pch_control <- pch
  }
  if (is.na(pch_treated)) {
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

  # Wrap the save_png option in a try catch block which ensures dev.off()
  # is called in the case of an error
  if (identical(save_png, TRUE)) {
    if (!endsWith(filenamepng, ".png")) {
      stop("'filenamepng' must end with .png")
    }
    tryCatch({
      png(filename = filenamepng, width = width, height = height)
      device_open <- TRUE
      .plot_undid(trends_data, combine, yticks,
                  ydecimal, xlab, ylab, ylim, xlabels,
                  date_format, xaxlabsz, yaxlabsz,
                  treatment_colour, ylabels,
                  control_colour, pch_treated, pch_control,
                  lwd, treatment_indicator_alpha,
                  treatment_indicator_col,
                  treatment_indicator_lwd,
                  treatment_indicator_lty, legend_on,
                  legend_location, legend_cex, simplify_legend,
                  title)
    }, error = function(e) {
      message("An error occurred while plotting: ", e$message)
    }, finally = {
      if (device_open) {
        dev.off()
      }
    })
  } else if (identical(save_png, FALSE)) {
    .plot_undid(trends_data, combine, yticks,
                ydecimal, xlab, ylab, ylim, xlabels,
                date_format, xaxlabsz, yaxlabsz,
                treatment_colour, ylabels,
                control_colour, pch_treated, pch_control,
                lwd, treatment_indicator_alpha,
                treatment_indicator_col,
                treatment_indicator_lwd,
                treatment_indicator_lty, legend_on,
                legend_location, legend_cex, simplify_legend,
                title)
  } else {
    stop("`save_png` must be set to either `TRUE` or `FALSE`.")
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

#' @keywords internal
#' Plots parallel trends
.plot_undid <- function(trends_data, combine, yticks,
                        ydecimal, xlab, ylab, ylim, xlabels,
                        date_format, xaxlabsz, yaxlabsz,
                        treatment_colour, ylabels,
                        control_colour, pch_treated, pch_control,
                        lwd, treatment_indicator_alpha,
                        treatment_indicator_col,
                        treatment_indicator_lwd,
                        treatment_indicator_lty, legend_on,
                        legend_location, legend_cex, simplify_legend,
                        title) {

  if (identical(combine, TRUE)) {
    .plot_undid_combined(trends_data, yticks, ydecimal,
                         xlab, ylab, ylim, xlabels, date_format,
                         xaxlabsz, yaxlabsz, treatment_colour,
                         control_colour, pch_treated, pch_control,
                         lwd, treatment_indicator_alpha,
                         treatment_indicator_col, ylabels,
                         treatment_indicator_lwd,
                         treatment_indicator_lty, legend_on,
                         legend_location, legend_cex, title)
  } else if (identical(combine, FALSE)) {
    .plot_undid_separate(trends_data, yticks, ydecimal,
                         control_colour, treatment_colour,
                         pch_treated, pch_control,
                         xlab, ylab, xlabels, date_format,
                         xaxlabsz, yaxlabsz, lwd,
                         treatment_indicator_alpha,
                         treatment_indicator_col,
                         treatment_indicator_lty,
                         treatment_indicator_lwd,
                         legend_on, ylabels,
                         simplify_legend, legend_location,
                         legend_cex, title, ylim)
  }
}

#' @keywords internal
#' Plots parallel trends with silos combined by treatment status
.plot_undid_combined <- function(trends_data, yticks, ydecimal,
                                 xlab, ylab, ylim, xlabels, date_format,
                                 xaxlabsz, yaxlabsz, treatment_colour,
                                 control_colour, pch_treated, pch_control,
                                 lwd, treatment_indicator_alpha,
                                 treatment_indicator_col, ylabels,
                                 treatment_indicator_lwd,
                                 treatment_indicator_lty, legend_on,
                                 legend_location, legend_cex, title) {

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
           cex = legend_cex)
  } else if (!(identical(legend_on, FALSE))) {
    stop("`legend_on` must be set to either `TRUE` or `FALSE`.")
  }

}

#' @keywords internal
#' Plots parallel trends with each silo separate
.plot_undid_separate <- function(trends_data, yticks, ydecimal,
                                 control_colour, treatment_colour,
                                 pch_treated, pch_control,
                                 xlab, ylab, xlabels, date_format,
                                 xaxlabsz, yaxlabsz, lwd,
                                 treatment_indicator_alpha,
                                 treatment_indicator_col,
                                 treatment_indicator_lty,
                                 treatment_indicator_lwd,
                                 legend_on, ylabels,
                                 simplify_legend, legend_location,
                                 legend_cex, title, ylim) {

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
             cex = legend_cex)
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
             cex = legend_cex)
    }
  }
}

#' @keywords internal
#' Validate arguments
.validate_plotting_args <- function(covariates, save_csv, combine, save_png,
                                    simplify_legend, legend_on, interpolate,
                                    dir_path, pch, pch_control, pch_treated,
                                    legend_location, control_color,
                                    control_colour, treatment_color,
                                    treatment_colour, treatment_indicator_col,
                                    lwd, xticks, yticks, xaxlabsz, yaxlabsz,
                                    width, height, ydecimal, legend_cex,
                                    treatment_indicator_alpha,
                                    treatment_indicator_lwd,
                                    treatment_indicator_lty, ylim,
                                    ylabels, xdates, xlab, ylab, title,
                                    date_format) {
  # Validate all TRUE/FALSE arguments
  log_args <- list(covariates = covariates, save_csv = save_csv,
                   combine = combine, save_png = save_png,
                   simplify_legend = simplify_legend, legend_on = legend_on,
                   interpolate = interpolate)
  for (arg in names(log_args)) {
    .validate_logical_args(log_args[[arg]], arg)
  }

  # Validate the dir_path
  .validate_dir_path(dir_path)

  # Validate pch args
  .validate_pch(pch, "pch")
  .validate_pch(pch_treated, "pch_treated")
  .validate_pch(pch_control, "pch_control")

  # Validate legend_location
  valid_legend_locations <- c("bottomright", "bottom", "bottomleft", "left",
                              "topleft", "top", "topright", "right", "center")
  if (!legend_location %in% valid_legend_locations) {
    stop("`legend_location` must be one of: ",
         paste(valid_legend_locations, collapse = ", "))
  }

  # Validate colours
  .validate_colours(control_colour, "control_colour")
  .validate_colours(treatment_colour, "treatment_colour")
  .validate_colours(control_color, "control_color")
  .validate_colours(treatment_color, "treatment_color")
  .validate_colours(treatment_indicator_col, "treatment_indicator_col")

  # Validate numeric args
  .validate_numeric(lwd, "lwd", 0, 1000)
  .validate_numeric(xticks, "xticks", 0, 1000)
  .validate_numeric(yticks, "yticks", 0, 1000)
  .validate_numeric(xaxlabsz, "xaxlabsz", 0, 1000)
  .validate_numeric(yaxlabsz, "yaxlabsz", 0, 1000)
  .validate_numeric(width, "width", 0, 10000)
  .validate_numeric(height, "height", 0, 10000)
  .validate_numeric(ydecimal, "ydecimal", 0, 16)
  .validate_numeric(legend_cex, "legend_cex", 0, 1000)
  .validate_numeric(treatment_indicator_alpha, "treatment_indicator_alpha",
                    0, 1)
  .validate_numeric(treatment_indicator_lwd, "treatment_indicator_lwd",
                    0, 1000)
  .validate_numeric(treatment_indicator_lty, "treatment_indicator_lty",
                    0, 6)

  # Validate ylim
  .validate_ylim(ylim)

  # Validate ylabels
  if (!is.numeric(ylabels) && !is.null(ylabels)) {
    stop("`ylabels` must be entered as numeric values or as `NULL`.")
  }

  # Validate xdates
  if (!is.null(xdates) && !inherits(xdates, "Date")) {
    stop("`xdates` must be of class 'Date' or be `NULL`.")
  }

  # Validate title arguments
  .validate_titles(ylab)
  .validate_titles(xlab)
  .validate_titles(title)

  # Validate date_format
  .validate_date_format(date_format)

}

#' @keywords internal
#' checks that input is given as TRUE or FALSE
.validate_logical_args <- function(logical, arg_name) {
  if (!(identical(logical, TRUE) || identical(logical, FALSE))) {
    stop(paste(arg_name, "must be a single `TRUE` or `FALSE` value."))
  }
}

#' @keywords internal
#' Validate the pch arguments
.validate_pch <- function(pch, arg_name) {
  if (!(is.numeric(pch) || is.na(pch))) {
    stop(paste0("`", arg_name, "` must be a numeric value between 0 and 25,
                or NA."))
  }
  if (is.numeric(pch)) {
    if (any(pch < 0 | pch > 25)) {
      stop(paste0("`", arg_name, "` must be between 0 and 25."))
    }
  }
}

#' @keywords internal
#' Validate colour args
.validate_colours <- function(arg, arg_name) {
  if (!(is.character(arg) || is.null(arg))) {
    stop(paste(arg_name, "must be a character value or set to `NULL`."))
  }
}

#' @keywords internal
#' Validate numeric args
.validate_numeric <- function(arg, arg_name, min, max) {
  if (!(is.numeric(arg) && length(arg) == 1)) {
    stop(paste(arg_name, "must be a numeric value of length 1."))
  }
  if (arg < min || arg > max) {
    stop(paste(arg_name, "must be bounded by", min, "and", max))
  }
}

#' @keywords internal
#' Validate dir_path
.validate_dir_path <- function(dir_path) {
  if (!is.character(dir_path) || length(dir_path) != 1) {
    stop("`dir_path` must be a single character string.")
  }
  if (!dir.exists(dir_path)) {
    stop("The specified directory does not exist: ", dir_path)
  }
}

#' @keywords internal
#' Validate ylim
.validate_ylim <- function(ylim) {
  if (!is.null(ylim) && (!(is.numeric(ylim) && length(ylim) == 2))) {
    stop("`ylim` must be a numeric vector of length 2 or `NULL`.")
  }
  if ((!is.null(ylim)) && (ylim[1] >= ylim[2])) {
    stop("`ylim` must have ylim[1] < ylim[2].")
  }
}

#' @keywords internal
#' Validate titles
.validate_titles <- function(arg, arg_name) {
  if ((!is.character(arg) || length(arg) != 1) && !is.na(arg)) {
    stop(paste(arg_name, "must be a character string of length 1 or `NA`."))
  }
}

#' @keywords internal
#' Validate date format
.validate_date_format <- function(date_format) {
  if (!is.character(date_format) || length(date_format) != 1) {
    stop("'date_format' must be a character string of length 1.")
  }
  if (!grepl("%", date_format, fixed = TRUE)) {
    stop("'date_format' must be formatted in standard R format with
         at least one '%'.")
  }
}