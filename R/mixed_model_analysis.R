mixed_model_analysis <- function(.df, .dv, .time, .phase, .participant = NULL) {
  # Sort the data frame by phase and then by time
  .df <- .df[order(.df[[.phase]], .df[[.time]]), ]

  # Add a global time variable
  .df$time <- 1:nrow(.df)

  # Split the data frame by phase
  split_df <- split(.df, .df[[.phase]])

  # Use lapply to iterate over each phase and calculate time_in_phase
  split_df <- lapply(split_df, function(sub_df) {
    sub_df$time_in_phase <- rev(seq_len(nrow(sub_df))) - 1
    return(sub_df)
  })

  # Combine the list of data frames back into a single data frame
  .df <- do.call(rbind, split_df)

  # Order the rows of the data frame by the original row names (optional)
  .df <- .df[order(as.numeric(row.names(.df))), ]

  mod_form <- stats::as.formula(paste0(.dv, " ~ ", "time_in_phase + ",.phase, " + time_in_phase*",.phase))

  if (is.null(.participant)){
    .df$participant <- factor(rep("x",nrow(.df)))
    re_form <- stats::as.formula("~time|participant")
  } else {
    re_form <- stats::as.formula(paste0("~time|",.participant))
  }

  mod <- nlme::gls(model       = mod_form,
                   data        = .df,
                   method      = "REML",
                   correlation = nlme::corAR1(, form=re_form))

  .df[[paste0("predicted_", .dv)]] <- predict(mod)

  # Identify where phase changes occur and calculate midpoints
  phase_changes <- which(diff(as.numeric(factor(.df[[.phase]]))) != 0)
  midpoints <- .df$time[phase_changes] + .5

  ggp <- ggplot2::ggplot()+
    ggplot2::geom_line(data= .df, ggplot2::aes(x =time,
                                               y = .data[[paste0("predicted_", .dv)]],
                                               col = "red"),
                       size= 2)+
    ggplot2::geom_point(data= .df, ggplot2::aes(x =time,
                                                 y= .data[[.dv]],
                                                 col = "blue"),
                        size= 2)+
    ggplot2::geom_vline(xintercept = midpoints,
                        linetype = "dashed",
                        size= 1.2)+
    ggplot2::theme_minimal()+
    ggplot2::ylab(.dv)+
    ggplot2::xlab("time")+
    ggplot2::scale_colour_manual(values=c("red"  = "red",
                                 "blue" = "blue"),
                        labels= c("data", "model"))+
    # scale_x_continuous(breaks= 1:10, minor_breaks = NULL)+
    # scale_y_continuous(breaks= seq(0, 18, 2), minor_breaks = NULL, limits = c(0, 18))+
    ggplot2::theme(axis.ticks.x=ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank()) #+
    # annotate(geom="text",
    #          x=3,
    #          y=17,
    #          label="Exposure",
    #          color="black")+
    # annotate(geom="text",
    #          x=8,
    #          y=17,
    #          label="Exposure + CT",
    #          color="black")

  return(list(data = .df,
              fitted_mod = mod,
              plot = ggp))

}

