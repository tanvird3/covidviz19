shinyServer(function(input, output) {
  # get the constant charts
  constant_charts <- function() {
    # plot the global bar plot
    figG <- plot_ly(
      x = c("Confirmed", "Deaths", "Recovered"),
      y = c(confirmed_cases, deaths, recovered),
      text = c(confirmed_cases, deaths, recovered),
      textposition = "auto",
      name = paste("Global Situation as of", bangladesh_data[nrow(bangladesh_data), ncol(bangladesh_data)]),
      type = "bar",
      marker = list(color = "#0072B2"),
      showlegend = F,
      hoverinfo = "x+y",
      width = 900
    ) %>% layout(xaxis = list(title = paste(
      "Global Situation as of", bangladesh_data[nrow(bangladesh_data), ncol(bangladesh_data)]
    )),
    yaxis = list(hoverformat = ",.0f")) %>% add_annotations(
      text = paste("Global Situation as of", bangladesh_data[nrow(bangladesh_data), ncol(bangladesh_data)]),
      x = 0.1,
      y = 1.05,
      yref = "paper",
      xref = "paper",
      xanchor = "middle",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = 15)
    )
    
    # plot the summary bar plot for bangladesh
    figB <- plot_ly(
      x = c("Confirmed", "Deaths", "Recovered"),
      y = unlist(c(bangladesh_data[nrow(bangladesh_data), c(1:3)])),
      text = unlist(c(bangladesh_data[nrow(bangladesh_data), c(1:3)])),
      textposition = "auto",
      name = paste("Situation of Bangladesh as of", bangladesh_data[nrow(bangladesh_data), ncol(bangladesh_data)]),
      type = "bar",
      marker = list(color = "#009E73"),
      showlegend = F,
      hoverinfo = "x+y",
      width = 900
    ) %>% layout(xaxis = list(title = paste(
      "Situation of Bangladesh as of", bangladesh_data[nrow(bangladesh_data), ncol(bangladesh_data)]
    )),
    yaxis = list(hoverformat = ",.0f")) %>% add_annotations(
      text = paste("Bangladesh Situation as of", bangladesh_data[nrow(bangladesh_data), ncol(bangladesh_data)]),
      x = 0.9,
      y = 1.05,
      yref = "paper",
      xref = "paper",
      xanchor = "middle",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = 15)
    )
    
    # subplot these two summary plots
    figG <- subplot(figG, figB, nrows = 1)
    
    # plot the global time series
    global_time <-
      plot_ly(
        global_conf_u,
        x = ~ Date,
        y = ~ global_conf_u$global,
        name = "Confirmed",
        type = "scatter",
        mode = "lines",
        width = 900
      ) %>% layout(title = "<br>GLOBAL COVID-19 TIME SERIES",
                   yaxis = list(title = "Count"))
    
    global_time <-
      global_time %>% plotly::add_trace(
        global_death_u,
        x =  ~ Date,
        y =  ~ global_death_u$global,
        name = "Death"
      )
    
    global_time <-
      global_time %>% add_trace(
        global_recov_u,
        x =  ~ Date,
        y =  ~ global_recov_u$global,
        name = "Recovery"
      )
    
    # recovery to death ratio of the world
    global_recov_dead <-
      plot_ly(
        global_recov_u,
        x = ~ Date,
        y = ~ global_recov_u$global / global_death_u$global * 100,
        name = "Global Recovery to Death Ratio",
        type = "scatter",
        mode = "lines",
        width = 900
      ) %>% layout(title = "<br>Global Recovery to Death Ratio",
                   yaxis = list(title = "Recovery to Death (%)"))
    
    
    # plot cfr of the world
    global_death_u <-
      mutate(global_death_u, daily_case = c(NA, diff(global_death_u[, ncol(global_death_u)])))
    global_conf_u <-
      mutate(global_conf_u, daily_case = c(NA, diff(global_conf_u[, ncol(global_conf_u)])))
    
    global_cfr <-
      plot_ly(
        global_conf_u,
        x = ~ Date,
        y = ~ global_death_u$global / global_conf_u$global * 100,
        name = "Cumulative CFR",
        type = "scatter",
        mode = "lines",
        width = 900
      ) %>% layout(title = "<br>Global Case Fatality Rate (%)",
                   yaxis = list(title = "CFR (%)"))
    
    global_cfr <-
      global_cfr %>% plotly::add_trace(
        global_conf_u,
        x =  ~ Date,
        y =  ~ global_death_u$daily_case / global_conf_u$daily_case * 100,
        name = "Daily CFR"
      )
    
    # % change over 3 days
    global_conf_u <-
      mutate(global_conf_u, change = c(rep(NA, 3), diff(global, 3) / global[1:(nrow(global_conf_u) -
                                                                                 3)]))
    global_death_u <-
      mutate(global_death_u, change = c(rep(NA, 3), diff(global, 3) / global[1:(nrow(global_death_u) -
                                                                                  3)]))
    
    global_p <-
      plot_ly(
        global_conf_u,
        x = ~ Date,
        y = ~ change * 100,
        name = "New Cases",
        type = "scatter",
        mode = "lines",
        width = 900
      ) %>% layout(title = "<br>Global % Change over 3 Days",
                   yaxis = list(title = "% Change"))
    
    global_p <-
      global_p %>% plotly::add_trace(
        global_death_u,
        x =  ~ Date,
        y =  ~ global_death_u$change * 100,
        name = "Deaths"
      )
    
    return(
      list(
        figG = figG,
        global_time = global_time,
        global_recov_dead = global_recov_dead,
        global_cfr = global_cfr,
        global_p = global_p
      )
    )
    
  }
  
  # output for this part
  output_get1 <- reactive({constant_charts()})
  
  # now plot the variable plots
  corona_visual <- function(countries) {
    if (length(countries) == 0) {
      countries <- "Uganda"
    }
    # compare the situation of bangladesh with other countries
    # filter the data of the given countries
    world_confirmed <-
      filter(confirmed_data,
             confirmed_data[, 2] %in% c("Bangladesh", countries))
    
    world_death <-
      filter(death_data, death_data[, 2] %in% c("Bangladesh", countries))
    
    world_recover <-
      filter(recover_data,
             recover_data[, 2] %in% c("Bangladesh", countries))
    
    # bring the data into right format
    # confirmed cases
    world_confirmed <- as.data.frame(t(world_confirmed))
    
    world_conf <- world_confirmed[5:nrow(world_confirmed), ]
    
    names(world_conf) <- unlist(c(world_confirmed[2,]))
    
    world_conf$Date <- row.names(world_conf)
    
    world_conf$Date <-
      as.Date(world_conf$Date, format = "%m/%d/%y")
    
    world_conf[, 1:(ncol(world_conf) - 1)] <-
      data.frame(apply(world_conf[, 1:(ncol(world_conf) - 1)], 2, as.numeric))
    
    ## get the date of first confirmed case
    wc <- world_conf[, 1:(ncol(world_conf) - 1)]
    
    a <- apply(wc, 1, sum)
    
    w <- which(a > 0)[1]
    
    mindate <- world_conf$Date[w]
    
    # get rid of the 0 values with NA
    world_conf_mod <- na_if(world_conf, 0)
    
    # turn into log scale (if that option is selected)
    # manage the title of the plots as well
    title_conf <- "<br>Cumulative Confirmed Cases"
    
    if (input$Scale == "Log") {
      world_conf_mod[,-ncol(world_conf_mod)] <-
        sapply(world_conf_mod[,-ncol(world_conf_mod)], function(x)
          log(x))
      title_conf <- "<br>Cumulative Confirmed Cases (In Log Scale)"
    }
    
    # fatality
    world_death <- as.data.frame(t(world_death))
    
    world_dead <- world_death[5:nrow(world_death), ]
    
    names(world_dead) <- unlist(c(world_death[2,]))
    
    world_dead$Date <- row.names(world_dead)
    
    world_dead$Date <-
      as.Date(world_dead$Date, format = "%m/%d/%y")
    
    world_dead[, 1:(ncol(world_dead) - 1)] <-
      data.frame(apply(world_dead[, 1:(ncol(world_dead) - 1)], 2, as.numeric))
    
    ## get the date of the first death
    wd <- world_dead[, 1:(ncol(world_dead) - 1)]
    
    d <- apply(wd, 1, sum)
    
    wd <- which(d > 0)[1]
    
    mindateD <- world_dead$Date[wd]
    
    # get rid of the 0 values with NA
    world_dead_mod <- na_if(world_dead, 0)
    
    # convert into log scale (if that option is selected)
    # manage the title as well
    title_dead <- "<br>Cumulative Deaths"
    
    if (input$Scale == "Log") {
      world_dead_mod[,-ncol(world_dead_mod)] <-
        sapply(world_dead_mod[,-ncol(world_dead_mod)], function(x)
          log(x))
      title_dead <- "<br>Cumulative Deaths (In Log Scale)"
    }
    
    # recovered cases
    world_recover <- as.data.frame(t(world_recover))
    
    world_recov <- world_recover[5:nrow(world_recover), ]
    
    names(world_recov) <- unlist(c(world_recover[2,]))
    
    world_recov$Date <- row.names(world_recov)
    
    world_recov$Date <-
      as.Date(world_recov$Date, format = "%m/%d/%y")
    
    world_recov[, 1:(ncol(world_recov) - 1)] <-
      data.frame(apply(world_recov[, 1:(ncol(world_recov) - 1)], 2, as.numeric))
    
    ## get the date of first recovered case
    wr <- world_recov[, 1:(ncol(world_recov) - 1)]
    
    r <- apply(wr, 1, sum)
    
    wr <- which(r > 0)[1]
    
    mindateR <- world_recov$Date[wr]
    
    # get rid of the 0 values with NA
    world_recov_mod <- na_if(world_recov, 0)
    
    # convert the country charts into log scale (if that option is selected)
    # manage title as well
    title_recov <- "<br>Cumulative Recovered Cases"
    
    if (input$Scale == "Log") {
      world_recov_mod[,-ncol(world_recov_mod)] <-
        sapply(world_recov_mod[,-ncol(world_recov_mod)], function(x)
          log(x))
      title_recov <- "<br>Cumulative Recovered Cases (In Log Scale)"
    }
    
    # plot the comparison time series
    # confirmed cases
    fig_confirm <-
      plot_ly(
        world_conf_mod,
        x = ~ Date,
        y = ~ world_conf_mod[, 1],
        name = names(world_conf_mod)[1],
        type = "scatter",
        mode = "lines",
        width = 900
      ) %>% layout(
        title = title_conf,
        xaxis = list(range = c(mindate, max(
          world_conf_mod$Date
        ))),
        yaxis = list(title = "Cumulative Confirmed Cases")
      )
    
    for (trace in colnames(world_conf_mod)[2:(ncol(world_conf_mod) - 1)]) {
      fig_confirm <-
        fig_confirm %>% plotly::add_trace(y = as.formula(paste0("~`", trace, "`")), name = trace)
    }
    
    # fatalities
    fig_dead <-
      plot_ly(
        world_dead_mod,
        x =  ~ Date,
        y =  ~ world_dead_mod[, 1],
        name = names(world_dead_mod)[1],
        type = "scatter",
        mode = "lines",
        width = 900
      ) %>% layout(
        title = title_dead,
        xaxis = list(range = c(mindateD, max(
          world_dead_mod$Date
        ))),
        yaxis = list(title = "Cumulative Deaths")
      )
    
    for (trace in colnames(world_dead_mod)[2:(ncol(world_dead_mod) - 1)]) {
      fig_dead <-
        fig_dead %>% plotly::add_trace(y = as.formula(paste0("~`", trace, "`")), name = trace)
    }
    
    # recovered cases
    fig_recov <-
      plot_ly(
        world_recov_mod,
        x =   ~ Date,
        y =  ~ world_recov_mod[, 1],
        name = names(world_recov_mod)[1],
        type = "scatter",
        mode = "lines",
        width = 900
      ) %>% layout(
        title = title_recov,
        xaxis = list(range = c(mindateR, max(
          world_recov_mod$Date
        ))),
        yaxis = list(title = "Cumulative Recovered Cases")
      )
    
    for (trace in colnames(world_recov_mod)[2:(ncol(world_recov_mod) - 1)]) {
      fig_recov <-
        fig_recov %>% plotly::add_trace(y = as.formula(paste0("~`", trace, "`")), name = trace)
    }
    
    # comparative chart since first detected case
    # get the maximum days of the selected countries since first case detected
    max_case <-
      c(1:max(colSums(world_conf[, 1:(ncol(world_conf) - 1)] != 0)))
    
    # modify the data a bit if log is selected
    world_conf_c <- world_conf
    
    title <-
      "<br>Cumulative Confirmed Cases since the First Case was Confirmed"
    
    if (input$Scale == "Log") {
      world_conf_c[,-ncol(world_conf_c)] <-
        sapply(world_conf_c[,-ncol(world_conf_c)], function(x)
          log(x))
      title <-
        "<br>Cumulative Confirmed Cases (In Log Scale) since the First Case was Confirmed"
    }
    
    fig_confirm_S <-
      plot_ly(
        world_conf_c,
        x = ~ max_case,
        y = ~ c(world_conf_c[, 1][world_conf[, 1] != 0],
                rep(NA, (
                  length(max_case) - length(world_conf_c[, 1][world_conf[, 1] != 0])
                ))),
        name = names(world_conf_c)[1],
        type = "scatter",
        mode = "lines",
        width = 900
      ) %>% layout(
        title = title,
        xaxis = list(range = c(1, length(max_case)), title = "Days Since First Confirmed Case"),
        yaxis = list(title = "Cumulative Confirmed Cases")
      )
    
    for (trace in colnames(world_conf_c)[2:(ncol(world_conf_c) - 1)]) {
      fig_confirm_S <-
        fig_confirm_S %>% plotly::add_trace(y = c(world_conf_c[, trace][world_conf[, trace] !=
                                                                          0], rep(NA, (
                                                                            length(max_case) - length(world_conf_c[, trace][world_conf[, trace] != 0])
                                                                          ))), name = trace)
    }
    
    # comparative death chart since first death
    # get the maximum length of the given countries since first death occured
    max_case_D <-
      c(1:max(colSums(world_dead[, 1:(ncol(world_dead) - 1)] != 0)))
    
    # mofify the data a bit if log is selected
    world_dead_c <- world_dead
    
    title <-
      "<br>Cumulative Deaths since the First Fatality Occured"
    
    if (input$Scale == "Log") {
      world_dead_c[,-ncol(world_dead_c)] <-
        sapply(world_dead_c[,-ncol(world_dead_c)], function(x)
          log(x))
      title <-
        "<br>Cumulative Deaths (In Log Scale) since the First Fatality Occured"
    }
    
    fig_confirm_D <-
      plot_ly(
        world_dead_c,
        x = ~ max_case_D,
        y = ~ c(world_dead_c[, 1][world_dead[, 1] != 0],
                rep(NA, (
                  length(max_case_D) - length(world_dead_c[, 1][world_dead[, 1] != 0])
                ))),
        name = names(world_dead_c)[1],
        type = "scatter",
        mode = "lines",
        width = 900
      ) %>% layout(
        title = title,
        xaxis = list(range = c(1, length(max_case_D)), title = "Days Since First Death"),
        yaxis = list(title = "Cumulative Deaths")
      )
    
    for (trace in colnames(world_dead_c)[2:(ncol(world_dead_c) - 1)]) {
      fig_confirm_D <-
        fig_confirm_D %>% plotly::add_trace(y = c(world_dead_c[, trace][world_dead[, trace] !=
                                                                          0], rep(NA, (
                                                                            length(max_case_D) - length(world_dead_c[, trace][world_dead[, trace] != 0])
                                                                          ))), name = trace)
    }
    
    # get the recover to death ratio data frame and get the plot
    # calculate the ratio
    recov_death <-
      world_recov[, 1:(ncol(world_recov) - 1)] / world_dead[, 1:(ncol(world_dead) - 1)]
    
    # get rid of NA and Inf values
    recov_death[is.na(recov_death)] <- 0
    recov_death[recov_death == Inf] <- 0
    recov_death$Date <- row.names(recov_death)
    
    # set the date column
    recov_death$Date <-
      as.Date(recov_death$Date, format = "%m/%d/%y")
    
    
    fig_Ratio <-
      plot_ly(
        recov_death,
        x = ~ max_case_D,
        y = ~ c(recov_death[, 1][world_dead[, 1] != 0], # plot the ratio only since the first death occured
                rep(NA, (
                  length(max_case_D) - length(recov_death[, 1][world_dead[, 1] != 0]) # fill the rest length of max_case_d with NA
                ))),
        name = names(recov_death)[1],
        type = "scatter",
        mode = "lines",
        width = 900
      ) %>% layout(
        title = "<br>Cumulative Recovery to Cumulative Death since the First Fatality Occured",
        xaxis = list(range = c(1, length(max_case_D)), title = "Days Since First Death"),
        yaxis = list(title = "Cumulative Recovery to Cumulative Death")
      )
    
    for (trace in colnames(recov_death)[2:(ncol(recov_death) - 1)]) {
      fig_Ratio <-
        fig_Ratio %>% plotly::add_trace(y = c(recov_death[, trace][world_dead[, trace] !=
                                                                     0], rep(NA, (
                                                                       length(max_case_D) - length(recov_death[, trace][world_dead[, trace] != 0])
                                                                     ))), name = trace)
    }
    
    # generate the case fatality rate plot (dead/confirmed) daily and cumulative basis
    # first cumulative
    # get the ratio
    dead_conf <-
      world_dead[, 1:(ncol(world_dead) - 1)] / world_conf[, 1:(ncol(world_conf) - 1)] * 100
    
    # get rid of na and inf
    dead_conf[is.na(dead_conf)] <- 0
    dead_conf[dead_conf == Inf] <- 0
    dead_conf$Date <- row.names(dead_conf)
    
    # set the date column
    dead_conf$Date <-
      as.Date(dead_conf$Date, format = "%m/%d/%y")
    
    # now calculate the daily incidents
    # create the daily death series
    dead_daily <-
      data.frame(sapply(world_dead[, 1:(ncol(world_dead) - 1)], function(x)
        diff(x)))
    
    
    # create the daily confirmed case series
    conf_daily <-
      data.frame(sapply(world_conf[, 1:(ncol(world_conf) - 1)], function(x)
        diff(x)))
    
    # create the daily death to conf series
    dead_conf_daily <- dead_daily / conf_daily * 100
    
    # cleanse the data
    dead_conf_daily[is.na(dead_conf_daily)] <- 0
    
    dead_conf_daily[dead_conf_daily == Inf] <- 0
    
    dead_conf_daily[dead_conf_daily < 0] <- 0
    
    # now generate the blank list of plots plots (cfr)
    fig_cfr <- list()
    
    
    # 3 Day % change in confirmed cases and fatality
    # 3 day difference of deaths
    dead_daily_p <-
      data.frame(sapply(world_dead[, 1:(ncol(world_dead) - 1)], function(x)
        diff(x, 3) / x[1:(length(x) - 3)] * 100))
    
    # cleanse the data
    #dead_daily_p[is.na(dead_daily_p)] <- 0
    
    #dead_daily_p[dead_daily_p == Inf] <- 0
    
    dead_daily_p[dead_daily_p < 0] <- 0
    
    # 3 day difference of cases
    conf_daily_p <-
      data.frame(sapply(world_conf[, 1:(ncol(world_conf) - 1)], function(x)
        diff(x, 3) / x[1:(length(x) - 3)] * 100))
    
    # cleanse the data
    #conf_daily_p[is.na(conf_daily_p)] <- 0
    
    #conf_daily_p[conf_daily_p == Inf] <- 0
    
    conf_daily_p[conf_daily_p < 0] <- 0
    
    # now generate the blank list of plots (3 day % change)
    fig_cp <- list()
    
    # we now have to generate a series d_trace starting from 1 to the number of days elapsed since the first death
    # there are cluntries/regions without any deaths, in that case get rid of the error
    # if no death occured d_trace is just NA
    for (i in 1:(ncol(dead_conf_daily))) {
      if (!is.na(which(world_dead[, i] != 0)[1])) {
        # if there are deaths
        d_trace <-
          c(1:(nrow(world_dead) + 1 - which(world_dead[, i] != 0)[1])) # take from the last row value to the first death
      } else {
        d_trace <- NA # if no death just leave an NA
      }
      
      # create a new data frame where the first column will be 1. days since first death (2 times)
      # 2. rbind(cum cfr, daily cfr), 3. type (first cum cfr, then daily cfr)
      cfr_d <-
        data.frame(
          trace = rep(d_trace, 2),
          dc = c(
            tail(dead_conf[, i], length(d_trace)),
            tail(dead_conf_daily[, i], length(d_trace))
          ),
          Type = c(rep("Cumulative CFR", length(
            tail(dead_conf[, i], length(d_trace))
          )), rep("Daily CFR",
                  length(
                    tail(dead_conf_daily[, i], length(d_trace))
                  )))
        )
      
      # now generate the plots (as list)
      fig_cfr[[i]] <-
        cfr_d %>%
        plot_ly(
          x = ~ trace,
          y = ~ dc,
          color = ~ Type,
          colors = c("#D55E00", "#56B4E9"),
          legendgroup =  ~ Type,
          type = "scatter",
          mode = "lines",
          width = 900,
          height =  200 * ncol(dead_conf_daily),
          showlegend = ifelse(i == ncol(dead_conf_daily), T, F)
        ) %>% layout(
          title = "<br>Case Fatality Rate (%)",
          xaxis = list(title = ifelse(
            i == ncol(dead_conf_daily), "Days Since First Death", ""
          )),
          yaxis = list(title = "CFR (%)")
        ) %>% add_annotations(
          text = paste(" ", names(dead_conf_daily)[i], sep = "<br>"),
          x = 0.5,
          y = 1,
          yref = "paper",
          xref = "paper",
          xanchor = "middle",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 15)
        )
      
      # now the 3 day % change plot
      # create a new data frame where the first column will be 1. days since first death (2 times)
      # 2. rbind(3 day % change confirm, 3 day % change death), 3. type (first 3 day % change conf, then 3 day % change death)
      change_p <-
        data.frame(
          trace = rep(d_trace, 2),
          dc = c(
            tail(conf_daily_p[, i], length(d_trace)),
            tail(dead_daily_p[, i], length(d_trace))
          ),
          Type = c(rep("New Cases", length(
            tail(conf_daily_p[, i], length(d_trace))
          )), rep("New Fatalities",
                  length(
                    tail(dead_daily_p[, i], length(d_trace))
                  )))
        )
      
      # now generate the plots (as list)
      fig_cp[[i]] <-
        change_p %>%
        plot_ly(
          x = ~ trace,
          y = ~ dc,
          color = ~ Type,
          colors = c("#D55E00", "#56B4E9"),
          legendgroup =  ~ Type,
          type = "scatter",
          mode = "lines",
          width = 900,
          height =  300 * ncol(dead_conf_daily),
          showlegend = ifelse(i == ncol(dead_conf_daily), T, F)
        ) %>% layout(
          title = "<br>3-Day % Change",
          xaxis = list(title = ifelse(
            i == ncol(dead_conf_daily), "Days Since First Death", ""
          )),
          yaxis = list(title = "3-DAY % CHANGE")
        ) %>% add_annotations(
          text = paste(" ", names(dead_conf_daily)[i], sep = "<br>"),
          x = 0.5,
          y = 1,
          yref = "paper",
          xref = "paper",
          xanchor = "middle",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 15)
        )
      
    }
    
    # get all the plots in a subplot
    fig_cfr_print <-
      subplot(
        fig_cfr,
        nrows = ncol(dead_conf_daily),
        titleY = T,
        titleX = T
      )
    
    
    # get all the plots in a subplot
    fig_cp_print <-
      subplot(
        fig_cp,
        nrows = ncol(dead_conf_daily),
        titleY = T,
        titleX = T
      )
    
    
    return(
      list(
        fig_confirm = fig_confirm,
        fig_dead = fig_dead,
        fig_recov = fig_recov,
        fig_confirm_S = fig_confirm_S,
        fig_confirm_D = fig_confirm_D,
        fig_Ratio = fig_Ratio,
        fig_cfr_print = fig_cfr_print,
        fig_cp_print = fig_cp_print
      )
    )
    
  }
  
  # generate the outputs
  output_get2 <- reactive({corona_visual(input$countries)})
  
  output$figG <-
    renderPlotly({
      output_get1()$figG
    })
  
  output$global_time <-
    renderPlotly({
      output_get1()$global_time
    })
  
  output$global_recov_dead <-
    renderPlotly({
      output_get1()$global_recov_dead
    })
  
  output$global_cfr <-
    renderPlotly({
      output_get1()$global_cfr
    })
  
  output$global_p <-
    renderPlotly({
      output_get1()$global_p
    })
  
  output$fig_confirm <-
    renderPlotly({
      output_get2()$fig_confirm
    })
  
  output$fig_dead <-
    renderPlotly({
      output_get2()$fig_dead
    })
  
  output$fig_recov <-
    renderPlotly({
      output_get2()$fig_recov
    })
  
  output$fig_confirm_S <-
    renderPlotly({
      output_get2()$fig_confirm_S
    })
  
  output$fig_confirm_D <-
    renderPlotly({
      output_get2()$fig_confirm_D
    })
  
  output$fig_Ratio <-
    renderPlotly({
      output_get2()$fig_Ratio
    })
  
  output$fig_cfr_print <-
    renderPlotly({
      output_get2()$fig_cfr_print
    })
  
  output$fig_cp_print <-
    renderPlotly({
      output_get2()$fig_cp_print
    })
  
  lapply(c(
    "fig_confirm",
    "fig_dead",
    "fig_recov",
    "fig_confirm_S",
    "fig_confirm_D",
    "fig_Ratio",
    "fig_cfr_print",
    "fig_cp_print"
  ), function(x)
    outputOptions(output, x, suspendWhenHidden = F))
})