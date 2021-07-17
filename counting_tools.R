# This simple script contains some tools for counting variable levels
# And calculating the characteristic of numeric variables


# libraries

  require(plyr)
  require(tidyverse)
  require(stringi)
  
# functions for factors -------

  complete_cases <- function(inp_tbl, index_var = 'ID') {
    
    ## excludes individuals with the incomplete longitudinal record
    
    out_tbl <- inp_tbl %>% 
      dlply(index_var, function(x) if(all(complete.cases(x))) x else NULL) %>% 
      compact %>% 
      reduce(rbind) %>% 
      as_tibble
    
    return(out_tbl)
    
  }

  count_feature <- function(inp_tbl, var_to_count, remove_na = T) {
    
    ## calculates the percentage and number of participants with/without the given feature
    
    if(remove_na) {
      
      count_tbl <- inp_tbl %>% 
        filter(!is.na(.data[[var_to_count]]), 
               .data[[var_to_count]] != 'no_answer')
      
    } else {
      
      count_tbl <- inp_tbl
      
    }
    
    feature_counts <- count_tbl %>% 
      count(.data[[var_to_count]]) %>% 
      mutate(percent = n/sum(n) * 100, 
             total_n = sum(n))
    
    return(feature_counts)
    
  }

  count_feature_lst <- function(inp_tbl, var_to_count_vec, remove_na = T, positive_only = T) {
    
    ## wrapper for variable vectors
    
    count_tbl <- var_to_count_vec %>% 
      map(count_feature, 
          inp_tbl = inp_tbl, 
          remove_na = remove_na) %>% 
      map(set_names, 
          c('feature_strata', 
            'n', 
            'percent', 
            'total_n')) %>% 
      set_names(var_to_count_vec) %>% 
      map2_dfr(., names(.), 
               function(x, y) mutate(x, feature = y))
    
    if(positive_only) {
      
      count_tbl <- count_tbl %>% 
        filter(feature_strata == 'yes')
      
    }
    
    return(count_tbl)
    
  }
  
  pie_feature <- function(perc_tbl, plot_title = names(perc_tbl)[1], plot_subtitle = NULL, 
                          order_tbl = NULL, pie = T, repel = T, fill_colors = NULL, cust_theme = NULL) {
    
    ## plots the distribution of the given feature: takes an output of the count_feature function
    ## a tibble with order for plotting may be provided (names: plot_order, feature_strata)
    
    plotting_tbl <- perc_tbl %>% 
      set_names(c('feature_strata', 
                  'n', 
                  'percent', 
                  'total_n'))
    
    plot_tag <- paste('n =', plotting_tbl[['total_n']][1])
    
    if(!is.null(order_tbl)) {
      
      plotting_tbl <- plotting_tbl %>% 
        left_join(., 
                  order_tbl, 
                  by = 'feature_strata') %>% 
        arrange(desc(plot_order))
      
    } else {
      
      plotting_tbl <- plotting_tbl %>% 
        arrange(desc(feature_strata))
      
    }
    
    plotting_tbl <- plotting_tbl %>% 
      mutate(plot_lab = paste(signif(percent, 3), '%', sep = ''), 
             plot_y = cumsum(percent) - 0.5 * percent)
    
    if(!is.null(order_tbl)) {
      
      perc_plot <- plotting_tbl %>% 
        ggplot(aes(x = '', 
                   y = percent, plot_order, 
                   fill = reorder(feature_strata, plot_order)))
      
    } else {
      
      perc_plot <- plotting_tbl %>% 
        ggplot(aes(x = '', 
                   y = percent, 
                   fill = feature_strata))
      
    }
    
    if(is.null(cust_theme)) {
      
      plot_theme <- theme_minimal()
      
    } else {
      
      plot_theme <- cust_theme
      
    }
    
    perc_plot <- perc_plot + 
      geom_bar(stat = 'identity', 
               position = 'stack', 
               color = 'black') + 
      plot_theme + 
      theme(axis.title.x = element_blank(), 
            axis.text.x = element_blank(), 
            axis.ticks.x = element_blank()) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           y = '% of complete answers')
    
    if(repel) {
      
      perc_plot <- perc_plot + 
        geom_label_repel(aes(label = plot_lab, 
                             y = plot_y), 
                         size = 2.6, 
                         box.padding = 0.1, 
                         label.padding = 0.1, 
                         show.legend = F)
      
    } else {
      
      perc_plot <- perc_plot + 
        geom_label(aes(label = plot_lab, 
                       y = plot_y), 
                   size = 2.6, 
                   label.padding = 0.1, 
                   show.legend = F)
      
    }
    
    if(pie) {
      
      perc_plot <- perc_plot + 
        coord_polar(theta = 'y') + 
        theme(axis.line = element_blank(), 
              axis.title = element_blank(), 
              axis.text = element_blank(), 
              axis.ticks = element_blank())
      
    }
    
    if(!is.null(fill_colors)) {
      
      perc_plot <- perc_plot + 
        scale_fill_manual(values = fill_colors, 
                          name = '')
      
    }
    
    return(perc_plot)
    
    
  }
  
# statistics for numeric variables -----
  
  variable_stats <- function(inp_tbl, variable) {
    
    require(sciplot)
    
    ## returns a medley of stats for the given variable
    
    variable_vec <- inp_tbl[[variable]]
    complete_var_vec <- variable_vec[!is.na(variable_vec)]
    
    stat_vector <- tibble(variable = variable, 
                          n_total = length(variable_vec), 
                          n_complete = length(complete_var_vec), 
                          mean = mean(complete_var_vec, na.rm = T), 
                          sd = sd(complete_var_vec, na.rm = T), 
                          se = se(complete_var_vec, na.rm = T), 
                          median = median(complete_var_vec, na.rm = T), 
                          perc25 = quantile(complete_var_vec, 0.25, na.rm = T) %>% unname, 
                          perc75 = quantile(complete_var_vec, 0.75, na.rm = T) %>% unname, 
                          min = min(complete_var_vec, na.rm = T), 
                          max = max(complete_var_vec, na.rm = T))
    
    return(stat_vector)
    
  }
  
# variable analysis functions -----
  
  analyze_numeric <- function(inp_tbl, variable, split_var = NULL) {
    
    ## analyzes a numeric feature for each cohort subgroups
    ## defined by split_var separately and compares
    ## the median/means with U and T test, respectively
    ## currently works only for the split vars with two levels
    
    if(is.null(split_var)) {
      
      return(variable_stats(inp_tbl = inp_tbl, 
                            variable = variable))
      
      
    }
    
    ## model frame
    
    mod_frame <- inp_tbl[, c(variable, split_var)]
    
    ## descriptive stats
    
    stat_tbls <- inp_tbl %>% 
      dlply(split_var, 
            variable_stats, 
            variable = variable)
    
    ## test formula
    
    test_formula <- paste(variable, 
                          split_var, 
                          sep = '~') %>% 
      as.formula
    
    ## normality and variance equality checks
    
    norm_check <- inp_tbl[[variable]] %>% 
      shapiro.test
    
    var_check <- car::leveneTest(test_formula, 
                                 data = inp_tbl)
    
    check_tbl <- tibble(distribution = c('normality', 
                                         'homogeneity'), 
                        test = c('Shapiro', 
                                 'Levene'), 
                        p_value = c(norm_check$p.value, 
                                    var_check$`Pr(>F)`[1]))
    
    ## testing
    
    if(length(levels(factor(inp_tbl[[split_var]]))) == 2) {
      
      tests <- list(t = t.test, 
                    u = wilcox.test) %>% 
        map(function(x) x(test_formula, 
                          data = inp_tbl))
      
      ## testing summary
      
      tst_summary <- tests %>% 
        map_dfr(get_test_summary) %>% 
        mutate(test = c('t', 'u'), 
               variable = variable)
      
      
    } else {
      
      tests <- list(anova = aov, 
                    kruskal = kruskal.test) %>% 
        map(function(x) x(test_formula, 
                          data = inp_tbl))
      
      summary_kruskal <- tests$kruskal %>% 
        get_test_summary %>% 
        mutate(parameter2 = NA)
      
      summary_anova <- tests$anova %>% 
        summary
      
      summary_anova <- tibble(statistic = summary_anova[[1]][1, 4], 
                              parameter = summary_anova[[1]][1, 1], 
                              parameter2 = summary_anova[[1]][1, 2], 
                              p_value = summary_anova[[1]][1, 5])
      
      tst_summary <- rbind(summary_anova, 
                           summary_kruskal) %>% 
        mutate(variable = variable, 
               test = c('anova', 'kruskal')) %>%
        select(variable,
               test, 
               statistic, 
               parameter, 
               parameter2, 
               p_value)
      
    }
    
    
    ## output
    
    out_obj <- list(variable = variable, 
                    var_class = 'numeric', 
                    split_level_no = length(levels(factor(inp_tbl[[split_var]]))), 
                    mod_frame = mod_frame, 
                    stat_tables = stat_tbls, 
                    assumptions = check_tbl, 
                    test = tests, 
                    summary = tst_summary)
    
    attr(out_obj, 'class') <- 'var_analysis'
    
    return(out_obj)
    
  }
  
  analyze_factor <- function(inp_tbl, variable, split_var = NULL) {
    
    ## analyzes a factor feature for each collective defined by split_var separately and compares
    ## the distribution with Chi2 test
    
    if(is.null(split_var)) {
      
      return(count_feature(inp_tbl = inp_tbl, 
                           var_to_count = variable))
      
      
    }
    
    ## mod frame saved as NULL
    
    mod_frame = NULL
    
    ## descriptive statistics
    
    stat_tbls <- inp_tbl %>% 
      dlply(split_var, 
            count_feature, 
            var_to_count = variable)
    
    ## testing
    
    test <- stat_tbls %>% 
      map(function(x) x[, 1:2]) %>% 
      reduce(full_join, 
             by = names(stat_tbls[[1]])[1])
    
    test <- test[, 2:3] %>% 
      map_dfc(function(x) ifelse(is.na(x), 0, x)) %>% 
      chisq.test
    
    ## test summary
    
    summary <- test %>% 
      get_test_summary %>% 
      mutate(test = 'chi_sq', 
             variable = variable)
    
    ## output
    
    out_obj <- list(variable = variable, 
                    var_class = 'factor', 
                    split_level_no = length(levels(factor(inp_tbl[[split_var]]))), 
                    mod_frame = mod_frame, 
                    stat_tables = stat_tbls, 
                    assumptions = NA, 
                    test = test, 
                    summary = summary)
    
    attr(out_obj, 'class') <- 'var_analysis'
    
    return(out_obj)
    
  }
  
  analyze_feature <- function(inp_tbl, variable, split_var = NULL) {
    
    ## a wrapper around the functions coded above.
    ## for numeric variables: a set of descriptive statistics including mean, median, SD, IQR
    ## is computed separately for the North and South cohort and the means/medians compared 
    ## with the T test and Mann-Whitney/Wilcoxon test
    ## for the factor features: counts and percents of cases in each strata are calculated
    ## for each cohort separately and their distributions compared with Chi2 test

    if(class(inp_tbl[[variable]]) == 'numeric') {
      
      return(analyze_numeric(inp_tbl = inp_tbl, 
                             variable = variable, 
                             split_var = split_var))
      
    } else if(class(inp_tbl[[variable]]) == 'factor') {
      
      return(analyze_factor(inp_tbl = inp_tbl, 
                            variable = variable, 
                            split_var = split_var))
      
    } else {
      
      stop('The function handles only numeric and factor variables')
      
    }
    
  }
  
# plotting analysis results ------
  
  plot_analysis_factor <- function(analysis_obj, signif_digits = 3, 
                                   label = NULL, 
                                   y_lab = '% complete answers', legend_title = NULL, 
                                   labeller = NULL, fill_colors = NULL, pie = T, 
                                   cust_theme = NULL) {
    
    ## makes a bar or pie plot for the analysis object
    
    ## plot subtitle and tag
    
    plot_subtitle <- ifelse(analysis_obj$summary$p_value < 0.05, 
                            paste('p =', signif(analysis_obj$summary$p_value, 2)), 
                            'ns')
    
    if(!is.null(labeller)) {
      
      analysis_obj$stat_tables <- analysis_obj$stat_tables %>% 
        set_names(labeller[names(analysis_obj$stat_tables)])
      
    }
    
    plot_tag <- analysis_obj$stat_tables %>% 
      map2_chr(., names(.), function(x, y) paste(y, ': n = ', x$total_n[1])) %>% 
      paste(collapse = ', ') %>% 
      paste('\n', .)
    
    ## plotting table
    
    plotting_tbl <- analysis_obj$stat_tables %>% 
      map(arrange, 
          desc(.data[[analysis_obj$variable]])) %>% 
      map(mutate, 
          plot_lab = if(!pie) signif(percent, signif_digits) else paste(signif(percent, signif_digits), '%', sep = ''), 
          plot_y = cumsum(percent) - 0.5*percent) %>% 
      map2_dfr(., names(.), 
               function(x, y) mutate(x, split_var = y))
    
    ## plotting
    
    if(pie) {
      
      analysis_plot <- plotting_tbl %>% 
        ggplot(aes(x = '', 
                   y = percent, 
                   fill = .data[[analysis_obj$variable]])) + 
        geom_bar(color = 'black', 
                 stat = 'identity') + 
        geom_label_repel(aes(label = plot_lab, 
                             y = plot_y), 
                         size = 2.6, 
                         box.padding = 0.1, 
                         label.padding = 0.1, 
                         show.legend = F) + 
        facet_grid(. ~ split_var) + 
        coord_polar(theta = 'y')
      
      if(is.null(cust_theme)) {
        
        plot_theme <- theme_void()
        
      } else {
        
        plot_theme <- cust_theme
        
      }
      
      analysis_plot <- analysis_plot + 
        plot_theme + 
        theme(axis.title = element_blank(), 
              axis.ticks = element_blank(), 
              axis.line = element_blank(), 
              axis.text = element_blank(), 
              plot.tag.position = 'bottom') + 
        labs(title = label, 
             subtitle = plot_subtitle, 
             tag = plot_tag, 
             fill = legend_title)
      
    } else {
      
      analysis_plot <- plotting_tbl %>% 
        ggplot(aes(x = split_var, 
                   y = percent, 
                   fill = .data[[analysis_obj$variable]])) + 
        geom_bar(color = 'black', 
                 stat = 'identity') + 
        geom_label(aes(label = plot_lab, 
                       y = plot_y), 
                   size = 2.6, 
                   show.legend = F)
      
      if(is.null(cust_theme)) {
        
        plot_theme <- theme_classic()
        
      } else {
        
        plot_theme <- cust_theme
        
      }
      
      analysis_plot <- analysis_plot + 
        plot_theme + 
        theme(axis.title.x = element_blank(), 
              plot.tag.position = 'bottom') + 
        labs(title = label, 
             subtitle = plot_subtitle, 
             tag = plot_tag, 
             y = y_lab, 
             fill = legend_title)
      
    }
    
    if(!is.null(fill_colors)) {
      
      analysis_plot <- analysis_plot + 
        scale_fill_manual(values = fill_colors)
      
    }

    return(analysis_plot)
    
  }
  
  plot_analysis_numeric <- function(analysis_obj, signif_digits = 3, 
                                    label = NULL, 
                                    y_lab = analysis_obj$variable, legend_title = NULL, 
                                    labeller = NULL, fill_colors = NULL, violin = F, 
                                    cust_theme = NULL) {
    
    ## makes a box or violin plot based on the analysis object
    
    ## plot subtitle and tag
    
    if(analysis_obj$split_level_no == 2) {
      
      plot_subtitle <- ifelse(any(analysis_obj$summary$p_value < 0.05), 
                              paste('pT =', signif(analysis_obj$summary$p_value[1], 2), 
                                    ', pU = ', signif(analysis_obj$summary$p_value[2], 2), 
                                    sep = ''), 
                              'ns')
      
    } else {
      
      plot_subtitle <- ifelse(any(analysis_obj$summary$p_value < 0.05), 
                              paste('pANOVA =', signif(analysis_obj$summary$p_value[1], 2), 
                                    ', pK-W = ', signif(analysis_obj$summary$p_value[2], 2), 
                                    sep = ''), 
                              'ns')
      
    }
    
    
    
    if(!is.null(labeller)) {
      
      analysis_obj$stat_tables <- analysis_obj$stat_tables %>% 
        set_names(labeller[names(analysis_obj$stat_tables)])
      
    }
    
    plot_tag <- analysis_obj$stat_tables %>% 
      map2_chr(., names(.), function(x, y) paste(y, ': n = ', x$n_complete[1])) %>% 
      paste(collapse = ', ') %>% 
      paste('\n', .)
    
    ## summary table
    
    split_var <- names(analysis_obj$mod_frame)[2]
    
    if(!is.null(labeller)) {
      
      analysis_obj$mod_frame[[2]] <- labeller[analysis_obj$mod_frame[[2]]]
      
      
    }
    
    summ_tbl <- analysis_obj$stat_tables %>% 
      map2_dfr(., names(.), 
               function(x, y) mutate(x, strata = y)) %>% 
      select(strata, 
             median, 
             perc25, 
             perc75) %>% 
      set_names(c(split_var, 
                  'median',
                  'perc25', 
                  'perc75'))
    
    ## plotting
    
    analysis_plot <- analysis_obj$mod_frame %>% 
      ggplot(aes(x = .data[[split_var]], 
                 y = .data[[analysis_obj$variable]], 
                 fill = .data[[split_var]]))
    
    if(violin) {
      
      analysis_plot <- analysis_plot + 
        geom_violin(alpha = 0.25, 
                    show.legend = F)
      
    } else {
      
      analysis_plot <- analysis_plot  + 
        geom_boxplot(alpha = 0.25, 
                     outlier.color = NA, 
                     show.legend = F)
      
    }
    
    if(is.null(cust_theme)) {
      
      plot_theme <- theme_classic()
      
    } else {
      
      plot_theme <- cust_theme
      
    }
    
    analysis_plot <- analysis_plot + 
      geom_point(size = 2, 
                 shape = 21, 
                 color = 'black', 
                 alpha = 0.3, 
                 position = position_jitter(width = 0.15, 
                                            height = 0.1)) + 
      plot_theme + 
      theme(axis.title.x = element_blank(), 
            plot.tag.position = 'bottom') + 
      labs(title = label, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           y = y_lab, 
           fill = legend_title)
    
    if(violin) {
      
      analysis_plot <- analysis_plot + 
        geom_errorbar(data = summ_tbl, 
                      aes(y = median, 
                          ymin = perc25, 
                          ymax = perc75), 
                      width = 0.1, 
                      color = 'black', 
                      size = 1) +  
        geom_point(data = summ_tbl, 
                   aes(y = median), 
                   shape = 23, 
                   size = 3, 
                   color = 'black', 
                   fill = 'orangered2')
      
    }
    
    if(!is.null(fill_colors)) {
      
      analysis_plot <- analysis_plot + 
        scale_fill_manual(values = fill_colors)
      
    }
    
    return(analysis_plot)
    
  }
  
  plot_analysis <- function(analysis_obj, signif_digits = 3, 
                            label = NULL, 
                            y_lab = analysis_obj$variable, legend_title = NULL, 
                            labeller = NULL, fill_colors = NULL, cust_theme = NULL, ...) {
    
    ## a mother wrapper for the plotting functions declared above
    ## fill colors may be provided as a single string with colors separated by commas
    
    if(!is.null(fill_colors)) {
      
      if(stri_detect(fill_colors, fixed = ', ')) {
        
        fill_colors <- stri_split_fixed(fill_colors, 
                                        pattern = ', ') %>% 
          unlist
        
      }
      
    }
    
    if(analysis_obj$var_class == 'numeric') {
      
      return(plot_analysis_numeric(analysis_obj = analysis_obj, 
                                   signif_digits = signif_digits, 
                                   label = label, y_lab = y_lab, 
                                   legend_title = legend_title, 
                                   labeller = labeller, 
                                   fill_colors = fill_colors, 
                                   cust_theme = cust_theme, ...))
      
    } else {
      
      return(plot_analysis_factor(analysis_obj = analysis_obj, 
                                  signif_digits = signif_digits, 
                                  label = label, y_lab = y_lab, 
                                  legend_title = legend_title, 
                                  labeller = labeller, 
                                  fill_colors = fill_colors, 
                                  cust_theme = cust_theme, ...))
      
    }
    
  }
  
  
# making a summary table -----
  
  get_feature_summary <- function(analysis_obj, label = NA, signif_digits = 3) {
    
    ## makes a table with cohort characteristics which may be used in your paper
    
    if(class(analysis_obj) == 'list') {
      
      summary_tbl <- list(analysis_obj = analysis_obj, 
                          label = label) %>% 
        pmap_dfr(get_feature_summary, 
                 signif_digits = signif_digits)
      
      return(summary_tbl)
      
    }
    
    if(analysis_obj$var_class == 'numeric') {
      
      desc_stats <- analysis_obj$stat_tables %>% 
        map(mutate, 
            mean_cell = paste('mean(SD) = ', 
                              signif(mean, signif_digits), 
                              ' (', 
                              signif(sd, signif_digits), 
                              ')', sep = ''), 
            median_cell = paste('median(IQR) = ', 
                                signif(median, signif_digits), 
                                ' (', 
                                signif(perc25, signif_digits), 
                                ' - ', 
                                signif(perc75, signif_digits), 
                                ')', sep = ''), 
            min_max_cell = paste('Min, max = ', 
                                 signif(min, signif_digits), 
                                 ' - ', 
                                 signif(max, signif_digits), 
                                 sep = ''), 
            tbl_cell = paste(mean_cell, 
                             median_cell, 
                             min_max_cell, 
                             paste('ncomplete =', 
                                   n_complete), 
                             sep = '\n'))
      
      tbl_record <- tibble(variable = analysis_obj$variable, 
                           label = label, 
                           class = analysis_obj$var_class, 
                           strata1 = desc_stats[[1]]$tbl_cell, 
                           strata2 = desc_stats[[2]]$tbl_cell, 
                           p_T = analysis_obj$summary$p_value[1], 
                           p_U = analysis_obj$summary$p_value[2], 
                           p_chi = NA)
      
    } else {
      
      desc_stats <- analysis_obj$stat_tables %>% 
        map(function(x) list(strata = x[[1]], 
                             percent = x[[3]], 
                             number = x[[2]]) %>% 
              pmap_chr(function(strata, percent, number) paste(strata, 
                                                               ': ', 
                                                               signif(percent, signif_digits), 
                                                               '% (', 
                                                               number, 
                                                               ')', sep = '')) %>% 
              paste(collapse = '\n') %>% 
              paste('\nncomplete =', x[[4]][1]))
      
      tbl_record <- tibble(variable = analysis_obj$variable, 
                           label = label, 
                           class = analysis_obj$var_class, 
                           strata1 = desc_stats[[1]], 
                           strata2 = desc_stats[[2]], 
                           p_T = NA, 
                           p_U = NA, 
                           p_chi = analysis_obj$summary$p_value[1])
      
    }
    
    return(tbl_record)
    
  }
  
# extractor functions: obtaining counts and testing summaries from an analysis object -----
  
  extract_counts <- function(analysis_object) {
    
    ## extracts the merged count/percent table from an analysis object
    
    if(analysis_object$var_class != 'factor') {
      
      stop('No counts can be extracted for a numeric variable')
      
    }
    
    count_tbl <- analysis_object$stat_tables %>% 
      map2_dfr(., names(.), 
               function(x, y) mutate(x, split_var = y)) %>% 
      set_names(c('strata', 
                  'n', 
                  'percent', 
                  'total_n', 
                  'split_var')) %>%
      mutate(split_var = factor(split_var, 
                                attr(analysis_object$stat_tables, 'split_labels')[, 1]), 
             variable = analysis_object$variable)
    
    return(count_tbl %>% 
             as_tibble)
    
  }
  
  extract_test_summary <- function(analysis_object) {
    
    ## extracts a summary of an analysis object
    
    return(analysis_object$summary)
    
  }
  
# varia ------
  
  get_test_summary <- function(h_test_obj) {
    
    ## returns a basic summary of the h test object as a tibble
    ## containing the statistic, df and p value
    
    if(!is.null(h_test_obj$parameter)) {
      
      return(tibble(statistic = h_test_obj$statistic, 
                    parameter = h_test_obj$parameter, 
                    p_value = h_test_obj$p.value))
      
    } else {
      
      return(tibble(statistic = h_test_obj$statistic, 
                    parameter = NA, 
                    p_value = h_test_obj$p.value))
      
    }
    
  }
  
# END ----