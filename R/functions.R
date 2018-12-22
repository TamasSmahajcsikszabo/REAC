# Functions ---------------------------------------------------------------
#' Tidies up the dataset and prepares later data analysis
#'
#' \code{shaker} prepares data for later analysis like calculating Attention Bias Variability (ABV) index.
#' @param data A dataset in tabular format.
#' @param grouping A list of strings determining by which cathegorical variable you would like to group your data.
#' @param gather A single character vector determining by which cathegorical variable you would like to gather your data.
#' @param value A single character vector identifying the column containing your measured variable like reaction time as RT.
#' @examples
#' shaker(example, grouping = c("year", "subject"), gather = "trial", value = "RT")
#' @seealso
#' \code{\link{winzer}} for winsorizing your data, \code{\link{ABV}} for calculating ABV-index
shaker <- function(data, grouping, gather, value, ...){

  gather <-  as.name(gather)
  value <-  as.name(value)

  indices <- vector("integer", length(col))
  names <- as_tibble(names(data)) %>%
    mutate(index = row_number())

  for (i in seq_along(grouping)) {
    indices[[i]] <- names$index[[i]]
  }

  selection <- colnames(data[-indices])

  tidied <- data %>%
    gather_(selection, key = gather, value = value) %>%
    as_data_frame()

  tidied = as_tibble(tidied) %>% distinct()
  tidied

}


#' Calculates ABV-index
#'
#' \code{ABV} calculates Attention Bias Variability (ABV) index.
#' @param data A dataset in tabular format.
#' @param grouping A list of strings determining by which cathegorical variable you would like to group your data.
#' @param trial A single character vector determining the name of your measured variable, like reaction time.
#' @param value A single character vector identifying the column containing your measured variable like reaction time as RT.
#' @param label A single character vector identifying the column containing trial types.
#' @param type A  character vector identifying the names of your trial types. First column shall address incongruent trials.
#' @param ID A  character vector identifying the name of column for subjects.
#' @param bin_width Single integer value which gives the number of consecutive trials handled in one bin.
#' @examples
#' ABV(example2, grouping = c("year", "subject"), trial = "trial", bin_width = 3, value = "RT", label = "label", type = c("Incongruent", "Congruent"), ID = "subject")
#' @seealso
#' \code{\link{winzer}} for winsorizing your data, \code{\link{shaker}} for reshape your data.
ABV <- function (data, bin_width, ID, trial, grouping, value, label, type) {
  ICVC <- data %>%
    mutate(trial_num = str_sub(trial, start = as.numeric(str_locate(trial, "[1,2,3,4,5,6,7,8,9]+")[,1]), end = length(trial))) %>%
    mutate(trial_num = as.integer(trial_num))

  new_grouping <- as.vector(unlist(c(strsplit(grouping, " "), "trial_num")))
  ICVC <- ICVC %>%
    arrange_at(vars(new_grouping))

  ICVC <- ICVC %>%
    group_by_at(vars(grouping)) %>%
    mutate(index = row_number()) %>%
    mutate(bin = (index %/% bin_width)+1) %>%
    ungroup()

  ICVC_grouping <- as.vector(unlist(c(strsplit(grouping, " "), "bin", "label")))
  loc <- str_locate(names(ICVC), value) %>%
    as_tibble() %>%
    mutate(index = row_number()) %>%
    filter(!is.na(start)) %>%
    dplyr::select(index) %>%
    unlist()

  ICVC <- ICVC %>%
    rename(new_RT = names(ICVC[,loc])) %>%
    group_by_at(vars(ICVC_grouping)) %>%
    summarise(mean_RT = mean(new_RT, na.rm = TRUE))

  ICVC <- ICVC %>%
    spread(length(ICVC)-1:length(ICVC), key=label, value=mean_RT) ### last two columns selection

  loc_1 <- str_locate(names(ICVC), type[1]) %>%
    as_tibble() %>%
    mutate(index = row_number()) %>%
    filter(!is.na(start)) %>%
    dplyr::select(index) %>%
    unlist()

  loc_2 <- str_locate(names(ICVC), type[2]) %>%
    as_tibble() %>%
    mutate(index = row_number()) %>%
    filter(!is.na(start)) %>%
    dplyr::select(index) %>%
    unlist()

  ICVC_index <-  as_tibble(ICVC[,loc_1] - ICVC[,loc_2]) %>% rename(ICVC = Incongruent)

  ICVC <- bind_cols(ICVC, ICVC_index)

  SD <- ICVC %>%
    ungroup() %>%
    group_by_at(vars(grouping)) %>%
    summarise(SD = sd(ICVC, na.rm = TRUE))

  ICVC <-
    ICVC %>% left_join(SD)

  mean_RT <-  data %>%
    mutate(trial_num = str_sub(trial, start = as.numeric(str_locate(trial, "[1,2,3,4,5,6,7,8,9]+")[,1]), end = length(trial))) %>%
    mutate(trial_num = as.integer(trial_num)) %>%
    arrange_(new_grouping) %>%
    group_by_at(grouping) %>%
    mutate(index = row_number()) %>%
    mutate(bin = (index %/% bin_width)+1) %>%
    ungroup() %>%
    group_by_at(grouping) %>%
    summarise(mean_RT = mean(RT, na.rm=TRUE))


  ICVC <- ICVC %>%
    left_join(mean_RT) %>%
    mutate(SD = as.double(SD)) %>%
    mutate(ABV = SD / mean_RT)

  ICVC

}


#' Winsorizes your data
#'
#' \code{winzer} performs winsorization on your data.
#' @param data A dataset in tabular format.
#' @param grouping A list of strings determining by which cathegorical variable you would like to group your data.
#' @param value A single character vector identifying the column containing your measured variable like reaction time as RT.
#' @param label A string name indicating the columns containing trials or observation points.
#' @param x Single value declaration for lower quantile of winsorizing.
#' @param y Single value declaration for upper quantile of winsorizing.
#' @param z Single value declaration for IQR multiplier.
#' @examples
#' winzer(example, grouping = c("year", "subject"), x = 0.25, y = 0.75, z = 1.5, label = "trial", value = "RT")
#' @seealso
#' \code{\link{ABV}} for ABV-index, \code{\link{shaker}} for reshape your data.
#' For extended winsorization see \code{\link[WRS2]{winvar}} and the win function family of the WRS package.
winzer <- function(data, x, y, z, grouping, value, label, ...){
loc <- str_locate(names(data), value) %>%
  as_tibble() %>%
  mutate(index = row_number()) %>%
  filter(!is.na(start)) %>%
  dplyr::select(index) %>% unlist()

data <- data %>%
    rename(value = names(data[,loc]))


  winsorized <- data %>%
    group_by_at(vars(grouping)) %>%
    mutate(
      "lower" = quantile(value, c(x), na.rm=TRUE),
      "upper" = quantile(value, c(y), na.rm=TRUE),
      IQR = z * IQR(value, na.rm=TRUE)) %>%
    mutate(
      upper = `upper` + IQR,
      lower = `lower` - IQR
    ) %>%
    mutate(
      above_upper = if_else(value > upper, "above", "normal"),
      below_lower = if_else(value < lower, "below", "normal")) %>%
    ungroup()

  ranked_upper <- winsorized %>%
    filter(above_upper == "normal") %>%
    group_by_at(vars(grouping)) %>%
    arrange(desc(value)) %>%
    top_n(1, value) %>%
    rename(upper_replacer = value) %>%
    select(grouping, upper_replacer) %>%
    ungroup()

  ranked_lower <- winsorized %>%
    filter(below_lower == "normal") %>%
    group_by_at(vars(grouping)) %>%
    arrange((value)) %>%
    top_n(-1, value) %>%
    rename(lower_replacer = value) %>%
    select(grouping, lower_replacer) %>%
    ungroup()

  winsorized_for_analysis <-
    winsorized %>%
    select(grouping, trial, value) %>%
    spread(key=trial, value=value ) %>%
    left_join(ranked_upper) %>%
    left_join(ranked_lower)

  gathered <- names(winsorized_for_analysis) %>% as_tibble() %>%  filter(str_detect(value, label)) %>% unlist() %>% as.vector()
  rest <- names(winsorized_for_analysis) %>% as_tibble() %>%  filter(!str_detect(value, label)) %>% unlist() %>% as.vector()

  winsorized_for_analysis <- winsorized_for_analysis %>%
    distinct() %>%
    group_by_at(vars(rest)) %>%
    gather(gathered, key=trial, value=value) %>%
    ungroup() %>%
    rename(temp = value) %>%
    mutate(
      RT = if_else(temp > upper_replacer, upper_replacer,
        if_else(temp < lower_replacer, lower_replacer, temp))) %>%
    select(grouping, trial, value) %>%
    spread(key=trial, value=value)

  winsorized_for_analysis
}
