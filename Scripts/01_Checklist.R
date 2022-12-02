##  PROJECT: USAID/GH/OHA DQA Support for PEPFAR Countries
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: DQA Checklist & Timeframe
##  LICENCE: MIT
##  DATE:    2022-11-22
##  UPDATED: 2022-12-01

# Libraries ----

  library(tidyverse)
  library(readxl)
  library(grabr)
  library(glamr)
  library(glitr)
  library(janitor)
  library(glue)
  library(gt)
  library(ggtext)
  library(patchwork)
  library(lubridate)
  library(calendR)
  library(emojifont)
  library(extrafont)

  source("./Scripts/00_Utilities.R")

# GLOBALS ----

  ## Dirs ----

  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_graphics <- "Graphics"
  dir_images <- "Images"

  open_path(".")

  ## Params

  fy_starts <- 10
  fy_ends <- 9
  fy_qtr <- 3
  fy_mths <- fy_months(start = fy_starts)

# DATA ----

  pepfar_calender <- glamr::pepfar_data_calendar %>%
    mutate(
      start = case_when(
        quarter == 1 ~ fy_mths[1],
        quarter == 2 ~ fy_mths[fy_qtr * 1 + 1],
        quarter == 3 ~ fy_mths[fy_qtr * 2 + 1],
        quarter == 4 ~ fy_mths[fy_qtr * 3 + 1],
        TRUE ~ NA_integer_
      ),
      start = case_when(
        start %in% 10:12 ~ ymd(paste0(fiscal_year -1, str_pad(start, 2, "left", "0"), "01")),
        start %ni% 10:12 ~ ymd(paste0(fiscal_year, str_pad(start, 2, "left", "0"), "01"))
      ),
      end = case_when(
        quarter == 1 ~ fy_mths[fy_qtr * 1],
        quarter == 2 ~ fy_mths[fy_qtr * 2],
        quarter == 3 ~ fy_mths[fy_qtr * 3],
        quarter == 4 ~ fy_mths[fy_qtr * 4],
        TRUE ~ NA_integer_
      ),
      end = case_when(
        end %in% 10:12 ~ ceiling_date(ymd(paste0(fiscal_year -1, str_pad(end, 2, "left", "0"), "02")), "month") -1,
        end %ni% 10:12 ~ ceiling_date(ymd(paste0(fiscal_year, str_pad(end, 2, "left", "0"), "02")), "month") -1
      ),
    ) %>%
    relocate(start, end, .after = quarter)

  fy_years <- pepfar_calender %>%
    distinct(fiscal_year) %>%
    arrange(fiscal_year) %>%
    pull()

  prev_fy_days <- pepfar_calender %>%
    filter(fiscal_year == min(fiscal_year)) %>%
    distinct(quarter, start, end) %>%
    arrange(quarter) %>%
    mutate(begin = min(start), close = max(end)) %>%
    distinct(begin, close)

  curr_fy_days <- pepfar_calender %>%
    filter(fiscal_year == max(fiscal_year)) %>%
    distinct(quarter, start, end) %>%
    arrange(quarter) %>%
    mutate(begin = min(start), close = max(end)) %>%
    distinct(begin, close)

  curr_fy_days <- seq(from = curr_fy_days$begin,
                      to = curr_fy_days$close,
                      by = "1 day")

# VIZ ----

  ## Params for VIZ ----

  curr_fy_dqa_days1 <- curr_fy_days %>%
    map_chr(function(.day){

      #format(.day, "%m-%d")

      label <- pepfar_calender %>%
        filter(fiscal_year == max(fiscal_year) |
                 fiscal_year == min(fiscal_year) & quarter == 4) %>%
        rowwise() %>%
        mutate(check = case_when(
          format(ymd(.day), "%w") %in% c(0, 6) ~ "Weekend",
          format(ymd(.day), "%w") %ni% c(0, 6) && ymd(.day) %within% interval(ymd(entry_open), ymd(entry_close)) ~ paste0("FY", str_sub(fiscal_year, 3, 4), " Q", quarter, " - ", type),
          TRUE ~ NA_character_
        )) %>%
        ungroup() %>%
        filter(!is.na(check)) %>%
        pull(check) %>%
        first()

      print(paste(.day, "=>", label))

      return(label)
    })

  curr_fy_dqa_days1 %>%
    na.omit() %>%
    unique() %>%
    sort()

  curr_fy_dqa_days2 <- curr_fy_dqa_days1 %>%
    str_remove("FY\\d{2} Q\\d{1} - ") %>%
    str_to_sentence()

  curr_fy_dqa_days2 %>%
    na.omit() %>%
    unique() %>%
    sort()

  unique(na.omit(curr_fy_dqa_days2)) %>%
    sort()

  spec_colors1 <- c(glitr::si_palettes$siei[1:(length(unique(curr_fy_dqa_days1)) -2)],
                   glitr::trolley_grey_light)

  spec_colors2 <- c(glitr::si_palettes$siei[1:(length(unique(curr_fy_dqa_days2)) -2)],
                   glitr::trolley_grey_light)

  spec_colors3 <- c(glitr::burnt_sienna_light,
                    glitr::old_rose_light,
                    glitr::trolley_grey_light)

  ## Datim Reporting Calendar ----

  viz_calendar1 <- calendR(year = fy_years[2],
                         start_date = ymd(paste0(fy_years[1], str_pad(fy_starts, 2, "left", "0"), "01")),
                         end_date = ceiling_date(ymd(paste0(fy_years[2], str_pad(fy_ends, 2, "left", "0"), "02")), "month") -1,
                         title = paste0("FY", fy_years[2]),
                         subtitle = "USAID - DQA Checkpoints Dates",
                         orientation = "p",
                         start = "M",
                         special.days = "weekend",
                         special.col = glitr::trolley_grey_light)

  viz_calendar2 <- calendR(year = fy_years[2],
                           start_date = ymd(paste0(fy_years[1], str_pad(fy_starts, 2, "left", "0"), "01")),
                           end_date = ceiling_date(ymd(paste0(fy_years[2], str_pad(fy_ends, 2, "left", "0"), "02")), "month") -1,
                           title = paste0("FY", fy_years[2]),
                           subtitle = "USAID - DQA Checkpoints Dates",
                           orientation = "p",
                           start = "S",
                           special.days = curr_fy_dqa_days2,
                           special.col = spec_colors2)

  viz_calendar2

  viz_calendar3 <- calendR(year = fy_years[2],
                           start_date = ymd(paste0(fy_years[1], str_pad(fy_starts, 2, "left", "0"), "01")),
                           end_date = ceiling_date(ymd(paste0(fy_years[2], str_pad(fy_ends, 2, "left", "0"), "02")), "month") -1,
                           title = "",
                           #title = paste0("FY", fy_years[2]),
                           #title.size = 30,
                           #subtitle = "DATIM MER Data Reporting Dates",
                           #subtitle.size = 15,
                           orientation = "p",
                           start = "S",
                           special.days = curr_fy_dqa_days2,
                           special.col = spec_colors3,
                           col = grey20k,
                           lwd = 0.2,
                           text.size = 15,
                           months.size = 15,
                           weeknames.size = 8,
                           day.size = 6)

  viz_calendar3

  ggsave(filename = file.path(dir_images, "DQA CHECKLIST - Datim Calendar.png"),
         plot = viz_calendar3,
         width = 6, height = 6,
         units = "in",
         dpi = 300)

  ggsave(filename = file.path(dir_images, "DQA CHECKLIST - Datim Calendar.svg"),
         plot = viz_calendar3,
         width = 6, height = 6,
         units = "in",
         dpi = 300)

  ggsave(filename = file.path(dir_images, "DQA CHECKLIST - Datim Calendar.pdf"),
         plot = viz_calendar3#,
         #width = 6, height = 6,
         #units = "in",
         #dpi = 300
         )

  ## Datim Reporting DQA Timeline ----

  df_rep_calender1 <- pepfar_calender %>%
    filter(fiscal_year == max(fiscal_year) & quarter != 4|
             fiscal_year == min(fiscal_year) & quarter == 4) %>%
    mutate(period = paste0("FY", str_sub(fiscal_year, 3, 4), " Q", quarter)) %>%
    relocate(period, .before = 1) %>%
    select(period, type, starts_with("entry")) %>%
    pivot_longer(cols = starts_with("entry"),
                 names_to = "entry",
                 values_to = "dates") %>%
    mutate(entry = str_remove(entry, "entry_"))

  df_rep_calender1 <- df_rep_calender1 %>%
    group_by(period, type) %>%
    mutate(entry_open = dates[entry == "open"],
           entry_close = dates[entry == "close"]) %>%
    ungroup() %>%
    mutate(
      limit_color = case_when(
        type == "initial" & entry == "open" ~ old_rose_light,
        type == "clean" & entry == "open" ~ burnt_sienna_light,
        TRUE ~ trolley_grey_light
      ),
      segment_color = case_when(
        type == "initial" ~ old_rose_light,
        type == "clean" ~ burnt_sienna_light
      )
    )

  df_rep_calender1_dqa1 <- df_rep_calender1 %>%
    filter(type == "initial" & entry == "open" |
             type == "clean" & entry == "close") %>%
    mutate(
      dqa_label = case_when(
        entry == "open" ~ fontawesome("fa-database"),
        entry == "close" ~ fontawesome("fa-book"),
        TRUE ~ NA_character_
      )
    )

  df_rep_calender1_dqa2 <- df_rep_calender1_dqa1 %>%
    group_by(period) %>%
    mutate(entry_close = entry_close[type == "clean"]) %>%
    ungroup()

  df_rep_calender2 <- pepfar_calender %>%
    filter(fiscal_year == max(fiscal_year) & quarter != 4|
             fiscal_year == min(fiscal_year) & quarter == 4) %>%
    mutate(period = paste0("FY", str_sub(fiscal_year, 3, 4), " Q", quarter)) %>%
    relocate(period, .before = 1) %>%
    select(period, type, starts_with("entry")) %>%
    pivot_wider(names_from = type,
                values_from = starts_with("entry"))

  viz_check_points <- df_rep_calender1 %>%
    ggplot(aes(x = dates, y = 1)) +
    # Initial/Clean Timeline
    geom_segment(aes(xend = entry_close, yend = 1, color = segment_color),
                 size = 3) +
    geom_point(aes(fill = limit_color, color = limit_color),
               shape = 21, size = 4) +
    geom_text(data = filter(df_rep_calender1, entry == "open"),
              aes(y = 2.2, label = str_to_sentence(type)),
              size = 6, hjust = 0, color = trolley_grey) +
    geom_text(data = filter(df_rep_calender1, entry == "open"),
              aes(y = 0, label = entry_open),
              size = 6, hjust = 0, color = trolley_grey) +
    geom_text(data = filter(df_rep_calender1, entry == "close"),
              aes(y = .0, label = entry_close),
              size = 6, hjust = 1, color = trolley_grey) +
    # DQA Timeline
    geom_segment(data = df_rep_calender1_dqa2,
                 aes(x = dates, xend = entry_close, y = 4, yend = 4),
                     color = trolley_grey_light, size = 3) +
    geom_text(data = df_rep_calender1_dqa1,
              aes(x = dates, y = 4, label = dqa_label),
              family='fontawesome-webfont', size = 12, color = trolley_grey) +
    geom_text(data = filter(df_rep_calender1_dqa1, entry == "open"),
              aes(x = ymd(dates) + 10 , y = 4),
              label = fontawesome("fa-check-square-o"),
              family='fontawesome-webfont', size = 15, color = trolley_grey) +
    geom_text(data = filter(df_rep_calender1_dqa1, entry == "open"),
              aes(x = ymd(dates) + 20 , y = 4),
              label = fontawesome("fa-check-square-o"),
              family='fontawesome-webfont', size = 15, color = trolley_grey) +
    geom_text(data = filter(df_rep_calender1_dqa1, entry == "open"),
              aes(x = dates, y = 6),
              label = "Accept, Review & Document findings",
              size = 10, hjust = 0, color = trolley_grey) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_y_discrete() +
    scale_x_date(position = "top") +
    facet_wrap(~period, ncol = 1, scales = "free", strip.position = "top") +
    labs(x = "", y = "",
         #title = "DATIM - MER Data Entry and Review Check Points",
         #subtitle = "Accept data from IP, review for completeness and accuracy, and document findings and resolutions"
         ) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          plot.title = element_markdown(size = 30, color = usaid_black),
          plot.subtitle = element_text(size = 25, color = usaid_black),
          strip.placement = "outside",
          strip.text = element_text(size = 30, color = trolley_grey, hjust = .03, face = "bold"))


  viz_check_points

  ggsave(filename = file.path(dir_images, "DQA CHECKLIST - CheckPoints.png"),
         plot = viz_check_points,
         width = 5, height = 7,
         units = "in",
         scale = 1,
         dpi = 320)

  ggsave(filename = file.path(dir_images, "DQA CHECKLIST - CheckPoints.svg"),
         plot = viz_check_points,
         width = 5, height = 7,
         units = "in",
         scale = 1,
         dpi = 320)

  ggsave(filename = file.path(dir_images, "DQA CHECKLIST - CheckPoints.pdf"),
         plot = viz_check_points,
         width = 5, height = 7,
         units = "in",
         scale = 1,
         dpi = 320)

  ## Approval Flow ----

  appr_labels2 <- c(
    "pending at partner",
    "submitted by partner",
    "accepted by agency",
    "submitted by agency",
    "accepted by inter-agency",
    "submitted by inter-agency",
    "accepted by HQ"
  )

  appr_labels2 <- c(
    "pending at<br/><span style='color:#8980cb'>partner</span>",
    "submitted by<br/><span style='color:#8980cb'>partner</span>",
    "accepted by<br/><span style='color:#2057a7'>agency</span>",
    "submitted by<br/><span style='color:#2057a7'>agency</span>",
    "accepted by<br/><span style='color:#212721'>inter-agency</span>",
    "submitted by<br/><span style='color:#212721'>inter-agency</span>",
    "accepted by<br/><span style='color:#212721'>HQ</span>"
  )

  appr_step <- 1:length(appr_labels1)

  df_appr <- tibble(
    step = appr_step,
    desc = appr_labels2
  ) %>%
    mutate(to = lead(step, 1))

  viz_check_workflow <- df_appr %>%
    ggplot(aes(x=step, y = 1)) +
    geom_segment(aes(xend = to, yend = 1),
                 size = 2, linetype = "solid", color = trolley_grey_light) +
    geom_point(shape = 21, size = 20, fill = scooter_light, color = trolley_grey_light, stroke = 2) +
    geom_rect(aes(xmin = 1.5, xmax = 4.5, ymin = .5, ymax = 1.5),
              fill = NA, color = usaid_red, size = 1, linetype = "dashed") +
    geom_text(aes(label = step),
              size = 60/.pt, fontface = "bold", color = "white") +
    geom_text(aes(x = 2.50, y = 1.3),
                  label = fontawesome("fa-check-square-o"),
                  family='fontawesome-webfont',
                  size = 60/.pt, color = trolley_grey) +
    geom_text(aes(x = 3.50, y = 1.3),
              label = fontawesome("fa-book"),
              family='fontawesome-webfont',
              size = 60/.pt, color = trolley_grey) +
    geom_richtext(aes(y = 1, label = str_replace(desc, "^(\\S+) (\\S+) ", "\\1 \\2\n")),
              family='Source Sans Pro SemiBold',
              size = 18/.pt, vjust = 2, fontface = "bold",
              color = trolley_grey, label.color = NA) +
    geom_text(aes(x = 3, y = 1.5),
              label = "USAID's RESPONSABILY",
              family='Source Sans Pro SemiBold',
              size = 35/.pt, fontface = "bold",
              vjust = -.5, color = usaid_red) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0 , 2)) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank())

  viz_check_workflow

  ggsave(filename = file.path(dir_images, "DQA CHECKLIST - workflow.png"),
         plot = viz_check_workflow,
         width = 10,
         height = 5,
         units = "in",
         scale = 1,
         dpi = 320)

  ggsave(filename = file.path(dir_images, "DQA CHECKLIST - workflow.svg"),
         plot = viz_check_workflow,
         width = 10,
         height = 5,
         units = "in",
         scale = 1,
         dpi = 320)


  ggsave(filename = file.path(dir_images, "DQA CHECKLIST - workflow.pdf"),
         plot = viz_check_workflow,
         width = 10,
         height = 5,
         units = "in",
         scale = 1,
         dpi = 320)
