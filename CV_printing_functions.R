# Helpers for printing sections of the CV
# Based on nstrayer/cv CV_printing_functions.R

library(tidyverse)
library(magrittr)
library(glue)

# CV printer object -------------------------------------------------------

#' Create a CV_Printer object for printing sections of a CV/Resume
#'
#' @param data_location Path to folder containing CSVs with CV data
#' @param pdf_mode Is the document being converted to PDF?
#' @param resume_mode Is the document a resume (shorter, filtered version)?
#' @param sheet_is_publicly_readable Can the google sheets data be read without auth?
#' @return A CV_Printer object
create_CV_object <- function(data_location,
                              pdf_mode = FALSE,
                              resume_mode = FALSE,
                              sheet_is_publicly_readable = TRUE) {

  cv <- list(
    pdf_mode = pdf_mode,
    links = c()
  )

  is_google_sheets_location <- stringr::str_detect(data_location, "docs.google.com")

  if(is_google_sheets_location){
    if(sheet_is_publicly_readable){
      # This tells googlesheets4 to not try to auth since we're reading a public sheet
      googlesheets4::gs4_deauth()
    } else {
      # To use private sheets see: https://googlesheets4.tidyverse.org/articles/auth.html
      googlesheets4::gs4_auth()
    }

    read_gsheet <- function(sheet_id){
      googlesheets4::read_sheet(data_location, sheet = sheet_id, skip = 1, col_types = "c")
    }
    cv$entries_data  <- read_gsheet(sheet_id = "entries")
    cv$skills        <- read_gsheet(sheet_id = "language_skills")
    cv$text_blocks   <- read_gsheet(sheet_id = "text_blocks")
    cv$contact_info  <- read_gsheet(sheet_id = "contact_info")
  } else {
    # Read from local CSVs, skipping first row (which is a description)
    cv$entries_data  <- readr::read_csv(paste0(data_location, "entries.csv"),         skip = 1, show_col_types = FALSE)
    cv$skills        <- readr::read_csv(paste0(data_location, "language_skills.csv"), skip = 1, show_col_types = FALSE)
    cv$text_blocks   <- readr::read_csv(paste0(data_location, "text_blocks.csv"),     skip = 1, show_col_types = FALSE)
    cv$contact_info  <- readr::read_csv(paste0(data_location, "contact_info.csv"),    skip = 1, show_col_types = FALSE)
  }

  extract_year <- function(date_col){
    date_col %>%
      stringr::str_extract("(4[0-9]{3}|[1-2][0-9]{3})") %>%
      as.integer()
  }

  parse_dates <- function(df){
    df %>%
      dplyr::mutate(
        start = extract_year(start),
        end   = extract_year(end)
      )
  }

  # Clean up entries data and filter to resume if needed
  cv$entries_data %<>%
    tidyr::unite(
      tidyr::starts_with('description'),
      col = "description_bullets",
      sep = "\n- ",
      na.rm = TRUE
    ) %>%
    dplyr::mutate(
      description_bullets = ifelse(description_bullets != "", paste0("- ", description_bullets), ""),
      start = ifelse(start == "NULL", NA, start),
      end   = ifelse(end   == "NULL", NA, end),
      in_resume = as.logical(in_resume)
    ) %>%
    parse_dates()

  if(resume_mode){
    cv$entries_data %<>% dplyr::filter(in_resume)
  }

  cv
}


# Printing functions ------------------------------------------------------

#' Print out a section of the CV
#'
#' @param cv CV_Printer object
#' @param section_id Character id of section to print
#' @param glue_template String template passed to glue_data
print_section <- function(cv, section_id, glue_template = "default") {

  if(glue_template == "default"){
    glue_template <- "
### {title}

{loc}

{institution}

{timeline}

{description_bullets}


"
  }

  section_data <- dplyr::filter(cv$entries_data, section == section_id)

  # Take ten longest entries to avoid weird issues
  # If there is no data for this section skip
  if(nrow(section_data) == 0) {
    return(invisible(NULL))
  }

  section_data %<>%
    dplyr::mutate(
      timeline = dplyr::case_when(
        is.na(end)    ~ glue("Current - {start}"),
        is.na(start)  ~ as.character(end),
        start == end  ~ as.character(end),
        TRUE          ~ glue("{end} - {start}")
      )
    )

  print(glue::glue_data(section_data, glue_template))

  invisible(cv)
}


#' Print out a text block from the CV
#'
#' @param cv CV_Printer object
#' @param label Label of text block to print
print_text_block <- function(cv, label){
  text_block <- dplyr::filter(cv$text_blocks, loc == label) %>%
    dplyr::pull(text)

  cat(sanitize_links(cv, text_block))

  invisible(cv)
}


#' Print contact information section of the CV
#'
#' @param cv CV_Printer object
print_contact_info <- function(cv){
  cv$contact_info %>%
    glue::glue_data("- <i class='fa fa-{icon}'></i> {contact}")  %>%
    print()

  invisible(cv)
}


#' Print out skill bars for the language skills section
#'
#' @param cv CV_Printer object
#' @param out_of Max value of skills
print_skill_bars <- function(cv, out_of = 5){
  bar_color      <- "#969696"
  bar_background <- "#d9d9d9"

  cv$skills %>%
    dplyr::filter(!is.na(level)) %>%
    dplyr::mutate(width_percent = round(100 * as.numeric(level) / out_of)) %>%
    glue::glue_data(
      "<div class = 'skill-bar'",
      "style = \"background:linear-gradient(to right,",
      "{bar_color} {width_percent}%,",
      "{bar_background} {width_percent}% 100%)\" >",
      "{skill}",
      "</div>"
    ) %>%
    print()

  invisible(cv)
}


#' Print links section at end of document (PDF mode)
#'
#' @param cv CV_Printer object
print_links <- function(cv){
  n_links <- length(cv$links)
  if (n_links > 0) {
    cat("
Links {data-concise=true}
--------------------------------------------------------------------------------

")
    purrr::walk2(cv$links, 1:n_links, function(link, index){
      print(glue::glue('{index}. {link}'))
    })
  }

  invisible(cv)
}


#' Sanitize links, replacing markdown links with superscripts in PDF mode
#'
#' @param cv CV_Printer object
#' @param text Character string to sanitize
sanitize_links <- function(cv, text){
  if(cv$pdf_mode){
    link_titles  <- stringr::str_extract_all(text, "(?<=\\[).+?(?=\\]\\()") %>% purrr::pluck(1)
    link_destinations <- stringr::str_extract_all(text, "(?<=\\]\\().+?(?=\\))") %>% purrr::pluck(1)

    if(length(link_titles) != 0){
      for(i in seq_along(link_titles)){
        cv$links <<- c(cv$links, link_destinations[i])
        n_links <- length(cv$links)

        full_link   <- paste0("[", link_titles[i], "](", link_destinations[i], ")")
        replacement <- paste0(link_titles[i], "^[", n_links, "]^")
        text <- stringr::str_replace(text, stringr::fixed(full_link), replacement)
      }
    }
  }
  text
}
