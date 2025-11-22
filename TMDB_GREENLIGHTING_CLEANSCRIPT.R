# ============================================================
# TMDB Cleaning Script
# Purpose: Clean and preprocess TMDB dataset (1990–2023)
# Author: Rohith Ramakrishnappa
###############################################

# --- 0) Setup ---------------------------------------------------------------
install.packages('tidyverse', dependencies=TRUE)
install.packages('readr', dependencies=TRUE)
install.packages('janitor', dependencies=TRUE)
install.packages('lubridate', dependencies=TRUE)
install.packages('stringr', dependencies=TRUE)
install.packages('purrr', dependencies=TRUE)
install.packages('dplyr', dependencies=TRUE)


library(tidyverse)
library(readr)
library(janitor)
library(lubridate)
library(stringr)
library(purrr)
library(dplyr)

# >>>>>> EDIT THIS <<<<<<
csv_path <- "TMDB_movie_dataset_1990_2023.csv"   # set to your combined Excel filename

# Toggles
SAVE_CSV        <- TRUE   # set TRUE to write CSVs at the end
EXCLUDE_ADULT   <- FALSE   # << keep adult rows (FALSE). Set TRUE to exclude by 'adult' flag.
FILTER_EXPLICIT <- FALSE   # optional: extra conservative keyword filter; default OFF

# Expected columns (as you provided)
expected <- c(
        "id","title","vote_average","vote_count","status","release_date","revenue","runtime",
        "adult","backdrop_path","budget","homepage","imdb_id","original_language","original_title",
        "overview","popularity","poster_path","tagline","genres","production_companies",
        "production_countries","spoken_languages","keywords"
)

# --- 1) Read & basic schema checks -----------------------------------------
raw <- read_csv(csv_path) %>% janitor::clean_names()

missing_cols <- setdiff(expected, names(raw))
extra_cols   <- setdiff(names(raw), expected)

cat("==== RAW LOAD ====/n")
cat("Rows:", nrow(raw), "  Cols:", ncol(raw), "/n")
if (length(missing_cols)) cat("Missing columns:", paste(missing_cols, collapse=", "), "/n")
if (length(extra_cols))   cat("Extra columns:   ", paste(extra_cols,   collapse=", "), "/n")

# Minimal guarantees
stopifnot(all(c("id","release_date","popularity") %in% names(raw)))

# --- 2) Utilities -----------------------------------------------------------
to_na_if_blank <- function(x) ifelse(str_squish(x) == "", NA, x)

# Robust release_date parser: allows ISO text OR Excel serials (e.g., 40374)
parse_release_date <- function(x){
        d <- suppressWarnings(lubridate::ymd(x))
        if (is.na(d)) {
                numx <- suppressWarnings(as.numeric(x))
                if (!is.na(numx)) d <- as.Date(numx, origin = "1899-12-30")
        }
        d
}

# Normalize comma-separated lists
norm_list <- function(x){
        x <- ifelse(is.na(x), NA_character_, x)
        x <- str_replace_all(x, "//s*,//s*", ",")
        x <- str_replace_all(x, ",{2,}", ",")
        x <- str_remove_all(x, "^,|,$")
        x
}

# --- 3) De-dup, type coercions, basic cleansing ----------------------------
num_cols <- c("vote_average","vote_count","revenue","runtime","budget","popularity")

tmdb_clean <- raw %>%
        # 3.1 Trim whitespace, empty-to-NA (combine into one mutate)
        mutate(across(where(is.character), ~ str_squish(.) %>% na_if(""))) %>%
        
        # 3.2 De-duplicate by id: keep row with more info
        mutate(non_na = rowSums(!is.na(.))) %>%
        arrange(desc(non_na)) %>%
        distinct(id, .keep_all = TRUE) %>%
        select(-non_na) %>%
        
        # 3.3 Coerce numerics only for character columns
        mutate(across(all_of(num_cols), ~ suppressWarnings(as.numeric(.)))) %>%
        
        # 3.4 Fix invalids / extreme runtimes
        mutate(
                budget  = ifelse(budget < 0, NA_real_, budget),
                revenue = ifelse(revenue < 0, NA_real_, revenue),
                runtime = ifelse(runtime <= 0 | runtime > 500, NA_real_, runtime)
        ) %>%
        
        # 3.5 Parse dates & derive time features (vectorized instead of map)
        mutate(
                release_date   = suppressWarnings(ymd(release_date)),
                release_year   = year(release_date),
                release_month  = month(release_date, label = TRUE, abbr = TRUE),
                release_season = case_when(
                        month(release_date) %in% c(12,1,2) ~ "Winter",
                        month(release_date) %in% c(3,4,5)  ~ "Spring",
                        month(release_date) %in% c(6,7,8)  ~ "Summer",
                        month(release_date) %in% c(9,10,11)~ "Fall",
                        TRUE ~ NA_character_
                ),
                release_decade = if_else(!is.na(release_year), (release_year %/% 10) * 10L, NA_integer_)
        ) %>%
        
        # 3.6 Adult flag
        mutate(adult_flag = str_to_lower(as.character(adult)) %in% c("1","true","t","yes","y"))
# If you ever need to exclude adult rows, set EXCLUDE_ADULT <- TRUE
if (EXCLUDE_ADULT) {
        tmdb_clean <- tmdb_clean %>% filter(!adult_flag)
}

# Optional explicit filter (OFF by default)
if (FILTER_EXPLICIT) {
        # Replace with your own conservative list if needed
        explicit_terms <- c("term1","term2")
        has_explicit <- function(txt) {
                if (is.na(txt)) return(FALSE)
                str_detect(str_to_lower(txt), paste0("//b(", paste(explicit_terms, collapse="|"), ")//b"))
        }
        tmdb_clean <- tmdb_clean %>%
                filter(!(has_explicit(title) | has_explicit(overview) | has_explicit(keywords)))
}

# --- 4) Normalize multi-value text fields ----------------------------------
tmdb_clean <- tmdb_clean %>%
        mutate(
                genres               = norm_list(genres),
                production_companies = norm_list(production_companies),
                production_countries = norm_list(production_countries),
                spoken_languages     = norm_list(spoken_languages),
                keywords             = norm_list(keywords),
                original_language    = str_to_lower(original_language)
        )

# --- 5) Quality flags (optional diagnostics) --------------------------------
tmdb_clean <- tmdb_clean %>%
        mutate(
                runtime_flag = case_when(
                        is.na(runtime)         ~ "missing",
                        runtime <= 0           ~ "nonpositive",
                        runtime > 360          ~ ">360min",
                        TRUE ~ "ok"
                ),
                budget_flag = case_when(
                        is.na(budget)          ~ "missing",
                        budget == 0            ~ "zero",
                        budget > 5e9           ~ ">5B",
                        TRUE ~ "ok"
                ),
                revenue_flag = case_when(
                        is.na(revenue)         ~ "missing",
                        revenue > 5e9          ~ ">5B",
                        TRUE ~ "ok"
                )
        )

# --- 6) Light engineered features (pre-release safe) -----------------------
tmdb_clean <- tmdb_clean %>%
        mutate(
                log_budget  = log1p(pmax(budget, 0)),
                runtime_bin = case_when(
                        is.na(runtime)         ~ NA_character_,
                        runtime <= 90          ~ "<=90",
                        runtime <= 120         ~ "91-120",
                        runtime <= 180         ~ "121-180",
                        TRUE                   ~ ">180"
                ),
                overview_len = nchar(ifelse(is.na(overview), "", overview)),
                tagline_len  = nchar(ifelse(is.na(tagline),  "", tagline)),
                spoken_n     = ifelse(is.na(spoken_languages), 0L, str_count(spoken_languages, ",") + 1L),
                country_n    = ifelse(is.na(production_countries), 0L, str_count(production_countries, ",") + 1L)
        )

# --- 7) Slim modeling view --------------------------------------------------
# Keep post-release fields for now; you’ll exclude them in your modeling recipe
keep_cols <- c(
        "id","title","original_title","overview","tagline",
        "release_date","release_year","release_month","release_season","release_decade",
        "popularity","vote_average","vote_count","status",
        "budget","log_budget","revenue","runtime","runtime_bin",
        "original_language","spoken_languages","spoken_n",
        "production_countries","country_n","production_companies",
        "genres","keywords","adult","adult_flag",
        "backdrop_path","poster_path","homepage","imdb_id"
)
tmdb_model_input <- tmdb_clean %>% select(any_of(keep_cols))

# --- 8) Console diagnostics -------------------------------------------------
cat("/n==== CLEAN SUMMARY ====/n")
cat("Rows:", nrow(tmdb_clean), "  Cols:", ncol(tmdb_clean), "/n")
tmdb_clean %>% count(adult_flag) %>% print(n=Inf)    # how many adult rows remain
tmdb_clean %>% count(runtime_flag, sort = TRUE) %>% print(n=Inf)
tmdb_clean %>% count(budget_flag,  sort = TRUE) %>% print(n=Inf)
tmdb_clean %>% count(revenue_flag, sort = TRUE) %>% print(n=Inf)

summary(tmdb_model_input$popularity)
summary(tmdb_model_input$log_budget)
summary(tmdb_model_input$runtime)

# --- 9) Optional CSV exports ------------------------------------------------
if (SAVE_CSV) {
        readr::write_csv(tmdb_clean,       "tmdb_1990_2023_clean_full.csv")
        readr::write_csv(tmdb_model_input, "tmdb_1990_2023_model_input.csv")
        cat("/nCSV files written: tmdb_clean_full.csv, tmdb_model_input.csv/n")
}

# Objects now in your environment:
# - tmdb_clean
# - tmdb_model_input
###############################################
