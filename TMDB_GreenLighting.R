# ============================================================
# TMDB Pre-release Hit + Revenue Triage
# Author: Rohith Ramakrishnappa
# ============================================================

# -------------------------- 0) Libraries & Options ---------------------------
suppressPackageStartupMessages({
        library(tidyverse)
        library(tidymodels)
        library(dials)         # parameter sets & space-filling grids
        library(janitor)
        library(lubridate)
        library(stringr)
        library(scales)
        library(ggrepel)
        library(vip)
        library(yardstick)
        library(textrecipes)
        library(glmnet)
        library(patchwork)
        library(fmsb)
        library(openxlsx)
        library(parallel); library(doParallel)
        library(xgboost)
        library(purrr)
})
# Make "yes" the positive class for yardstick metrics
options(yardstick.event_first = "second")
tidymodels::tidymodels_prefer()
options(dplyr.summarise.inform = FALSE)
SAVE_SPREADSHEETS <- FALSE

# Optional: fixed subsample fallback for XGBoost (set to numeric like 0.8 to enable)
FIXED_SUBSAMPLE <- NA_real_ # NA -> tune sample_size; numeric in (0,1] -> fixed subsample via engine

# ------------------------------ 0a) Branding --------------------------------
wrap_lab <- function(x, width = 25) str_wrap(x, width = width)
brand_cols <- c(
        primary   = "#01B4E4", # TMDB Blue
        secondary = "#90CEA1", # TMDB Green
        dark      = "#0D253F",
        accent1   = "#F58518",
        accent2   = "#54A24B",
        accent3   = "#B279A2",
        gray      = "#7F7F7F"
)
brand_theme <- function(base_size = 13) {
        theme_minimal(base_size = base_size) +
                theme(
                        plot.title.position = "plot",
                        plot.title   = element_text(face = "bold", color = brand_cols[["dark"]]),
                        plot.subtitle= element_text(color = brand_cols[["dark"]]),
                        axis.title   = element_text(color = brand_cols[["dark"]]),
                        axis.text    = element_text(color = brand_cols[["dark"]]),
                        legend.title = element_text(color = brand_cols[["dark"]]),
                        legend.text  = element_text(color = brand_cols[["dark"]]),
                        panel.grid.minor = element_blank()
                )
}
theme_set(brand_theme(12))

# ------------------------- 0b) Parallel Backend ------------------------------
CORES_TO_USE <- max(1, parallel::detectCores(logical = TRUE) - 1)
cl <- parallel::makePSOCKcluster(CORES_TO_USE)
doParallel::registerDoParallel(cl)
on.exit(parallel::stopCluster(cl), add = TRUE)

# ---------------------------- 0c) Threshold Grid -----------------------------
thr_seq <- seq(0.05, 0.95, by = 0.01)

# ============================================================
# 1) Helpers (DRY)
# ============================================================
# List/data.frame/atomic to comma-separated text
flatten_to_text <- function(x) {
        if (is.null(x)) return("")
        if (is.atomic(x)) return(as.character(x))
        if (is.data.frame(x)) {
                if ("name" %in% names(x)) return(paste(na.omit(x$name), collapse = ", "))
                return(paste(na.omit(unlist(x)), collapse = ", "))
        }
        if (is.list(x)) return(paste(na.omit(unlist(x)), collapse = ", "))
        as.character(x)
}
# Clean production companies
clean_pc <- function(x) {
        x %>% as.character() %>% str_replace_all("\\s+", " ") %>% str_squish() %>% replace_na("")
}
# Studio/IP bucketing
studio_bucket_from_pc <- function(pc_string) {
        s <- str_to_lower(str_squish(as.character(pc_string)))
        case_when(
                str_detect(s, "\\bmarvel\\b") ~ "Marvel",
                str_detect(s, "\\bdc\\b|dc films|dc studios") ~ "DC",
                str_detect(s, "walt disney|\\bdisney\\b|\\bpixar\\b") ~ "Disney",
                str_detect(s, "warner bros") ~ "Warner",
                str_detect(s, "paramount") ~ "Paramount",
                str_detect(s, "universal pictures") ~ "Universal",
                str_detect(s, "columbia pictures|sony pictures") ~ "Sony",
                TRUE ~ "Other"
        )
}
# Shared feature engineering
add_shared_features <- function(df) {
        df %>%
                mutate(
                        release_date = suppressWarnings(ymd(release_date)),
                        release_year = coalesce(release_year, year(release_date)),
                        budget       = as.numeric(budget),
                        revenue      = as.numeric(revenue),
                        runtime      = as.numeric(runtime),
                        log_budget   = log1p(budget),
                        runtime_bin  = cut(runtime, breaks = c(0, 90, 120, Inf),
                                           labels = c("short","mid","long"), right = FALSE),
                        release_season = case_when(
                                month(release_date) %in% 3:5  ~ "Spring",
                                month(release_date) %in% 6:8  ~ "Summer",
                                month(release_date) %in% 9:11 ~ "Autumn",
                                TRUE ~ "Winter"
                        ),
                        release_decade = paste0(floor(release_year/10)*10, "s"),
                        month_release  = if_else(is.na(release_date), 6L, month(release_date)),
                        production_companies = clean_pc(production_companies),
                        production_countries = as.character(production_countries),
                        original_language    = as.character(original_language),
                        spoken_n  = as.numeric(spoken_n),
                        country_n = as.numeric(country_n),
                        studio_bucket = studio_bucket_from_pc(production_companies)
                )
}
# Recipe fragments (text & nominal)
add_text_steps <- function(rec,
                           max_genres = 25,
                           max_countries = 15,
                           max_companies = 35) {
        comma_tokenizer <- function(x) stringr::str_split(x, ",\\s*")
        rec %>%
                step_mutate(
                        genres = as.character(genres),
                        production_countries = as.character(production_countries),
                        production_companies = as.character(production_companies)
                ) %>%
                step_tokenize(genres, custom_token = comma_tokenizer) %>%
                step_tokenfilter(genres, max_tokens = max_genres) %>%
                step_tfidf(genres) %>%
                step_tokenize(production_countries, custom_token = comma_tokenizer) %>%
                step_tokenfilter(production_countries, max_tokens = max_countries) %>%
                step_tfidf(production_countries) %>%
                step_tokenize(production_companies, custom_token = comma_tokenizer) %>%
                step_tokenfilter(production_companies, max_tokens = max_companies) %>%
                step_tfidf(production_companies)
}
add_nominal_steps <- function(rec) {
        rec %>%
                step_mutate(
                        runtime_bin     = factor(runtime_bin),
                        original_language = factor(original_language),
                        release_season  = factor(release_season),
                        release_decade  = factor(release_decade),
                        studio_bucket   = factor(studio_bucket)
                ) %>%
                step_novel(runtime_bin, original_language, release_season, release_decade, studio_bucket) %>%
                step_unknown(runtime_bin, original_language, release_season, release_decade, studio_bucket) %>%
                step_dummy(runtime_bin, original_language, release_season, release_decade, studio_bucket, one_hot = TRUE) %>%
                step_zv(all_predictors())
}
# ENB utilities
enb_at_threshold <- function(prob, truth_hit, tau,
                             gain = GAIN_PER_TP, cost = COST_PER_SEL) {
        sel <- prob >= tau
        tp  <- sum(sel & truth_hit == "yes")
        sel_n <- sum(sel)
        gain * tp - cost * sel_n
}
tau_star_by_enb <- function(prob, truth_hit,
                            thr = thr_seq,
                            gain = GAIN_PER_TP, cost = COST_PER_SEL) {
        enb <- vapply(thr, function(t) enb_at_threshold(prob, truth_hit, t, gain, cost), numeric(1))
        thr[which.max(enb)]
}
f_beta_tau <- function(prob, truth_hit, thr = thr_seq, beta = 1) {
        scores <- vapply(thr, function(t) {
                sel <- prob >= t
                tp <- sum(sel & truth_hit == "yes")
                fp <- sum(sel & truth_hit == "no")
                fn <- sum(!sel & truth_hit == "yes")
                precision <- if (((tp+fp) > 0)) tp/(tp+fp) else NA_real_
                recall    <- if (((tp+fn) > 0)) tp/(tp+fn) else NA_real_
                if (is.na(precision) || is.na(recall)) return(NA_real_)
                (1+beta^2) * precision * recall / (beta^2 * precision + recall)
        }, numeric(1))
        thr[which.max(scores)]
}
# Genre-τ table & helpers
tau_genre_table <- function(prob, truth_hit, genres_chr,
                            thr = thr_seq,
                            gain = GAIN_PER_TP, cost = COST_PER_SEL,
                            n_min = 30, fallback_tau = NULL) {
        valid_long <- tibble(.pred_yes = prob, hit = truth_hit, genres = genres_chr) %>%
                tidyr::separate_rows(genres, sep = ",\\s*") %>%
                dplyr::filter(genres != "")
        tau_tbl <- valid_long %>%
                group_by(genres) %>%
                summarise(
                        n_valid = n(),
                        best_thr = {
                                enb_list <- vapply(thr, function(tau) {
                                        sel <- .pred_yes >= tau
                                        tp  <- sum(sel & hit == "yes")
                                        sel_n <- sum(sel)
                                        gain * tp - cost * sel_n
                                }, numeric(1))
                                thr[which.max(enb_list)]
                        },
                        ENB_raw = max(vapply(thr, function(tau) {
                                sel <- .pred_yes >= tau
                                tp  <- sum(sel & hit == "yes")
                                sel_n <- sum(sel)
                                gain * tp - cost * sel_n
                        }, numeric(1))),
                        .groups = "drop"
                ) %>%
                mutate(best_thr_stable = if_else(n_valid >= n_min, best_thr, coalesce(fallback_tau, 0.5)))
        tau_tbl
}
best_genre_safe <- function(genres_chr, tau_tbl) {
        if (is.null(genres_chr) || is.na(genres_chr) || genres_chr == "") return(NA_character_)
        gl <- unlist(str_split(as.character(genres_chr), ",\\s*"))
        ok <- gl[gl %in% tau_tbl$genres]
        if (length(ok) == 0) return(NA_character_)
        enb_map <- tau_tbl$ENB_raw[match(ok, tau_tbl$genres)]
        ok[which.max(enb_map)]
}
tau_used_from_genres <- function(genres_chr, tau_tbl, tau_star) {
        bg <- best_genre_safe(genres_chr, tau_tbl)
        if (!is.na(bg)) tau_tbl$best_thr_stable[tau_tbl$genres == bg] else tau_star
}
# Generic confusion-matrix plot (used in visuals)
cm_plot <- function(df, truth_col = hit, estimate_col, title = "Confusion Matrix") {
        cm_tbl <- yardstick::conf_mat(df, truth = {{truth_col}}, estimate = {{estimate_col}})
        cm_df  <- as_tibble(cm_tbl$table) %>% mutate(Truth = as.factor(Truth), Prediction = as.factor(Prediction))
        totals <- cm_df %>% group_by(Truth) %>% mutate(row_total = sum(n), rate = ifelse(row_total > 0, n/row_total, NA_real_)) %>% ungroup()
        ggplot(totals, aes(x = Prediction, y = Truth, fill = n)) +
                geom_tile(color = "white", linewidth = 0.7) +
                geom_text(aes(label = paste0(n, "\n(", scales::percent(rate, accuracy = 0.1), ")")), size = 4, color = brand_cols[["dark"]], lineheight = 0.9) +
                scale_fill_gradient(low = brand_cols[["secondary"]], high = brand_cols[["primary"]], name = "Count") +
                labs(title = title, x = "Predicted class", y = "Actual class") +
                theme_minimal(base_size = 13) +
                theme(legend.position = "right", panel.grid = element_blank())
}

calibration_curve <- function(df, p_col = .pred_yes, y_col = hit, n_bins = 10) {
        df %>%
                transmute(p = {{p_col}}, y = as.factor({{y_col}})) %>%
                mutate(bin = ntile(p, n_bins)) %>%
                group_by(bin) %>%
                summarise(p_mean = mean(p, na.rm = TRUE),
                          y_rate = mean(y == "yes", na.rm = TRUE), n = n(), .groups = "drop")
}

calibration_by_genre <- function(df, genres_col = genres, p_col = .pred_yes, y_col = hit, n_bins = 6, top_n = 6) {
        top_genres <- df %>%
                mutate(genres = as.character({{genres_col}})) %>%
                separate_rows(genres, sep = ",\\s*") %>%
                filter(!is.na(genres), genres != "") %>%
                count(genres, sort = TRUE) %>%
                slice_head(n = top_n) %>% pull(genres)
        
        df %>%
                mutate(genres = as.character({{genres_col}})) %>%
                separate_rows(genres, sep = ",\\s*") %>%
                filter(genres %in% top_genres, !is.na(genres), genres != "") %>%
                group_by(genres) %>%
                mutate(bin = ntile({{p_col}}, n_bins)) %>%
                group_by(genres, bin) %>%
                summarise(p_mean = mean({{p_col}}, na.rm = TRUE),
                          y_rate = mean({{y_col}} == "yes", na.rm = TRUE),
                          n = n(), .groups = "drop")
}

budget_vs_predrev_plot <- function(df, title_text, roi_bar = 1.5, rev_bar = NA_real_) {
        if (nrow(df) == 0) return(ggplot() + theme_void() + labs(title = paste(title_text, "— No rows")))
        if (is.na(rev_bar)) rev_bar <- quantile(df$predicted_revenue, 0.60, na.rm = TRUE)
        
        ggplot(df, aes(x = budget, y = predicted_revenue)) +
                geom_point(aes(color = p_hit, shape = studio_bucket), size = 3.4) +
                ggrepel::geom_text_repel(
                        aes(label = paste0(rank, ". ", title_s, " (", df$release_year[1], ")")),
                        size = 3, color = brand_cols[["dark"]], box.padding = 0.3, seed = 42, max.overlaps = 30
                ) +
                scale_color_gradient(low = brand_cols[["secondary"]], high = brand_cols[["primary"]],
                                     name = "p(hit)", labels = label_percent(accuracy = 1)) +
                scale_x_continuous(labels = label_dollar(accuracy = 1)) +
                scale_y_continuous(labels = label_dollar(accuracy = 1)) +
                labs(title = title_text, x = "Actual Budget ($)", y = "Predicted Revenue ($)") +
                theme_minimal(base_size = 13) +
                theme(legend.position = "right", panel.grid.minor = element_blank(), plot.title.position = "plot")
}# ============================================================
# 2) Load Data, Flatten Text Lists, Build Frames
# ============================================================
# df0: read and clean
# setwd("<your working dir>")
df0 <- readr::read_csv("tmdb_1990_2023_model_input.csv", show_col_types = FALSE) %>%
        janitor::clean_names()
# Flatten potential list-columns to comma-separated text BEFORE shared features
for (col in c("genres", "production_countries", "production_companies")) {
        if (col %in% names(df0)) {
                if (is.list(df0[[col]])) {
                        df0[[col]] <- purrr::map_chr(df0[[col]], flatten_to_text)
                } else {
                        df0[[col]] <- as.character(df0[[col]])
                }
                df0[[col]] <- stringr::str_squish(dplyr::coalesce(df0[[col]], ""))
        }
}
df0 <- df0 %>% add_shared_features()
actual_rev_key <- df0 %>%
        mutate(release_year = coalesce(release_year, year(release_date))) %>%
        select(title, release_year, revenue) %>% distinct()

df_cls <- df0 %>%
        filter(!is.na(budget), budget > 0, !is.na(revenue)) %>%
        mutate(hit = factor(if_else(revenue >= 2 * budget, "yes", "no"), levels = c("no","yes")))
rev_df <- df0 %>%
        filter(!is.na(budget), budget > 0, !is.na(revenue)) %>%
        mutate(log_revenue = log1p(revenue))

train_df  <- df_cls %>% filter(release_year <= 2018)
valid_df  <- df_cls %>% filter(release_year %in% 2019:2021)
test_df   <- df_cls %>% filter(release_year >= 2022)
train_rev <- rev_df %>% filter(release_year <= 2018)
valid_rev <- rev_df %>% filter(release_year %in% 2019:2021)
test_rev  <- rev_df %>% filter(release_year >= 2022)

# ============================================================
# 2a) CALIBRATION + LOGS (Option A)
# ============================================================
K_PNA <- 0.60
valid_econ <- valid_df %>%
        select(title, release_year, hit, revenue, budget) %>%
        mutate(pna_est = K_PNA * budget,
               profit  = revenue - budget - pna_est)
raw_cost <- median(valid_econ$pna_est, na.rm = TRUE)
raw_gain <- median(valid_econ$profit[valid_econ$hit == "yes"], na.rm = TRUE)
#COST_PER_SEL_FLOOR <- 2e6
#GAIN_PER_TP_FLOOR  <- 4e6
COST_PER_SEL <- max(raw_cost, COST_PER_SEL_FLOOR)
GAIN_PER_TP <- max(raw_gain, GAIN_PER_TP_FLOOR)
cat(sprintf(
        "\n[CALIBRATION]\n raw COST_PER_SEL = %s; raw GAIN_PER_TP = %s\n floor(COST) = %s; floor(GAIN) = %s\n => USED COST_PER_SEL = %s; USED GAIN_PER_TP = %s\n",
        scales::dollar(raw_cost), scales::dollar(raw_gain),
        scales::dollar(COST_PER_SEL_FLOOR), scales::dollar(GAIN_PER_TP_FLOOR),
        scales::dollar(COST_PER_SEL), scales::dollar(GAIN_PER_TP)
))
cat(sprintf("\n[CALIBRATED] COST_PER_SEL=%s; GAIN_PER_TP=%s (k=%.2f)\n",
            scales::dollar(COST_PER_SEL), scales::dollar(GAIN_PER_TP), K_PNA))

# ============================================================
# 3) Recipes & Models (Classification + Revenue)
# ============================================================
# --- Classification recipe (TOKENIZE FIRST, then nominal)
rec_cls <- recipe(
        hit ~ log_budget + runtime_bin + original_language +
                release_season + release_decade + genres + production_countries +
                spoken_n + country_n + production_companies + studio_bucket,
        data = train_df
) %>%
        add_text_steps() %>%
        add_nominal_steps()

# --- Revenue recipe (TOKENIZE FIRST, then nominal)
rec_rev <- recipe(
        log_revenue ~ log_budget + runtime_bin + original_language +
                release_season + release_decade + genres + production_countries +
                spoken_n + country_n + production_companies + studio_bucket +
                release_year + month_release,
        data = train_rev
) %>%
        add_text_steps() %>%
        add_nominal_steps()

# --- Specs: Logit, RF, XGB
logit_spec <- logistic_reg(penalty = tune(), mixture = tune()) %>% set_engine("glmnet")
rf_spec    <- rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
        set_engine("ranger", importance = "impurity", num.threads = CORES_TO_USE) %>%
        set_mode("classification")

# XGB classification spec
if (is.na(FIXED_SUBSAMPLE)) {
        xgb_cls_spec <- boost_tree(
                trees          = tune(),
                tree_depth     = tune(),
                learn_rate     = tune(),
                loss_reduction = tune(),
                min_n          = tune(),
                sample_size    = tune()
        ) %>%
                set_mode("classification") %>%
                set_engine("xgboost", nthread = CORES_TO_USE)
} else {
        xgb_cls_spec <- boost_tree(
                trees          = tune(),
                tree_depth     = tune(),
                learn_rate     = tune(),
                loss_reduction = tune(),
                min_n          = tune()
        ) %>%
                set_mode("classification") %>%
                set_engine("xgboost", nthread = CORES_TO_USE, subsample = FIXED_SUBSAMPLE)
}

wf_logit <- workflow() %>% add_model(logit_spec) %>% add_recipe(rec_cls)
wf_rf    <- workflow() %>% add_model(rf_spec)    %>% add_recipe(rec_cls)
wf_xgb_c <- workflow() %>% add_model(xgb_cls_spec) %>% add_recipe(rec_cls)

ctrl <- control_grid(parallel_over = "resamples", allow_par = TRUE, save_pred = TRUE, verbose = TRUE)
set.seed(42)
rs <- vfold_cv(train_df, v = 5, strata = hit)
metric_fun <- metric_set(roc_auc, pr_auc)

# --- XGB Classification params + grid
if (is.na(FIXED_SUBSAMPLE)) {
        xgb_params_cls <- parameters(
                trees(),
                tree_depth(),
                learn_rate(range = c(1e-3, 0.2)),
                loss_reduction(),
                min_n(),
                sample_prop(range = c(0.5, 1.0))   # proportion mapped to sample_size
        )
} else {
        xgb_params_cls <- parameters(
                trees(), tree_depth(), learn_rate(range = c(1e-3, 0.2)), loss_reduction(), min_n()
        )
}
set.seed(42)
xgb_grid_cls <- grid_space_filling(xgb_params_cls, size = 30)

# --- Tuning
set.seed(42)
logit_tuned <- tune_grid(wf_logit, rs,
                         grid = grid_regular(penalty(), mixture(), levels = 7),
                         metrics = metric_fun, control = ctrl)
set.seed(42)
rf_tuned    <- tune_grid(wf_rf, rs,
                         grid = grid_regular(mtry(range = c(5, 60)), min_n(), levels = 7),
                         metrics = metric_fun, control = ctrl)
set.seed(42)
xgb_c_tuned <- tune_grid(wf_xgb_c, rs,
                         grid = xgb_grid_cls,
                         metrics = metric_fun, control = ctrl)

# --- Select best by PR-AUC, tie-break ROC-AUC
best_logit <- logit_tuned %>%
        tune::collect_metrics() %>%
        dplyr::filter(.metric %in% c("pr_auc","roc_auc")) %>%
        tidyr::pivot_wider(names_from = .metric, values_from = mean) %>%
        arrange(desc(pr_auc), desc(roc_auc)) %>%
        slice(1) %>% select(penalty, mixture) %>% distinct()

best_rf <- rf_tuned %>%
        tune::collect_metrics() %>%
        dplyr::filter(.metric %in% c("pr_auc","roc_auc")) %>%
        tidyr::pivot_wider(names_from = .metric, values_from = mean) %>%
        arrange(desc(pr_auc), desc(roc_auc)) %>%
        slice(1) %>% select(mtry, min_n) %>% distinct()

best_xgb_c <- xgb_c_tuned %>%
        tune::collect_metrics() %>%
        dplyr::filter(.metric %in% c("pr_auc","roc_auc")) %>%
        tidyr::pivot_wider(names_from = .metric, values_from = mean) %>%
        arrange(desc(pr_auc), desc(roc_auc)) %>%
        slice(1) %>% select(trees, tree_depth, learn_rate, loss_reduction, min_n, dplyr::any_of("sample_size")) %>% distinct()

train_valid <- bind_rows(train_df, valid_df)
final_logit <- finalize_workflow(wf_logit, best_logit) %>% fit(train_valid)
final_rf    <- finalize_workflow(wf_rf,    best_rf)    %>% fit(train_valid)
final_xgb_c <- finalize_workflow(wf_xgb_c, best_xgb_c) %>% fit(train_valid)

# --- Model selection by VALID ENB@tau*
valid_pred_logit <- predict(final_logit, valid_df, type = "prob") %>% bind_cols(valid_df %>% select(hit))
valid_pred_rf    <- predict(final_rf,    valid_df, type = "prob") %>% bind_cols(valid_df %>% select(hit))
valid_pred_xgb   <- predict(final_xgb_c, valid_df, type = "prob") %>% bind_cols(valid_df %>% select(hit))

tau_logit <- tau_star_by_enb(valid_pred_logit$.pred_yes, valid_pred_logit$hit)
tau_rf    <- tau_star_by_enb(valid_pred_rf$.pred_yes,    valid_pred_rf$hit)
tau_xgb   <- tau_star_by_enb(valid_pred_xgb$.pred_yes,   valid_pred_xgb$hit)

enb_logit <- enb_at_threshold(valid_pred_logit$.pred_yes, valid_pred_logit$hit, tau_logit)
enb_rf    <- enb_at_threshold(valid_pred_rf$.pred_yes,    valid_pred_rf$hit, tau_rf)
enb_xgb   <- enb_at_threshold(valid_pred_xgb$.pred_yes,   valid_pred_xgb$hit, tau_xgb)

enb_table <- tibble(Model = c("Random Forest","Logistic Regression","XGBoost"), ENB = c(enb_rf, enb_logit, enb_xgb))
chosen_ix <- which.max(enb_table$ENB)
if (chosen_ix == 1) { final_cls <- final_rf;    chosen_model_name <- "Random Forest" }
if (chosen_ix == 2) { final_cls <- final_logit; chosen_model_name <- "Logistic Regression" }
if (chosen_ix == 3) { final_cls <- final_xgb_c; chosen_model_name <- "XGBoost" }
cat(sprintf("\n[SELECTED] %s (Highest VALID ENB among RF/Logit/XGB)\n", chosen_model_name))

# --- Recompute VALID predictions from chosen classifier
valid_pred <- predict(final_cls, valid_df, type = "prob") %>% bind_cols(valid_df %>% select(hit, genres))

# --- τ*, τ_F1, τ_F0.5
tau_star <- tau_star_by_enb(valid_pred$.pred_yes, valid_pred$hit)
tau_f1   <- f_beta_tau(valid_pred$.pred_yes, valid_pred$hit, beta = 1)
tau_f05  <- f_beta_tau(valid_pred$.pred_yes, valid_pred$hit, beta = 0.5)
cat(sprintf("Global tau*: %.3f; tau_F1: %.3f; tau_F0.5: %.3f\n", tau_star, tau_f1, tau_f05))

# --- Genre-τ table (fallback to tau*)
tau_genre_tbl <- tau_genre_table(valid_pred$.pred_yes, valid_pred$hit, as.character(valid_pred$genres),
                                 thr = thr_seq, gain = GAIN_PER_TP, cost = COST_PER_SEL,
                                 n_min = 30, fallback_tau = tau_star)
tau_genre <- setNames(tau_genre_tbl$best_thr_stable, tau_genre_tbl$genres)

# --- Revenue models (regression): RF + XGB, pick best by VALID RMSE (+ R^2 tie-break)
rev_rf_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
        set_engine("ranger", num.threads = CORES_TO_USE) %>%
        set_mode("regression")

if (is.na(FIXED_SUBSAMPLE)) {
        rev_xgb_spec <- boost_tree(
                trees          = tune(),
                tree_depth     = tune(),
                learn_rate     = tune(),
                loss_reduction = tune(),
                min_n          = tune(),
                sample_size    = tune()
        ) %>%
                set_mode("regression") %>%
                set_engine("xgboost", nthread = CORES_TO_USE)
} else {
        rev_xgb_spec <- boost_tree(
                trees          = tune(),
                tree_depth     = tune(),
                learn_rate     = tune(),
                loss_reduction = tune(),
                min_n          = tune()
        ) %>%
                set_mode("regression") %>%
                set_engine("xgboost", nthread = CORES_TO_USE, subsample = FIXED_SUBSAMPLE)
}

wf_rev_rf  <- workflow() %>% add_model(rev_rf_spec)  %>% add_recipe(rec_rev)
wf_rev_xgb <- workflow() %>% add_model(rev_xgb_spec) %>% add_recipe(rec_rev)

set.seed(42)
rs_rev <- vfold_cv(train_rev, v = 5)
metric_fun_rev <- metric_set(rmse, rsq)

if (is.na(FIXED_SUBSAMPLE)) {
        xgb_params_reg <- parameters(
                trees(), tree_depth(), learn_rate(range = c(1e-3, 0.2)), loss_reduction(), min_n(), sample_prop(range = c(0.5, 1.0))
        )
} else {
        xgb_params_reg <- parameters(
                trees(), tree_depth(), learn_rate(range = c(1e-3, 0.2)), loss_reduction(), min_n()
        )
}
set.seed(42)
xgb_grid_reg <- grid_space_filling(xgb_params_reg, size = 30)

set.seed(42)
rev_rf_tuned <- tune_grid(
        wf_rev_rf, rs_rev,
        grid = grid_regular(mtry(range = c(5, 70)), min_n(), levels = 7),
        metrics = metric_fun_rev, control = ctrl
)
set.seed(42)
rev_xgb_tuned <- tune_grid(
        wf_rev_xgb, rs_rev,
        grid = xgb_grid_reg,
        metrics = metric_fun_rev, control = ctrl
)

# Select best by RMSE (primary) + R^2 tie-break (robust to NA)
best_rev_rf <- rev_rf_tuned %>%
        collect_metrics() %>% filter(.metric %in% c("rmse","rsq")) %>%
        pivot_wider(names_from = .metric, values_from = mean) %>%
        mutate(rsq = coalesce(rsq, -Inf)) %>%
        arrange(rmse, desc(rsq)) %>% slice(1) %>%
        select(mtry, min_n) %>% distinct()

best_rev_xgb <- rev_xgb_tuned %>%
        collect_metrics() %>% filter(.metric %in% c("rmse","rsq")) %>%
        pivot_wider(names_from = .metric, values_from = mean) %>%
        mutate(rsq = coalesce(rsq, -Inf)) %>%
        arrange(rmse, desc(rsq)) %>% slice(1) %>%
        select(trees, tree_depth, learn_rate, loss_reduction, min_n, any_of("sample_size")) %>% distinct()

final_rev_rf  <- finalize_workflow(wf_rev_rf,  best_rev_rf)  %>% fit(bind_rows(train_rev, valid_rev))
final_rev_xgb <- finalize_workflow(wf_rev_xgb, best_rev_xgb) %>% fit(bind_rows(train_rev, valid_rev))

# Compare best tuned rows
rf_best  <- rev_rf_tuned  %>% collect_metrics() %>% filter(.metric %in% c("rmse","rsq")) %>%
        pivot_wider(names_from = .metric, values_from = mean) %>% mutate(rsq = coalesce(rsq, -Inf)) %>% arrange(rmse, desc(rsq)) %>% slice(1)
xgb_best <- rev_xgb_tuned %>% collect_metrics() %>% filter(.metric %in% c("rmse","rsq")) %>%
        pivot_wider(names_from = .metric, values_from = mean) %>% mutate(rsq = coalesce(rsq, -Inf)) %>% arrange(rmse, desc(rsq)) %>% slice(1)
rf_rmse <- rf_best$rmse;  rf_rsq <- if (is.infinite(rf_best$rsq)) NA_real_ else rf_best$rsq
xgb_rmse <- xgb_best$rmse; xgb_rsq <- if (is.infinite(xgb_best$rsq)) NA_real_ else xgb_best$rsq

if (xgb_rmse <= rf_rmse) { final_rev <- final_rev_xgb; rev_model_name <- "XGBoost" } else { final_rev <- final_rev_rf; rev_model_name <- "Random Forest" }
cat(sprintf("[SELECTED Revenue] %s (RMSE=%.4f, R^2=%s)\n",
            rev_model_name,
            ifelse(rev_model_name == "XGBoost", xgb_rmse, rf_rmse),
            ifelse(rev_model_name == "XGBoost", ifelse(is.na(xgb_rsq), "NA", sprintf("%.4f", xgb_rsq)),
                   ifelse(is.na(rf_rsq),  "NA", sprintf("%.4f", rf_rsq)))))

# --- Duan smearing factor from the chosen revenue model
trainvalid_rev <- bind_rows(train_rev, valid_rev)
pred_train_log <- predict(final_rev, trainvalid_rev) %>% rename(pred_log = .pred)
duan_factor <- exp(mean(trainvalid_rev$log_revenue - pred_train_log$pred_log, na.rm = TRUE))

# ============================================================
# 3a) SENSITIVITY: Gain–Cost grid (Option C)
# ============================================================

pred_valid <- predict(final_cls, valid_df, type = "prob") %>% bind_cols(valid_df %>% select(hit))
pred_test  <- predict(final_cls, test_df,  type = "prob") %>% bind_cols(test_df  %>% select(hit))

valid_counts <- purrr::map_df(thr_seq, function(tau) {
        sel <- pred_valid$.pred_yes >= tau
        tibble(threshold = tau, TP = sum(sel & pred_valid$hit == "yes"), Selected = sum(sel))
})

test_counts <- purrr::map_df(thr_seq, function(tau) {
        sel <- pred_test$.pred_yes >= tau
        tibble(threshold = tau, TP = sum(sel & pred_test$hit == "yes"), Selected = sum(sel))
})

gain_grid <- seq(3e6, 10e6, by = 0.5e6)
cost_grid <- seq(0.5e6, 3e6,  by = 0.25e6)

gc_grid <- tidyr::expand_grid(G = gain_grid, C = cost_grid)

sens_valid <- gc_grid %>%
        tidyr::crossing(valid_counts) %>%
        mutate(ENB = G * TP - C * Selected) %>%
        group_by(G, C) %>% slice_max(ENB, n = 1, with_ties = FALSE) %>% ungroup() %>%
        transmute(G, C, tau_star_GC = threshold, ENB_valid = ENB)

sens_test <- sens_valid %>%
        inner_join(test_counts, by = c("tau_star_GC" = "threshold")) %>%
        mutate(ENB_test = G * TP - C * Selected) %>%
        select(G, C, tau_star_GC, ENB_test)

best_gc_valid <- sens_valid %>% slice_max(ENB_valid, n = 1, with_ties = FALSE)
best_gc_test  <- sens_test  %>% slice_max(ENB_test,  n = 1, with_ties = FALSE)

cat(sprintf("\n[SENSITIVITY] Best on VALID: G=%s, C=%s, tau*=%0.3f\n",
            scales::dollar(best_gc_valid$G[1]), scales::dollar(best_gc_valid$C[1]), best_gc_valid$tau_star_GC[1]))
cat(sprintf("[SENSITIVITY] Best on TEST : G=%s, C=%s, tau*=%0.3f\n",
            scales::dollar(best_gc_test$G[1]),  scales::dollar(best_gc_test$C[1]),  best_gc_test$tau_star_GC[1]))


# ============================================================
# 4) Score TEST & Business Tables (clean upstream joins)
# ============================================================
# Classification predictions (carry budget directly from test_df)
pred_cls_test <- predict(final_cls, test_df, type = "prob") %>%
        dplyr::bind_cols(
                test_df %>% dplyr::select(
                        hit, title, release_year, genres, budget,
                        production_companies, studio_bucket, release_date, runtime,
                        original_language, production_countries, spoken_n, country_n
                )
        ) %>%
        mutate(genres = as.character(genres)) %>%
        rowwise() %>% mutate(
                best_genre = best_genre_safe(genres, tau_genre_tbl),
                tau_used   = ifelse(!is.na(best_genre), as.numeric(tau_genre[best_genre]), tau_star),
                pred_global= ifelse(.pred_yes >= tau_star, "yes", "no"),
                pred_genre = ifelse(.pred_yes >= tau_used, "yes", "no")
        ) %>% ungroup() %>%
        mutate(
                pred_f1  = ifelse(.pred_yes >= tau_f1,  "yes", "no"),
                pred_f05 = ifelse(.pred_yes >= tau_f05, "yes", "no"),
                ENB_global = case_when(
                        pred_global == "yes" & hit == "yes" ~ GAIN_PER_TP - COST_PER_SEL,
                        pred_global == "yes" & hit == "no"  ~ -COST_PER_SEL,
                        TRUE ~ 0
                ),
                ENB_genre = case_when(
                        pred_genre == "yes" & hit == "yes" ~ GAIN_PER_TP - COST_PER_SEL,
                        pred_genre == "yes" & hit == "no"  ~ -COST_PER_SEL,
                        TRUE ~ 0
                ),
                ENB_f1 = case_when(
                        pred_f1 == "yes" & hit == "yes" ~ GAIN_PER_TP - COST_PER_SEL,
                        pred_f1 == "yes" & hit == "no"  ~ -COST_PER_SEL,
                        TRUE ~ 0
                ),
                ENB_f05 = case_when(
                        pred_f05 == "yes" & hit == "yes" ~ GAIN_PER_TP - COST_PER_SEL,
                        pred_f05 == "yes" & hit == "no"  ~ -COST_PER_SEL,
                        TRUE ~ 0
                )
        )

# Predict revenue for TEST (only join predicted_revenue)
test_for_rev <- test_df %>% add_shared_features()
pred_rev_test <- predict(final_rev, test_for_rev) %>%
        rename(pred_log_revenue = .pred) %>%
        mutate(predicted_revenue = duan_factor * expm1(pred_log_revenue)) %>%
        bind_cols(test_for_rev %>% select(title, release_year)) %>%
        select(title, release_year, predicted_revenue) %>%
        distinct(title, release_year, .keep_all = TRUE)

pred_cls_test <- pred_cls_test %>%
        left_join(pred_rev_test, by = c("title","release_year"))

# Combined decisions (deduplicate actual_rev_key)
actual_rev_key_clean <- actual_rev_key %>% distinct(title, release_year, .keep_all = TRUE)
official_policy <- "pred_genre"
override_mult   <- 1.50
override_abs    <- 80e6

combined_decisions <- pred_cls_test %>%
        left_join(actual_rev_key_clean, by = c("title","release_year")) %>%
        mutate(
                actual = as.character(hit),
                `ENB-based decision` = .data[[official_policy]],
                `Per-title ENB` = case_when(
                        official_policy == "pred_f05"   ~ ENB_f05,
                        official_policy == "pred_f1"    ~ ENB_f1,
                        official_policy == "pred_genre" ~ ENB_genre,
                        official_policy == "pred_global"~ ENB_global,
                        TRUE ~ NA_real_
                ),
                `Predicted revenue` = predicted_revenue,
                `Actual revenue`    = revenue,
                Budget = budget,
                p_hit = .pred_yes,
                `Expected ENB if Market` = p_hit * GAIN_PER_TP - COST_PER_SEL,
                rev_to_budget = `Predicted revenue` / if_else(is.na(Budget) | Budget <= 0, 1, Budget),
                `Realized ENB if Market` = case_when(
                        `ENB-based decision` == "yes" & actual == "yes" ~ GAIN_PER_TP - COST_PER_SEL,
                        `ENB-based decision` == "yes" & actual == "no"  ~ -COST_PER_SEL,
                        TRUE ~ 0
                ),
                weak_ip = studio_bucket %in% c("Other"),
                `Override recommendation` = case_when(
                        `ENB-based decision` == "yes" ~ "Market (Model)",
                        is.na(`Predicted revenue`) | is.na(Budget) ~ "Data needed",
                        rev_to_budget >= override_mult & !weak_ip ~ "Review (High ROI)",
                        `Predicted revenue` >= override_abs & !weak_ip ~ "Review (High revenue)",
                        TRUE ~ "Do Not Market"
                )
        ) %>%
        select(
                title, release_year, studio_bucket, genres,
                `Actual (hit)` = actual,
                `ENB-based decision`, `Per-title ENB`,
                `Predicted revenue`, `Actual revenue`, Budget, p_hit,
                `Expected ENB if Market`, `Realized ENB if Market`, `Override recommendation`
        ) %>%
        arrange(desc(`Expected ENB if Market`))

cat("\n=== Combined decision table (policy = ", official_policy, ", model = ", chosen_model_name, ") ===\n", sep = "")
print(utils::head(combined_decisions, 20))

# ============================================================
# 5) Visuals & KPIs
# ============================================================

# --- Metrics across models for business view
roc_auc_logit <- logit_tuned %>% collect_metrics() %>% filter(.metric == "roc_auc") %>% summarise(mean = mean(mean)) %>% pull(mean)
roc_auc_rf    <- rf_tuned    %>% collect_metrics() %>% filter(.metric == "roc_auc") %>% summarise(mean = mean(mean)) %>% pull(mean)
roc_auc_xgb   <- xgb_c_tuned %>% collect_metrics() %>% filter(.metric == "roc_auc") %>% summarise(mean = mean(mean)) %>% pull(mean)
pr_auc_logit  <- logit_tuned %>% collect_metrics() %>% filter(.metric == "pr_auc")  %>% summarise(mean = mean(mean)) %>% pull(mean)
pr_auc_rf     <- rf_tuned    %>% collect_metrics() %>% filter(.metric == "pr_auc")  %>% summarise(mean = mean(mean)) %>% pull(mean)
pr_auc_xgb    <- xgb_c_tuned %>% collect_metrics() %>% filter(.metric == "pr_auc")  %>% summarise(mean = mean(mean)) %>% pull(mean)

# Portfolio F1/F0.5 (approx) for display from chosen classifier decisions
f1_score  <- max(yardstick::f_meas_vec(pred_cls_test$hit, factor(pred_cls_test$pred_f1,  levels = c("no","yes")), event_level = "second"), na.rm = TRUE)
f05_score <- max(yardstick::f_meas_vec(pred_cls_test$hit, factor(pred_cls_test$pred_f05, levels = c("no","yes")), event_level = "second", beta = 0.5), na.rm = TRUE)

roc_auc_label <- "Overall Accuracy"
pr_auc_label  <- "Precision under Pressure"
f1_label      <- "Balanced Success Rate"
f05_label     <- "Precision Priority"

model_perf <- tibble(
        Model = c("Logistic Regression", "Random Forest", "XGBoost"),
        !!roc_auc_label := c(roc_auc_logit, roc_auc_rf, roc_auc_xgb),
        !!pr_auc_label  := c(pr_auc_logit,  pr_auc_rf, pr_auc_xgb),
        !!f1_label      := c(f1_score,      f1_score,  f1_score),
        !!f05_label     := c(f05_score,     f05_score, f05_score)
)
model_perf_long <- model_perf %>% pivot_longer(cols = -Model, names_to = "Metric", values_to = "Score")

# Radar chart
radar_data <- model_perf %>% column_to_rownames("Model")
radar_max  <- rep(1, ncol(radar_data))
radar_min  <- rep(0, ncol(radar_data))
radar_df   <- rbind(radar_max, radar_min, radar_data)
radarchart(radar_df, axistype = 1,
           pcol = c(brand_cols["primary"], brand_cols["secondary"], brand_cols["accent3"]),
           pfcol = c(scales::alpha(brand_cols["primary"], 0.3),
                     scales::alpha(brand_cols["secondary"], 0.3),
                     scales::alpha(brand_cols["accent3"], 0.3)),
           plwd = 2, cglcol = brand_cols["dark"], axislabcol = brand_cols["dark"])
legend("topright", legend = rownames(radar_data),
       col = c(brand_cols["primary"], brand_cols["secondary"], brand_cols["accent3"]), lty = 1, lwd = 2)

# Lollipop
ggplot(model_perf_long, aes(x = Metric, y = Score, color = Model)) +
        geom_segment(aes(x = Metric, xend = Metric, y = 0, yend = Score), color = brand_cols[["dark"]]) +
        geom_point(size = 6) +
        scale_color_manual(values = c(brand_cols["primary"], brand_cols["secondary"], brand_cols["accent3"])) +
        labs(title = "Model Performance (Business View)", y = "Score") +
        theme_minimal(base_size = 14)

# Heatmap
ggplot(model_perf_long, aes(x = Metric, y = Model, fill = Score)) +
        geom_tile(color = "white") +
        geom_text(aes(label = round(Score, 2)), color = brand_cols[["dark"]], size = 5) +
        scale_fill_gradient(low = brand_cols["secondary"], high = brand_cols["primary"]) +
        labs(title = "Model Performance Heatmap", fill = "Score") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Confusion matrices for chosen classifier
pred_cls_test <- pred_cls_test %>%
        mutate(hit = factor(hit, levels = c("no","yes")),
               pred_global = factor(pred_global, levels = c("no","yes")),
               pred_genre  = factor(pred_genre,  levels = c("no","yes")),
               pred_f1     = factor(pred_f1,     levels = c("no","yes")),
               pred_f05    = factor(pred_f05,    levels = c("no","yes")))

p_global <- cm_plot(pred_cls_test, truth_col = hit, estimate_col = pred_global, title = expression("Confusion Matrix — Global " * tau^"*"))
p_genre  <- cm_plot(pred_cls_test, truth_col = hit, estimate_col = pred_genre,  title = expression("Confusion Matrix — Genre " * tau))
p_f1     <- cm_plot(pred_cls_test, truth_col = hit, estimate_col = pred_f1,     title = "Confusion Matrix — F1 Threshold")
p_f05    <- cm_plot(pred_cls_test, truth_col = hit, estimate_col = pred_f05,    title = "Confusion Matrix — F0.5 Threshold")
(p_global | p_genre) / (p_f1 | p_f05)

# ENB curve on VALID + markers + Genre-τ horizontal
enb_curve_valid <- purrr::map_df(thr_seq, function(tau) {
        sel <- valid_pred$.pred_yes >= tau
        tp  <- sum(sel & valid_pred$hit == "yes")
        sel_n <- sum(sel)
        tibble(threshold = tau, TP = tp, Selected = sel_n,
               ENB = GAIN_PER_TP * tp - COST_PER_SEL * sel_n)
})

pred_valid_genre <- valid_pred %>%
        mutate(genres = as.character(genres)) %>%
        rowwise() %>%
        mutate(best_genre = best_genre_safe(genres, tau_genre_tbl),
               tau_used   = ifelse(!is.na(best_genre), as.numeric(tau_genre[best_genre]), tau_star),
               pred_genre = ifelse(.pred_yes >= tau_used, "yes", "no")) %>%
        ungroup()

enb_genre_valid <- sum(case_when(
        pred_valid_genre$pred_genre == "yes" & pred_valid_genre$hit == "yes" ~ GAIN_PER_TP - COST_PER_SEL,
        pred_valid_genre$pred_genre == "yes" & pred_valid_genre$hit == "no"  ~ -COST_PER_SEL,
        TRUE ~ 0
), na.rm = TRUE)

ggplot(enb_curve_valid, aes(x = threshold, y = ENB/1e6)) +
        geom_line(color = brand_cols[["primary"]], linewidth = 1.2) +
        geom_vline(xintercept = tau_star, linetype = "dashed",  color = brand_cols[["dark"]]) +
        geom_vline(xintercept = tau_f1,   linetype = "dotdash", color = brand_cols[["accent2"]]) +
        geom_vline(xintercept = tau_f05,  linetype = "dotdash", color = brand_cols[["accent1"]]) +
        geom_hline(yintercept = enb_genre_valid/1e6, linetype = "longdash", color = brand_cols[["accent3"]]) +
        labs(
                title = "Expected Net Benefit vs Threshold (Validation)",
                subtitle = "Single ENB curve from chosen classifier (Option A economics); horizontal shows Genre‑τ portfolio ENB.",
                x = "Threshold (τ)", y = "ENB (Millions)"
        ) + theme_minimal(base_size = 14)


# ---- 1) Policy marker DF for legend ----
markers_df <- tibble::tibble(
        policy     = factor(c("Global τ*", "F1", "F0.5", "Genre‑τ ENB"),
                            levels = c("Global τ*", "F1", "F0.5", "Genre‑τ ENB")),
        threshold  = c(tau_star, tau_f1, tau_f05, NA_real_),  # NA for horizontal line
        yintercept = c(NA_real_, NA_real_, NA_real_, enb_genre_valid / 1e6)
)

# ---- 2) Brand‑aligned aesthetics for legend ----
policy_colors   <- c(
        "Global τ*"   = brand_cols[["dark"]],
        "F1"          = brand_cols[["accent2"]],
        "F0.5"        = brand_cols[["accent1"]],
        "Genre‑τ ENB" = brand_cols[["accent3"]]
)
policy_linetype <- c(
        "Global τ*"   = "dashed",
        "F1"          = "dotdash",
        "F0.5"        = "dotdash",
        "Genre‑τ ENB" = "longdash"
)

# ---- 3) Helper: ENB at/near a given τ from enb_curve_valid ----
enb_at_tau <- function(curve_df, tau) {
        curve_df %>%
                dplyr::mutate(.diff = abs(threshold - tau)) %>%
                dplyr::slice_min(.diff, n = 1, with_ties = FALSE) %>%
                dplyr::pull(ENB)
}

# Pull ENB values (in dollars) for the policy thresholds on VALID
enb_at_tau_star <- enb_at_tau(enb_curve_valid, tau_star)
enb_at_tau_f1   <- enb_at_tau(enb_curve_valid, tau_f1)
enb_at_tau_f05  <- enb_at_tau(enb_curve_valid, tau_f05)

# ---- 4) Main plot ----
p_enb <- ggplot(enb_curve_valid, aes(x = threshold, y = ENB / 1e6)) +
        # ENB curve
        geom_line(color = brand_cols[["primary"]], linewidth = 1.2) +
        
        # Vertical marker lines (create legend entries by mapping to 'policy')
        geom_vline(
                data = dplyr::filter(markers_df, !is.na(threshold)),
                aes(xintercept = threshold, color = policy, linetype = policy),
                linewidth = 0.9, show.legend = TRUE
        ) +
        
        # Horizontal Genre‑τ ENB
        geom_hline(
                data = dplyr::filter(markers_df, !is.na(yintercept)),
                aes(yintercept = yintercept, color = policy, linetype = policy),
                linewidth = 0.9, show.legend = TRUE
        ) +
        
        labs(
                title    = "Expected Net Benefit vs Threshold (Validation)",
                subtitle = "Single ENB curve from chosen classifier (Option A economics);\nHorizontal line shows Genre‑τ portfolio ENB.",
                x        = "Threshold (τ)",
                y        = "ENB (Millions)",
                color    = NULL,
                linetype = NULL
        ) +
        scale_color_manual(values = policy_colors) +
        scale_linetype_manual(values = policy_linetype) +
        theme_minimal(base_size = 14) +
        theme(
                legend.position    = "right",
                plot.title.position = "plot"
        )

# ---- 5) Annotation labels: τ and ENB for all policies ----
y_top <- max(enb_curve_valid$ENB, na.rm = TRUE) / 1e6

p_enb <- p_enb +
        # τ* (Global) – show τ and ENB
        annotate(
                "label", x = tau_star, y = y_top,
                label = paste0("τ* = ", round(tau_star, 3),
                               "\nENB = ", scales::dollar(enb_at_tau_star)),
                size = 3, fill = "white", color = policy_colors[["Global τ*"]]
        ) +
        # τ_F1 – show τ and ENB
        annotate(
                "label", x = tau_f1, y = y_top * 0.92,
                label = paste0("τ_F1 = ", round(tau_f1, 3),
                               "\nENB = ", scales::dollar(enb_at_tau_f1)),
                size = 3, fill = "white", color = policy_colors[["F1"]]
        ) +
        # τ_F0.5 – show τ and ENB
        annotate(
                "label", x = tau_f05, y = y_top * 0.84,
                label = paste0("τ_F0.5 = ", round(tau_f05, 3),
                               "\nENB = ", scales::dollar(enb_at_tau_f05)),
                size = 3, fill = "white", color = policy_colors[["F0.5"]]
        ) +
        # Genre‑τ portfolio ENB – show ENB as a left‑aligned label on the line
        annotate(
                "label",
                x = min(enb_curve_valid$threshold, na.rm = TRUE) + 0.03,
                y = enb_genre_valid / 1e6,
                label = paste0("Genre‑τ ENB = ", scales::dollar(enb_genre_valid)),
                hjust = 0, size = 3, fill = "white", color = policy_colors[["Genre‑τ ENB"]]
        )

# Render
print(p_enb)




# VIP plots for chosen models
if (chosen_model_name == "XGBoost") {
        vip::vip(final_xgb_c %>% extract_fit_parsnip(), num_features = 20) + ggtitle("Top 20 Features — XGBoost (Classification)")
} else if (chosen_model_name == "Random Forest") {
        vip::vip(final_rf %>% extract_fit_parsnip(), num_features = 20) + ggtitle("Top 20 Features — Random Forest (Classification)")
} else {
        vip::vip(final_logit %>% extract_fit_parsnip(), num_features = 20) + ggtitle("Top 20 Features — GLMNET (Classification)")
}

# Sensitivity heatmaps (Option C)
# Validation heatmap
ggplot(sens_valid, aes(x = G/1e6, y = C/1e6, fill = ENB_valid/1e6)) +
        geom_tile() +
        geom_text(aes(label = paste0("τ*=", round(tau_star_GC, 2))), color = "white", size = 3) +
        scale_fill_viridis_c(option = "C", name = "ENB (M)") +
        scale_x_continuous(breaks = unique(gain_grid/1e6)) +
        scale_y_continuous(breaks = unique(cost_grid/1e6)) +
        labs(title = "Gain–Cost Sensitivity (Validation)",
             subtitle = "ENB on VALID with τ*(G,C) chosen per cell",
             x = "Gain per TP (M)", y = "Cost per Selection (M)") +
        theme_minimal(base_size = 13)

# Test heatmap
ggplot(sens_test, aes(x = G/1e6, y = C/1e6, fill = ENB_test/1e6)) +
        geom_tile() +
        geom_text(aes(label = paste0("τ*=", round(tau_star_GC, 2))), color = "white", size = 3) +
        scale_fill_viridis_c(option = "C", name = "ENB (M)") +
        scale_x_continuous(breaks = unique(gain_grid/1e6)) +
        scale_y_continuous(breaks = unique(cost_grid/1e6)) +
        labs(title = "Gain–Cost Sensitivity (Test)",
             subtitle = "τ*(G,C) chosen on VALID, evaluated on TEST",
             x = "Gain per TP (M)", y = "Cost per Selection (M)") +
        theme_minimal(base_size = 13)

# ============================================================
# 5b) Advanced Visuals — Per‑Title ENB curve & Genre Contribution
# ============================================================

plot_single_title_enb <- function(test_df_row, final_cls, tau_star, tau_f1, tau_f05,
                                  tau_genre_tbl, gain = GAIN_PER_TP, cost = COST_PER_SEL,
                                  thr = thr_seq) {
        stopifnot(nrow(test_df_row) == 1)
        p_hat <- predict(final_cls, test_df_row, type = "prob")$.pred_yes[1]
        genres_chr <- as.character(test_df_row$genres[1])
        best_genre <- best_genre_safe(genres_chr, tau_genre_tbl)
        tau_used <- if (!is.na(best_genre)) as.numeric(tau_genre_tbl$best_thr_stable[tau_genre_tbl$genres == best_genre]) else tau_star
        enb_genre_point <- if (p_hat >= tau_used) gain * p_hat - cost else 0
        
        single_enb <- tibble(
                threshold = thr,
                ENB = if_else(threshold <= p_hat, gain * p_hat - cost, 0)
        )
        
        ggplot(single_enb, aes(threshold, ENB/1e6)) +
                geom_line(color = brand_cols[["primary"]], linewidth = 1.2) +
                geom_vline(xintercept = tau_star, linetype = "dashed",  color = brand_cols[["dark"]]) +
                geom_vline(xintercept = tau_f1,   linetype = "dotdash", color = brand_cols[["accent2"]]) +
                geom_vline(xintercept = tau_f05,  linetype = "dotdash", color = brand_cols[["accent1"]]) +
                geom_vline(xintercept = p_hat,    linetype = "solid",  color = brand_cols[["gray"]], alpha = 0.7) +
                annotate("label", x = p_hat, y = max(single_enb$ENB)/1e6,
                         label = paste0("p(hit) = ", round(p_hat, 3)), size = 3, fill = "white") +
                geom_point(aes(x = tau_used, y = enb_genre_point/1e6), color = brand_cols[["accent3"]], size = 3) +
                annotate("label", x = tau_used, y = enb_genre_point/1e6,
                         label = paste0("Genre‑τ ENB: ", scales::dollar(enb_genre_point)), size = 3, fill = "white",
                         color = brand_cols[["accent3"]]) +
                labs(
                        title = paste0("Per‑Title ENB vs Threshold — ", test_df_row$title[1], " (", test_df_row$release_year[1], ")"),
                        subtitle = "ENB(τ) is constant for τ ≤ p(hit) and 0 for τ > p(hit). Policy markers shown.",
                        x = "Threshold (τ)", y = "ENB (Millions)"
                ) + theme_minimal(base_size = 14)
}

plot_top_genres_enb <- function(pred_cls_test, top_n = 15) {
        genre_contrib <- pred_cls_test %>%
                mutate(decision = pred_genre) %>%  # using Genre‑τ policy
                separate_rows(genres, sep = ",\\s*") %>%
                group_by(genres) %>%
                summarise(
                        n = n(),
                        ENB = sum(case_when(
                                decision == "yes" & hit == "yes" ~ GAIN_PER_TP - COST_PER_SEL,
                                decision == "yes" & hit == "no"  ~ -COST_PER_SEL,
                                TRUE ~ 0
                        ), na.rm = TRUE), .groups = "drop"
                ) %>% arrange(desc(ENB)) %>% slice_head(n = top_n)
        
        ggplot(genre_contrib, aes(x = reorder(genres, ENB), y = ENB/1e6)) +
                geom_col(fill = brand_cols[["accent3"]]) +
                coord_flip() +
                geom_text(aes(label = scales::dollar(ENB)), hjust = -0.1, size = 3, color = brand_cols[["dark"]]) +
                labs(title = paste0("Top ", top_n, " Genres by ENB (Genre‑τ policy, Test)"),
                     x = NULL, y = "ENB (Millions)") +
                theme_minimal(base_size = 14)
}

# Example usage for advanced visuals
example_row <- test_df %>% filter(title == "Mission: Impossible - Dead Reckoning Part One", release_year == 2023) %>% slice(1)
if (nrow(example_row) == 1) {
        p_single_enb <- plot_single_title_enb(example_row, final_cls, tau_star, tau_f1, tau_f05, tau_genre_tbl,
                                              gain = GAIN_PER_TP, cost = COST_PER_SEL, thr = thr_seq)
        print(p_single_enb)
}

p_genre_enb <- plot_top_genres_enb(pred_cls_test, top_n = 15)
print(p_genre_enb)

# ============================================================

# --- Portfolio ENB by Policy (Test) ---
policy_enb <- tibble(
        policy = c("Global τ*", "Genre τ", "F1", "F0.5"),
        ENB    = c(
                sum(pred_cls_test$ENB_global, na.rm = TRUE),
                sum(pred_cls_test$ENB_genre,  na.rm = TRUE),
                sum(pred_cls_test$ENB_f1,     na.rm = TRUE),
                sum(pred_cls_test$ENB_f05,    na.rm = TRUE)
        )
)

p_policy <- ggplot(policy_enb, aes(x = reorder(policy, ENB), y = ENB/1e6, fill = policy)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = scales::dollar(ENB)), hjust = -0.05, size = 4, color = brand_cols[["dark"]]) +
        coord_flip(clip = "off") +
        expand_limits(y = max(policy_enb$ENB/1e6) * 1.12) +
        scale_fill_manual(values = c(brand_cols[["primary"]], brand_cols[["accent3"]], brand_cols[["accent2"]], brand_cols[["accent1"]])) +
        labs(title = "Portfolio ENB by Policy (Test ≥ 2022)", x = NULL, y = "ENB (Millions)") +
        theme_minimal(base_size = 14)
print(p_policy)


# --- Top Genres by ENB (Genre-τ policy, Test) ---
genre_contrib <- pred_cls_test %>%
        mutate(decision = pred_genre) %>%
        separate_rows(genres, sep = ",\\s*") %>%
        group_by(genres) %>%
        summarise(
                n = n(),
                ENB = sum(case_when(
                        decision == "yes" & hit == "yes" ~ GAIN_PER_TP - COST_PER_SEL,
                        decision == "yes" & hit == "no"  ~ -COST_PER_SEL,
                        TRUE ~ 0
                ), na.rm = TRUE), .groups = "drop"
        ) %>% arrange(desc(ENB)) %>% slice_head(n = 10)

p_genre_enb <- ggplot(genre_contrib, aes(x = reorder(genres, ENB), y = ENB/1e6)) +
        geom_col(fill = brand_cols[["accent3"]]) +
        coord_flip() +
        geom_text(aes(label = scales::dollar(ENB)), hjust = -0.1, size = 3, color = brand_cols[["dark"]]) +
        labs(title = "Top 10 Genres by ENB (Genre-τ policy, Test)", x = NULL, y = "ENB (Millions)") +
        theme_minimal(base_size = 14)
print(p_genre_enb)

# 6) 2023 Shortlists & KPIs
# ============================================================

biz23 <- pred_cls_test %>%
        filter(release_year == 2023) %>%
        left_join(actual_rev_key, by = c("title","release_year")) %>%
        mutate(
                `p(hit)` = .pred_yes,
                exp_enb_if_market = .pred_yes * GAIN_PER_TP - COST_PER_SEL,
                decision_tau_star = factor(pred_global, levels = c("no","yes")),
                decision_genre_tau= factor(pred_genre,  levels = c("no","yes")),
                actual_hit = factor(hit, levels = c("no","yes")),
                realized_enb_tau_star = ENB_global,
                realized_enb_genre_tau= ENB_genre,
                PredRevenue = predicted_revenue,
                ActualRevenue = revenue
        )

shortlist_2023 <- combined_decisions %>%
        filter(release_year == 2023) %>%
        mutate(
                `Rank by Expected ENB` = rank(-`Expected ENB if Market`, ties.method = "min"),
                `Rank by Pred Revenue` = rank(-`Predicted revenue`, ties.method = "min")
        ) %>%
        arrange(desc(`Expected ENB if Market`))

# Top-10 TP picked (τ*)
top10_hits_tau_star <- biz23 %>%
        filter(decision_tau_star == "yes", actual_hit == "yes") %>%
        arrange(desc(exp_enb_if_market)) %>%
        slice_head(n = 10) %>%
        transmute(
                title, genres,
                `p(hit)` = round(`p(hit)`, 3),
                Budget = dollar(budget),
                `Predicted Rev` = dollar(PredRevenue),
                `Expected ENB`  = dollar(exp_enb_if_market),
                `Realized ENB`  = dollar(realized_enb_tau_star)
        )

# Top-10 TP picked (Genre-τ)
top10_hits_genre_tau <- biz23 %>%
        filter(decision_genre_tau == "yes", actual_hit == "yes") %>%
        arrange(desc(exp_enb_if_market)) %>%
        slice_head(n = 10) %>%
        transmute(
                title, genres,
                `p(hit)` = round(`p(hit)`, 3),
                Budget = dollar(budget),
                `Predicted Rev` = dollar(PredRevenue),
                `Expected ENB`  = dollar(exp_enb_if_market),
                `Realized ENB`  = dollar(realized_enb_genre_tau)
        )

cat("\n=== TOP 10 HITS PICKED — τ* (2023) ===\n"); print(top10_hits_tau_star, n = Inf)
cat("\n=== TOP 10 HITS PICKED — Genre-τ (2023) ===\n"); print(top10_hits_genre_tau, n = Inf)

# KPIs (portfolio)
kpi_2023_tau_star <- biz23 %>%
        filter(decision_tau_star == "yes") %>%
        summarise(
                Selected = n(),
                TP = sum(actual_hit == "yes", na.rm = TRUE),
                Precision = ifelse(Selected > 0, TP/Selected, NA_real_),
                Portfolio_ENB = sum(realized_enb_tau_star, na.rm = TRUE),
                Total_Predicted_Revenue = sum(PredRevenue, na.rm = TRUE),
                Total_Actual_Revenue    = sum(ActualRevenue, na.rm = TRUE)
        ) %>%
        mutate(across(c(Portfolio_ENB, Total_Predicted_Revenue, Total_Actual_Revenue), dollar))

kpi_2023_genre_tau <- biz23 %>%
        filter(decision_genre_tau == "yes") %>%
        summarise(
                Selected = n(),
                TP = sum(actual_hit == "yes", na.rm = TRUE),
                Precision = ifelse(Selected > 0, TP/Selected, NA_real_),
                Portfolio_ENB = sum(realized_enb_genre_tau, na.rm = TRUE),
                Total_Predicted_Revenue = sum(PredRevenue, na.rm = TRUE),
                Total_Actual_Revenue    = sum(ActualRevenue, na.rm = TRUE)
        ) %>%
        mutate(across(c(Portfolio_ENB, Total_Predicted_Revenue, Total_Actual_Revenue), dollar))

cat("\n=== KPIs — 2023 τ* ===\n"); print(kpi_2023_tau_star)
cat("\n=== KPIs — 2023 Genre-τ ===\n"); print(kpi_2023_genre_tau)

# ============================================================
# 7) Title-level Scoring Helpers & Examples
# ============================================================

score_row <- function(row_df, final_cls, final_rev,
                      tau_star, tau_f1, tau_f05, tau_genre_tbl, duan_factor,
                      override_mult = 1.50, override_abs = 80e6,
                      gain = GAIN_PER_TP, cost = COST_PER_SEL,
                      actual_rev_key = NULL) {
        
        nr <- add_shared_features(row_df)
        p  <- predict(final_cls, nr, type = "prob")$.pred_yes[1]
        
        tau_used <- tau_used_from_genres(nr$genres[1], tau_genre_tbl, tau_star)
        
        pred_rev_log <- tryCatch(predict(final_rev, nr)$.pred, error = function(e) NA_real_)
        pred_rev     <- if (is.na(pred_rev_log)) NA_real_ else duan_factor * expm1(pred_rev_log)
        
        pred_global <- ifelse(p >= tau_star, "yes", "no")
        pred_genre  <- ifelse(p >= tau_used, "yes", "no")
        pred_f1     <- ifelse(p >= tau_f1,  "yes", "no")
        pred_f05    <- ifelse(p >= tau_f05, "yes", "no")
        
        expected_enb <- p * gain - cost
        studio_bkt   <- studio_bucket_from_pc(nr$production_companies[1])
        weak_ip      <- studio_bkt %in% "Other"
        roi          <- pred_rev / max(nr$budget[1], 1)
        
        override_rec <- case_when(
                pred_global == "yes" & pred_genre == "yes" & pred_f1 == "yes" ~ "Market (Model)",
                is.na(pred_rev) | is.na(nr$budget[1]) ~ "Data needed",
                roi >= override_mult & !weak_ip ~ "Review (High ROI)",
                pred_rev >= override_abs & !weak_ip ~ "Review (High revenue)",
                TRUE ~ "Do Not Market"
        )
        
        out <- tibble(
                title = nr$title[1],
                release_year = nr$release_year[1],
                genres = nr$genres[1],
                budget = nr$budget[1],
                `p(hit)` = round(p, 3),
                tau_star = tau_star, tau_used = tau_used, tau_f1 = tau_f1, tau_f05 = tau_f05,
                pred_global, pred_genre, pred_f1, pred_f05,
                `Expected ENB if Market` = expected_enb,
                `Predicted revenue` = pred_rev,
                studio_bucket = studio_bkt,
                `Override recommendation` = override_rec
        )
        
        if (!is.null(actual_rev_key)) {
                out <- out %>%
                        left_join(actual_rev_key, by = c("title","release_year")) %>%
                        rename(`Actual revenue` = revenue)
        }
        out
}

score_existing_title <- function(title_pattern,
                                 match_mode = c("regex", "exact", "fixed"),
                                 year = NULL,
                                 official_policy = "pred_genre",
                                 override_mult = 1.50,
                                 override_abs  = 80e6,
                                 format = c("pretty","raw")) {
        match_mode <- match.arg(match_mode)
        format     <- match.arg(format)
        
        td <- test_df
        if (!is.null(year)) td <- dplyr::filter(td, release_year == year)
        td <- td %>%
                mutate(.title_lc = str_to_lower(title), .pat_lc = str_to_lower(title_pattern)) %>%
                dplyr::filter(case_when(
                        match_mode == "exact" ~ .title_lc == .pat_lc,
                        match_mode == "fixed" ~ str_detect(.title_lc, stringr::fixed(.pat_lc, ignore_case = TRUE)),
                        TRUE ~ str_detect(.title_lc, .pat_lc)
                )) %>%
                select(-.title_lc, -.pat_lc)
        
        if (nrow(td) == 0) {
                message("No test rows matched: ", title_pattern, if (!is.null(year)) paste0(" in ", year) else "")
                return(tibble())
        }
        
        out <- purrr::map_df(seq_len(nrow(td)), function(i) {
                score_row(td[i,], final_cls, final_rev,
                          tau_star, tau_f1, tau_f05, tau_genre_tbl, duan_factor,
                          override_mult, override_abs,
                          actual_rev_key = actual_rev_key)
        })
        
        if (format == "raw") return(out)
        out %>%
                transmute(
                        title, release_year, genres, studio_bucket,
                        budget = dollar(budget),
                        `p(hit)`, tau_star, tau_used, tau_f1, tau_f05,
                        pred_global, pred_genre, pred_f1, pred_f05,
                        `Expected ENB if Market` = dollar(`Expected ENB if Market`),
                        `Predicted revenue`      = dollar(`Predicted revenue`),
                        `Actual revenue`         = ifelse(is.na(`Actual revenue`), NA, dollar(`Actual revenue`)),
                        `Override recommendation`
                )
}

score_new_title <- function(new_row,
                            official_policy = "pred_genre",
                            override_mult = 1.50,
                            override_abs  = 80e6,
                            format = c("pretty","raw")) {
        format <- match.arg(format)
        out <- score_row(new_row, final_cls, final_rev,
                         tau_star, tau_f1, tau_f05, tau_genre_tbl, duan_factor,
                         override_mult, override_abs,
                         actual_rev_key = NULL)
        if (format == "raw") return(out)
        out %>%
                transmute(
                        title, release_year, genres, studio_bucket,
                        budget = dollar(budget),
                        `p(hit)`, tau_star, tau_used, tau_f1, tau_f05,
                        pred_global, pred_genre, pred_f1, pred_f05,
                        `Expected ENB if Market` = dollar(`Expected ENB if Market`),
                        `Predicted revenue`      = dollar(`Predicted revenue`),
                        `Override recommendation`
                )
}

# ============================================================
# 8) Example Calls
# ============================================================

# Single title scoring (existing TEST title)
pred_title_df <- score_existing_title("Mission: Impossible - Dead Reckoning Part One", match_mode = "exact", year = 2023)
print(pred_title_df, n = Inf)

# 2023 Top-5 plots by policy (Budget vs PredRev)
YEAR_FOCUS <- 2023; TOP_N <- 5
shorten <- function(x, n = 28) ifelse(nchar(x) <= n, x, paste0(substr(x, 1, n-1), "…"))

base_2023 <- pred_cls_test %>%
        filter(release_year == YEAR_FOCUS) %>%
        transmute(
                title, release_year, title_s = shorten(title, 26), genres,
                studio_bucket = ifelse(is.na(as.character(studio_bucket)), "Other", as.character(studio_bucket)),
                p_hit = as.numeric(.pred_yes),
                predicted_revenue = as.numeric(predicted_revenue),
                budget = as.numeric(budget),
                hit = as.character(hit),
                pred_genre = as.character(pred_genre),
                pred_global= as.character(pred_global)
        ) %>%
        filter(!is.na(predicted_revenue), predicted_revenue > 0,
               !is.na(budget), budget > 0) %>%
        arrange(desc(p_hit), desc(predicted_revenue), desc(budget)) %>%
        distinct(title, release_year, .keep_all = TRUE)

gate_hits <- function(df) df %>% filter(hit == "yes")

top_for_policy <- function(df, policy_col) {
        df %>%
                gate_hits() %>%
                filter(.data[[policy_col]] == "yes") %>%
                arrange(desc(p_hit), desc(predicted_revenue), desc(predicted_revenue / budget)) %>%
                slice_head(n = TOP_N) %>%
                mutate(rank = row_number())
}

top_genre_tau_2023 <- top_for_policy(base_2023, "pred_genre")
top_tau_star_2023  <- top_for_policy(base_2023, "pred_global")

p_genre_2023_br <- budget_vs_predrev_plot(top_genre_tau_2023, "Top‑5 — Genre‑τ (Actual Budget vs Predicted Revenue) — 2023")
p_tstar_2023_br <- budget_vs_predrev_plot(top_tau_star_2023,  "Top‑5 — Global τ* (Actual Budget vs Predicted Revenue) — 2023")
p_genre_2023_br; p_tstar_2023_br


#------------------------------Model Metrics Comaprison--------------------------
summarize_best <- function(tuned_obj, model_name) {
        tuned_obj %>%
                collect_metrics() %>%
                filter(.metric %in% c("roc_auc", "pr_auc")) %>%
                group_by(.metric) %>%
                slice_max(order_by = mean, n = 1, with_ties = FALSE) %>%
                ungroup() %>%
                transmute(
                        Model     = model_name,
                        Metric    = .metric,
                        Score     = mean,
                        StdErr    = std_err,
                        Resamples = n
                )
}

# ---- Build the tables -------------------------------------
metrics_long <- bind_rows(
        summarize_best(logit_tuned, "Logistic Regression"),
        summarize_best(rf_tuned,    "Random Forest"),
        summarize_best(xgb_c_tuned, "XGBoost")
)

metrics_wide <- metrics_long %>%
        select(Model, Metric, Score) %>%
        pivot_wider(names_from = Metric, values_from = Score)

# ---- Inspect tables  ----------------------------
print(metrics_long)
print(metrics_wide)
knitr::kable(metrics_wide, digits = 4, caption = "Best PR-AUC and ROC-AUC by Model")

# ---- Bar plot with value labels ---------------------------------------
# Long format for plotting: one row per (Model, Metric)
plot_df <- metrics_long %>%
        mutate(
                Metric = case_when(
                        Metric == "pr_auc"  ~ "PR-AUC",
                        Metric == "roc_auc" ~ "ROC-AUC",
                        TRUE ~ Metric
                )
        )

ggplot(plot_df, aes(x = Model, y = Score, fill = Metric)) +
        geom_col(position = position_dodge(width = 0.7), width = 0.6) +
        geom_text(
                aes(label = sprintf("%.3f", Score)),
                position = position_dodge(width = 0.7),
                vjust = -0.35,
                size = 4,
                color = "#0D253F"
        ) +
        scale_fill_manual(values = c("PR-AUC" = "#01B4E4", "ROC-AUC" = "#90CEA1")) +
        scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, by = 0.1)) +
        labs(
                title = "Model Comparison: PR-AUC vs ROC-AUC (Best Tuned)",
                x = NULL,
                y = "AUC Score",
                fill = NULL
        ) +
        theme_minimal(base_size = 13) +
        theme(
                plot.title.position = "plot",
                legend.position = "top",
                axis.text.x = element_text(angle = 0, hjust = 0.5)
        )


#-----------------------------CSV-----------------------------
                        
readr::write_csv(policy_enb, "policy_enb_latest.csv")
readr::write_csv(enb_curve_valid, "enb_curve_valid_latest.csv")
readr::write_csv(genre_contrib, "genre_contrib_latest.csv")
readr::write_csv(sens_valid, "sens_valid_latest.csv")
readr::write_csv(sens_test, "sens_test_latest.csv")
readr::write_csv(top_genre_tau_2023, "top_genre_tau_2023.csv")
readr::write_csv(top_tau_star_2023, "top_tau_star_2023.csv")

# --------------------------- END OF FILE ---------------------




