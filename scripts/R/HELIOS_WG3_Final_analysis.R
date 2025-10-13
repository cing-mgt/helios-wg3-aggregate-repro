# ==================================================================
# HELIOS WG3 — Manuscript Figure 4
# - Uses ITC_status column; fallback to country lists (if present)
# - Exports to figures/ as high-quality PDF + TIFF (LZW)
# - Author: Stella Tamana
# =================================================================

suppressPackageStartupMessages({
  library(data.table); library(dplyr);   library(readxl)
  library(ggplot2);     library(ComplexUpset); library(cowplot)
  library(ggnewscale);  library(ragg)
})

# --------------------------
# 0) Utilities
# --------------------------
mk_bool <- function(v){
  # Always return TRUE/FALSE; robust to factors/characters/numerics/NA
  x <- v
  if (is.logical(x)) return(replace(x, is.na(x), FALSE))
  if (is.numeric(x)) return(replace(x == 1, is.na(x), FALSE))
  x <- tolower(trimws(as.character(x)))
  out <- x %in% c("checked","true","yes","1")
  out[is.na(out)] <- FALSE
  out
}

read_smart <- function(path){
  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    as.data.frame(readxl::read_excel(path), check.names = FALSE)
  }
}

# Normalize ITC labels
norm_itc_lbl <- function(v){
  x <- tolower(trimws(as.character(v)))
  yes_set <- c("itc","yes","y","1","true",
               "itc status: itc","itc_status: itc","itc_statuts: itc")
  no_set  <- c("non-itc","non itc","nonitc","no","n","0","false",
               "itc status: non-itc","itc_status: non-itc","itc_statuts: non-itc")
  out <- ifelse(x %in% yes_set, "ITC",
                ifelse(x %in% no_set,  "Non-ITC", NA))
  factor(out, levels = c("ITC","Non-ITC"))
}

# --------------------------
# 1) Paths + load + filter
# --------------------------
# --- Robust project-aware paths ---
get_script_dir <- function() {
  # Works with Rscript, Source, and most IDEs
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  path <- sub(file_arg, "", args[grep(file_arg, args)])
  if (length(path)) return(normalizePath(dirname(path), winslash = "/"))
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(p)) return(normalizePath(dirname(p), winslash = "/"))
  }
  normalizePath(getwd(), winslash = "/")  # last resort: working dir
}

find_project_root <- function(start_dir, marker_data = "WG3_survey_responses.xlsx", project_name = "helios-wg3-aggregate-repro") {
  cur <- normalizePath(start_dir, winslash = "/")
  repeat {
    # Heuristics to recognize the repo root
    if (file.exists(file.path(cur, "data", marker_data)) ||
        basename(cur) == project_name ||
        dir.exists(file.path(cur, ".git"))) {
      return(cur)
    }
    parent <- dirname(cur)
    if (parent == cur) break  # reached filesystem root
    cur <- parent
  }
  # Fallback to start_dir if no markers found
  normalizePath(start_dir, winslash = "/")
}

script_dir   <- get_script_dir()
project_root <- find_project_root(script_dir)

# Inputs/outputs anchored to the detected project root:
data_path <- file.path(project_root, "data", "WG3_survey_responses.xlsx")
figdir    <- file.path(project_root, "figures")

# Create output dir and assert input exists
dir.create(figdir, recursive = TRUE, showWarnings = FALSE)
stopifnot(file.exists(data_path))

# Try to detect a "Complete" flag column (case-insensitive); otherwise keep all
complete_col <- names(wg3_all)[grepl("(^|\\b)complete(\\b|$)", names(wg3_all), ignore.case = TRUE)][1]
if (!is.na(complete_col)) {
  v <- tolower(trimws(as.character(wg3_all[[complete_col]])))
  keep <- v %in% c("complete","completed","yes","true","1")
  wg3 <- wg3_all[keep, , drop = FALSE]
} else {
  wg3 <- wg3_all
}

# Affiliation merge (only if Affiliation exists)
wg3 <- safe_join_affiliation(wg3_all, wg3)

# --------------------------
# 2) ITC labels (require ITC_status-like column)
# --------------------------

# Small helper (define if not already defined earlier)
if (!exists("norm_itc_lbl")) {
  norm_itc_lbl <- function(v){
    x <- tolower(trimws(as.character(v)))
    out <- ifelse(x %in% c("itc","yes","y","1","true"), "ITC",
                  ifelse(x %in% c("non-itc","non itc","nonitc","no","n","0","false"), "Non-ITC", NA))
    factor(out, levels = c("ITC","Non-ITC"))
  }
}

# Accept common header variants; `ITC_status` is the target
col_itc <- intersect(c("ITC_status","ITC_statuts","ITC","COST_status"), names(wg3_all))[1]
if (is.na(col_itc)) {
  stop("Required column `ITC_status` not found in the Excel.\n",
       "Please add it to data/WG3_survey_responses.xlsx (or name it one of: ",
       "`ITC_status`, `ITC_statuts`, `ITC`, `COST_status`).")
}

wg3_all$Country_ITC_lbl <- norm_itc_lbl(wg3_all[[col_itc]])

# Keep only rows with known label (for plots requiring it)
wg3 <- wg3_all[!is.na(wg3_all$Country_ITC_lbl), , drop = FALSE]

# Optional QC
if (nrow(wg3) != 44) {
  message(sprintf("QC note: expected 44 rows after filters, got %d.", nrow(wg3)))
}

# =============================================
# 3) Figure 4 — Storage (A) + Standards (B)
# =============================================

tol <- c("#77AADD","#EE8866","#EEDD88","#FFAABB","#99DDFF",
         "#44BB99","#BBCC33","#AAAA00","#DDDDDD")  # tol[9] grey

# Reuse Country_ITC_lbl (align by ID to be safe)
stopifnot("Country_ITC_lbl" %in% names(wg3_all))
wg3$Country_ITC_lbl <- wg3_all$Country_ITC_lbl[match(wg3$ID, wg3_all$ID)]

# Main storage question → Yes/No/Unknown
main_q <- grep("^Do you store all, or part of, your data locally on your premises for research\\??",
               names(wg3), value = TRUE, ignore.case = TRUE)[1]
stopifnot(!is.na(main_q))

wg3$LocalStorage <- dplyr::case_when(
  tolower(trimws(as.character(wg3[[main_q]]))) %in% c("yes","y") ~ "Yes",
  tolower(trimws(as.character(wg3[[main_q]]))) %in% c("no","n")  ~ "No",
  TRUE ~ "Unknown"
)
yes_ids <- wg3$ID[wg3$LocalStorage == "Yes"]
no_ids  <- wg3$ID[wg3$LocalStorage == "No"]
unk_ids <- wg3$ID[wg3$LocalStorage == "Unknown"]

# ---- Panel A (Grouped stacked bar) ----

# Technologies (Yes)
tech_cols <- grep("^Which technology do you use for storing your data\\? \\(choice=", names(wg3), value = TRUE)
X_store <- data.frame(ID = wg3$ID, check.names = FALSE)
for (nm in tech_cols) X_store[[nm]] <- mk_bool(wg3[[nm]])
setDT(X_store)

store_label_order <- c("Excel","REDCap or similar","Hospital EHR",
                       "Other technology","Hospital PACS","I don't know")
for (lbl in store_label_order) {
  hit <- grep(paste0("\\(choice=", lbl, "\\)"), names(X_store), value = TRUE)
  if (length(hit) == 1) setnames(X_store, hit, lbl)
}
hit_other <- grep("\\(choice=Other, specify", names(X_store), value = TRUE)
if (length(hit_other) == 1 && !"Other technology" %in% names(X_store))
  setnames(X_store, hit_other, "Other technology")
hit_idk <- grep("I don.?t know", names(X_store), value = TRUE, ignore.case = TRUE)
if (length(hit_idk) == 1 && !"I don't know" %in% names(X_store))
  setnames(X_store, hit_idk, "I don't know")
X_store <- as.data.frame(X_store)
store_sets <- store_label_order[store_label_order %in% names(X_store)]

# Reasons (No)
no_cols <- grep("^If no, please specify \\(choice=", names(wg3), value = TRUE)
X_no <- data.frame(ID = wg3$ID, check.names = FALSE)
for (nm in no_cols) X_no[[nm]] <- mk_bool(wg3[[nm]])
setDT(X_no)
reason_map <- list(
  "Data is only stored for clinical use"        = "Clinical use only",
  "never stored.*third-party"                   = "Secure third-party",
  "^If no, please specify \\(choice=Other"      = "Other"
)
for (pat in names(reason_map)) {
  hit <- grep(pat, names(X_no), value = TRUE, ignore.case = TRUE)
  if (length(hit) == 1) setnames(X_no, hit, reason_map[[pat]])
}
X_no <- as.data.frame(X_no)
reason_order <- c("Clinical use only","Secure third-party","Other")
reason_sets  <- reason_order[reason_order %in% names(X_no)]

# Collapse each respondent to one bucket within group
# YES
yes_wide <- if (length(store_sets)) X_store[, c("ID", store_sets), drop = FALSE] %>% dplyr::filter(ID %in% yes_ids) else data.frame(ID = yes_ids)
if (ncol(yes_wide) > 1) {
  k_yes <- rowSums(yes_wide[, store_sets, drop = FALSE], na.rm = TRUE)
  counts_single_yes <- sapply(store_sets, function(s) sum(yes_wide[[s]] & (k_yes == 1), na.rm = TRUE))
  yes_df <- data.frame(subcategory = names(counts_single_yes), n = as.integer(counts_single_yes)) %>% dplyr::filter(n > 0)
  if (sum(k_yes >= 2) > 0) yes_df <- dplyr::bind_rows(yes_df, data.frame(subcategory = "Multiple technologies", n = sum(k_yes >= 2)))
  if (sum(k_yes == 0)  > 0) yes_df <- dplyr::bind_rows(yes_df, data.frame(subcategory = "Unspecified technology", n = sum(k_yes == 0)))
} else yes_df <- data.frame(subcategory = "Unspecified technology", n = length(yes_ids))
yes_df$group <- "Yes"

# NO
no_wide <- if (length(reason_sets)) X_no[, c("ID", reason_sets), drop = FALSE] %>% dplyr::filter(ID %in% no_ids) else data.frame(ID = no_ids)
if (ncol(no_wide) > 1) {
  k_no <- rowSums(no_wide[, reason_sets, drop = FALSE], na.rm = TRUE)
  counts_single_no <- sapply(reason_sets, function(s) sum(no_wide[[s]] & (k_no == 1), na.rm = TRUE))
  no_df <- data.frame(subcategory = names(counts_single_no), n = as.integer(counts_single_no)) %>% dplyr::filter(n > 0)
  if (sum(k_no >= 2) > 0) no_df <- dplyr::bind_rows(no_df, data.frame(subcategory = "Multiple reasons", n = sum(k_no >= 2)))
  if (sum(k_no == 0)  > 0) no_df <- dplyr::bind_rows(no_df, data.frame(subcategory = "Unspecified reason", n = sum(k_no == 0)))
} else no_df <- data.frame(subcategory = "Unspecified reason", n = length(no_ids))
no_df$group <- "No"

# UNKNOWN
unk_df <- data.frame(group = "Unknown", subcategory = "Unknown/unspecified", n = length(unk_ids))

# Assemble & order
A_long_yes <- yes_df %>% mutate(group = factor("Yes", levels = c("Yes","No","Unknown")))
A_long_no  <- no_df  %>% mutate(group = factor("No",  levels = c("Yes","No","Unknown")))
A_long_unk <- unk_df %>% mutate(group = factor(group, levels = c("Yes","No","Unknown")))

tech_order_present   <- c(store_label_order, "Multiple technologies", "Unspecified technology")
tech_order_present   <- tech_order_present[tech_order_present %in% A_long_yes$subcategory]
reason_order_present <- c(reason_order,      "Multiple reasons",      "Unspecified reason")
reason_order_present <- reason_order_present[reason_order_present %in% A_long_no$subcategory]

A_long_yes$subcategory <- factor(A_long_yes$subcategory, levels = tech_order_present)
A_long_no$subcategory  <- factor(A_long_no$subcategory,  levels = reason_order_present)

# Monochrome shades per group
yes_pal <- grDevices::colorRampPalette(c("#EAF3FB", "#77AADD", "#2B6EA3"))
no_pal  <- grDevices::colorRampPalette(c("#FEF0E9", "#EE8866", "#9C3E1D"))
order_with_ends <- function(levels_vec, unspec, multi){
  lv <- levels_vec[!is.na(levels_vec)]
  c(unspec, setdiff(lv, c(unspec, multi)), intersect(multi, lv))
}
yes_shade_order <- order_with_ends(levels(A_long_yes$subcategory), "Unspecified technology", "Multiple technologies")
no_shade_order  <- order_with_ends(levels(A_long_no$subcategory),  "Unspecified reason",    "Multiple reasons")
tech_fill_map   <- setNames(yes_pal(length(yes_shade_order)), yes_shade_order)
reason_fill_map <- setNames(no_pal(length(no_shade_order)),   no_shade_order)

totals_A <- rbind(
  data.frame(group="Yes",     n = length(yes_ids)),
  data.frame(group="No",      n = length(no_ids)),
  data.frame(group="Unknown", n = length(unk_ids))
)
totals_A$group <- factor(totals_A$group, levels = c("Yes","No","Unknown"))

A_bar <- ggplot() +
  # YES
  geom_col(
    data = A_long_yes,
    aes(x = group, y = n, fill = subcategory, order = as.numeric(subcategory)),
    color = "white", linewidth = 0.2,
    position = position_stack(reverse = TRUE)
  ) +
  geom_text(
    data = subset(A_long_yes, n >= 1),
    aes(x = group, y = n, label = n, group = subcategory, order = as.numeric(subcategory)),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    size = 3.0, show.legend = FALSE
  ) +
  scale_fill_manual(
    values = tech_fill_map, breaks = tech_order_present,
    name = "Technology (Yes)",
    guide = guide_legend(order = 1, direction = "vertical",
                         ncol = 1, byrow = FALSE, title.position = "top")
  ) +
  ggnewscale::new_scale_fill() +
  # NO
  geom_col(
    data = A_long_no,
    aes(x = group, y = n, fill = subcategory, order = as.numeric(subcategory)),
    color = "white", linewidth = 0.2,
    position = position_stack(reverse = TRUE)
  ) +
  geom_text(
    data = subset(A_long_no, n >= 1),
    aes(x = group, y = n, label = n, group = subcategory, order = as.numeric(subcategory)),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    size = 3.0, show.legend = FALSE
  ) +
  scale_fill_manual(
    values = reason_fill_map, breaks = reason_order_present,
    name = "Reasons (No)",
    guide = guide_legend(order = 2, direction = "vertical",
                         ncol = 1, byrow = FALSE, title.position = "top")
  ) +
  # UNKNOWN (single bar with total label)
  geom_col(
    data = A_long_unk,
    aes(x = group, y = n),
    fill = tol[9], color = "white", linewidth = 0.2, show.legend = FALSE
  ) +
  geom_text(
    data = subset(totals_A, group == "Unknown"),
    aes(x = group, y = n, label = n),
    vjust = -0.35, size = 3.2
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = NULL, y = "Number of respondents") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "right",
    panel.grid.major.x = element_blank(),
    plot.margin = margin(6, 16, 6, 8)
  )

# ---- Panel B (Standards among Yes-only) ----
pat_list <- c("OMOP","FHIR","CDISC","CDISK","i2b2","VCF","proprietary","I don't know","Other")
std_cols <- names(wg3)[Reduce(`|`, lapply(pat_list, function(p) grepl(p, names(wg3), ignore.case = TRUE)))]
std_cols <- std_cols[!grepl("platform|ITHANET|ClinVar|HbVar|LOVD|Varsome", std_cols, ignore.case = TRUE)]

X_std <- data.frame(ID = wg3$ID, check.names = FALSE)
for (nm in std_cols) X_std[[nm]] <- mk_bool(wg3[[nm]])
setDT(X_std)
rename_map <- list(
  "OMOP"="OMOP","FHIR"="FHIR","CDISC|CDISK"="CDISC","i2b2"="i2b2","VCF"="VCF",
  "Custom proprietary"="Custom proprietary format",
  "I don't know"="I don't know","Other"="Other"
)
for (pat in names(rename_map)) {
  hit <- grep(pat, names(X_std), value = TRUE, ignore.case = TRUE)
  if (length(hit) == 1) setnames(X_std, hit, rename_map[[pat]])
}
X_std <- as.data.frame(X_std)
std_order <- c("Custom proprietary format","I don't know","VCF","i2b2","FHIR","CDISC","OMOP","Other")
std_sets  <- std_order[std_order %in% names(X_std)]

collapse_one_bucket <- function(ids_subset){
  wide <- if (length(std_sets)) X_std[, c("ID", std_sets), drop = FALSE] %>% dplyr::filter(ID %in% ids_subset) else data.frame(ID = ids_subset)
  if (ncol(wide) > 1) {
    k <- rowSums(wide[, std_sets, drop = FALSE], na.rm = TRUE)
    counts_single <- sapply(std_sets, function(s) sum(wide[[s]] & (k == 1), na.rm = TRUE))
    out <- data.frame(subcategory = names(counts_single), n = as.integer(counts_single)) %>% dplyr::filter(n > 0)
    if (sum(k >= 2) > 0) out <- dplyr::bind_rows(out, data.frame(subcategory = "Multiple standards", n = sum(k >= 2)))
    if (sum(k == 0)  > 0) out <- dplyr::bind_rows(out, data.frame(subcategory = "Unspecified standard", n = sum(k == 0)))
  } else out <- data.frame(subcategory = "Unspecified standard", n = length(ids_subset))
  out
}

meta_yes <- wg3 %>% dplyr::filter(ID %in% yes_ids) %>% dplyr::select(ID, Country_ITC_lbl)
itc_ids     <- meta_yes$ID[meta_yes$Country_ITC_lbl == "ITC"]
nonitc_ids  <- meta_yes$ID[meta_yes$Country_ITC_lbl == "Non-ITC"]
unknown_ids <- meta_yes$ID[is.na(meta_yes$Country_ITC_lbl)]

itc_df     <- collapse_one_bucket(itc_ids);     if (nrow(itc_df))     itc_df$group <- "ITC"
nonitc_df  <- collapse_one_bucket(nonitc_ids);  if (nrow(nonitc_df))  nonitc_df$group <- "Non-ITC"
unknown_df <- collapse_one_bucket(unknown_ids); if (nrow(unknown_df)) unknown_df$group <- "Unknown"

present_groups <- c(if (length(itc_ids)) "ITC",
                    if (length(nonitc_ids)) "Non-ITC",
                    if (length(unknown_ids)) "Unknown")

A_itc <- if (exists("itc_df")) itc_df else data.frame()
A_non <- if (exists("nonitc_df")) nonitc_df else data.frame()
A_unk <- if (exists("unknown_df")) unknown_df else data.frame()

A_itc$subcategory <- factor(A_itc$subcategory, levels = c(std_order,"Multiple standards","Unspecified standard"))
A_non$subcategory <- factor(A_non$subcategory, levels = c(std_order,"Multiple standards","Unspecified standard"))

# palettes per COST group
itc_base    <- "#35B9A5"; nonitc_base <- "#9553D1"
make_shades <- function(light, mid, dark, n) grDevices::colorRampPalette(c(light, mid, dark))(n)
order_with_ends_std <- function(levels_vec) {
  lv <- levels_vec[!is.na(levels_vec)]
  c("Unspecified standard", setdiff(lv, c("Unspecified standard","Multiple standards")), "Multiple standards")
}
itc_levels <- order_with_ends_std(levels(A_itc$subcategory))
non_levels <- order_with_ends_std(levels(A_non$subcategory))
itc_cols   <- setNames(make_shades("#E7F6F2", itc_base,    "#0E5F54", length(itc_levels)), itc_levels)
non_cols   <- setNames(make_shades("#F1E7FA", nonitc_base, "#4D1D85", length(non_levels)), non_levels)

totals_B <- bind_rows(
  if (nrow(A_itc)) data.frame(group = "ITC",     n = sum(A_itc$n)),
  if (nrow(A_non)) data.frame(group = "Non-ITC", n = sum(A_non$n)),
  if (nrow(A_unk)) data.frame(group = "Unknown", n = sum(A_unk$n))
)
totals_B$group <- factor(totals_B$group, levels = present_groups)

B_bar <- ggplot() +
  # ITC
  geom_col(
    data = A_itc,
    aes(x = factor("ITC", levels = present_groups),
        y = n, fill = subcategory, order = as.numeric(subcategory)),
    color = "white", linewidth = 0.2,
    position = position_stack(reverse = TRUE), na.rm = TRUE
  ) +
  geom_text(
    data = subset(A_itc, n >= 1),
    aes(x = factor("ITC", levels = present_groups),
        y = n, label = n, group = subcategory, order = as.numeric(subcategory)),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    size = 3.0, show.legend = FALSE
  ) +
  scale_fill_manual(
    values = itc_cols, breaks = names(itc_cols),
    name = "Standards (ITC)",
    guide = guide_legend(order = 1, direction = "vertical",
                         ncol = 1, byrow = FALSE, title.position = "top")
  ) +
  ggnewscale::new_scale_fill() +
  # Non-ITC
  geom_col(
    data = A_non,
    aes(x = factor("Non-ITC", levels = present_groups),
        y = n, fill = subcategory, order = as.numeric(subcategory)),
    color = "white", linewidth = 0.2,
    position = position_stack(reverse = TRUE), na.rm = TRUE
  ) +
  geom_text(
    data = subset(A_non, n >= 1),
    aes(x = factor("Non-ITC", levels = present_groups),
        y = n, label = n, group = subcategory, order = as.numeric(subcategory)),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    size = 3.0, show.legend = FALSE
  ) +
  scale_fill_manual(
    values = non_cols, breaks = names(non_cols),
    name = "Standards (Non-ITC)",
    guide = guide_legend(order = 2, direction = "vertical",
                         ncol = 1, byrow = FALSE, title.position = "top")
  ) +
  # Unknown (single bar with cap label)
  geom_col(
    data = A_unk,
    aes(x = factor("Unknown", levels = present_groups), y = n),
    fill = tol[9], color = "white", linewidth = 0.2,
    show.legend = FALSE, na.rm = TRUE
  ) +
  geom_text(
    data = subset(totals_B, group == "Unknown"),
    aes(x = group, y = n, label = n),
    vjust = -0.35, size = 3.2
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = NULL, y = "Number of respondents") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "right",
    panel.grid.major.x = element_blank(),
    plot.margin = margin(6, 16, 6, 8)
  )

# Assemble Figure 4 (with legends)
A_with_legend <- A_bar + ggtitle(NULL) +
  theme(
    legend.position      = "right",
    legend.direction     = "vertical",
    legend.justification = c(0, 1),
    legend.title         = element_text(size = 10),
    legend.text          = element_text(size = 9),
    legend.key.size      = grid::unit(3, "mm"),
    legend.spacing.y     = grid::unit(1.5, "mm"),
    plot.margin          = margin(6, 16, 6, 8)
  )

B_with_legend <- B_bar + ggtitle(NULL) +
  theme(
    legend.position      = "right",
    legend.direction     = "vertical",
    legend.justification = c(0, 1),
    legend.title         = element_text(size = 10),
    legend.text          = element_text(size = 9),
    legend.key.size      = grid::unit(3, "mm"),
    legend.spacing.y     = grid::unit(1.5, "mm"),
    plot.margin          = margin(6, 16, 6, 8)
  )

Figure4_AB <- cowplot::plot_grid(
  A_with_legend, B_with_legend,
  ncol = 1,
  labels = c("A.", "B."),
  label_fontface = "bold",
  label_size = 12,
  label_x = 0.01, label_y = 0.985,
  hjust = 0, vjust = 1
)

# Export Figure 4
ggsave(file.path(figdir, "Manuscript_Figure4.pdf"),  Figure4_AB,
       device = function(file, width, height, ...) grDevices::pdf(file = file, width = width, height = height, useDingbats = FALSE, ...),
       width = 6.9, height = 7.6, units = "in", bg = "white")

ggsave(file.path(figdir, "Manuscript_Figure4.tiff"), Figure4_AB,
       device = ragg::agg_tiff,
       width = 6.9, height = 7.6, units = "in",
       dpi = 600, compression = "lzw", bg = "white")

message("Done. Wrote figures/Manuscript_Figure2.(pdf|tiff) and figures/Manuscript_Figure4.(pdf|tiff)")

