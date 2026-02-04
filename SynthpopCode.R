library(synthpop)

real_df <- read.csv(
  "C:/Users/GorjanB/Gorjan/Desktop/Vezilka/netflix_titles.csv",
  stringsAsFactors = FALSE
)

# Drop high-cardinality / text columns
real_df$show_id <- NULL
real_df$title <- NULL
real_df$director <- NULL
real_df$cast <- NULL
real_df$description <- NULL

# Convert date_added into simpler features (year, month)
real_df$date_added <- as.Date(real_df$date_added, format="%B %d, %Y")
real_df$added_year <- as.integer(format(real_df$date_added, "%Y"))
real_df$added_month <- as.integer(format(real_df$date_added, "%m"))
real_df$date_added <- NULL

# Simplify duration: extract number + unit (min/seasons)
real_df$duration_num <- as.integer(sub(" .*", "", real_df$duration))
real_df$duration_unit <- sub("^[0-9]+ ", "", real_df$duration)
real_df$duration <- NULL

# ---- Reduce COUNTRY levels (top N + Other) ----
topN_country <- 30
ct <- sort(table(real_df$country), decreasing = TRUE)
top_countries <- names(ct)[1:min(topN_country, length(ct))]
real_df$country <- ifelse(real_df$country %in% top_countries, real_df$country, "Other")

# ---- Reduce LISTED_IN levels ----
# keep only first genre, then top N + Other
real_df$listed_in <- sub(",.*", "", real_df$listed_in)

topN_genre <- 25
gt <- sort(table(real_df$listed_in), decreasing = TRUE)
top_genres <- names(gt)[1:min(topN_genre, length(gt))]
real_df$listed_in <- ifelse(real_df$listed_in %in% top_genres, real_df$listed_in, "Other")

# Convert remaining character cols to factor
char_cols <- sapply(real_df, is.character)
real_df[char_cols] <- lapply(real_df[char_cols], as.factor)

# Sanity check: should both be <= 60 now
nlevels(real_df$country)
nlevels(real_df$listed_in)

sort(sapply(real_df, function(x) if (is.factor(x)) nlevels(x) else NA),
     decreasing = TRUE)

# ---- NA sanity check + patch (duration_num often has NA) ----
colSums(is.na(real_df))

if (any(is.na(real_df$duration_num))) {
  real_df$duration_num[is.na(real_df$duration_num)] <- median(real_df$duration_num, na.rm = TRUE)
  real_df$duration_num <- as.integer(real_df$duration_num)
}

# ---- Speed/stability fix: disable expensive predictors ----
# ---- Build predictor matrix manually ----
p <- ncol(real_df)
pm <- matrix(1, nrow = p, ncol = p)

colnames(pm) <- names(real_df)
rownames(pm) <- names(real_df)

# A variable should not predict itself
diag(pm) <- 0

# Disable expensive predictors
pm[, "country"] <- 0
pm[, "listed_in"] <- 0

# Run synthesis (timed)
system.time(
  syn_obj <- syn(real_df, predictor.matrix = pm, print.flag = FALSE)
)

synthetic_df <- syn_obj$syn
View(synthetic_df)


# Diagnostics
compare(syn_obj, real_df)
head(synthetic_df)

# Optional: see which methods synthpop used per column
syn_obj$method

write.csv(
  synthetic_df,
  file = "synthetic_netflix_tabular.csv",
  row.names = FALSE
)
