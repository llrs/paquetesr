library("dplyr")
download_history <- function() {
  tmp_f <- tempfile(pattern = "cransays-history", fileext = ".zip")
  tmp_dir <- tempdir()
  download.file("https://github.com/r-hub/cransays/archive/history.zip",
                destfile = tmp_f)
  # We unzip the files
  dat <- unzip(tmp_f, exdir = tmp_dir, setTimes = TRUE)
  dat <- dat[endsWith(dat, ".csv")]
  
  # First two heading systems:
  incoming_1 <- dat[startsWith(basename(dat), "cran-incoming_-")]
  headers_1 <- lapply(incoming_1, read.csv, nrow = 1, header = FALSE)
  headers_1_length <- lengths(headers_1)
  header_1 <- lapply(incoming_1[headers_1_length == 10], read.csv)
  h1 <- do.call(rbind, header_1)
  header_2 <- lapply(incoming_1[headers_1_length == 11], read.csv)
  h2 <- do.call(rbind, header_2)
  h1[, setdiff(colnames(h2), colnames(h1))] <- NA
  h12 <- rbind(h1, h2[, colnames(h1)])
  
  # Stable heading system 1
  incoming_2 <- dat[startsWith(basename(dat), "cran-incoming-")]
  headers_2 <- lapply(incoming_2, read.csv, nrow = 1, header = FALSE)
  headers_2_length <- lengths(headers_2)
  
  # De difference between headers are the length if they are reordered/rename
  # It might fail.
  header_3 <- lapply(incoming_2[headers_2_length == 5], read.csv)
  h3 <- do.call(rbind, header_3)
  header_4 <- lapply(incoming_2[headers_2_length == 6], read.csv)
  h4 <- do.call(rbind, header_4)
  h3[, setdiff(colnames(h4), colnames(h3))] <- NA
  h34 <- rbind(h3[, colnames(h4)], h4)
  rbind(h12[, colnames(h34)], h34)
}

cran_submissions <- download_history()

cran_submissions <- cran_submissions |> 
  arrange(package, snapshot_time, folder) |> 
  group_by(package, snapshot_time) |> 
  mutate(n = 1:n()) |> 
  filter(n == n()) |> 
  ungroup() |> 
  select(-n)


## ----submissions_cleanup--------------------------------------------------------------------------
diff0 <- structure(0, class = "difftime", units = "hours")
cran_submissions <- cran_submissions |> 
  arrange(package, version, snapshot_time) |> 
  group_by(package) |> 
  # Packages last seen in queue less than 24 ago are considered same submission
  mutate(diff_time = difftime(snapshot_time,  lag(snapshot_time), units = "hour"),
         diff_time = if_else(is.na(diff_time), diff0, diff_time), # Fill NAs
         diff_v = version != lag(version),
         diff_v = ifelse(is.na(diff_v), TRUE, diff_v), # Fill NAs
         near_t = abs(diff_time) <= 24,
         resubmission = !near_t | diff_v, 
         resubmission = if_else(resubmission == FALSE & diff_time == 0, 
                                TRUE, resubmission),
         resubmission_n = cumsum(as.numeric(resubmission)),
         new_version = !near_t & diff_v, 
         new_version = if_else(new_version == FALSE & diff_time == 0, 
                               TRUE, new_version),
         submission_n = cumsum(as.numeric(new_version))) |>
  ungroup() |> 
  select(-diff_time, -diff_v, -new_version, -resubmission)

saveRDS(cran_submissions, file = "output/cran_till_now.RDS")

l <- lapply(unique(cran_submissions$package),
            function(x){
              y <- tryCatch(pkgsearch::cran_package_history(x), 
                            error = function(e){FALSE})
              if (!isFALSE(y)) {
                z <- y$`Date/Publication`
                a <- z[!is.na(z)]
                return(lubridate::as_datetime(z))
              } else {
                return(NA)
              }
            })
names(l) <- unique(cran_submissions$package)
saveRDS(l, "output/CRAN_archival_dates.RDS")
