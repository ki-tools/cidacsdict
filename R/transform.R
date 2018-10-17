 utils::globalVariables(c("birth_year", "name"))

#' Transform DATASUS data according to a dictionary
#'
#' @param data a data frame to transform
#' @param dict a dictionary to use for the transformation
#' @param keep an optional vector of column names to keep (others will be omitted)
#' @param language should the resulting transformed data be in English or Portuguese? Currently not implemented.
#' @param quiet logical - should messages be suppressed?
#' @examples
#' d <- transform_data(sinasc_example, subset(cdcs_dict, db == "SINASC"))
#'
#' \dontrun{
#' fin <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/DNRES/DNRO2013.DBC"
#' fout <- tempfile(fileext = ".DBC")
#' download.file(fin, destfile = fout)
#' d <- read_datasus(fout)
#' d <- transform_data(d, subset(cdcs_dict, db == "SINASC"))
#' }
#' @export
transform_data <- function(data, dict, keep = NULL,
  language = c("en", "br"), quiet = FALSE) {

  names(data) <- tolower(names(data))

  language <- match.arg(language)
  if (language == "br")
    message("Note: Portuguese messages have not been implemented yet...")

  if (is.null(keep)) {
    if (is.null(dict)) {
      keep <- names(data)
    } else {
      keep <- dict$name
    }
  }

  d1 <- setdiff(names(data), keep)
  d2 <- setdiff(keep, names(data))

  keep <- intersect(tolower(keep), names(data))

  lbls <- dict$label_en
  names(lbls) <- dict$name

  if (!quiet) {
    if (length(d1) > 0) {
      message("There are ", length(d1), " variables in the data that either aren't in the dictionary or aren't in 'keep':")
      message(paste0("  ", d1, collapse = "\n"))
    }
    if (length(d2) > 0) {
      message("There are ", length(d2), " variables in the dictionary or 'keep' that aren't in the data:")
      for (nm in d2) {
        txt <- nm
        if (!is.null(dict))
          txt <- paste0(txt, ": ", lbls[nm])
        message("  ", txt)
      }
    }
  }

  data <- data[, names(data) %in% keep]

  for (nm in keep) {
    tmp <- data[[nm]]
    dct <- subset(dict, name == nm)
    mp <- unlist(dct$map_en[[1]])

    if (is.factor(tmp))
      tmp <- as.character(tmp)

    if (length(mp) > 1 && !is.na(mp)) {
      if (!quiet)
        message("Mapping ", nm, "...")
      tmp <- unname(mp[match(tmp, names(mp))])
      tmp <- factor(tmp, levels = unique(mp))
    }

    if (dct$type == "integer") {
      tmp <- as.integer(tmp)
    } else if (dct$type %in% c("numeric", "double")) {
      tmp <- as.numeric(tmp)
    } else if (dct$type == "date") {
      tmp <- as.Date(tmp, format = "%d%m%Y")
    }

    data[[nm]] <- tmp
    names(data)[which(names(data) == nm)] <- dct$name_en
  }

  data
}

#' Clean DATASUS data
#' 
#' @param data a data frame
#' @export
#' @importFrom tibble as.tibble
#' @examples
#' d <- transform_data(sinasc_example, subset(cdcs_dict, db == "SINASC"))
#' clean_data(d)
#' 
#' \dontrun{
#' fin <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/DNRES/DNRO2013.DBC"
#' fout <- tempfile(fileext = ".DBC")
#' download.file(fin, destfile = fout)
#' d <- read_datasus(fout)
#' d <- transform_data(d, subset(cdcs_dict, db == "SINASC"))
#' d <- clean_data(d)
#' }
clean_data <- function(data) {
  nms <- names(data)

  # make apgar codes integer
  if ("apgar1" %in% nms) {
    message("Fixing implausible values of apgar1...")
    data$apgar1 <- as.integer(data$apgar1)
    data$apgar1[data$apgar1 == 99] <- NA
  }

  if ("apgar5" %in% nms) {
    message("Fixing implausible values of apgar5...")
    data$apgar5 <- as.integer(data$apgar5)
    data$apgar5[data$apgar5 == 99] <- NA
  }

  if ("n_live_child" %in% nms) {
    message("Fixing implausible values of n_live_child...")
    data$n_live_child[data$n_live_child == 99] <- NA
  }

  if ("n_dead_child" %in% nms) {
    message("Fixing implausible values of n_dead_child...")
    data$n_dead_child[data$n_dead_child == 99] <- NA
  }

  # we'll use this variable a lot so we'll add it here
  if ("birth_date" %in% nms) {
    message("Adding birth year...")
    data$birth_year <- as.integer(format(data$birth_date, "%Y"))
    data <- subset(data, birth_year > 1860 | is.na(birth_year))
  }

  if ("death_date" %in% nms) {
    message("Adding death year...")
    data$death_year <- as.integer(format(data$death_date, "%Y"))
  }

  if ("brthwt_g" %in% nms) {
    message("Fixing implausible values of birth weight...")
    data$brthwt_g[data$brthwt_g == 0] <- NA
    data$brthwt_g[data$brthwt_g == 9999] <- NA
  }

  # fix implausible mother's age values
  if ("m_age_yrs" %in% nms) {
    message("Fixing implausible values of mother's age...")
    data$m_age_yrs[data$m_age_yrs == 0] <- NA
    data$m_age_yrs[data$m_age_yrs == 99] <- NA
  }

  mc <- brazilmunis::br_muni_codes[, c("muni_code", "state_code", "micro_code", "meso_code")]
  mc$muni_code <- substr(mc$muni_code, 1, 6)
  mc_nms <- names(mc)

  # it turns out that the 7th character of municipality codes doesn't matter
  idx <- which(grepl("_muni_code", nms))
  if (length(idx) > 0) {
    message("Fixing muni codes...")
    for (cur_nm in nms[idx]) {
      # data[[cur_nm]] <- substr(data[[cur_nm]], 1, 6)
      prefix <- gsub("(.*)_muni_code", "\\1", cur_nm)
      names(mc) <- paste0(prefix, "_", mc_nms)
      cur_muni <- paste0(prefix, "_muni_code")
      message("Adding state, micro, meso codes for ", cur_muni, "...")
      data <- merge(data, mc, by = cur_muni, all.x = TRUE)
      # make muni codes integer???
    }
  }

  data <- tibble::as.tibble(data)
}
