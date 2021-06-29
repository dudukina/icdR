utils::globalVariables(c("X1", "X2", "X2_1", "X2_2", "X2_1_1", "X2_1_1", "X2_1_2", "X2_1_3", "X4"))

#' Reads ICD-8 codes
#'
#' Reads ICD-8 codes from SKS-browser https://medinfo.dk/sks/brows.php. The columns include the ICD-8 code, diagnosis in Danish or Latin, the date the code was introduced for use, the date the code was changed, the date the code usage was ceased
#'
#' @name read_icd8
#' @param sks_link is the link to SKS-browser
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @return the dataframe with ICD-10 and NOMESCO procedure codes used in Danish healthcare system
#' @export
#' @examples
#' icd8 <- read_icd8()
read_icd8 <- function(sks_link = "ftp://filer.sst.dk/filer/sks/data/skscomplete/SGDklass_ICD8.txt"){

  icd_8_sks <- readr::read_table(sks_link, col_names = FALSE, locale = readr::locale(encoding = "latin1"))

  icd_8_sks %<>% dplyr::mutate(
    X1 = stringr::str_extract(X1, pattern = "[0-9]+"),
    X2_1 = stringr::str_extract(X2, pattern = "[0-9]+"),
    X2_2 = stringr::str_sub(X2, start = 25, end = length(icd_8_sks$X2)),
    X2_1_1 = lubridate::ymd(stringr::str_sub(X2_1, 1, 8)),
    X2_1_2 = lubridate::ymd(stringr::str_sub(X2_1, 9, 16)),
    X2_1_3 = lubridate::ymd(stringr::str_sub(X2_1, 17, 26))
  ) %>%
    dplyr::select(X1, X2_2, X2_1_1, X2_1_2, X2_1_3) %>%
    dplyr::rename(icd_8_code = X1, diagnosis = X2_2, date_start = X2_1_1, date_change = X2_1_2, date_finish = X2_1_3)

}

#' Reads ICD-10 codes and NOMESCO procedure codes
#'
#' Reads ICD-10 codes and NOMESCO procedure codes from 1995 from SKS-browser https://medinfo.dk/sks/brows.php. The columns include the ICD-10 code, diagnosis in Danish, the date the code was introduced for use, the date the code was changed, the date the code usage was ceased, and SKS version
#'
#' @name read_icd10
#' @param sks_link is the link to SKS-browser
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @return the dataframe with ICD-10 and NOMESCO procedure codes used in Danish healthcare system
#' @export
#' @examples
#' icd10 <- read_icd10()
read_icd10 <- function(sks_link = "ftp://filer.sst.dk/filer/sks/data/skscomplete/SKScomplete.txt"){
  icd_10_sks <- readr::read_table(sks_link, col_names = FALSE, locale = readr::locale(encoding = "latin1"))

icd_10_sks %<>% dplyr::mutate(
  X1 = stringr::str_sub(X1, start = 4, end = length(icd_10_sks$X1)),
  X2_1 = stringr::str_extract(X2, pattern = "[0-9]+"),
  X2_2 = stringr::str_sub(X2, start = 25, end = length(icd_10_sks$X2)),
  X2_1_1 = lubridate::ymd(stringr::str_sub(X2_1, 1, 8)),
  X2_1_2 = lubridate::ymd(stringr::str_sub(X2_1, 9, 16)),
  X2_1_3 = lubridate::ymd(stringr::str_sub(X2_1, 17, 26))
) %>%
  dplyr::select(X1, X2_2, X2_1_1, X2_1_2, X2_1_3, X4) %>%
  dplyr::rename(icd_10_code = X1, diagnosis = X2_2, date_start = X2_1_1, date_change = X2_1_2, date_finish = X2_1_3, sks_version = X4)
}

#' Reads procedure/operations codes
#'
#' Reads procedure/operations codes from SKS-browser before 1995 https://medinfo.dk/sks/brows.php. The columns include the procedure code, name of the procedure in Danish, the date the code was introduced for use, the date the code was changed, the date the code usage was ceased
#'
#' @name read_opr
#' @param sks_link is the link to SKS-browser
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @return the dataframe with procedure codes before year 1995 used in Danish healthcare system
#' @export
#' @examples
#' opr <- read_opr()
read_opr <- function(sks_link = "ftp://filer.sst.dk/filer/sks/data/skscomplete/OPRklass_1995.txt"){

opr <- readr::read_table(sks_link, col_names = FALSE, locale = readr::locale(encoding = "latin1"))

opr %<>%
  dplyr::select(-c(3, 4)) %>%
  dplyr::mutate(
  X1 = stringr::str_sub(X1, start = 4, end = length(opr$X1)),
  X2_1 = stringr::str_extract(X2, pattern = "[0-9]+"),
  X2_2 = stringr::str_sub(X2, start = 25, end = length(opr$X2)),
  X2_1_1 = lubridate::ymd(stringr::str_sub(X2_1, 1, 8)),
  X2_1_2 = lubridate::ymd(stringr::str_sub(X2_1, 9, 16)),
  X2_1_3 = lubridate::ymd(stringr::str_sub(X2_1, 17, 26))
) %>%
  dplyr::select(X1, X2_2, X2_1_1, X2_1_2, X2_1_3) %>%
  dplyr::rename(opr_code_1995 = X1, operation = X2_2, date_start = X2_1_1, date_change = X2_1_2, date_finish = X2_1_3)
}

#' Reads the Anatomical Therapeutic Chemical (ATC) Classification codes
#'
#' Reads the Anatomical Therapeutic Chemical (ATC) Classification codes from medstat.dk (not from https://www.whocc.no/atc_ddd_index/).The available data include the ATC code column, the name of the active drug agent, the unit (DDD or other; not availbale for every drug)
#'
#' @name read_atc
#' @param atc_link is the link to medstat.dk
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @return the dataframe with ATC codes
#' @export
#' @examples
#' atc <- read_atc()
read_atc <- function(atc_link = "https://medstat.dk/da/download/file/YXRjX2dyb3Vwcy50eHQ="){
  atc <- readr::read_delim(atc_link, delim = ";", locale = readr::locale("en")) %>%
    dplyr::select(ATC = 1, drug = 2, unit = 4)
}
