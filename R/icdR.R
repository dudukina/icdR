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
read_icd8 <- function(sks_link = "https://filer.sundhedsdata.dk/sks/data/skscomplete/SGDklass_ICD8.txt"){

  icd_8_sks <- readr::read_table(sks_link, col_names = FALSE, locale = readr::locale(encoding = "latin1"))

  icd_8_sks %<>%
    mutate(across(.cols = X2:X6, ~case_when(is.na(.x) ~ "", T ~ .x))) %>%
    tidyr::unite(., col = "X2", "X2":"X6", sep = " ") %>%
    dplyr::mutate(
      X1 = stringr::str_extract(X1, pattern = "[0-9]+"),
      X2_1 = stringr::str_extract(X2, pattern = "[0-9]+"),
      X2_2 = stringr::str_sub(X2, start = 25, end = length(icd_8_sks$X2)),
      X2_1_1 = lubridate::ymd(stringr::str_sub(X2_1, 1, 8)),
      X2_1_2 = lubridate::ymd(stringr::str_sub(X2_1, 9, 16)),
      X2_1_3 = lubridate::ymd(stringr::str_sub(X2_1, 17, 26))
    )  %>%
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
read_icd10 <- function(sks_link = "https://filer.sundhedsdata.dk/sks/data/skscomplete/SKScomplete.txt"){
  icd_10_sks <- readr::read_table(sks_link, col_names = FALSE, locale = readr::locale(encoding = "latin1"))

  icd_10_sks %<>%
    mutate(across(.cols = X2:X6, ~case_when(is.na(.x) ~ "", T ~ .x))) %>%
    tidyr::unite(., col = "X2", "X2":"X6", sep = " ") %>%
    dplyr::mutate(
      X1 = stringr::str_sub(X1, start = 4, end = length(icd_10_sks$X1)),
      X2_1 = stringr::str_extract(X2, pattern = "[0-9]+"),
      X2_2 = stringr::str_sub(X2, start = 25, end = length(icd_10_sks$X2)),
      X2_1_1 = lubridate::ymd(stringr::str_sub(X2_1, 1, 8)),
      X2_1_2 = lubridate::ymd(stringr::str_sub(X2_1, 9, 16)),
      X2_1_3 = lubridate::ymd(stringr::str_sub(X2_1, 17, 26))
    ) %>%
    dplyr::select(X1, X2_2, X2_1_1, X2_1_2, X2_1_3) %>%
    dplyr::rename(icd_10_code = X1, diagnosis = X2_2, date_start = X2_1_1, date_change = X2_1_2, date_finish = X2_1_3)
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
read_opr <- function(sks_link = "https://filer.sundhedsdata.dk/sks/data/skscomplete/OPRklass_1995.txt"){

  opr <- readr::read_table(sks_link, col_names = FALSE, locale = readr::locale(encoding = "latin1"))

  opr %<>%
    mutate(across(.cols = X2:X6, ~case_when(is.na(.x) ~ "", T ~ .x))) %>%
    tidyr::unite(., col = "X2", "X2":"X6", sep = " ") %>%
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
read_atc <- function(atc_link = "https://medstat.dk/da/download/file/YXRjX2NvZGVfdGV4dC50eHQ="){
  atc <- readr::read_delim(atc_link, delim = ";", locale = locale=locale(encoding="latin1")) %>%
    dplyr::select(ATC = 1, drug = 2, unit = 4, lang = 5) %>% 
    filter(lang == 1) %>%
    select(-lang)
}


#' Find relevant icd-8 codes
#'
#' Finds specific icd-8 codes provided as regex
#'
#' @name find_icd8
#' @param regex is the regex of icd-8 codes
#' @param icd_8 is the data with icd-8 codes
#' @param icd_8_code is the column (variable) with icd-8 codes
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom stringr str_detect
#' @importFrom dplyr filter
#'
#' @return the dataframe with icd-8 codes
#' @export
#' @examples
#' find_icd8(regex = "^249")
find_icd8 <- function(icd_8 = icd_8, icd_8_code = icd_8_code, regex, negate = FALSE){
  codes <- icd_8 %>%
    dplyr::filter(
      stringr::str_detect(string = {{icd_8_code}}, regex, negate = negate) & str_length(icd_8_code) > 4
    )
}


#' Find relevant icd-10 codes
#'
#' Finds specific icd-10 codes provided as regex
#'
#' @name find_icd10
#' @param regex is the regex of icd-10 codes
#' @param icd_10 is the data with icd-10 codes
#' @param icd_10_code is the column (variable) with icd-10 codes
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom stringr str_detect
#' @importFrom dplyr filter
#'
#' @return the dataframe with icd-8 codes
#' @export
#' @examples
#' find_icd10(regex = "^DE10|^DE11|^DH360")
find_icd10 <- function(icd_10 = icd_10, icd_10_code = icd_10_code, regex, negate = FALSE){
  codes <- icd_10 %>%
    dplyr::filter(
      stringr::str_detect(string = {{icd_10_code}}, regex, negate = negate)
    )
}


#' Find any relevant icd-8 or icd-10 codes
#'
#' Finds specific icd codes provided as regex
#'
#' @name find_icd
#' @param regex is the regex of icd codes
#' @param icd is the data with icd codes
#' @param icd_code is the column (variable) with icd codes
#' @param is_icd_8_code is the indicator of whether the code is icd-8 code
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom stringr str_detect
#' @importFrom dplyr filter
#'
#' @return the dataframe with icd-8 codes
#' @export
#' @examples
#' find_icd(regex = "^DE10|^DE11|^DH360|^249|^250")
find_icd <- function(icd_data = icd_data, icd_code = icd_code, regex = NULL, negate = FALSE, is_icd_8_code = FALSE,
                     regex_icd_8 = NULL, regex_icd_10 = NULL){
  if (is_icd_8_code == FALSE && is.null(regex_icd_8) && is.null(regex_icd_10)) {
    codes <- icd_data %>%
    dplyr::filter(
      stringr::str_detect(string = {{icd_code}}, regex, negate = negate)
    )} else {
      codes_8 <- icd_data %>%
        dplyr::filter(
          stringr::str_detect(string = {{icd_code}}, regex_icd_8, negate = negate) & str_length(icd_code) > 4
        )

      codes_10 <- icd_data %>%
        dplyr::filter(
          stringr::str_detect(string = {{icd_code}}, regex_icd_10, negate = negate)
        )

      codes <- codes_8 %>% full_join(codes_10)
    }
}
