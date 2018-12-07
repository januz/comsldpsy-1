#' Transform raw data
#'
#' Applies all transformations to the raw data.
#'
#' @param data tbl. The data frame to be transformed
#' @return tbl. The transformed data frame
#' @export
transform_data <- function(data) {
  data %>%
    combine_wrt() %>%
    recode_analyze() %>%
    create_nationality() %>%
    recode_education_mother() %>%
    create_sld_vars() %>%
    create_psy_vars()
}

#' Combine grade-specific WRT scores
#'
#' Create variables `wrt_*` combinining the separate variables
#'   for grade 3 and 4.
#'
#' @inherit transform_data
#' @export
combine_wrt <- function(data) {
  data %>%
    dplyr::mutate_at(
      dplyr::vars(dplyr::matches("wrt.*_t_own|wrt.*_rs|wrt.*_z_own")),
      dplyr::funs(as.numeric(.))
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(dplyr::matches("wrt.*impl")),
      dplyr::funs(as.character(.))
    ) %>%
    dplyr::mutate(
      wrt_t_own = dplyr::case_when(
        is.na(wrt3_t_own) & is.na(wrt4_t_own) ~ NA_real_,
        is.na(wrt3_t_own) ~ wrt4_t_own,
        TRUE ~ wrt3_t_own
      ),
      wrt_z_own = dplyr::case_when(
        is.na(wrt3_z_own) & is.na(wrt4_z_own) ~ NA_real_,
        is.na(wrt3_z_own) ~ wrt4_z_own,
        TRUE ~ wrt3_z_own
      ),
      wrt_impl = dplyr::case_when(
        is.na(wrt3_impl) & is.na(wrt4_impl) ~ NA_character_,
        is.na(wrt3_impl) ~ wrt4_impl,
        TRUE ~ wrt3_impl
      )) %>%
    dplyr::select(-dplyr::matches("wrt3.*|wrt4.*"))
}

#' Recode variable `analyze`
#'
#' Recode `analyze` to 1 for all children without siblings.
#'
#' @inherit transform_data
#' @export
recode_analyze <- function(data) {
  data %>%
    dplyr::mutate(
      analyze = dplyr::case_when(
        analyze == "no" ~ 0,
        analyze == "yes" ~ 1,
        TRUE ~ 1
      )
    )
}

#' Create variable `nationality`
#'
#' Create variable indicating whether a child has German
#'   or other nationality.
#'
#' @inherit transform_data
#' @export
create_nationality <- function(data) {
  data %>%
    dplyr::mutate(
      nationality = ifelse(
        anamn_12_c___15 == "Checked", "German", "non-German"
      ),
      nationality = as.factor(nationality),
      nationality = sjlabelled::set_label(nationality, "child's nationality")
    )
}

#' Recode variable `education_mother`
#'
#' Recode `education_mother` to 3 levels instead of 5 by combining
#'   the highest two school degrees as well as
#'   no degree and the lowest school degree
#'
#' @inherit transform_data
#' @export
recode_education_mother <- function(data) {
  data %>%
    dplyr::mutate(
      education_mother = forcats::fct_collapse(
        anamn_1,
        Abitur = c("Fachabitur", "Allgemeine Hochschulreife"),
        `kein/Hauptschulabschluss` = c("kein Abschluss", "Hauptschulabschluss")
      ),
      education_mother = data.table::setattr(
        education_mother,
        "redcapLevels",
        seq(0, along.with = levels(education_mother))
      ),
      education_mother = data.table::setattr(
        education_mother,
        "redcapLabels",
        levels(education_mother)
      ),
      education_mother = sjlabelled::set_label(
        education_mother,
        "mother's education"
      )
    )
}

#' Create variables indicating SLD status
#'
#' Create variables `dsm5_cutoff_35{_*}` indicating SLD status according to
#'   DSM-V. A cutoff of z-score <= -1.5 (T-score <= 35) is used to classify
#'   children as having an SLD and a z-score > -1 to classify children as not
#'   having an SLD.
#'
#'   `dsm5_cutoff_35`: groups children as having no SLD, an isolated SLD in
#'   reading, spelling, or arithmetic or the different combinations of the
#'   three SLDs (8 levels).
#'
#'   `dsm5_cutoff_35_{read/spell/math}`: group children as having or not
#'   having a certain SLD.
#'
#'   `dsm5_cutoff_35_01`: groups children as having or not having any SLD.
#'
#'   `dsm5_cutoff_35_012`: groups children as having no, an isolated SLD, or
#'   comorbid SLDs.
#'
#'   `dsm5_cutoff_35_n`: counts number of SLDs a child has.
#'
#' @inherit transform_data
#' @export
create_sld_vars <- function(data) {
  data %>%
    # fine SLD categories (0-7)
    dplyr::mutate(
      dsm5_cutoff_35 := dplyr::case_when(
        cody_z_own <= -1.5 & wllp_z_own <= -1.5 & wrt_z_own <= -1.5 ~ 7,
        cody_z_own <= -1.5 & wllp_z_own  > -1   & wrt_z_own <= -1.5 ~ 6,
        cody_z_own <= -1.5 & wllp_z_own <= -1.5 & wrt_z_own  > -1   ~ 5,
        cody_z_own  > -1   & wllp_z_own <= -1.5 & wrt_z_own <= -1.5 ~ 4,
        cody_z_own <= -1.5 & wllp_z_own  > -1   & wrt_z_own  > -1   ~ 3,
        cody_z_own  > -1   & wllp_z_own  > -1   & wrt_z_own <= -1.5 ~ 2,
        cody_z_own  > -1   & wllp_z_own <= -1.5 & wrt_z_own  > -1   ~ 1,
        cody_z_own  > -1   & wllp_z_own  > -1   & wrt_z_own  > -1   ~ 0,
        TRUE ~ NA_real_
      ),
      dsm5_cutoff_35 := data.table::setattr(
        .data[["dsm5_cutoff_35"]], "redcapLevels", c(0, 1, 2, 3, 4, 5, 6, 7)
      ),
      dsm5_cutoff_35 := data.table::setattr(
        .data[["dsm5_cutoff_35"]], "redcapLabels", c(
          "no disorder", # 0
          "isolated reading disorder", # 1
          "isolated spelling disorder", # 2
          "isolated arithmetic disorder", # 3
          "comorbid reading & spelling", # 4
          "comorbid reading & arithmetic", # 5
          "comorbid spelling & arithmetic", # 6
          "comorbid reading, spelling, & arithmetic" # 7
        )
      ),
      dsm5_cutoff_35 = redcapAPI::redcapFactorFlip(dsm5_cutoff_35),
      dsm5_cutoff_35 = sjlabelled::set_label(
        dsm5_cutoff_35, "disorder according to DSM-V (7 categories)"
      )
    ) %>%

    # reading disorder
    dplyr::mutate(
      dsm5_cutoff_35_read := dplyr::case_when(
        wllp_z_own <= -1.5 ~ 1,
        wllp_z_own  > -1   ~ 0,
        TRUE ~ NA_real_
      ),
      dsm5_cutoff_35_read := data.table::setattr(
        .data[["dsm5_cutoff_35_read"]], "redcapLevels", c(0, 1)
      ),
      dsm5_cutoff_35_read := data.table::setattr(
        .data[["dsm5_cutoff_35_read"]], "redcapLabels", c(
          "no problems",
          "indication of problems"
        )
      ),
      dsm5_cutoff_35_read = redcapAPI::redcapFactorFlip(dsm5_cutoff_35_read),
      dsm5_cutoff_35_read = sjlabelled::set_label(
        dsm5_cutoff_35_read,
        "disorder according to DSM-V - reading (no/yes)"
      )
    ) %>%

    # spelling disorder
    dplyr::mutate(
      dsm5_cutoff_35_spell := dplyr::case_when(
        wrt_z_own <= -1.5 ~ 1,
        wrt_z_own  > -1   ~ 0,
        TRUE ~ NA_real_
      ),
      dsm5_cutoff_35_spell := data.table::setattr(
        .data[["dsm5_cutoff_35_spell"]], "redcapLevels", c(0, 1)
      ),
      dsm5_cutoff_35_spell := data.table::setattr(
        .data[["dsm5_cutoff_35_spell"]], "redcapLabels", c(
          "no problems",
          "indication of problems"
        )
      ),
      dsm5_cutoff_35_spell = redcapAPI::redcapFactorFlip(dsm5_cutoff_35_spell),
      dsm5_cutoff_35_spell = sjlabelled::set_label(
        dsm5_cutoff_35_spell,
        "disorder according to DSM-V - spelling (no/yes)"
      ),

      # math disorder
      dsm5_cutoff_35_math := dplyr::case_when(
        cody_z_own <= -1.5 ~ 1,
        cody_z_own  > -1   ~ 0,
        TRUE ~ NA_real_
      ),
      dsm5_cutoff_35_math := data.table::setattr(
        .data[["dsm5_cutoff_35_math"]], "redcapLevels", c(0, 1)
      ),
      dsm5_cutoff_35_math := data.table::setattr(
        .data[["dsm5_cutoff_35_math"]], "redcapLabels", c(
          "no problems",
          "indication of problems"
        )
      ),
      dsm5_cutoff_35_math = redcapAPI::redcapFactorFlip(dsm5_cutoff_35_math),
      dsm5_cutoff_35_math = sjlabelled::set_label(
        dsm5_cutoff_35_math,
        "disorder according to DSM-V - arithmetic (no/yes)"
      )
    ) %>%

    # no SLD vs. any SLD
    dplyr::mutate(
      dsm5_cutoff_35_01 := dplyr::case_when(
        dsm5_cutoff_35_read == "no problems" &
          dsm5_cutoff_35_spell == "no problems" &
          dsm5_cutoff_35_math == "no problems" ~ 0,
        dsm5_cutoff_35_read == "indication of problems" |
          dsm5_cutoff_35_spell == "indication of problems" |
          dsm5_cutoff_35_math == "indication of problems" ~ 1,
        TRUE ~ NA_real_
      ),
      dsm5_cutoff_35_01 := data.table::setattr(
        .data[["dsm5_cutoff_35_01"]], "redcapLevels", c(0, 1)
      ),
      dsm5_cutoff_35_01 := data.table::setattr(
        .data[["dsm5_cutoff_35_01"]], "redcapLabels", c(
          "no problems",
          "indication of problems"
        )
      ),
      dsm5_cutoff_35_01 = redcapAPI::redcapFactorFlip(dsm5_cutoff_35_01),
      dsm5_cutoff_35_01 = sjlabelled::set_label(
        dsm5_cutoff_35_01,
        "disorder according to DSM-V (no/yes)"
      )
    ) %>%

    # number of SLDs
    dplyr::mutate(
      dsm5_cutoff_35_01 = redcapAPI::redcapFactorFlip(dsm5_cutoff_35_01),
      dsm5_cutoff_35_read = redcapAPI::redcapFactorFlip(dsm5_cutoff_35_read),
      dsm5_cutoff_35_spell = redcapAPI::redcapFactorFlip(dsm5_cutoff_35_spell),
      dsm5_cutoff_35_math = redcapAPI::redcapFactorFlip(dsm5_cutoff_35_math)
    ) %>%
    sjmisc::row_sums(
      dsm5_cutoff_35_read,
      dsm5_cutoff_35_spell,
      dsm5_cutoff_35_math,
      n = 0,
      var = "dsm5_cutoff_35_n"
    ) %>%
    dplyr::mutate(
      dsm5_cutoff_35_n = ifelse(
        dsm5_cutoff_35_n == 0, dsm5_cutoff_35_01, dsm5_cutoff_35_n
      ),
      dsm5_cutoff_35_n = sjlabelled::set_label(
        dsm5_cutoff_35_n,
        "disorder according to DSM-V - number of SLDs"
      ),
      dsm5_cutoff_35_01 = redcapAPI::redcapFactorFlip(dsm5_cutoff_35_01),
      dsm5_cutoff_35_read = redcapAPI::redcapFactorFlip(dsm5_cutoff_35_read),
      dsm5_cutoff_35_spell = redcapAPI::redcapFactorFlip(dsm5_cutoff_35_spell),
      dsm5_cutoff_35_math = redcapAPI::redcapFactorFlip(dsm5_cutoff_35_math)
    ) %>%

    # no vs. isolated vs. comorbid SLD
    dplyr::mutate(
      dsm5_cutoff_35_012 := dplyr::case_when(
        dsm5_cutoff_35_01 == "no problems" ~ 0,
        dsm5_cutoff_35_n  == 1 ~ 1,
        dsm5_cutoff_35_n   > 1 ~ 2,
        TRUE ~ NA_real_
      ),
      dsm5_cutoff_35_012 := data.table::setattr(
        .data[["dsm5_cutoff_35_012"]], "redcapLevels", c(0, 1, 2)
      ),
      dsm5_cutoff_35_012 := data.table::setattr(
        .data[["dsm5_cutoff_35_012"]], "redcapLabels", c(
          "no problems",
          "isolated disorder",
          "comorbid disorders"
        )
      ),
      dsm5_cutoff_35_012 = redcapAPI::redcapFactorFlip(dsm5_cutoff_35_012),
      dsm5_cutoff_35_012 = sjlabelled::set_label(
        dsm5_cutoff_35_012,
        "disorder according to DSM-V (no/isolated/comorbid)"
      )
    )
}

#' Create variables indicating psychopath. problems
#'
#' Create variables indicating children's psychopathological problems.
#'   A z-score >= 1 is used to classify children as having problems and a
#'   z-score < 1 as not having problems.
#'
#'   `{psychopath}_cat`: group children as having / not having specific
#'   psychopath. problems.
#'
#'   `psychopaths_n`: counts number of areas in which a child has
#'   psychopath. problems.
#'
#' @inherit transform_data
#' @export
create_psy_vars <- function(data) {
  data %>%
    dplyr::mutate(
      sca_e_z_cat = dplyr::case_when(
        sca_e_z_own <  1 ~ 0,
        sca_e_z_own >= 1 ~ 1,
        TRUE ~ NA_real_
      ),
      adhs_z_cat = dplyr::case_when(
        adhs_z_own <  1 ~ 0,
        adhs_z_own >= 1 ~ 1,
        TRUE ~ NA_real_
      ),
      des_z_cat = dplyr::case_when(
        des_z_own <  1 ~ 0,
        des_z_own >= 1 ~ 1,
        TRUE ~ NA_real_
      ),
      ssv_z_cat = dplyr::case_when(
        ssv_z_own <  1 ~ 0,
        ssv_z_own >= 1 ~ 1,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(ends_with("z_cat")),
      dplyr::funs(data.table::setattr(., "redcapLevels", c(0, 1)))
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(ends_with("z_cat")),
      dplyr::funs(data.table::setattr(., "redcapLabels", c(
        "no problems",
        "indication of problems"
      )))
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(ends_with("z_cat")),
      dplyr::funs(redcapAPI::redcapFactorFlip(.))
    ) %>%

    dplyr::mutate(
      psychopaths_n = redcapAPI::redcapFactorFlip(sca_e_z_cat) +
        redcapAPI::redcapFactorFlip(adhs_z_cat) +
        redcapAPI::redcapFactorFlip(des_z_cat) +
        redcapAPI::redcapFactorFlip(ssv_z_cat)
    )
}
