#' Data frame with all exclusion criteria
#'
#' @export
get_filter_cond <- function(){
  tibble::tribble(
    ~group, ~condition, ~filter,

    "incomplete data", "not all days completed", "childs_complete == 2",
    "incomplete data", "parent questionnaires incomplete", "fbbssv_complete == 2",

    "implausible data", "CFT performance implausible", "cft_impl == 'no'",
    "implausible data", "CODY performance implausible", "cody_impl == 'no'",
    "implausible data", "WLLP-R performance implausible", "wllp_impl == 'no'",
    "implausible data", "WRT performance implausible", "wrt_impl == 'no'",

    "no siblings", "no siblings", "analyze != 0",

    "IQ in normal range", "IQ in normal range", "cft_iq_own_kl > 70",

    "other criteria",
      "no 'Krankheiten des Mittelohres und des Warzenfortsatzes'",
      "anamn_23_icd___44 != 'Checked'",
    "other criteria",
      "no 'Sonstige Krankheiten des Ohres'",
      "anamn_23_icd___70 != 'Checked'",
    "other criteria",
      "no 'Affektionen der Aderhaut und der Netzhaut'",
      "anamn_23_icd___2 != 'Checked'",
    "other criteria",
      "no 'Affektionen der Augenmuskeln etc.'",
      "anamn_23_icd___3 != 'Checked'",
    "other criteria",
      "no 'Affektionen der Linse'",
      "anamn_23_icd___4 != 'Checked'",
    "other criteria",
      "no 'Affektionen der Sklera, Hornhaut, Iris, des Ziliarkörpers'",
      "anamn_23_icd___5 != 'Checked'",
    "other criteria",
      "no blindness",
      "anamn_23_icd___54 != 'Checked'",
    "other criteria",
      "no 'Sonstige Affektionen des Auges und der Augenanhangsgebilde'",
      "anamn_23_icd___55 != 'Checked'",
    "other criteria",
      "no 'Angeborene Fehlbildungen des Auges, des Ohres, des Gesichtes und des Halses'",
      "anamn_23_icd___9 != 'Checked'",
    "other criteria",
      "no 'Verletzungen des Kopfes'",
      "anamn_23_icd___91 != 'Checked'",
    "other criteria",
      "no 'Episodische und paroxysmale Krankheiten des Nervensystems'",
      "anamn_23_icd___28 != 'Checked'",
    "other criteria",
      "no 'Krankheiten von Nerven etc.'",
      "anamn_23_icd___48 != 'Checked'",
    "other criteria",
      "no 'Symptome, die das Nervensystem und das Muskel-Skelett-System betreffen'",
      "anamn_23_icd___80 != 'Checked'",
    "other criteria",
      "no 'Systematrophien, die vorwiegend das Zentralnervensystem betreffen'",
      "anamn_23_icd___83 != 'Checked'",
    "other criteria",
      "no 'Lähmungssyndrome'",
      "anamn_23_icd___92 != 'Checked'",
    "other criteria",
      "no chromosomal defects",
      "anamn_23_icd___18 != 'Checked'"
  )
}

#' Filter data
#'
#' Filter out participants that fulfill one or several the exclusion criteria.
#'
#' @param data tbl. The data frame to be filtered
#' @param df_filter_cond tbl. A data frame with exclusion criteria
#' @return tbl. The filtered data frame
#' @export
filter_data <- function(data, df_filter_cond) {
  # combine all filter conditions
  vct_filter_cond_comb <- paste(df_filter_cond$filter, collapse = " & ")

  data %<>%
    dplyr::filter_(vct_filter_cond_comb)
}
