#' Get Default Variables to Include
#'
#' This function returns a character vector containing the names of
#' default variables that are included in the prepare_data
#' function. These variables cover various aspects such as patient
#' information, injury details, time-related data, and outcomes.
#'
#' @return A character vector containing the names of default variables.
#'
#' @examples
#' default.variables <- get_default_variables_to_be_included()
#'
#' @export
get_default_variables_to_be_included <- function() {
    c("AlarmRePrioritised", "dt_alarm_hosp", "dt_alarm_scene", "dt_ed_emerg_proc", "dt_ed_first_ct", "dt_ed_norm_be", "ed_be_art", "ed_emerg_proc", "ed_emerg_proc_other", "ed_gcs_motor", "ed_gcs_sum", "ed_inr", "ed_inr_NotDone", "ed_intub_type", "ed_intubated", "ed_rr_value", "ed_sbp_value", "FirstTraumaDT_NotDone", "hosp_dischg_dest", "hosp_los_days", "hosp_vent_days", "host_care_level", "host_transfered", "host_vent_days_NotDone", "inj_dominant", "inj_intention", "inj_mechanism", "ISS", "NISS", "NumberOfActions", "NumberOfInjuries", "pre_card_arrest", "pre_gcs_motor", "pre_gcs_sum", "pre_intub_type", "pre_intubated", "pre_provided", "pre_rr_value", "pre_sbp_value", "pre_transport", "pt_age_yrs", "pt_asa_preinjury", "pt_Gender", "res_gos_dischg", "res_survival", "TraumaAlarmAtHospital")
}
    
