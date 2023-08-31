#' Label Data Function
#'
#' This function takes a data frame as input and performs various labeling and factor conversion operations on its columns. The labeled data is returned with new factor levels and column names.
#'
#' @param data A data frame containing the data to be labeled and converted. No default.
#' @return A labeled data frame with updated factor levels and column names.
#'
#' @export
label_data <- function(data) {
    ## Check argument
    assertthat::assert_that(is.data.frame(data))

    ## Label data

    ## Change factors to readable names ##
    ## "korrekt_triage"  
    ##data$korrekt_triage <- factor(      ##### Ta bort!?!?!
    ##  data$korrekt_triage,
    ##  levels = c("Ja", "Nej", "Övertag annat sjukhus"), 
    ##  labels = c("Korrekt triage",
    ##             "Incorrect triage",
    ##             "Transfer from other hosptial"))
    
    ## host_transfered
    data$host_transfered <- factor(
        data$host_transfered,
        levels = c(1, 2, 3, 4), 
        labels = c("Not moved",
                   "Moved to KUH",
                   "Moved from KUH",
                   "Moved both to and from KUH")) 
    
    ## "ed_tta"
    
    ## ###### TA BORT - registrerat t.om   2017-11-06
    ##  data$ed_tta <- factor(
    ##    data$ed_tta,
    ##    levels = c(1, 2), 
    ##    labels = c("Trauma team activation",
    ##               "No trauma team activation"))   
    
    ## ed_emerg_proc_other
    data$ed_emerg_proc_other <- factor(
        data$ed_emerg_proc_other,
        levels = c(1, 2, 3, 4, 5), 
        labels = c("Chest drain",
                   "External fixation of fracture",
                   "Major fracture surgery",
                   "Wound revision in operating room",
                   "Other action")) 
    
    ## ed_emerg_proc 
    data$ed_emerg_proc <- factor(
        data$ed_emerg_proc,
        levels = c(1, 2, 3, 4, 5, 6, 7, 8), 
        labels = c("Thoracotomy",
                   "Laparotomy",
                   "Pelvic Packing",
                   "Revascularization",
                   "Radiological intervention",
                   "Craniotomy",
                   "Intracranial pressure measurement",
                   "Other")) 
    
    ## pre_transport 
    data$pre_transport <- factor(
        data$pre_transport,
        levels = c(1, 2, 3, 4, 5, 6, 7), 
        labels = c("Ground ambulance",
                   "Helicopter ambulance",
                   "Ambulance flight",
                   "Private/public vehicle",
                   "Walking",
                   "Police",
                   "Other")) 
    
    ## ed_intub_type 
    data$ed_intub_type <- factor(
        data$ed_intub_type,
        levels = c(1, 2, 3, 4), 
        labels = c("Intubation/surgical airway with medication",
                   "Supraglottic airway using medication",
                   "Intubation/surgical airway without medication",
                   "Supraglottic airway without medication")) 
    
    ## ed_intubated - ihopslagen med pre intub i var intub 
    data$ed_intubated <- factor(
        data$ed_intubated,
        levels = c(1, 2), 
        labels = c("Yes",
                   "No"))
    
    ## pre_intub_type 
    data$pre_intub_type <- factor(
        data$pre_intub_type,
        levels = c(1, 2, 3, 4), 
        labels = c("Intubation/surgical airway with medication",
                   "Supraglottic airway using medication",
                   "Intubation/surgical airway without medication",
                   "Supraglottic airway without medication"))  
    
    ##  pre_intubated ihopslagning med i var intub
    data$pre_intubated <- factor(
        data$pre_intubated,
        levels = c(1, 2), 
        labels = c("Yes",
                   "No"))  
    
    ## "pre_provided"
    data$pre_provided <- factor(
        data$pre_provided,
        levels = c(1, 3, 4), 
        labels = c("No prehospital care",
                   "Treatment without physician",
                   "Treatment with physician"))          
    
    ## "FirstTraumaDT_NotDone"  
    data$FirstTraumaDT_NotDone <- factor(
        data$FirstTraumaDT_NotDone,
        levels = c(0, 1), 
        labels = c("CT done",
                   "CT not done"))
    
    ## "AlarmRePrioritised" - Ev ta bort pga reg tid från 2017?
    data$AlarmRePrioritised <- factor(
        data$AlarmRePrioritised,
        levels = c(1, 2, 3, 4), 
        labels = c("no reprioritization",
                   "reprioritization to level 1",
                   "reprioritization to level 2",
                   "Alarm cancelled"))
    
    ## "TraumaAlarmAtHospital" - Ev ta bort pga reg tid från 2017
    data$TraumaAlarmAtHospital <- factor(
        data$TraumaAlarmAtHospital,
        levels = c(1, 2, 99), 
        labels = c("Trauma alarm level 1",
                   "Trauma alarm level 2",
                   "No trauma alarm"))
    
    ## "TraumaAlarmCriteria" 
                                        #
    ##  ### Behövs denna????
    ##  data$TraumaAlarmCriteria <- factor(
    ##    data$TraumaAlarmCriteria,
    ##    levels = c(1, 2, 3), 
    ##    labels = c("National, 2 levels",
    ##               "National, 1 level",
    ##               "local"))
    
    ##  "hosp_dischg_dest"
    data$hosp_dischg_dest <- factor(
        data$hosp_dischg_dest,
        levels = c(1, 2, 3, 4, 5, 6, 7), 
        labels = c("Home",
                   "Rehab",
                   "Morgue",
                   "ICU (higher care level)",
                   "ICU (same care level)",
                   "Other department",
                   "Psychiatric care"))
    
    ## "host_vent_days_NotDone"
    data$host_vent_days_NotDone <- factor(
        data$host_vent_days_NotDone,
        levels = c(0, 1),
        labels = c("On ventilator", "Not on ventilator"))
    
    ## "ed_be_art_NotDone"
                                        #
    ##  ######  OBS - Inkongruent/fel jmf med övrig BE kolumn  
    ##  
    ##  data$ed_be_art_NotDone <- factor(
    ##    data$ed_be_art_NotDone,
    ##    levels = c(0, 1),
    ##    labels = c("Done", "Not done"))
                                        #
    ## "ed_inr_NotDone"   
    data$ed_inr_NotDone <- factor(
        data$ed_inr_NotDone,
        levels = c(0, 1),
        labels = c("Done", "Not done"))
    
    
    ##"pt_asa_preinjury"       
    data$pt_asa_preinjury <- factor(
        data$pt_asa_preinjury,
        levels = c(1, 2, 3, 4), 
        labels = c("A healthy patient",
                   "Mild systemic disease",
                   "Severe systemic disease",
                   "Life threatening disease"))
    
    ##"pre_card_arrest" 
    data$pre_card_arrest <- factor(
        data$pre_card_arrest,
        levels = c(1, 2),
        labels = c("Yes", "No"))
    
    ##"inj_dominant"
    data$inj_dominant <- factor(
        data$inj_dominant,
        levels = c(1, 2),
        labels = c("Blunt", "Penetrating"))
    
    ## "inj_mechanism"  
    data$inj_mechanism <- factor(
        data$inj_mechanism,
        levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
        labels = c("Motor vehicle accident",
                   "Motorcycle accident",
                   "Bicycle accident",
                   "Injured pedestrian",
                   "Other vehicle accident",
                   "Gunshot wound",
                   "Injury from sharp object",
                   "Struck by a blunt object",
                   "low energy fall",
                   "High energy fall",
                   "Explosion",
                   "Other injury"))
    
    ## "inj_intention" 
    data$inj_intention <- factor(
        data$inj_intention,
        levels = c(1, 2, 3),
        labels = c("Accident", "Self-inflicted", "Abuse"))
    
    ##"intub"  
    data$intub <- factor(
        data$intub,
        levels = c(1, 2, 3),
        labels = c("Inhospital", "Not intubated", "Prehospital"))
    
    ##"host_care_level" 
    data$host_care_level <- factor(
        data$host_care_level,
        levels = c(1, 2, 3, 4, 5), 
        labels = c("Emergency department",
                   "General ward",
                   "Surgical ward",
                   "Specialist ward/Intermediate ward",
                   "Intensive care unit"))
    
    ##"pt_Gender"              
    data$pt_Gender <- factor(
        data$pt_Gender,
        levels = c(2, 1),
        labels = c("Female", "Male"))
    
    ## "res_survival"
    data$res_survival <- factor(
        data$res_survival,
        levels = c(1, 2),     
        labels = c("Yes", "No"))
    
    ## "ofi" 
    data$ofi <- factor(
        data$ofi,
        levels = c("Yes", "No"),
        labels = c("Opportunity for improvement", "No opportunity for improvement"))
    
    ## ######################
    ## Change column names ##
    ## ######################
    
    labelled::var_label(data) <- list(
        ofi = "Opportunity for improvement",
        ed_gcs_sum = "ED GCS",
        ed_sbp_value = "ED Systolic Blood Pressure",
        dt_ed_first_ct = "Time to first CT",
        dt_ed_emerg_proc = "Time to definitive treatment",
        intub = "Intubated",
        host_care_level = "Highest level of care",
        pt_age_yrs = "Age",
        ed_rr_value = "Respiratory rate",
        ISS = "Injury severity score",
        pt_Gender = "Gender",
        res_survival = "Dead at 30 days",
        hosp_los_days = "Days in hospital", 
        inj_dominant ="Injury type",          
        inj_mechanism ="Injury mechanism",
        inj_intention ="Injury intention",
        pt_asa_preinjury ="ASA-score before injury",      
        pre_card_arrest ="PH cardiac arrest",
        pre_gcs_sum ="PH GCS",
        pre_gcs_motor ="PH GCS - Motor response",         
        ed_gcs_motor ="ED GCS",
        pre_sbp_value ="PH systolic blood preassure",         
        pre_rr_value ="PH respiratory rate",
        ed_be_art ="First base excess",     
        ed_inr ="First INR",
        ed_inr_NotDone ="INR not meassured",
        hosp_vent_days ="Days on ventilator",        
        host_vent_days_NotDone ="Ventilator not required",
        hosp_dischg_dest ="Discharge destination",      
        res_gos_dischg ="Discharge glascow outcome scale",
        TraumaAlarmAtHospital ="Type of trauma alarm",
        AlarmRePrioritised ="reprioritizations",
        NISS ="New Injury Severity Score",
        FirstTraumaDT_NotDone ="CT not done",
        dt_alarm_hosp ="Time, alarm to hospital arrival",         
        pre_provided ="Highest PH competence",
        pre_intub_type ="PH airway management",
        pre_intubated = "Intubated prehospitally",
        ed_intub_type ="Hospital airway management",
        ed_intubated = "Intubated in the ED",
        pre_transport ="Arrival mode",         
        ed_emerg_proc ="Emergency procedure",
        ed_emerg_proc_other ="Other emergency prodecures",
        host_transfered ="Transfer status",
        dt_alarm_scene ="Injury scene time",     
        dt_ed_norm_be ="Time to normal base excess",
        NumberOfActions ="Number of interventions",
        NumberOfInjuries ="Number of injuries")
    
    labelled.data <- data
    return(labelled.data)
}
