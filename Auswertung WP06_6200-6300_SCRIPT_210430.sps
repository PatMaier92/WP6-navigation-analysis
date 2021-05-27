* Encoding: UTF-8.
* Script for calculating summary scores and other variables. 

SORT CASES BY info_id(A).

* ECAS.
COMPUTE ECAS_sub_memory= ECAS_q3 + ECAS_q15_delay_score + ECAS_q16.
EXECUTE.
COMPUTE ECAS_sub_spatial= ECAS_q9 + ECAS_q10 + ECAS_q11.
EXECUTE.
COMPUTE ECAS_sub_language= ECAS_q1 + ECAS_q2 + ECAS_q4.
EXECUTE.
COMPUTE ECAS_sub_verbal_fluency= ECAS_q5 + ECAS_q8 .
EXECUTE.
COMPUTE ECAS_sub_executive= ECAS_q6 + ECAS_q7 + ECAS_q12 + ECAS_q14.
EXECUTE.
COMPUTE ECAS_ALS_unspecific=ECAS_sub_memory + ECAS_sub_spatial.
EXECUTE.
COMPUTE ECAS_ALS_specific=ECAS_sub_language + ECAS_sub_verbal_fluency + ECAS_sub_executive.
EXECUTE.
COMPUTE ECAS_total_score=ECAS_sub_language + ECAS_sub_verbal_fluency + ECAS_sub_executive +
    ECAS_sub_memory + ECAS_sub_spatial.
EXECUTE.

* SPART.
COMPUTE SPART_delay_min=(SPART_time_start_II - SPART_time_start_I) / 60.
EXECUTE.
COMPUTE SPART_mean_I=MEAN(SPART_q1_I,SPART_q2_I,SPART_q3_I).
EXECUTE.

* FIVE_P.
COMPUTE FIVE_P_productivity=FIVE_P_raw_num_total-FIVE_P_raw_num_perserv-FIVE_P_rule_broken.
EXECUTE.
COMPUTE FIVE_P_flexibility=FIVE_P_raw_num_perserv / FIVE_P_productivity *100.
EXECUTE.
COMPUTE FIVE_P_strategy=FIVE_P_raw_num_strategy / FIVE_P_productivity *100.
EXECUTE.

* PTSOT. 
COMPUTE PTSOT_q1_cor=123.
EXECUTE.
COMPUTE PTSOT_q2_cor=237.
EXECUTE.
COMPUTE PTSOT_q3_cor=83.
EXECUTE.
COMPUTE PTSOT_q4_cor=156.
EXECUTE.
COMPUTE PTSOT_q5_cor=319.
EXECUTE.
COMPUTE PTSOT_q6_cor=235.
EXECUTE.
COMPUTE PTSOT_q7_cor=333.
EXECUTE.
COMPUTE PTSOT_q8_cor=260.
EXECUTE.
COMPUTE PTSOT_q9_cor=280.
EXECUTE.
COMPUTE PTSOT_q10_cor=48.
EXECUTE.
COMPUTE PTSOT_q11_cor=26.
EXECUTE.
COMPUTE PTSOT_q12_cor=150.
EXECUTE.
COMPUTE PTSOT_q1_dev=Abs(PTSOT_q1 - PTSOT_q1_cor).
EXECUTE.
COMPUTE PTSOT_q2_dev=Abs(PTSOT_q2- PTSOT_q2_cor).
EXECUTE.
COMPUTE PTSOT_q3_dev=Abs(PTSOT_q3- PTSOT_q3_cor).
EXECUTE.
COMPUTE PTSOT_q4_dev=Abs(PTSOT_q4- PTSOT_q4_cor).
EXECUTE.
COMPUTE PTSOT_q5_dev=Abs(PTSOT_q5- PTSOT_q5_cor).
EXECUTE.
COMPUTE PTSOT_q6_dev=Abs(PTSOT_q6- PTSOT_q6_cor).
EXECUTE.
COMPUTE PTSOT_q7_dev=Abs(PTSOT_q7- PTSOT_q7_cor).
EXECUTE.
COMPUTE PTSOT_q8_dev=Abs(PTSOT_q8- PTSOT_q8_cor).
EXECUTE.
COMPUTE PTSOT_q9_dev=Abs(PTSOT_q9- PTSOT_q9_cor).
EXECUTE.
COMPUTE PTSOT_q10_dev=Abs(PTSOT_q10- PTSOT_q10_cor).
EXECUTE.
COMPUTE PTSOT_q11_dev=Abs(PTSOT_q11- PTSOT_q11_cor).
EXECUTE.
COMPUTE PTSOT_q12_dev=Abs(PTSOT_q12- PTSOT_q12_cor).
EXECUTE.
COMPUTE PTSOT_mean_dev=MEAN(PTSOT_q1_dev, PTSOT_q2_dev, 
    PTSOT_q3_dev,PTSOT_q4_dev,PTSOT_q5_dev,PTSOT_q6_dev,PTSOT_q7_dev,PTSOT_q8_dev,PTSOT_q9_dev,
    PTSOT_q10_dev,PTSOT_q11_dev,PTSOT_q12_dev).
EXECUTE.

* SBSDS.
COMPUTE sbsds_total_score=MEAN(sbsds_q1_inv,sbsds_q2,sbsds_q3_inv,sbsds_q4_inv,sbsds_q5_inv,
    sbsds_q6,sbsds_q7_inv,sbsds_q8,sbsds_q9_inv,sbsds_q10,sbsds_q11,sbsds_q12,sbsds_q13,sbsds_q14_inv,
    sbsds_q15).
EXECUTE.

* cleaning education variables. 
COMPUTE dfb_q3_years_school_clean=dfb_q3_years_school.
EXECUTE.
IF(dfb_q3_years_school_clean > 13) dfb_q3_years_school_clean = 13.  /* cut-off 13 Jahre.
EXECUTE.

COMPUTE dfb_q3_years_apprent_clean=dfb_q3_years_apprent.
EXECUTE.
IF(dfb_q3_years_apprent_clean > 4) dfb_q3_years_apprent_clean = 4. /* cut-off 4 Jahre.
EXECUTE.

COMPUTE dfb_q3_years_uni_clean=dfb_q3_years_uni.
EXECUTE.
IF(dfb_q3_years_uni_clean > 6) dfb_q3_years_uni_clean = 6.  /* cut-off 6 Jahre (Medizin).
EXECUTE.

COMPUTE dfb_q3_years_edu_total=SUM(dfb_q3_years_school_clean, dfb_q3_years_apprent_clean, dfb_q3_years_uni_clean). 
EXECUTE.

* additional manual changes:
- Adding missing entries (test date, starmaze start and end time)
- Changing implausible values (i.e. 44 when scale from 1-7, age after education 16 even though university study) to missing value 999. Check raw data!
- Changing highest education to correct value (i.e. google search of participant, Sozialanamnese in Arztbrief) 
