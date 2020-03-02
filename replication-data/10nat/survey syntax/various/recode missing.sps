
DESCRIPTIVES VARIABLES=Eff6 Trust1 Trust2 Trust3 Int1 Int2 Int3 Exp1 Exp6 Exp7 Exp8 Talk Pol1 Pol2 
    Pol3 Peg1 Peg2 Rel1 Rel2 Ethn1 Ethn2 Ethn3 SocDist1 Eff1 Eff2 Eff3 Eff4 Eff5 Followpol Homeinternet 
    Trus1 Exp2 Exp3 Exp4 Id1 Exp5 Str1 Rel3
  /SAVE
  /STATISTICS=MEAN STDDEV MIN MAX.

RECODE caseid_US weight_US tvmerge_US H1_I1 H2_I2 H3_I3 S1_I6 H5_I5 H6_I7 H7_D1 I3_1 I3_2 CC 
    know9_US know10_US know11_US know12_US know14_US marry_US att1_US att2_US att3_US att4_US att5_US 
    att6_US trust_US promise_US statement_US politics_US local_int_US nat_int_US internat_int_US 
    media_freq_tv_US media_freq_newsp_US media_freq_radio_US media_freq_online_US media_freq_family_US 
    educ_US marstat_US pew_bornagain_US pew_religimp_US pew_churatd_US pew_prayer_US religpew_US 
    religpew_protestant_US ideo5_US newsint_US occup1_US income_US votereg_US inputstate_US caseid_IN 
    gender_IN state_IN state2_IN H4_I4_Ind Q9_Ind Q10_Ind Q11_Ind Q12_Ind Q13_Ind Q17_IN Q18_IN att1_IN 
    att2_IN att3_IN att4_IN Q20_IN trust_IN Q32_IN Q33_IN Q34_IN media_freq_tv_IN media_freq_newsp_IN 
    media_freq_radio_IN media_freq_online_IN media_freq_family_IN Q36_IN party_IN party_strength_IN 
    Q39_1_IN Q39_2_IN Q39_3_IN Q39_4_IN Q40_IN Q41_IN Q43_IN Q44_IN Q45_IN educ_IN marstat_IN 
    votereg_IN churatd_IN relig_IN income_IN newsint_IN caseid_IT H7_D1_Italy H8_D2_Italy H9_D3_Italy 
    S2_D3_Italy S3_D4_Italy Eff1_IT Eff2_IT Eff3_IT Eff4_IT Eff5_IT Eff6_IT Trust1_IT Trust2_IT 
    Trust3_IT Followpol_IT Int1_IT Int2_IT Int3_IT media_freq_Exp1_IT media_freq_Exp2_IT 
    media_freq_Exp3_IT media_freq_Exp4_IT media_freq_Exp6_IT media_freq_Exp7_IT media_freq_Exp8_IT 
    media_freq_Parla_IT tvnews_qual_IT fair_IT informative_IT HomeInternet_IT marstat_IT churatd_IT 
    party_IT party_strength_IT gender_IT Q19_JP Q20_JP Q21_JP Q22_JP Q23_JP Q24_JP Q25_JP Q26_JP Q27_JP 
    Q28_JP Q29_JP Q30_JP Q31_JP Q32_JP Q33_JP Q34_JP Q35_JP Q36_JP Q37_JP Q38_JP Q39_JP Q40_JP Q41_JP 
    gender_AS educ_AS post_educ_AS AH2_I2_Aus H9_I8_Aus AHD1_1_Aus AH1_I1_Aus S2_D2_Aus HI_I1 
    AHD2_2_Aus Exp1_AS Exp6_AS Exp7_AS Exp8_AS Talk_AS AExp1_AS AExp4_AS AExp5_AS AExp6_AS Int1_AS 
    Int2_AS Int3_AS Eff2_AS Eff3_AS Eff6_AS Trust1_AS Trust2_AS Trust3_AS AAtt1_AS AAtt2_AS AAtt3_AS 
    marstat_AS htype_AS religion_AS churatd_AS income_AS employ_AS employ_org_AS top_AS country_AS 
    lang_home_AS caseid_NO weight_NO H8_D2_Nor H9_D2_Nor S2_D3_Nor S3_D4_Nor S4_D8_Nor Eff1_NO Eff2_NO 
    Eff3_NO Eff4_NO Eff5_NO Eff6_NO Trust1_NO Trust2_NO Trust3_NO Followpol_NO Int1_NO Int2_NO Int3_NO 
    Exp1_NO Exp2_NO Exp3_NO Exp6_NO Exp7_NO Exp8_NO Talk_NO HomeInternet_NO Id1_NO Str1_NO churatd_NO 
    religpew_NO income_NO Hldmemb_NO Hldchild_NO Gender_NO marry_UK att1_UK att2_UK att3_UK att4_UK 
    att5_UK att6_UK trust_UK promise_UK statement_UK politics_UK local_int_UK nat_int_UK 
    internat_int_UK media_freq_tv_UK media_freq_bbc_UK media_freq_itv_UK media_freq_chan4_UK 
    media_freq_other_UK media_freq_newsp_UK media_freq_radio_UK media_freq_online_UK 
    media_freq_family_UK internet_UK party_UK party_strength_UK commsize_UK location_UK marstatuk_UK 
    religionukf_UK ideo5_UK newsint_UK incomeuk_UK caseid_CA weight_CA treat_CA lang_CA inputstate_CA 
    H3_I3_3 H3_I3_2 Can1_Can H9_D3_Can S2_D4_Can H10_D5_Can S3_D6_Can Q5B_CA pew_churatd_CA 
    religpew_protestant_CA religpew_CA race_8_CA marstat_CA income_CA gender_CA birthyr_CA VB11s_CA 
    race_1_CA race_2_CA race_3_CA race_4_CA race_5_CA race_6_CA race_7_CA occup1_CA Q39_CA Q38_CA 
    Q37_CA Q31_CA Q32_CA Q33_CA Q34_CA Q35_CA Q36_CA trust_CA promise_CA statement_CA politics_CA 
    local_int_CA nat_int_CA internat_int_CA media_freq_tv_CA media_freq_cbc_CA media_freq_ctv_CA 
    media_freq_cnn_CA media_freq_newsp_CA media_freq_radio_CA media_freq_online_CA internet_CA Q1_CA 
    Q2_CA Q3_CA Q4_CA Q5_CA Q6_CA Q7a_CA Q7b_CA Q7c_CA Q7d_CA Q7e_CA Q7f_CA Q8a_CA Q8b_CA Q8c_CA Q8d_CA 
    Q8e_CA Q8f_CA Q9a_CA Q9b_CA Q9c_CA Q9d_CA Q9e_CA Q9f_CA Q10a_CA Q10b_CA Q10c_CA Q10d_CA Q10e_CA 
    Q10f_CA Q11_CA Q12_CA Q13_CA Q14_CA Q15_CA Q16_CA Q17_CA Q18_CA Q19_CA Q20_CA Q21_CA Q22_CA Q23_CA 
    Q24_CA Q25_CA Q26_CA att1_CA att2_CA att3_CA att4_CA att5_CA marry_CA employuk_UK pew_churatd_UK 
    religionuk_UK ethnicuk_UK E1_UK H8_D2_UK H9_I8_UK S2_D3_UK S3_D4_UK S4_D8_UK E3_UK Educ_NO 
    caseid_UK AVote1_AS AVote2_AS AExp3_AS AExp2_AS Q42_JP Q43_JP Q44_JP Q45_JP Q46_JP Q47_JP Q48_JP 
    Q49_JP marstat_JP ideo_JP income_JP inpustate_JP caseid_AS weight_AS Q10_H8_D2_Japan 
    Q11_H9_I8_Japan Q12_S2_D3_Japan Q13_S3_D4_Japan Q17_S4_D6_JP educ_JP H4_I4 gender_JP educ_IT 
    caseid_JP S4_D8_IT pid3_US race_US pid7_US gender_US tv_choice_US internet_US (MISSING=SYSMIS).
EXECUTE.
