

cd "E:\Eugene\PDFs\Lubuntu_pdf\temp\CRIC_AASK\AASK"
log using "AASK_5group_CKD_NOIMPUTE", text replace

//##########  DATA PREPARATION FOR THE AASK ############################///

// Group-level data: AASK_new.dta ((corrected using AASK_10YR_clinical_sinaID.xls))
// Individual-level data (egfr): egfr.csv
// Individual-level data (uric acid): uricacid.dta
// Individual-level data (APOL1 & Phenotypes): AASK_clean_pheno-APOL1_SLC40A1.xls

set seed 20210427
clear all

import excel using AASK_clean_pheno-APOL1_SLC40A1.xls, clear firstrow
gen pid = PID
save demo1, replace

import delimited  using egfr.csv, clear
gen Date = date(date, "MDY")
format Date %tdNN/DD/CCYY
drop date
save egfr, replace


use "AASK.dta", clear

gen pid = PID

codebook sinai
// 978 Individuals

drop _merge
drop if PID == . 
/// 136 obs. omitted due to the duplication of the same individual (same sina_ids, but different pids)

codebook sinai
// 842 Individuals



// check if the individuals with the same pids, but different sina_ids
sort pid 
quietly by pid: gen dup = cond(_N==1,0,_n)
tab dup
//// 4 duplication of the individuals with the same pids
// checked with the uric acid data vidually and decieded to keep the existing ids 
// in the uric acid dataset
drop if sinai != 199 & PID == 50006
drop if sinai != 99 & PID == 160033
drop if sinai != 175 & PID == 160087
drop if sinai != 1101 & PID == 170050
drop dup


sort pid 
quietly by pid: gen dup = cond(_N==1,0,_n)
tab dup
/// no duplication ids (n=838)
drop dup

merge 1:1 sinai using demo1, update force
drop _merge
drop if RAND_DT == .
/// omit duplication ids (n=3)
/// no duplication ids (still n=838)
codebook sinai
/// 838 individuals
drop uricacid

merge 1:m sinai using uricacid
sort sinai Date
codebook sinai
// 975 individuals

drop if pid == .
// drop participants without PID: 844 obs./ 
codebook sinai
// 838 individuals

drop if _merge == 1
// drop participants without follow-ups: 125 individuals/ 
codebook sinai
// 713 individuals


drop _merge
merge m:m pid Date using egfr
codebook sinai
// 713 individuals

sort  pid Date

bysort pid : replace Name = Name[1] if Name == ""
bysort pid : replace bp = bp[1] if bp == ""
bysort pid : replace DRUG = DRUG[1] if DRUG == ""

bysort pid : replace sinai = sinai[1] if sinai == .
bysort pid : replace PID = PID[1] if PID == .


bysort pid : replace AgeatRandomization = AgeatRandomization[1] if AgeatRandomization == .
bysort pid : replace Gender1Male2Female = Gender1Male2Female[1] if Gender1Male2Female == .
bysort pid : replace ethnic = ethnic[1] if ethnic == .

bysort pid : replace drinking = drinking[1] if drinking == .
bysort pid : replace Smoking0Never1CurrentSmoke = Smoking0Never1CurrentSmoke[1] if Smoking0Never1CurrentSmoke == .
bysort pid : replace householdincome = householdincome[1] if householdincome == .
bysort pid : replace SerumUricAcidmgdL = SerumUricAcidmgdL[1] if SerumUricAcidmgdL == .
bysort pid : replace Ucr = Ucr[1] if Ucr == .
bysort pid : replace GFRatG01visit = GFRatG01visit[1] if GFRatG01visit == .
bysort pid : replace SystolicBPmmHg = SystolicBPmmHg[1] if SystolicBPmmHg == .
bysort pid : replace DiastolicBPmmHg = DiastolicBPmmHg[1] if DiastolicBPmmHg == .
bysort pid : replace BodyMassIndex = BodyMassIndex[1] if BodyMassIndex == .
bysort pid : replace SerumGlucose = SerumGlucose[1] if SerumGlucose == .
bysort pid : replace Uprotein = Uprotein[1] if Uprotein == .
bysort pid : replace Total_cholesterol = Total_cholesterol[1] if Total_cholesterol == .
bysort pid : replace HDL = HDL[1] if HDL == .
bysort pid : replace LDL = LDL[1] if LDL == .
bysort pid : replace TG = TG[1] if TG == .
bysort pid : replace Apol1riskalleles = Apol1riskalleles[1] if Apol1riskalleles == ""



drop _merge

gen ini_ua = SerumUricAcidmgdL
gen ini_crea = Ucr
gen ini_egfr = GFRatG01visit
gen ini_systolic = SystolicBPmmHg
gen ini_diastolic = DiastolicBPmmHg
gen ini_bmi = BodyMassIndex
gen ini_glucose = SerumGlucose
gen ini_upro = Uprotein
gen ini_tc = Total_cholesterol
gen ini_hdl = HDL
gen ini_ldl = LDL
gen ini_tg = TG
encode Apol1riskalleles, gen(apol1)

codebook sinai
// 713 individuals; 15,353 obs.

save AASK_cleaned.dta, replace




//##########  HYPOTHESIS TESTING FOR THE AASK ############################///

//################################################################################///
//1.	Trajectory Analysis
// i.      Outcome: CKD (eGFR)
// ii.      Predictors: sUA (6.8), hdl, ldl, total cholesterol, triglycerides, and IL-6; SBP for AASK
// iii.      Subgroups: APOL1 (high or low)
//========================================================================================
//========================================================================================

// AASK /////////////////////////////////////////////////////////////////////////////////

use "AASK_cleaned.dta", clear


//ssc install egenmore, replace
egen sex_quant = xtile(ini_ua), n(4) by(Gender1Male2Female)
tab sex_quant Gender1Male2Female

centile ini_ua, centile(25) // 6.9
centile ini_ua, centile(50) // 8.1
centile ini_ua, centile(75) // 9.5
centile ini_ua, centile(100) // 14.8

summarize ini_ua if Gender1Male2Female==1 & sex_quant==1 // male Q1: - 7.3
summarize ini_ua if Gender1Male2Female==2 & sex_quant==1 // female Q1: - 6.2

summarize ini_ua if Gender1Male2Female==1 & sex_quant==2 // male Q2: 7.4 - 8.4
summarize ini_ua if Gender1Male2Female==2 & sex_quant==2 // female Q2: 6.3 - 7.4

summarize ini_ua if Gender1Male2Female==1 & sex_quant==3 // male Q3: 8.5 - 9.8
summarize ini_ua if Gender1Male2Female==2 & sex_quant==3 // female Q3: 7.5 - 8.9

summarize ini_ua if Gender1Male2Female==1 & sex_quant==4 // male Q4: 9.9 - 14.8
summarize ini_ua if Gender1Male2Female==2 & sex_quant==4 // female Q4: 9 - 13.8


// new categories for baseline_UA
recode ini_ua (9.9/max=3 "3: Q4") (8.5/9.8=2 "2: Q3") (7.4/8.4=1 "1: Q2") ///
(min/7.3=0 "0: Q1") if Gender1Male2Female==1, gen(sUA_4g_sex1)

recode ini_ua (9/max=3 "3: Q4") (7.5/8.9=2 "2: Q3") (6.3/7.4=1 "1: Q2") (min/6.2=0 "0: Q1") if Gender1Male2Female==2, gen(sUA_4g_sex2)
gen sUA_4g_sex=sUA_4g_sex1 if sUA_4g_sex1!=.
replace sUA_4g_sex=sUA_4g_sex2 if sUA_4g_sex1==.

recode ini_ua (6.8/max=1 "High 6.8") (min/6.8=0 "Low 6.8"), gen(sUA_68)

recode ini_tc (240/max=1 "1: High Total Chol.") (min/240=0 "0: Low Total Chol."), gen(tc_group)
recode ini_hdl (40/max=1 "1: High HDL") (min/40=0 "0: Low HDL"), gen(hdl_group)
recode ini_ldl (160/max=1 "1: High LDL") (min/160=0 "0: Low LDL"), gen(ldl_group)
recode ini_tg (200/max=1 "1: High Triglycerides") (min/200=0 "0: Low Triglycerides"), gen(tg_group)


recode apol1 (2=0) (3=1) (4=2), gen(apol1_additive)
recode apol1 (2=0) (3=1) (4=1), gen(apol1_dominant)
recode apol1 (2=0) (3=0) (4=1), gen(apol1_recessive)

// generate follow-up duration (continuous, in years)
sort sinai Date
bysort sinai: gen do_enter1 = Date[1]
gen t_duration1 = (Date - do_enter1)/365.25
sum t_duration1, detail
label variable t_duration1 "Time of Duration in Treatment (Years)" 

gen glucose_group = 0
replace glucose_group = 1 if ini_glucose >= 110

gen TG_group = 0
replace TG_group = 1 if ini_tg >= 150

gen HDL_group = 0
replace HDL_group = 1 if ini_hdl < 40 & Gender1Male2Female==1
replace HDL_group = 1 if ini_hdl < 50 & Gender1Male2Female==2

gen bp_group=0
replace bp_group=1 if ini_systolic >= 130 | ini_diastolic >= 85

gen metabolic = glucose_group + TG_group + HDL_group + bp_group
recode metabolic (0=0) (1=0) (2=0) (3=1) (4=1), gen(metabolic01)

recode ini_upro (0.15/max=1 "1: High 150mg/day") (min/0.15=0 "0: Low 150mg/day"), gen(proteinuria_15)
recode ini_upro (0.2/max=1 "1: High 200mg/day") (min/0.2=0 "0: Low 200mg/day"), gen(proteinuria_2)

gen hypertension_cat = 0
replace hypertension_cat = 1 if ini_systolic < 120 & ini_diastolic < 80
replace hypertension_cat = 2 if (120 <= ini_systolic & ini_systolic <= 129) & ini_diastolic < 80
replace hypertension_cat = 3 if (130 <= ini_systolic & ini_systolic <= 139) | (80 <= ini_diastolic & ini_diastolic <= 89)
replace hypertension_cat = 4 if 140 <= ini_systolic | 90 <= ini_diastolic

label define hypertension_cat_lab 1 "Normal" 2 "Elevated" 3 "Hypertension Stage1" 4 "Hypertension Stage2"
label values hypertension_cat hypertension_cat_lab 



//egen pattern = concat(ini_ace ini_betablk ini_cablk ini_diuretic ///
//peripheral_aantagonist central_aantagonist ini_vasodil) 
//tab pattern 

//table pattern drugclass


egen ini_egfr_cl = std(ini_egfr)
gen ini_egfr_cl1 = ini_egfr_cl
replace ini_egfr_cl1 = -3 if ini_egfr_cl <= -3
replace ini_egfr_cl1 = 3 if ini_egfr_cl >= 3
replace ini_egfr_cl1 = . if ini_egfr ==.

egen ini_ua_cl = std(ini_ua)
gen ini_ua_cl1 = ini_ua_cl
replace ini_ua_cl1 = -3 if ini_ua_cl <= -3
replace ini_ua_cl1 = 3 if ini_ua_cl >= 3
replace ini_ua_cl1 = . if ini_ua ==.

egen ini_tc_cl = std(ini_tc)
gen ini_tc_cl1 = ini_tc_cl
replace ini_tc_cl1 = -3 if ini_tc_cl <= -3
replace ini_tc_cl1 = 3 if ini_tc_cl >= 3
replace ini_tc_cl1 = . if ini_tc ==.

egen ini_ldl_cl = std(ini_ldl)
gen ini_ldl_cl1 = ini_ldl_cl
replace ini_ldl_cl1 = -3 if ini_ldl_cl <= -3
replace ini_ldl_cl1 = 3 if ini_ldl_cl >= 3
replace ini_ldl_cl1 = . if ini_ldl ==.

egen ini_hdl_cl = std(ini_hdl)
gen ini_hdl_cl1 = ini_hdl_cl
replace ini_hdl_cl1 = -3 if ini_hdl_cl <= -3
replace ini_hdl_cl1 = 3 if ini_hdl_cl >= 3
replace ini_hdl_cl1 = . if ini_hdl ==.

gen ini_tg_lg = ln(ini_tg)
egen ini_tg_cl = std(ini_tg_lg)
gen ini_tg_cl1 = ini_tg_cl
replace ini_tg_cl1 = -3 if ini_tg_cl <= -3
replace ini_tg_cl1 = 3 if ini_tg_cl >= 3
replace ini_tg_cl1 = -3 if ini_tg ==.


egen ini_systolic_cl = std(ini_systolic)
gen ini_systolic_cl1 = ini_systolic_cl
replace ini_systolic_cl1 = -3 if ini_systolic_cl <= -3
replace ini_systolic_cl1 = 3 if ini_systolic_cl >= 3
replace ini_systolic_cl1 = . if ini_systolic ==.

egen ini_bmi_cl = std(ini_bmi)
gen ini_bmi_cl1 = ini_bmi_cl
replace ini_bmi_cl1 = -3 if ini_bmi_cl <= -3
replace ini_bmi_cl1 = 3 if ini_bmi_cl >= 3
replace ini_bmi_cl1 = . if ini_bmi ==.

gen ini_upro_cl0 = log(ini_upro)
egen ini_upro_cl1 = std(ini_upro_cl0)
replace ini_upro_cl1 = -3 if ini_upro_cl1 <= -3
replace ini_upro_cl1 = 3 if ini_upro_cl1 >= 3
replace ini_upro_cl1 = . if ini_upro ==.

egen ini_glucose_cl1 = std(ini_glucose)
replace ini_glucose_cl1 = -3 if ini_glucose_cl1 <= -3
replace ini_glucose_cl1 = 3 if ini_glucose_cl1 >= 3
replace ini_glucose_cl1 = . if ini_glucose ==.

gen ini_upcr =  ini_upro / ini_crea

gen ini_upcr_cl0 = log(ini_upcr)
egen ini_upcr_cl1 = std(ini_upcr_cl0)
replace ini_upcr_cl1 = -3 if ini_upcr_cl1 <= -3
replace ini_upcr_cl1 = 3 if ini_upcr_cl1 >= 3
replace ini_upcr_cl1 = . if ini_upcr ==.


label define sUA_4g_sex_lab 0 "Q1" 1 "Q2" 2 "Q3" 3 "Q4"
label values sUA_4g_sex sUA_4g_sex_lab 

encode DrugGroupAACEBBetaCCCB, gen(DrugGroupAACEBBetaCCCB_cat)
encode BPGoalLLowBPGoalMUsual, gen(BPGoalLLowBPGoalMUsual_cat)

// household income criteria (Wright 1996)
gen householdincomecat = 0
replace householdincomecat = 1 if householdincome == 1
replace householdincomecat = 2 if householdincome == 2
replace householdincomecat = 3 if householdincome == 3 |  householdincome == 4
replace householdincomecat = 4 if householdincome == 5
replace householdincomecat = 5 if householdincome == 6
replace householdincomecat = 6 if 6 < householdincome
replace householdincomecat = . if householdincome == 11


sort sinai Date 


save temp, replace






///////////////////////////////////////bp///////////////////////////////////////////////////////////
import excel "bp_all_0521.xls", sheet("bp_all_0521") firstrow clear


gen sinai = SinaiID
gen Date = VisitDate
format Date %tdnn/dd/CCYY
keep sinai Date Systolic Diastolic
sort sinai Date 
save bp1, replace


use temp, clear
sort sinai Date 

merge m:m sinai Date using bp_traj
drop if _merge == 2
sort sinai Date 
drop if Systolic == .

bys sinai: gen nvisit = _n

keep sinai nvisit Systolic Diastolic Gender1Male2Female AgeatRandomization t_duration1

// for now, we selected the nvisit <= 13 (n=215)
table nvisit
keep if nvisit <=13

reshape wide Systolic Diastolic Gender1Male2Female AgeatRandomization t_duration1,i(sinai) j(nvisit)

save tempa.dta, replace

////////// basic model by time
// all model
use tempa, clear

traj, var(Systolic*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(3 3) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous Systolic in AASK) ci //

use tempa, clear
traj, var(Systolic*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(3 3 3) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous Systolic in AASK) ci // AIC = -31952.18/ -31932.32

keep sinai _traj_Group
save 3Sys.dta, replace

use tempa, clear
traj, var(Systolic*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(3 3 3 3) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous Systolic in AASK) ci // AIC = -31844.08/ -31817.20
keep sinai _traj_Group
save 4Sys.dta, replace

use tempa, clear
traj, var(Systolic*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(3 3 3 3 3) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous Systolic in AASK) ci // AIC = -31847.27/ -31813.37
keep sinai _traj_Group
save 5Sys.dta, replace
// target: group4

use tempa, clear
traj, var(Systolic*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(3 3 3 3 3 3) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous Systolic in AASK) ci // AIC = -31811.62/ -31770.71
keep sinai _traj_Group
save 6Sys.dta, replace


use tempa, clear
traj, var(Systolic*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(3 3 3 3 3 3 3) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous Systolic in AASK) ci // AIC = -8630.45
keep sinai _traj_Group
save 7Sys.dta, replace



use tempa, clear

traj, var(Diastolic*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(3 3) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous Diastolic in AASK) ci

use tempa, clear
traj, var(Diastolic*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(3 3 3) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous Diastolic in AASK) ci // BIC = -28305.27/ -28285.41

keep sinai _traj_Group
save 3Dias.dta, replace

use tempa, clear
traj, var(Diastolic*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(3 3 3 3) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous Diastolic in AASK) ci // BIC = -28249.19/ -28222.31
keep sinai _traj_Group
save 4Dias.dta, replace

use tempa, clear
traj, var(Diastolic*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(3 3 3 3 3) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous Diastolic in AASK) ci // BIC = -28211.48/ -28177.59
keep sinai _traj_Group
save 5Dias.dta, replace
// target: group4

use tempa, clear
traj, var(Diastolic*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(3 3 3 3 3 3) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous Diastolic in AASK) ci // AIC = -8681.49
keep sinai _traj_Group
save 6Dias.dta, replace


use tempa, clear
traj, var(Diastolic*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(3 3 3 3 3 3 3) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous Diastolic in AASK) ci // AIC = -8630.45
keep sinai _traj_Group
save 7Dias.dta, replace





///////////////////sUA///////////////////////////////////////////////////////////
use temp, clear
drop if uricacid==.
sort sinai Date 


bys sinai: gen nvisit = _n

table nvisit

keep sinai nvisit uricacid Gender1Male2Female AgeatRandomization t_duration1



reshape wide uricacid Gender1Male2Female AgeatRandomization t_duration1,i(sinai) j(nvisit)


save tempa.dta, replace


// net from http://www.andrew.cmu.edu/user/bjones/trajplot
// net install traj, force

////////// basic model by time
// all model
use tempa, clear


traj, var(uricacid*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(1 1 1) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous sUA in AASK) ci 

keep sinai _traj_Group
save 3sua.dta, replace

use tempa, clear
traj, var(uricacid*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(1 1 1 1) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous sUA in AASK) ci 
keep sinai _traj_Group
save 4sua.dta, replace

use tempa, clear
traj, var(uricacid*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(1 1 1 1 1) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous sUA in AASK) ci
keep sinai _traj_Group
save 5sua.dta, replace


use tempa, clear
traj, var(uricacid*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(1 1 1 1 1 1) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous sUA in AASK) ci
keep sinai _traj_Group
save 6sua.dta, replace


use tempa, clear
traj, var(uricacid*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(1 1 1 1 1 1 1) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous sUA in AASK) ci 
keep sinai _traj_Group
save 7sua.dta, replace



use tempa, clear
traj, var(uricacid*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(2 2 2 2 2) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous sUA in AASK) ci 
keep sinai _traj_Group
save 5sua_quad.dta, replace

use tempa, clear
traj, var(uricacid*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(3 3 3 3 3) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous sUA in AASK) ci 
keep sinai _traj_Group
save 5sua_cubic.dta, replace
// target: group4


matrix list e(V)

// male model
use tempa, clear
keep if Gender1Male2Female1==1

traj, var(uricacid*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(1 1 1 1 1) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous sUA in AASK) ci

keep sinai _traj_Group
save 5sua_male.dta, replace

// female model
use tempa, clear
keep if Gender1Male2Female1==2

traj, var(uricacid*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(1 1 1 1 1) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous sUA in AASK) ci

keep sinai _traj_Group
save 5sua_female.dta, replace


/////////// Diastolic
use temp,clear

merge m:1 sinai using 5sua_cubic
keep if _traj_Group != .
save tempa.dta, replace

sort sinai
quietly by sinai: gen dup = cond(_N==1,0,_n)
drop if dup>1

anova ini_diastolic _traj_Group

bys _traj_Group: summarize ini_diastolic

pwmean ini_diastolic, over(_traj_Group) mcompare(noadjust) effects


/////////// Systolic
use temp,clear

merge m:1 sinai using 5sua_cubic
keep if _traj_Group != .
save tempa.dta, replace

sort sinai
quietly by sinai: gen dup = cond(_N==1,0,_n)
drop if dup>1

anova ini_systolic _traj_Group

bys _traj_Group: summarize ini_systolic

pwmean ini_systolic, over(_traj_Group) mcompare(noadjust) effects




/////////// Incident of CKD
use temp,clear

merge m:1 sinai using 5sua_cubic
keep if _traj_Group != .
save tempa.dta, replace

/////


sort sinai Date
bysort sinai: gen ini_Date = Date[1]
format ini_Date %tdCY-N-D

bysort sinai: gen datesince =Date - Date[1]
gen syears=(datesince)/365.25
label var syears "Time since diagnosis (years)"
bysort sinai: egen last_date = max(syears)



bysort sinai: gen egfr_dec = (egfr - egfr[_n-1])/(egfr[_n-1])
replace egfr_dec=0 if egfr_dec==.
bysort sinai: egen egfr_dec_n = min(egfr_dec)


gen egfr_dec_t=0 if egfr!= .
replace egfr_dec_t=1 if egfr <= ini_egfr*.5
gen egfr_dec_t1= syears if egfr_dec_t == 1

bysort sinai: egen egfr_dec_date = min(egfr_dec_t1)
gen egfr_dec1=0  
replace egfr_dec1=1 if egfr_dec_date!=.

replace egfr_dec_date=last_date if egfr_dec1==0





gen egfr_dec_tad=0 if egfr!=0
replace egfr_dec_tad=1 if egfr_dec_t == 1 | ///
(al_dial_dt <= Date & al_dial_dt !=.) | (al_death_dt <= Date & al_death_dt !=.)
gen egfr_dec_tad_1 = syears if egfr_dec_tad == 1


bysort sinai: egen egfr_dec_datead = min(egfr_dec_tad_1)
gen egfr_decad=0  
replace egfr_decad=1 if egfr_dec_datead!=.

replace egfr_dec_datead=last_date if egfr_decad==0
gen egfr_decad_final = 0 if egfr_decad != .
replace egfr_decad_final = 1 if egfr_decad == 1 & egfr_dec_datead <= syears







gen egfr_dec_t15=0 if egfr!=0
replace egfr_dec_t15=1 if egfr <= 15
gen egfr_dec_t15_1= syears if egfr_dec_t15 == 1

bysort sinai: egen egfr_dec_date15 = min(egfr_dec_t15_1)
gen egfr_dec15=0  
replace egfr_dec15=1 if egfr_dec_date15!=.

replace egfr_dec_date15=last_date if egfr_dec15==0




gen egfr_dec_t15a=0 if egfr!=0
replace egfr_dec_t15a=1 if egfr_dec_t15 == 1 | egfr_dec_t == 1
gen egfr_dec_t15a_1= syears if egfr_dec_t15a == 1

gen egfr_dec_t15ad=0 if egfr!=0
replace egfr_dec_t15ad=1 if egfr_dec_t15 == 1 | egfr_dec_t == 1 | ///
(al_dial_dt <= Date & al_dial_dt !=.) | (al_death_dt <= Date & al_death_dt !=.)
gen egfr_dec_t15ad_1= syears if egfr_dec_t15ad == 1


bysort sinai: egen egfr_dec_date15a = min(egfr_dec_t15a_1)
gen egfr_dec15a=0  
replace egfr_dec15a=1 if egfr_dec_date15a!=.

bysort sinai: egen egfr_dec_date15ad = min(egfr_dec_t15ad_1)
gen egfr_dec15ad=0  
replace egfr_dec15ad=1 if egfr_dec_date15ad!=.


replace egfr_dec_date15a=last_date if egfr_dec15a==0
gen egfr_dec15a_final = 0 if egfr_dec15a != .
replace egfr_dec15a_final = 1 if egfr_dec15a == 1 & egfr_dec_date15a <= syears

replace egfr_dec_date15ad=last_date if egfr_dec15ad==0
gen egfr_dec15ad_final = 0 if egfr_dec15ad != .
replace egfr_dec15ad_final = 1 if egfr_dec15ad == 1 & egfr_dec_date15ad <= syears



gen egfr_dec_t30=0 if egfr!=0
replace egfr_dec_t30=1 if egfr <= 30
gen egfr_dec_t30_1= syears if egfr_dec_t30 == 1

bysort sinai: egen egfr_dec_date30 = min(egfr_dec_t30_1)
gen egfr_dec30=0  
replace egfr_dec30=1 if egfr_dec_date30!=.

replace egfr_dec_date30=last_date if egfr_dec30==0


gen egfr_dec_t30a=0 if egfr!=0
replace egfr_dec_t30a=1 if egfr_dec_t30 == 1 | egfr_dec_t == 1
gen egfr_dec_t30a_1= syears if egfr_dec_t30a == 1

gen egfr_dec_t30ad=0 if egfr!=0
replace egfr_dec_t30ad=1 if egfr_dec_t30 == 1 | egfr_dec_t == 1 | ///
(al_dial_dt <= Date & al_dial_dt !=.) | (al_death_dt <= Date & al_death_dt !=.)
gen egfr_dec_t30ad_1= syears if egfr_dec_t30ad == 1


bysort sinai: egen egfr_dec_date30a = min(egfr_dec_t30a_1)
gen egfr_dec30a=0  
replace egfr_dec30a=1 if egfr_dec_date30a!=.
replace egfr_dec_date30a=last_date if egfr_dec30a==0
gen egfr_dec30a_final = 0 if egfr_dec30a != .
replace egfr_dec30a_final = 1 if egfr_dec30a == 1 & egfr_dec_date30a <= syears

bysort sinai: egen egfr_dec_date30ad = min(egfr_dec_t30ad_1)
gen egfr_dec30ad=0  
replace egfr_dec30ad=1 if egfr_dec_date30ad!=.
replace egfr_dec_date30ad=last_date if egfr_dec30ad==0
gen egfr_dec30ad_final = 0 if egfr_dec30ad != .
replace egfr_dec30ad_final = 1 if egfr_dec30ad == 1 & egfr_dec_date30ad <= syears

gen t_death_dur = (DEATH_DT - ini_Date)/365.25


gen al_yr_xdu = al_mos_xdu/12
gen al_yr_xdu1 = al_mos_xdu1/12
gen al_yr_xdp = al_mos_xdp/12

recode UPro_Cr (0.22/max=1 "Proteinuria") (min/0.22=0 "No proteinuria"), gen(upcr22)

save tempb.dta, replace


////// HYP 12.5.4

use tempb, clear

sort sinai
quietly by sinai: gen dup = cond(_N==1,0,_n)
drop if dup>1

tab egfr_dec30ad _traj_Group, chi2

bys _traj_Group: table egfr_dec30ad


gen test_1 = 0 if _traj_Group == 4
replace test_1 = 1 if _traj_Group == 5


tab egfr_dec30ad test_1, chi2


gen test_2 = 0 if _traj_Group == 4
replace test_2 = 1 if _traj_Group == 1


tab egfr_dec30ad test_2, chi2

gen test_3 = 0 if _traj_Group == 4
replace test_3 = 1 if _traj_Group == 2


tab egfr_dec30ad test_3, chi2

gen test_4 = 0 if _traj_Group == 4
replace test_4 = 1 if _traj_Group == 3


tab egfr_dec30ad test_4, chi2



////// HYP 12.6.1




/////##########50%########## w/ dialysis; group 1///////////////////////////////////////
// W/O apol1////////////////
use tempb, clear
stset t_duration1, failure(egfr_decad_final==1) id(sinai) exit(t_death_dur)


sts graph, by(_traj_Group) failure risktable xlabel(0(2)12.29)
sts graph, by(egfr_decad_final) failure risktable xlabel(0(2)12.29)
stdescribe 

stsum, by(_traj_Group)

stptime , by(_traj_Group) per(1000) dd(1) at(0(2)12.29)


// m1
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female, efron vce(robust) nolog

stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M1)

//m2
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1, efron vce(robust) nolog



//m3
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1, efron vce(robust) nolog


//m4
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog



stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M4)





////// HYP 12.6.2



/////##########50% and 15########## w/ dialysis; group // 
/////w/ APOL1/////////////////////////////////////
use tempb, clear
stset t_duration1, failure(egfr_dec15ad_final==1) id(sinai) exit(t_death_dur)


sts graph, by(_traj_Group) failure risktable xlabel(0(2)12.29)
sts graph, by(egfr_dec15ad_final) failure risktable xlabel(0(2)12.29)
stdescribe 

stsum, by(_traj_Group)

stptime , by(_traj_Group) per(1000) dd(1) at(0(2)12.29)


// m1
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female, efron vce(robust) nolog

stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M1)

//m2
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1, efron vce(robust) nolog



//m3
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1, efron vce(robust) nolog


//m4
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
i.apol1_additive c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog



stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M4)



/////6.8


use tempb.dta, clear
stset t_duration1, failure(egfr_dec15ad_final==1) id(sinai) exit(t_death_dur)


sts graph, by(sUA_68) failure risktable xlabel(0(2)12.28)
sts graph, by(egfr_dec15ad_final) failure risktable xlabel(0(2)12.28)
stdescribe 


stsum, by(sUA_68)

stptime, by(sUA_68) per(1000) dd(1) at(0(2)12.28)
stir sUA_68



// m1
stcox ib0.sUA_68 ///
c.AgeatRandomization i.Gender1Male2Female, efron vce(robust) nolog


//m2
stcox ib0.sUA_68 ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1, efron vce(robust) nolog



//m3
stcox ib0.sUA_68 ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1, efron vce(robust) nolog


//m4
stcox ib0.sUA_68 ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
i.apol1_additive c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog





/////Q4


use tempb.dta, clear
stset t_duration1, failure(egfr_dec15ad_final==1) id(sinai) exit(t_death_dur)


sts graph, by(sUA_68) failure risktable xlabel(0(2)12.28)
sts graph, by(egfr_dec15ad_final) failure risktable xlabel(0(2)12.28)
stdescribe 


stsum, by(sUA_68)

stptime, by(sUA_68) per(1000) dd(1) at(0(2)12.28)
stir sUA_68



// m1
stcox ib0.sUA_4g_sex ///
c.AgeatRandomization i.Gender1Male2Female, efron vce(robust) nolog


//m2
stcox ib0.sUA_4g_sex ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1, efron vce(robust) nolog



//m3
stcox ib0.sUA_4g_sex ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1, efron vce(robust) nolog


//m4
stcox ib0.sUA_4g_sex ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
i.apol1_additive c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog






/////##########50% and 15########## w/ dialysis; group // 
/////w/o APOL1/////////////////////////////////////
use tempb, clear
stset t_duration1, failure(egfr_dec15ad_final==1) id(sinai) exit(t_death_dur)


sts graph, by(_traj_Group) failure risktable xlabel(0(2)12.29)
sts graph, by(egfr_dec15ad_final) failure risktable xlabel(0(2)12.29)
stdescribe 

stsum, by(_traj_Group)

stptime , by(_traj_Group) per(1000) dd(1) at(0(2)12.29)

///// ref: high_decreasing
// m1
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female, efron vce(robust) nolog

stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M1)

//m2
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1, efron vce(robust) nolog



//m3
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1, efron vce(robust) nolog


//m4
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog



stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M4)






/// roc for the compound effect 
// m4: age (o)
use tempb, clear
stset t_duration1, failure(egfr_dec15ad_final==1) id(sinai) exit(t_death_dur)

stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

predict hr1
generate invhr1=1/hr1

// m4: age (x)
stcox ib4._traj_Group ///
i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

predict hr2
generate invhr2=1/hr2

generate censind=1-_d if _st==1
somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(c)
//somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(z)

lincom invhr1-invhr2 

/// roc for the compound effect 
// m4: gender (o)
use tempb, clear
stset t_duration1, failure(egfr_dec15ad_final==1) id(sinai) exit(t_death_dur)

stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

predict hr1
generate invhr1=1/hr1

// m4: gender (x)
stcox ib4._traj_Group ///
c.AgeatRandomization ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

predict hr2
generate invhr2=1/hr2

generate censind=1-_d if _st==1
somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(c)
//somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(z)

lincom invhr1-invhr2 


/// roc for the compound effect 
// m4: drinking (o)
use tempb, clear
stset t_duration1, failure(egfr_dec15ad_final==1) id(sinai) exit(t_death_dur)

stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

predict hr1
generate invhr1=1/hr1

// m4: drinking (x)
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

predict hr2
generate invhr2=1/hr2

generate censind=1-_d if _st==1
somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(c)
//somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(z)

lincom invhr1-invhr2 


/// roc for the compound effect 
// m4: smoking (o)
use tempb, clear
stset t_duration1, failure(egfr_dec15ad_final==1) id(sinai) exit(t_death_dur)

stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

predict hr1
generate invhr1=1/hr1

// m4: smoking (x)
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

predict hr2
generate invhr2=1/hr2

generate censind=1-_d if _st==1
somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(c)
//somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(z)

lincom invhr1-invhr2 


/// roc for the compound effect 
// m4: income (o)
use tempb, clear
stset t_duration1, failure(egfr_dec15ad_final==1) id(sinai) exit(t_death_dur)

stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

predict hr1
generate invhr1=1/hr1

// m4: income (x)
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

predict hr2
generate invhr2=1/hr2

generate censind=1-_d if _st==1
somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(c)
//somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(z)

lincom invhr1-invhr2 


/// roc for the compound effect 
// m4: bmi (o)
use tempb, clear
stset t_duration1, failure(egfr_dec15ad_final==1) id(sinai) exit(t_death_dur)

stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

predict hr1
generate invhr1=1/hr1

// m4: bmi (x)
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

predict hr2
generate invhr2=1/hr2

generate censind=1-_d if _st==1
somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(c)
//somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(z)

lincom invhr1-invhr2 



/// roc for the compound effect 
// m4: egfr (o)
use tempb, clear
stset t_duration1, failure(egfr_dec15ad_final==1) id(sinai) exit(t_death_dur)

stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

predict hr1
generate invhr1=1/hr1

// m4: egfr (x)
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

predict hr2
generate invhr2=1/hr2

generate censind=1-_d if _st==1
somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(c)
//somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(z)

lincom invhr1-invhr2 


/// roc for the compound effect 
// m4: upro (o)
use tempb, clear
stset t_duration1, failure(egfr_dec15ad_final==1) id(sinai) exit(t_death_dur)

stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

predict hr1
generate invhr1=1/hr1

// m4: upro (x)
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

predict hr2
generate invhr2=1/hr2

generate censind=1-_d if _st==1
somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(c)
//somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(z)

lincom invhr1-invhr2 



/// roc for the compound effect 
// m4: upcr (o)
use tempb, clear
stset t_duration1, failure(egfr_dec15ad_final==1) id(sinai) exit(t_death_dur)

stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

predict hr1
generate invhr1=1/hr1

// m4: upcr (x)
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

predict hr2
generate invhr2=1/hr2

generate censind=1-_d if _st==1
somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(c)
//somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(z)

lincom invhr1-invhr2 


/// roc for the compound effect 
// m4: glucose (o)
use tempb, clear
stset t_duration1, failure(egfr_dec15ad_final==1) id(sinai) exit(t_death_dur)

stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

predict hr1
generate invhr1=1/hr1

// m4: glucose (x)
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

predict hr2
generate invhr2=1/hr2

generate censind=1-_d if _st==1
somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(c)
//somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(z)

lincom invhr1-invhr2 


/// roc for the compound effect 
// m4: systolic (o)
use tempb, clear
stset t_duration1, failure(egfr_dec15ad_final==1) id(sinai) exit(t_death_dur)

stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

predict hr1
generate invhr1=1/hr1

// m4: systolic (x)
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_tc_cl1, efron vce(robust) nolog

predict hr2
generate invhr2=1/hr2

generate censind=1-_d if _st==1
somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(c)
//somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(z)

lincom invhr1-invhr2 


/// roc for the compound effect 
// m4: tc (o)
use tempb, clear
stset t_duration1, failure(egfr_dec15ad_final==1) id(sinai) exit(t_death_dur)

stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

predict hr1
generate invhr1=1/hr1

// m4: tc (x)
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1, efron vce(robust) nolog

predict hr2
generate invhr2=1/hr2

generate censind=1-_d if _st==1
somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(c)
//somersd _t invhr1 invhr2 if _st==1, cenind(censind) tdist transf(z)

lincom invhr1-invhr2 







///// ref: low_stable
// m1
stcox ib1._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female, efron vce(robust) nolog

stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M1)

//m2
stcox ib1._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1, efron vce(robust) nolog



//m3
stcox ib1._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1, efron vce(robust) nolog


//m4
stcox ib1._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog



stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M4)

///// ref: ext. high_stable
// m1
stcox ib5._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female, efron vce(robust) nolog

stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M1)

//m2
stcox ib5._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1, efron vce(robust) nolog



//m3
stcox ib5._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1, efron vce(robust) nolog


//m4
stcox ib5._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog



stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M4)


/////6.8


use tempb.dta, clear
stset t_duration1, failure(egfr_dec15ad_final==1) id(sinai) exit(t_death_dur)


sts graph, by(sUA_68) failure risktable xlabel(0(2)12.28)
sts graph, by(egfr_dec15ad_final) failure risktable xlabel(0(2)12.28)
stdescribe 


stsum, by(sUA_68)

stptime, by(sUA_68) per(1000) dd(1) at(0(2)12.28)
stir sUA_68



// m1
stcox ib0.sUA_68 ///
c.AgeatRandomization i.Gender1Male2Female, efron vce(robust) nolog


//m2
stcox ib0.sUA_68 ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1, efron vce(robust) nolog



//m3
stcox ib0.sUA_68 ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1, efron vce(robust) nolog


//m4
stcox ib0.sUA_68 ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog





/////Q4


use tempb.dta, clear
stset t_duration1, failure(egfr_dec15ad_final==1) id(sinai) exit(t_death_dur)


sts graph, by(sUA_68) failure risktable xlabel(0(2)12.28)
sts graph, by(egfr_dec15ad_final) failure risktable xlabel(0(2)12.28)
stdescribe 


stsum, by(sUA_68)

stptime, by(sUA_68) per(1000) dd(1) at(0(2)12.28)
stir sUA_68



// m1
stcox ib0.sUA_4g_sex ///
c.AgeatRandomization i.Gender1Male2Female, efron vce(robust) nolog


//m2
stcox ib0.sUA_4g_sex ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1, efron vce(robust) nolog



//m3
stcox ib0.sUA_4g_sex ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1, efron vce(robust) nolog


//m4
stcox ib0.sUA_4g_sex ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog









/////##########50% and 15########## w/ dialysis; group // 
/////w/o APOL1///////////// Post////////////////////////
use tempb, clear

di date("20010928","YMD")
keep if Date > 15246


stset t_duration1, failure(egfr_dec15ad_final==1) id(sinai) exit(t_death_dur)


sts graph, by(_traj_Group) failure risktable xlabel(0(2)12.29)
sts graph, by(egfr_dec15ad_final) failure risktable xlabel(0(2)12.29)
stdescribe 

stsum, by(_traj_Group)

stptime , by(_traj_Group) per(1000) dd(1) at(0(2)12.29)


// m1
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female, efron vce(robust) nolog

stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M1)

//m2
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1, efron vce(robust) nolog



//m3
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1, efron vce(robust) nolog


//m4
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog



stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M4)



/////6.8


use tempb.dta, clear
stset t_duration1, failure(egfr_dec15ad_final==1) id(sinai) exit(t_death_dur)


sts graph, by(sUA_68) failure risktable xlabel(0(2)12.28)
sts graph, by(egfr_dec15ad_final) failure risktable xlabel(0(2)12.28)
stdescribe 


stsum, by(sUA_68)

stptime, by(sUA_68) per(1000) dd(1) at(0(2)12.28)
stir sUA_68



// m1
stcox ib0.sUA_68 ///
c.AgeatRandomization i.Gender1Male2Female, efron vce(robust) nolog


//m2
stcox ib0.sUA_68 ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1, efron vce(robust) nolog



//m3
stcox ib0.sUA_68 ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1, efron vce(robust) nolog


//m4
stcox ib0.sUA_68 ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog





/////Q4


use tempb.dta, clear
stset t_duration1, failure(egfr_dec15ad_final==1) id(sinai) exit(t_death_dur)


sts graph, by(sUA_68) failure risktable xlabel(0(2)12.28)
sts graph, by(egfr_dec15ad_final) failure risktable xlabel(0(2)12.28)
stdescribe 


stsum, by(sUA_68)

stptime, by(sUA_68) per(1000) dd(1) at(0(2)12.28)
stir sUA_68



// m1
stcox ib0.sUA_4g_sex ///
c.AgeatRandomization i.Gender1Male2Female, efron vce(robust) nolog


//m2
stcox ib0.sUA_4g_sex ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1, efron vce(robust) nolog



//m3
stcox ib0.sUA_4g_sex ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1, efron vce(robust) nolog


//m4
stcox ib0.sUA_4g_sex ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog





////// HYP 12.6.3


/////##########50% and 30########## w/ dialysis; group 1/////////////////////////////////////////////
use tempb.dta, clear
stset t_duration1, failure(egfr_dec30ad_final==1) id(sinai) exit(t_death_dur)


sts graph, by(_traj_Group) failure risktable xlabel(0(2)12.28)
sts graph, by(egfr_dec30ad_final) failure risktable xlabel(0(2)12.28)
stdescribe 


stsum, by(_traj_Group)

stptime, by(_traj_Group) per(1000) dd(1) at(0(2)12.28)




// m1
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female, efron vce(robust) nolog

stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M1)

//m2
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1, efron vce(robust) nolog



//m3
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1, efron vce(robust) nolog


//m4
stcox ib4._traj_Group ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
i.apol1_additive c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M4)




/////##########50% and 30########## w/ dialysis: baseline UA ///////////////////////////////////////

/////6.8



use tempb.dta, clear
stset t_duration1, failure(egfr_dec30ad_final==1) id(sinai) exit(t_death_dur)


sts graph, by(sUA_68) failure risktable xlabel(0(2)12.28)
sts graph, by(egfr_dec30ad_final) failure risktable xlabel(0(2)12.28)
stdescribe 


stsum, by(sUA_68)

stptime, by(sUA_68) per(1000) dd(1) at(0(2)12.28)
stir sUA_68



// m1
stcox ib0.sUA_68 ///
c.AgeatRandomization i.Gender1Male2Female, efron vce(robust) nolog


//m2
stcox ib0.sUA_68 ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1, efron vce(robust) nolog



//m3
stcox ib0.sUA_68 ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1, efron vce(robust) nolog


//m4
stcox ib0.sUA_68 ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
i.apol1_additive c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog




///// Q4



use tempb.dta, clear
stset t_duration1, failure(egfr_dec30ad_final==1) id(sinai) exit(t_death_dur)


sts graph, by(sUA_4g_sex) failure risktable xlabel(0(2)12.28)
sts graph, by(egfr_dec30ad_final) failure risktable xlabel(0(2)12.28)
stdescribe 


stsum, by(sUA_4g_sex)

stptime, by(sUA_4g_sex) per(1000) dd(1) at(0(2)12.28)




// m1
stcox i.sUA_4g_sex ///
c.AgeatRandomization i.Gender1Male2Female, efron vce(robust) nolog


//m2
stcox i.sUA_4g_sex ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1, efron vce(robust) nolog



//m3
stcox i.sUA_4g_sex ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1, efron vce(robust) nolog


//m4
stcox i.sUA_4g_sex ///
c.AgeatRandomization i.Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
i.apol1_additive c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog





/////////// DrugGroupAACEBBetaCCCB_cat

// group  4 as a reference (by DrugGroupAACEBBetaCCCB_cat)
// ACE 
use tempb.dta, clear
encode DrugGroupAACEBBetaCCCB, generate (DrugGroupAACEBBetaCCCB_num)
keep if DrugGroupAACEBBetaCCCB_num == 1
stset t_duration1, failure(egfr_dec30ad_final==1) id(sinai) exit(t_death_dur)

sts graph, by(_traj_Group) failure risktable xlabel(0(2)12.29)
sts graph, by(egfr_dec30ad_final) failure risktable xlabel(0(2)12.29)
stdescribe 

stsum, by(sUA_68)

stptime , by(sUA_68) per(1000) dd(1) at(0(2)12.29)
stir sUA_68


// m1
stcox ib4._traj_Group ///
c.AgeatRandomization, efron vce(robust) nolog

test 3._traj_Group = 1._traj_Group
test 3._traj_Group = 2._traj_Group
test 3._traj_Group = 4._traj_Group

stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M1)

//m2
stcox ib4._traj_Group ///
c.AgeatRandomization ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1, efron vce(robust) nolog



//m3
stcox ib4._traj_Group ///
c.AgeatRandomization ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1, efron vce(robust) nolog


//m4
stcox ib4._traj_Group ///
c.AgeatRandomization ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
i.apol1_additive c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

test 3._traj_Group = 1._traj_Group
test 3._traj_Group = 2._traj_Group
test 3._traj_Group = 4._traj_Group

stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M4)


// BETA
use tempb.dta, clear
encode DrugGroupAACEBBetaCCCB, generate (DrugGroupAACEBBetaCCCB_num)
keep if DrugGroupAACEBBetaCCCB_num == 2
stset t_duration1, failure(egfr_dec30ad_final==1) id(sinai) exit(t_death_dur)

sts graph, by(_traj_Group) failure risktable xlabel(0(2)12.29)
sts graph, by(egfr_dec30ad_final) failure risktable xlabel(0(2)12.29)
stdescribe 

stsum, by(sUA_68)

stptime , by(sUA_68) per(1000) dd(1) at(0(2)12.29)
stir sUA_68


// m1
stcox ib4._traj_Group ///
c.AgeatRandomization, efron vce(robust) nolog

test 3._traj_Group = 1._traj_Group
test 3._traj_Group = 2._traj_Group
test 3._traj_Group = 4._traj_Group

stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M1)

//m2
stcox ib4._traj_Group ///
c.AgeatRandomization ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1, efron vce(robust) nolog



//m3
stcox ib4._traj_Group ///
c.AgeatRandomization ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1, efron vce(robust) nolog


//m4
stcox ib4._traj_Group ///
c.AgeatRandomization ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
i.apol1_additive c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

test 3._traj_Group = 1._traj_Group
test 3._traj_Group = 2._traj_Group
test 3._traj_Group = 4._traj_Group

stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M4)


// CCB 
use tempb.dta, clear
encode DrugGroupAACEBBetaCCCB, generate (DrugGroupAACEBBetaCCCB_num)
keep if DrugGroupAACEBBetaCCCB_num == 3
stset t_duration1, failure(egfr_dec30ad_final==1) id(sinai) exit(t_death_dur)

sts graph, by(_traj_Group) failure risktable xlabel(0(2)12.29)
sts graph, by(egfr_dec30ad_final) failure risktable xlabel(0(2)12.29)
stdescribe 

stsum, by(sUA_68)

stptime , by(sUA_68) per(1000) dd(1) at(0(2)12.29)
stir sUA_68


// m1
stcox ib4._traj_Group ///
c.AgeatRandomization, efron vce(robust) nolog

test 3._traj_Group = 1._traj_Group
test 3._traj_Group = 2._traj_Group
test 3._traj_Group = 4._traj_Group

stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M1)

//m2
stcox ib4._traj_Group ///
c.AgeatRandomization ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1, efron vce(robust) nolog



//m3
stcox ib4._traj_Group ///
c.AgeatRandomization ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1, efron vce(robust) nolog


//m4
stcox ib4._traj_Group ///
c.AgeatRandomization ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
i.apol1_additive c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog


stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M4)




// group  4 as a reference (by BPGoalLLowBPGoalMUsual_cat)
// Low BP goal
use tempb.dta, clear
encode BPGoalLLowBPGoalMUsual, generate (BPGoalLLowBPGoalMUsual_num)
keep if BPGoalLLowBPGoalMUsual_num == 1

stset t_duration1, failure(egfr_dec30ad_final==1) id(sinai) exit(t_death_dur)

sts graph, by(_traj_Group) failure risktable xlabel(0(2)12.29)
sts graph, by(egfr_dec30ad_final) failure risktable xlabel(0(2)12.29)
stdescribe 

stsum, by(sUA_68)

stptime , by(sUA_68) per(1000) dd(1) at(0(2)12.29)
stir sUA_68


// m1
stcox ib4._traj_Group ///
c.AgeatRandomization, efron vce(robust) nolog

test 3._traj_Group = 1._traj_Group
test 3._traj_Group = 2._traj_Group
test 3._traj_Group = 4._traj_Group

stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M1)

//m2
stcox ib4._traj_Group ///
c.AgeatRandomization ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1, efron vce(robust) nolog



//m3
stcox ib4._traj_Group ///
c.AgeatRandomization ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1, efron vce(robust) nolog


//m4
stcox ib4._traj_Group ///
c.AgeatRandomization ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
i.apol1_additive c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

test 3._traj_Group = 1._traj_Group
test 3._traj_Group = 2._traj_Group
test 3._traj_Group = 4._traj_Group

stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M4)


// Usual
use tempb.dta, clear
encode BPGoalLLowBPGoalMUsual, generate (BPGoalLLowBPGoalMUsual_num)
keep if BPGoalLLowBPGoalMUsual_num == 2

stset t_duration1, failure(egfr_dec30ad_final==1) id(sinai) exit(t_death_dur)

sts graph, by(_traj_Group) failure risktable xlabel(0(2)12.29)
sts graph, by(egfr_dec30ad_final) failure risktable xlabel(0(2)12.29)
stdescribe 

stsum, by(sUA_68)

stptime , by(sUA_68) per(1000) dd(1) at(0(2)12.29)
stir sUA_68


// m1
stcox ib4._traj_Group ///
c.AgeatRandomization, efron vce(robust) nolog

test 3._traj_Group = 1._traj_Group
test 3._traj_Group = 2._traj_Group
test 3._traj_Group = 4._traj_Group

stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M1)

//m2
stcox ib4._traj_Group ///
c.AgeatRandomization ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1, efron vce(robust) nolog



//m3
stcox ib4._traj_Group ///
c.AgeatRandomization ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1, efron vce(robust) nolog


//m4
stcox ib4._traj_Group ///
c.AgeatRandomization ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
i.apol1_additive c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

test 3._traj_Group = 1._traj_Group
test 3._traj_Group = 2._traj_Group
test 3._traj_Group = 4._traj_Group

stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M4)







////// HYP 12.7

use tempb, clear

sort sinai
quietly by sinai: gen dup = cond(_N==1,0,_n)
drop if dup>1

tab al_evt_xdp _traj_Group, chi2

bys _traj_Group: table al_evt_xdp


gen test_1 = 0 if _traj_Group == 4
replace test_1 = 1 if _traj_Group == 5


tab al_evt_xdp test_1, chi2


gen test_2 = 0 if _traj_Group == 4
replace test_2 = 1 if _traj_Group == 1


tab al_evt_xdp test_2, chi2

gen test_3 = 0 if _traj_Group == 4
replace test_3 = 1 if _traj_Group == 2


tab al_evt_xdp test_3, chi2

gen test_4 = 0 if _traj_Group == 4
replace test_4 = 1 if _traj_Group == 3


tab al_evt_xdp test_4, chi2




////// HYP 12.8

use tempb, clear
sort sinai
quietly by sinai: gen dup = cond(_N==1,0,_n)
drop if dup>1

stset al_yr_xdp, failure(al_evt_xdp==1) id(sinai) exit(t_death_dur)

sts graph, by(_traj_Group) failure risktable xlabel(0(2)12.29)
sts graph, by(al_evt_xdp) failure risktable xlabel(0(2)12.29)
stdescribe 

stsum, by(sUA_68)

stptime , by(sUA_68) per(1000) dd(1) at(0(2)12.29)
stir sUA_68




// group  4 as a reference (by Gender)
// male 
use tempb.dta, clear
keep if Gender1Male2Female == 1
stset t_duration1, failure(egfr_dec30ad_final==1) id(sinai) exit(t_death_dur)

sts graph, by(_traj_Group) failure risktable xlabel(0(2)12.29)
sts graph, by(egfr_dec30ad_final) failure risktable xlabel(0(2)12.29)
stdescribe 

stsum, by(sUA_68)

stptime , by(sUA_68) per(1000) dd(1) at(0(2)12.29)
stir sUA_68


// m1
stcox ib4._traj_Group ///
c.AgeatRandomization, efron vce(robust) nolog


stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M1)

//m2
stcox ib4._traj_Group ///
c.AgeatRandomization ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1, efron vce(robust) nolog



//m3
stcox ib4._traj_Group ///
c.AgeatRandomization ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1, efron vce(robust) nolog


//m4
stcox ib4._traj_Group ///
c.AgeatRandomization ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
i.apol1_additive c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M4)


// female 
use tempb.dta, clear
keep if Gender1Male2Female == 2
stset t_duration1, failure(egfr_dec30ad_final==1) id(sinai) exit(t_death_dur)

sts graph, by(_traj_Group) failure risktable xlabel(0(2)12.29)
sts graph, by(egfr_dec30ad_final) failure risktable xlabel(0(2)12.29)
stdescribe 

stsum, by(sUA_68)

stptime , by(sUA_68) per(1000) dd(1) at(0(2)12.29)
stir sUA_68


// m1
stcox ib4._traj_Group ///
c.AgeatRandomization, efron vce(robust) nolog

test 1._traj_Group = 2._traj_Group
test 1._traj_Group = 3._traj_Group
test 1._traj_Group = 4._traj_Group

stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M1)

//m2
stcox ib4._traj_Group ///
c.AgeatRandomization ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1, efron vce(robust) nolog



//m3
stcox ib4._traj_Group ///
c.AgeatRandomization ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1, efron vce(robust) nolog


//m4
stcox ib4._traj_Group ///
c.AgeatRandomization ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
i.apol1_additive c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1, efron vce(robust) nolog

test 1._traj_Group = 2._traj_Group
test 1._traj_Group = 3._traj_Group
test 1._traj_Group = 4._traj_Group


stcurve, cumhaz at1(_traj_Group=1) at2(_traj_Group=2) at3(_traj_Group=3) at4(_traj_Group=4) at5(_traj_Group=5) ///
xlabel(0(2)12.29) title(Cox Proportional Hazard Regression: M4)

/////// demographics ////////////////////////////////////////////////////////////
use tempb.dta, clear

bys _traj_Group: sum uricacid


sort sinai
quietly by sinai: gen dup = cond(_N==1,0,_n)
drop if dup>1

sum AgeatRandomization
tab Gender1Male2Female
sum ini_bmi
tab Smoking0Never1CurrentSmoke
sum ini_glucose
sum SerumAlbumingdL
sum ini_crea
sum ini_egfr
sum ini_upcr
sum ini_ua
sum ini_systolic
sum ini_diastolic
sum MAPmmHg
tab BPGoalLLowBPGoalMUsual
tab householdincomecat
tab drinking
sum ini_hdl
sum ini_ldl
sum ini_tc
sum ini_tg
tab apol1_additive
tab householdincomecat

sum AgeatRandomization,d 
sum ini_glucose,d


bys _traj_Group: sum AgeatRandomization
bys _traj_Group: tab Gender1Male2Female
bys _traj_Group: sum ini_bmi
bys _traj_Group: tab Smoking0Never1CurrentSmoke
bys _traj_Group: sum ini_glucose
bys _traj_Group: sum SerumAlbumingdL
bys _traj_Group: sum ini_crea
bys _traj_Group: sum ini_egfr
bys _traj_Group: sum ini_upcr
bys _traj_Group: sum ini_ua
bys _traj_Group: sum ini_systolic
bys _traj_Group: sum ini_diastolic
bys _traj_Group: sum MAPmmHg
bys _traj_Group: tab BPGoalLLowBPGoalMUsual
bys _traj_Group: tab householdincomecat
bys _traj_Group: tab drinking
bys _traj_Group: sum ini_hdl
bys _traj_Group: sum ini_ldl
bys _traj_Group: sum ini_tc
bys _traj_Group: sum ini_tg
bys _traj_Group: tab apol1_additive
bys _traj_Group: tab householdincomecat

bys _traj_Group: sum AgeatRandomization,d
bys _traj_Group: sum ini_glucose,d



mdesc egfr uricacid AgeatRandomization ///
drinking Smoking0Never1CurrentSmoke householdincomecat ini_bmi_cl1 ///
ini_egfr_cl1 ini_upro_cl1 ini_upcr_cl1 ///
apol1_additive ini_glucose_cl1 ini_systolic_cl1 ini_tc_cl1


oneway AgeatRandomization _traj_Group, tab
tab Gender1Male2Female _traj_Group, chi2
oneway ini_bmi _traj_Group, tab
tab Smoking0Never1CurrentSmoke _traj_Group, chi2
oneway ini_glucose _traj_Group, tab
oneway SerumAlbumingdL _traj_Group, tab
oneway ini_crea _traj_Group, tab
oneway ini_egfr _traj_Group, tab
oneway ini_upcr _traj_Group, tab
oneway ini_ua _traj_Group, tab
oneway ini_systolic _traj_Group, tab
oneway ini_diastolic _traj_Group, tab
oneway MAPmmHg _traj_Group, tab
tab BPGoalLLowBPGoalMUsual _traj_Group, chi2
tab householdincomecat _traj_Group, chi2
tab drinking _traj_Group, chi2
oneway ini_hdl _traj_Group, tab
oneway ini_ldl _traj_Group, tab
oneway ini_tc _traj_Group, tab
oneway ini_tg _traj_Group, tab
tab apol1_additive _traj_Group, chi2
tab householdincomecat _traj_Group, chi2


log close



sort sinai Date
egen date_max = max(Date), by(sinai)
format date_max %tdNN/DD/CCYY


