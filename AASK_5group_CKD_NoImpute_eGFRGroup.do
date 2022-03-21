

cd "E:\DATA\Cho\CRIC_AASK\AASK"
log using "E:\DATA\Cho\CRIC_AASK\AASK_5group_CKD_NOIMPUTE_eGFRGroup", text replace

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





///////////////////eGFR///////////////////////////////////////////////////////////
use temp, clear
drop if egfr==.
sort pid Date 


bys pid: gen nvisit = _n

table nvisit


keep pid nvisit egfr Gender1Male2Female AgeatRandomization t_duration1


xtmixed egfr Gender1Male2Female##c.t_duration1 AgeatRandomization || pid:, var reml

margins, at(t_duration1=(0(3)12)) vsquish

marginsplot, x(t_duration1)


reshape wide egfr Gender1Male2Female AgeatRandomization t_duration1,i(pid) j(nvisit)


save tempa.dta, replace


// net from http://www.andrew.cmu.edu/user/bjones/trajplot
// net install traj, force

////////// basic model by time
// all model
use tempa, clear


traj, var(egfr*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(1 1 1) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous eGFR in AASK) ci 

keep pid _traj_Group
save 3egfr.dta, replace

use tempa, clear
traj, var(egfr*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(1 1 1 1) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous eGFR in AASK) ci 
keep pid _traj_Group
save 4egfr.dta, replace

use tempa, clear
traj, var(egfr*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(1 1 1 1 1) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous eGFR in AASK) ci
keep pid _traj_Group
save 5egfr.dta, replace




use tempa, clear
traj, var(egfr*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(1 1 1 1 1 1) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous eGFR in AASK) ci
keep pid _traj_Group
save 6egfr.dta, replace



use tempa, clear
traj, var(egfr*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(2 2 2 2) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous eGFR in AASK) ci 
keep pid _traj_Group
save 4egfr_quad.dta, replace

use tempa, clear
traj, var(egfr*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(3 3 3 3) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous eGFR in AASK) ci 
keep pid _traj_Group
save 4egfr_cubic.dta, replace
// target: group4



use temp,clear

merge m:1 pid using 5egfr
gen egfr5 = _traj_Group


xtmixed egfr egfr5##c.t_duration1 AgeatRandomization Gender1Male2Female || pid:, var reml
margins egfr5, at(t_duration1=(0(3)12)) vsquish
marginsplot, x(t_duration1)



///////////////////sUA///////////////////////////////////////////////////////////
use temp, clear
drop if uricacid==.
sort pid Date 


bys pid: gen nvisit = _n

table nvisit

keep pid nvisit uricacid Gender1Male2Female AgeatRandomization t_duration1



reshape wide uricacid Gender1Male2Female AgeatRandomization t_duration1,i(pid) j(nvisit)


save tempa.dta, replace


// net from http://www.andrew.cmu.edu/user/bjones/trajplot
// net install traj, force

////////// basic model by time
// all model
use tempa, clear


traj, var(uricacid*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(1 1 1) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous sUA in AASK) ci 

keep pid _traj_Group
save 3sua.dta, replace

use tempa, clear
traj, var(uricacid*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(1 1 1 1) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous sUA in AASK) ci 
keep pid _traj_Group
save 4sua.dta, replace

use tempa, clear
traj, var(uricacid*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(1 1 1 1 1) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous sUA in AASK) ci
keep pid _traj_Group
save 5sua.dta, replace


use tempa, clear
traj, var(uricacid*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(1 1 1 1 1 1) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous sUA in AASK) ci
keep pid _traj_Group
save 6sua.dta, replace


use tempa, clear
traj, var(uricacid*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(1 1 1 1 1 1 1) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous sUA in AASK) ci 
keep pid _traj_Group
save 7sua.dta, replace



use tempa, clear
traj, var(uricacid*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(2 2 2 2 2) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous sUA in AASK) ci 
keep pid _traj_Group
save 5sua_quad.dta, replace

use tempa, clear
traj, var(uricacid*) indep(t_duration1*) model(cnorm) min(-999) max(999) order(3 3 3 3 3) sigmabygroup detail
trajplot, xtitle(# of visit) ytitle(Continuous sUA in AASK) ci 
keep pid _traj_Group
save 5sua_cubic.dta, replace
// target: group4


use temp,clear

merge m:1 pid using 5sua_cubic
gen sua5_cubic = _traj_Group
drop _traj_Group
drop _merge

merge m:1 pid using 4egfr_quad
gen egfr4_quad = _traj_Group
drop _traj_Group
drop _merge

recode sua5_cubic (1=0)(2=0)(3=0)(4=1)(5=0), gen(sua5_cubic_bin)
recode egfr4_quad (1=1)(2=0)(3=0)(4=0), gen(egfr4_quad_bin)

save temp1, replace

sort pid Date
quietly by pid: gen dup = cond(_N==1,0,_n)
drop if dup > 1

drop if sua5_cubic_bin == .
drop if egfr4_quad_bin == .

tab sua5_cubic_bin egfr4_quad_bin, all

use temp1,clear


xtmixed egfr ib1.sua5_cubic##c.t_duration1 AgeatRandomization Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1 || pid:, var reml
margins sua5_cubic, at(t_duration1=(0(3)12)) vsquish
marginsplot, x(t_duration1)



xtmixed egfr ib4.sua5_cubic##c.t_duration1 AgeatRandomization Gender1Male2Female ///
i.drinking i.Smoking0Never1CurrentSmoke i.householdincomecat c.ini_bmi_cl1 ///
c.ini_egfr_cl1 c.ini_upro_cl1 c.ini_upcr_cl1 ///
c.ini_glucose_cl1 c.ini_systolic_cl1 c.ini_tc_cl1 || pid:, var reml
margins sua5_cubic, at(t_duration1=(0(3)12)) vsquish
marginsplot, x(t_duration1)

log close


