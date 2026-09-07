*! ============================================================================
*! 01_analysis.do -- regression analysis (Appendix Tables A5-A11)
*!
*! ARCHIVED, NOT WIRED INTO THE PIPELINE. Kept here as the source of record for
*! the regression results in the manuscript.
*!
*! INPUT PATH: the original reads from "H:\Econ Thesis\filtered_GSS.csv".
*! The equivalent file in this repo is data/processed/filtered_GSS.csv, built
*! by R/01_prepare_data.R. 
*!
*! Produces: regression_results.rtf, happiness_models.doc, vce_results.rtf,
*!           combined_models.rtf, regression_results_temporal.doc
*! ============================================================================

clear all
import delimited "H:\Econ Thesis\filtered_GSS.csv", clear
cd "H:\Econ Thesis"

drop if year == 2002
* Step 4: Label key variables
rename alternative_w~r alt_worker_dummy
label variable alt_worker_dummy "Alternative Work Arrangement (Dummy)"
label variable alternative_w~s "Alternative Work Arrangement (Status Label)"
label define altwork_lbl 0 "Regular Worker" 1 "Alt Work Arrangement"
label values alt_worker_dummy altwork_lbl
label variable happy "General Happiness"
label variable satjob "Job Satisfaction"
label variable educ "Education (Years)"
label variable conrinc "Constant Dollar Income"
label variable weekswrk "Weeks Worked"
label variable sex "Gender"
label variable married_dummy "Married"

* Encode Value and assign reference
encode happy, gen(happy_num)
encode satjob, gen(satjob_num)
encode region, gen(region_num)
encode broad_industry, gen(industry_num)

encode race, gen(race_num)
label list race_num
char race_num[omit] 3

encode sex, gen(sex_num)
label list sex_num
char sex_num[omit] 2


gen income_k = conrinc / 1000

* Recode satjob_num in the correct ordinal order
gen satjob_num_fix = .

replace satjob_num_fix = 1 if satjob == "Very dissatisfied"
replace satjob_num_fix = 2 if satjob == "A little dissatisfied"
replace satjob_num_fix = 3 if satjob == "Moderately satisfied"
replace satjob_num_fix = 4 if satjob == "Very satisfied"

* Replace the original variable
drop satjob_num
rename satjob_num_fix satjob_num
tab satjob_num


* Step 5: Summary Statistics: testing if signficant
gen happy_not = (happy_num == 1)
gen happy_pretty = (happy_num == 2)
gen happy_very = (happy_num == 3)

prtest happy_not, by(alt_worker_dummy)
prtest happy_pretty, by(alt_worker_dummy)
prtest happy_very, by(alt_worker_dummy)

gen sat_verydissatisfied = (satjob_num == 1)
gen sat_alittledissatisfied = (satjob_num == 2)
gen sat_moderate = (satjob_num == 3)
gen sat_very = (satjob_num == 4)

prtest sat_verydissatisfied, by(alt_worker_dummy)
prtest sat_alittledissatisfied, by(alt_worker_dummy)
prtest sat_moderate, by(alt_worker_dummy)
prtest sat_very, by(alt_worker_dummy)


prtest mproff, by(alt_worker_dummy)
ttest educ, by(alt_worker_dummy)
ttest income_k, by(alt_worker_dummy)
gen female = (sex_num == 1)
prtest female, by(alt_worker_dummy)
ttest weekswrk, by(alt_worker_dummy)
ttest age, by(alt_worker_dummy)
prtest married_dummy, by(alt_worker_dummy)

* By worktype now
drop if wrktype == "Paid by a temporary agency"
encode wrktype, gen(emp_type)
label variable emp_type "Employment Relationship"
label list emp_type
char emp_type[omit] 3
tab emp_type

gen is_regular  = (emp_type == 3)
gen is_indep    = (emp_type == 1)
gen is_oncall   = (emp_type == 2)
gen is_contract = (emp_type == 4)

foreach group in indep oncall contract {

    display "========================================"
    display "Comparing Regular Employees vs. `group'"
    display "========================================"

    * Happiness breakdown
    prtest happy_not    if is_regular | is_`group', by(is_`group')
    prtest happy_pretty if is_regular | is_`group', by(is_`group')
    prtest happy_very   if is_regular | is_`group', by(is_`group')

    * Job satisfaction breakdown
    prtest sat_verydissatisfied    if is_regular | is_`group', by(is_`group')
    prtest sat_alittledissatisfied if is_regular | is_`group', by(is_`group')
    prtest sat_moderate            if is_regular | is_`group', by(is_`group')
    prtest sat_very                if is_regular | is_`group', by(is_`group')

    * Covariates
    prtest mproff if is_regular | is_`group', by(is_`group')
    ttest educ if is_regular | is_`group', by(is_`group')
    ttest income_k if is_regular | is_`group', by(is_`group')
    ttest age if is_regular | is_`group', by(is_`group')
    prtest female if is_regular | is_`group', by(is_`group')
    ttest weekswrk if is_regular | is_`group', by(is_`group')
    prtest married_dummy if is_regular | is_`group', by(is_`group')
}




* Step 6: Regression Model
eststo h1: reg happy_num ib3.emp_type, vce(robust)
eststo h2: reg happy_num ib3.emp_type mproff educ income_k age married_dummy ib2.sex_num ib3.race_num, vce(robust)
eststo h3: reg happy_num ib3.emp_type mproff educ income_k age married_dummy ib2.sex_num ib3.race_num ///
    i.region_num i.year, vce(robust)
eststo h4: reg happy_num ib3.emp_type mproff educ income_k age married_dummy ib2.sex_num ib3.race_num ///
    i.region_num i.year i.industry_num, vce(robust)

eststo j1: reg satjob_num ib3.emp_type, vce(robust)
eststo j2: reg satjob_num ib3.emp_type mproff educ income_k age married_dummy ib2.sex_num ib3.race_num, vce(robust)
eststo j3: reg satjob_num ib3.emp_type mproff educ income_k age married_dummy ib2.sex_num ib3.race_num ///
    i.region_num i.year, vce(robust)
eststo j4: reg satjob_num ib3.emp_type mproff educ income_k age married_dummy ib2.sex_num ib3.race_num ///
    i.region_num i.year i.industry_num, vce(robust)

* Run model with clustered SEs
esttab h1 h2 h3 h4 j1 j2 j3 j4 using regression_results.rtf, ///
    b(%9.3f) se(%9.3f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    label ///
    mtitles("H1" "H2" "H3" "H4" "J1" "J2" "J3" "J4") ///
    title("Effect of Alternative Work on Happiness and Job Satisfaction") ///
    align(center) ///
    compress replace ///
    nonumber noobs


* Step 7: Medidation Through Job Satisfaction
eststo clear

* Model 1: Employment Type only
reg happy_num ib3.emp_type, vce(robust)
outreg2 using happiness_models.doc, replace se dec(3) addstat(Adj R-squared, e(r2_a)) ctitle(Model 1)

* Model 2: Add Job Satisfaction
reg happy_num ib3.emp_type satjob_num, vce(robust)
outreg2 using happiness_models.doc, append se dec(3) addstat(Adj R-squared, e(r2_a)) ctitle(Model 2)

* Model 3: Add Sociodemographic Controls
reg happy_num ib3.emp_type satjob_num mproff educ income_k age married_dummy ib2.sex_num ib3.race_num, vce(robust)
outreg2 using happiness_models.doc, append se dec(3) addstat(Adj R-squared, e(r2_a)) ctitle(Model 3)

* Model 4: Add Fixed Effects
reg happy_num ib3.emp_type satjob_num mproff educ income_k age married_dummy ib2.sex_num ib3.race_num ///
    i.region_num i.industry_num i.year, vce(robust)
outreg2 using happiness_models.doc, append se dec(3) addstat(Adj R-squared, e(r2_a)) ctitle(Model 4)


* Step 8: mediatation test
sem (satjob_num <- alt_worker_dummy educ income_k age married_dummy sex_num race_num region_num industry_num year) ///
    (happy_num <- satjob_num alt_worker_dummy educ income_k age married_dummy sex_num race_num region_num industry_num year)

estat teffects

* for each worker
tab emp_type, gen(emp_dummy)

* Mediation by employment type — compare each type vs reference (emp_type == 3)
levelsof emp_type, local(types)

foreach t of local types {
    * Skip emp_type 3 if you want it as reference
    if `t' != 3 {
        gen emptype_`t' = (emp_type == `t')

        display "===== Employment Type `t' vs Regular (Type 3) ====="
        sem (satjob_num <- emptype_`t' educ income_k age married_dummy sex_num race_num region_num industry_num year) ///
            (happy_num <- satjob_num emptype_`t' educ income_k age married_dummy sex_num race_num region_num industry_num year)
        estat teffects
    }
}


* Step 9: Robustness Check
* eststo h1: reg happy_num ib3.emp_type, vce(cluster industry_num)
eststo x2: reg happy_num ib3.emp_type mproff educ income_k age married_dummy ib2.sex_num ib3.race_num ///
    i.region_num i.industry_num i.year, vce(cluster industry_num)

* eststo j1: reg satjob_num ib3.emp_type, vce(cluster industry_num)
eststo y2: reg satjob_num ib3.emp_type mproff educ income_k age married_dummy ib2.sex_num ib3.race_num ///
    i.region_num i.industry_num i.year, vce(cluster industry_num)

esttab x2 y2 using vce_results.rtf, ///
    b(%9.3f) se(%9.3f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    label ///
    mtitles("H1" "H2") ///
    title("Clustering Standard Error by Industry") ///
    align(center) ///
    compress replace ///
    nonumber noobs



* Run ordered logistic regressions
* ologit happy_num ib3.emp_type, vce(cluster industry_num)
ologit happy_num ib3.emp_type mproff educ income_k age married_dummy ib2.sex_num ib3.race_num ///
    i.region_num i.industry_num i.year, vce(cluster industry_num)

eststo happy_model

* ologit satjob_num ib3.emp_type, vce(cluster industry_num)
ologit satjob_num ib3.emp_type mproff educ income_k age married_dummy ib2.sex_num ib3.race_num ///
    i.region_num i.industry_num i.year, vce(cluster industry_num)

eststo satjob_model

* Quadratic
gen age2 = age^2
gen income_k2 = income_k^2

*reg happy_num ib3.emp_type mproff educ income_k income_k2 age age2 married_dummy ib2.sex_num ib3.race_num ///
    i.region_num i.industry_num i.year, vce(cluster industry_num)

*reg satjob_num ib3.emp_type mproff educ income_k income_k2 age age2 married_dummy ib2.sex_num ib3.race_num ///
    i.region_num i.industry_num i.year, vce(cluster industry_num)


eststo reg_happy_quad: reg happy_num ib3.emp_type##c.age ib3.emp_type##c.age2 mproff educ ///
    ib3.emp_type##c.income_k ib3.emp_type##c.income_k2 married_dummy ib2.sex_num ib3.race_num ///
    i.region_num i.industry_num i.year, vce(cluster industry_num)

eststo reg_satjob_quad: reg satjob_num ib3.emp_type##c.age ib3.emp_type##c.age2 mproff educ ///
    ib3.emp_type##c.income_k ib3.emp_type##c.income_k2 married_dummy ib2.sex_num ib3.race_num ///
    i.region_num i.industry_num i.year, vce(cluster industry_num)

esttab happy_model satjob_model reg_happy_quad reg_satjob_quad using combined_models.rtf, ///
    b(3) se(3) star(* 0.1 ** 0.05 *** 0.01) label compress replace ///
    title("Combined Models: Ordered Logit and OLS with Quadratic Terms") ///
    mtitles("Ologit: Happiness" "Ologit: Job Satisfaction" "OLS: Happiness (Quad)" "OLS: Job Sat. (Quad)") ///
    nonumber noobs align(center)



* Explore heterogenity between industries*

levelsof industry_num, local(ind_nums)

foreach i of local ind_nums {
    preserve
    keep if industry_num == `i'

    * Get readable label (optional)
    local label : label industry_num `i'
    di "=== Running for Industry `i': `label' ==="

    * Run regression
    reg happy_num ib3.emp_type satjob_num educ income_k age married_dummy ///
        ib2.sex_num ib3.race_num i.region_num i.year, vce(robust)

    * Safe estimate name using numeric code only
    estimates store model_ind`i'

    restore
}



levelsof industry_num, local(ind_nums)

foreach i of local ind_nums {
    preserve
    keep if industry_num == `i'

    * Get readable label (optional)
    local label : label industry_num `i'
    di "=== Running for Industry `i': `label' ==="

    * Run regression

    reg satjob_num ib3.emp_type educ income_k age married_dummy ///
        ib2.sex_num ib3.race_num i.region_num i.year, vce(robust)

    * Safe estimate name using numeric code only
    estimates store model_ind`i'

    restore
}


* Time trends
* First, run the regression with employment type-year interaction
eststo t1: reg happy_num ib3.emp_type##i.year mproff educ income_k age married_dummy ib2.sex_num ib3.race_num ///
     i.region_num i.industry_num, vce(cluster industry_num)
eststo t2: reg satjob_num ib3.emp_type##i.year mproff educ income_k age married_dummy ib2.sex_num ib3.race_num ///
     i.region_num i.industry_num, vce(cluster industry_num)


esttab t1 t2 using "regression_results_temporal.doc", ///
    b(3) se(3) star(* 0.1 ** 0.05 *** 0.01) label compress replace ///
    title("Temporal Analysis") ///
    mtitles("Happiness" "Job Satisfaction") ///
    nonumber noobs align(center)


* Interaction terms to consider
* Sex
reg happy_num ib3.emp_type##ib2.sex_num mproff educ income_k age married_dummy ib3.race_num ///
    i.region_num i.industry_num i.year, vce(cluster industry_num)
reg satjob_num ib3.emp_type##ib2.sex_num mproff educ income_k age married_dummy ib3.race_num ///
    i.region_num i.industry_num i.year, vce(cluster industry_num)
* Marriage
reg happy_num ib3.emp_type##i.married_dummy mproff educ income_k age ib2.sex_num ib3.race_num ///
    i.region_num i.industry_num i.year, vce(cluster industry_num)
reg satjob_num ib3.emp_type##i.married_dummy mproff educ income_k age ib2.sex_num ib3.race_num ///
    i.region_num i.industry_num i.year, vce(cluster industry_num)
* Occupation Status
reg happy_num ib3.emp_type##i.mproff educ income_k age ib2.sex_num ib3.race_num ///
    i.region_num i.industry_num i.year, vce(cluster industry_num)
reg satjob_num ib3.emp_type##i.mproff educ income_k age ib2.sex_num ib3.race_num ///
    i.region_num i.industry_num i.year, vce(cluster industry_num)


* Store models with eststo
eststo h1: reg happy_num ib3.emp_type##ib2.sex_num mproff educ income_k age married_dummy ib3.race_num ///
    i.region_num i.industry_num i.year, vce(cluster industry_num)

eststo s1: reg satjob_num ib3.emp_type##ib2.sex_num mproff educ income_k age married_dummy ib3.race_num ///
    i.region_num i.industry_num i.year, vce(cluster industry_num)

eststo h2: reg happy_num ib3.emp_type##i.married_dummy mproff educ income_k age ib2.sex_num ib3.race_num ///
    i.region_num i.industry_num i.year, vce(cluster industry_num)

eststo s2: reg satjob_num ib3.emp_type##i.married_dummy mproff educ income_k age ib2.sex_num ib3.race_num ///
    i.region_num i.industry_num i.year, vce(cluster industry_num)

eststo h3: reg happy_num ib3.emp_type##i.mproff educ income_k age ib2.sex_num ib3.race_num ///
    i.region_num i.industry_num i.year, vce(cluster industry_num)

eststo s3: reg satjob_num ib3.emp_type##i.mproff educ income_k age ib2.sex_num ib3.race_num ///
    i.region_num i.industry_num i.year, vce(cluster industry_num)
