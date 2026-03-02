********************************************************************************
*                               Preliminaries                                  *
********************************************************************************
clear all


use "$output/data_cand_8897", clear

**define sets of controls 
global census90="pasdedip1990  sup1990 agri1990 ouvr1990  pop_65_plus1990  pop_15_241990"
global urbancontrol="cheflieudep nb_villes"
global cc="CC_charges_fonctio CC_produits_fonctio"
global dads = "DADS_nbestab DADS_sumwage DADS_share_top1  DADS_nbworker"
global elec = "limit_cst change_limit_cst inscrits1 elec_margin_c_88 secround_circo_c_88"

//time-varying controls only
global cand_cont = "female rerun Dincumbent mayor other_mandate"
global circo_cont="limit_cst c_female c_rerun c_Dincumbent c_mayor c_other_mandate inscrits1 nb_party1 nb_party2 nb_party3 nb_party4 nb_party5 nb_party6 nb_party7 nb_party8 nb_party9 nb_party10 nb_party11 nb_party12 nb_party13 nb_party14 nb_party15 nb_party16 nb_party17 nb_party18 DADS_nbestab DADS_sumwage DADS_share_top1  DADS_nbworker CC_charges_fonctio CC_produits_fonctio"

global trends = "ratio_local1_partydep_8188 ratio_local1_partydep_7881 ratio_local1_partydep_7378 ratio_local1_partydep_6873 ratio_local1_partydep_6768"

**fixed effects
egen id_circo=group(codegeo) 
egen id_indiv=group(codegeo party id_cand)	
egen id_yearparty=group(party year) 
egen id_year=group(year)
egen id_circoyear=group(codegeo year) 
egen id_partycirc=group(codegeo party) 

**predicted amount of Firm donations per voter in 1993
capture drop temp_* disp_*
foreach x of varlist *_i_93 *_c_93 change_limit_cst elec_margin_c_88 secround_circo_c_88{
gen temp_`x'=`x'
gen disp_`x'=`x'==.
replace temp_`x'=-1 if `x'==.
}

reghdfe dons_firms temp_* disp_* if year==1993, absorb(id_yearparty) cluster(id_circo)
predict pred93 if year==1993
drop temp_* disp_*

byso id_cand: egen pred_dons_firms=min(pred93) //within candidate
gen inter_pred_dons_firms=pred_dons_firms*id_year

byso id_partycirc: egen pred_dons_firms_p=mean(pred93) if party!="other" //within party*district
gen inter_pred_dons_firms_p=pred_dons_firms_p*id_year

**same, including initial contributions in 93
capture drop temp_* disp_*
foreach x of varlist dons_indiv_93 party_contrib_93 personal_contrib_93 *_i_93 *_c_93 change_limit_cst elec_margin_c_88 secround_circo_c_88{
gen temp_`x'=`x'
gen disp_`x'=`x'==.
replace temp_`x'=-1 if `x'==.
}

reghdfe dons_firms temp_* disp_* if year==1993, absorb(id_yearparty) cluster(id_circo)
predict pred93_2 if year==1993
drop temp_* disp_*

byso id_cand: egen pred_dons_firms_2=min(pred93_2) //within candidate
gen inter_pred_dons_firms_2=pred_dons_firms_2*id_year

byso id_partycirc: egen pred_dons_firms_p_2=mean(pred93_2) if party!="other" //within party*district
gen inter_pred_dons_firms_p_2=pred_dons_firms_p_2*id_year

save "$temp/analysis", replace


********************************************************************************
*                                    Tables                                    *
********************************************************************************

********************************************************************************
* Table 3: Impact of firm donations on local vs. national campaigning
global cand_cont = "female rerun Dincumbent mayor other_mandate"

estimates clear
local j=0

foreach out in ratio_local1_std sh_local1 sh_nat1 { 

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

* control
foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

* fixed effects	
byso id_indiv: gen temp=_N
keep if temp>1 //restrict the sample to candidates running twice
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 //we put candidates who are alone in their party in a common year fixed effect
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 //we drop singletons if we cannot make a category with more than one candidate
drop temp

* redefine Firm donations as loss in donations
replace std_dons_firms=-std_dons_firms
label var std_dons_firms "Firm donations (loss)"

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms std_dons_firms, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store indiv_`j'
}

esttab indiv_1 indiv_2 indiv_3 using "$main/Table3.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 



///////////////// ROBUSTNESS CHECK 1: No controls ////////////////////////

* Table 3: Impact of firm donations on local vs. national campaigning

estimates clear
local j=0

foreach out in ratio_local1_std sh_local1 sh_nat1 { 

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

* control
foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

* fixed effects	
byso id_indiv: gen temp=_N
keep if temp>1 //restrict the sample to candidates running twice
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 //we put candidates who are alone in their party in a common year fixed effect
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 //we drop singletons if we cannot make a category with more than one candidate
drop temp

* redefine Firm donations as loss in donations
replace std_dons_firms=-std_dons_firms
label var std_dons_firms "Firm donations (loss)"

reghdfe `out' disp_* /*$cand_cont inter_pred_dons_firms*/ std_dons_firms, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store indiv_`j'
}

esttab indiv_1 indiv_2 indiv_3 using "$main/Table3.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 



///////////////// ROBUSTNESS CHECK 2: No candidate controls ////////////////////////

* Table 3: Impact of firm donations on local vs. national campaigning


estimates clear
local j=0

foreach out in ratio_local1_std sh_local1 sh_nat1 { 

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

* control
foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

* fixed effects	
byso id_indiv: gen temp=_N
keep if temp>1 //restrict the sample to candidates running twice
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 //we put candidates who are alone in their party in a common year fixed effect
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 //we drop singletons if we cannot make a category with more than one candidate
drop temp

* redefine Firm donations as loss in donations
replace std_dons_firms=-std_dons_firms
label var std_dons_firms "Firm donations (loss)"

reghdfe `out' disp_* /*$cand_cont*/ inter_pred_dons_firms std_dons_firms, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store indiv_`j'
}

esttab indiv_1 indiv_2 indiv_3 using "$main/Table3.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 





///////////////// ROBUSTNESS CHECK 3: Breaking Down `Other Mandate': Three Distinct Roles ////////////////////////

* Table 3: Impact of firm donations on local vs. national campaigning

global cand_cont = "female rerun Dincumbent mayor conseiller_departemental senateur depute_europeen"

estimates clear
local j=0

foreach out in ratio_local1_std sh_local1 sh_nat1 { 

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

* control
foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

* fixed effects	
byso id_indiv: gen temp=_N
keep if temp>1 //restrict the sample to candidates running twice
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 //we put candidates who are alone in their party in a common year fixed effect
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 //we drop singletons if we cannot make a category with more than one candidate
drop temp

* redefine Firm donations as loss in donations
replace std_dons_firms=-std_dons_firms
label var std_dons_firms "Firm donations (loss)"

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms std_dons_firms, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store indiv_`j'
}

esttab indiv_1 indiv_2 indiv_3 using "$main/Table3.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 


///////////////// ROBUSTNESS CHECK 4: Number of Mandates Instead of Individual Mandates ////////////////////////

* Table 3: Impact of firm donations on local vs. national campaigning

global cand_cont = "female rerun Dincumbent mandate_count"

estimates clear
local j=0

foreach out in ratio_local1_std sh_local1 sh_nat1 { 

local j=`j'+1

use "$temp/analysis", clear
egen mandate_count = rowtotal(mayor conseiller_departemental senateur depute_europeen) 

keep if `out'!=. & sample_rest==1 & sample_did==1

* control
foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

* fixed effects	
byso id_indiv: gen temp=_N
keep if temp>1 //restrict the sample to candidates running twice
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 //we put candidates who are alone in their party in a common year fixed effect
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 //we drop singletons if we cannot make a category with more than one candidate
drop temp

* redefine Firm donations as loss in donations
replace std_dons_firms=-std_dons_firms
label var std_dons_firms "Firm donations (loss)"

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms std_dons_firms, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store indiv_`j'
}

esttab indiv_1 indiv_2 indiv_3 using "$main/Table3.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 



