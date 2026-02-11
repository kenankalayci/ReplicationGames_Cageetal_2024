********************************************************************************
*                               Preliminaries                                  *
********************************************************************************

clear all

use "$output/data_cand_8897", clear

global census90="pasdedip1990  sup1990 agri1990 ouvr1990  pop_65_plus1990  pop_15_241990"
global urbancontrol="cheflieudep nb_villes"
global cc="CC_charges_fonctio CC_produits_fonctio"
global dads = "DADS_nbestab DADS_sumwage DADS_share_top1  DADS_nbworker"
global elec = "limit_cst change_limit_cst inscrits1 elec_margin_c_88 secround_circo_c_88"

global cand_cont = "female rerun Dincumbent mayor other_mandate"
global circo_cont="limit_cst c_female c_rerun c_Dincumbent c_mayor c_other_mandate inscrits1 nb_party1 nb_party2 nb_party3 nb_party4 nb_party5 nb_party6 nb_party7 nb_party8 nb_party9 nb_party10 nb_party11 nb_party12 nb_party13 nb_party14 nb_party15 nb_party16 nb_party17 nb_party18 DADS_nbestab DADS_sumwage DADS_share_top1  DADS_nbworker CC_charges_fonctio CC_produits_fonctio"

global trends = "ratio_local1_partydep_8188 ratio_local1_partydep_7881 ratio_local1_partydep_7378 ratio_local1_partydep_6873 ratio_local1_partydep_6768"

egen id_circo=group(codegeo) 
egen id_indiv=group(codegeo party id_cand)	
egen id_yearparty=group(party year) 
egen id_year=group(year)
egen id_circoyear=group(codegeo year) 
egen id_partycirc=group(codegeo party) 

capture drop temp_* disp_*
foreach x of varlist *_i_93 *_c_93 change_limit_cst elec_margin_c_88 secround_circo_c_88{
gen temp_`x'=`x'
gen disp_`x'=`x'==.
replace temp_`x'=-1 if `x'==.
}

reghdfe dons_firms temp_* disp_* if year==1993, absorb(id_yearparty) cluster(id_circo)
predict pred93 if year==1993
drop temp_* disp_*

byso id_cand: egen pred_dons_firms=min(pred93) 
gen inter_pred_dons_firms=pred_dons_firms*id_year

byso id_partycirc: egen pred_dons_firms_p=mean(pred93) if party!="other" 
gen inter_pred_dons_firms_p=pred_dons_firms_p*id_year

capture drop temp_* disp_*
foreach x of varlist dons_indiv_93 party_contrib_93 personal_contrib_93 *_i_93 *_c_93 change_limit_cst elec_margin_c_88 secround_circo_c_88{
gen temp_`x'=`x'
gen disp_`x'=`x'==.
replace temp_`x'=-1 if `x'==.
}

reghdfe dons_firms temp_* disp_* if year==1993, absorb(id_yearparty) cluster(id_circo)
predict pred93_2 if year==1993
drop temp_* disp_*

byso id_cand: egen pred_dons_firms_2=min(pred93_2) 
gen inter_pred_dons_firms_2=pred_dons_firms_2*id_year

byso id_partycirc: egen pred_dons_firms_p_2=mean(pred93_2) if party!="other" 
gen inter_pred_dons_firms_p_2=pred_dons_firms_p_2*id_year

save "$temp/analysis", replace

********************************************************************************
*                                    Tables                                    *
********************************************************************************


********************************************************************************
* Table E.10: Comparison of included and excluded observations

use "$temp/analysis" , clear

keep if sample_did==1

byso id_indiv: gen temp=_N
gen  selec=-(temp>1) //opposite sign for table
drop temp

keep if year==1993 & sample_rest==1

est clear

********************************************************************************	
quietly do "$dofiles/_labels.do"
********************************************************************************

estpost  ttest female rerun  Dincumbent  mayor other_mandate  revenues dons_firms dons_indiv personal_contrib party_contrib, by(selec) esample

esttab using "$appendix/TableE10.tex", ///
replace ///
cell("mu_1(fmt(%15.2fc) label(Mean included))  N_1(fmt(%15.0fc) label(N included))  mu_2(fmt(%15.2fc) label(Mean excluded)) N_2(fmt(%15.0fc) label(N excluded)) b (fmt(%15.2fc) label(Diff)) p (fmt(%15.2fc) label(p-value)) " ) wide ///
label noobs nonumber


********************************************************************************
* Table E.11: Firm donations and selection into sample
estimates clear 

use "$temp/analysis", clear

keep if sample_rest==1 & sample_did==1

gen temp=text_size_sr1!=.
byso id_indiv: egen temp2=total(temp)
gen nomissing_manif=temp2==2
drop temp*
ta nomissing_manif if year==1993

gen temp=montant_total!=.
byso id_indiv: egen temp2=total(temp)
gen nomissing_dons=temp2==2
drop temp*
ta nomissing_dons if year==1993

* control
foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

keep if year==1993

reghdfe rerun_select  disp_* $cand_cont pred_dons_firms std_dons_firms, absorb(id_yearparty) cluster(id_circo)	
sum rerun_select if year==1993
estadd scalar ymean=r(mean)
estimate store select_1

reghdfe nomissing_manif disp_* $cand_cont pred_dons_firms std_dons_firms if rerun_select==1, absorb(id_yearparty) cluster(id_circo)	
sum nomissing_manif if year==1993
estadd scalar ymean=r(mean)
estimate store select_2


use "$temp/analysis", clear

keep if sample_rest==1 & sample_did==1

keep if bigparty!="other" & bigparty!=""

foreach x in $cand_cont{
byso id_partycirc year: egen p_`x'=mean(`x')
gen disp_`x'=p_`x'==.
replace p_`x'=-1 if p_`x'==.
}

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

keep if year==1993

reghdfe party_select  disp_* $cand_cont pred_dons_firms std_dons_firms, absorb(id_yearparty) cluster(id_circo)
sum party_select if year==1993
estadd scalar ymean=r(mean)
estimate store select_3

esttab select_*  using "$appendix/TableE11.tex", ///
replace keep(std_dons_firms) b(3) se lab nomtitles ///
style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Candidate in\\next election}" "\shortstack{Manifesto\\available}" "\shortstack{Party in\\next election}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars("ymean Mean outcome before ban" "r2_within R2-Within")	


********************************************************************************
*Table E.12: Impact of firm donations on total revenue and other contributions & Table E.13: Impact of firm donations on shares of other contributions in total revenue

estimates clear

**contributions
local j=0

foreach out in revenues_main dons_indiv party_contrib personal_contrib{

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp	

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

replace dons_firms=-dons_firms

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms dons_firms, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store sub_`j'
}

**share in total revenue
local j=0

foreach out in sh_dons_indiv_main sh_party_contrib_main sh_personal_contrib_main{

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

replace sh_dons_firms_main=-sh_dons_firms_main

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms sh_dons_firms_main, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store sub2_`j'
}

label var dons_firms "Firm donations (loss)"
label var sh_dons_firms_main "Share of firm don. (loss)"

*** Table  E12
esttab sub_*  using "$appendix/TableE12.tex", ///
replace keep(dons_firms) b(3) se lab nomtitles ///
style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Total\\revenue}" "\shortstack{Donations\\from individuals}" "\shortstack{Party\\contributions}" "\shortstack{Personal\\contributions}", pattern(1 1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars("ymean Mean outcome before ban" "r2_within R2-Within")

*** Table E13
esttab sub2_* using "$appendix/TableE13.tex", ///
replace keep(sh_dons_firms_main) b(3) se lab nomtitles ///
style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Share of donations\\from individuals}" "\shortstack{Share of party\\contributions}" "\shortstack{Share of personal\\contributions}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars("ymean Mean outcome before ban" "r2_within R2-Within")


********************************************************************************
* Table E.14: Robust impact on local vs. national campaigning, Depending on the availability of donations data

estimates clear

use "$temp/analysis", clear

local j=0
local k=0

foreach cond in "std_dons_firms!=." "montant_total==private_donation_firms" {
local k=`k'+1

foreach out of varlist ratio_local1_std sh_local1 sh_nat1{
local j=`j'+1

use "$temp/analysis", clear

keep if `cond'

keep if `out'!=. & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

replace std_dons_firms=-std_dons_firms
label var std_dons_firms "Firm donations (loss)"

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms std_dons_firms, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store std_`k'_`j'
}
}

*** Table E14a)
esttab std_1* using "$appendix/TableE14a.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 

*** Table E14b)
esttab std_2* using "$appendix/TableE14b.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 


********************************************************************************
* Table E.15: Robust impact on local vs. national campaigning, Clustering standard errors at the department level

estimates clear

local j=0

foreach out in ratio_local1_std sh_local1 sh_nat1{

local j=`j'+1

use "$temp/analysis", clear

egen id_dep=group(dep)

keep if `out'!=. &  sample_rest==1 & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

replace std_dons_firms=-std_dons_firms
label var std_dons_firms "Firm donations (loss)"

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms std_dons_firms, absorb(id_indiv id_yearparty) cluster(id_dep)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store clust_`j'
}

esttab clust_1 clust_2 clust_3 using "$appendix/TableE15.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 


********************************************************************************
*Table E.16: Robust impact on local vs. national campaigning, Alternative definitions of the local index

estimates clear
local j=0

foreach out in ratio_local1_std ratio_local1_2_std ratio_local1_3_std ratio_local1_5_std ratio_local1_5{ 

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

replace std_dons_firms=-std_dons_firms
label var std_dons_firms "Firm donations (loss)"

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms std_dons_firms, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store indiv_`j'
}

esttab indiv_1 indiv_2 indiv_3 indiv_4 indiv_5 using "$appendix/TableE16.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("Local index", pattern(1 0 0 0 0)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 


********************************************************************************
*Table E.17: Robust impact on local vs. national campaigning, Alternative definitions of firm donations loss

* Table E17a: Log firm donations loss
estimates clear

local j=0

foreach out in ratio_local1_std sh_local1 sh_nat1{

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

foreach x in dons_firms{
egen temp=sd(ln(`x'+1)) if year==1993
egen temp2=min(temp)
replace ln_`x'=-ln(`x'+1)/temp2
sum ln_`x'
drop temp*
}
label var ln_dons_firms "Log Firm donations (loss)"

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms ln_dons_firms, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store ln_`j'
}

esttab ln_1 ln_2 ln_3 using "$appendix/TableE17a.tex", ///
replace keep(ln_dons_firms)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 


********************************************************************************
* Table E17b: Losing any firm donations

estimates clear

local j=0

foreach out in ratio_local1_std sh_local1 sh_nat1{

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

replace atlone=-atlone
label var atlone "Losing Firm donations"

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms atlone, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store dummy_`j'
}

esttab dummy_1 dummy_2 dummy_3 using "$appendix/TableE17b.tex", ///
replace keep(atlone)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 


********************************************************************************
* Table E17c: Number of firm donations lost

estimates clear

local j=0

foreach out in ratio_local1_std sh_local1 sh_nat1{

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2
drop temp

replace nb_don=-nb_don
label var nb_don "Number of Firm donations"

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms nb_don, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store nb_`j'
}

esttab nb_1 nb_2 nb_3 using "$appendix/TableE17c.tex", ///
replace keep(nb_don)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 


********************************************************************************
* Table E17d: Quadratic loss in firm donations

estimates clear

local j=0

foreach out in ratio_local1_std sh_local1 sh_nat1{

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

replace std_dons_firms=-std_dons_firms
gen std_dons_firms2=(std_dons_firms^2)

label var std_dons_firms "Firm don."
label var std_dons_firms2 "Firm don.$^2$"

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms std_dons_firms std_dons_firms2, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store sq_`j'
}

esttab sq_1 sq_2 sq_3 using "$appendix/TableE17d.tex", ///
replace keep(std_dons_firms std_dons_firms2)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 


********************************************************************************
* Table E17e: Quintiles of loss in firm donations

estimates clear
local j=0

foreach out in ratio_local1_std sh_local1 sh_nat1 ratio_local1_5{ 

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

foreach x in "20" "40" "60" "80" "100"{
replace dons_firms_`x'p=-dons_firms_`x'p
}
label var dons_firms_20p "1st quintile"
label var dons_firms_40p "2nd quintile"
label var dons_firms_60p "3rd quintile"
label var dons_firms_80p "4th quintile"
label var dons_firms_100p "5th quintile"

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms dons_firms_20p dons_firms_40p dons_firms_60p dons_firms_80p dons_firms_100p, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store indiv_`j'
}	

esttab indiv_1 indiv_2 indiv_3 using "$appendix/TableE17e.tex", ///
replace keep(dons_firms_*)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 


********************************************************************************
* Table E.18: Robust impact on local vs. national campaigning, Changing the set of controls

* Table E18a: Including district times year fixed effects

estimates clear

local j=0

foreach out in ratio_local1_std sh_local1 sh_nat1{

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

byso id_circoyear: gen temp=_N
replace id_circoyear=10000+year if temp<2
drop temp

replace std_dons_firms=-std_dons_firms
label var std_dons_firms "Firm donations (loss)"
reghdfe `out' disp_* $cand_cont inter_pred_dons_firms std_dons_firms, absorb(id_indiv id_circoyear id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store circfe_`j'
}

esttab circfe_1 circfe_2 circfe_3 using "$appendix/TableE18a.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 


********************************************************************************
* Table E.18b: Including district-level characteristics

estimates clear

local j=0

foreach out in ratio_local1_std sh_local1 sh_nat1{

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

foreach x in $circo_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

replace std_dons_firms=-std_dons_firms
label var std_dons_firms "Firm donations (loss)"

reghdfe `out' disp_* $cand_cont $circo_cont inter_pred_dons_firms std_dons_firms, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store nocirc_`j'
}

esttab nocirc_1 nocirc_2 nocirc_3 using "$appendix/TableE18b.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 


********************************************************************************
* Table E.18c: Interacting a post-ban indicator with 1993 controls

estimates clear

local j=0

foreach out in ratio_local1_std sh_local1 sh_nat1{

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

gen post=year==1997
foreach x of varlist *_i_93 *_c_93 change_limit_cst elec_margin_c_88 secround_circo_c_88{ 
gen d_`x'=`x'==.
replace `x'=-1 if `x'==.
gen p_`x'=post*`x'
gen dp_`x'=post*d_`x'
}

replace std_dons_firms=-std_dons_firms
label var std_dons_firms "Firm donations (loss)"

reghdfe `out' disp_* $cand_cont dp_* p_* std_dons_firms, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store trend_`j'
}

esttab trend_1 trend_2 trend_3 using "$appendix/TableE18c.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 


********************************************************************************
* Table E.18d: Interacting a post-ban indicator with 1993 controls and contributions

estimates clear

local j=0

foreach out in ratio_local1_std sh_local1 sh_nat1{

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

gen post=year==1997
foreach x of varlist *_i_93 *_c_93 change_limit_cst elec_margin_c_88 secround_circo_c_88 dons_indiv_93 personal_contrib_93 party_contrib_93{ //covariates included in the analysis of determinants + contributions
gen d_`x'=`x'==.
replace `x'=-1 if `x'==.
gen p_`x'=post*`x'
gen dp_`x'=post*d_`x'
}

replace std_dons_firms=-std_dons_firms
label var std_dons_firms "Firm donations (loss)"

reghdfe `out' disp_* $cand_cont dp_* p_* std_dons_firms, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store trend_contrib_`j'
}

esttab trend_contrib_1 trend_contrib_2 trend_contrib_3 using "$appendix/TableE18d.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 


********************************************************************************
* Table E.18e : Interacting a post-ban indicator with 1993 controls and 1988 contributions

estimates clear

local j=0

foreach out in ratio_local1_std sh_local1 sh_nat1{

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

gen post=year==1997
foreach x of varlist *_i_93 *_c_93 change_limit_cst elec_margin_c_88 secround_circo_c_88 dons_indiv_88 personal_contrib_88 party_contrib_88{ //covariates included in the analysis of determinants
gen d_`x'=`x'==.
replace `x'=-1 if `x'==.
gen p_`x'=post*`x'
gen dp_`x'=post*d_`x'
}

replace std_dons_firms=-std_dons_firms
label var std_dons_firms "Firm donations (loss)"

reghdfe `out' disp_* $cand_cont dp_* p_* std_dons_firms, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store trend_`j'
}

esttab trend_1 trend_2 trend_3 using "$appendix/TableE18e.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 


********************************************************************************
* E18f) Excluding time-varying individual controls

estimates clear
local j=0

foreach out in ratio_local1_std sh_local1 sh_nat1{ 

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2
drop temp

replace std_dons_firms=-std_dons_firms
label var std_dons_firms "Firm donations (loss)"

reghdfe `out' inter_pred_dons_firms std_dons_firms, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store indiv_`j'
}

esttab indiv_1 indiv_2 indiv_3 using "$appendix/TableE18f.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 


********************************************************************************
* Table E.19: Robust impact on local vs. national campaigning, Nearest-neighbor matching estimation

use "$temp/analysis", clear

keep if ratio_local1_std!=. & sample_rest==1 & sample_did==1

capture drop year2
gen year2=1
replace year2=2 if year==1997
tsset id_indiv year2
foreach var of varlist ratio_local1_std sh_local1 sh_nat1{
gen delta_`var'=`var'-L.`var'
byso id_indiv: egen temp=min(delta_`var')
replace delta_`var'=temp
drop temp
}
duplicates tag id_indiv, gen(t)
tab t
keep if t==1
bysort id_indiv: egen private_donation_firms_93 = total(cond(year == 1993, private_donation_firms_cst, .))
gen Dcorporate_donations=(private_donation_firms_93>0)
label var Dcorporate_donations "Firm donations"

keep if year==1993

assert party!=""
gen party2=party
replace party2="ind" if party=="other"
replace party2="other" if party!="FN" & party!="UMP" & party!="PS" & party!="PC" & party!="Verts" & party2!="ind" //for the matching erxercise we try to match candidates from small parties together, instead of pulling them with independents like we do for the determinants and heterogeneity analysis

encode party2, gen(p)

foreach x of varlist *_i_93 *_c_93 change_limit_cst elec_margin_c_88 secround_circo_c_88{
replace `x'=-1 if `x'==.
}

********************************************************************************	
quietly do "$dofiles/_labels.do"
********************************************************************************

eststo clear
foreach var of varlist ratio_local1_std sh_local1 sh_nat1{
eststo: teffects nnmatch (delta_`var' *_i_93) (Dcorporate_donations), ematch(p) vce(robust) biasadj(*_i_93) metric(euclidean)
estadd local MatchCand "\checkmark", replace
estadd local MatchDistrict "", replace
eststo: teffects nnmatch (delta_`var'  *_i_93 *_c_93 change_limit_cst elec_margin_c_88 secround_circo_c_88) (Dcorporate_donations), ematch(p) vce(robust) biasadj( *_i_93 *_c_93 change_limit_cst elec_margin_c_88 secround_circo_c_88) metric(euclidean)
estadd local MatchCand "\checkmark", replace
estadd local MatchDistrict "\checkmark", replace
}

label var Dcorporate_donations "Any Firm donation in 1993"

esttab using "$appendix/TableE19.tex", replace label ///
cells(b(star fmt(%15.3fc)) se(par fmt(%15.3fc))) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(MatchCand MatchDistrict N, fmt(0 0 %15.0fc) labels("Match on candidate characteristics" "Match on district characteristics" "Observations")) ///
collabels(none) mlabels(none) ///
mgroups("Local index" "Local references" "National references", pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))

//NB: this table needs to be edited manually to remove the "r1vsr0" before "Any Firm donation in 1993" and remove the "ATE" line above


********************************************************************************
* Table E.20: Robust impact on local vs. national campaigning, Within-party approach

estimates clear

local j=0

foreach out in ratio_local1_std sh_local1 sh_nat1{

local j=`j'+1

use "$temp/analysis", clear

keep if bigparty!="other" & bigparty!=""

keep if `out'!=. & sample_rest==1 & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_partycirc: egen temp=mean(year)
ta temp
keep if temp!=1993 & temp!=1997 
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

replace std_dons_firms=-std_dons_firms
label var std_dons_firms "Firm donations (loss)"

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms_p std_dons_firms, absorb(id_partycirc id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store party_`j'
}

esttab party_1 party_2 party_3 using "$appendix/TableE20.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 


********************************************************************************
* Table E.21: Impact on partisan leaning, Additional results

estimates clear

local j=0

foreach out in score_noloc_sr1 abs_score_noloc_sr1 abs_loading1{ 

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

replace std_dons_firms=-std_dons_firms
label var std_dons_firms "Firm donations (loss)"

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms std_dons_firms, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store indiv_`j'
}

local i=0

foreach out in score_noloc_sr1 abs_score_noloc_sr1 abs_loading1{ 
local i=`i'+1

use "$temp/analysis", clear
keep if `out'!=. & sample_rest==1 & sample_did==1

foreach p in "FN" "UMP" "PS" "PC" "Verts" "other"{
gen std_dons_firms_`p'=std_dons_firms*(bigparty=="`p'")
label var std_dons_firms_`p' "`p'*Firm don."
}

label var std_dons_firms_FN "Far-right*Firm don."
label var std_dons_firms_UMP "Right*Firm don."
label var std_dons_firms_PS "Socialist*Firm don."
label var std_dons_firms_PC "Communist*Firm don."
label var std_dons_firms_Verts "Green*Firm don."
label var std_dons_firms_other "Other*Firm don."

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

//loss in revenue
foreach p in "FN" "UMP" "PS" "PC" "Verts" "other"{
replace std_dons_firms_`p'=-std_dons_firms_`p'
}


********************************************************************************	
quietly : do "$dofiles/_labels.do"
********************************************************************************

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms std_dons_firms_PC std_dons_firms_Verts std_dons_firms_PS std_dons_firms_UMP std_dons_firms_FN std_dons_firms_other, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store m_`i'

label var std_dons_firms "Firm donations (loss)"
}

esttab indiv_1 m_1 indiv_2 m_2 indiv_3 m_3 using "$appendix/TableE21.tex", ///
replace keep(std_dons_firms std_dons_firms_*) b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Left-right\\score}" "\shortstack{Extremeness}" "\shortstack{Mean word\\extremeness}", pattern(1 0 1 0 1 0)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome" "r2_within R2-Within")


********************************************************************************
* Table E.22: Impact on total revenue and other contributions by party

estimates clear

local i=0

foreach out in revenues_main dons_indiv party_contrib personal_contrib{
local i=`i'+1

use "$temp/analysis", clear
keep if `out'!=. & sample_rest==1 & sample_did==1

foreach p in "FN" "UMP" "PS" "PC" "Verts" "other"{
gen dons_firms_`p'=dons_firms*(bigparty=="`p'")
}


foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

foreach p in "FN" "UMP" "PS" "PC" "Verts" "other"{
replace dons_firms_`p'=-dons_firms_`p'
}

********************************************************************************	
quietly : do "$dofiles/_labels.do"
********************************************************************************

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms dons_firms_PC dons_firms_Verts dons_firms_PS dons_firms_UMP dons_firms_FN dons_firms_other, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store m_`i'
}

esttab m_* using "$appendix/TableE22.tex", ///
replace keep(dons_firms_*) b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Total\\revenue}" "\shortstack{Donations\\from individuals}" "\shortstack{Party\\contributions}" "\shortstack{Personal\\contributions}", pattern(1 1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome" "r2_within R2-Within")


********************************************************************************
* Table E.23: Impact of firm donations on broad policy topics by party type

estimates clear

local j=0

foreach out in economy2_prob1 social2_prob1 homeland2_prob1 foreign2_prob1{
local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

gen std_dons_firms_main=std_dons_firms*mainparty
label var std_dons_firms_main "Mainstream*Firm don."
gen std_dons_firms_niche=std_dons_firms*nicheparty
label var std_dons_firms_niche "Non-mainstream*Firm don."
gen std_dons_firms_other=std_dons_firms*otherparty
label var std_dons_firms_other "Other*Firm don."
				
foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

foreach p in "main" "niche" "other"{
replace std_dons_firms_`p'=-std_dons_firms_`p'
}

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms std_dons_firms_main std_dons_firms_niche std_dons_firms_other, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store topic_`j'
}

esttab topic_* using "$appendix/TableE23.tex", ///
replace keep(std_dons_firms_*)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Economic\\policy}" "\shortstack{Social\\policy}" "\shortstack{Homeland and\\administration}" "\shortstack{Foreign\\policy}", pattern(1 1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 


********************************************************************************
* Table E.24: Impact on manifesto quality

estimates clear
local j=0

foreach out of varlist text_size_sr1 sh_self1 concen_topic{

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

replace std_dons_firms=-std_dons_firms
label var std_dons_firms "Firm donations (loss)"

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms std_dons_firms, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store other_`j'
}

esttab other_* using "$appendix/TableE24.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Manifesto\\length}" "\shortstack{Personal\\references}" "\shortstack{Topic\\concentration}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome" "r2_within R2-Within") 


********************************************************************************
* Table E.25: Impact on local vs. national campaigning by contribution sources

estimates clear 

local j=0

foreach out in ratio_local1_std sh_local1 sh_nat1{

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

local ind = "std_dons_firms"
foreach p in `ind'{
replace `p'=-`p'
}

label var std_dons_firms "Firm donations (loss)"

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms std_dons_firms std_dons_indiv std_personal_contrib std_party_contrib, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store source_`j'
}

esttab  source_1 source_2 source_3 using "$appendix/TableE25.tex", ///
replace keep(std_dons_firms std_dons_indiv std_personal_contrib std_party_contrib) b(3) se lab nomtitles ///
style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("Local index" "Local frequency" "National frequency", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars("ymean Mean outcome before ban" "r2_within R2-Within")


********************************************************************************
*Table E.26: Impact on local vs. campaigning, Share of firm donations in total revenue 
local j=0

foreach out in ratio_local1_std sh_local1 sh_nat1{

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

replace sh_dons_firms_main=-sh_dons_firms_main
label var sh_dons_firms_main "Share of Firm don. (loss)"

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms sh_dons_firms_main, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store sh_`j'

}

esttab sh_1 sh_2 sh_3 using "$appendix/TableE26.tex", ///
replace keep(sh_dons_firms_main) b(3) se lab nomtitles ///
style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("Local index" "Local frequency" "National frequency", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars("ymean Mean outcome before ban" "r2_within R2-Within")


********************************************************************************
* Table E.27: Heterogeneity by type of contributing firms

est clear
local i=0

foreach out of varlist ratio_local1_std sh_local1 sh_nat1{
local i=`i'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

local ind1 = "std_dons_local std_dons_multiple"
local ind2 = "std_dons_local std_dons_multicirc std_dons_onecirc"
local ind3 = "std_dons_local std_dons_oneparty std_dons_multiparty"

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

foreach p in `ind1' `ind2' `ind3'{
replace `p'=-`p'
}

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms `ind1', absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store donor_`i'

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms `ind2', absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store donorcirc_`i'

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms `ind3', absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store donorparty_`i'
}


esttab  donor_1  donorcirc_1 donorparty_1 donor_2  donorcirc_2 donorparty_2 donor_3  donorcirc_3 donorparty_3 using "$appendix/TableE27.tex", ///
replace keep(`ind1'  `ind2' `ind3') b(3) se lab nomtitles ///
style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("{Local index}"  "{Frequency of local references}" "{Frequency of national references}" , pattern(1 0 0 1 0 0 1 0 0)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars("ymean Mean outcome before ban" "r2_within R2-Within")


********************************************************************************
* Table E.28: Heterogeneity by candidate type

estimates clear

local i=0

foreach out in ratio_local1_std sh_local1 sh_nat1{ 
local i=`i'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

* controls and interactions
foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

foreach x in female_i_93 rerun_i_93 Dincumbent_i_93 mayor_i_93 other_mandate_i_93{
gen I_`x'=`x'*std_dons_firms
replace I_`x'=-1*std_dons_firms if `x'==.
gen dI_`x'=(`x'==.)*std_dons_firms
}

********************************************************************************	
quietly : do "$dofiles/_labels.do"
******************************************************************************** 

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2
drop temp


replace std_dons_firms=-std_dons_firms
label var std_dons_firms "Corportate donations (loss)"

foreach x in female_i_93 rerun_i_93 Dincumbent_i_93 mayor_i_93 other_mandate_i_93{
replace I_`x'=-I_`x'
}

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms dI_* std_dons_firms I_*, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store cand_`i'
}

esttab cand_*  using "$appendix/TableE28.tex", ///
replace keep(std_dons_firms I_*) b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}" "\shortstack{Left-right\\score}" "\shortstack{Extremeness}", pattern(1 1 1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within")


********************************************************************************
* Table E.29: Heterogeneity by donor's sector of activity

est clear
local i=0

foreach out in ratio_local1_std sh_local1 sh_nat1{

local i=`i'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

local ind1 = "std_dons_other_sector std_dons_construction std_dons_economy std_dons_environment std_dons_industry std_dons_pme std_dons_unknown"

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

foreach p in `ind1'{
replace `p'=-`p'
}

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms `ind1', absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store donorsect_`i'
}


esttab donorsect_*  using "$appendix/TableE29.tex", ///
replace keep(`ind1') b(3) se  lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome" "r2_within R2-Within")


********************************************************************************
* Table E.30: Heterogeneous effect on policy topics by donor's sector of activity

est clear
local i=0

foreach out in economy2_prob1 social2_prob1 homeland2_prob1 foreign2_prob1{

local i=`i'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

local ind1 = "std_dons_other_sector std_dons_construction std_dons_economy std_dons_environment std_dons_industry std_dons_pme std_dons_unknown"

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

foreach p in `ind1'{
replace `p'=-`p'
}


reghdfe `out' disp_* $cand_cont inter_pred_dons_firms `ind1', absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store donorsect2_`i'
}

esttab donorsect2_*  using "$appendix/TableE30.tex", ///
replace keep(`ind1') b(3) se  lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Economic\\policy}" "\shortstack{Social\\policy}" "\shortstack{Homeland and\\administration}" "\shortstack{Foreign\\policy}", pattern(1 1 1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome" "r2_within R2-Within")


********************************************************************************
* Table E.31: Impact of firm donations on local vs. national campaigning, Sub-sample of elected representatives

local j=0

foreach out in ratio_local1_std sh_local1 sh_nat1 score_sr1 abs_score_sr1{

local j=`j'+1

use "$temp/analysis", clear

keep if winner==1

keep if `out'!=. & sample_rest==1 & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

replace std_dons_firms=-std_dons_firms
label var std_dons_firms "Firm donations (loss)"

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms std_dons_firms, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store elec_`j'
}

esttab elec_* using "$appendix/TableE31.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}" "\shortstack{Left-right\\score}" "\shortstack{Extremeness}", pattern(1 1 1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars("ymean Mean outcome before ban" "r2_within R2-Within") 


********************************************************************************
* Table E.32: Impact of firm donations on interventions, Low- and high-visibility debates

est clear 

estimates clear
local j=0

foreach k in 0 1 {
foreach out in nb_deb_high`k'_total  deb_high`k'_ratio_local_std  deb_high`k'_sh_local  deb_high`k'_sh_nat{

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. &  sample_rest==1 & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

replace std_dons_firms=-std_dons_firms
label var std_dons_firms "Firm donations (loss)"

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms std_dons_firms, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store m`k'_`j'
}
}

*** Table 32a)
esttab m0_* using "$appendix/TableE32a.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab  nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Number\\of interventions}" "\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1  1  1  1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome" "r2_within R2-Within")

*** Table 32b)
esttab m1_*  using "$appendix/TableE32b.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles  ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Number\\of interventions}" "\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1  1  1  1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome" "r2_within R2-Within") 


********************************************************************************
* Table E.33: Impact of firm donations on broad policy topics in legislative discourse

*written questions
estimates clear
local j=0

foreach out in qu_economy2_prob qu_social2_prob qu_homeland2_prob qu_foreign2_prob{

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

replace std_dons_firms=-std_dons_firms
label var std_dons_firms "Firm donations (loss)"

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms std_dons_firms, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store m1_`j'
}

*debates
local j=0

foreach out in deb_economy2_prob deb_social2_prob deb_homeland2_prob deb_foreign2_prob{

local j=`j'+1

use "$temp/analysis", clear

keep if `out'!=. & sample_rest==1 & sample_did==1

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp


replace std_dons_firms=-std_dons_firms
label var std_dons_firms "Firm donations (loss)"

reghdfe `out'  disp_* $cand_cont inter_pred_dons_firms std_dons_firms, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store m2_`j'
}

*** Table E33a) 
esttab m1_* using "$appendix/TableE33a.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Economic\\policy}" "\shortstack{Social\\policy}" "\shortstack{Homeland and\\administration}" "\shortstack{Foreign\\policy}", pattern(1 1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome" "r2_within R2-Within") 

*** Table E33b)
esttab m2_*  using "$appendix/TableE33b.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Economic\\policy}" "\shortstack{Social\\policy}" "\shortstack{Homeland and\\administration}" "\shortstack{Foreign\\policy}", pattern(1 1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome" "r2_within R2-Within") 



********************************************************************************
*                                    Figures                                   *
********************************************************************************

********************************************************************************
* Figure D.11: District-level determinants of firm donations in 1993
use "$temp/analysis", clear

keep if sample_rest==1 & year==1993

replace secround_circo_c_88=1-secround_circo_c_88 

foreach x in $urbancontrol $dads $cc $census90  $census_old limit_cst change_limit_cst inscrits1 elec_margin_c_88{
egen temp=sd(`x') 
egen temp2=min(temp)
gen std_`x'=`x'/temp2
sum std_`x'
drop temp*
}

global std_urbancontrol=	"std_cheflieudep std_nb_villes"
global std_dads = 			"std_DADS_nbestab std_DADS_sumwage std_DADS_share_top1  std_DADS_nbworker"
global std_census90=		"std_pasdedip1990  std_sup1990 std_agri1990 std_ouvr1990  std_pop_65_plus1990  std_pop_15_241990"
global std_cc=				"std_CC_charges_fonctio"
global std_elec=			"std_limit_cst std_change_limit_cst std_inscrits1  std_elec_margin_c_88 secround_circo_c_88"

replace bigparty="Far-right" if bigparty=="FN"
replace bigparty="Right" if bigparty=="UMP"
replace bigparty="Socialist" if bigparty=="PS"
replace bigparty="Communist" if bigparty=="PC"
replace bigparty="Green" if bigparty=="Verts"

encode bigparty, gen(bigparty_n)

foreach v of varlist $std_dads $std_cc{
replace `v'=`v'*10 //rescale variabes for which the CI are very large so they can fit on the figure
}

global depvar = "nb_don dons_firms"

********************************************************************************	
quietly : do "$dofiles/_labels.do"
********************************************************************************
est clear
foreach var in $depvar {
eststo : reg `var' female rerun  Dincumbent  mayor other_mandate ib6.bigparty_n  $std_urbancontrol $std_dads $std_cc $std_census90 $std_elec, cluster(codegeo)
estadd ysumm
est store p_`var'
}


*** # donations: Figure D11a)		
coefplot p_nb_don, keep($std_urbancontrol $std_dads $std_cc $std_census90 $std_elec)  xline(0, lcolor(cranberry)) ///
order ($std_urbancontrol $std_dads $std_cc $std_census90 $std_elec) ///
xtitle("Number of firm donations") ///
headings(std_cheflieudep="{bf:Urban controls}" std_DADS_nbestab1993="{bf:Economic activity}" std_pasdedip1990="{bf:Educational level}" std_agri1990 ="{bf:Occupational structure}" std_std_pop_65_plus1990 ="{bf:Age structure}" std_limit_cst ="{bf:Electoral controls}") ///
mlabsize(medium) msymbol(d) ///
graphregion(color(white)) xlabel(-4 (2) 4)

graph export "$appendix/FigureD11a.pdf", replace

*** Amount donations: Figure D11b)		
coefplot p_dons_firms, keep($std_urbancontrol $std_dads $std_cc $std_census90 $std_elec)  xline(0, lcolor(cranberry)) ///
headings(std_cheflieudep="{bf:Urban controls}" std_DADS_nbestab1993="{bf:Economic activity}" std_pasdedip1990="{bf:Educational level}" std_agri1990 ="{bf:Occupational structure}" std_pop_65_plus1990 ="{bf:Age structure}" std_limit_cst ="{bf:Electoral controls}") /// 
xtitle("Amount of firm donations (euro/voter)") /// 
mlabsize(medium) msymbol(d) ///
graphregion(color(white)) xlabel(-.2 (.1) .2)

graph export "$appendix/FigureD11b.pdf", replace


********************************************************************************
*Figure D.12: Firm donations and trends in local index before 1988

est clear
foreach var in $depvar {
eststo : reg `var' female rerun  Dincumbent  mayor other_mandate ib4.bigparty_n  $std_urbancontrol $std_dads $std_cc $std_census90 $std_elec ratio_local1_partydep_8188 ratio_local1_partydep_7881 ratio_local1_partydep_7378 ratio_local1_partydep_6873 ratio_local1_partydep_6768, cluster(codegeo)
estadd ysumm
est store p_`var'
}

label var ratio_local1_partydep_8188 "1981-1988"
label var ratio_local1_partydep_7881 "1978-1981"
label var ratio_local1_partydep_7378 "1973-1978"
label var ratio_local1_partydep_6873 "1968-1973"
label var ratio_local1_partydep_6768 "1967-1968"

coefplot p_dons_firms, keep(female rerun  Dincumbent  mayor other_mandate 1.bigparty_n 5.bigparty_n ratio_local1*)  xline(0, lcolor(cranberry)) ///
order (female rerun  Dincumbent  mayor other_mandate 1.bigparty_n 5.bigparty_n ratio_local1*) ///
xtitle("Amount of firm donations (euro/voter)") ///
headings(female="{bf:Candidate and Party}" ratio_local1_partydep_8188="{bf:Pre-trends Local Index}") ///
mlabsize(medium) msymbol(d) ///
graphregion(color(white))

graph export "$appendix/FigureD12.pdf", replace


********************************************************************************
*Figure D.13: Impact of firm donations on policy topics in the manifestos

estimates clear

local j=0
foreach sector in agriculture construction culture defense economy education employment environment europe foreign health industry interior justice pme public sport{

local j=`j'+1

use "$temp/analysis", clear

keep if `sector'_prob1!=. & sample_rest==1 & sample_did==1

byso year: egen temp=mean(`sector'_prob1)
byso year: egen temp2=mean(`sector'_prob1)
gen std_`sector'_prob1=(`sector'_prob1-temp)/temp2 //standardize outcomes
drop temp*

foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2 
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

gen v`sector'=-std_dons_firms

reghdfe std_`sector'_prob1 disp_* $cand_cont inter_pred_dons_firms v`sector', absorb(id_indiv id_yearparty) cluster(id_circo)
sum `sector'_prob1 if year==1993
estadd scalar ymean=r(mean)
estadd local party "\checkmark"
estadd local circyear ""
estadd local partyyear "\checkmark"
estadd local cont "\checkmark"
estadd local time "93-97"
estimate store sect_`j'
}

coefplot	///
(sect_13 ,  keep( vinterior ) msymbol(o) mcolor(navy) lcolor(navy) ciopts(lcolor(navy)) offset(.08)) ///
(sect_2  , 	keep( vconstruction ) msymbol(o) mcolor(navy) lcolor(navy) ciopts(lcolor(navy))) ///
(sect_5  , 	keep( veconomy ) msymbol(o) mcolor(navy) lcolor(navy) ciopts(lcolor(navy))) ///
(sect_8  , 	keep( venvironment ) msymbol(o) mcolor(navy) lcolor(navy) ciopts(lcolor(navy))) ///
(sect_6  , 	keep( veducation ) msymbol(o) mcolor(navy) lcolor(navy) ciopts(lcolor(navy))) ///
(sect_1  ,  keep( vagriculture) msymbol(o) mcolor(navy) lcolor(navy) ciopts(lcolor(navy)) ) ///
(sect_3  , 	keep( vculture ) msymbol(o) mcolor(navy) lcolor(navy) ciopts(lcolor(navy))) ///
(sect_16 , 	keep( vpublic ) msymbol(o) mcolor(navy) lcolor(navy) ciopts(lcolor(navy))) ///
(sect_15 , 	keep( vpme) msymbol(o) mcolor(navy) lcolor(navy) ciopts(lcolor(navy))) ///
(sect_9  , 	keep( veurope ) msymbol(o) mcolor(navy) lcolor(navy) ciopts(lcolor(navy))) ///
(sect_11 , 	keep( vhealth ) msymbol(o) mcolor(navy) lcolor(navy) ciopts(lcolor(navy))) ///
(sect_17 , 	keep(  vsport ) msymbol(o) mcolor(navy) lcolor(navy) ciopts(lcolor(navy))) ///
(sect_14 , 	keep( vjustice ) msymbol(o) mcolor(navy) lcolor(navy) ciopts(lcolor(navy))) ///
(sect_12 , 	keep( vindustry ) msymbol(o) mcolor(navy) lcolor(navy) ciopts(lcolor(navy))) ///
(sect_4  , 	keep( vdefense ) msymbol(o) mcolor(navy) lcolor(navy) ciopts(lcolor(navy)) ) ///
(sect_10 , 	keep( vforeign ) msymbol(o) mcolor(navy) lcolor(navy) ciopts(lcolor(navy))) ///
(sect_7  , 	keep( vemployment ) msymbol(o) mcolor(navy) lcolor(navy) ciopts(lcolor(navy))) ///
	 ,  grid(b)  ///
xline(0, lcolor(red)) ///
order(. vconstruction vpme vpublic vinterior vculture vhealth vagriculture  venvironment   vindustry veconomy  veurope vdefense vsport veducation  ///
vforeign vemployment vjustice  . ) ///
coeflabels( vagriculture="Agriculture" vconstruction="Construction and amenities" vculture="Culture" ///
vdefense="Military and defense" veducation="Education" veconomy="Economy" vemployment="Employment" ///
venvironment="Environment" veurope="European policy" vforeign="Foreign policy" vhealth="Health" ///
vindustry="Industry" vinterior="Homeland security" vjustice="Justice" ///
vpme="Retail" vpublic="Public administration" vsport="Sport and entertainment") ///
legend(off) graphregion(color(white)) ///
xtitle("Effect on standardized topic prevalence")

graph export "$appendix/FigureD13.pdf", replace
