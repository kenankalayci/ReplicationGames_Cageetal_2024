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

estimates clear
local j=0

foreach out in ratio_local1_std sh_local1 sh_nat1 { 

local j=`j'+1

use "$temp/analysis", clear
* remove previously generated variables from earlier loop iterations
*capture drop disp_*

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

esttab indiv_1 indiv_2 indiv_3 using "$main/Table3p.tex", ///
replace keep(std_dons_firms)  ///
cells("b(fmt(3)) se(fmt(3) par) p(fmt(3) par)") ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 


********************************************************************************
* Table 4: Impact of firm donations on partisan leaning

estimates clear

local j=0

foreach out in score_sr1 abs_score_sr1 index_originality sh_nat_party1{ 

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

esttab indiv_1 indiv_2 indiv_3 indiv_4 using "$main/Table4p.tex", ///
replace keep(std_dons_firms)  ///
cells("b(fmt(3)) se(fmt(3) par) p(fmt(3) par)") ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Left-right\\score}" "\shortstack{Extremeness}"  "\shortstack{Originality\\index}"  "\shortstack{National party\\references}", pattern(1 1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 


********************************************************************************
* Table 5: Heterogeneity by party

estimates clear

local i=0

foreach out in ratio_local1_std sh_local1 sh_nat1 score_sr1 abs_score_sr1{ 
local i=`i'+1

use "$temp/analysis", clear
keep if `out'!=. & sample_rest==1 & sample_did==1

foreach p in "FN" "UMP" "PS" "PC" "Verts" "other"{
gen std_dons_firms_`p'=std_dons_firms*(bigparty=="`p'")
}

* control
foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

* fixed effects
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
replace std_dons_firms_`p'=-std_dons_firms_`p'
}

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms std_dons_firms_PC std_dons_firms_Verts std_dons_firms_PS std_dons_firms_UMP std_dons_firms_FN std_dons_firms_other, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store m_`i'
}

foreach out in index_originality sh_nat_party1{ 
local i=`i'+1

use "$temp/analysis", clear
keep if `out'!=. & sample_rest==1 & sample_did==1

assert bigparty!="other"

foreach p in "FN" "UMP" "PS" "PC" "Verts"{
gen std_dons_firms_`p'=std_dons_firms*(bigparty=="`p'")
}

* control
foreach x in $cand_cont{
gen disp_`x'=`x'==.
replace `x'=-1 if `x'==.
}

* fixed effects
byso id_indiv: gen temp=_N
keep if temp>1
drop temp

byso id_yearparty: gen temp=_N
replace id_yearparty=10000+year if temp<2
drop temp

byso id_yearparty: gen temp=_N
keep if temp>=2 
drop temp

foreach p in "FN" "UMP" "PS" "PC" "Verts"{
replace std_dons_firms_`p'=-std_dons_firms_`p'
}

gen std_dons_firms_other=.

********************************************************************************	
quietly : do "$dofiles/_labels.do"
********************************************************************************

reghdfe `out' disp_* $cand_cont inter_pred_dons_firms std_dons_firms_PC std_dons_firms_Verts std_dons_firms_PS std_dons_firms_UMP std_dons_firms_FN, absorb(id_indiv id_yearparty) cluster(id_circo)
sum `out' if year==1993
estadd scalar ymean=r(mean)
estimate store m_`i'
}

esttab m_* using "$main/Table5p.tex", ///
replace keep(std_dons_firms_*) b(3) se lab nomtitles ///
cells("b(fmt(3)) se(fmt(3) par) p(fmt(3) par)") ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}" "\shortstack{Left-right\\score}" "\shortstack{Extremeness}" "\shortstack{Originality\\index}"  "\shortstack{National party\\references}", pattern(1 1 1 1 1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome" "r2_within R2-Within")


********************************************************************************
* Table 6: Impact of firm donations on broad policy topics

estimates clear

local j=0

foreach out in economy2_prob1 social2_prob1 homeland2_prob1 foreign2_prob1{

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
estimate store topic_`j'
}


esttab topic_* using "$main/Table6p.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles ///
cells("b(fmt(3)) se(fmt(3) par) p(fmt(3) par)") ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Economic\\policy}" "\shortstack{Social\\policy}" "\shortstack{Homeland and\\administration}" "\shortstack{Foreign\\policy}", pattern(1 1 1 1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome before ban" "r2_within R2-Within") 


********************************************************************************
* Table 7: Impact of firm donations on legislative activity and discourse
* 7a) Written questions to the government

*> Written questions 

estimates clear
local j=0

foreach out in nb_qu_total qu_ratio_local_std qu_sh_local qu_sh_nat{

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

esttab m1_* using "$main/Table7ap.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles ///
cells("b(fmt(3)) se(fmt(3) par) p(fmt(3) par)") ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Number\\of questions}" "\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1  1  1  1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome" "r2_within R2-Within") 


********************************************************************************
* 7b) Debate interventions

estimates clear
local j=0

foreach out in nb_deb_total deb_ratio_local_std deb_sh_local deb_sh_nat{

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

esttab m1_* using "$main/Table7bp.tex", ///
replace keep(std_dons_firms)  ///
b(3) se lab nomtitles ///
cells("b(fmt(3)) se(fmt(3) par) p(fmt(3) par)") ///
unstack style(tex) lines compress star(* 0.10 ** 0.05 *** 0.01) nonotes ///
mgroups("\shortstack{Number\\of interventions}" "\shortstack{Local\\index}" "\shortstack{Local\\references}" "\shortstack{National\\references}", pattern(1  1  1  1)  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars( "ymean Mean outcome" "r2_within R2-Within") 



********************************************************************************
*                                    Figures                                   *
********************************************************************************

********************************************************************************
* Figure 2: Candidate-level determinants of firm donations in 1993

use "$temp/analysis", clear

keep if sample_rest==1 & year==1993

**standardize the variables 
replace secround_circo_c_88=1-secround_circo_c_88 //change definition of secround_circo to correspond to NO runoff

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

**rename parties as in the paper
replace bigparty="Far-right" if bigparty=="FN"
replace bigparty="Right" if bigparty=="UMP"
replace bigparty="Socialist" if bigparty=="PS"
replace bigparty="Communist" if bigparty=="PC"
replace bigparty="Green" if bigparty=="Verts"

encode bigparty, gen(bigparty_n)

global depvar = "nb_don dons_firms"

est clear
foreach var in $depvar {
eststo : reg `var' female rerun  Dincumbent  mayor other_mandate ib6.bigparty_n  $std_urbancontrol $std_dads $std_cc $std_census90 $std_elec, cluster(codegeo)
estadd ysumm
est store p_`var'
}

*** # donations: Figure 2a) 	
coefplot p_nb_don  , keep( female rerun  Dincumbent  mayor other_mandate 2.bigparty_n 5.bigparty_n 3.bigparty_n 4.bigparty_n 1.bigparty_n)  xline(0, lcolor(cranberry)) ///
order (female rerun  Dincumbent  mayor other_mandate  1.bigparty_n 3.bigparty_n 5.bigparty_n  4.bigparty_n 2.bigparty_n )  ///
xtitle("Number of firm donations") ///
headings(female="{bf:Candidate}"  1.bigparty_n="{bf:Party}" ) ///
mlabsize(medium) msymbol(d) ///
graphregion(color(white))

graph export  "$main/Figure2a.pdf", replace

*** Amount donations: Figure 2b)
coefplot p_dons_firms  , keep(female rerun  Dincumbent  mayor other_mandate 2.bigparty_n 5.bigparty_n 3.bigparty_n 4.bigparty_n 1.bigparty_n)  xline(0, lcolor(cranberry)) ///
order (female rerun  Dincumbent  mayor other_mandate  1.bigparty_n 3.bigparty_n 5.bigparty_n  4.bigparty_n 2.bigparty_n ) ///
xtitle("Amount of firm donations (euro/voter)") ///
headings(female="{bf:Candidate}"  1.bigparty_n="{bf:Party}" ) ///
mlabsize(medium) msymbol(d) ///
graphregion(color(white))

graph export  "$main/Figure2b.pdf", replace


********************************************************************************
* Figure 3: Campaign revenue composition in 1993 and 1997

use "$temp/analysis", clear
keep if sample_rest==1 & sample_did==1

label var dons_firms "Firm donations"

byso id_indiv: gen temp=_N
keep if temp>1
drop temp

gen atl_=(dons_firms>0 & year==1993) 
byso id_indiv (year): replace atl_=atl_[1]

********************************************************************************	
quietly: do "$dofiles/_labels.do"
********************************************************************************

graph bar dons_firms dons_indiv party_contrib personal_contrib ,   ///
over(year, label( angle(0))) ///
over(atl_, label( angle(0 ) labsize(small) )) ///
legend(label( 1 "Firm donations") label(2  "Individual donations") label(3 "Party contributions") label(4 "Personal contributions") size(small) row(2)) ///
bar(1, bcolor(white) lcolor(cranberry)) ///
bar(2, bcolor(gray) lcolor(black)) ///
bar(3, bcolor(dimgray) lcolor(black)) ///
bar(4, bcolor(black) lcolor(black)) ///
ytitle("Cst € per voter") graphregion(color(white)) ///
stack

graph export  "$main/Figure3.pdf", replace
