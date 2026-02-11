* Create dataset : Step 3

********************************************************************************
* 1) Merge electoral data from Bekkouche, Cagé, Dewitte (2022) with donation-level data

use "$raw_data/elections/data_legi_donations_results_controls_1988_2002_R1R2", clear

count if candidat==""
li if candidat==""
assert r(N)==0

gen codedep_num=codedepartement 
replace codedep_num="2.1" if codedepartement=="2a"
replace codedep_num="2.2" if codedepartement=="2b"
destring codedep_num, replace

*** keep the only variables that we will use later on but we can come back easily and add as per necessary
keep  election year codegeo candidat candidat_* cand party sexe total_expenditures total_revenues private_donation_individuals party_contribution kind_contribution other personal_contribution balance apport_personnels_versement apport_personnels_avances ///
dons total_expenditures_cst total_revenues_cst private_donation_individuals_cst party_contribution_cst kind_contribution_cst other_cst other_kind_cst personal_contribution_cst balance_cst private_donation_firms private_donation_firms_cst apport_personnels_versement_cst apport_personnels_avances_cst ///
codedepartement circonscription codecirconscription limit limit_cst

*** corrections of some mistakes in recorded amounts, identified manually
replace total_revenues=total_revenues*10 if cand==9452 & year==1993
replace total_revenues_cst=total_revenues_cst*10 if cand==9452 & year==1993
replace total_revenues=personal_contribution+private_donation_individuals if cand==10869 & year==1993 
replace total_revenues_cst=personal_contribution_cst+private_donation_individuals_cst if cand==10869 & year==1993
 replace personal_contribution_cst=total_revenues_cst if inlist(cand, 17615, 23578, 29975, 29977, 32860) & year==1993
replace personal_contribution=total_revenues if inlist(cand, 17615, 23578, 29975, 29977, 32860) & year==1993
replace personal_contribution_cst=personal_contribution_cst/10 if cand==25868  & year==1997
replace personal_contribution=personal_contribution/10 if cand==25868  & year==1997
replace total_revenues_cst=total_expenditures_cst if inlist(cand, 7160, 10863)  & year==1997
replace total_revenues=total_expenditures if inlist(cand, 7160, 10863)  & year==1997
replace total_revenues_cst=total_expenditures_cst if inlist(cand, 19133)  & year==1993
replace total_revenues=total_expenditures if inlist(cand, 19133)  & year==1993
replace personal_contribution_cst=total_revenues_cst if inlist(cand, 16132)  & year==1997
replace personal_contribution=total_revenues if inlist(cand, 16132)  & year==1997
replace total_revenues_cst=party_contribution_cst+private_donation_individuals_cst+other_cst if inlist(cand, 11153) & year==1993
replace total_revenues=party_contribution+private_donation_individuals+other if inlist(cand, 11153) & year==1993

count if private_donation_firms_cst!=0 & private_donation_firms_cst!=. & year!=1993
assert r(N)==0

tempfile early
save `early', replace
 
*** merge with donations data
keep if year==1993
tempfile cand93
save `cand93', replace

keep cand year codegeo
duplicates drop
tempfile allcand
save `allcand', replace

preserve 
use "$output/donations_with_id_cand_donor.dta", clear

gen unknown=(sector=="unknown")
count if montant==.
li cand donor_name if montant==.

*** corrections of mistakes in original data, identified manually
replace montant=20000/2 if montant==. & cand==27873 
replace montant=16500 if montant==. & cand==27508 
replace montant=7532 if montant==75322
replace montant=10000 if montant==100000
replace montant=. if montant==150000

*** donation amount by donor type
replace donor_multiparty=0 if local_donor==1
gen don_multiparty=montant*donor_multiparty*(1-local_donor)

replace donor_oneparty=0 if local_donor==1
gen don_oneparty=montant*donor_oneparty*(1-local_donor)

gen don_multicirc=montant*donor_multi_circo*(1-local_donor)
gen don_local=montant*local_donor

gen other_sector=sector=="agriculture" | sector=="culture" | sector=="foreign/travel" | sector=="health" | sector=="justice" | sector=="public/political" | sector=="sports/asso"

keep  donor_name montant don_* cand codegeo year sector    ///
other_sector agriculture construction culture economy environment foreign health industry justice pme public sport unknown ///
local_donor  donor_multiparty donor_oneparty donor_multi_circo

tempfile donation
save `donation', replace
restore

merge 1:m cand using `donation'
count if _merge==2
assert r(N)==0
keep if _merge==3
drop _merge

gen don_agriculture=montant if sector=="agriculture"
gen don_construction=montant if sector=="construction"
gen don_culture=montant if sector=="culture"
gen don_economy=montant if sector=="economy/fin"
gen don_environment=montant if sector=="environment-energy"
gen don_foreign=montant if sector=="foreign/travel"
gen don_health=montant if sector=="health"
gen don_industry=montant if sector=="industry"
gen don_justice=montant if sector=="justice"
gen don_pme=montant if sector=="pme/retail"
gen don_public=montant if sector=="public/political"
gen don_sport=montant if sector=="sports/asso"
gen don_unknown=montant if sector=="unknown"

gen don_other_sector=montant if other_sector==1

*** test that all types of donation amounts are defined correctly
egen temp=rsum( don_agriculture don_construction don_culture don_economy don_environment don_foreign don_health don_industry don_justice don_pme don_public don_sport don_unknown)
count if temp!=montant & montant!=.
assert r(N)==0
drop temp

egen temp=rsum( don_other_sector don_construction don_economy don_environment don_industry don_pme don_unknown)
count if temp!=montant & montant!=.
assert r(N)==0
drop temp

byso codegeo cand: egen temp2=min(montant)
count if temp2==.
assert r(N)==0
drop temp2 

*** number and amount of different types of donations per candidate
gen temp=1
collapse (sum) don_* other_sector agriculture construction culture economy environment foreign health industry justice pme public sport unknown  ///
montant_total=montant  nb_don=temp nb_local=local_donor nb_multiparty=donor_multiparty nb_oneparty=donor_oneparty nb_multicirc=donor_multi_circo, by(cand codegeo)

foreach sector in other_sector agriculture construction culture economy environment foreign health industry justice pme public sport unknown{
rename `sector' nb_`sector'
}

tempfile montant_sector
save `montant_sector', replace

merge 1:1 cand codegeo using `cand93'
assert _merge!=1
drop _merge

count if montant_total==0 
assert r(N)==0
li candidat codegeo private_donation_firms if montant_total==. & private_donation_firms>0 & private_donation_firms!=.  //few donations missing

count if montant_total==. & (private_donation_firms==0 | private_donation_firms==.) 
replace montant_total=0 if montant_total==.  & private_donation_firms==0 //set total amount of donations to 0 when it is missing and when firm donations are 0 in initial data

count if (private_donation_firms==0 & montant_total!=. & montant_total>0) | (private_donation_firms==. & montant_total!=. & montant_total>0) 
assert r(N)==0

*** replace types of donation amounts by 0 if missing
foreach  sector in other_sector agriculture construction culture economy environment foreign health industry justice pme public sport unknown{
replace don_`sector'=0 if don_`sector'==. & montant_total!=.
replace nb_`sector'=0 if nb_`sector'==. & montant_total!=.
}

replace nb_don=0 if nb_don==. & montant_total!=.
foreach x in "local" "multiparty" "oneparty" "multicirc"{
replace don_`x'=0 if don_`x'==. & montant_total!=.
replace nb_`x'=0 if nb_`x'==. & montant_total!=.
}
gen don_multiple=montant_total-don_local
gen nb_multiple=nb_don-nb_local
gen don_onecirc=montant_total-don_local-don_multicirc
gen nb_onecirc=nb_don-nb_local-nb_multicirc

gen atlone=montant_total>0 | private_donation_firms>0 if montant_total!=. |  private_donation_firms!=.

gen cand93=cand
codebook cand93

tempfile data93
save `data93', replace

use `early', clear
keep if year==1988 | year==1997
append using `data93'

*** merge with donations from 1988
ren cand id_cand
gen total_private_donation=private_donation_firms + private_donation_individuals

gen total_private_donation_cst=total_private_donation*.2538071 //conversion in constant euros

merge 1:1 year id_cand using "$raw_data/revenues/data_donations_88_done", update
assert _merge==1 | _merge==4
drop _merge

bys id_cand: gen nb_election=_N //number of elections a candidate is present

*** set firm donations to 0 after the ban
replace montant_total=0 if year>1993
replace atlone=0 if year>1993
replace nb_don=0 if year>1993

foreach topic in other_sector agriculture construction culture economy environment foreign health industry justice pme public sport unknown {
replace don_`topic'=0 if year>1993
replace nb_`topic'=0 if year>1993
}

foreach x in "local"  "multiparty" "oneparty" "multiple" "multicirc" "onecirc"{
replace nb_`x'=0 if year>1993
replace don_`x'=0 if year>1993
}

count if (private_donation_firms==. & private_donation_firms_cst!=.) | (private_donation_firms!=. & private_donation_firms_cst==.)
assert r(N)==0

*** conversion in constant euros
gen montant_total_cst=montant_total*.1968454 if year==1993 
replace montant_total_cst=montant_total*.1885449 if year==1997 

foreach var in      don_other_sector don_agriculture don_construction don_culture don_economy don_environment don_foreign don_health don_industry don_justice don_pme don_public don_sport don_unknown ///
don_local don_multiple  don_multiparty don_oneparty don_multicirc don_onecirc{
gen `var'_cst=`var'*.1968454 if year==1993
replace `var'_cst=`var'*.1885449 if year==1997
}

********************************************************************************
* 2) Merge the candidate-level dataset with outcomes

gen dep=substr(codegeo, 1, 2)
gen circ=substr(codegeo, -2, .)

replace dep="2A" if dep=="2a"
replace dep="2B" if dep=="2b"

ren party party_init

merge 1:1 year id_cand using "$output/outcomes"
assert _merge==3
drop _merge

********************************************************************************
* 3) Additional variable cleaning

*** gender
gen female=sexe=="f"
replace sexe=upper(sexe)
li year codegeo candidat if sexe==""
//sexe missing for some candidates

*** extremeness = absolute left-righ score
foreach s in "score_sr"{
gen abs_`s'1=abs(`s'1)
}
gen abs_score_noloc_sr1=abs(score_noloc_sr1)

*** standardize local index
foreach x in "ratio_local1" "ratio_local1_2" "ratio_local1_3" "ratio_local1_4" "ratio_local1_5" "qu_ratio_local" "deb_ratio_local" "deb_high1_ratio_local" "deb_high0_ratio_local" "ratio_local_party1"{
byso year: egen temp=sd(`x')
gen `x'_std=`x'/temp
drop temp
}

** *originality index
byso year: egen temp=sd(mean_originality_nuance)
gen index_originality=mean_originality_nuance/temp
drop temp*

*** parties
count if party=="" & missing==0
assert r(N)==0

gen bigparty=party
replace bigparty="other" if !inlist(party, "PS", "UMP", "FN", "PC", "Verts") & party!=""
count if bigparty=="" & missing==0
assert r(N)==0

**number of candidates per party
count if (bigparty=="" & party!="") | (party=="" & bigparty!="")
assert r(N)==0

egen id_party=group(party)
replace id_party=0 if id_party==. //candidates with missing party info

egen temp=max(id_party)
local max=temp
drop temp
forval i=0/`max'{
byso codegeo year: egen nb_party`i'=total(id_party==`i') if id_party!=.
}
gen mainparty=party=="PS" | party=="UMP" | party=="PC" if party!=""
gen nicheparty=party=="FN" | party=="Verts" if party!=""
gen otherparty=bigparty=="other" if party!=""
assert mainparty+nicheparty+otherparty==1 if party!=""

ren private_donation_individuals_cst private_donation_indiv_cst //original name too long

*** contributions per registered voters
count if inscrits1==.
assert r(N)==0

foreach var in montant_total_cst total_expenditures_cst private_donation_firms_cst private_donation_indiv_cst party_contribution_cst kind_contribution_cst other_kind_cst personal_contribution_cst total_revenues_cst{
gen `var'_i=`var'/inscrits1
}

foreach var in  don_other_sector don_agriculture don_construction don_culture don_economy don_environment don_foreign don_health don_industry don_justice don_pme don_public don_sport don_unknown ///
don_local  don_multiple  don_multiparty don_oneparty don_multicirc don_onecirc{
gen `var'_cst_i=`var'_cst/inscrits1
}
gen nb_dons=nb_don/inscrits1 

*** rename variables that are too long 
ren montant_total_cst_i montant_donations
ren private_donation_firms_cst_i  dons_firms
ren private_donation_indiv_cst_i dons_indiv
ren party_contribution_cst_i party_contrib
ren personal_contribution_cst_i personal_contrib
ren total_revenues_cst_i revenues
ren total_expenditures_cst_i expenditures

foreach var in other_sector agriculture construction culture economy environment foreign health industry justice pme public sport unknown{
ren don_`var'_cst_i dons_`var'
}

foreach var in local   multiparty oneparty multiple multicirc onecirc{
ren don_`var'_cst_i dons_`var' 
}

*** logs (standardized)
foreach var in montant_donations dons_firms dons_indiv party_contrib personal_contrib revenues expenditures{
egen temp=sd(ln(`var'+1))
gen ln_`var'=ln(`var'+1)/temp
drop temp
}

foreach var in other_sector agriculture construction culture economy environment foreign health industry justice pme public sport unknown{
egen temp=sd(ln(dons_`var'+1))
gen ln_dons_`var'=ln(dons_`var'+1)/temp
drop temp
}

foreach var in local  multiparty oneparty multicirc onecirc{
egen temp=sd(ln(dons_`var'+1))
gen ln_dons_`var'=ln(dons_`var'+1)/temp
drop temp
}

*** shares of total revenue
foreach var in dons_firms dons_indiv party_contrib personal_contrib{
gen sh_`var'=100*(`var'/revenues)
sum sh_`var'
count if sh_`var'>100 & sh_`var'!=. & year!=1988 
assert r(N)==0
}

foreach var in other_sector agriculture construction culture economy environment foreign health industry justice pme public sport unknown{
gen sh_dons_`var'=100*(dons_`var'/revenues)
sum sh_dons_`var'
}

foreach var in  multiparty oneparty multicirc onecirc{
gen sh_dons_`var'=100*(dons_`var'/revenues)
sum sh_dons_`var'
}

*** standardized contributions
foreach x in "montant_donations" "dons_firms" "dons_indiv" "party_contrib" "personal_contrib" "revenues"{
egen temp=sd(`x') if year==1993
egen temp2=min(temp)
gen std_`x'=`x'/temp2
sum std_`x'
drop temp*
}

foreach x in other_sector agriculture construction culture economy environment foreign health industry justice pme public sport unknown{
egen temp=sd(dons_`x') if year==1993
egen temp2=min(temp)
gen std_dons_`x'=dons_`x'/temp2
sum std_dons_`x'
drop temp*
}

foreach x in local multiple multiparty oneparty multicirc onecirc {
egen temp=sd(dons_`x') if year==1993
egen temp2=min(temp)
gen std_dons_`x'=dons_`x'/temp2
sum std_dons_`x'
drop temp*
}

*** electoral controls 
gen turnout1=100*(votants1/inscrits1)

byso year codegeo: gen nb_candidates=_N

gen secround=voix2!=. 
byso year codegeo: egen secround_circo=max(secround)
byso year codegeo: egen nb_candidates2=total(secround) if secround_circo==1
replace secround=. if secround_circo==0

count if perc_voix_exp1==. & missing==0
assert r(N)==0
bys codegeo year: egen temp=max(perc_voix_exp1) if perc_voix_exp1!=.
bys codegeo year: egen score_winner1=min(temp)
drop temp
sum score_winner1

byso codegeo year: egen temp=max(perc_voix_exp2) if perc_voix_exp2!=.
bys codegeo year: egen score_winner2=min(temp)
drop temp
sum score_winner2

gen score_winner=score_winner1 if secround_circo==0
replace score_winner=score_winner2 if secround_circo==1

gen winner=(abs(perc_voix_exp1-score_winner1)<0.00001) if secround_circo==0
replace winner=(abs(perc_voix_exp2-score_winner2)<0.00001) if secround_circo==1
bys codegeo year: egen temp=total(winner)
assert temp==1
drop temp*

byso codegeo year (perc_voix_exp1): gen elec_margin=perc_voix_exp1-perc_voix_exp1[_n-1] if winner==1 & secround_circo==0
byso codegeo year (perc_voix_exp2): replace elec_margin=perc_voix_exp2-perc_voix_exp2[_n-1] if winner==1 & secround_circo==1
sum elec_margin
byso codegeo year: egen temp=min(elec_margin)
replace elec_margin=temp
drop temp
count if (!(elec_margin>=0 & elec_margin<=100) & nb_candidates2!=1) | nb_candidates2==1 & elec_margin!=.
assert r(N)==0
label var elec_margin "Election margin"

foreach x in "88" "93" "97"{
	gen temp=year==19`x'
	byso id_cand: egen run_`x'=total(temp)
	drop temp
	assert run_`x'==0 | run_`x'==1
}
gen rerun=run_88==1 if year==1993
replace rerun=run_93==1 if year==1997

byso codegeo party id_cand: egen temp=total(run_97)
gen rerun_select=temp>0
drop temp
ta rerun_select if year==1993

byso codegeo party: egen temp=total(run_97)
gen party_select=temp>0
drop temp
ta party_select if year==1993

tempfile analysis
save `analysis', replace

*** merge with district-level controls 
use "$raw_data/controls/districts_controls", clear

rename *1993_cst *1993
rename *1997_cst *1997

keep codegeo *1993 *1997 *1990 *1982 cheflieudep nb_villes

unab mylist: *1993 *1997
foreach v of local mylist {
egen temp=total(`v')
replace `v'=. if temp==0 //sets variables with all 0 to missing
drop temp
}

unab mylist: *1993
foreach v of local mylist {
	local stubs `"`stubs' `=substr("`v'",1,length("`v'")-4)'"' 
} 
reshape long `stubs', i(codegeo) j(year)

merge 1:m codegeo year using `analysis'
assert _merge==3 if year==1993 | year==1997
drop _merge

*** merge with pre-trends in local index at party*department level
gen dep_trend=dep
replace dep_trend="20" if dep_trend=="2A" | dep_trend=="2B"

merge m:1 dep_trend bigparty using "$raw_data/outcomes/trends_6793_partydep"
assert _merge!=2
drop _merge

gen other_mandate=(depute_europeen+conseiller_departemental+senateur)

*** individual controls at district level
foreach x of varlist female rerun Dincumbent mayor other_mandate {
byso codegeo year: egen c_`x'=mean(`x')
}

*** past outcomes, measured in 1988 
foreach out in "secround_circo" "elec_margin"{
gen temp=`out' if year==1988
byso codegeo: egen `out'_c_88=min(temp)
drop temp
}

foreach out of varlist score_sr1 abs_score_sr1 ratio_local1 sh_local1 sh_nat1 sh_self1 index_originality {
byso codegeo party: egen temp=mean(`out') if year==1988
byso codegeo party: egen `out'_p_88=min(temp)
drop temp
byso codegeo party id_cand: egen temp=mean(`out') if year==1988
byso codegeo party id_cand: egen `out'_i_88=min(temp)
drop temp
}

*** change in spending limit between 1993 and 1997
foreach x in "93" "97"{
gen temp_`x'=limit_cst if year==19`x'
byso codegeo year: egen temp=sd(temp_`x')
assert temp==0 | temp==.
drop temp
byso codegeo: egen limit_cst_`x'=min(temp_`x')
drop temp*
}
gen change_limit_cst=limit_cst_97-limit_cst_93

global census90="pasdedip1990  sup1990 agri1990 ouvr1990  pop_65_plus1990  pop_15_241990"
global census_old="pasdedip1982  sup1982 agri1982 ouvr1982  pop_65_plus1982  pop_15_241982"
global urbancontrol="cheflieudep nb_villes"
global cc="  CC_charges_fonctio CC_produits_fonctio"
global dads = "   DADS_nbestab DADS_sumwage DADS_share_top1  DADS_nbworker"
global cand_cont = "female rerun Dincumbent mayor other_mandate"

*** controls measured in 1993
foreach x in $urbancontrol $dads $cc $census90 inscrits1 limit_cst{
byso codegeo year: egen temp_`x'=mean(`x')
gen temp2=temp_`x' if year==1993
byso codegeo: egen `x'_c_93=min(temp2)
drop temp*
}

foreach x in $cand_cont{
byso codegeo party: egen temp=mean(`x') if year==1993
byso codegeo party: egen `x'_p_93=min(temp)
drop temp
byso codegeo party id_cand: egen temp=mean(`x') if year==1993
byso codegeo party id_cand: egen `x'_i_93=min(temp)
drop temp
}

*** controls measured in 1997
foreach x in $urbancontrol $dads $cc $census90 inscrits1 limit_cst{
byso codegeo year: egen temp_`x'=mean(`x')
gen temp2=temp_`x' if year==1997
byso codegeo: egen `x'_c_97=min(temp2)
drop temp*
}

foreach x in $cand_cont{
byso codegeo party: egen temp=mean(`x') if year==1997
byso codegeo party: egen `x'_p_97=min(temp)
drop temp
byso codegeo party id_cand: egen temp=mean(`x') if year==1997
byso codegeo party id_cand: egen `x'_i_97=min(temp)
drop temp
}

*** controls measured in 1988
local cont= "female Dincumbent"
foreach x in `cont'{
byso codegeo year: egen temp_`x'=mean(`x')
gen temp2=temp_`x' if year==1988
byso codegeo: egen `x'_c_88=min(temp2)
drop temp*
}
//we are missing some info in 1988 so we can't include all controls

local cont= "female Dincumbent"
foreach x in `cont'{
byso codegeo party: egen temp=mean(`x') if year==1988
byso codegeo party: egen `x'_p_88=min(temp)
drop temp
byso codegeo party id_cand: egen temp=mean(`x') if year==1988
byso codegeo party id_cand: egen `x'_i_88=min(temp)
drop temp
}

*** past contributions
local var = "dons_firms dons_indiv personal_contrib party_contrib"
foreach x in `var'{
byso codegeo party id_cand: egen temp=mean(`x') if year==1988
byso codegeo party id_cand: egen `x'_88=min(temp)
drop temp
}

*** change in contributions
local var = "dons_firms dons_indiv personal_contrib party_contrib"
foreach x in `var'{
byso codegeo party id_cand: egen temp=mean(`x') if year==1997
byso codegeo party id_cand: egen `x'_97=min(temp)
drop temp
byso codegeo party id_cand: egen temp=mean(`x') if year==1993
byso codegeo party id_cand: egen `x'_93=min(temp)
drop temp
gen `x'_9397=`x'_97-`x'_93
egen temp=sd(`x'_9397) if year==1993
egen temp2=min(temp)
gen std_`x'_9397=`x'_9397/temp2
sum std_`x'_9397
drop temp*
}

*** sample restrictions
count if std_dons_firms==. & montant_total!=.
assert r(N)==0
count if std_dons_firms!=. & montant_total==. //54 obs

gen sample_rest=std_dons_firms!=.
replace sample_rest=0 if montant_total==. //candidates with both aggregate and detailed amounts of donations defined

foreach var of varlist std_dons_indiv dons_indiv std_personal_contrib personal_contrib std_party_contrib party_contrib{
	replace `var'=0 if `var'==. & dons_firms !=. //set other sources of revenue to zero if only firm donations available
	}
replace revenues=dons_indiv+dons_firms+party_contrib+personal_contrib if revenues==. & dons_firms!=.

foreach x in "dons_firms" "dons_indiv" "party_contrib" "personal_contrib"{
	replace sh_`x'=100*(`x'/revenues) if dons_firms!=. & sh_`x'==.
	}

*** revenue as sum of four main sources
gen revenues_main=dons_indiv+dons_firms+party_contrib+personal_contrib
foreach x in "dons_firms" "dons_indiv" "party_contrib" "personal_contrib"{
	gen sh_`x'_main=100*(`x'/revenues_main)
}

*** quintiles of firm donations
gen dons_firms_0=dons_firms==0 if dons_firms!=.
_pctile dons_firms if year==1993 & dons_firms>0 & dons_firms!=., p(20, 40, 60, 80) 

gen dons_firms_20p=dons_firms<r(r1) if year==1993 & dons_firms>0 & dons_firms!=.
gen dons_firms_40p=dons_firms>=r(r1) & dons_firms<r(r2) if year==1993 & dons_firms>0 & dons_firms!=.
gen dons_firms_60p=dons_firms>=r(r2) & dons_firms<r(r3) if year==1993 & dons_firms>0 & dons_firms!=.
gen dons_firms_80p=dons_firms>=r(r3) & dons_firms<r(r4) if year==1993 & dons_firms>0 & dons_firms!=.
gen dons_firms_100p=dons_firms>=r(r4) if year==1993 & dons_firms>0 & dons_firms!=.
foreach x in "20" "40" "60" "80" "100"{
	replace dons_firms_`x'p=0 if dons_firms==0
	count if year>1988 & ((dons_firms_`x'!=. & dons_firms==.) | (dons_firms_`x'==. & dons_firms!=.))
	assert r(N)==0
}

gen temp=dons_firms_20p+dons_firms_40p+dons_firms_60p+dons_firms_80p+dons_firms_100p+dons_firms_0
assert temp==1 | temp==.
drop temp

*** DiD sample
gen sample_did=(year==1993 | year==1997)

*** final dataset
order id_cand year id_1988 id_1993 id_1997 

********************************************************************************
*** drop intermediate variables not used in the analysis
drop  dep_trend sh_local1_partydep_1967 sh_nat1_partydep_1967 ratio_local1_partydep_1967 sh_local1_partydep_1968 sh_nat1_partydep_1968 ratio_local1_partydep_1968 sh_local1_partydep_1973 sh_nat1_partydep_1973 ratio_local1_partydep_1973 sh_local1_partydep_1978 sh_nat1_partydep_1978 ratio_local1_partydep_1978 sh_local1_partydep_1981 sh_nat1_partydep_1981 ratio_local1_partydep_1981 sh_local1_partydep_1988 sh_nat1_partydep_1988 ratio_local1_partydep_1988 sh_local1_partydep_1993 sh_nat1_partydep_1993 ratio_local1_partydep_1993 sh_local1_partydep_8893 sh_local1_partydep_8188 sh_local1_partydep_7881 sh_local1_partydep_7378 sh_local1_partydep_6873 sh_local1_partydep_6768 sh_nat1_partydep_8893 sh_nat1_partydep_8188 sh_nat1_partydep_7881 sh_nat1_partydep_7378 sh_nat1_partydep_6873 sh_nat1_partydep_6768  turnout1 nb_candidates secround nb_candidates2 score_winner1 score_winner2 kind_contribution_cst_i other_kind_cst_i  perc_15_19 perc_20_24 perc_65_plus perc_diplome_sup perc_sans_diplome perc_ouvriers perc_chom  candidat_lemonde candidat_ministere candidat_funding election party_init sexe  cand93  total_private_donation total_private_donation_cst  ins_l2 vot_l2 exp_l2 voix_l2 ins_l1 vot_l1 exp_l1 voix_l1 voixexp_l1 voixexp_l2 nuance text_size_noloc_sr1 sh_other1 deb_sh_dep deb_sh_com qu_sh_dep qu_sh_com text_size sh_dep1 sh_com1 voix1_lemonde inscrits1_lemonde votants1_lemonde exprimes1_lemonde  perc_15_19 perc_20_24 perc_65_plus perc_diplome_sup perc_sans_diplome perc_ouvriers perc_chom  total_depenses depenses_achats depenses_achatsart39 depenses_services_exterieurs depenses_frais_de_personnel depenses_diffusion depenses_frais_financiers depenses_charges_diverses depenses_frais_postaux depenses_impression_journaux depenses_voyages depenses_listing_mairie depenses_reception depenses_fournitures depenses_electricite depenses_locations depenses_espace_publicitaire depenses_honoraires DADS_with_ceo circ circonscription codedepartement codecirconscription dons other text_size_loading1  inscrits2 perc_abs_ins2 votants2 perc_vot_ins2 exprimes2 voix2 blancsetnuls2 voix2_lemonde inscrits2_lemonde votants2_lemonde exprimes2_lemonde missing ratio_local1_4 ratio_local1_4_std perc_voix_exp2 dons_firms_9397 std_dons_firms_9397  std_dons_indiv_9397  dons_indiv_9397 personal_contrib_9397 std_personal_contrib_9397  party_contrib_9397 std_party_contrib_9397 dons_firms_0


********************************************************************************
*** add labels
do "$dofiles/_labels.do"

*** save dataset
save "$output/data_cand_8897", replace
