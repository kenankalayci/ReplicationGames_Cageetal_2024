* Create dataset : Step 2

********************************************************************************
* 1) Merge electoral data from Bekkouche, Cagé, Dewitte (2022) with text outcomes and results from LePennec (2022), for each election year separately

use "$raw_data/elections/data_legi_donations_results_controls_1988_2002_R1R2", clear

gen codedep_num=codedepartement 
replace codedep_num="2.1" if codedepartement=="2a"
replace codedep_num="2.2" if codedepartement=="2b"
destring codedep_num, replace

gen dep=substr(codegeo, 1, 2)
gen circ=substr(codegeo, -2, .)

replace dep="2A" if dep=="2a"
replace dep="2B" if dep=="2b"

ren cand id_cand

so dep circ candidat
gen idu=_n
save "$temp/tomerge", replace

*_______________________________________________________________________________
*                                        1993
*_______________________________________________________________________________

keep if year==1993
save "$temp/tomerge93", replace

*** Merge with id_1993 from LePennec 2022:
use "$raw_data/elections/l_cand_1993.dta", clear

keep if id_1993!=.

keep id_1993 dep_1993 circ_1993 nom_1993 prenom_1993
rename dep_1993 dep
rename circ_1993 circ
rename nom_1993 nom
rename prenom_1993 prenom

gen candidat=lower(nom + " " + prenom)
replace candidat=subinstr(candidat, "-", " ", .)
replace candidat=subinstr(candidat, "'", " ", .)

so dep circ candidat
gen idm=_n

reclink dep circ candidat using "$temp/tomerge93", idu(idu) idm(idm) req(dep circ) gen(match)
assert _merge==3

li candidat Ucandidat match if match<1 & match>0.99
li dep circ candidat Ucandidat match if match<0.99 & match>0.98
li dep circ candidat Ucandidat match if match<0.98 & match>0.97
li dep circ candidat Ucandidat match if match<0.97 & match>0.96
li dep circ candidat Ucandidat match if match<0.96 & match>0.9
li dep circ candidat Ucandidat match if match<0.9
//everything merges well

keep if match!=.
byso id_1993: gen temp=_N
assert temp==1
drop temp _merge

byso idu: gen temp=_N
assert temp==1
drop temp

drop candidat
rename Ucandidat candidat
drop Udep Ucirc idu idm

save "$temp/temp", replace

*** Check for discrepancies between two original datasets:
use "$temp/tomerge93", clear
merge 1:1 dep circ candidat using "$temp/temp"
li dep circ candidat nom prenom if _merge==1

gen missing=_merge==1
drop _merge
replace id_1993=10000+_n if missing==1

save "$temp/temp93", replace

*** Merge with text outcomes dataset:

*** left-right score 
insheet using "$raw_data/outcomes/df_srproj_scaling_1993.csv", clear
keep if tour==1
egen temp=sd(covarsnuance3)
gen score_sr1=covarsnuance3/temp
drop v1 x tour temp covarsnuance3
rename m text_size_sr1

replace score_sr1=. if text_size_sr1==0 //score should be missing when content empty

rename id id_1993
gen year=1993

merge 1:1 year id_1993 using "$temp/temp93"
assert _merge!=1
drop _merge

save "$temp/temp93", replace

*** absolute word loading 
insheet using "$raw_data/outcomes/df_loadings_1993.csv", clear
keep if tour==1
egen temp=sd(abs_loading)
gen abs_loading1=abs_loading/temp
drop v1 tour temp abs_loading
rename text_size text_size_loading1

replace abs_loading1=. if text_size_loading1==0 //should be missing when content empty

gen year=1993

merge 1:1 year id_1993 using "$temp/temp93"
assert _merge!=1
drop _merge

count if (text_size_loading1!=text_size_sr1)
assert r(N)==0
save "$temp/temp93", replace

*** topic prevalence in written questions
insheet using "$raw_data/outcomes/df_srproj2_logit_questions_1993.csv", clear
keep id_1993 homeland_prob foreign_prob economy_prob social_prob

gen year=1993
foreach x in "homeland" "foreign" "economy" "social"{
gen qu_`x'2_prob=`x'_prob*100
}

keep year id_1993 qu_*

merge 1:1 year id_1993 using "$temp/temp93"
assert _merge!=1
drop _merge
save "$temp/temp93", replace

*** total number of written questions to government
insheet using "$raw_data/outcomes/df_cat_questions_1993.csv", clear

foreach x of varlist  interior education environment pme health economy construction public employment justice agriculture defense foreign industry culture sport europe other{
rename `x' nb_qu_`x'
}
egen nb_qu_total=rsum(nb_qu_*)
gen year=1993

merge 1:1 year id_1993 using "$temp/temp93"
assert _merge!=1
drop _merge
save "$temp/temp93", replace

*** narrow policy topic prevalence
insheet using "$raw_data/outcomes/df_srproj_logit_manif_1993.csv", clear
assert text_size!=0
keep if tour==1

keep id_1993  agriculture_prob construction_prob culture_prob defense_prob economy_prob education_prob employment_prob environment_prob europe_prob foreign_prob health_prob industry_prob interior_prob justice_prob other pme_prob public_prob sport_prob sector_pred tour interior education environment pme health economy construction public employment justice agriculture defense foreign industry culture sport europe 
reshape wide  agriculture_prob construction_prob culture_prob defense_prob economy_prob education_prob employment_prob environment_prob europe_prob foreign_prob health_prob industry_prob interior_prob justice_prob other pme_prob public_prob sport_prob sector_pred interior education environment pme health economy construction public employment justice agriculture defense foreign industry culture sport europe, i( id_1993) j(tour)

foreach var in agriculture_prob construction_prob culture_prob defense_prob economy_prob education_prob employment_prob environment_prob europe_prob foreign_prob health_prob industry_prob interior_prob justice_prob pme_prob public_prob sport_prob{
replace `var'1=`var'1*100
}

gen year=1993
merge 1:1 year id_1993 using "$temp/temp93"
assert _merge!=1
drop _merge
save "$temp/temp93", replace

*** broad topic policy prevalence
insheet using "$raw_data/outcomes/df_srproj2_logit_manif_1993.csv", clear
assert text_size!=0
keep if tour==1

keep id_1993 tour homeland_prob foreign_prob economy_prob social_prob
reshape wide homeland_prob foreign_prob economy_prob social_prob, i( id_1993) j(tour)

gen year=1993
foreach x in "homeland" "foreign" "economy" "social"{
gen `x'2_prob1=`x'_prob1*100
}

keep year id_1993 *2_prob1

merge 1:1 year id_1993 using "$temp/temp93"
assert _merge!=1
drop _merge
save "$temp/temp93", replace

*** topic prevalence in debate interventions
insheet using "$raw_data/outcomes/df_srproj2_logit_debat_1993.csv", clear
drop if text_size==0 //obs unusable because interventions too short to be classified

keep id_1993 homeland_prob foreign_prob economy_prob social_prob

gen year=1993
foreach x in "homeland" "foreign" "economy" "social"{
gen deb_`x'2_prob=`x'_prob*100
}

keep year id_1993 deb_*

merge 1:1 year id_1993 using "$temp/temp93"
assert _merge!=1
drop _merge
save "$temp/temp93", replace

*** total number of debate interventions
insheet using "$raw_data/outcomes/df_cat_debats_1993.csv", clear

foreach x of varlist  interior education environment pme health economy construction public employment justice agriculture defense foreign industry culture sport europe other{
rename `x' nb_deb_`x'
}
egen nb_deb_total=rsum(nb_deb_*)
gen year=1993

merge 1:1 year id_1993 using "$temp/temp93"
assert _merge!=1
drop _merge
save "$temp/temp93", replace

*** total number of debate interventions in high vs. low visibility debates
forval r=0/1{
insheet using "$raw_data/outcomes/df_total_debats_high`r'_1993.csv", clear
gen year=1993

merge 1:1 year id_1993 using "$temp/temp93"
assert _merge!=1
drop _merge

save "$temp/temp93", replace
}

*** frequency of local references
insheet using "$raw_data/outcomes/df_local_1993.csv", clear
keep if tour==1
gen year=1993

gen sh_dep1=100*(nb_dep/text_size)
gen sh_com1=100*(nb_com/text_size)
gen sh_local1=sh_com1+sh_dep1
drop v1 tour dep nuance*

merge 1:1 year id_1993 using "$temp/temp93"
assert _merge!=1
drop _merge
save "$temp/temp93", replace

*** frequency of national references
insheet using "$raw_data/outcomes/df_nat_1993.csv", clear
keep if tour==1
gen year=1993

gen sh_nat1=100*(nb_nat/text_size)
drop v1 tour text_size

merge 1:1 year id_1993 using "$temp/temp93"
assert _merge!=1
drop _merge

gen ratio_local1=log((100+sh_local1)/(100+sh_nat1)) //local index
sum ratio_local1

gen ratio_local1_2=log((100000+sh_local1)/(100000+sh_nat1))
sum ratio_local1_2

gen ratio_local1_3=log((0.1+sh_local1)/(0.1+sh_nat1))
sum ratio_local1_3

gen ratio_local1_4=log((sh_local1)/(sh_nat1))
sum ratio_local1_4

gen ratio_local1_5=(sh_local1)/(sh_nat1+sh_local1)
sum ratio_local1_5

save "$temp/temp93", replace

*** frequency of national party specific references
insheet using "$raw_data/outcomes/df_nat_party_1993.csv", clear
keep if tour==1
gen year=1993

gen sh_nat_party1=100*(nb_nat/text_size)
drop v1 tour text_size nb_nat

merge 1:1 year id_1993 using "$temp/temp93"
assert _merge!=1
drop _merge

gen ratio_local_party1=log((100+sh_local1)/(100+sh_nat_party1)) //local index
sum ratio_local_party1

save "$temp/temp93", replace

*** frequency of local references in written questions
insheet using "$raw_data/outcomes/df_local_qu_1993.csv", clear
gen year=1993

gen qu_sh_dep=100*(nb_dep/text_size)
gen qu_sh_com=100*(nb_com/text_size)
gen qu_sh_local=qu_sh_com+qu_sh_dep
drop v1 text_size nb_dep nb_com

merge 1:1 year id_1993 using "$temp/temp93"
assert _merge!=1
drop _merge
save "$temp/temp93", replace

*** frequency of national references
insheet using "$raw_data/outcomes/df_nat_qu_1993.csv", clear
gen year=1993

gen qu_sh_nat=100*(nb_nat/text_size)
drop v1 text_size nb_nat

merge 1:1 year id_1993 using "$temp/temp93"
assert _merge!=1
drop _merge

gen qu_ratio_local=log((100+qu_sh_local)/(100+qu_sh_nat)) //local index
sum qu_ratio_local

save "$temp/temp93", replace

*** frequency of local references in debate interventions
insheet using "$raw_data/outcomes/df_local_deb_1993.csv", clear
gen year=1993

gen deb_sh_dep=100*(nb_dep/text_size)
gen deb_sh_com=100*(nb_com/text_size)
gen deb_sh_local=deb_sh_com+deb_sh_dep
drop v1 text_size nb_dep nb_com

merge 1:1 year id_1993 using "$temp/temp93"
assert _merge!=1
drop _merge
save "$temp/temp93", replace

*** frequency of national references in debate interventions
insheet using "$raw_data/outcomes/df_nat_deb_1993.csv", clear
gen year=1993

gen deb_sh_nat=100*(nb_nat/text_size)
drop v1 text_size nb_nat

merge 1:1 year id_1993 using "$temp/temp93"
assert _merge!=1
drop _merge

gen deb_ratio_local=log((100+deb_sh_local)/(100+deb_sh_nat)) //local index
sum deb_ratio_local

save "$temp/temp93", replace

*** frequency of local vs. national references in debate interventions with high vs. low visibility
forval r=0/1{
insheet using "$raw_data/outcomes/df_local_deb_high`r'_1993.csv", clear
gen year=1993

gen deb_sh_dep=100*(nb_dep/text_size)
gen deb_sh_com=100*(nb_com/text_size)
gen deb_high`r'_sh_local=deb_sh_com+deb_sh_dep
drop v1 text_size nb_dep nb_com deb_sh_dep deb_sh_com

merge 1:1 year id_1993 using "$temp/temp93"
assert _merge!=1
drop _merge
save "$temp/temp93", replace

insheet using "$raw_data/outcomes/df_nat_deb_high`r'_1993.csv", clear
gen year=1993

gen deb_high`r'_sh_nat=100*(nb_nat/text_size)
drop v1 text_size nb_nat

merge 1:1 year id_1993 using "$temp/temp93"
assert _merge!=1
drop _merge

gen deb_high`r'_ratio_local=log((100+deb_high`r'_sh_local)/(100+deb_high`r'_sh_nat))
sum deb_high`r'_ratio_local
save "$temp/temp93", replace
}

*** frequency of references to first and last names
insheet using "$raw_data/outcomes/df_nom_1993.csv", clear
keep if tour==1
gen year=1993

gen sh_self1=100*(nom_self+prenom_self)/text_size

gen sh_other1=100*(nom_all+prenom_all-nom_self-prenom_self)/text_size
drop v1 tour text_size

merge 1:1 year id_1993 using "$temp/temp93"
assert _merge!=1
drop _merge
save "$temp/temp93", replace

*** originality relative to other manifestos from the same party
insheet using "$raw_data/outcomes/df_sim_nuance_1993.csv", clear
gen year=1993

drop v1

merge 1:1 year id_1993 using "$temp/temp93"
assert _merge!=1
drop _merge
save "$temp/temp93", replace

*** left-right score without local references
insheet using "$raw_data/outcomes/df_srproj_scaling_noloc_1993.csv", clear
keep if tour==1
egen temp=sd(covarsnuance3)
gen score_noloc_sr1=covarsnuance3/temp
drop v1 x tour temp covarsnuance3
rename m text_size_noloc_sr1

replace score_noloc_sr1=. if text_size_noloc_sr1==0 

rename id id_1993
gen year=1993

merge 1:1 year id_1993 using "$temp/temp93"
assert _merge!=1
drop _merge

count if text_size_sr1<text_size_noloc_sr1
assert r(N)==0
count if score_sr1==score_noloc_sr1 & text_size_sr1!=text_size_noloc_sr1
assert r(N)==0
count if (score_sr1==. & score_noloc_sr1!=.) | (score_sr1!=. & score_noloc_sr1==.) 
assert r(N)==0
corr score_noloc_sr1 score_sr1
save "$temp/temp93", replace


* Merges back with electoral results from Le Pennec (2022):
use "$raw_data/elections/l_cand_1993", replace

keep if id_1993!=.
gen year=1993
keep id_1993 year ins_l1_1993 vot_l1_1993 exp_l1_1993 voix_l1_1993 voixexp_l1_1993 ins_l2_1993 vot_l2_1993 exp_l2_1993 voix_l2_1993 voixexp_l2_1993 nuance_1993 nuance2_1993 nuance3_1993 incumb_1993

rename *_1993 *
rename id id_1993
merge 1:1 year id_1993 using "$temp/temp93"
count if _merge!=3 & missing==0
assert r(N)==0

count if id_cand==.
assert r(N)==0

drop _merge

*** compare parties across datasets
count if nuance1!="" & nuance!="" & nuance1!=nuance
count if party==""
li codegeo candidat nuance nuance1 parti_lemonde voix1 missing if party==""
ta nuance1 if party==""
replace party="other" if party=="" & (nuance1!="" | nuance!="")

*** compare vote shares across datasets
count if voix1==.
count if voix1_lemonde==.
byso codegeo: egen temp=total(perc_voix_exp1) 
sum temp
byso codegeo: egen temp2=total(voixexp_l1) 
sum temp2
li codegeo candidat exprimes1 voix1_lemonde voix_l1 temp temp2 missing if voix1==. 
li codegeo candidat exprimes1 voix1 voix_l1 temp temp2 missing if voix1_lemonde==. 
drop temp*

replace voix1=voix_l1 if voix1==. //replace missing number of votes from Bekkouche et al. (2022) with number of votes from Ministry of the Interior

count if voix2==. & voix2_lemonde!=.
li codegeo candidat exprimes2 voix2_lemonde voix_l2 if voix2==. & voix2_lemonde!=. 
count if voix2_lemonde==. & voix2!=.
gen perc_voix_exp2=100*(voix2/exprimes2)
byso codegeo: egen temp=total(perc_voix_exp2) 
sum temp
byso codegeo: egen temp2=total(voixexp_l2) 
sum temp2
li codegeo candidat inscrits2 exprimes2 voix2_lemonde voix_l2 temp temp2 missing if voix2==. & voix_l2!=. //candidate with missing number of votes in BCD's data
drop temp*

replace voix2=voix_l2 if voix2==. //replace missing number of votes from Bekkouche et al. (2022) with number of votes from Ministry of the Interior

li candidat exprimes2 votants2 voix2 exp_l2 vot_l2 voix_l2 if codegeo=="16 02"
replace exprimes2=exp_l2 if codegeo=="16 02"
replace votants2=vot_l2 if codegeo=="16 02"

li candidat exprimes2 votants2 voix2 exp_l2 vot_l2 voix_l2 if codegeo=="16 01"
replace exprimes2=exp_l2 if codegeo=="16 01"
replace votants2=vot_l2 if codegeo=="16 01"

li candidat voix2 exp_l2 ins_l2 if codegeo=="31 04"
replace exprimes2=exp_l2 if codegeo=="31 04"
replace inscrits2=ins_l2 if codegeo=="31 04"
replace votants2=vot_l2 if codegeo=="31 04"

li candidat voix2 exp_l2 ins_l2 if codegeo=="78 03"
replace exprimes2=exp_l2 if codegeo=="78 03"
replace inscrits2=ins_l2 if codegeo=="78 03"
replace votants2=vot_l2 if codegeo=="78 03"

gen codegeo93=codegeo
save "$temp/merge93", replace

*_______________________________________________________________________________
*                                        1997
*_______________________________________________________________________________

use "$temp/tomerge", clear
keep if year==1997
save "$temp/tomerge97", replace


* Merge with id_1997:
use "$raw_data/elections/l_cand_1997", clear
keep if id_1997!=.

keep id_1997 dep_1997 circ_1997 nom_1997 prenom_1997
rename dep_1997 dep
rename circ_1997 circ
rename nom_1997 nom
rename prenom_1997 prenom

gen temp=substr(dep, 1, 1)
drop if temp=="Z"
drop temp //drop overseas departments and territories

gen candidat=lower(nom + " " + prenom)
replace candidat=subinstr(candidat, "-", " ", .)
replace candidat=subinstr(candidat, "'", " ", .)

so dep circ candidat
gen idm=_n

reclink dep circ candidat using "$temp/tomerge97", idu(idu) idm(idm) req(dep circ) gen(match)
assert _merge==3

li candidat Ucandidat match if match<1 & match>0.999
li dep circ candidat Ucandidat match if match<0.999 & match>0.995
li dep circ candidat Ucandidat match if match<0.995 & match>0.99
li dep circ candidat Ucandidat match if match<0.99 & match>0.98
li dep circ candidat Ucandidat match if match<0.98 & match>0.95
li dep circ candidat Ucandidat match if match<0.95 
//everything merges well

keep if match!=.
byso id_1997: gen temp=_N
assert temp==1
drop temp _merge

byso idu: gen temp=_N
assert temp==1
drop temp

drop candidat
rename Ucandidat candidat
drop Udep Ucirc idu idm

save "$temp/temp.dta", replace

* Check for discrepancies between two original datasets:
use "$temp/tomerge97.dta", clear
merge 1:1 dep circ candidat using "$temp/temp"
li dep circ candidat candidat_lemonde candidat_funding if _merge==1

gen missing=_merge==1 //candidates for whom we are missing information because they are unclear matches
drop _merge
replace id_1997=10000+_n if missing==1

save "$temp/temp97", replace


*Merge with text outcomes:
*** left-right score 
insheet using "$raw_data/outcomes/df_srproj_scaling_1997.csv", clear
keep if tour==1
egen temp=sd(covarsnuance3)
gen score_sr1=covarsnuance3/temp
drop v1 x tour temp covarsnuance3
rename m text_size_sr1

replace score_sr1=. if text_size_sr1==0 //score should be missing when content empty

rename id id_1997
gen year=1997

merge 1:1 year id_1997 using "$temp/temp97"
assert _merge!=1
drop _merge

save "$temp/temp97", replace

*** absolute word loading 
insheet using "$raw_data/outcomes/df_loadings_1997.csv", clear
keep if tour==1
egen temp=sd(abs_loading)
gen abs_loading1=abs_loading/temp
drop v1 tour temp abs_loading
rename text_size text_size_loading1

replace abs_loading1=. if text_size_loading1==0 //should be missing when content empty

gen year=1997

merge 1:1 year id_1997 using "$temp/temp97"
assert _merge!=1
drop _merge

count if (text_size_loading1!=text_size_sr1)
assert r(N)==0
save "$temp/temp97", replace

*** topic prevalence in written questions
insheet using "$raw_data/outcomes/df_srproj2_logit_questions_1997.csv", clear
keep id_1997 homeland_prob foreign_prob economy_prob social_prob

gen year=1997
foreach x in "homeland" "foreign" "economy" "social"{
gen qu_`x'2_prob=`x'_prob*100
}

keep year id_1997 qu_*

merge 1:1 year id_1997 using "$temp/temp97"
assert _merge!=1
drop _merge
save "$temp/temp97", replace

*** total number of written questions to government
insheet using "$raw_data/outcomes/df_cat_questions_1997.csv", clear

foreach x of varlist  interior education environment pme health economy construction public employment justice agriculture defense foreign industry culture sport europe other{
rename `x' nb_qu_`x'
}
egen nb_qu_total=rsum(nb_qu_*)
gen year=1997

merge 1:1 year id_1997 using "$temp/temp97"
assert _merge!=1
drop _merge
save "$temp/temp97", replace

*** narrow policy topic prevalence
insheet using "$raw_data/outcomes/df_srproj_logit_manif_1997.csv", clear
drop if text_size==0 //few empty manifestos after text pre-processing with unreliable topic measures
keep if tour==1

keep id_1997  agriculture_prob construction_prob culture_prob defense_prob economy_prob education_prob employment_prob environment_prob europe_prob foreign_prob health_prob industry_prob interior_prob justice_prob other pme_prob public_prob sport_prob sector_pred tour interior education environment pme health economy construction public employment justice agriculture defense foreign industry culture sport europe 
reshape wide  agriculture_prob construction_prob culture_prob defense_prob economy_prob education_prob employment_prob environment_prob europe_prob foreign_prob health_prob industry_prob interior_prob justice_prob other pme_prob public_prob sport_prob sector_pred interior education environment pme health economy construction public employment justice agriculture defense foreign industry culture sport europe, i( id_1997) j(tour)

foreach var in agriculture_prob construction_prob culture_prob defense_prob economy_prob education_prob employment_prob environment_prob europe_prob foreign_prob health_prob industry_prob interior_prob justice_prob pme_prob public_prob sport_prob{
replace `var'1=`var'1*100
}

gen year=1997
merge 1:1 year id_1997 using "$temp/temp97"
assert _merge!=1
drop _merge
save "$temp/temp97", replace

*** broad topic policy prevalence
insheet using "$raw_data/outcomes/df_srproj2_logit_manif_1997.csv", clear
drop if text_size==0 //few empty manifestos after pre-processing with unreliable topic measures
keep if tour==1

keep id_1997 tour homeland_prob foreign_prob economy_prob social_prob
reshape wide homeland_prob foreign_prob economy_prob social_prob, i( id_1997) j(tour)

gen year=1997
foreach x in "homeland" "foreign" "economy" "social"{
gen `x'2_prob1=`x'_prob1*100
}

keep year id_1997 *2_prob1

merge 1:1 year id_1997 using "$temp/temp97"
assert _merge!=1
drop _merge
save "$temp/temp97", replace

*** topic prevalence in debate interventions
insheet using "$raw_data/outcomes/df_srproj2_logit_debat_1997.csv", clear
drop if text_size==0 //observations unusable because interventions too short to be classified

keep id_1997 homeland_prob foreign_prob economy_prob social_prob

gen year=1997
foreach x in "homeland" "foreign" "economy" "social"{
gen deb_`x'2_prob=`x'_prob*100
}

keep year id_1997 deb_*

merge 1:1 year id_1997 using "$temp/temp97"
assert _merge!=1
drop _merge
save "$temp/temp97", replace

*** total number of debate interventions
insheet using "$raw_data/outcomes/df_cat_debats_1997.csv", clear

foreach x of varlist  interior education environment pme health economy construction public employment justice agriculture defense foreign industry culture sport europe other{
rename `x' nb_deb_`x'
}
egen nb_deb_total=rsum(nb_deb_*)
gen year=1997

merge 1:1 year id_1997 using "$temp/temp97"
assert _merge!=1
drop _merge
save "$temp/temp97", replace

*** total number of debate interventions in high vs. low visibility debates
forval r=0/1{
insheet using "$raw_data/outcomes/df_total_debats_high`r'_1997.csv", clear
gen year=1997

merge 1:1 year id_1997 using "$temp/temp97"
assert _merge!=1
drop _merge

save "$temp/temp97", replace
}

*** frequency of local references
insheet using "$raw_data/outcomes/df_local_1997.csv", clear
keep if tour==1
gen year=1997

gen sh_dep1=100*(nb_dep/text_size)
gen sh_com1=100*(nb_com/text_size)
gen sh_local1=sh_com1+sh_dep1
drop v1 tour dep nuance*

merge 1:1 year id_1997 using "$temp/temp97"
assert _merge!=1
drop _merge
save "$temp/temp97", replace

*** frequency of national references
insheet using "$raw_data/outcomes/df_nat_1997.csv", clear
keep if tour==1
gen year=1997

gen sh_nat1=100*(nb_nat/text_size)
drop v1 tour text_size

merge 1:1 year id_1997 using "$temp/temp97"
assert _merge!=1
drop _merge

gen ratio_local1=log((100+sh_local1)/(100+sh_nat1)) //local index
sum ratio_local1

gen ratio_local1_2=log((100000+sh_local1)/(100000+sh_nat1))
sum ratio_local1_2

gen ratio_local1_3=log((0.1+sh_local1)/(0.1+sh_nat1))
sum ratio_local1_3

gen ratio_local1_4=log((sh_local1)/(sh_nat1))
sum ratio_local1_4

gen ratio_local1_5=(sh_local1)/(sh_nat1+sh_local1)
sum ratio_local1_5

save "$temp/temp97", replace

*** frequency of national party specific references
insheet using "$raw_data/outcomes/df_nat_party_1997.csv", clear
keep if tour==1
gen year=1997

gen sh_nat_party1=100*(nb_nat/text_size)
drop v1 tour text_size nb_nat

merge 1:1 year id_1997 using "$temp/temp97"
assert _merge!=1
drop _merge

gen ratio_local_party1=log((100+sh_local1)/(100+sh_nat_party1)) //local index
sum ratio_local_party1

save "$temp/temp97", replace

*** frequency of local references in written questions
insheet using "$raw_data/outcomes/df_local_qu_1997.csv", clear
gen year=1997

gen qu_sh_dep=100*(nb_dep/text_size)
gen qu_sh_com=100*(nb_com/text_size)
gen qu_sh_local=qu_sh_com+qu_sh_dep
drop v1 text_size nb_dep nb_com

merge 1:1 year id_1997 using "$temp/temp97"
assert _merge!=1
drop _merge
save "$temp/temp97", replace

*** frequency of national references
insheet using "$raw_data/outcomes/df_nat_qu_1997.csv", clear
gen year=1997

gen qu_sh_nat=100*(nb_nat/text_size)
drop v1 text_size nb_nat

merge 1:1 year id_1997 using "$temp/temp97"
assert _merge!=1
drop _merge

gen qu_ratio_local=log((100+qu_sh_local)/(100+qu_sh_nat)) //local index
sum qu_ratio_local

save "$temp/temp97", replace

*** frequency of local references in debate interventions
insheet using "$raw_data/outcomes/df_local_deb_1997.csv", clear
gen year=1997

gen deb_sh_dep=100*(nb_dep/text_size)
gen deb_sh_com=100*(nb_com/text_size)
gen deb_sh_local=deb_sh_com+deb_sh_dep
drop v1 text_size nb_dep nb_com

merge 1:1 year id_1997 using "$temp/temp97"
assert _merge!=1
drop _merge
save "$temp/temp97", replace

*** frequency of national references in debate interventions
insheet using "$raw_data/outcomes/df_nat_deb_1997.csv", clear
gen year=1997

gen deb_sh_nat=100*(nb_nat/text_size)
drop v1 text_size nb_nat

merge 1:1 year id_1997 using "$temp/temp97"
assert _merge!=1
drop _merge

gen deb_ratio_local=log((100+deb_sh_local)/(100+deb_sh_nat)) //local index
sum deb_ratio_local

save "$temp/temp97", replace

*** frequency of local vs. national references in debate interventions with high vs. low visibility
forval r=0/1{
insheet using "$raw_data/outcomes/df_local_deb_high`r'_1997.csv", clear
gen year=1997

gen deb_sh_dep=100*(nb_dep/text_size)
gen deb_sh_com=100*(nb_com/text_size)
gen deb_high`r'_sh_local=deb_sh_com+deb_sh_dep
drop v1 text_size nb_dep nb_com deb_sh_dep deb_sh_com

merge 1:1 year id_1997 using "$temp/temp97"
assert _merge!=1
drop _merge
save "$temp/temp97", replace

insheet using "$raw_data/outcomes/df_nat_deb_high`r'_1997.csv", clear
gen year=1997

gen deb_high`r'_sh_nat=100*(nb_nat/text_size)
drop v1 text_size nb_nat

merge 1:1 year id_1997 using "$temp/temp97"
assert _merge!=1
drop _merge

gen deb_high`r'_ratio_local=log((100+deb_high`r'_sh_local)/(100+deb_high`r'_sh_nat))
sum deb_high`r'_ratio_local
save "$temp/temp97", replace
}

*** frequency of references to first and last names
insheet using "$raw_data/outcomes/df_nom_1997.csv", clear
keep if tour==1
gen year=1997

gen sh_self1=100*(nom_self+prenom_self)/text_size

gen sh_other1=100*(nom_all+prenom_all-nom_self-prenom_self)/text_size
drop v1 tour text_size

merge 1:1 year id_1997 using "$temp/temp97"
assert _merge!=1
drop _merge
save "$temp/temp97", replace

*** originality relative to other manifestos from the same party
insheet using "$raw_data/outcomes/df_sim_nuance_1997.csv", clear
gen year=1997

drop v1

merge 1:1 year id_1997 using "$temp/temp97"
assert _merge!=1
drop _merge

replace mean_similarity_nuance=. if text_size_sr1==0 //few empty manifestos with unreliable similarity measure set to missing

save "$temp/temp97", replace

*** left-right score without local references
insheet using "$raw_data/outcomes/df_srproj_scaling_noloc_1997.csv", clear
keep if tour==1
egen temp=sd(covarsnuance3)
gen score_noloc_sr1=covarsnuance3/temp
drop v1 x tour temp covarsnuance3
rename m text_size_noloc_sr1

replace score_noloc_sr1=. if text_size_noloc_sr1==0 

rename id id_1997
gen year=1997

merge 1:1 year id_1997 using "$temp/temp97"
assert _merge!=1
drop _merge

count if text_size_sr1<text_size_noloc_sr1
assert r(N)==0
count if score_sr1==score_noloc_sr1 & text_size_sr1!=text_size_noloc_sr1
assert r(N)==0
count if (score_sr1==. & score_noloc_sr1!=.) | (score_sr1!=. & score_noloc_sr1==.) & text_size_noloc_sr1!=0
assert r(N)==0
corr score_noloc_sr1 score_sr1
save "$temp/temp97", replace


* Merges back with electoral results from Le Pennec (2022)
use  "$raw_data/elections/l_cand_1997", clear
keep if id_1997!=.
gen year=1997

gen temp=substr(dep_1997, 1, 1)
drop if temp=="Z"
drop temp //drop overseas territories

keep id_1997 year ins_l1_1997 vot_l1_1997 exp_l1_1997 voix_l1_1997 voixexp_l1_1997 ins_l2_1997 exp_l2_1997 vot_l2_1997 voix_l2_1997 voixexp_l2_1997 nuance_1997 nuance2_1997 nuance3_1997

rename *_1997 *
rename id id_1997
merge 1:1 year id_1997 using "$temp/temp97"
count if (_merge!=3 & missing==0) | (_merge==3 & missing==1)
assert r(N)==0

count if id_cand==.
assert r(N)==0
drop _merge

*** compare parties across datasets
count if nuance1!="" & nuance!="" & nuance1!=nuance
assert r(N)==0
count if party==""
li codegeo candidat nuance nuance1 parti_lemonde voix1 missing if party==""
ta nuance1 if party==""
replace party="other" if party=="" & (nuance1!="" | nuance!="")

*** compare vote shares across datasets
byso codegeo: egen temp=total(perc_voix_exp1) 
sum temp
byso codegeo: egen temp2=total(voixexp_l1) 
sum temp2
count if voix1==.
li codegeo candidat exprimes1 voix1_lemonde voix_l1 temp temp2 missing if voix1==. 
li codegeo candidat exprimes1 voix1_lemonde voix_l1 temp temp2 if voix1==. & missing==0 
count if voix1_lemonde==.
li codegeo candidat exprimes1 voix1 voix_l1 temp temp2 if voix1_lemonde==. & missing==0 
drop temp*

replace voix1=voix_l1 if voix1==.
replace voix1=voix1_lemonde if voix1==.

count if voix2==. & voix2_lemonde!=.
li codegeo candidat exprimes2 voix2_lemonde voix_l2 if voix2==. & voix2_lemonde!=. 
count if voix2_lemonde==. & voix2!=.
gen perc_voix_exp2=100*(voix2/exprimes2)
byso codegeo: egen temp=total(perc_voix_exp2) 
sum temp
byso codegeo: egen temp2=total(voixexp_l2) 
sum temp2
li codegeo candidat exprimes2 voix2_lemonde voix_l2 temp temp2 missing if voix2==. & voix_l2!=.
drop temp*

replace voix2=voix_l2 if voix2==.

gen codegeo97=codegeo
save "$temp/merge97", replace

*_______________________________________________________________________________
*                                        1988
*_______________________________________________________________________________

use "$temp/tomerge", clear
keep if year==1988
save "$temp/tomerge88", replace


* Merge with id_1988:
use  "$raw_data/elections/l_cand_1988", clear
keep if id_1988!=.

keep id_1988 dep_1988 circ_1988 nom_1988 prenom_1988
rename dep_1988 dep
rename circ_1988 circ
rename nom_1988 nom
rename prenom_1988 prenom

gen candidat=lower(nom + " " + prenom)
replace candidat=subinstr(candidat, "-", " ", .)
replace candidat=subinstr(candidat, "'", " ", .)

so dep circ candidat
gen idm=_n

reclink dep circ candidat using "$temp/tomerge88", idu(idu) idm(idm) req(dep circ) gen(match)
assert _merge==3

li candidat Ucandidat match if match<1 & match>0.9
li candidat Ucandidat match if match<0.9
//everything merges well

keep if match!=.
byso id_1988: gen temp=_N
assert temp==1
drop temp _merge

byso idu: gen temp=_N
assert temp==1
drop temp

drop candidat
rename Ucandidat candidat
drop Udep Ucirc idu idm
save "$temp/temp", replace

* check for discrepancies between two original datasets:
use "$temp/tomerge88", clear
merge 1:1 dep circ candidat using "$temp/temp"
li dep circ candidat nom prenom if _merge==1

gen missing=0
drop _merge
save "$temp/temp88", replace


* merges back with electoral results from Le Pennec (2022):
use  "$raw_data/elections/l_cand_1988", clear
keep if id_1988!=.
gen year=1988
keep id_1988 year ins_l1_1988 vot_l1_1988 exp_l1_1988 voix_l1_1988 voixexp_l1_1988 ins_l2_1988 exp_l2_1988 vot_l2_1988 voix_l2_1988 voixexp_l2_1988 nuance_1988 nuance2_1988 nuance3_1988 incumb_1988

rename *_1988 *
rename id id_1988
merge 1:1 year id_1988 using "$temp/temp88"
count if _merge!=3 & missing==0
assert r(N)==0

count if id_cand==.
assert r(N)==0
drop _merge

*** compare parties across datasets
count if nuance1!="" & nuance!="" & nuance1!=nuance
count if party==""
li codegeo candidat nuance nuance1 parti_lemonde voix1 missing if party=="" 
ta nuance1 if party==""
replace party="UDF" if party=="" & nuance1=="udf"
replace party="PS" if party=="" & nuance1=="soc"
replace party="PRG" if party=="" & nuance1=="rdg"
replace party="other" if party=="" & (nuance1!="" | nuance!="")

*** compare vote shares across datasets
count if voix1==.
count if voix1_lemonde==.
byso codegeo: egen temp=total(perc_voix_exp1) 
sum temp
byso codegeo: egen temp2=total(voixexp_l1) 
sum temp2
li codegeo candidat voix1_lemonde voix_l1 temp temp2 missing if voix1==. 
li codegeo candidat voix1 voix_l1 temp temp2 missing if voix1_lemonde==. 
drop temp*

count if voix2==. & voix2_lemonde!=.
li codegeo candidat exprimes2 voix2_lemonde voix_l2 if voix2==. & voix2_lemonde!=. 
count if voix2_lemonde==. & voix2!=.
gen perc_voix_exp2=100*(voix2/exprimes2)
byso codegeo: egen temp=total(perc_voix_exp2) 
sum temp
byso codegeo: egen temp2=total(voixexp_l2) 
sum temp2
li codegeo candidat exprimes2 voix2_lemonde voix_l2 temp temp2 missing if voix2==. & voix_l2!=. 
drop temp*

replace Dincumbent=incumb 

gen codegeo88=codegeo
save "$temp/merge88", replace

********************************************************************************
* 2) Append all election years together and construct some additional outcomes

use  "$temp/merge97", clear
append using "$temp/merge93"
append using "$temp/merge88"

*** replace electoral shares to take into account the cleaning above
foreach x in "inscrits1" "abstentions1" "exprimes1" "votants1" "blancsetnuls1" "inscrits2" "exprimes2" "votants2" "blancsetnuls2"{
byso year codegeo: egen temp=min(`x')
replace `x'=temp if `x'==. //fill in blanks 
drop temp
}

count if inscrits1==.
assert r(N)==0
count if exprimes1==.
assert r(N)==0
count if votants1==.
assert r(N)==0

count if inscrits2==. & voix2!=.
assert r(N)==0
count if exprimes2==. & voix2!=.
assert r(N)==0
count if votants2==. & voix2!=.
assert r(N)==0

forval i=1/2{
replace perc_vot_ins`i'=100*(votants`i'/inscrits`i')
replace perc_voix_exp`i'=100*(voix`i'/exprimes`i')
}
replace perc_abs_ins1=100*(abstentions1/inscrits1)
replace perc_exp_ins1=100*(exprimes1/inscrits1)
replace perc_exp_vot1=100*(exprimes1/votants1)
replace perc_bin_ins1=100*(blancsetnuls1/inscrits1)
replace perc_bin_vot1=100*(blancsetnuls1/votants1)
replace perc_voix_ins1=100*(voix1/inscrits1)

gen mean_originality_nuance=-mean_similarity_nuance
assert mean_originality_nuance==. if party!="FN" & party!="UMP" & party!="PS" & party!="PC" & party!="Verts" //outcome defined only for main parties

gen temp=100-(economy2_prob1+social2_prob1+homeland2_prob1+foreign2_prob1)
gen concen_topic=(economy2_prob1/100)^2+(social2_prob1/100)^2+(homeland2_prob1/100)^2+(foreign2_prob1/100)^2+(temp/100)^2 //concentration of policy topics
drop temp

*** keep only variables used as outcomes in later analysis
keep year id_cand id_1988 id_1993 id_1997 missing nuance nuance1 voix_* ins_* vot_* exp_* voixexp_* party voix* inscrits* votants* exprimes* votants* blancsetnuls* perc_*  ///
deb_* qu_* score* abs* ratio* sh_* *_total text_size* *_prob1 concen_* ///
Dincumbent mayor conseiller_departemental senateur depute_europeen mean_*

********************************************************************************
*** add labels
do "$dofiles/_labels.do"

*** save dataset
save "$output/outcomes", replace


