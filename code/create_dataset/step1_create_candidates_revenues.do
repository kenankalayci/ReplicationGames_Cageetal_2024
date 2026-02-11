* Create dataset : Step 1

********************************************************************************
* Raw data 1: Digitized data campaign accounts

use "$raw_data/revenues/raw_coporate_donations.dta", clear ///14,497 donations

*** Data cleaning
* colette cordat: no donations received	
drop if  key_candid=="18 02 cordat colette"
*jacques floch: removed because 0 donations in the recap and the sum of fundings.
drop if  key_candid=="44 04 floch jacques"
*alain grevy:removed because 0 donations in the recap and the sum of fundings.
drop if  key_candid=="74 03 grevy alain"
*jacques floch:removed because 0 donations in the recap and the sum of fundings.
drop if  key_candid=="59 02 de greve jean raymond"

***  Manual cleaning of names
replace key_candid="13 04 isoardo andre" if key_candid=="13 04 isoardo andre sebastien"
replace key_candid="19 01 aubert raymond" if key_candid=="19 01 aubert raymond max"
replace key_candid="24 03 laviale pierre claude" if key_candid=="24 03 laviale claude"
replace key_candid="30 02 casas simon" if key_candid=="30 02 domb dit simon"
replace key_candid="31 06 de veyrinas francoise" if key_candid=="31 06 hebrard de veyrinas francoise"
replace key_candid="39 03 santa cruz jean pierre" if key_candid=="39 03 santa cruz jean etienne"
replace key_candid="40 03 emmanuelli henri" if key_candid=="40 03 emmanueli henri"
replace key_candid="41 03 d alancon hubert" if key_candid=="41 03 audemart d alencon hubert"
replace key_candid="49 06 de charette herve" if key_candid=="49 06  de charette herve"
replace key_candid="59 08 terrier francoise" if key_candid=="59 08 doogbaud nee terrier francoise"
replace key_candid="60 04 macudzinski serge" if key_candid=="60 04 maculzinski serge"
replace key_candid="60 06 gonnot francois michel" if key_candid=="60 06 gonnot michel"
replace key_candid="64 02 cantet michel" if key_candid=="64 02 cantet michel eugene roger"
replace key_candid="71 01 voisin gerard" if key_candid=="71 01 voisin gerard jean maurice"
replace key_candid="73 02 bonhopierre" if key_candid=="73 02 bonhomme pierre"
replace key_candid="77 06 chabason lucien" if key_candid=="77 06 jocelyne chabason lucien"
replace key_candid="81 04 carayon bernard" if key_candid=="81 04 carayon andre"
replace key_candid="84 01 rezouau mouloud" if key_candid=="84 01 rezouali mouloud"
replace key_candid="74 03 grevy alain" if key_candid=="74 03 grevy alain"

drop page
gen year=1993

gen key_candid2=key_candid

tempfile corp_don
save `corp_don', replace

********************************************************************************
* Raw data 2: Bekkouche, Cage, Dewitte (JPubE, 2022)
																																	
use "$raw_data/elections/data_legi_donations_results_controls_1988_2002_R1R2", clear

isid cand year
keep if year==1993 // we keep 1993 to merge with the donation dataset
count

***  three identifiers for candidates' names
gen key_candid=codegeo+" "+candidat
gen key_candid2=codegeo+" "+candidat_lemonde

***  private_donations (only the total in this dataset)
gen yes_private_donations=private_donation_firms_cst>0
replace yes_private_donations=. if private_donation_firms_cst==.

*** 5 missing values that we replace by zeros
replace  private_donation_firms_cst=0 if private_donation_firms_cst==.
cap drop yes_private_donations
gen yes_private_donations=private_donation_firms_cst>0
replace yes_private_donations=. if private_donation_firms_cst==.
tab yes_private_donations, m

replace key_candid=subinstr(key_candid, "  ", " ", .)
keep if yes_private_donations==1

*** in order to match with the donations' dataset, we keep only when private_donations >0
drop if private_donation_firms_cst==.

*** we merge with the dataset on donations
merge 1:m key_candid year using `corp_don'

preserve 
keep if _merge==3
count // 1,616 cand
tempfile matched_1
save `matched_1', replace
restore

keep if _merge==1
keep codegeo  year key_candid2  cand party nuance1
merge 1:m key_candid2 using `corp_don'

keep if _merge==3
append using `matched_1'
count  
assert r(N)==14484

keep montant codegeo entreprise key_candid cand  party nuance1
replace montant=subinstr(montant, " ", "", .)
destring montant, replace // 3 missing values : 03 02 pozzoli bernard & 49 04 pohu jean pierre: corrected later at the result / statistics level
ren entreprise donor_name

*** generate a donation identifyer
 order cand 
gsort cand -montant
bys cand: gen num_don=_n
tostring num_don, replace
gen len_num=strlen(num_don)
replace num_don="0"+num_don if len_num==1
drop len_num
tostring cand, gen(cand_idstr)
gen len_num=strlen(cand_idstr)
replace cand_idstr="0"+cand_idstr if len_num==1
gen id_donation=cand_idstr+" "+num_don
isid id_donation
assert cand!=.

********************************************************************************
* Raw data 3: donors names and sectors from manual cleaning of the dataset (see appendix A1 & folder 3.sources/firm_donations)

merge 1:m id_donation using "$raw_data/revenues/donations_sectors.dta", gen(m2)
assert m2==3
drop m2 _merge

*** generate sector x size of the donor 
*** multi donor * sector
gen sector_multidonor=sector
replace sector_multidonor="" if local_donor==1

*** big donor * sector
gen sector_bigcompany=sector
replace sector_bigcompany="" if belong_bigcompany==0

*** donations per district
preserve
keep codegeo  donor_id
gen dep=substr(codegeo, 1, 2)
gen a=1
collapse (sum) nb_dons_circo=a, by(codegeo donor_id)
tempfile nb_circo
save `nb_circo', replace
restore

preserve	
keep codegeo  donor_id
gen dep=substr(codegeo, 1, 2)
gen a=1
collapse (sum) nb_dons_dep=a, by(dep donor_id)
tempfile nb_dep
save `nb_dep', replace
restore

merge m:1 codegeo donor_id using `nb_circo', nogen
gen dep=substr(codegeo, 1, 2)
merge m:1 dep donor_id using `nb_dep', nogen

*** district of the donations
preserve
keep  donor_id   codegeo
duplicates drop
bys donor_id: gen nb_circo_actif=_N
tab nb_circo_actif
tempfile circo_actif
save `circo_actif'
restore

preserve
keep  donor_id   dep
duplicates drop
bys donor_id: gen nb_dep_actif=_N
tab nb_dep_actif
tempfile dep_actif
save `dep_actif'
restore

merge m:1 codegeo donor_id using `circo_actif', nogen
merge m:1 dep donor_id using `dep_actif', nogen

gen donor_multi_dep=nb_dep_actif>1
gen donor_multi_circo=nb_circo_actif>1
			
*** cross party vs single party donor
preserve
keep  donor_id  party 
duplicates drop
bys donor_id: gen nb_party=_N if party!="other"

tempfile party_nb
save `party_nb'
restore

merge m:1 donor_id  party  using `party_nb', nogen

gen donor_multiparty=nb_party>1 if nb_dons_donor>1 & nb_party!=. //excludes those who give only to independents since we don't know if it's the same independent movement or not
gen donor_oneparty=nb_party==1 if nb_dons_donor>1  & nb_party!=.

byso  donor_id: egen temp=min(donor_multiparty)
replace donor_multiparty=temp
drop temp
byso  donor_id: egen temp=min(donor_oneparty)
replace donor_oneparty=temp
drop temp

//we use the nuance from Ministere de l'interieur to take independents into account and classify by orientation as for scaling
gen dondedroite=(nuance1=="dvd" | nuance1=="exd" |  nuance1=="frn" |  nuance1=="rpr" |  nuance1=="udf") if nuance1=="dvd" | nuance1=="exd" |  nuance1=="frn" |  nuance1=="rpr" |  nuance1=="udf" | nuance1=="exg" | nuance1=="maj" |  nuance1=="rdg" |  nuance1=="com" |  nuance1=="soc" |  nuance1=="vec" //well identified nuances

byso donor_id: egen temp=max(dondedroite)
gen nomissing=temp!=.
drop temp

byso donor_id: egen temp=mean(dondedroite) if nomissing==1
gen donor_right=temp==1 if nomissing==1 & nb_dons_donor>1
gen donor_left=temp==0 if nomissing==1 & nb_dons_donor>1
gen donor_notideo=temp>0 & temp<1 if nomissing==1 & nb_dons_donor>1
drop temp nomissing

byso donor_id: gen temp=_n
ta donor_left if temp==1
ta donor_right if temp==1
ta donor_notideo if temp==1
drop temp len_num num_d belong_bigcompany sector_bigcompany  dondedroite

********************************************************************************
*** add labels
do "$dofiles/_labels.do"

*** save dataset
save "$output/donations_with_id_cand_donor.dta", replace
