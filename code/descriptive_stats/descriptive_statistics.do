
********************************************************************************
*                                    Figures                                   *
********************************************************************************

********************************************************************************
*** Figure 1: Revenues and firm donations across parties

use "$output/data_cand_8897", clear

keep if year==1993 | year==1997
replace private_donation_firms_cst=0 if private_donation_firms_cst==.

count if total_revenues_cst==.

gen nb_cand_wd=1
replace nb_cand_wd=. if private_donation_firms_cst==.

replace bigparty="Far-right" if bigparty=="FN"
replace bigparty="Right" if bigparty=="UMP"
replace bigparty="Socialist" if bigparty=="PS"
replace bigparty="Communist" if bigparty=="PC"
replace bigparty="Green" if bigparty=="Verts"
replace bigparty="Other" if bigparty=="other"

gen a=1
collapse (mean)   total_revenues_cst private_donation_firms_cst (sum)  sum_nb_cand_wd=nb_cand_wd sum_total_revenues_cst= total_revenues_cst sum_private_donation_firms_cst=private_donation_firms_cst a , by(party bigparty year)

preserve
gen sh_don=private_donation_firms_cst/total_revenues_cst
bys bigparty: sum sh_don
gen order=.

replace order=1 if bigparty=="Communist"
replace order=2 if bigparty=="Green"
replace order=3 if bigparty=="Socialist"
replace order=4 if bigparty=="Right"
replace order=5 if bigparty=="Far-right"
replace order=6 if bigparty=="Other"

graph bar total_revenues_cst private_donation_firms_cst , ///
stack over(year , label(labsize(vsmall)))  over(bigparty , sort(order) label(labsize(small))) /// 
bar(1, fcolor(black) lcolor(black)) bar(2, fcolor(white) lcolor(black)) ///
ytitle("Revenues in €")  ///
legend(label(1 "Other revenues")  label(2 "Firm donations") size(small)) ///
ylab(0 "0" 20000 "20,000" 40000 "40,000" 60000 "60,000" 80000 "80,000" 100000 "100,000" 120000 "120,000", labsize(small) angle(horizontal)) ///
graphregion(color(white))

graph export  "$main/Figure2.pdf", replace	

restore


********************************************************************************
*                                    Tables                                    *
********************************************************************************

********************************************************************************
*** Table 1: campaign spendings and revenues

use "$output/data_cand_8897", clear

keep if year==1993 | year==1997

keep year codegeo candidat id_cand private_donation_indiv_cst private_donation_firms_cst total_expenditures_cst total_revenues_cst party_contribution_cst kind_contribution_cst other_cst personal_contribution_cst other_kind_cst sh_dons_firms sh_dons_indiv sh_party_contrib    sh_personal_contrib nb_elec

gen x=_n
reshape wide private_donation_indiv_cst private_donation_firms_cst total_expenditures_cst total_revenues_cst party_contribution_cst kind_contribution_cst other_cst personal_contribution_cst other_kind_cst sh_dons_firms sh_dons_indiv sh_party_contrib  sh_personal_contrib 	, i(x) j(year)

********************************************************************************	
quietly : do "$dofiles/_labels.do"
********************************************************************************

global 	stat_revenues total_expenditures_cst1993 total_expenditures_cst1997  ///
total_revenues_cst1993 total_revenues_cst1997  ///
sh_dons_firms1993 sh_dons_firms1997  ///
sh_dons_indiv1993 sh_dons_indiv1997  ///
sh_personal_contrib1993 sh_personal_contrib1997  ///
sh_party_contrib1993 sh_party_contrib1997 

eststo clear
estpost sum $stat_revenues, d

eststo : esttab using "$main/Table1.tex", label replace ///
cells("mean(fmt(%15.0fc %15.0fc %15.0fc %15.0fc %15.2fc %15.2fc %15.2fc %15.2fc) label(Mean)) p50(fmt(%15.0fc  %15.0fc %15.0fc %15.0fc %15.2fc %15.2fc %15.2fc %15.2fc) label(Median)) sd(fmt(%15.0fc %15.0fc %15.0fc %15.0fc %15.2fc %15.2fc %15.2fc %15.2fc) label(sd)) min(fmt(%15.0fc) label(Min)) max(fmt(%15.0fc) label(Max)) count(fmt(%15.0fc) label(N))") ///
style(tex) nonum noobs ///
mlabels("Spending (cst \euro)", pattern(1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
refcat(total_expenditures_cst1993 "\textbf{Total spending per candidate}" total_revenues_cst1993 "\textbf{Total revenues}" sh_dons_firms1993 "\textbf{Share firm donations}"sh_dons_indiv1993 "\textbf{Share individual donations}" sh_personal_contrib1993 "\textbf{Share personal contributions}" sh_party_contrib1993 "\textbf{Share party contributions}", nolabel)


********************************************************************************
* Table 2: Firm donations in 1993

//NB: this table is created in three steps: we append three tables to study the candidate, donations and donor level.

*** A) Part 1: summarize at the level of the candidate

est clear 

use "$output/data_cand_8897", clear
keep if year==1993
tempfile cand 
save `cand', replace

gen yes_donation=(private_donation_firms_cst>0 & private_donation_firms_cst!=.)

replace dons_firms=0 if dons_firms==.

count if nb_don!=.
sum nb_don if yes_donation==1, d
replace nb_don=r(mean) if nb_don==. & yes_donation==1
replace nb_don=0 if nb_don==. & yes_donation==0
sum private_donation_firms_cst  if yes_donation==1, d
replace private_donation_firms_cst=24406.27 if private_donation_firms_cst==. & yes_donation==1
replace private_donation_firms_cst=0 if private_donation_firms_cst==. & yes_donation==0

estpost  summarize yes_donation nb_don private_donation_firms_cst dons_firms sh_dons_firms, d

esttab using "$main/Table2.tex", ///
replace  nonumber noobs label style(tex) ///
cells((mean(fmt(%15.2fc %15.2fc %15.0fc  %15.2fc %15.2fc  ) label("Mean")) ///
p50(fmt(%15.2fc %15.2fc %15.0fc  %15.2fc %15.2fc) label("Median")) /// 
p75(fmt(%15.0fc %15.2fc %15.0fc  %15.2fc %15.2fc) label("p75")) ///
sd(fmt( %15.2fc %15.2fc %15.0fc %15.2fc %15.2fc %15.2fc) label("sd")) ///
count(fmt(%15.0fc) label("N"))))  ///
refcat(yes_donation "\textbf{A. Candidates}", nolabel) ///
coeflabels(yes_donation "Firm donations $>$ 0 (\%)" nb_don "\# Firm donations" dons_firms "Firm donations (euros/voter)" private_donation_firms_cst "Firm donations (\euro)" sh_dons_firms "\% Firm donations in total revenue") ///
postfoot("") 


*** B) Part 2: Donor level

use "$output/donations_with_id_cand_donor.dta", clear
ren cand id_cand
merge m:1 id_cand year using `cand', gen(m2)

//We need to manually fix two mistakes from the original donations data
replace montant=20000/2 if montant==. & id_cand==27873  //pozzoli bernard, divide total amount between 2 donations
replace montant=16500 if montant==. & id_cand==27508 //pohu jean pierre, divide total amount between 1 donation

gen montant_cst=montant*.1968454
cap drop nb_dons
gen nb_dons=1
gen montant_cst_loc=montant_cst
replace montant_cst_loc=. if local_donor==0

gen nb_dons_loc=nb_dons
replace nb_dons_loc=. if local_donor==0

drop if donor_id==""


local type ="local_donor nb_circo_actif donor_left donor_right donor_notideo donor_multiparty donor_oneparty"
foreach x in `type'{
byso donor_id: egen t=sd(`x')
assert t==0 if t!=.
drop t
}

collapse (mean) `type' mean_montant_cst=montant_cst   mean_montant_cst_loc=montant_cst_loc  (sum)   nb_dons sum_montant_cst=montant_cst nb_dons_loc  sum_montant_cst_loc=montant_cst_loc   , by(donor_id )

gen small_donor=local_donor
gen morethanone=1-local_donor
gen local=nb_circo_actif==1 if local_donor==0
gen multipledonor=nb_circo_actif>1 if local_donor==0


est clear

estpost  summarize nb_dons sum_montant_cst small_donor  morethanone   local multipledonor donor_oneparty donor_multiparty, d

********************************************************************************	
quietly : do "$dofiles/_labels.do"
********************************************************************************

label var nb_dons					"\# Firm donations"
label var mean_montant_cst 			"Mean Donation Value (\euro)"
label var sum_montant_cst 			"Total donations (\euro)"

esttab   using "$main/Table2.tex", ///
append  label style(tex) mlabels(none) mtitles(none) collabels(none) noobs nonumber ///
cells((mean(fmt(%15.2fc %15.0fc %15.2fc) label("Mean")) ///
p50(fmt(%15.2fc %15.0fc %15.2fc) label("Median")) /// 
p75(fmt(%15.2fc %15.0fc %15.2fc) label("p75")) ///
sd(fmt(%15.2fc %15.0fc %15.2fc) label("sd")) ///
count(fmt(%15.0fc) label("N"))))  ///
refcat(nb_dons "\textbf{B. Donors}" , nolabel) ///
prehead("")   posthead("")		///
postfoot("")


***C) Part 3: Donation level

use "$output/donations_with_id_cand_donor.dta", clear

ren cand id_cand

replace montant=20000/2 if montant==. & id_cand==27873  
replace montant=16500 if montant==. & id_cand==27508

replace montant=7532 if montant==75322     
replace montant=10000 if montant==100000
replace montant=. if montant==150000     

gen montant_cst=montant*.1968454

gen montant_small=.
replace montant_small=montant_cst if nb_dons_donor==1
gen montant_morethanone=.
replace montant_morethanone=montant_cst if nb_dons_donor>1

estpost summarize montant_cst montant_small montant_morethanone  , d 

esttab  using "$main/Table2.tex", ///
append  label style(tex) mlabels(none) mtitles(none) collabels(none) noobs nonumber ///
cells((mean(fmt(%15.0fc) label("Mean")) ///
p50(fmt(%15.0fc) label("Median")) /// 
p75(fmt(%15.0fc) label("p75")) ///
sd(fmt(%15.0fc) label("sd")) ///
count(fmt(%15.0fc) label("N"))))  ///
refcat(montant_cst "\textbf{C. Donations}", nolabel) ///
coeflabels(montant_cst "Donation value (\euro)" montant_small "Donation value from small donors (\euro)" montant_morethanone "Donation value from multiple donors (\euro)" ) ///
prehead("")  posthead("") 


