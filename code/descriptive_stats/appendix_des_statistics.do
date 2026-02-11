********************************************************************************
*                                   APPENDIX                                   *
*                                    Figures                                   *
********************************************************************************

********************************************************************************
* Figure A1 :Descriptive statistics on firm donations, depending on the sector of activity

use "$output/donations_with_id_cand_donor.dta", clear

replace sector="Agriculture" if sector=="agriculture"
replace sector="Construction" if sector=="construction"
replace sector="Culture" if sector=="culture"
replace sector="Economy-Finance" if sector=="economy/fin"
replace sector="Environment-Energy" if sector=="environment-energy"
replace sector="Travel" if sector=="foreign/travel"
replace sector="Health" if sector=="health"
replace sector="Industry" if sector=="industry"
replace sector="Justice" if sector=="justice"
replace sector="Retail" if sector=="pme/retail"
replace sector="NGOs" if sector=="public/political"
replace sector="Sport" if sector=="sports/asso"
replace sector="Unknown" if sector=="unknown"

encode sector, gen(secnum)
tab secnum

bys sector: gen numsec=_N

gen sector2=sector
replace sector2="Other" if numsec<500

gen montant_cst=montant*.1968454

* Figure A1a)
byso donor_id: gen numdon=_N
graph bar numdon, over(sector2, sort(numdon) label(angle(45))) ytitle("Mean number of donations") graphregion(color(white))

graph export "$appendix/FigureA1a.pdf", replace

* Figure A1b)
graph bar montant_cst , over(sector2, sort(montant_cst) label(angle(45))) ytitle("Mean donation in cst euros") graphregion(color(white))

graph export "$appendix/FigureA1b.pdf", replace


********************************************************************************
* Figure B.1: Candidates' accounts: 1988 and 1993, Anecdotal evidence from 15 departments 

use "$output/data_cand_8897", replace 

keep if inlist(dep,"01","10","14","19","23","24","27") | inlist(dep,"36","41","49","57","74","76","87","89") //departments for which we have 1988 data
keep if year==1988 | year==1993
bysort id_cand : gen dup=cond(_N==1,0,_n)
gen repeat=(dup>0)
drop dup 
tab repeat if year==1988

order year id_cand expenditures revenues personal_contrib party_contrib dons_indiv dons_firms
sort id_cand year  

duplicates tag id_cand, gen(t)
preserve
keep if t==1


graph bar (mean) expenditures revenues personal_contrib party_contrib dons_indiv dons_firms, over(year) ///
ytitle("Cst € per voter") ///
ylabel(0(0.2)0.6, angle(horizontal)) ///
legend(size(small) order(1 "Total expenditures" 2 "Total revenues" 3 "(of which) Personal contributions" 4 "(of which) Party contributions" 5 "(of which) Individual donations"  6 "(of which) Firm donations")) ///
graphregion(color(white))
restore

graph export "$appendix/FigureB1.pdf", replace


********************************************************************************
*Figure D.7: Kernel density of the local index by party

local letters = "a b"
local t=0
foreach y in "1993" "1997"{
local t=`t'+1
local l=word("`letters'", `t')

//density for each nuance
use "$output/data_cand_8897", clear
count if year==`y' & bigparty!="" & ratio_local1!=.
kdensity ratio_local1 if bigparty=="PC" & year==`y' & ratio_local1<0.1 & ratio_local1>-0.1
local j=2*r(bwidth)
kdensity ratio_local1 if bigparty=="PC" & year==`y' & ratio_local1<0.1 & ratio_local1>-0.1 , bw(`j') nograph generate(x epan1)
keep if epan!=.
keep x epan
save "$temp/toappend", replace

use "$output/data_cand_8897", clear
kdensity ratio_local1 if bigparty=="PS" & year==`y' & ratio_local1<0.1 & ratio_local1>-0.1 
local j=2*r(bwidth)
kdensity ratio_local1 if bigparty=="PS" & year==`y' & ratio_local1<0.1 & ratio_local1>-0.1 , bw(`j') nograph generate(x epan2)
keep if epan!=.
keep x epan
append using "$temp/toappend"
save "$temp/toappend", replace

use "$output/data_cand_8897", clear
kdensity ratio_local1 if bigparty=="Verts" & year==`y' & ratio_local1<0.1 & ratio_local1>-0.1
local j=2*r(bwidth)
kdensity ratio_local1 if bigparty=="Verts" & year==`y' & ratio_local1<0.1 & ratio_local1>-0.1 , bw(`j') nograph generate(x epan3)
keep if epan!=.
keep x epan
append using "$temp/toappend"
save "$temp/toappend", replace

use "$output/data_cand_8897", clear
kdensity ratio_local1 if bigparty=="UMP" & year==`y' & ratio_local1<0.1 & ratio_local1>-0.1 
local j=2*r(bwidth)
kdensity ratio_local1 if bigparty=="UMP" & year==`y' & ratio_local1<0.1 & ratio_local1>-0.1 , bw(`j') nograph generate(x epan4)
keep if epan!=.
keep x epan
append using "$temp/toappend"
save "$temp/toappend", replace

use "$output/data_cand_8897", clear
kdensity ratio_local1 if bigparty=="FN" & year==`y' & ratio_local1<0.1 & ratio_local1>-0.1
local j=2*r(bwidth)
kdensity ratio_local1 if bigparty=="FN" & year==`y' & ratio_local1<0.1 & ratio_local1>-0.1 , bw(`j') nograph generate(x epan5)
keep if epan!=.
keep x epan
append using "$temp/toappend"
save "$temp/toappend", replace

use "$output/data_cand_8897", clear
kdensity ratio_local1 if bigparty=="other" & year==`y' & ratio_local1<0.1 & ratio_local1>-0.1
local j=2*r(bwidth)
kdensity ratio_local1 if bigparty=="other" & year==`y' & ratio_local1<0.1 & ratio_local1>-0.1 , bw(`j') nograph generate(x epan6)
keep if epan!=.
keep x epan
append using "$temp/toappend"

forval i=1/6{
label var epan`i' "Density"
}
label var x "Local index"
twoway (line epan1 x, lc(maroon)) (line epan2 x, lc(pink)) (line epan3 x, lc(green)) (line epan4 x, lc(midblue)) (line epan5 x, lc(dknavy)) (line epan6 x, lc(orange)), ytitle(Density) ylab(0 (5) 30) xlab(-0.1 (0.02) 0.1) graphregion(color(white)) ///
legend(rows(2) symx(5) order(1 "Communist" 3 "Green" 2 "Socialist" 4 "Right" 5 "Far-right" 6 "Other"))

graph export "$appendix/FigureD7`l'.pdf", replace
}


********************************************************************************
* Figure D8 :  Kernel density of left-right score by party

local letters = "a b"
local t=0
foreach y in "1993" "1997"{
local t=`t'+1
local l=word("`letters'", `t')

//density for each nuance
use "$output/data_cand_8897", clear
count if year==`y' & bigparty!="" & score_sr1!=.
kdensity score_sr1 if bigparty=="PC" & year==`y'
local j=2*r(bwidth)
kdensity score_sr1 if bigparty=="PC" & year==`y', bw(`j') nograph generate(x epan1)
keep if epan!=.
keep x epan
save "$temp/toappend", replace

use "$output/data_cand_8897", clear
kdensity score_sr1 if bigparty=="PS" & year==`y'
local j=2*r(bwidth)
kdensity score_sr1 if bigparty=="PS" & year==`y', bw(`j') nograph generate(x epan2)
keep if epan!=.
keep x epan
append using "$temp/toappend"
save "$temp/toappend", replace

use "$output/data_cand_8897", clear
kdensity score_sr1 if bigparty=="Verts" & year==`y'
local j=2*r(bwidth)
kdensity score_sr1 if bigparty=="Verts" & year==`y', bw(`j') nograph generate(x epan3)
keep if epan!=.
keep x epan
append using "$temp/toappend"
save "$temp/toappend", replace

use "$output/data_cand_8897", clear
kdensity score_sr1 if bigparty=="UMP" & year==`y'
local j=2*r(bwidth)
kdensity score_sr1 if bigparty=="UMP" & year==`y', bw(`j') nograph generate(x epan4)
keep if epan!=.
keep x epan
append using "$temp/toappend"
save "$temp/toappend", replace

use "$output/data_cand_8897", clear
kdensity score_sr1 if bigparty=="FN" & year==`y'
local j=2*r(bwidth)
kdensity score_sr1 if bigparty=="FN" & year==`y', bw(`j') nograph generate(x epan5)
keep if epan!=.
keep x epan
append using "$temp/toappend"
save "$temp/toappend", replace

use "$output/data_cand_8897", clear
kdensity score_sr1 if bigparty=="other" & year==`y'
local j=2*r(bwidth)
kdensity score_sr1 if bigparty=="other" & year==`y', bw(`j') nograph generate(x epan6)
keep if epan!=.
keep x epan
append using "$temp/toappend"
save "$temp/toappend", replace


forval i=1/6 {
label var epan`i' "Density"
}
label var x "Left-right score"
twoway (line epan1 x, lc(maroon)) (line epan2 x, lc(pink)) (line epan3 x, lc(green)) (line epan4 x, lc(midblue)) (line epan5 x, lc(dknavy)) (line epan6 x, lc(orange)), ytitle(Density) ylab(0 (0.5) 3.5) xlab(-3 (1) 3) graphregion(color(white)) ///
legend(rows(2) symx(5) order(1 "Communist" 3 "Green" 2 "Socialist" 4 "Right" 5 "Far-right" 6 "Other")) 

graph export "$appendix/FigureD8`l'.pdf", replace
}


********************************************************************************
* Figure D.9: Kernel density of homeland security prevalence by party

local letters = "a b"
local t=0
foreach y in "1993" "1997"{
local t=`t'+1
local l=word("`letters'", `t')

//density for each nuance
use "$output/data_cand_8897", clear
count if year==`y' & bigparty!="" & interior_prob1!=.

kdensity interior_prob1 if bigparty=="PC" & year==`y'
local j=2*r(bwidth)
kdensity interior_prob1 if bigparty=="PC" & year==`y', bw(`j') nograph generate(x epan1)
keep if epan!=.
keep x epan
save "$temp/toappend", replace

use "$output/data_cand_8897", clear
kdensity interior_prob1 if bigparty=="PS" & year==`y'
local j=2*r(bwidth)
kdensity interior_prob1 if bigparty=="PS" & year==`y', bw(`j') nograph generate(x epan2)
keep if epan!=.
keep x epan
append using "$temp/toappend"
save "$temp/toappend", replace

use "$output/data_cand_8897", clear
kdensity interior_prob1 if bigparty=="Verts" & year==`y'
local j=2*r(bwidth)
kdensity interior_prob1 if bigparty=="Verts" & year==`y', bw(`j') nograph generate(x epan3)
keep if epan!=.
keep x epan
append using "$temp/toappend"
save "$temp/toappend", replace

use "$output/data_cand_8897", clear
kdensity interior_prob1 if bigparty=="UMP" & year==`y'
local j=2*r(bwidth)
kdensity interior_prob1 if bigparty=="UMP" & year==`y', bw(`j') nograph generate(x epan4)
keep if epan!=.
keep x epan
append using "$temp/toappend"
save "$temp/toappend", replace

use "$output/data_cand_8897", clear
kdensity interior_prob1 if bigparty=="FN" & year==`y'
local j=2*r(bwidth)
kdensity interior_prob1 if bigparty=="FN" & year==`y', bw(`j') nograph generate(x epan5)
keep if epan!=.
keep x epan
append using "$temp/toappend"
save "$temp/toappend", replace

use "$output/data_cand_8897", clear
kdensity interior_prob1 if bigparty=="other" & year==`y'
local j=2*r(bwidth)
kdensity interior_prob1 if bigparty=="other" & year==`y', bw(`j') nograph generate(x epan6)
keep if epan!=.
keep x epan
append using "$temp/toappend"
save "$temp/toappend", replace

forval i=1/6{
label var epan`i' "Density"
}
label var x "Prevalence of homeland security"
twoway (line epan1 x, lc(maroon)) (line epan2 x, lc(pink)) (line epan3 x, lc(green)) (line epan4 x, lc(midblue)) (line epan5 x, lc(dknavy)) (line epan6 x, lc(orange)), ytitle(Density) ylab(0 (0.02) 0.1) xlab(0 (10) 100) graphregion(color(white)) ///
legend(rows(2) symx(5) order(1 "Communist" 3 "Green" 2 "Socialist" 4 "Right" 5 "Far-right"  6 "Other"))

graph export "$appendix/FigureD9`l'.pdf", replace
}


********************************************************************************
* Figure D.10: Kernel density of candidate originality by party

local letters = "a b"
local t=0
foreach y in "1993" "1997"{
local t=`t'+1
local l=word("`letters'", `t')

//density for each nuance
use "$output/data_cand_8897", clear
count if year==`y' & bigparty!="" & bigparty!="other" & index_originality!=.

kdensity index_originality if bigparty=="PC" & year==`y'
local j=2*r(bwidth)
kdensity index_originality if bigparty=="PC" & year==`y', bw(`j') nograph generate(x epan1)
keep if epan!=.
keep x epan
save "$temp/toappend", replace

use "$output/data_cand_8897", clear
kdensity index_originality if bigparty=="PS" & year==`y'
local j=2*r(bwidth)
kdensity index_originality if bigparty=="PS" & year==`y', bw(`j') nograph generate(x epan2)
keep if epan!=.
keep x epan
append using "$temp/toappend"
save "$temp/toappend", replace

use "$output/data_cand_8897", clear
kdensity index_originality if bigparty=="Verts" & year==`y'
local j=2*r(bwidth)
kdensity index_originality if bigparty=="Verts" & year==`y', bw(`j') nograph generate(x epan3)
keep if epan!=.
keep x epan
append using "$temp/toappend"
save "$temp/toappend", replace

use "$output/data_cand_8897", clear
kdensity index_originality if bigparty=="UMP" & year==`y'
local j=2*r(bwidth)
kdensity index_originality if bigparty=="UMP" & year==`y', bw(`j') nograph generate(x epan4)
keep if epan!=.
keep x epan
append using "$temp/toappend"
save "$temp/toappend", replace

use "$output/data_cand_8897", clear
kdensity index_originality if bigparty=="FN" & year==`y'
local j=2*r(bwidth)
kdensity index_originality if bigparty=="FN" & year==`y', bw(`j') nograph generate(x epan5)
keep if epan!=.
keep x epan
append using "$temp/toappend"
save "$temp/toappend", replace

forval i=1/5{
label var epan`i' "Density"
}
label var x "Originality index"
twoway (line epan1 x, lc(maroon)) (line epan2 x, lc(pink)) (line epan3 x, lc(green)) (line epan4 x, lc(midblue)) (line epan5 x, lc(dknavy)), ytitle(Density) ylab(0 (0.5) 3) xlab(-3 (1) 2) graphregion(color(white)) ///
legend(rows(2) symx(5) order(1 "Communist" 3 "Green" 2 "Socialist" 4 "Right" 5 "Far-right"))

graph export "$appendix/FigureD10`l'.pdf", replace
}



********************************************************************************
*                                   APPENDIX                                   *
*                                    Tables                                    *
********************************************************************************

********************************************************************************
* Table E1: Summary statistics: Firm donations in 1993, Sub-sample of candidates who received at least one firm donation
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

preserve 

replace sh_dons_firms=0 if sh_dons_firms==.
keep if yes_donation==1

eststo clear
estpost sum nb_don private_donation_firms_cst dons_firms sh_dons_firms, d
esttab  using "$appendix/TableE1.tex", label replace ///
cells((mean(fmt(%15.2fc %15.0fc %15.2fc  %15.2fc) label("Mean")) ///
p50(fmt(%15.2fc %15.0fc %15.2fc  %15.2fc) label("Median")) /// 
p75(fmt(%15.2fc %15.0fc %15.2fc  %15.2fc) label("p75")) ///
sd(fmt( %15.2fc %15.0fc %15.2fc %15.0fc %15.2fc) label("sd")) ///
count(fmt(%15.0fc) label("N"))))  ///
nonum noobs ///
refcat(yes_donation "\textbf{A. Candidates}", nolabel) ///
coeflabels(nb_don "\# Firm donations" dons_firms "Firm donations (euros/voter)" private_donation_firms_cst "Firm donations (euros)" sh_dons_firms "\% Firm donations in total revenue")

restore


********************************************************************************
* Table E2: Corporate donations at the district level

use "$output/data_cand_8897", clear

ren id_cand cand 
codebook inscrits1 if year==1993

keep if year==1993
gen nb_cand=1
gen nb_with_donations=nb_cand
replace nb_with_donations=. if private_donation_firms==0

tempfile cand_level
save `cand_level', replace

*collapse at district level
collapse (sum)   nb_cand nb_with_donations, by(codegeo inscrits1) 

tempfile cand_circo
save `cand_circo', replace

use "$output/donations_with_id_cand_donor.dta", clear
ren cand id_cand

merge m:1 codegeo using `cand_circo'

quietly : do "$dofiles/_labels.do"

*fixing the two issues from the data
replace montant=20000/2 if montant==. & id_cand==27873  
replace montant=16500 if montant==. & id_cand==27508 

*manual correction + fill 0 if empty in the district
replace montant=0 if montant==.
replace montant=7532 if montant==75322     
replace montant=10000 if montant==100000
replace montant=. if montant==150000   
replace local_donor=0 if local_donor==.

gen montant_cst=montant*.1968454
gen montant_i=montant_cst/inscrits1

gen nb_dons=1

collapse (mean) mean_montant_cst=montant_cst  (sum)  nb_dons sum_montant_cst=montant_cst, by(codegeo inscrits1 nb_cand nb_with_donations )

est clear

tempfile collapse_dis
save `collapse_dis', replace

estpost summarize inscrits1 nb_cand nb_with_donations nb_dons  mean_montant_cst  sum_montant_cst 

label var inscrits1 "Registered voters"
label var nb_cand "\# Candidates" 
label var nb_with_donations "\# Candidates with firm donations" 
label var nb_dons "\# Firm donations" 
label var mean_montant_cst "Mean firm donations (\euro)" 
label var sum_montant_cst "Total firm donations (\euro)" 

esttab  using "$appendix/TableE2.tex", ///
replace  nonumber noobs label ///
cells("mean(fmt(%15.0fc %15.2fc %15.2fc %15.2fc %15.0fc %15.0fc) label(Mean)) sd(fmt(%15.0fc %15.2fc %15.2fc %15.2fc %15.0fc %15.0fc) label(sd)) min(fmt(%15.0fc) label(Min)) max(fmt(%15.0fc) label(Max)) count(fmt(%15.0fc) label(N))") ///
refcat(inscrits1 "\textbf{Electoral district}" nb_dons "\textbf{Firm donations}", nolabel) 


********************************************************************************
*Table E3: Largest corporate donors in 1993

use "$raw_data/revenues/donations_sectors.dta", clear
replace montant=7532 if montant==75322     
replace montant=10000 if montant==100000
replace montant=. if montant==150000    
gen montant_cst=montant*.1968454

gen x=1

collapse (sum) montant_cst x, by(donor   donor_id)
gsort -montant_cst				
								
gsort -montant_cst
gen rank=_n
keep if rank<21 
keep    x montant_cst  donor_id donor
format montant_cst %15.0fc
	
********************************************************************************
* Table E4: Summary statistics by sector of activity

use "$output/donations_with_id_cand_donor.dta", clear
gen montant_cst=montant*.1968454

collapse (mean) meanmontant=montant_cst  (sum) summontant=montant_cst , by(donor_id sector )

replace sector="\textbf{Agriculture}" if sector=="agriculture"
replace sector="\textbf{Construction}" if sector=="construction"
replace sector="\textbf{Culture}" if sector=="culture"
replace sector="\textbf{Economy-Finance}" if sector=="economy/fin"
replace sector="\textbf{Environment-Energy}" if sector=="environment-energy"
replace sector="\textbf{Travel}" if sector=="foreign/travel"
replace sector="\textbf{Health}" if sector=="health"
replace sector="\textbf{Industry}" if sector=="industry"
replace sector="\textbf{Justice/Legal}" if sector=="justice"
replace sector="\textbf{Retail}" if sector=="pme/retail"
replace sector="\textbf{NGOs}" if sector=="public/political"
replace sector="\textbf{Sport}" if sector=="sports/asso"
replace sector="\textbf{Unknown}" if sector=="unknown"

est clear

label var meanmontant "Mean donation"
label var summontant "Total donations"

estpost tabstat meanmontant  summontant, by(sector) ///
statistics(mean sd min max N) columns(statistics) listwise

esttab using "$appendix/TableE4.tex", ///
replace  nonumber noobs label ///
cells("mean(fmt(%15.0fc) label(Mean)) sd(fmt(%15.0fc) label(sd)) min(fmt(%15.0fc) label(Min)) max(fmt(%15.0fc) label(Max)) count(fmt(%15.0fc) label(N))") 


********************************************************************************		
* Table E7: Summary stats for policy topics
est clear 

use "$output/data_cand_8897", clear

keep if year>1988

drop *2_prob1
estpost  summarize *_prob1 
esttab using "$appendix/TableE7.tex", ///
replace  nonumber noobs label style(tex) ///
cells(( mean(fmt(%15.2fc) label("Mean")) ///
sd(fmt(%15.2fc) label("sd"))))  ///
refcat(agriculture_prob1 "\textbf{Topic}", nolabel) ///
coeflabels(agriculture_prob1 "Agriculture" construction_prob1 "Construction and amenities" culture_prob1 "Culture" defense_prob1 "Military and defense" economy_prob1 "Economy" education_prob1 "Education" employment_prob1 "Employment" environment_prob1 "Environment" europe_prob1 "European policy" foreign_prob1 "Foreign policy" health_prob1 "Health" industry_prob1 "Industry" interior_prob1 "Homeland security" justice_prob1 "Justice" pme_prob1 "Retail" public_prob1 "Public administration" sport_prob1 "Sport and entertainment") 



********************************************************************************
* Table E8: Summary statistics by sector of activity

use "$raw_data/controls/districts_controls.dta", clear

global census="pasdedip1990  sup1990 agri1990 ouvr1990  pop_65_plus1990 pop_25_341990 "

est clear

quietly : do "$dofiles/_labels.do"

estpost sum  nb_villes cheflieureg pure_urban	///		
$census ///
CC_produits_fonctio1993_cst CC_charges_fonctio1993_cst    ///
DADS_nbestab1993 DADS_nbworker1993 DADS_sumwage1993 DADS_share_top11993  ///
CC_produits_fonctio1997_cst   CC_charges_fonctio1997_cst ///
DADS_nbestab1997 DADS_nbworker1997 DADS_sumwage1997 DADS_share_top11997 

label var pasdedip1990 "No diploma"
label var sup1990 "Higher education"
label var agri1990 "Agriculture"
label var  ouvr1990 "Blue-collar worker"
label var pop_65_plus1990 "65+ years old"
label var pop_25_341990 "25-34 years old"
label var CC_produits_fonctio1993_cst "District municipalities revenue"
label var CC_charges_fonctio1993_cst "District municipalities expenditure"
label var CC_produits_fonctio1997_cst "District municipalities revenue"
label var CC_charges_fonctio1997_cst "District municipalities expenditure"
label var DADS_nbestab1993 "Number of firms"
label var DADS_nbestab1997 "Number of firms"
label var DADS_nbworker1993 "Mean number of employees per municipality"
label var DADS_nbworker1997 "Mean number of employees per municipality"
label var DADS_sumwage1993 "Total payroll (in thousand euros)"
label var DADS_sumwage1997 "Total payroll (in thousand euros)"
label var DADS_share_top11993 "\% employees in top 1\%"
label var DADS_share_top11997 "\% employees in top 1\%"

eststo : esttab using "$appendix/TableE8.tex", label replace ///
cells("mean(fmt(%15.2fc  %15.2fc  %15.2fc  %15.0fc %15.0fc %15.0fc %15.0fc %15.0fc %15.0fc  %15.0fc  %15.0fc  %15.2fc  %15.2fc  %15.0fc   %15.2fc  %15.0fc %15.0fc  %15.2fc   %15.2fc   %15.0fc  %15.2fc) label(Mean))  sd(fmt(%15.2fc  %15.2fc  %15.2fc  %15.0fc %15.0fc %15.0fc %15.0fc %15.0fc %15.0fc  %15.0fc %15.0fc %15.2fc  %15.2fc  %15.0fc   %15.2fc  %15.0fc %15.0fc  %15.2fc   %15.2fc   %15.0fc  %15.2fc) label(sd)) min(fmt(%15.0fc) label(Min)) max(fmt(%15.0fc) label(Max)) count(fmt(%15.0fc) label(N))") ///
style(tex) nonum noobs ///
refcat(pasdedip1990 "\textbf{Census 1990}" CC_produits_fonctio1993_cst "\textbf{Covariates 1993 }" CC_produits_fonctio1997_cst "\textbf{Covariates 1997}" , nolabel)


********************************************************************************
*Table E.9: Summary statistics: firm donations in 1993, Sub-sample of candidates who run both in 1993 and 1997

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

preserve 

replace sh_dons_firms=0 if sh_dons_firms==.

keep if rerun_select==1

eststo clear
estpost sum yes_donation nb_don private_donation_firms_cst dons_firms sh_dons_firms, d
esttab  using "$appendix/TableE9.tex", label replace	///
cells((mean(fmt(%15.2fc %15.2fc %15.0fc %15.2fc  %15.2fc) label("Mean")) ///
p50(fmt(%15.2fc %15.2fc %15.0fc %15.2fc  %15.2fc) label("Median")) /// 
p75(fmt(%15.2fc %15.2fc %15.0fc %15.2fc  %15.2fc) label("p75")) ///
sd(fmt( %15.2fc %15.2fc %15.0fc %15.2fc %15.0fc %15.2fc) label("sd")) ///
count(fmt(%15.0fc) label("N"))))  ///
nonum noobs ///
refcat(yes_donation "\textbf{A. Candidates}", nolabel) ///
coeflabels(yes_donation "Firm donations\>0" nb_don "\# Firm donations" dons_firms "Firm donations (euros/voter)" private_donation_firms_cst "Firm donations (euros)" sh_dons_firms "\% Firm donations in total revenue")

restore

exit


