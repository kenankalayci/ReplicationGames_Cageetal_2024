********************************************************************************
*  Figure: Coefficient comparison across robustness specifications
*  (Table 3 outcomes: Local index, Local references, National references)
*
*  This do-file re-estimates the Table 3 specification under seven different
*  robustness configurations and produces a three-panel forest plot.
*
*  Prerequisites: run main_results.do first (to create $temp/analysis)
*
*  Specifications:
*    A. Original              (baseline from main_results.do)
*    B. Winsorized (1-99)     (from main_results_winzor.do)
*    C. No controls           (from alternative controls in report)
*    D. No candidate controls (from alternative controls in report)
*    E. Decomposed mandates   (from alternative controls in report)
*    F. Number of mandates    (from alternative controls in report)
*    G. Positive donors only  (from main_results_positive.do)
********************************************************************************

clear all
set more off

* Ensure required packages are installed
cap ssc install winsor2
cap ssc install coefplot


********************************************************************************
*  Define a program that prepares the sample identically to main_results.do
*  (lines 87-106), then runs reghdfe and stores the estimate.
*
*  Arguments:
*    1 = outcome variable name
*    2 = estimate store name
*    3 = control specification type:
*            "full"    = $cand_cont (original)
*            "none"    = no controls
*            "nocand"  = no candidate controls (only IV + FE)
*            "decomp"  = decompose other_mandate into 3 roles
*            "nbmand"  = replace individual mandates with total count
*    4 = sample restriction: "all" or "positive"
*    5 = winsorize: "no" or "yes"
********************************************************************************

capture program drop run_spec
program define run_spec, eclass
    args outcome storename controltype sampletype winsorize

    use "$temp/analysis", clear

    * --- Winsorize if requested ---
    if "`winsorize'" == "yes" {
        winsor2 std_dons_firms, cuts(1 99) replace
    }

    * --- Sample selection (same as main_results.do lines 87) ---
    keep if `outcome' != . & sample_rest == 1 & sample_did == 1

    * --- Restrict to positive-donation candidates if requested ---
    *     (same logic as main_results_positive.do lines 88-97)
    if "`sampletype'" == "positive" {
        preserve
            keep if year == 1993
            keep if std_dons_firms > 0
            keep id_indiv
            tempfile donors
            save `donors'
        restore
        merge m:1 id_indiv using `donors', keep(3) nogen
    }

    * --- Handle controls based on specification type ---

    if "`controltype'" == "full" {
        * Original: missing indicators + candidate controls
        foreach x in $cand_cont {
            gen disp_`x' = `x' == .
            replace `x' = -1 if `x' == .
        }
        local controls "disp_* $cand_cont"
    }
    else if "`controltype'" == "none" {
        * No controls at all (only FE + instrument)
        local controls ""
    }
    else if "`controltype'" == "nocand" {
        * No candidate-level controls (keep instrument but drop cand_cont)
        local controls ""
    }
    else if "`controltype'" == "decomp" {
        * Decompose other_mandate into three constituent roles
        * (other_mandate = conseiller_departemental + senateur + depute_europeen;
        *  see step3_merge_w_candidates.do line 482)
        foreach x in female rerun Dincumbent mayor conseiller_departemental senateur depute_europeen {
            capture gen disp_`x' = `x' == .
            capture replace `x' = -1 if `x' == .
        }
        local controls "disp_* female rerun Dincumbent mayor conseiller_departemental senateur depute_europeen"
    }
    else if "`controltype'" == "nbmand" {
        * Replace individual mandate dummies with a single count
        gen nb_mandates = mayor + other_mandate + Dincumbent
        foreach x in female rerun nb_mandates {
            capture gen disp_`x' = `x' == .
            capture replace `x' = -1 if `x' == .
        }
        local controls "disp_* female rerun nb_mandates"
    }

    * --- Fixed effects preparation (same as main_results.do lines 96-106) ---
    byso id_indiv: gen temp = _N
    keep if temp > 1
    drop temp

    byso id_yearparty: gen temp = _N
    replace id_yearparty = 10000 + year if temp < 2
    drop temp

    byso id_yearparty: gen temp = _N
    keep if temp >= 2
    drop temp

    * --- Redefine as loss (same as main_results.do line 109) ---
    replace std_dons_firms = -std_dons_firms
    label var std_dons_firms "Firm donations (loss)"

    * --- Estimate ---
    reghdfe `outcome' `controls' inter_pred_dons_firms std_dons_firms, ///
        absorb(id_indiv id_yearparty) cluster(id_circo)

    * Rename the coefficient in the stored estimate matrix so coefplot
    * displays the specification name rather than the variable name.
    tempname b V
    matrix `b' = e(b)
    matrix `V' = e(V)

    * Find the column index of std_dons_firms and rename it
    local cnames : colnames `b'
    local newcnames ""
    foreach c of local cnames {
        if "`c'" == "std_dons_firms" {
            local newcnames "`newcnames' `storename'"
        }
        else {
            local newcnames "`newcnames' `c'"
        }
    }
    matrix colnames `b' = `newcnames'
    matrix colnames `V' = `newcnames'
    matrix rownames `V' = `newcnames'

    ereturn repost b = `b' V = `V', rename
    estimates store `storename'
end


********************************************************************************
*  Run all 7 specifications x 3 outcomes = 21 regressions
********************************************************************************

* Outcome variable names (same as main_results.do line 79)
local outcomes "ratio_local1_std sh_local1 sh_nat1"
local outnames "localidx localref natref"

local spec_labels `" "A. Original" "B. Winsorized" "C. No controls" "D. No cand. controls" "E. Decomp. mandates" "F. Num. mandates" "G. Positive donors" "'

local i = 0
foreach out of local outcomes {
    local ++i
    local oname : word `i' of `outnames'

    run_spec `out' `oname'_A   full    all      no
    run_spec `out' `oname'_B   full    all      yes
    run_spec `out' `oname'_C   none    all      no
    run_spec `out' `oname'_D   nocand  all      no
    run_spec `out' `oname'_E   decomp  all      no
    run_spec `out' `oname'_F   nbmand  all      no
    run_spec `out' `oname'_G   full    positive no
}


********************************************************************************
*  Generate the three-panel coefficient plot
*
*  Each stored estimate has its coefficient renamed from std_dons_firms
*  to the estimate name (e.g. localidx_A). We use coeflabels() globally
*  to map these to readable specification labels on the y-axis.
********************************************************************************

* Common label mapping (used for all three panels)
local lbl_A `""A. Original""'
local lbl_B `""B. Winsorized""'
local lbl_C `""C. No controls""'
local lbl_D `""D. No cand. controls""'
local lbl_E `""E. Decomp. mandates""'
local lbl_F `""F. Num. mandates""'
local lbl_G `""G. Positive donors""'

* --- Panel 1: Local index ---
coefplot ///
    localidx_A localidx_B localidx_C localidx_D ///
    localidx_E localidx_F localidx_G, ///
    keep(localidx_A localidx_B localidx_C localidx_D ///
         localidx_E localidx_F localidx_G) ///
    coeflabels(localidx_A = `lbl_A' localidx_B = `lbl_B' ///
               localidx_C = `lbl_C' localidx_D = `lbl_D' ///
               localidx_E = `lbl_E' localidx_F = `lbl_F' ///
               localidx_G = `lbl_G') ///
    xline(0, lcolor(cranberry) lpattern(dash)) ///
    xtitle("Coefficient", size(small)) ///
    title("Local index", size(medium)) ///
    legend(off) ///
    msymbol(D) mcolor(navy) msize(small) ///
    ciopts(lcolor(navy) lwidth(medthick)) ///
    ylabel(, labsize(vsmall) angle(0) nogrid) ///
    graphregion(color(white)) ///
    name(g_localidx, replace)

* --- Panel 2: Local references ---
coefplot ///
    localref_A localref_B localref_C localref_D ///
    localref_E localref_F localref_G, ///
    keep(localref_A localref_B localref_C localref_D ///
         localref_E localref_F localref_G) ///
    coeflabels(localref_A = `lbl_A' localref_B = `lbl_B' ///
               localref_C = `lbl_C' localref_D = `lbl_D' ///
               localref_E = `lbl_E' localref_F = `lbl_F' ///
               localref_G = `lbl_G') ///
    xline(0, lcolor(cranberry) lpattern(dash)) ///
    xtitle("Coefficient", size(small)) ///
    title("Local references", size(medium)) ///
    legend(off) ///
    msymbol(D) mcolor(navy) msize(small) ///
    ciopts(lcolor(navy) lwidth(medthick)) ///
    ylabel(, labsize(vsmall) angle(0) nogrid) ///
    graphregion(color(white)) ///
    name(g_localref, replace)

* --- Panel 3: National references ---
coefplot ///
    natref_A natref_B natref_C natref_D ///
    natref_E natref_F natref_G, ///
    keep(natref_A natref_B natref_C natref_D ///
         natref_E natref_F natref_G) ///
    coeflabels(natref_A = `lbl_A' natref_B = `lbl_B' ///
               natref_C = `lbl_C' natref_D = `lbl_D' ///
               natref_E = `lbl_E' natref_F = `lbl_F' ///
               natref_G = `lbl_G') ///
    xline(0, lcolor(cranberry) lpattern(dash)) ///
    xtitle("Coefficient", size(small)) ///
    title("National references", size(medium)) ///
    legend(off) ///
    msymbol(D) mcolor(navy) msize(small) ///
    ciopts(lcolor(navy) lwidth(medthick)) ///
    ylabel(, labsize(vsmall) angle(0) nogrid) ///
    graphregion(color(white)) ///
    name(g_natref, replace)

* --- Combine into one figure ---
graph combine g_localidx g_localref g_natref, ///
    rows(1) ///
    title("Robustness of Table 3 Results Across Specifications", size(medsmall)) ///
    graphregion(color(white)) ///
    xsize(14) ysize(5)

graph export "$main/Figure_robustness.pdf", replace

* Clean up
graph drop g_localidx g_localref g_natref
