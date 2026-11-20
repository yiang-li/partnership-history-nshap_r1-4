# NSHAP Rounds 1-4 Partnership History

This repository contains code that harmonizes the National Social Life, Health, and Aging Project (NSHAP) Rounds 1-4 marital and cohabitation reports into a single long-format partnership history file, one row per relationship, along with the scripts that produce the tables and figures documenting that file.

The NSHAP data are restricted-access and cannot be redistributed. Researchers may obtain the public and restricted releases through NACDA/ICPSR at https://www.icpsr.umich.edu/web/pages/ICPSR/access/restricted/index.html. The Round 4 raw file used here was provided by the NSHAP study team.

Once data files are obtained, users should:
1. Set up the two folder levels shown below. The project folder holds `codes/` together with everything the scripts produce.

   ```
   project_parent/
     data/                      raw NSHAP releases
     tmp/                       working files rebuilt on every run
     nshapr1234_part_history/   the project folder
       codes/
       output/
       tables_figures/
   ```

2. Place the source files in `data/`: nshap_w1_core.dta, nshap_w1_maritalhistory.dta, nshap_w2_core.dta, nshap_w2_partnerupdate.dta, nshap_w2_partnerhistory.dta, nshap_w3_core.dta, nshapw3_main.dta, nshap_r4_core.dta, 2024_03_20_nshap_r4_raw_plus_lbq.dta, and r4_newrespondents_gender_ethgrp_educ.dta.
3. Download scripts to codes folder from Github Repo: https://github.com/yiang-li/partnership-history-nshap_r1-4

## The cleaned partnership file

`output/nshap_par_hisr1234.rds` contains 11,409 rows for 6,618 respondents: 11,268 marriage or cohabitation rows and 141 respondent-only rows for people with no reported marriage or cohabitation. The saved file has 31 columns:

| Variable | Meaning |
|---|---|
| `su_id` | Respondent identifier |
| `scid` | Derived partnership order within respondent |
| `round` | Round of the latest update contributing to the row |
| `mstatus` | Marital status of the relationship on that row |
| `marbeginm`, `marbeginy` | Reported marriage start month and year |
| `marendm`, `marendy` | Reported marriage end month and year |
| `livebfmr` | Lived together before marriage |
| `cobeginm`, `cobeginy` | Reported cohabitation start month and year |
| `coendm`, `coendy` | Reported cohabitation end month and year |
| `fstsexy` | Reported year of first sex with that partner |
| `beginy_est` | Earliest reported partnership year, with first-sex year as a fallback |
| `pgender` | Partner gender |
| `page` | Partner age |
| `pagerel` | Partner age relative to the respondent |
| `has_mar` | Row contains a marriage |
| `has_coh` | Row contains cohabitation, including premarital cohabitation |
| `mar_ended` | Marriage had ended by the row's latest update |
| `coh_ended` | Cohabitation had ended or transitioned to marriage |
| `end_reason` | Relationship ending category |
| `status_implied` | Row implied by latest marital status, not directly reported |
| `int_month`, `int_year` | Interview date of the latest update contributing to the row |
| `maritlst_latest` | Respondent's latest reported marital status |
| `part_history_14` | Final partnership history in 14 categories |
| `part_history_11` | Final partnership history in 11 categories |
| `part_history_7` | Final partnership history in 7 categories |
| `n_othersex_rounds` | Other sex partners reported, summed over rounds attended |

## Cleaning rules

Round 1 contains the initial retrospective marriage and cohabitation records. Round 1 skips the marriage end month and year for separated respondents, and for divorced respondents unless they now live with a partner, so the dissolution date is the month and year the respondent stopped living with their last spouse. Those dates are read as the marriage end for divorced and separated rows, so on those rows the marriage end date is when the couple stopped living together, not the date of a legal divorce. Partner gender is attached only to the current Round 1 union because the core file does not provide historical partner details for every relationship. Where a former cohabiting partner is still living, the partner cannot have died during that union, so the death indicator is set to no.

The Round 2 partner-update file changes the identified current Round 1 marriage or cohabitation, including a cohabitation that became a marriage, and a cohabitation whose partner the respondent married and then lost. For an ended Round 1 union, partner gender comes from the Round 1 partner. The Round 2 current-partner gender is used only for a relationship still intact in Round 2. In the Round 2 partner-history file, roster order, marriage status, and current-versus-ended status link marriage rows to existing union records, filling only previously missing partner details. Partner-history rows that repeat a current cohabitation already in the history are merged into it instead of being added as new rows. A partner-history row with a missing marital status is counted as a marriage when its roster position falls within the reported number of marriages.

In Rounds 3 and 4, prior-partner fields first close or convert the most recent eligible intact record. The marriage and cohabitation loops are reshaped separately because equal loop numbers do not identify the same relationship. In the marriage loop, the month and year the couple started living together mark the start of premarital cohabitation. In this harmonization, we corrected that due to raw data labeling error confirmed by the NORC field team, the marriage loop's end month and end year are the wedding date, not the end of the marriage, despite their names and their delivered variable labels: the questionnaire asks them "In what month and year did your marriage to NAME begin?", they are asked of every loop marriage, and the endings instead follow the question on whether the marriage ended in divorce or widowhood, in the month and year the spouse died for a widowhood and the month and year the couple stopped living together for a divorce or separation. The current spouse's own premarital cohabitation is reported separately again, in the question on living together before marriage and its month and year. Refused and don't-know ending responses in either round are retained long enough to establish an ended relationship with an unknown reason before negative codes become missing. Conversion of a cohabitation to marriage requires an explicit report that the respondent lived with that partner before marriage. An ending report that cannot be linked to an intact marriage is not added as another marriage when the respondent already has an ended marriage with the same end year. Current partnerships are reconciled against the core marital-status report at the end of each round. A loop partner recorded on the household roster line of the current spouse or partner is treated as that same person, not as another relationship. A repeated marriage loop is therefore merged into the existing current marriage, and a cohabitation loop on that line is dropped, because the current partner's own cohabitation dates come from the dedicated current-partner questions. The gender of the most recent interview's spouse or partner applies to every record the prior-partner update creates or closes, and the current partner's household roster age fills the partner age when the loop skipped the age question. Where the latest reported status is widowed but no relationship ended in widowhood, a relationship implied by that status is appended and flagged as implied. A separated or divorced mismatch is treated as a status change on a marriage already in the history, not as a new marriage.

## Cleaning assumptions and known limitations

- No relationship ID follows the same partner through all four rounds. A later update is linked to the most recent earlier relationship that matches the reported relationship type and current-versus-ended state. Four Round 3 partner cases remain identity-ambiguous after all status and date checks.
- Round 3 and Round 4 capture only five detailed marriage and five detailed cohabitation slots. The reported number of marriages and of other live-in romantic relationships can indicate evidence beyond those slots, but a count alone does not identify a partner, date, or relationship and is not converted to a record.
- A marriage reported without a wedding date remains a marriage with an unknown start date. A cohabitation date is not treated as a wedding date.
- A marriage with a reported begin, no reported marriage end, and cohabitation end dates is coded as separated: the couple stopped living together without a reported divorce, separation, or widowhood.
- We treat an unlinked marriage-ending report as the same ending when end years match, which avoids double counting but cannot prove the reports concern the same marriage. Rounds that repeat one marriage with identical dates are collapsed to the fuller row, and a wedding date reported both as intact and as ended is treated as one marriage that ended.
- A reported end date after the date of the interview is removed, but the report that the relationship ended is retained.
- The estimated partnership begin year may fall back on the first-sex year to help order relationships when no partnership year is reported.
