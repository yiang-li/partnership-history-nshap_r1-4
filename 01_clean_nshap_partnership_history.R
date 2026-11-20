# Author: Yiang Li
# Builds on an earlier draft by Elior Waskow.
# Date: 2026-07-29
# Clean NSHAP Rounds 1-4 partnership histories into one long file.
library(dplyr)
library(haven)
library(tidyr)

#### Round 1 ####
# Partner gender is available only for the current Round 1 partnership.
r1_partnerships <- read_dta("../data/nshap_w1_maritalhistory.dta") |>
  zap_labels() |>
  rename(partner_died = cdielive) |>
  # Round 1 skips marend* for separated respondents, and for divorced ones
  # unless they live with a partner now. Use the date they stopped living
  # with the spouse instead.
  mutate(
    # Former partner is still alive, so did not die during the union.
    partner_died = if_else(coalesce(cohabliv == 1, FALSE),
                           coalesce(partner_died, 0), partner_died),
    marendm = if_else(coalesce(mstatus %in% 2:3, FALSE),
                      coalesce(marendm, coendm), marendm),
    marendy = if_else(coalesce(mstatus %in% 2:3, FALSE),
                      coalesce(marendy, coendy), marendy),
    coendm = if_else(coalesce(mstatus %in% 2:3, FALSE) &
                       !coalesce(livebfmr == 1, FALSE) &
                       is.na(cobeginm) & is.na(cobeginy), NA, coendm),
    coendy = if_else(coalesce(mstatus %in% 2:3, FALSE) &
                       !coalesce(livebfmr == 1, FALSE) &
                       is.na(cobeginm) & is.na(cobeginy), NA, coendy)
  ) |>
  group_by(su_id) |>
  mutate(
    round = 1,
    n_current_partners = sum(coalesce(mstatus == 1, FALSE) |
                            coalesce(costatus == 1, FALSE))
  ) |>
  ungroup() |>
  left_join(
    read_dta("../data/nshap_w1_core.dta") |>
      zap_labels() |>
      transmute(
        su_id,
        int_month = as.integer(int_start) %% 12 + 1,
        int_year = 1960 + as.integer(int_start) %/% 12,
        current_status = as.numeric(maritlst), pgender_1
      ),
    by = "su_id"
  ) |>
  mutate(
    pgender = if_else(
      (n_current_partners == 1 &
         (coalesce(mstatus == 1, FALSE) |
            coalesce(costatus == 1, FALSE))) |
        (n_current_partners > 1 &
           ((current_status == 1 & coalesce(mstatus == 1, FALSE)) |
              (current_status == 2 & coalesce(costatus == 1, FALSE)))),
      pgender_1, NA
    ),
    marriage_record = if_else(
      mstatus %in% 1:4 | !is.na(marbeginm) | !is.na(marbeginy) |
        !is.na(marendm) | !is.na(marendy), 1, NA
    ),
    marriage_ended_record = if_else(mstatus %in% 2:4, 1, NA),
    cohabitation_record = if_else(
      costatus == 1 | !is.na(cobeginm) | !is.na(cobeginy) |
        (is.na(marriage_record) & (!is.na(coendm) | !is.na(coendy))) |
        (marriage_record == 1 & livebfmr == 1), 1, NA
    ),
    cohabitation_current_record = if_else(costatus == 1, 1, NA),
    cohabitation_ended_record = if_else(
      costatus == 0 & (!is.na(cobeginm) | !is.na(cobeginy) |
                         !is.na(coendm) | !is.na(coendy)),
      1, NA
    ),
    row_id = row_number()
  ) |>
  select(-n_current_partners, -current_status, -pgender_1, -cohabliv)

#### Round 2 ####
r2_interview <- read_dta("../data/nshap_w2_core.dta") |>
  select(su_id, int_start, pgender) |>
  zap_labels() |>
  transmute(su_id,
            int_month = as.integer(int_start) %% 12 + 1,
            int_year = 1960 + as.integer(int_start) %/% 12,
            current_pgender = as.numeric(pgender))

# Round 2 status and date updates.
r2_update <- read_dta("../data/nshap_w2_partnerupdate.dta") |>
  select(su_id, mstatus_w1, mstatus, costatus, marbeginm,
         marbeginy, marendm, marendy, livebfmr,
         cobeginm, cobeginy, coendm, coendy, partner_died = cdielive,
         deathm, deathy) |>
  zap_labels() |>
  # Use the partner's death date as the end date of that union.
  mutate(
    marendm = if_else(coalesce(mstatus == 4, FALSE),
                      coalesce(marendm, deathm), marendm),
    marendy = if_else(coalesce(mstatus == 4, FALSE),
                      coalesce(marendy, deathy), marendy),
    coendm = if_else(coalesce(mstatus == 5 | costatus == 0, FALSE),
                     coalesce(coendm, deathm), coendm),
    coendy = if_else(coalesce(mstatus == 5 | costatus == 0, FALSE),
                     coalesce(coendy, deathy), coendy),
    round = 2
  ) |>
  select(-deathm, -deathy) |>
  left_join(r2_interview, by = "su_id") |>
  left_join(
    read_dta("../data/nshap_w1_core.dta", col_select = c(su_id, pgender_1)) |>
      zap_labels() |>
      transmute(su_id, prior_pgender = as.numeric(pgender_1)),
    by = "su_id"
  ) |>
  mutate(
    pgender = if_else(coalesce(mstatus == 1 | costatus == 1, FALSE),
                      current_pgender, prior_pgender)
  )

# Update the current Round 1 marriage.
r2_marriage_updates <- r1_partnerships |>
  filter(mstatus == 1,
         su_id %in% filter(r2_update,
                           mstatus_w1 == 1, mstatus %in% 1:4)$su_id) |>
  group_by(su_id) |>
  slice_max(order_by = coalesce(marbeginy, 0), n = 1,
            with_ties = FALSE) |>
  ungroup() |>
  select(su_id, row_id) |>
  inner_join(r2_update |> filter(mstatus_w1 == 1, mstatus %in% 1:4),
             by = "su_id")

# Update or convert the current Round 1 cohabitation.
r2_cohabitation_updates <- r1_partnerships |>
  filter(cohabitation_current_record == 1,
         su_id %in% filter(
           r2_update,
           mstatus_w1 %in% c(2, 3) &
             (mstatus == 1 | costatus %in% c(0, 1))
         )$su_id) |>
  group_by(su_id) |>
  slice_max(order_by = coalesce(cobeginy, 0), n = 1,
            with_ties = FALSE) |>
  ungroup() |>
  select(su_id, row_id) |>
  inner_join(
    r2_update |>
      filter(mstatus_w1 %in% c(2, 3) &
               (mstatus == 1 | costatus %in% c(0, 1))),
    by = "su_id"
  )

# Additional Round 2 partners.
r2_partnerships <- read_dta("../data/nshap_w2_partnerhistory.dta") |>
  select(su_id, scpid_w2, nummarr, mstatus, costatus,
         pgender, page, poldyoung,
         marbeginm, marbeginy, marendm, marendy, livebfmr,
         cobeginm, cobeginy, coendm, coendy, partner_died = cdielive,
         fstsexy) |>
  group_by(su_id) |>
  mutate(
    marriage_roster_record = coalesce(
      is_tagged_na(mstatus) & na_tag(mstatus) %in% c("a", "b") &
        row_number(scpid_w2) <= as.numeric(first(nummarr)) +
          any(mstatus == 1, na.rm = TRUE),
      FALSE
    )
  ) |>
  ungroup() |>
  zap_labels() |>
  mutate(
    known_marriage = marriage_roster_record |
      coalesce(mstatus %in% 1:4, FALSE) |
      !is.na(marbeginm) | !is.na(marbeginy) |
      !is.na(marendm) | !is.na(marendy),
    known_cohabitation = coalesce(costatus %in% c(0, 1), FALSE) |
      !is.na(cobeginm) | !is.na(cobeginy) |
      !is.na(coendm) | !is.na(coendy) |
      (known_marriage & coalesce(livebfmr == 1, FALSE))
  ) |>
  mutate(pagerel = if_else(poldyoung %in% 1:3, poldyoung, NA)) |>
  select(-poldyoung) |>
  rename(scid = scpid_w2) |>
  mutate(round = 2) |>
  left_join(r2_interview, by = "su_id") |>
  mutate(
    current_cohabitation_repeat =
      coalesce(mstatus == 5 & costatus == 1, FALSE) &
      su_id %in% filter(r2_update, mstatus_w1 %in% c(2, 3),
                        mstatus == 5, costatus == 1)$su_id,
    # Partners not already in the history are added as new rows.
    adds_new_row = !current_cohabitation_repeat &
      ((!(su_id %in% union(r1_partnerships$su_id, r2_update$su_id)) &
          (known_marriage | known_cohabitation)) |
         !is.na(marbeginm) | !is.na(marbeginy) |
         !is.na(marendm) | !is.na(marendy) |
         !is.na(cobeginm) | !is.na(cobeginy) |
         !is.na(coendm) | !is.na(coendy) |
         costatus %in% c(0, 1))
  )

# Recode Round 1-2 yes/no answers to 1 = yes, 2 = no.
# Round 2 reports that match no Round 1 row are appended.
partnership_history <- r1_partnerships |>
  left_join(
    r2_marriage_updates |>
      transmute(
        row_id, updated = TRUE, r2_mstatus = mstatus,
        r2_marbeginm = marbeginm, r2_marbeginy = marbeginy,
        r2_marendm = marendm, r2_marendy = marendy,
        r2_livebfmr = livebfmr,
        r2_cobeginm = cobeginm, r2_cobeginy = cobeginy,
        r2_coendm = coendm, r2_coendy = coendy,
        r2_partner_died = partner_died, r2_pgender = pgender,
        r2_int_month = int_month, r2_int_year = int_year
      ),
    by = "row_id"
  ) |>
  mutate(
    updated = coalesce(updated, FALSE),
    marbeginm = coalesce(marbeginm, r2_marbeginm),
    marbeginy = coalesce(marbeginy, r2_marbeginy),
    marendm = coalesce(marendm, r2_marendm),
    marendy = coalesce(marendy, r2_marendy),
    livebfmr = coalesce(livebfmr, r2_livebfmr),
    cobeginm = coalesce(cobeginm, r2_cobeginm),
    cobeginy = coalesce(cobeginy, r2_cobeginy),
    coendm = coalesce(coendm, r2_coendm),
    coendy = coalesce(coendy, r2_coendy),
    partner_died = coalesce(partner_died, r2_partner_died),
    pgender = coalesce(pgender, r2_pgender),
    mstatus = coalesce(r2_mstatus, mstatus),
    marriage_ended_record = if_else(r2_mstatus %in% 2:4, 1,
                                    marriage_ended_record),
    round = if_else(updated, 2, round),
    int_month = if_else(updated, r2_int_month, int_month),
    int_year = if_else(updated, r2_int_year, int_year)
  ) |>
  select(-updated, -starts_with("r2_")) |>
  left_join(
    r2_cohabitation_updates |>
      transmute(
        row_id, updated = TRUE, r2_mstatus = mstatus,
        converted = coalesce(mstatus %in% c(1, 4), FALSE),
        r2_costatus = costatus,
        r2_marbeginm = marbeginm, r2_marbeginy = marbeginy,
        r2_marendm = marendm, r2_marendy = marendy,
        r2_livebfmr = livebfmr, r2_cobeginm = cobeginm,
        r2_cobeginy = cobeginy, r2_coendm = coendm,
        r2_coendy = coendy, r2_partner_died = partner_died,
        r2_pgender = pgender, r2_int_month = int_month,
        r2_int_year = int_year
      ),
    by = "row_id"
  ) |>
  mutate(
    updated = coalesce(updated, FALSE),
    converted = coalesce(converted, FALSE),
    marbeginm = if_else(converted, coalesce(marbeginm, r2_marbeginm),
                        marbeginm),
    marbeginy = if_else(converted, coalesce(marbeginy, r2_marbeginy),
                        marbeginy),
    marendm = if_else(converted, coalesce(marendm, r2_marendm), marendm),
    marendy = if_else(converted, coalesce(marendy, r2_marendy), marendy),
    livebfmr = if_else(converted, coalesce(livebfmr, r2_livebfmr), livebfmr),
    cobeginm = coalesce(cobeginm, r2_cobeginm),
    cobeginy = coalesce(cobeginy, r2_cobeginy),
    coendm = coalesce(coendm, r2_coendm),
    coendy = coalesce(coendy, r2_coendy),
    partner_died = coalesce(partner_died, r2_partner_died),
    pgender = coalesce(pgender, r2_pgender),
    mstatus = if_else(converted, r2_mstatus, mstatus),
    marriage_record = if_else(converted, 1, marriage_record),
    cohabitation_record = if_else(updated, 1, cohabitation_record),
    cohabitation_current_record = case_when(
      converted ~ NA,
      updated & r2_costatus == 1 ~ 1,
      updated & r2_costatus == 0 ~ NA,
      TRUE ~ cohabitation_current_record
    ),
    cohabitation_ended_record = if_else(
      converted | (updated & r2_costatus == 0), 1, cohabitation_ended_record
    ),
    round = if_else(updated, 2, round),
    int_month = if_else(updated, r2_int_month, int_month),
    int_year = if_else(updated, r2_int_year, int_year)
  ) |>
  select(-updated, -converted, -starts_with("r2_"),
         -row_id, -costatus) |>
  bind_rows(
    r2_update |>
      anti_join(bind_rows(select(r2_marriage_updates, su_id),
                          select(r2_cohabitation_updates, su_id)),
                by = "su_id") |>
      filter(
        (mstatus_w1 %in% c(2, 3) & mstatus == 1) |
          (mstatus_w1 %in% c(2, 3) & costatus == 1) |
          (mstatus_w1 %in% c(2, 3) & costatus == 0 &
             (!is.na(cobeginm) | !is.na(cobeginy) |
                !is.na(coendm) | !is.na(coendy))) |
          !is.na(marbeginm) | !is.na(marbeginy) |
          !is.na(marendm) | !is.na(marendy)
      ) |>
      mutate(
        marriage_record = if_else(
          mstatus == 1 | !is.na(marbeginm) | !is.na(marbeginy) |
            !is.na(marendm) | !is.na(marendy), 1, NA
        ),
        marriage_ended_record = if_else(
          marriage_record == 1 & mstatus %in% 2:4, 1, NA
        ),
        cohabitation_record = if_else(
          costatus == 1 | !is.na(cobeginm) | !is.na(cobeginy) |
            (is.na(marriage_record) & (!is.na(coendm) | !is.na(coendy))) |
            (marriage_record == 1 & livebfmr == 1), 1, NA
        ),
        cohabitation_current_record = if_else(costatus == 1, 1, NA),
        cohabitation_ended_record = if_else(costatus == 0, 1, NA),
        mstatus = if_else(is.na(marriage_record) &
                            cohabitation_record == 1, 5, mstatus)
      ) |>
      select(-mstatus_w1, -costatus, -current_pgender, -prior_pgender),
    r2_partnerships |>
      filter(adds_new_row) |>
      mutate(
        marriage_record = if_else(
          marriage_roster_record | mstatus %in% 1:4 |
            !is.na(marbeginm) | !is.na(marbeginy) |
            !is.na(marendm) | !is.na(marendy),
          1, NA
        ),
        marriage_ended_record = if_else(
          marriage_roster_record | mstatus %in% 2:4, 1, NA
        ),
        cohabitation_record = if_else(known_cohabitation, 1, NA),
        cohabitation_current_record = if_else(costatus == 1, 1, NA),
        cohabitation_ended_record = if_else(
          (known_cohabitation & costatus == 0) |
            !is.na(coendm) | !is.na(coendy),
          1, NA
        )
      ) |>
      select(-nummarr, -costatus,
             -marriage_roster_record, -known_marriage, -known_cohabitation,
             -current_pgender, -current_cohabitation_repeat, -adds_new_row)
  ) |>
  mutate(livebfmr = if_else(livebfmr == 0, 2, livebfmr),
         partner_died = if_else(partner_died == 0, 2, partner_died))

# Add details from duplicate reports of the current Round 2 cohabitation.
partnership_history <- partnership_history |>
  mutate(row_id = row_number()) |>
  left_join(
    partnership_history |>
      mutate(row_id = row_number()) |>
      filter(
        su_id %in% filter(r2_partnerships, current_cohabitation_repeat)$su_id,
        coalesce(cohabitation_current_record, 0) == 1,
        !coalesce(cohabitation_ended_record == 1, FALSE)
      ) |>
      group_by(su_id) |>
      slice_max(order_by = round * 10000 + coalesce(cobeginy, 0),
                n = 1, with_ties = FALSE) |>
      ungroup() |>
      select(su_id, row_id) |>
      inner_join(
        r2_partnerships |>
          filter(current_cohabitation_repeat) |>
          transmute(
            su_id, repeated_pgender = pgender, repeated_page = page,
            repeated_pagerel = pagerel, repeated_fstsexy = fstsexy,
            repeated_cobeginm = cobeginm, repeated_cobeginy = cobeginy,
            repeated_coendm = coendm, repeated_coendy = coendy,
            repeated_partner_died = partner_died
          ),
        by = "su_id"
      ) |>
      select(-su_id),
    by = "row_id"
  ) |>
  mutate(
    pgender = coalesce(pgender, repeated_pgender),
    page = coalesce(page, repeated_page),
    pagerel = coalesce(pagerel, repeated_pagerel),
    fstsexy = coalesce(fstsexy, repeated_fstsexy),
    cobeginm = coalesce(cobeginm, repeated_cobeginm),
    cobeginy = coalesce(cobeginy, repeated_cobeginy),
    coendm = coalesce(coendm, repeated_coendm),
    coendy = coalesce(coendy, repeated_coendy),
    partner_died = coalesce(partner_died, repeated_partner_died)
  ) |>
  select(-row_id, -starts_with("repeated_"))


# Add a Round 2 current partnership when the histories lack one.
partnership_history <- bind_rows(
  partnership_history,
  read_dta(
    "../data/nshap_w2_core.dta", col_select = c(su_id, maritlst)
  ) |>
    zap_labels() |>
    transmute(su_id, current_status = as.numeric(maritlst)) |>
    left_join(r2_interview, by = "su_id") |>
    left_join(
      partnership_history |>
        group_by(su_id) |>
        summarise(
          any_intact_marriage = any(
            (!is.na(marbeginy) | !is.na(marbeginm) |
               coalesce(marriage_record, 0) == 1) &
              !coalesce(marriage_ended_record == 1, FALSE) &
              is.na(marendy) & is.na(marendm) &
              (is.na(mstatus) | mstatus == 1),
            na.rm = TRUE
          ),
          any_intact_cohabitation = any(
            (!is.na(cobeginy) | !is.na(cobeginm) |
               coalesce(cohabitation_record, 0) == 1) &
              !coalesce(cohabitation_ended_record == 1, FALSE) &
              (coalesce(cohabitation_current_record, 0) == 1 |
                 (coalesce(marriage_record, 0) != 1 &
                    is.na(coendy) & is.na(coendm))),
            na.rm = TRUE
          ),
          .groups = "drop"
        ),
      by = "su_id"
    ) |>
    filter((current_status == 1 & !coalesce(any_intact_marriage, FALSE)) |
             (current_status == 2 &
                !coalesce(any_intact_cohabitation, FALSE))) |>
    transmute(
      su_id, round = 2,
      marriage_record = if_else(current_status == 1, 1, NA),
      cohabitation_record = if_else(current_status == 2, 1, NA),
      cohabitation_current_record = if_else(current_status == 2, 1, NA),
      mstatus = if_else(current_status == 1, 1, 5),
      pgender = current_pgender, int_month, int_year
    )
)

# Attach Round 2 partner details to the corresponding earlier partnership.
partnership_history <- partnership_history |>
  mutate(row_id = row_number()) |>
  left_join(
    partnership_history |>
      mutate(
        row_id = row_number(),
        relationship_type = case_when(
          coalesce(marriage_record, 0) == 1 |
            !is.na(marbeginm) | !is.na(marbeginy) |
            !is.na(marendm) | !is.na(marendy) ~ "marriage",
          coalesce(cohabitation_record, 0) == 1 |
            !is.na(cobeginm) | !is.na(cobeginy) |
            !is.na(coendm) | !is.na(coendy) ~ "cohabitation"
        ),
        current_relationship = case_when(
          relationship_type == "marriage" ~
            !coalesce(marriage_ended_record == 1, FALSE) & mstatus == 1,
          relationship_type == "cohabitation" ~
            coalesce(cohabitation_current_record == 1, FALSE),
          TRUE ~ FALSE
        )
      ) |>
      filter(!is.na(relationship_type)) |>
      group_by(su_id, relationship_type) |>
      arrange(
        desc(current_relationship),
        desc(coalesce(marbeginy, cobeginy, marendy, coendy, int_year)),
        .by_group = TRUE
      ) |>
      mutate(partner_order = row_number()) |>
      ungroup() |>
      inner_join(
        r2_partnerships |>
          filter(mstatus %in% 1:4 | marriage_roster_record) |>
          mutate(relationship_type = "marriage",
                 detail_current_relationship = coalesce(mstatus == 1, FALSE)) |>
          group_by(su_id, relationship_type) |>
          arrange(scid, .by_group = TRUE) |>
          mutate(partner_order = row_number()) |>
          ungroup() |>
          anti_join(r2_partnerships |>
                      filter(adds_new_row) |>
                      select(su_id, scid),
                    by = c("su_id", "scid")) |>
          filter(!is.na(pgender) | !is.na(page) |
                   !is.na(pagerel) | !is.na(fstsexy)) |>
          transmute(
            su_id, relationship_type, detail_current_relationship,
            partner_order, detail_pgender = pgender, detail_page = page,
            detail_pagerel = pagerel, detail_fstsexy = fstsexy
          ),
        by = c("su_id", "relationship_type", "partner_order")
      ) |>
      filter(current_relationship == detail_current_relationship) |>
      left_join(r2_interview |>
                  transmute(su_id, detail_int_month = int_month,
                            detail_int_year = int_year),
                by = "su_id") |>
      select(row_id, starts_with("detail_")),
    by = "row_id"
  ) |>
  mutate(
    partner_detail_updated =
      (is.na(pgender) & !is.na(detail_pgender)) |
      (is.na(page) & !is.na(detail_page)) |
      (is.na(pagerel) & !is.na(detail_pagerel)) |
      (is.na(fstsexy) & !is.na(detail_fstsexy)),
    pgender = coalesce(pgender, detail_pgender),
    page = coalesce(page, detail_page),
    pagerel = coalesce(pagerel, detail_pagerel),
    fstsexy = coalesce(fstsexy, detail_fstsexy),
    round = if_else(coalesce(partner_detail_updated, FALSE), 2, round),
    int_month = if_else(coalesce(partner_detail_updated, FALSE),
                        detail_int_month, int_month),
    int_year = if_else(coalesce(partner_detail_updated, FALSE),
                       detail_int_year, int_year)
  ) |>
  select(-row_id, -partner_detail_updated, -starts_with("detail_"))

#### Rounds 3-4 sources ####
r3_interview <- read_dta("../data/nshap_w3_core.dta") |>
  select(su_id, int_start) |>
  zap_labels() |>
  transmute(su_id,
            int_month = as.integer(int_start) %% 12 + 1,
            int_year = 1960 + as.integer(int_start) %/% 12)

r4_interview <- read_dta("../data/nshap_r4_core.dta",
                         col_select = c(su_id, int_start)) |>
  left_join(
    read_dta("../data/2024_03_20_nshap_r4_raw_plus_lbq.dta",
             col_select = c(su_id, intmo, intyr)),
    by = "su_id"
  ) |>
  zap_labels() |>
  transmute(su_id,
            int_month = case_when(
              !is.na(int_start) ~ as.integer(int_start) %% 12 + 1,
              intmo %in% 1:12 ~ as.numeric(intmo)
            ),
            int_year = if_else(
              !is.na(int_start),
              1960 + as.integer(int_start) %/% 12,
              if_else(intyr > 0, as.numeric(intyr), NA)
            ))

# Prior-partner updates: married, cohabiting, or romantic/sexual.

prior_partner_update <- bind_rows(
  read_dta("../data/nshapw3_main.dta",
           col_select = all_of(c("su_id",
                                 "w1m_still", "w1m_divwid", "w1m_widmo",
                                 "w1m_widyr", "w1m_sepdivmo", "w1m_sepdivyr",
                                 "w1c_still", "w1c_endmo", "w1c_endyr",
                                 "w1c_partdie", "w1c_marrpart", "w1c_marrmo",
                                 "w1c_marryr", "w1c_marstill", "w1c_divwid",
                                 "w1c_widmo", "w1c_widyr", "w1c_divmo",
                                 "w1c_divyr", "w1r_still", "w1r_marrpart",
                                 "w1r_marrmo", "w1r_marryr", "w1r_cohbmar",
                                 "w1r_cohbmarmo", "w1r_cohbmaryr",
                                 "w1r_marstill", "w1r_divwid", "w1r_widmo",
                                 "w1r_widyr", "w1r_divmo", "w1r_divyr",
                                 "w1r_evlive", "w1r_evbemo", "w1r_evbeyr",
                                 "w1r_cohstill", "w1r_cohendmo",
                                 "w1r_cohendyr", "w1r_partdie",
                                 "w1_spgender"))) |>
    zap_labels() |>
    mutate(source_round = 3, w1_spgender = as.numeric(w1_spgender)),
  read_dta("../data/2024_03_20_nshap_r4_raw_plus_lbq.dta",
           col_select = all_of(c("su_id",
                                 "w1m_still", "w1m_divwid", "w1m_widmo",
                                 "w1m_widyr", "w1m_sepdivmo", "w1m_sepdivyr",
                                 "w1c_still", "w1c_endmo", "w1c_endyr",
                                 "w1c_partdie", "w1c_marrpart", "w1c_marrmo",
                                 "w1c_marryr", "w1c_marstill", "w1c_divwid",
                                 "w1c_widmo", "w1c_widyr", "w1c_divmo",
                                 "w1c_divyr", "w1r_still", "w1r_marrpart",
                                 "w1r_marrmo", "w1r_marryr", "w1r_cohbmar",
                                 "w1r_cohbmarmo", "w1r_cohbmaryr",
                                 "w1r_marstill", "w1r_divwid", "w1r_widmo",
                                 "w1r_widyr", "w1r_divmo", "w1r_divyr",
                                 "w1r_evlive", "w1r_evbemo", "w1r_evbeyr",
                                 "w1r_cohstill", "w1r_cohendmo",
                                 "w1r_cohendyr", "w1r_partdie",
                                 "w1_spgender"))) |>
    zap_labels() |>
    mutate(source_round = 4, w1_spgender = as.numeric(w1_spgender))
) |>
  mutate(across(-c(su_id, source_round),
                ~ if_else(.x < 0, NA, as.numeric(.x)))) |>
  transmute(
    su_id, source_round, prior_pgender = w1_spgender,
    carried_cohabitation_married = coalesce(w1c_still == 1, FALSE) &
      coalesce(w1c_marrpart == 1, FALSE),
    married_partner = coalesce(w1c_marrpart == 1, FALSE) |
      coalesce(w1r_marrpart == 1, FALSE),
    new_marbeginm = coalesce(w1c_marrmo, w1r_marrmo),
    new_marbeginy = coalesce(w1c_marryr, w1r_marryr),
    new_livebfmr = case_when(
      coalesce(w1c_marrpart == 1, FALSE) ~ 1,
      married_partner & coalesce(w1r_cohbmar == 1, FALSE) ~ 1,
      married_partner ~ 2
    ),
    new_cobeginm = coalesce(w1r_cohbmarmo, w1r_evbemo),
    new_cobeginy = coalesce(w1r_cohbmaryr, w1r_evbeyr),
    marriage_ended = coalesce(w1m_still == 2, FALSE) |
      coalesce(w1c_marstill == 2, FALSE) |
      coalesce(w1r_marstill == 2, FALSE),
    new_end_type = coalesce(w1m_divwid, w1c_divwid, w1r_divwid),
    new_marendm = if_else(coalesce(new_end_type, 2) == 1,
                       coalesce(w1m_widmo, w1c_widmo, w1r_widmo),
                       coalesce(w1m_sepdivmo, w1c_divmo, w1r_divmo)),
    new_marendy = if_else(coalesce(new_end_type, 2) == 1,
                       coalesce(w1m_widyr, w1c_widyr, w1r_widyr),
                       coalesce(w1m_sepdivyr, w1c_divyr, w1r_divyr)),
    prior_cohabitation_ended = !married_partner &
      coalesce(w1c_still == 2, FALSE),
    cohabitation_ended = prior_cohabitation_ended |
      (!married_partner & coalesce(w1r_cohstill == 2, FALSE)),
    new_coendm = coalesce(w1c_endmo, w1r_cohendmo),
    new_coendy = coalesce(w1c_endyr, w1r_cohendyr),
    new_partner_died = coalesce(w1c_partdie, w1r_partdie),
    new_cohabitation = !married_partner & !cohabitation_ended &
      coalesce(w1r_evlive == 1, FALSE) &
      coalesce(w1r_cohstill == 1, FALSE)
  ) |>
  filter(married_partner | marriage_ended | cohabitation_ended |
           new_cohabitation)

# Loop age is skipped when the partner is already on the household roster,
# where lines 1-5 are roster A, line 6 roster B, and lines 8-22 roster D.
roster_ages <- bind_rows(
  read_dta("../data/nshapw3_main.dta") |>
    select(su_id, matches("^ra_age[1-5]$"), rb_age,
           matches("^rd_age([1-9]|1[0-5])$")) |>
    zap_labels() |>
    mutate(source_round = 3),
  read_dta("../data/2024_03_20_nshap_r4_raw_plus_lbq.dta") |>
    select(su_id, matches("^ra_age[1-5]$"), rb_age,
           matches("^rd_age([1-9]|1[0-5])$")) |>
    zap_labels() |>
    mutate(source_round = 4)
) |>
  rename(ra_age6 = rb_age) |>
  pivot_longer(-c(su_id, source_round), names_to = "roster",
               values_to = "roster_age") |>
  transmute(
    su_id, source_round,
    line = as.integer(sub("^r[ad]_age", "", roster)) +
      if_else(startsWith(roster, "rd_"), 7L, 0L),
    roster_age = as.numeric(roster_age)
  ) |>
  filter(!is.na(roster_age), roster_age > 0)

# Marriage and cohabitation loops are numbered separately, so loop 1 of each
# is not the same partner. In the marriage loop, togmo/togyr is when the
# couple started living together.
partner_loops <- bind_rows(
  read_dta("../data/nshapw3_main.dta") |>
    select(su_id,
           spouseln, maritlst,
           matches(paste0("^marr[1-5]_(togmo|togyr|endmo|endyr|splivtog|",
                          "spdivwid|spdivdt|spdivmo|spdivyr|deathmo|",
                          "deathyr|spgender|spage|spolder|firspsexyr|",
                          "sproster|spline)$")),
           matches(paste0("^cohab[1-5]_(cobegm|cohbegy|cohendm|cohendy|",
                          "cohenddt|cohdie|cohgender|cohage|cohagediff|",
                          "cohfstsexy|cohroster|cohline)$"))) |>
    zap_labels() |>
    mutate(source_round = 3, across(c(spouseln, maritlst), as.numeric)),
  read_dta("../data/2024_03_20_nshap_r4_raw_plus_lbq.dta") |>
    select(su_id,
           spouseln, maritlst,
           matches(paste0("^marr[1-5]_(togmo|togyr|endmo|endyr|splivtog|",
                          "spdivwid|spdivdt|spdivmo|spdivyr|deathmo|",
                          "deathyr|spgender|spage|spolder|firspsexyr|",
                          "sproster|spline)$")),
           matches(paste0("^cohab[1-5]_(cobegm|cohbegy|cohendm|cohendy|",
                          "cohenddt|cohdie|cohgender|cohage|cohagediff|",
                          "cohfstsexy|cohroster|cohline)$"))) |>
    zap_labels() |>
    mutate(source_round = 4, across(c(spouseln, maritlst), as.numeric))
) |>
  pivot_longer(
    cols = matches("^[a-z]+[1-5]_"),
    names_to = c("prefix", "loop_number", "suffix"),
    names_pattern = "(.*?)([1-5])_(.*)"
  ) |>
  unite("field_name", prefix, suffix, sep = "_") |>
  mutate(loop_number = as.integer(loop_number)) |>
  pivot_wider(names_from = field_name, values_from = value) |>
  # marr*_endmo/endyr is the wedding date (mislabeled in the raw data).
  rename(marr_marbeginm = marr_endmo, marr_marbeginy = marr_endyr) |>
  arrange(source_round, su_id, loop_number) |>
  mutate(
    current_marriage_repeat = (
      coalesce(marr_marbeginm == -3, FALSE) &
        coalesce(marr_marbeginy == -3, FALSE) &
        coalesce(marr_spdivwid == -3, FALSE)
    ) | (
      coalesce(marr_sproster == 1, FALSE) &
        coalesce(marr_spline == spouseln, FALSE) &
        coalesce(maritlst == 1, FALSE)
    ),
    current_cohabitation_repeat = coalesce(cohab_cohroster == 1, FALSE) &
      coalesce(cohab_cohline == spouseln, FALSE) &
      coalesce(maritlst %in% 1:2, FALSE),
    marriage_end_unknown = !current_marriage_repeat & if_any(
      c(marr_spdivwid, marr_spdivdt, marr_spdivmo,
        marr_spdivyr, marr_deathmo, marr_deathyr),
      ~ .x %in% c(-1, -2)
    ),
    cohabitation_end_unknown = cohab_cohenddt %in% c(-1, -2)
  ) |>
  mutate(across(where(is.numeric), ~ if_else(. < 0, NA, .)))

partner_loops <- partner_loops |>
  left_join(roster_ages |> rename(marr_roster_age = roster_age),
            by = c("su_id", "source_round", "marr_spline" = "line")) |>
  left_join(roster_ages |> rename(cohab_roster_age = roster_age),
            by = c("su_id", "source_round", "cohab_cohline" = "line"))

# Keep marriage slots with any reported information.
loop_marriages <- partner_loops |>
  filter(
    marriage_end_unknown |
      if_any(c(marr_togmo, marr_togyr, marr_marbeginm, marr_marbeginy,
               marr_splivtog, marr_spdivwid, marr_spdivmo,
               marr_spdivyr, marr_deathmo, marr_deathyr,
               marr_spgender, marr_spage, marr_spolder,
               marr_firspsexyr),
             ~ !is.na(.))
  ) |>
  transmute(
    su_id, source_round, loop_number, current_marriage_repeat,
    marriage_record = 1,
    marriage_ended_record = if_else(
      !is.na(marr_spdivmo) | !is.na(marr_spdivyr) |
        !is.na(marr_deathmo) | !is.na(marr_deathyr) |
        marr_spdivwid %in% 1:3 | marriage_end_unknown,
      1, NA
    ),
    pgender = marr_spgender,
    page = coalesce(marr_spage, marr_roster_age),
    pagerel = marr_spolder,
    fstsexy = marr_firspsexyr,
    marbeginm = marr_marbeginm,
    marbeginy = marr_marbeginy,
    marendm = coalesce(marr_deathmo, marr_spdivmo),
    marendy = coalesce(marr_deathyr, marr_spdivyr),
    livebfmr = marr_splivtog,
    cohabitation_record = if_else(marr_splivtog == 1, 1, NA),
    cohabitation_ended_record = if_else(marr_splivtog == 1,
                                        1, NA),
    marriage_end_type = coalesce(marr_spdivwid,
                       if_else(!is.na(marr_deathyr) | !is.na(marr_deathmo),
                               1, NA)),
    cobeginm = if_else(marr_splivtog == 1, marr_togmo, NA),
    cobeginy = if_else(marr_splivtog == 1, marr_togyr, NA)
  )

# Keep cohabitation slots with any reported information.
loop_cohabs <- partner_loops |>
  filter(
    !current_cohabitation_repeat,
    cohabitation_end_unknown |
      if_any(c(cohab_cobegm, cohab_cohbegy, cohab_cohendm,
               cohab_cohendy, cohab_cohenddt, cohab_cohdie,
               cohab_cohgender, cohab_cohage, cohab_cohagediff,
               cohab_cohfstsexy),
             ~ !is.na(.))
  ) |>
  transmute(
    su_id, source_round, loop_number,
    cohabitation_record = 1,
    cohabitation_current_record = if_else(cohab_cohenddt == 2,
                                          1, NA),
    cohabitation_ended_record = case_when(
      cohab_cohenddt == 2 ~ NA,
      cohabitation_end_unknown | !is.na(cohab_cohenddt) ~ 1
    ),
    pgender = cohab_cohgender,
    page = coalesce(cohab_cohage, cohab_roster_age),
    pagerel = cohab_cohagediff,
    fstsexy = cohab_cohfstsexy,
    cobeginm = cohab_cobegm,
    cobeginy = cohab_cohbegy,
    coendm = cohab_cohendm,
    coendy = cohab_cohendy,
    partner_died = cohab_cohdie
  )

# Current-partnership dates and partner characteristics.
current_partnership_details <- bind_rows(
  read_dta("../data/nshapw3_main.dta",
           col_select = c(su_id, spouseln, marriedm, marriedy, livbfmr,
                          livbfmrm, livbfmry, cohabitm, cohabity, spgender,
                          spage, agediff, w2p_frstsx_yr)) |>
    zap_labels() |>
    rename(fstsexy = w2p_frstsx_yr) |>
    mutate(source_round = 3),
  read_dta("../data/2024_03_20_nshap_r4_raw_plus_lbq.dta",
           col_select = c(su_id, spouseln, marriedm, marriedy, livbfmr,
                          livbfmrm, livbfmry, cohabitm, cohabity, spgender,
                          spage, agediff, fstsexdt_fstsexy)) |>
    zap_labels() |>
    rename(fstsexy = fstsexdt_fstsexy) |>
    mutate(source_round = 4)
) |>
  transmute(su_id, source_round,
            current_partner_line = as.numeric(spouseln),
            current_partner_marbeginm = as.numeric(marriedm),
            current_partner_marbeginy = as.numeric(marriedy),
            current_partner_livebfmr = as.numeric(livbfmr),
            current_partner_livebfmrm = as.numeric(livbfmrm),
            current_partner_livebfmry = as.numeric(livbfmry),
            current_partner_cobeginm = as.numeric(cohabitm),
            current_partner_cobeginy = as.numeric(cohabity),
            current_partner_pgender = as.numeric(spgender),
            current_partner_page = as.numeric(spage),
            current_partner_pagerel = as.numeric(agediff),
            current_partner_fstsexy = as.numeric(fstsexy)) |>
  mutate(across(-c(su_id, source_round),
                ~ if_else(. < 0, NA, as.numeric(.)))) |>
  left_join(roster_ages,
            by = c("su_id", "source_round", "current_partner_line" = "line")) |>
  mutate(current_partner_page = coalesce(current_partner_page, roster_age)) |>
  select(-current_partner_line, -roster_age)

#### Round 3 ####
# Close the most recent intact marriage.
partnership_history <- partnership_history |>
  mutate(row_id = row_number())
r3_marriage_closures <- partnership_history |>
  filter(su_id %in%
           filter(prior_partner_update, source_round == 3,
                  marriage_ended, !married_partner)$su_id,
         !is.na(marbeginy) | !is.na(marbeginm) |
           coalesce(marriage_record, 0) == 1 | coalesce(mstatus, 99) == 1,
         !coalesce(marriage_ended_record == 1, FALSE),
         is.na(mstatus) | mstatus == 1,
         is.na(marendy), is.na(marendm)) |>
  group_by(su_id) |>
  slice_max(order_by = round * 10000 + coalesce(marbeginy, 0),
            n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(su_id, row_id) |>
  inner_join(filter(prior_partner_update, source_round == 3), by = "su_id")
partnership_history <- partnership_history |>
  left_join(r3_marriage_closures |>
              left_join(r3_interview, by = "su_id") |>
              transmute(row_id, closed = TRUE, new_end_type, prior_pgender,
                        new_marendm, new_marendy, update_month = int_month,
                        update_year = int_year),
            by = "row_id") |>
  mutate(
    closed = coalesce(closed, FALSE),
    pgender = coalesce(pgender, prior_pgender),
    marendm = if_else(!is.na(new_marendm), new_marendm, marendm),
    marendy = if_else(!is.na(new_marendy), new_marendy, marendy),
    mstatus = case_when(
      closed & new_end_type == 1 ~ 4,
      closed & new_end_type == 2 ~ 2,
      closed & new_end_type == 3 ~ 3,
      closed ~ NA,
      TRUE ~ mstatus
    ),
    marriage_ended_record = if_else(closed, 1, marriage_ended_record),
    round = if_else(closed, 3, round),
    int_month = if_else(closed, update_month, int_month),
    int_year = if_else(closed, update_year, int_year)
  ) |>
  select(-closed, -new_end_type, -prior_pgender, -new_marendm, -new_marendy,
         -update_month, -update_year)

# Keep an ended marriage when no intact prior row can be linked.
r3_added_rows <- prior_partner_update |>
  filter(source_round == 3, marriage_ended, !married_partner) |>
  anti_join(r3_marriage_closures, by = "su_id") |>
  anti_join(
    partnership_history |>
      filter(
        coalesce(marriage_ended_record, 0) == 1,
        !is.na(marendy)
      ) |>
      distinct(su_id, new_marendy = marendy),
    by = c("su_id", "new_marendy")
  ) |>
  transmute(
    su_id, round = 3, marriage_record = 1, pgender = prior_pgender,
    marriage_ended_record = 1,
    marendm = new_marendm, marendy = new_marendy,
    mstatus = case_when(
      new_end_type == 1 ~ 4,
      new_end_type == 2 ~ 2,
      new_end_type == 3 ~ 3
    )
  )

# Convert an intact cohabitation to marriage or append the marriage.
r3_converted_cohabs <- partnership_history |>
  filter(su_id %in%
           filter(prior_partner_update, source_round == 3,
                  married_partner, new_livebfmr == 1)$su_id,
         !is.na(cobeginy) | !is.na(cobeginm) |
           coalesce(cohabitation_current_record, 0) == 1 |
           coalesce(cohabitation_record, 0) == 1,
         !coalesce(cohabitation_ended_record == 1, FALSE),
         is.na(coendy), is.na(coendm),
         is.na(marbeginy), is.na(marbeginm),
         is.na(marendy), is.na(marendm)) |>
  group_by(su_id) |>
  slice_max(order_by = round * 10000 + coalesce(cobeginy, 0),
            n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(su_id, row_id) |>
  inner_join(filter(prior_partner_update, source_round == 3,
                    married_partner), by = "su_id")
partnership_history <- partnership_history |>
  left_join(r3_converted_cohabs |>
              left_join(r3_interview, by = "su_id") |>
              transmute(row_id, converted = TRUE, prior_pgender,
                        new_marbeginm, new_marbeginy,
                        new_cobeginm, new_cobeginy,
                        marriage_ended, new_end_type, new_marendm, new_marendy,
                        update_month = int_month, update_year = int_year),
            by = "row_id") |>
  mutate(
    converted = coalesce(converted, FALSE),
    ended = coalesce(marriage_ended, FALSE),
    pgender = coalesce(pgender, prior_pgender),
    marbeginm = if_else(!is.na(new_marbeginm), new_marbeginm, marbeginm),
    marbeginy = if_else(!is.na(new_marbeginy), new_marbeginy, marbeginy),
    cobeginm = if_else(converted & !is.na(new_cobeginm),
                       new_cobeginm, cobeginm),
    cobeginy = if_else(converted & !is.na(new_cobeginy),
                       new_cobeginy, cobeginy),
    marriage_record = if_else(converted, 1, marriage_record),
    marriage_ended_record = if_else(converted & ended, 1,
                                    marriage_ended_record),
    cohabitation_record = if_else(converted, 1, cohabitation_record),
    cohabitation_current_record = if_else(converted, NA,
                                          cohabitation_current_record),
    cohabitation_ended_record = if_else(converted, 1,
                                        cohabitation_ended_record),
    livebfmr = if_else(converted, 1, livebfmr),
    marendm = if_else(ended & !is.na(new_marendm), new_marendm, marendm),
    marendy = if_else(ended & !is.na(new_marendy), new_marendy, marendy),
    mstatus = case_when(
      converted & marriage_ended & new_end_type == 1 ~ 4,
      converted & marriage_ended & new_end_type == 2 ~ 2,
      converted & marriage_ended & new_end_type == 3 ~ 3,
      converted & marriage_ended ~ NA,
      converted ~ 1,
      TRUE ~ mstatus
    ),
    round = if_else(converted, 3, round),
    int_month = if_else(converted, update_month, int_month),
    int_year = if_else(converted, update_year, int_year)
  ) |>
  select(-converted, -ended, -prior_pgender, -new_marbeginm, -new_marbeginy,
         -new_cobeginm, -new_cobeginy, -marriage_ended,
         -new_end_type, -new_marendm, -new_marendy, -update_month, -update_year)
r3_added_rows <- bind_rows(
  r3_added_rows,
  prior_partner_update |>
    filter(source_round == 3, married_partner) |>
    anti_join(r3_converted_cohabs, by = "su_id") |>
    transmute(
      su_id, round = 3, marriage_record = 1, pgender = prior_pgender,
      marriage_ended_record = if_else(marriage_ended, 1, NA),
      marbeginm = new_marbeginm, marbeginy = new_marbeginy,
      livebfmr = new_livebfmr,
      cobeginm = if_else(coalesce(new_livebfmr, 2) == 1,
                         new_cobeginm, NA),
      cobeginy = if_else(coalesce(new_livebfmr, 2) == 1,
                         new_cobeginy, NA),
      cohabitation_record = if_else(new_livebfmr == 1, 1, NA),
      cohabitation_ended_record = if_else(new_livebfmr == 1, 1, NA),
      marendm = if_else(marriage_ended, new_marendm, NA),
      marendy = if_else(marriage_ended, new_marendy, NA),
      mstatus = case_when(
        marriage_ended & new_end_type == 1 ~ 4,
        marriage_ended & new_end_type == 2 ~ 2,
        marriage_ended & new_end_type == 3 ~ 3,
        marriage_ended ~ NA,
        TRUE ~ 1
      )
    )
)

# Close the most recent intact cohabitation.
r3_cohab_closures <- partnership_history |>
  filter(su_id %in%
           filter(prior_partner_update, source_round == 3,
                  prior_cohabitation_ended)$su_id,
         !is.na(cobeginy) | !is.na(cobeginm) |
           coalesce(cohabitation_current_record, 0) == 1 |
           coalesce(cohabitation_record, 0) == 1,
         !coalesce(cohabitation_ended_record == 1, FALSE),
         is.na(coendy), is.na(coendm),
         is.na(marbeginy), is.na(marbeginm),
         is.na(marendy), is.na(marendm),
         coalesce(marriage_record, 0) != 1) |>
  group_by(su_id) |>
  slice_max(order_by = round * 10000 + coalesce(cobeginy, 0),
            n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(su_id, row_id) |>
  inner_join(filter(prior_partner_update, source_round == 3,
                    prior_cohabitation_ended),
             by = "su_id")
partnership_history <- partnership_history |>
  left_join(r3_cohab_closures |>
              left_join(r3_interview, by = "su_id") |>
              transmute(row_id, cohabitation_closed = TRUE, prior_pgender,
                        new_coendm, new_coendy, new_partner_died,
                        update_month = int_month, update_year = int_year),
            by = "row_id") |>
  mutate(
    cohabitation_closed = coalesce(cohabitation_closed, FALSE),
    pgender = coalesce(pgender, prior_pgender),
    coendm = if_else(!is.na(new_coendm), new_coendm, coendm),
    coendy = if_else(!is.na(new_coendy), new_coendy, coendy),
    partner_died = if_else(cohabitation_closed & !is.na(new_partner_died),
                       new_partner_died, partner_died),
    cohabitation_record = if_else(cohabitation_closed, 1, cohabitation_record),
    cohabitation_current_record = if_else(cohabitation_closed, NA,
                                          cohabitation_current_record),
    cohabitation_ended_record = if_else(cohabitation_closed, 1,
                                        cohabitation_ended_record),
    round = if_else(cohabitation_closed, 3, round),
    int_month = if_else(cohabitation_closed, update_month, int_month),
    int_year = if_else(cohabitation_closed, update_year, int_year)
  ) |>
  select(-cohabitation_closed, -prior_pgender, -new_coendm, -new_coendy,
         -new_partner_died, -update_month, -update_year)
r3_added_rows <- bind_rows(
  r3_added_rows,
  prior_partner_update |>
    filter(source_round == 3, cohabitation_ended) |>
    anti_join(r3_cohab_closures, by = "su_id") |>
    transmute(su_id, round = 3, cohabitation_record = 1,
              cohabitation_ended_record = 1, pgender = prior_pgender,
              cobeginm = new_cobeginm, cobeginy = new_cobeginy,
              coendm = new_coendm, coendy = new_coendy,
              partner_died = new_partner_died)
)

# Add a new intact cohabitation when needed.
r3_added_rows <- bind_rows(
  r3_added_rows,
  prior_partner_update |>
    filter(source_round == 3, new_cohabitation) |>
    anti_join(partnership_history |>
                filter(!is.na(cobeginy) | !is.na(cobeginm) |
                         coalesce(cohabitation_current_record, 0) == 1 |
                         coalesce(cohabitation_record, 0) == 1,
                       !coalesce(cohabitation_ended_record == 1, FALSE),
                       is.na(coendy), is.na(coendm),
                       is.na(marbeginy), is.na(marbeginm),
                       is.na(marendy), is.na(marendm),
                       coalesce(marriage_record, 0) != 1) |>
                distinct(su_id),
              by = "su_id") |>
    transmute(su_id, round = 3, cohabitation_record = 1,
              cohabitation_current_record = 1, pgender = prior_pgender,
              cobeginm = new_cobeginm, cobeginy = new_cobeginy)
)

partnership_history <- bind_rows(
  partnership_history |> select(-row_id),
  r3_added_rows |> left_join(r3_interview, by = "su_id")
)

# Merge the repeated current-spouse loops into the existing marriage.
partnership_history <- partnership_history |>
  mutate(row_id = row_number()) |>
  left_join(
    partnership_history |>
      mutate(row_id = row_number()) |>
      filter(
        su_id %in% filter(loop_marriages, source_round == 3,
                          current_marriage_repeat)$su_id,
        coalesce(marriage_record, 0) == 1 |
          !is.na(marbeginm) | !is.na(marbeginy),
        !coalesce(marriage_ended_record == 1, FALSE),
        is.na(marendm), is.na(marendy),
        is.na(mstatus) | mstatus == 1
      ) |>
      group_by(su_id) |>
      slice_max(order_by = round * 10000 + coalesce(marbeginy, 0),
                n = 1, with_ties = FALSE) |>
      ungroup() |>
      select(su_id, row_id) |>
      inner_join(
        loop_marriages |>
          filter(source_round == 3, current_marriage_repeat) |>
          transmute(
            su_id, update_pgender = pgender, update_page = page,
            update_pagerel = pagerel, update_fstsexy = fstsexy,
            update_marbeginm = marbeginm, update_marbeginy = marbeginy,
            update_livebfmr = livebfmr, update_cobeginm = cobeginm,
            update_cobeginy = cobeginy,
            update_cohabitation_record = cohabitation_record,
            update_cohabitation_ended_record = cohabitation_ended_record
          ),
        by = "su_id"
      ) |>
      left_join(r3_interview, by = "su_id") |>
      transmute(row_id, updated = TRUE,
                update_pgender, update_page, update_pagerel, update_fstsexy,
                update_marbeginm, update_marbeginy,
                update_livebfmr, update_cobeginm, update_cobeginy,
                update_cohabitation_record,
                update_cohabitation_ended_record,
                update_int_month = int_month, update_int_year = int_year),
    by = "row_id"
  ) |>
  mutate(
    updated = coalesce(updated, FALSE),
    pgender = coalesce(update_pgender, pgender),
    page = coalesce(update_page, page),
    pagerel = coalesce(update_pagerel, pagerel),
    fstsexy = coalesce(update_fstsexy, fstsexy),
    marbeginm = coalesce(marbeginm, update_marbeginm),
    marbeginy = coalesce(marbeginy, update_marbeginy),
    livebfmr = coalesce(update_livebfmr, livebfmr),
    cobeginm = coalesce(update_cobeginm, cobeginm),
    cobeginy = coalesce(update_cobeginy, cobeginy),
    cohabitation_record = coalesce(update_cohabitation_record,
                                   cohabitation_record),
    cohabitation_ended_record = coalesce(
      update_cohabitation_ended_record, cohabitation_ended_record
    ),
    marriage_record = if_else(updated, 1, marriage_record),
    marriage_ended_record = if_else(updated, NA, marriage_ended_record),
    mstatus = if_else(updated, 1, mstatus),
    round = if_else(updated, 3, round),
    int_month = if_else(updated, update_int_month, int_month),
    int_year = if_else(updated, update_int_year, int_year)
  ) |>
  select(-row_id, -updated, -starts_with("update_"))

partnership_history <- bind_rows(
  partnership_history,
  bind_rows(loop_marriages |> filter(!current_marriage_repeat),
            loop_cohabs) |>
    filter(source_round == 3) |>
    select(-source_round, -current_marriage_repeat) |>
    arrange(su_id, loop_number) |>
    mutate(round = 3) |>
    left_join(r3_interview, by = "su_id")
)

# Reconcile the Round 3 current partnership before Round 4 updates.
r3_status <- read_dta(
  "../data/nshap_w3_core.dta", col_select = c(su_id, maritlst)
) |>
  zap_labels() |>
  transmute(su_id, current_status = as.numeric(maritlst)) |>
  left_join(r3_interview, by = "su_id") |>
  rename(update_month = int_month, update_year = int_year)


partnership_history <- partnership_history |>
  mutate(row_id = row_number())

partnership_history <- partnership_history |>
  left_join(
    partnership_history |>
      inner_join(r3_status |>
                   filter(current_status %in% c(1, 2)) |>
                   left_join(filter(current_partnership_details,
                                    source_round == 3), by = "su_id"),
                 by = "su_id") |>
      filter(
        (current_status == 1 &
           (!is.na(marbeginy) | !is.na(marbeginm) |
              coalesce(marriage_record, 0) == 1) &
           !coalesce(marriage_ended_record == 1, FALSE) &
           is.na(marendy) & is.na(marendm) &
           (is.na(mstatus) | mstatus == 1)) |
          (current_status == 2 &
             (!is.na(cobeginy) | !is.na(cobeginm) |
                coalesce(cohabitation_current_record, 0) == 1 |
                coalesce(cohabitation_record, 0) == 1) &
             !coalesce(cohabitation_ended_record == 1, FALSE) &
             is.na(coendy) & is.na(coendm) &
             (coalesce(cohabitation_current_record, 0) == 1 |
                coalesce(marriage_record, 0) != 1))
      ) |>
      group_by(su_id) |>
      slice_max(
        order_by = round * 10000 +
          if_else(current_status == 1,
                  coalesce(marbeginy, 0), coalesce(cobeginy, 0)),
        n = 1, with_ties = FALSE
      ) |>
      ungroup() |>
      select(su_id, row_id, current_status, starts_with("current_partner_"),
             update_month, update_year) |>
      select(-su_id),
    by = "row_id"
  ) |>
  mutate(
    r3_current_filled = !is.na(current_status),
    marbeginm = if_else(coalesce(current_status == 1, FALSE),
                        coalesce(marbeginm, current_partner_marbeginm),
                        marbeginm),
    marbeginy = if_else(coalesce(current_status == 1, FALSE),
                        coalesce(marbeginy, current_partner_marbeginy),
                        marbeginy),
    livebfmr = if_else(coalesce(current_status == 1, FALSE),
                       coalesce(livebfmr, current_partner_livebfmr), livebfmr),
    cobeginm = case_when(
      coalesce(current_status == 2, FALSE) ~
        coalesce(cobeginm, current_partner_cobeginm),
      coalesce(current_status == 1 & current_partner_livebfmr == 1, FALSE) ~
        coalesce(cobeginm, current_partner_livebfmrm),
      TRUE ~ cobeginm
    ),
    cobeginy = case_when(
      coalesce(current_status == 2, FALSE) ~
        coalesce(cobeginy, current_partner_cobeginy),
      coalesce(current_status == 1 & current_partner_livebfmr == 1, FALSE) ~
        coalesce(cobeginy, current_partner_livebfmry),
      TRUE ~ cobeginy
    ),
    pgender = if_else(current_status %in% c(1, 2),
                      coalesce(current_partner_pgender, pgender), pgender),
    page = if_else(current_status %in% c(1, 2),
                   coalesce(current_partner_page, page), page),
    pagerel = if_else(current_status %in% c(1, 2),
                      coalesce(current_partner_pagerel, pagerel), pagerel),
    fstsexy = if_else(current_status %in% c(1, 2),
                      coalesce(fstsexy, current_partner_fstsexy), fstsexy),
    marriage_record = if_else(coalesce(current_status == 1, FALSE),
                              1, marriage_record),
    marriage_ended_record = if_else(coalesce(current_status == 1, FALSE), NA,
                                    marriage_ended_record),
    cohabitation_record = if_else(
      coalesce(current_status == 2, FALSE) |
        coalesce(current_status == 1 & current_partner_livebfmr == 1, FALSE), 1,
      cohabitation_record
    ),
    cohabitation_current_record = if_else(
      coalesce(current_status == 2, FALSE), 1,
      cohabitation_current_record
    ),
    cohabitation_ended_record = if_else(
      coalesce(current_status == 2, FALSE), NA,
      cohabitation_ended_record
    ),
    mstatus = case_when(
      current_status == 1 ~ 1,
      current_status == 2 ~ 5,
      TRUE ~ mstatus
    ),
    round = if_else(current_status %in% c(1, 2), 3, round),
    int_month = if_else(current_status %in% c(1, 2),
                        update_month, int_month),
    int_year = if_else(current_status %in% c(1, 2),
                       update_year, int_year)
  ) |>
  select(-row_id, -current_status, -starts_with("current_partner_"),
         -update_month, -update_year)

partnership_history <- bind_rows(
  partnership_history,
  r3_status |>
    filter(current_status %in% c(1, 2)) |>
    left_join(filter(current_partnership_details, source_round == 3),
              by = "su_id") |>
    anti_join(partnership_history |>
                filter(r3_current_filled) |>
                distinct(su_id),
              by = "su_id") |>
    transmute(
      su_id, round = 3,
      marriage_record = if_else(current_status == 1, 1, NA),
      cohabitation_record = if_else(
        current_status == 2 |
          coalesce(current_status == 1 & current_partner_livebfmr == 1, FALSE),
        1, NA
      ),
      cohabitation_current_record = if_else(current_status == 2, 1, NA),
      mstatus = if_else(current_status == 1, 1, 5),
      marbeginm = if_else(current_status == 1, current_partner_marbeginm, NA),
      marbeginy = if_else(current_status == 1, current_partner_marbeginy, NA),
      livebfmr = if_else(current_status == 1, current_partner_livebfmr, NA),
      cobeginm = case_when(
        current_status == 2 ~ current_partner_cobeginm,
        coalesce(current_status == 1 & current_partner_livebfmr == 1, FALSE) ~
          current_partner_livebfmrm
      ),
      cobeginy = case_when(
        current_status == 2 ~ current_partner_cobeginy,
        coalesce(current_status == 1 & current_partner_livebfmr == 1, FALSE) ~
          current_partner_livebfmry
      ),
      pgender = current_partner_pgender, page = current_partner_page,
      pagerel = current_partner_pagerel, fstsexy = current_partner_fstsexy,
      int_month = update_month, int_year = update_year
    )
) |>
  select(-r3_current_filled)


# Add status-implied records before classifying the Round 3 history.
r3_snapshot <- bind_rows(
  partnership_history,
  r3_status |>
    left_join(
      partnership_history |>
        group_by(su_id) |>
        summarise(
          in_history = TRUE,
          any_marriage = any(
            !is.na(marbeginm) | !is.na(marbeginy) |
              !is.na(marendm) | !is.na(marendy) |
              coalesce(marriage_record, 0) == 1
          ),
          any_widowed = any(coalesce(mstatus, 0) == 4 |
                              coalesce(marriage_end_type, 0) == 1),
          any_intact_marriage = any(
            (!is.na(marbeginm) | !is.na(marbeginy) |
               coalesce(marriage_record, 0) == 1) &
              !coalesce(marriage_ended_record == 1, FALSE) &
              !(mstatus %in% 2:4)
          ),
          .groups = "drop"
        ),
      by = "su_id"
    ) |>
    filter(
      (current_status %in% 3:5 &
         (!coalesce(any_marriage, FALSE) |
            (current_status == 5 & !coalesce(any_widowed, FALSE) &
               !coalesce(any_intact_marriage, FALSE)))) |
        (current_status == 6 & is.na(in_history))
    ) |>
    transmute(
      su_id, round = 3,
      marriage_record = if_else(current_status %in% 3:5, 1, NA),
      marriage_ended_record = if_else(current_status %in% 3:5, 1, NA),
      mstatus = case_when(
        current_status == 3 ~ 3,
        current_status == 4 ~ 2,
        current_status == 5 ~ 4,
        TRUE ~ 5
      ),
      int_month = update_month,
      int_year = update_year
    )
) |>
  semi_join(r3_status, by = "su_id") |>
  mutate(row_order = row_number()) |>
  group_by(su_id, marbeginy) |>
  mutate(
    intact = is.na(marendy) & is.na(marendm),
    duplicated_marriage = !is.na(marbeginy) & intact & sum(intact) > 1 &
      n_distinct(round[intact]) > 1 &
      n_distinct(marbeginm[intact], na.rm = TRUE) <= 1,
    reported_fields = rowSums(!is.na(pick(everything())))
  ) |>
  arrange(desc(reported_fields), .by_group = TRUE) |>
  filter(!duplicated_marriage | row_number() == 1) |>
  group_by(su_id, marendy, mstatus) |>
  mutate(
    duplicated_ending = !is.na(marendy) & n() > 1 &
      n_distinct(round) > 1 &
      n_distinct(marbeginy, na.rm = TRUE) <= 1 &
      n_distinct(marendm, na.rm = TRUE) <= 1
  ) |>
  arrange(desc(reported_fields), .by_group = TRUE) |>
  filter(!duplicated_ending | row_number() == 1) |>
  ungroup() |>
  arrange(row_order) |>
  select(-row_order, -intact, -duplicated_marriage, -duplicated_ending,
         -reported_fields) |>
  left_join(
    r3_status |> select(su_id, current_status),
    by = "su_id"
  ) |>
  mutate(
    mstatus = case_when(
      !is.na(mstatus) ~ mstatus,
      marriage_end_type == 1 ~ 4,
      marriage_end_type == 2 ~ 2,
      marriage_end_type == 3 ~ 3,
      (!is.na(marbeginm) | !is.na(marbeginy) |
         coalesce(marriage_record, 0) == 1) &
        !coalesce(marriage_ended_record == 1, FALSE) &
        is.na(marendm) & is.na(marendy) ~ 1,
      (!is.na(marendm) | !is.na(marendy)) & round <= 2 &
        partner_died == 1 ~ 4,
      (!is.na(marendm) | !is.na(marendy)) & round <= 2 &
        partner_died == 2 ~ 2,
      TRUE ~ 5
    ),
    has_mar = !is.na(marbeginm) | !is.na(marbeginy) |
      !is.na(marendm) | !is.na(marendy) |
      coalesce(marriage_record, 0) == 1,
    has_coh = !is.na(cobeginm) | !is.na(cobeginy) |
      (!has_mar & (!is.na(coendm) | !is.na(coendy))) |
      coalesce(cohabitation_current_record, 0) == 1 |
      (!has_mar & coalesce(cohabitation_record, 0) == 1) |
      (has_mar & coalesce(livebfmr == 1, FALSE)),
    relationship_order = case_when(
      has_mar ~ coalesce(marbeginy, marendy, int_year),
      has_coh ~ coalesce(cobeginy, coendy, int_year),
      TRUE ~ int_year
    )
  ) |>
  group_by(su_id) |>
  mutate(
    snapshot_scid = case_when(
      has_mar ~ rank(if_else(has_mar, relationship_order * 100 +
                               coalesce(marbeginm, 0), NA),
                     ties.method = "first", na.last = "keep"),
      has_coh ~ 100 + rank(if_else(has_coh, relationship_order * 100 +
                                     coalesce(cobeginm, 0), NA),
                           ties.method = "first", na.last = "keep"),
      TRUE ~ 200 + rank(if_else(!has_mar & !has_coh, round, NA),
                        ties.method = "first", na.last = "keep")
    ),
    intact_marriage = has_mar &
      !coalesce(marriage_ended_record == 1, FALSE) &
      is.na(marendm) & is.na(marendy) &
      (is.na(mstatus) | mstatus == 1),
    last_intact_marriage = max(
      if_else(intact_marriage, snapshot_scid, -Inf)
    ),
    mstatus = case_when(
      intact_marriage & snapshot_scid == last_intact_marriage &
        current_status == 3 ~ 3,
      intact_marriage & snapshot_scid == last_intact_marriage &
        current_status == 4 ~ 2,
      intact_marriage & snapshot_scid == last_intact_marriage &
        current_status == 5 ~ 4,
      intact_marriage & current_status != 1 & mstatus == 1 ~ NA,
      intact_marriage & current_status == 1 &
        snapshot_scid != last_intact_marriage & mstatus == 1 ~ NA,
      TRUE ~ mstatus
    ),
    marriage_ended_record = case_when(
      intact_marriage & current_status != 1 ~ 1,
      intact_marriage & current_status == 1 &
        snapshot_scid != last_intact_marriage ~ 1,
      TRUE ~ marriage_ended_record
    ),
    intact_cohabitation = has_coh &
      !coalesce(cohabitation_ended_record == 1, FALSE) &
      (coalesce(cohabitation_current_record, 0) == 1 |
         (!has_mar & is.na(coendm) & is.na(coendy))),
    last_intact_cohabitation = max(
      if_else(intact_cohabitation, snapshot_scid, -Inf)
    ),
    cohabitation_current_record = case_when(
      intact_cohabitation & current_status == 2 &
        snapshot_scid == last_intact_cohabitation ~ 1,
      intact_cohabitation ~ NA,
      TRUE ~ cohabitation_current_record
    ),
    cohabitation_ended_record = case_when(
      intact_cohabitation &
        !(current_status == 2 &
            snapshot_scid == last_intact_cohabitation) ~ 1,
      TRUE ~ cohabitation_ended_record
    )
  ) |>
  ungroup() |>
  mutate(
    end_reason = case_when(
      has_mar & mstatus == 4 ~ "widowed",
      has_mar & mstatus %in% c(2, 3) ~ "divorced_separated",
      has_mar & (!is.na(marendy) | !is.na(marendm) |
                   coalesce(marriage_ended_record, 0) == 1) ~
        "ended_unknown",
      has_mar ~ NA,
      has_coh & partner_died == 1 ~ "partner_died",
      has_coh & partner_died == 2 ~ "ended_other",
      has_coh & (!is.na(coendy) | !is.na(coendm) |
                   coalesce(cohabitation_ended_record, 0) == 1) ~
        "ended_unknown"
    ),
    mar_ended = has_mar &
      (!is.na(end_reason) | coalesce(marriage_ended_record, 0) == 1),
    coh_ended = case_when(
      !has_coh ~ FALSE,
      coalesce(cohabitation_current_record, 0) == 1 ~ FALSE,
      coalesce(cohabitation_ended_record, 0) == 1 ~ TRUE,
      has_mar ~ TRUE,
      !is.na(coendy) | !is.na(coendm) ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  group_by(su_id) |>
  filter(has_mar | has_coh | (!any(has_mar | has_coh) & row_number() == 1)) |>
  ungroup()

#### Round 4 ####
partnership_history <- partnership_history |>
  mutate(row_id = row_number())

# Merge a carried cohabiting partner into a compatible existing marriage.
r4_carried_partner_matches <- partnership_history |>
  filter(
    round == 3,
    coalesce(cohabitation_current_record, 0) == 1,
    !coalesce(cohabitation_ended_record == 1, FALSE),
    coalesce(marriage_record, 0) != 1,
    is.na(marbeginm), is.na(marbeginy),
    is.na(marendm), is.na(marendy)
  ) |>
  group_by(su_id) |>
  slice_max(order_by = coalesce(cobeginy, int_year),
            n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(
    su_id, cohabitation_row_id = row_id,
    cohab_cobeginm = cobeginm, cohab_cobeginy = cobeginy,
    cohab_pgender = pgender, cohab_page = page,
    cohab_pagerel = pagerel, cohab_fstsexy = fstsexy
  ) |>
  inner_join(
    prior_partner_update |>
      filter(source_round == 4, carried_cohabitation_married,
             !marriage_ended),
    by = "su_id"
  ) |>
  inner_join(
    partnership_history |>
      filter(
        coalesce(marriage_record, 0) == 1,
        !coalesce(marriage_ended_record == 1, FALSE),
        is.na(marendm), is.na(marendy)
      ) |>
      transmute(
        su_id, marriage_row_id = row_id,
        existing_marbeginm = marbeginm,
        existing_marbeginy = marbeginy,
        existing_round = round
      ),
    by = "su_id"
  ) |>
  filter(
    !is.na(new_marbeginm) | !is.na(new_marbeginy),
    is.na(new_marbeginm) | is.na(existing_marbeginm) |
      new_marbeginm == existing_marbeginm,
    is.na(new_marbeginy) | is.na(existing_marbeginy) |
      new_marbeginy == existing_marbeginy,
    (!is.na(new_marbeginm) & !is.na(existing_marbeginm) &
       new_marbeginm == existing_marbeginm) |
      (!is.na(new_marbeginy) & !is.na(existing_marbeginy) &
         new_marbeginy == existing_marbeginy)
  ) |>
  group_by(su_id) |>
  slice_max(order_by = existing_round * 10000 +
              coalesce(existing_marbeginy, 0),
            n = 1, with_ties = FALSE) |>
  ungroup() |>
  left_join(r4_interview, by = "su_id")

partnership_history <- partnership_history |>
  left_join(
    r4_carried_partner_matches |>
      transmute(
        row_id = marriage_row_id, merged = TRUE,
        new_marbeginm, new_marbeginy,
        cohab_cobeginm, cohab_cobeginy,
        cohab_pgender, cohab_page, cohab_pagerel, cohab_fstsexy,
        update_month = int_month, update_year = int_year
      ),
    by = "row_id"
  ) |>
  mutate(
    merged = coalesce(merged, FALSE),
    marbeginm = coalesce(marbeginm, new_marbeginm),
    marbeginy = coalesce(marbeginy, new_marbeginy),
    cobeginm = coalesce(cobeginm, cohab_cobeginm),
    cobeginy = coalesce(cobeginy, cohab_cobeginy),
    pgender = coalesce(pgender, cohab_pgender),
    page = coalesce(page, cohab_page),
    pagerel = coalesce(pagerel, cohab_pagerel),
    fstsexy = coalesce(fstsexy, cohab_fstsexy),
    marriage_record = if_else(merged, 1, marriage_record),
    cohabitation_record = if_else(merged, 1, cohabitation_record),
    cohabitation_current_record = if_else(merged, NA,
                                          cohabitation_current_record),
    cohabitation_ended_record = if_else(merged, 1,
                                        cohabitation_ended_record),
    livebfmr = if_else(merged, 1, livebfmr),
    mstatus = if_else(merged, 1, mstatus),
    round = if_else(merged, 4, round),
    int_month = if_else(merged, update_month, int_month),
    int_year = if_else(merged, update_year, int_year)
  ) |>
  filter(!(row_id %in%
             r4_carried_partner_matches$cohabitation_row_id)) |>
  select(-merged, -new_marbeginm, -new_marbeginy,
         -starts_with("cohab_"), -update_month, -update_year)

# Close the most recent intact marriage.
r4_marriage_closures <- partnership_history |>
  filter(su_id %in%
           filter(prior_partner_update, source_round == 4,
                  marriage_ended, !married_partner)$su_id,
         !is.na(marbeginy) | !is.na(marbeginm) |
           coalesce(marriage_record, 0) == 1 | coalesce(mstatus, 99) == 1,
         !coalesce(marriage_ended_record == 1, FALSE),
         is.na(mstatus) | mstatus == 1,
         is.na(marendy), is.na(marendm)) |>
  group_by(su_id) |>
  slice_max(order_by = round * 10000 + coalesce(marbeginy, 0),
            n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(su_id, row_id) |>
  inner_join(filter(prior_partner_update, source_round == 4), by = "su_id")
partnership_history <- partnership_history |>
  left_join(r4_marriage_closures |>
              left_join(r4_interview, by = "su_id") |>
              transmute(row_id, closed = TRUE, new_end_type, prior_pgender,
                        new_marendm, new_marendy, update_month = int_month,
                        update_year = int_year),
            by = "row_id") |>
  mutate(
    closed = coalesce(closed, FALSE),
    pgender = coalesce(pgender, prior_pgender),
    marendm = if_else(!is.na(new_marendm), new_marendm, marendm),
    marendy = if_else(!is.na(new_marendy), new_marendy, marendy),
    mstatus = case_when(
      closed & new_end_type == 1 ~ 4,
      closed & new_end_type == 2 ~ 2,
      closed & new_end_type == 3 ~ 3,
      closed ~ NA,
      TRUE ~ mstatus
    ),
    marriage_ended_record = if_else(closed, 1, marriage_ended_record),
    round = if_else(closed, 4, round),
    int_month = if_else(closed, update_month, int_month),
    int_year = if_else(closed, update_year, int_year)
  ) |>
  select(-closed, -new_end_type, -prior_pgender, -new_marendm, -new_marendy,
         -update_month, -update_year)

# Keep an ended marriage when no intact prior row can be linked.
r4_added_rows <- prior_partner_update |>
  filter(source_round == 4, marriage_ended, !married_partner) |>
  anti_join(r4_marriage_closures, by = "su_id") |>
  anti_join(
    partnership_history |>
      filter(
        coalesce(marriage_ended_record, 0) == 1,
        !is.na(marendy)
      ) |>
      distinct(su_id, new_marendy = marendy),
    by = c("su_id", "new_marendy")
  ) |>
  transmute(
    su_id, round = 4, marriage_record = 1, pgender = prior_pgender,
    marriage_ended_record = 1,
    marendm = new_marendm, marendy = new_marendy,
    mstatus = case_when(
      new_end_type == 1 ~ 4,
      new_end_type == 2 ~ 2,
      new_end_type == 3 ~ 3
    )
  )

# Marriages to the prior-round partner: convert or append.
r4_converted_cohabs <- partnership_history |>
  filter(su_id %in%
           filter(prior_partner_update, source_round == 4,
                  married_partner, new_livebfmr == 1)$su_id,
         !is.na(cobeginy) | !is.na(cobeginm) |
           coalesce(cohabitation_current_record, 0) == 1 |
           coalesce(cohabitation_record, 0) == 1,
         !coalesce(cohabitation_ended_record == 1, FALSE),
         is.na(coendy), is.na(coendm),
         is.na(marbeginy), is.na(marbeginm),
         is.na(marendy), is.na(marendm),
         coalesce(marriage_record, 0) != 1) |>
  group_by(su_id) |>
  slice_max(order_by = round * 10000 + coalesce(cobeginy, 0),
            n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(su_id, row_id) |>
  inner_join(filter(prior_partner_update, source_round == 4,
                    married_partner), by = "su_id")
partnership_history <- partnership_history |>
  left_join(r4_converted_cohabs |>
              left_join(r4_interview, by = "su_id") |>
              transmute(row_id, converted = TRUE, prior_pgender,
                        new_marbeginm, new_marbeginy,
                        new_cobeginm, new_cobeginy,
                        marriage_ended, new_end_type, new_marendm, new_marendy,
                        update_month = int_month, update_year = int_year),
            by = "row_id") |>
  mutate(
    converted = coalesce(converted, FALSE),
    ended = coalesce(marriage_ended, FALSE),
    pgender = coalesce(pgender, prior_pgender),
    marbeginm = if_else(!is.na(new_marbeginm), new_marbeginm, marbeginm),
    marbeginy = if_else(!is.na(new_marbeginy), new_marbeginy, marbeginy),
    cobeginm = if_else(converted & !is.na(new_cobeginm),
                       new_cobeginm, cobeginm),
    cobeginy = if_else(converted & !is.na(new_cobeginy),
                       new_cobeginy, cobeginy),
    marriage_record = if_else(converted, 1, marriage_record),
    marriage_ended_record = if_else(converted & ended, 1,
                                    marriage_ended_record),
    cohabitation_record = if_else(converted, 1, cohabitation_record),
    cohabitation_current_record = if_else(converted, NA,
                                          cohabitation_current_record),
    cohabitation_ended_record = if_else(converted, 1,
                                        cohabitation_ended_record),
    livebfmr = if_else(converted, 1, livebfmr),
    marendm = if_else(ended & !is.na(new_marendm), new_marendm, marendm),
    marendy = if_else(ended & !is.na(new_marendy), new_marendy, marendy),
    mstatus = case_when(
      converted & marriage_ended & new_end_type == 1 ~ 4,
      converted & marriage_ended & new_end_type == 2 ~ 2,
      converted & marriage_ended & new_end_type == 3 ~ 3,
      converted & marriage_ended ~ NA,
      converted ~ 1,
      TRUE ~ mstatus
    ),
    round = if_else(converted, 4, round),
    int_month = if_else(converted, update_month, int_month),
    int_year = if_else(converted, update_year, int_year)
  ) |>
  select(-converted, -ended, -prior_pgender, -new_marbeginm, -new_marbeginy,
         -new_cobeginm, -new_cobeginy, -marriage_ended,
         -new_end_type, -new_marendm, -new_marendy, -update_month, -update_year)
r4_added_rows <- bind_rows(
  r4_added_rows,
  prior_partner_update |>
    filter(source_round == 4, married_partner) |>
    anti_join(r4_converted_cohabs, by = "su_id") |>
    anti_join(r4_carried_partner_matches, by = "su_id") |>
    transmute(
      su_id, round = 4, marriage_record = 1, pgender = prior_pgender,
      marriage_ended_record = if_else(marriage_ended, 1, NA),
      marbeginm = new_marbeginm, marbeginy = new_marbeginy,
      livebfmr = new_livebfmr,
      cobeginm = if_else(coalesce(new_livebfmr, 2) == 1,
                         new_cobeginm, NA),
      cobeginy = if_else(coalesce(new_livebfmr, 2) == 1,
                         new_cobeginy, NA),
      cohabitation_record = if_else(new_livebfmr == 1, 1, NA),
      cohabitation_ended_record = if_else(new_livebfmr == 1, 1, NA),
      marendm = if_else(marriage_ended, new_marendm, NA),
      marendy = if_else(marriage_ended, new_marendy, NA),
      mstatus = case_when(
        marriage_ended & new_end_type == 1 ~ 4,
        marriage_ended & new_end_type == 2 ~ 2,
        marriage_ended & new_end_type == 3 ~ 3,
        marriage_ended ~ NA,
        TRUE ~ 1
      )
    )
)

# Close the most recent intact cohabitation.
r4_cohab_closures <- partnership_history |>
  filter(su_id %in%
           filter(prior_partner_update, source_round == 4,
                  prior_cohabitation_ended)$su_id,
         !is.na(cobeginy) | !is.na(cobeginm) |
           coalesce(cohabitation_current_record, 0) == 1 |
           coalesce(cohabitation_record, 0) == 1,
         !coalesce(cohabitation_ended_record == 1, FALSE),
         is.na(coendy), is.na(coendm),
         is.na(marbeginy), is.na(marbeginm),
         is.na(marendy), is.na(marendm),
         coalesce(marriage_record, 0) != 1) |>
  group_by(su_id) |>
  slice_max(order_by = round * 10000 + coalesce(cobeginy, 0),
            n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(su_id, row_id) |>
  inner_join(filter(prior_partner_update, source_round == 4,
                    prior_cohabitation_ended),
             by = "su_id")
partnership_history <- partnership_history |>
  left_join(r4_cohab_closures |>
              left_join(r4_interview, by = "su_id") |>
              transmute(row_id, cohabitation_closed = TRUE, prior_pgender,
                        new_coendm, new_coendy, new_partner_died,
                        update_month = int_month, update_year = int_year),
            by = "row_id") |>
  mutate(
    cohabitation_closed = coalesce(cohabitation_closed, FALSE),
    pgender = coalesce(pgender, prior_pgender),
    coendm = if_else(!is.na(new_coendm), new_coendm, coendm),
    coendy = if_else(!is.na(new_coendy), new_coendy, coendy),
    partner_died = if_else(cohabitation_closed & !is.na(new_partner_died),
                       new_partner_died, partner_died),
    cohabitation_record = if_else(cohabitation_closed, 1, cohabitation_record),
    cohabitation_current_record = if_else(cohabitation_closed, NA,
                                          cohabitation_current_record),
    cohabitation_ended_record = if_else(cohabitation_closed, 1,
                                        cohabitation_ended_record),
    round = if_else(cohabitation_closed, 4, round),
    int_month = if_else(cohabitation_closed, update_month, int_month),
    int_year = if_else(cohabitation_closed, update_year, int_year)
  ) |>
  select(-cohabitation_closed, -prior_pgender, -new_coendm, -new_coendy,
         -new_partner_died, -update_month, -update_year)
r4_added_rows <- bind_rows(
  r4_added_rows,
  prior_partner_update |>
    filter(source_round == 4, cohabitation_ended) |>
    anti_join(r4_cohab_closures, by = "su_id") |>
    transmute(su_id, round = 4, cohabitation_record = 1,
              cohabitation_ended_record = 1, pgender = prior_pgender,
              cobeginm = new_cobeginm, cobeginy = new_cobeginy,
              coendm = new_coendm, coendy = new_coendy,
              partner_died = new_partner_died)
)

# Add a new intact cohabitation when needed.
r4_added_rows <- bind_rows(
  r4_added_rows,
  prior_partner_update |>
    filter(source_round == 4, new_cohabitation) |>
    anti_join(partnership_history |>
                filter(!is.na(cobeginy) | !is.na(cobeginm) |
                         coalesce(cohabitation_current_record, 0) == 1 |
                         coalesce(cohabitation_record, 0) == 1,
                       !coalesce(cohabitation_ended_record == 1, FALSE),
                       is.na(coendy), is.na(coendm),
                       is.na(marbeginy), is.na(marbeginm),
                       is.na(marendy), is.na(marendm),
                       coalesce(marriage_record, 0) != 1) |>
                distinct(su_id),
              by = "su_id") |>
    transmute(su_id, round = 4, cohabitation_record = 1,
              cohabitation_current_record = 1, pgender = prior_pgender,
              cobeginm = new_cobeginm, cobeginy = new_cobeginy)
)

partnership_history <- bind_rows(
  partnership_history |> select(-row_id),
  r4_added_rows |> left_join(r4_interview, by = "su_id")
)

# Merge the repeated current-spouse loops into the existing marriage.
partnership_history <- partnership_history |>
  mutate(row_id = row_number()) |>
  left_join(
    partnership_history |>
      mutate(row_id = row_number()) |>
      filter(
        su_id %in% filter(loop_marriages, source_round == 4,
                          current_marriage_repeat)$su_id,
        coalesce(marriage_record, 0) == 1 |
          !is.na(marbeginm) | !is.na(marbeginy),
        !coalesce(marriage_ended_record == 1, FALSE),
        is.na(marendm), is.na(marendy),
        is.na(mstatus) | mstatus == 1
      ) |>
      group_by(su_id) |>
      slice_max(order_by = round * 10000 + coalesce(marbeginy, 0),
                n = 1, with_ties = FALSE) |>
      ungroup() |>
      select(su_id, row_id) |>
      inner_join(
        loop_marriages |>
          filter(source_round == 4, current_marriage_repeat) |>
          transmute(
            su_id, update_pgender = pgender, update_page = page,
            update_pagerel = pagerel, update_fstsexy = fstsexy,
            update_marbeginm = marbeginm, update_marbeginy = marbeginy,
            update_livebfmr = livebfmr, update_cobeginm = cobeginm,
            update_cobeginy = cobeginy,
            update_cohabitation_record = cohabitation_record,
            update_cohabitation_ended_record = cohabitation_ended_record
          ),
        by = "su_id"
      ) |>
      left_join(r4_interview, by = "su_id") |>
      transmute(row_id, updated = TRUE,
                update_pgender, update_page, update_pagerel, update_fstsexy,
                update_marbeginm, update_marbeginy,
                update_livebfmr, update_cobeginm, update_cobeginy,
                update_cohabitation_record,
                update_cohabitation_ended_record,
                update_int_month = int_month, update_int_year = int_year),
    by = "row_id"
  ) |>
  mutate(
    updated = coalesce(updated, FALSE),
    pgender = coalesce(update_pgender, pgender),
    page = coalesce(update_page, page),
    pagerel = coalesce(update_pagerel, pagerel),
    fstsexy = coalesce(update_fstsexy, fstsexy),
    marbeginm = coalesce(marbeginm, update_marbeginm),
    marbeginy = coalesce(marbeginy, update_marbeginy),
    livebfmr = coalesce(update_livebfmr, livebfmr),
    cobeginm = coalesce(update_cobeginm, cobeginm),
    cobeginy = coalesce(update_cobeginy, cobeginy),
    cohabitation_record = coalesce(update_cohabitation_record,
                                   cohabitation_record),
    cohabitation_ended_record = coalesce(
      update_cohabitation_ended_record, cohabitation_ended_record
    ),
    marriage_record = if_else(updated, 1, marriage_record),
    marriage_ended_record = if_else(updated, NA, marriage_ended_record),
    mstatus = if_else(updated, 1, mstatus),
    round = if_else(updated, 4, round),
    int_month = if_else(updated, update_int_month, int_month),
    int_year = if_else(updated, update_int_year, int_year)
  ) |>
  select(-row_id, -updated, -starts_with("update_"))

partnership_history <- bind_rows(
  partnership_history,
  bind_rows(loop_marriages |> filter(!current_marriage_repeat),
            loop_cohabs) |>
    filter(source_round == 4) |>
    select(-source_round, -current_marriage_repeat) |>
    arrange(su_id, loop_number) |>
    mutate(round = 4) |>
    left_join(r4_interview, by = "su_id")
)

#### Remove double-reported marriages ####
# Keep the most complete of duplicate intact or ended marriages.
partnership_history <- partnership_history |>
  mutate(row_order = row_number()) |>
  group_by(su_id, marbeginy) |>
  mutate(
    intact = is.na(marendy) & is.na(marendm),
    duplicated_marriage = !is.na(marbeginy) & intact & sum(intact) > 1 &
      n_distinct(round[intact]) > 1 &
      n_distinct(marbeginm[intact], na.rm = TRUE) <= 1,
    reported_fields = rowSums(!is.na(pick(everything())))
  ) |>
  arrange(desc(reported_fields), .by_group = TRUE) |>
  filter(!duplicated_marriage | row_number() == 1) |>
  group_by(su_id, marendy, mstatus) |>
  mutate(
    duplicated_ending = !is.na(marendy) & n() > 1 &
      n_distinct(round) > 1 &
      n_distinct(marbeginy, na.rm = TRUE) <= 1 &
      n_distinct(marendm, na.rm = TRUE) <= 1
  ) |>
  arrange(desc(reported_fields), .by_group = TRUE) |>
  filter(!duplicated_ending | row_number() == 1) |>
  ungroup() |>
  arrange(row_order) |>
  select(-row_order, -intact, -duplicated_marriage, -duplicated_ending,
         -reported_fields)

#### Add current partnerships missing from the histories ####
# Latest reported marital status, interview date, and partnership details.
latest_current_partnership <- bind_rows(
  read_dta("../data/nshap_w1_core.dta") |>
    transmute(su_id, interview_round = 1, marital_status = as.numeric(maritlst),
              int_month = as.integer(int_start) %% 12 + 1,
              int_year = 1960 + as.integer(int_start) %/% 12),
  read_dta("../data/nshap_w2_core.dta") |>
    transmute(su_id, interview_round = 2, marital_status = as.numeric(maritlst),
              int_month = as.integer(int_start) %% 12 + 1,
              int_year = 1960 + as.integer(int_start) %/% 12),
  read_dta("../data/nshap_w3_core.dta") |>
    transmute(su_id, interview_round = 3, marital_status = as.numeric(maritlst),
              int_month = as.integer(int_start) %% 12 + 1,
              int_year = 1960 + as.integer(int_start) %/% 12),
  read_dta("../data/2024_03_20_nshap_r4_raw_plus_lbq.dta") |>
    transmute(su_id, interview_round = 4,
              marital_status = as.numeric(maritlst)) |>
    left_join(r4_interview, by = "su_id")
) |>
  filter(!is.na(marital_status) & marital_status > 0) |>
  group_by(su_id) |>
  summarise(maritlst_latest = marital_status[which.max(interview_round)],
            latest_round = max(interview_round),
            latest_interview_month = int_month[which.max(interview_round)],
            latest_interview_year = int_year[which.max(interview_round)],
            .groups = "drop") |>
  left_join(
    bind_rows(
      read_dta(
        "../data/nshap_w1_core.dta", col_select = c(su_id, pgender_1)
      ) |>
        zap_labels() |>
        transmute(
          su_id, source_round = 1,
          current_partner_marbeginm = NA, current_partner_marbeginy = NA,
          current_partner_livebfmr = NA, current_partner_livebfmrm = NA,
          current_partner_livebfmry = NA,
          current_partner_cobeginm = NA, current_partner_cobeginy = NA,
          current_partner_pgender = as.numeric(pgender_1),
          current_partner_page = NA, current_partner_pagerel = NA,
          current_partner_fstsexy = NA
        ),
      r2_interview |>
        transmute(
          su_id, source_round = 2,
          current_partner_marbeginm = NA, current_partner_marbeginy = NA,
          current_partner_livebfmr = NA, current_partner_livebfmrm = NA,
          current_partner_livebfmry = NA,
          current_partner_cobeginm = NA, current_partner_cobeginy = NA,
          current_partner_pgender = current_pgender, current_partner_page = NA,
          current_partner_pagerel = NA, current_partner_fstsexy = NA
        ),
      current_partnership_details
    ) |>
      rename(latest_round = source_round),
    by = c("su_id", "latest_round")
  )

# Fill current-partnership details on an existing row.
partnership_history <- partnership_history |>
  mutate(row_id = row_number())

partnership_history <- partnership_history |>
  left_join(
    partnership_history |>
      inner_join(latest_current_partnership |>
                   filter(maritlst_latest == 1), by = "su_id") |>
      filter((!is.na(marbeginm) | !is.na(marbeginy) |
                coalesce(marriage_record, 0) == 1),
             !coalesce(marriage_ended_record == 1, FALSE),
             is.na(marendm), is.na(marendy),
             is.na(mstatus) | mstatus == 1) |>
      group_by(su_id) |>
      slice_max(order_by = round * 10000 + coalesce(marbeginy, 0),
                n = 1, with_ties = FALSE) |>
      ungroup() |>
      select(row_id, starts_with("current_partner_"), latest_round,
             latest_interview_month, latest_interview_year) |>
      mutate(selected_current_marriage = TRUE),
    by = "row_id"
  ) |>
  mutate(
    current_marriage = coalesce(selected_current_marriage, FALSE),
    marbeginm = coalesce(marbeginm, current_partner_marbeginm),
    marbeginy = coalesce(marbeginy, current_partner_marbeginy),
    livebfmr = coalesce(livebfmr, current_partner_livebfmr),
    cobeginm = if_else(coalesce(current_partner_livebfmr == 1, FALSE),
                       coalesce(cobeginm, current_partner_livebfmrm), cobeginm),
    cobeginy = if_else(coalesce(current_partner_livebfmr == 1, FALSE),
                       coalesce(cobeginy, current_partner_livebfmry), cobeginy),
    pgender = coalesce(current_partner_pgender, pgender),
    page = coalesce(current_partner_page, page),
    pagerel = coalesce(current_partner_pagerel, pagerel),
    fstsexy = coalesce(fstsexy, current_partner_fstsexy),
    cohabitation_record = if_else(
      coalesce(current_partner_livebfmr == 1, FALSE), 1, cohabitation_record
    ),
    marriage_record = if_else(current_marriage, 1, marriage_record),
    marriage_ended_record = if_else(current_marriage, NA,
                                    marriage_ended_record),
    mstatus = if_else(current_marriage, 1, mstatus),
    round = if_else(current_marriage, latest_round, round),
    int_month = if_else(current_marriage, latest_interview_month, int_month),
    int_year = if_else(current_marriage, latest_interview_year, int_year)
  ) |>
  select(-current_marriage, -starts_with("current_partner_"),
         -latest_round, -latest_interview_month, -latest_interview_year)

partnership_history <- partnership_history |>
  left_join(
    partnership_history |>
      inner_join(latest_current_partnership |>
                   filter(maritlst_latest == 2), by = "su_id") |>
      filter((!is.na(cobeginm) | !is.na(cobeginy) |
                coalesce(cohabitation_record, 0) == 1),
             coalesce(marriage_record, 0) != 1 |
               coalesce(cohabitation_current_record, 0) == 1,
             !coalesce(cohabitation_ended_record == 1, FALSE),
             is.na(coendm), is.na(coendy)) |>
      group_by(su_id) |>
      slice_max(order_by = round * 10000 +
                  1000 * coalesce(cohabitation_current_record, 0) +
                  coalesce(cobeginy, 0),
                n = 1, with_ties = FALSE) |>
      ungroup() |>
      select(row_id, starts_with("current_partner_"), latest_round,
             latest_interview_month, latest_interview_year) |>
      mutate(selected_current_cohabitation = TRUE),
    by = "row_id"
  ) |>
  mutate(
    current_cohabitation = coalesce(selected_current_cohabitation, FALSE),
    cobeginm = coalesce(cobeginm, current_partner_cobeginm),
    cobeginy = coalesce(cobeginy, current_partner_cobeginy),
    pgender = coalesce(current_partner_pgender, pgender),
    page = coalesce(current_partner_page, page),
    pagerel = coalesce(current_partner_pagerel, pagerel),
    fstsexy = coalesce(fstsexy, current_partner_fstsexy),
    cohabitation_record = if_else(current_cohabitation,
                                  1, cohabitation_record),
    cohabitation_current_record = if_else(current_cohabitation,
                                          1, cohabitation_current_record),
    cohabitation_ended_record = if_else(current_cohabitation, NA,
                                        cohabitation_ended_record),
    mstatus = if_else(current_cohabitation &
                        coalesce(marriage_record, 0) != 1,
                      5, mstatus),
    round = if_else(current_cohabitation, latest_round, round),
    int_month = if_else(current_cohabitation, latest_interview_month,
                        int_month),
    int_year = if_else(current_cohabitation, latest_interview_year, int_year)
  ) |>
  select(-row_id, -current_cohabitation, -starts_with("current_partner_"),
         -latest_round, -latest_interview_month, -latest_interview_year)

# Add partnerships implied by status but absent from the history.
partnership_history <- bind_rows(
  partnership_history,
  latest_current_partnership |>
    left_join(
      partnership_history |>
        group_by(su_id) |>
        summarise(
          in_history = TRUE,
          any_marriage = any(!is.na(marbeginy) | !is.na(marbeginm) |
                          !is.na(marendy) | !is.na(marendm) |
                          coalesce(marriage_record, 0) == 1),
          any_intact_marriage = any((!is.na(marbeginy) | !is.na(marbeginm) |
                                  coalesce(marriage_record, 0) == 1) &
                                 !coalesce(marriage_ended_record == 1, FALSE) &
                                 !(mstatus %in% 2:4)),
          any_widowed = any(coalesce(mstatus, 0) == 4 |
                              coalesce(marriage_end_type, 0) == 1),
          any_intact_cohabitation = any(
            (!is.na(cobeginy) | !is.na(cobeginm) |
               coalesce(cohabitation_record, 0) == 1) &
              !coalesce(cohabitation_ended_record == 1, FALSE) &
              (coalesce(cohabitation_current_record, 0) == 1 |
                 (is.na(coendy) & is.na(coendm) &
                    coalesce(marriage_record, 0) != 1)),
            na.rm = TRUE
          ),
          .groups = "drop"
        ),
      by = "su_id"
    ) |>
    mutate(
      add_current_marriage = maritlst_latest == 1 &
        !coalesce(any_intact_marriage, FALSE),
      add_current_cohabitation = maritlst_latest == 2 &
        !coalesce(any_intact_cohabitation, FALSE),
      add_former_marriage = maritlst_latest %in% 3:5 &
        (!coalesce(any_marriage, FALSE) |
           (maritlst_latest == 5 & !coalesce(any_widowed, FALSE) &
              !coalesce(any_intact_marriage, FALSE))),
      add_placeholder = maritlst_latest == 6 & is.na(in_history)
    ) |>
    filter(add_current_marriage | add_current_cohabitation |
             add_former_marriage | add_placeholder) |>
    transmute(
      su_id, status_implied = TRUE,
      marriage_record = if_else(add_current_marriage | add_former_marriage,
                                1, NA),
      marriage_ended_record = if_else(add_former_marriage, 1, NA),
      cohabitation_record = if_else(
        add_current_cohabitation |
          (add_current_marriage &
             coalesce(current_partner_livebfmr == 1, FALSE)),
        1, NA
      ),
      cohabitation_current_record = if_else(add_current_cohabitation,
                                            1, NA),
      mstatus = case_when(
        add_current_marriage ~ 1,
        add_current_cohabitation | add_placeholder ~ 5,
        add_former_marriage & maritlst_latest == 3 ~ 3,
        add_former_marriage & maritlst_latest == 4 ~ 2,
        add_former_marriage & maritlst_latest == 5 ~ 4
      ),
      marbeginm = if_else(add_current_marriage, current_partner_marbeginm, NA),
      marbeginy = if_else(add_current_marriage, current_partner_marbeginy, NA),
      livebfmr = if_else(add_current_marriage, current_partner_livebfmr, NA),
      cobeginm = case_when(
        add_current_cohabitation ~ current_partner_cobeginm,
        add_current_marriage & coalesce(current_partner_livebfmr == 1, FALSE) ~
          current_partner_livebfmrm
      ),
      cobeginy = case_when(
        add_current_cohabitation ~ current_partner_cobeginy,
        add_current_marriage & coalesce(current_partner_livebfmr == 1, FALSE) ~
          current_partner_livebfmry
      ),
      pgender = if_else(add_current_marriage | add_current_cohabitation,
                        current_partner_pgender, NA),
      pagerel = if_else(add_current_marriage | add_current_cohabitation,
                        current_partner_pagerel, NA),
      fstsexy = if_else(add_current_marriage | add_current_cohabitation,
                        current_partner_fstsexy, NA),
      page = if_else(add_current_marriage | add_current_cohabitation,
                     current_partner_page, NA),
      selected_current_marriage = add_current_marriage,
      selected_current_cohabitation = add_current_cohabitation,
      round = latest_round,
      int_month = latest_interview_month,
      int_year = latest_interview_year
    )
)

#### mstatus, scid, and end reason ####
nshap_par_hisr1234 <- partnership_history |>
  left_join(latest_current_partnership |>
              select(su_id, maritlst_latest), by = "su_id") |>
  mutate(
    future_marriage_end = coalesce(marendy > int_year, FALSE) |
      coalesce(marendy == int_year & marendm > int_month, FALSE),
    future_cohabitation_end = coalesce(coendy > int_year, FALSE) |
      coalesce(coendy == int_year & coendm > int_month, FALSE),
    marriage_ended_record = if_else(future_marriage_end,
                                    1, marriage_ended_record),
    cohabitation_ended_record = if_else(future_cohabitation_end,
                                        1, cohabitation_ended_record),
    marendm = if_else(future_marriage_end, NA, marendm),
    marendy = if_else(future_marriage_end, NA, marendy),
    coendm = if_else(future_cohabitation_end, NA, coendm),
    coendy = if_else(future_cohabitation_end, NA, coendy),
    # mstatus: 1 married, 2 divorced, 3 separated, 4 widowed, 5 never married.
    mstatus = case_when(
      !is.na(mstatus) ~ mstatus,
      is.na(mstatus) & marriage_end_type == 1 ~ 4,
      is.na(mstatus) & marriage_end_type == 2 ~ 2,
      is.na(mstatus) & marriage_end_type == 3 ~ 3,
      is.na(mstatus) & (!is.na(marbeginy) | !is.na(marbeginm) |
                          coalesce(marriage_record, 0) == 1) &
        !coalesce(marriage_ended_record == 1, FALSE) &
        is.na(marendy) & is.na(marendm) &
        is.na(coendm) & is.na(coendy) ~ 1,
      is.na(mstatus) & (!is.na(marendy) | !is.na(marendm)) &
        round <= 2 & partner_died == 1 ~ 4,
      is.na(mstatus) & (!is.na(marendy) | !is.na(marendm)) &
        round <= 2 & partner_died == 2 ~ 2,
      is.na(mstatus) & (!is.na(marbeginy) | !is.na(marbeginm) |
                          coalesce(marriage_record, 0) == 1) &
        is.na(marendy) & is.na(marendm) &
        (!is.na(coendm) | !is.na(coendy)) ~ 3,
      is.na(mstatus) & is.na(marbeginy) & is.na(marbeginm) &
        is.na(marendy) & is.na(marendm) &
        coalesce(marriage_record, 0) != 1 ~ 5
    ),
    has_mar = !is.na(marbeginm) | !is.na(marbeginy) |
      !is.na(marendm) | !is.na(marendy) |
      coalesce(marriage_record, 0) == 1,
    has_coh = !is.na(cobeginm) | !is.na(cobeginy) |
      (!has_mar & (!is.na(coendm) | !is.na(coendy))) |
      coalesce(cohabitation_current_record, 0) == 1 |
      (!has_mar & coalesce(cohabitation_record, 0) == 1) |
      (has_mar & coalesce(livebfmr == 1, FALSE)),
    marriage_reported_end = has_mar &
      (coalesce(marriage_ended_record, 0) == 1 | !is.na(marendy) |
         !is.na(marendm) | mstatus %in% 2:4),
    marriage_order_year = case_when(
      !is.na(marbeginy) ~ marbeginy,
      !is.na(marendy) ~ marendy,
      !is.na(int_year) ~ int_year
    ),
    cohabitation_order_year = case_when(
      !is.na(cobeginy) ~ cobeginy,
      !is.na(coendy) ~ coendy,
      !is.na(int_year) ~ int_year
    )
  ) |>
  # Drop a marriage repeated across rounds with the same dates, keeping the
  # most complete row. A wedding date reported as both intact and ended is one
  # marriage that ended.
  group_by(su_id, mstatus, marbeginy, marbeginm, marendy, marendm) |>
  arrange(desc(rowSums(!is.na(pick(everything())))), .by_group = TRUE) |>
  filter(!has_mar | n_distinct(round) == 1 | row_number() == 1) |>
  group_by(su_id, marbeginy, marbeginm) |>
  filter(!has_mar | is.na(marbeginy) | is.na(marbeginm) |
           marriage_reported_end | !any(marriage_reported_end)) |>
  ungroup() |>
  # scid: marriages 1+, non-marital cohabitations 101+, placeholders 201+.
  group_by(su_id) |>
  mutate(
    scid = case_when(
      has_mar ~
        rank(if_else(has_mar, coalesce(marriage_order_year, 0) * 100 +
                       coalesce(marbeginm, 0), NA),
             ties.method = "first", na.last = "keep"),
      has_coh & !has_mar ~
        100 + rank(if_else(has_coh & !has_mar,
                           coalesce(cohabitation_order_year, 0) * 100 +
                             coalesce(cobeginm, 0), NA),
                   ties.method = "first", na.last = "keep"),
      TRUE ~ 200 + rank(if_else(!has_mar & !has_coh, coalesce(round, 0), NA),
                        ties.method = "first", na.last = "keep")
    ),
    # Reconcile unresolved records to the latest reported status.
    intact_marriage = has_mar &
      !coalesce(marriage_ended_record == 1, FALSE) &
      is.na(marendy) & is.na(marendm) &
      (is.na(mstatus) | mstatus == 1),
    has_selected_marriage = any(
      intact_marriage & coalesce(selected_current_marriage, FALSE)
    ),
    last_intact_marriage = max(if_else(
      intact_marriage &
        (coalesce(selected_current_marriage, FALSE) |
           !has_selected_marriage),
      scid, -Inf
    )),
    mstatus = case_when(
      intact_marriage & scid == last_intact_marriage & maritlst_latest == 3 ~ 3,
      intact_marriage & scid == last_intact_marriage & maritlst_latest == 4 ~ 2,
      intact_marriage & scid == last_intact_marriage & maritlst_latest == 5 ~ 4,
      intact_marriage & maritlst_latest != 1 & mstatus == 1 ~ NA,
      intact_marriage & maritlst_latest == 1 &
        scid != last_intact_marriage & mstatus == 1 ~ NA,
      TRUE ~ mstatus
    ),
    marriage_ended_record = case_when(
      intact_marriage & maritlst_latest != 1 ~ 1,
      intact_marriage & maritlst_latest == 1 & scid != last_intact_marriage ~ 1,
      TRUE ~ marriage_ended_record
    ),
    intact_cohabitation = has_coh &
      !coalesce(cohabitation_ended_record == 1, FALSE) &
      (coalesce(cohabitation_current_record, 0) == 1 |
         (!has_mar & is.na(coendm) & is.na(coendy))),
    has_selected_cohabitation = any(
      intact_cohabitation & coalesce(selected_current_cohabitation, FALSE)
    ),
    last_intact_cohabitation = max(if_else(
      intact_cohabitation &
        (coalesce(selected_current_cohabitation, FALSE) |
           !has_selected_cohabitation),
      scid, -Inf
    )),
    cohabitation_current_record = case_when(
      intact_cohabitation & maritlst_latest == 2 &
        scid == last_intact_cohabitation ~ 1,
      intact_cohabitation ~ NA,
      TRUE ~ cohabitation_current_record
    ),
    cohabitation_ended_record = case_when(
      intact_cohabitation & !(maritlst_latest == 2 &
                       scid == last_intact_cohabitation) ~ 1,
      TRUE ~ cohabitation_ended_record
    )
  ) |>
  ungroup() |>
  mutate(
    # End reason is missing while the relationship is intact.
    end_reason = case_when(
      has_mar & mstatus == 4 ~ "widowed",
      has_mar & mstatus %in% c(2, 3) ~ "divorced_separated",
      has_mar & (!is.na(marendy) | !is.na(marendm) |
                   coalesce(marriage_ended_record, 0) == 1) ~
        "ended_unknown",
      has_mar ~ NA,
      has_coh & partner_died == 1 ~ "partner_died",
      has_coh & partner_died == 2 ~ "ended_other",
      has_coh & (!is.na(coendy) | !is.na(coendm) |
                   coalesce(cohabitation_ended_record, 0) == 1) ~
        "ended_unknown"
    ),
    mar_ended = has_mar &
      (!is.na(end_reason) | coalesce(marriage_ended_record, 0) == 1),
    coh_ended = case_when(
      !has_coh ~ FALSE,
      coalesce(cohabitation_current_record, 0) == 1 ~ FALSE,
      coalesce(cohabitation_ended_record, 0) == 1 ~ TRUE,
      has_mar ~ TRUE,
      !is.na(coendy) | !is.na(coendm) ~ TRUE,
      TRUE ~ FALSE
    ),
    # Earliest partnership year, or the first-sex year if none was reported.
    beginy_est = case_when(
      !is.na(marbeginy) & !is.na(cobeginy) ~ pmin(marbeginy, cobeginy),
      !is.na(marbeginy) ~ marbeginy,
      !is.na(cobeginy) ~ cobeginy,
      !is.na(fstsexy) & fstsexy >= 1900 ~ fstsexy
    ),
    page = if_else(page > 0 & page <= 120, page, NA),
    fstsexy = if_else(fstsexy >= 1900 & fstsexy <= int_year, fstsexy, NA),
    status_implied = coalesce(status_implied, FALSE)
  ) |>
  # Respondents with no partnerships keep one placeholder row.
  group_by(su_id) |>
  filter(has_mar | has_coh | (!any(has_mar | has_coh) & row_number() == 1)) |>
  ungroup() |>
  arrange(su_id, scid) |>
  select(su_id, scid, round, mstatus, marbeginm, marbeginy,
         marendm, marendy, livebfmr, cobeginm, cobeginy, coendm, coendy,
         fstsexy, beginy_est, pgender, page, pagerel, has_mar, has_coh,
         mar_ended, coh_ended, end_reason, status_implied,
         int_month, int_year, maritlst_latest)

#### Partnership-history trajectories (7/11/14 categories) ####
# Partnership history typologies with 14, 11, and 7 categories.
respondent_trajectories <- bind_rows(
  r3_snapshot |> mutate(snapshot = 3, latest_status = current_status),
  nshap_par_hisr1234 |> mutate(snapshot = 4, latest_status = maritlst_latest)
) |>
  group_by(snapshot, su_id) |>
  summarise(
    n_marriages = sum(has_mar),
    n_cohabitations = sum(has_coh),
    n_nonmarital_cohabitations = sum(has_coh & !has_mar),
    n_divorced_separated = sum(has_mar & end_reason == "divorced_separated",
                        na.rm = TRUE),
    n_widowed = sum(has_mar & end_reason == "widowed", na.rm = TRUE),
    n_ended_unknown = sum(has_mar & end_reason == "ended_unknown",
                              na.rm = TRUE),
    unknown_dissolution = any(end_reason == "ended_unknown", na.rm = TRUE),
    ever_widowed = any((has_mar & end_reason == "widowed") |
                         latest_status == 5, na.rm = TRUE),
    any_intact_marriage = any(has_mar & !mar_ended),
    any_intact_cohabitation = any(has_coh & !coh_ended),
    .groups = "drop"
  ) |>
  mutate(
    part_history_14 = case_when(
      n_marriages == 0 & n_nonmarital_cohabitations == 0 ~
        "Never partnered",
      n_marriages == 0 & n_nonmarital_cohabitations == 1 &
        any_intact_cohabitation ~
        "Continuously cohabiting",
      n_marriages == 0 & n_nonmarital_cohabitations >= 2 &
        any_intact_cohabitation ~
        "Cohabiting, after prior cohabitation(s)",
      n_marriages == 0 ~
        "Formerly cohabited, never married",
      n_marriages == 1 & any_intact_marriage ~
        "Continuously married",
      n_marriages >= 2 & any_intact_marriage & n_divorced_separated == 1 &
        n_widowed == 0 & n_ended_unknown == 0 ~
        "Remarried, after divorce/separation",
      n_marriages >= 2 & any_intact_marriage & n_widowed == 1 &
        n_divorced_separated == 0 & n_ended_unknown == 0 ~
        "Remarried, after widowhood",
      n_marriages >= 2 & any_intact_marriage &
        (n_divorced_separated + n_widowed + n_ended_unknown) > 1 ~
        "Remarried, after multiple dissolutions",
      n_marriages >= 2 & any_intact_marriage ~
        "Remarried, prior dissolution(s) unknown",
      !any_intact_marriage & any_intact_cohabitation ~
        "Cohabiting, formerly married",
      !any_intact_marriage & n_divorced_separated == 1 & n_widowed == 0 &
        n_ended_unknown == 0 ~
        "Divorced/separated",
      !any_intact_marriage & n_widowed == 1 & n_divorced_separated == 0 &
        n_ended_unknown == 0 ~
        "Widowed",
      !any_intact_marriage & n_ended_unknown == 0 ~
        "Formerly married, multiple dissolutions",
      !any_intact_marriage ~
        "Formerly married, dissolution(s) unknown",
      TRUE ~ "Other"
    ),
    part_history_11 = case_when(
      n_marriages == 0 & n_nonmarital_cohabitations == 0 ~ "Never partnered",
      n_marriages == 0 & any_intact_cohabitation ~ "Cohabiting, never married",
      n_marriages == 0 ~ "Formerly cohabited, never married",
      n_marriages == 1 & any_intact_marriage ~ "Continuously married",
      n_marriages >= 2 & any_intact_marriage & n_divorced_separated == 1 &
        n_widowed == 0 & n_ended_unknown == 0 ~
        "Remarried, after divorce/separation",
      n_marriages >= 2 & any_intact_marriage & n_widowed == 1 &
        n_divorced_separated == 0 & n_ended_unknown == 0 ~
        "Remarried, after widowhood",
      n_marriages >= 2 & any_intact_marriage ~
        "Remarried, multiple or unknown dissolutions",
      !any_intact_marriage & any_intact_cohabitation ~
        "Cohabiting, formerly married",
      !any_intact_marriage & n_divorced_separated == 1 & n_widowed == 0 &
        n_ended_unknown == 0 ~ "Divorced/separated",
      !any_intact_marriage & n_widowed == 1 & n_divorced_separated == 0 &
        n_ended_unknown == 0 ~ "Widowed",
      !any_intact_marriage ~
        "Formerly married, multiple or unknown dissolutions",
      TRUE ~ "Other"
    ),
    part_history_7 = case_when(
      n_marriages == 0 & n_nonmarital_cohabitations == 0 ~ "Never partnered",
      n_marriages == 0 & any_intact_cohabitation ~ "Currently cohabiting",
      n_marriages == 0 ~ "Formerly cohabited",
      any_intact_marriage & n_marriages == 1 ~ "Continuously married",
      any_intact_marriage ~ "Remarried",
      !any_intact_marriage & any_intact_cohabitation ~ "Currently cohabiting",
      !any_intact_marriage & n_widowed >= 1 ~ "Widowed",
      !any_intact_marriage ~ "Divorced/separated",
      TRUE ~ "Other"
    )
  )

nshap_par_hisr1234 <- nshap_par_hisr1234 |>
  left_join(respondent_trajectories |>
              filter(snapshot == 4) |>
              select(su_id, part_history_14, part_history_11,
                     part_history_7),
            by = "su_id") |>
  # Other sex partners are only counted. Each round asks about partners since
  # the last interview, or the past five years at a first interview.
  left_join(
    bind_rows(
      read_dta("../data/nshap_w1_core.dta",
               col_select = c(su_id, aotrsex, sexlt5yr)) |>
        zap_labels() |>
        transmute(su_id, round = 1,
                  n_partners = coalesce(as.numeric(aotrsex),
                                       as.numeric(sexlt5yr))),
      read_dta("../data/nshap_w2_partnerhistory.dta",
               col_select = c(su_id, numothsex)) |>
        zap_labels() |>
        group_by(su_id) |>
        summarise(round = 2,
                  n_partners = max(as.numeric(numothsex), -Inf, na.rm = TRUE),
                  .groups = "drop"),
      read_dta("../data/nshapw3_main.dta",
               col_select = c(su_id, othsexpart, numothsex)) |>
        zap_labels() |>
        transmute(su_id, round = 3,
                  n_partners = if_else(as.numeric(othsexpart) == 2, 0,
                                       as.numeric(numothsex))),
      read_dta("../data/2024_03_20_nshap_r4_raw_plus_lbq.dta",
               col_select = c(su_id, othsexpart, numothsex)) |>
        zap_labels() |>
        transmute(su_id, round = 4,
                  n_partners = if_else(as.numeric(othsexpart) == 2, 0,
                                       as.numeric(numothsex)))
    ) |>
      mutate(n_partners = if_else(is.finite(n_partners) & n_partners >= 0,
                                  n_partners, NA)) |>
      group_by(su_id) |>
      summarise(n_othersex_rounds = if_else(all(is.na(n_partners)), NA,
                                            sum(n_partners, na.rm = TRUE)),
                .groups = "drop"),
    by = "su_id"
  )

r3_trajectories <- respondent_trajectories |>
  filter(snapshot == 3) |>
  transmute(su_id,
            r3_part_history_14 = part_history_14,
            r3_part_history_11 = part_history_11,
            r3_part_history_7 = part_history_7,
            r3_unknown_dissolution = unknown_dissolution,
            r3_n_marriages = n_marriages,
            r3_n_cohabitations = n_cohabitations,
            r3_ever_widowed = ever_widowed)

saveRDS(nshap_par_hisr1234, "output/nshap_par_hisr1234.rds")
saveRDS(r3_trajectories, "../tmp/nshap_partnership_history_round3.rds")

# Stata version with variable and value labels.
write_dta(
  nshap_par_hisr1234 |>
    mutate(
      su_id = labelled(su_id, label = "Respondent identifier"),
      scid = labelled(
        scid, label = "Derived partnership order within respondent"
      ),
      round = labelled(
        round, c("Round 1" = 1, "Round 2" = 2, "Round 3" = 3, "Round 4" = 4),
        label = "Round of the latest update contributing to the row"
      ),
      mstatus = labelled(
        mstatus,
        c("married" = 1, "divorced" = 2, "separated" = 3, "widowed" = 4,
          "never married this partner" = 5),
        label = "Marital status of the relationship on that row"
      ),
      marbeginm = labelled(marbeginm, label = "Reported marriage start month"),
      marbeginy = labelled(marbeginy, label = "Reported marriage start year"),
      marendm = labelled(marendm, label = "Reported marriage end month"),
      marendy = labelled(marendy, label = "Reported marriage end year"),
      livebfmr = labelled(
        livebfmr, c("yes" = 1, "no" = 2),
        label = "Lived together before marriage"
      ),
      cobeginm = labelled(
        cobeginm, label = "Reported cohabitation start month"
      ),
      cobeginy = labelled(
        cobeginy, label = "Reported cohabitation start year"
      ),
      coendm = labelled(coendm, label = "Reported cohabitation end month"),
      coendy = labelled(coendy, label = "Reported cohabitation end year"),
      fstsexy = labelled(
        fstsexy, label = "Reported year of first sex with this partner"
      ),
      beginy_est = labelled(
        beginy_est,
        label = "Earliest reported partnership year, first-sex year fallback"
      ),
      pgender = labelled(
        pgender, c("male" = 1, "female" = 2), label = "Partner gender"
      ),
      page = labelled(page, label = "Partner age"),
      pagerel = labelled(
        pagerel, c("older" = 1, "younger" = 2, "same age" = 3),
        label = "Partner age relative to the respondent"
      ),
      has_mar = labelled(
        as.integer(has_mar), c("no" = 0L, "yes" = 1L),
        label = "Row contains a marriage"
      ),
      has_coh = labelled(
        as.integer(has_coh), c("no" = 0L, "yes" = 1L),
        label = "Row contains cohabitation, including premarital cohabitation"
      ),
      mar_ended = labelled(
        as.integer(mar_ended), c("no" = 0L, "yes" = 1L),
        label = "Marriage had ended by the row's latest update"
      ),
      coh_ended = labelled(
        as.integer(coh_ended), c("no" = 0L, "yes" = 1L),
        label = "Cohabitation had ended or transitioned to marriage"
      ),
      end_reason = labelled(
        end_reason,
        label = "Relationship ending category, missing while intact"
      ),
      int_month = labelled(
        int_month, label = "Interview month of the latest update"
      ),
      int_year = labelled(
        int_year, label = "Interview year of the latest update"
      ),
      maritlst_latest = labelled(
        maritlst_latest,
        c("married" = 1, "living with a partner" = 2, "separated" = 3,
          "divorced" = 4, "widowed" = 5, "never married" = 6),
        label = "Respondent's latest reported marital status"
      ),
      part_history_14 = labelled(
        part_history_14, label = "Final partnership history in 14 categories"
      ),
      part_history_11 = labelled(
        part_history_11, label = "Final partnership history in 11 categories"
      ),
      part_history_7 = labelled(
        part_history_7, label = "Final partnership history in 7 categories"
      ),
      n_othersex_rounds = labelled(
        n_othersex_rounds,
        label = "Other sex partners reported, summed over rounds attended"
      ),
      status_implied = labelled(
        as.integer(status_implied), c("no" = 0L, "yes" = 1L),
        label = "Row implied by latest marital status, not directly reported"
      )
    ),
  "output/nshap_par_hisr1234.dta"
)
