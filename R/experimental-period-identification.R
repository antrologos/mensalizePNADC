#' Experimental Period Identification Strategies
#'
#' Provides experimental strategies for improving period identification rates
#' beyond the standard deterministic algorithm. All strategies respect the nested
#' identification hierarchy: weeks require fortnights, fortnights require months.
#'
#' @description
#' Three experimental strategies are available, all properly nested by period:
#' \itemize{
#'   \item \strong{probabilistic}: For narrow ranges (2 possible periods),
#'     classifies based on where most of the date interval falls. Assigns only
#'     when confidence exceeds threshold.
#'   \item \strong{upa_aggregation}: Extends strictly identified periods to other
#'     observations in the same UPA-V1014 within the quarter, if a sufficient
#'     proportion already have strict identification.
#'   \item \strong{both}: Sequentially applies probabilistic strategy first, then
#'     UPA aggregation on top. Guarantees identification rate >= max of individual strategies.
#' }
#'
#' @param crosswalk A crosswalk data.table from \code{pnadc_identify_periods()}
#' @param strategy Character specifying which strategy to apply.
#'   Options: "none", "probabilistic", "upa_aggregation", "both"
#' @param confidence_threshold Numeric (0-1). Minimum confidence required to
#'   assign a probabilistic period. Used by probabilistic and combined strategies.
#'   Default 0.9.
#' @param upa_proportion_threshold Numeric (0-1). Minimum proportion of UPA
#'   observations (within quarter) that must have strict identification with consensus
#'   for extending to unidentified observations. Default 0.5.
#' @param verbose Logical. If TRUE, print progress information.
#'
#' @return A modified crosswalk with additional columns.
#'   (default), output is directly compatible with \code{pnadc_apply_periods()}:
#'   \itemize{
#'     \item \strong{Experimental columns} (always included):
#'     \itemize{
#'       \item \code{ref_month_in_quarter_aggreg}: Experimentally assigned month position in quarter (1-3, or NA)
#'       \item \code{ref_month_in_quarter_aggreg_confidence}: Confidence of month assignment (0-1, proportion
#'         of date interval in assigned period; values below threshold are removed)
#'       \item \code{ref_fortnight_in_month_aggreg}: Experimentally assigned fortnight position in quarter (1-6, or NA)
#'       \item \code{ref_fortnight_in_month_aggreg_confidence}: Confidence of fortnight assignment (0-1)
#'       \item \code{ref_week_in_month_aggreg}: Experimentally assigned week position in quarter (1-12, or NA)
#'       \item \code{ref_week_in_month_aggreg_confidence}: Confidence of week assignment (0-1)
#'     }
#'     \item \strong{Derived columns} (when \code{include_derived = TRUE}):
#'     \itemize{
#'       \item \code{ref_month_start}, \code{ref_month_end}, \code{ref_month_in_quarter},
#'         \code{ref_month_yyyymm}, \code{ref_month_weeks}: Combined strict + experimental month
#'         (strict takes priority). Start/end are Sunday/Saturday of IBGE month boundaries.
#'       \item \code{determined_month}: TRUE if month is assigned (strictly or experimentally)
#'       \item \code{ref_fortnight_start}, \code{ref_fortnight_end}, \code{ref_fortnight_in_quarter},
#'         \code{ref_fortnight_yyyyff}: Combined strict + experimental fortnight.
#'         Start/end are Sunday/Saturday of IBGE fortnight boundaries.
#'       \item \code{determined_fortnight}: TRUE if fortnight is assigned
#'       \item \code{ref_week_start}, \code{ref_week_end}, \code{ref_week_in_quarter},
#'         \code{ref_week_yyyyww}: Combined strict + experimental week.
#'         Start/end are Sunday/Saturday of IBGE week boundaries.
#'       \item \code{determined_week}: TRUE if week is assigned
#'       \item \code{probabilistic_assignment}: TRUE if any period was assigned experimentally
#'         (vs strictly deterministic)
#'     }
#'   }
#'
#' @details
#' ## Nesting Enforcement
#'
#' All strategies enforce proper nesting:
#' \itemize{
#'   \item Fortnights can only be assigned if month is identified (strictly OR experimentally)
#'   \item Weeks can only be assigned if fortnight is identified (strictly OR experimentally)
#' }
#'
#' ## Probabilistic Strategy
#'
#' For each period type (processed in order: months, then fortnights, then weeks):
#' \enumerate{
#'   \item Check that the required parent period is identified
#'   \item If bounds are narrowed to exactly 2 sequential periods, calculate
#'     which period contains most of the date interval
#'   \item Calculate confidence based on the proportion of interval in the likely period (0-1)
#'   \item Only assign if confidence >= \code{confidence_threshold}
#' }
#'
#' For months: aggregates at UPA-V1014 level (but within-quarter, unlike strict algorithm)
#' For fortnights and weeks: works at household level
#'
#' ## UPA Aggregation Strategy
#'
#' Extends strictly identified periods based on consensus within geographic groups:
#' \itemize{
#'   \item \strong{Months}: Uses UPA-V1014 level (panel design ensures same month)
#'   \item \strong{Fortnights/Weeks}: Uses UPA level (all households in same UPA
#'     are interviewed in same fortnight/week within a quarter)
#' }
#' \enumerate{
#'   \item Calculate proportion of observations with strictly identified period
#'   \item If proportion >= \code{upa_proportion_threshold} AND consensus exists, extend
#'   \item Apply in nested order: months first, then fortnights, then weeks
#' }
#'
#' ## Combined Strategy ("both")
#'
#' Sequentially applies both strategies to maximize identification:
#' \enumerate{
#'   \item First, apply the probabilistic strategy (captures observations with
#'     narrow date ranges and high confidence)
#'   \item Then, apply UPA aggregation (extends based on strict consensus
#'     within UPA/UPA-V1014 groups)
#' }
#'
#' This guarantees that "both" identifies at least as many observations as
#' either individual strategy alone. The strategies operate independently
#' (UPA aggregation considers only strict identifications), so the result
#' is the union of both strategies.
#'
#' ## Integration with Weight Calibration
#'
#' The output can be passed directly to \code{pnadc_apply_periods()} for weight calibration.
#' The derived columns combine strict and experimental assignments, with strict taking priority. Use the
#' \code{probabilistic_assignment} flag to filter if you only want strict determinations.
#'
#' @note
#' These strategies produce "experimental" assignments, not strict determinations.
#' The standard \code{pnadc_identify_periods()} function should be used for
#' rigorous analysis. Experimental outputs are useful for:
#' \itemize{
#'   \item Sensitivity analysis
#'   \item Robustness checks
#'   \item Research into identification algorithm improvements
#' }
#'
#' @seealso
#' \code{\link{pnadc_identify_periods}} to build the crosswalk that this function modifies.
#' \code{\link{pnadc_apply_periods}} to apply period crosswalk and calibrate weights.
#'
#' @examples
#' \dontrun{
#' # Build standard crosswalk
#' crosswalk <- pnadc_identify_periods(pnadc_data)
#'
#' # Apply experimental strategies
#' crosswalk_exp <- pnadc_experimental_periods(
#'   crosswalk,
#'   strategy = "probabilistic",
#'   confidence_threshold = 0.9
#' )
#'
#' # Check how many additional assignments we get
#' crosswalk_exp[, .(
#'   strict = sum(!is.na(ref_month_in_quarter) & !probabilistic_assignment),
#'   experimental = sum(probabilistic_assignment, na.rm = TRUE),
#'   total = sum(determined_month)
#' )]
#'
#' # Use directly with calibration (experimental output is compatible)
#' result <- pnadc_apply_periods(pnadc_data, crosswalk_exp,
#'                               period = "month", calibrate = TRUE)
#'
#' # Or filter to only strict determinations
#' strict_only <- crosswalk_exp[probabilistic_assignment == FALSE | is.na(probabilistic_assignment)]
#' }
#'
#' @export
pnadc_experimental_periods <- function(
    crosswalk,
    strategy = c("probabilistic", "upa_aggregation", "both"),
    confidence_threshold = 0.9,
    upa_proportion_threshold = 0.5,
    verbose = TRUE
) {

  strategy <- match.arg(strategy)

  # Validate
  checkmate::assert_data_frame(crosswalk)
  checkmate::assert_number(confidence_threshold, lower = 0, upper = 1)
  checkmate::assert_number(upa_proportion_threshold, lower = 0, upper = 1)
  checkmate::assert_logical(verbose)


  # Copy crosswalk to avoid modifying original
  result <- data.table::copy(crosswalk)

  # ==========================================================================
  # STRATEGY DISPATCH
  # ==========================================================================

  if (strategy == "probabilistic") {
    result <- PNADCperiods:::.apply_probabilistic(
      crosswalk = result,
      confidence_threshold = confidence_threshold,
      verbose = verbose
    )
  } else if (strategy == "upa_aggregation") {
    result <- PNADCperiods:::.apply_upa_aggregation(
      crosswalk = result,
      threshold = upa_proportion_threshold,
      verbose   = verbose)
  } else if (strategy == "both") {
    result <- PNADCperiods:::.apply_combined_strategy(
      crosswalk = result,
      confidence_threshold = confidence_threshold,
      upa_threshold        = upa_proportion_threshold,
      verbose              = verbose)
  }

  result
}


# =============================================================================
# PROBABILISTIC STRATEGY (Nested) - OPTIMIZED
# =============================================================================

#' Apply Probabilistic Strategy with Proper Nesting (Optimized)
#'
#' Uses date bounds stored in crosswalk from
#' pnadc_identify_periods(store_date_bounds=TRUE). This avoids:
#' - Copying and processing the full data (~90% of computation)
#' - Redundant date calculations
#' - Multiple aggregations
#'
#' @param crosswalk Crosswalk data.table (may contain pre-computed date bounds)
#' @param confidence_threshold Minimum confidence to assign
#' @param verbose Print progress
#' @return Modified crosswalk with probabilistic columns
#'
#' @keywords internal
#' @noRd
.apply_probabilistic <- function(crosswalk, confidence_threshold, verbose) {

  # ==========================================================================
  # PRE-COMPUTE ALL IBGE BOUNDARIES ONCE
  # ==========================================================================

  calendar_1 <- unique(crosswalk[ref_month_in_quarter == 1,
                                 .(Ano, Trimestre,

                                   week_1_start,
                                   week_1_end,
                                   week_2_start,
                                   week_2_end,
                                   week_3_start,
                                   week_3_end,
                                   week_4_start,
                                   week_4_end)])

  calendar_2 <- unique(crosswalk[ref_month_in_quarter == 2,
                                 .(Ano, Trimestre,

                                   week_5_start = week_1_start,
                                   week_5_end   = week_1_end,
                                   week_6_start = week_2_start,
                                   week_6_end   = week_2_end,
                                   week_7_start = week_3_start,
                                   week_7_end   = week_3_end,
                                   week_8_start = week_4_start,
                                   week_8_end   = week_4_end)])


  calendar_3 <- unique(crosswalk[ref_month_in_quarter ==3,
                                 .(Ano, Trimestre,

                                   week_9_start  = week_1_start,
                                   week_9_end    = week_1_end,
                                   week_10_start = week_2_start,
                                   week_10_end   = week_2_end,
                                   week_11_start = week_3_start,
                                   week_11_end   = week_3_end,
                                   week_12_start = week_4_start,
                                   week_12_end   = week_4_end)])

  data.table::setkey(calendar_1, Ano, Trimestre)
  data.table::setkey(calendar_2, Ano, Trimestre)
  data.table::setkey(calendar_3, Ano, Trimestre)

  calendar_quarter_weeks = calendar_1 |> merge(calendar_2, all.x = T) |> merge(calendar_3, all.x = T)


  calendar_month_weeks <- unique(crosswalk[!is.na(ref_month_in_quarter),
                                           .(Ano, Trimestre, ref_month_in_quarter,

                                             week_1_start,
                                             week_1_end,
                                             week_2_start,
                                             week_2_end,
                                             week_3_start,
                                             week_3_end,
                                             week_4_start,
                                             week_4_end)])


  rm(calendar_1,
     calendar_2,
     calendar_3); gc()

  # ==========================================================================
  # STEP 1: MONTH PROBABILISTIC
  # ==========================================================================


  # Pre-computing the calendar
  # ==========================================================================

  crosswalk[ , week_1_start := NULL]
  crosswalk[ , week_1_end   := NULL]
  crosswalk[ , week_2_start := NULL]
  crosswalk[ , week_2_end   := NULL]
  crosswalk[ , week_3_start := NULL]
  crosswalk[ , week_3_end   := NULL]
  crosswalk[ , week_4_start := NULL]
  crosswalk[ , week_4_end   := NULL]

  if (verbose) cat("  Phase 1: Month probabilistic identification...\n")

  # Set key for faster operations
  data.table::setkey(crosswalk, Ano, Trimestre, UPA, V1014)

  # Calculate month range
  crosswalk[, month_range := month_max_upa - month_min_upa + 1]

  # For range == 2, sequential, not strictly determined: calculate probabilistic assignment
  crosswalk[, month_prob_filter :=
              month_range == 2L &
              is.na(ref_month_in_quarter) &
              !is.na(date_min) &
              !is.na(date_max)
  ]

  # Joining with the calendar
  crosswalk[calendar_quarter_weeks,
            on = .(Ano, Trimestre),
            `:=`(week_1_start = i.week_1_start,
                 week_1_end   = i.week_1_end,
                 week_2_start = i.week_2_start,
                 week_2_end   = i.week_2_end,
                 week_3_start = i.week_3_start,
                 week_3_end   = i.week_3_end,
                 week_4_start = i.week_4_start,
                 week_4_end   = i.week_4_end,

                 week_5_start  = i.week_5_start,
                 week_5_end    = i.week_5_end,
                 week_6_start  = i.week_6_start,
                 week_6_end    = i.week_6_end,
                 week_7_start  = i.week_7_start,
                 week_7_end    = i.week_7_end,
                 week_8_start  = i.week_8_start,
                 week_8_end    = i.week_8_end,

                 week_9_start  = i.week_9_start,
                 week_9_end    = i.week_9_end,
                 week_10_start = i.week_10_start,
                 week_10_end   = i.week_10_end,
                 week_11_start = i.week_11_start,
                 week_11_end   = i.week_11_end,
                 week_12_start = i.week_12_start,
                 week_12_end   = i.week_12_end

            )]

  # When possible months are Months 1 and 2 within quarter
  # ==========================================================================

  # Adjusting minimum and maximum possible dates for observation whose probable months were narrowed down to 1 or 2
  crosswalk[month_prob_filter == 1 & month_max_upa == 2 & month_min_upa == 1,
            `:=`(
              date_min = fifelse(date_min < week_1_start, week_1_start, date_min),
              date_max = fifelse(date_max > week_8_end,   week_8_end,   date_max)
            )]

  # Calculating days within months 1 and 2
  crosswalk[month_prob_filter == 1 & month_max_upa == 2 & month_min_upa == 1,
            `:=`(
              days_within_month_1_min1_max2 = pmax(0, week_4_end - date_min     + 1),
              days_within_month_2_min1_max2 = pmax(0, date_max   - week_5_start + 1)
            )]
  attributes(crosswalk$days_within_month_1_min1_max2) <- NULL
  attributes(crosswalk$days_within_month_2_min1_max2) <- NULL

  # Calculating share of days within months 1 and 2
  crosswalk[month_prob_filter == 1 & month_max_upa == 2 & month_min_upa == 1,
            `:=`(
              prop_month_1_min1_max2 = days_within_month_1_min1_max2/(days_within_month_1_min1_max2 + days_within_month_2_min1_max2 + 1e-10),
              prop_month_2_min1_max2 = days_within_month_2_min1_max2/(days_within_month_1_min1_max2 + days_within_month_2_min1_max2 + 1e-10)
            )]

  # Assigning probable months
  crosswalk[month_prob_filter == 1 & month_max_upa == 2 & month_min_upa == 1,
            `:=`(
              prob_month_min1_max2 = fcase(prop_month_1_min1_max2 >= confidence_threshold, 1,
                                           prop_month_2_min1_max2 >= confidence_threshold, 2,
                                           default = NA),

              determined_probable_month = TRUE)]


  # When possible months are Months 2 and 3 within quarter
  # ==========================================================================

  # Adjusting minimum and maximum possible dates for observation whose probable months were narrowed down to 2 or 3
  crosswalk[month_prob_filter == 1 & month_max_upa == 3 & month_min_upa == 2,
            `:=`(
              date_min = fifelse(date_min < week_5_start, week_5_start, date_min),
              date_max = fifelse(date_max > week_12_end,   week_12_end, date_max)
            )]

  # Calculating days within months 2 and 3
  crosswalk[month_prob_filter == 1 & month_max_upa == 3 & month_min_upa == 2,
            `:=`(
              days_within_month_2_min2_max3 = pmax(0, week_8_end - date_min     + 1),
              days_within_month_3_min2_max3 = pmax(0, date_max   - week_9_start + 1)
            )]
  attributes(crosswalk$days_within_month_2_min2_max3) <- NULL
  attributes(crosswalk$days_within_month_3_min2_max3) <- NULL

  # Calculating share of days within months 2 and 3
  crosswalk[month_prob_filter == 1 & month_max_upa == 3 & month_min_upa == 2,
            `:=`(
              prop_month_2_min2_max3 = days_within_month_2_min2_max3/(days_within_month_2_min2_max3 + days_within_month_3_min2_max3 + 1e-10),
              prop_month_3_min2_max3 = days_within_month_3_min2_max3/(days_within_month_2_min2_max3 + days_within_month_3_min2_max3 + 1e-10)
            )]

  # Assigning probable months
  crosswalk[month_prob_filter == 1 & month_max_upa == 3 & month_min_upa == 2,
            `:=`(
              prob_month_min2_max3 = fcase(prop_month_2_min2_max3 >= confidence_threshold, 2,
                                           prop_month_3_min2_max3 >= confidence_threshold, 3,
                                           default = NA))]


  # Assigning Months
  # ==========================================================================

  crosswalk[month_prob_filter == 1 & determined_month == F,
            `:=`(
              prob_ref_month_in_quarter = fcase(!is.na(prob_month_min1_max2), prob_month_min1_max2,
                                                !is.na(prob_month_min2_max3), prob_month_min2_max3,
                                                default = NA)
            )]


  crosswalk[, determined_probable_month := fifelse(!is.na(prob_ref_month_in_quarter), TRUE, FALSE)]

  # Aggregating by UPA-V1014 across quarters
  crosswalk[month_prob_filter == 1,
            `:=`(
              prob_ref_month_in_quarter_min = min(prob_ref_month_in_quarter, na.rm = T),
              prob_ref_month_in_quarter_max = max(prob_ref_month_in_quarter, na.rm = T)
            ),
            by = c("UPA", "V1014")]


  crosswalk[month_prob_filter == 1 & prob_ref_month_in_quarter_min == prob_ref_month_in_quarter_max,

            `:=`(ref_month_in_quarter = prob_ref_month_in_quarter_min,
                 determined_month = TRUE,
                 probabilistic_assignment = TRUE)]


  crosswalk[month_prob_filter == 1 & !is.na(ref_month_in_quarter),
            `:=`(
              ref_month_in_year = ref_month_in_quarter + (Trimestre - 1)*3
            )]


  # Cleaning up
  # ==========================================================================

  crosswalk[,
            `:=`(
              days_within_month_1_min1_max2 = NULL,
              days_within_month_2_min1_max2 = NULL,
              prop_month_1_min1_max2        = NULL,
              prop_month_2_min1_max2        = NULL,
              prob_month_min1_max2          = NULL,

              days_within_month_2_min2_max3 = NULL,
              days_within_month_3_min2_max3 = NULL,
              prop_month_2_min2_max3        = NULL,
              prop_month_3_min2_max3        = NULL,
              prob_month_min2_max3          = NULL,
              prob_ref_month_in_quarter     = NULL,
              prob_ref_month_in_quarter_min = NULL,
              prob_ref_month_in_quarter_max = NULL,

              week_1_start  = NULL,
              week_1_end    = NULL,
              week_2_start  = NULL,
              week_2_end    = NULL,
              week_3_start  = NULL,
              week_3_end    = NULL,
              week_4_start  = NULL,
              week_4_end    = NULL,

              week_5_start  = NULL,
              week_5_end    = NULL,
              week_6_start  = NULL,
              week_6_end    = NULL,
              week_7_start  = NULL,
              week_7_end    = NULL,
              week_8_start  = NULL,
              week_8_end    = NULL,

              week_9_start  = NULL,
              week_9_end    = NULL,
              week_10_start = NULL,
              week_10_end   = NULL,
              week_11_start = NULL,
              week_11_end   = NULL,
              week_12_start = NULL,
              week_12_end   = NULL,

              month_range       = NULL,
              month_prob_filter = NULL
            )]

  gc()


  # Stats
  # ==========================================================================

  n_total      <- crosswalk[, .N]
  n_month_prob <- sum(crosswalk$determined_probable_month, na.rm = T)

  if (verbose) {
    cat(sprintf("    Assigned months to %s observations, representing %.1f%% of the total (confidence >= %.0f%%)\n",
                format(n_month_prob, big.mark = ","),
                n_month_prob/n_total * 100,
                confidence_threshold * 100))
  }


  # ==========================================================================
  # STEP 2: FORTNIGHT PROBABILISTIC (using pre-computed integer bounds)
  # ==========================================================================

  if (verbose) cat("  Phase 2: Fortnight probabilistic identification (nested)...\n")


  # For range == 2, sequential, not strictly determined: calculate probabilistic assignment
  crosswalk[, fortnight_prob_filter :=
              !is.na(ref_month_in_quarter) &
              #probabilistic_assignment == T &
              is.na(ref_fortnight_in_month) &
              !is.na(date_min) &
              !is.na(date_max)
  ]


  # When possible fortnights are fortnights 1 and 2 within months
  # ==========================================================================


  # Freeing memory after deleting columns
  gc()

  # Joining with the calendar
  crosswalk[calendar_month_weeks,
            on = .(Ano, Trimestre, ref_month_in_quarter),
            `:=`(week_1_start = i.week_1_start,
                 week_1_end   = i.week_1_end,
                 week_2_start = i.week_2_start,
                 week_2_end   = i.week_2_end,
                 week_3_start = i.week_3_start,
                 week_3_end   = i.week_3_end,
                 week_4_start = i.week_4_start,
                 week_4_end   = i.week_4_end
            )]

  # Adjusting minimum and maximum possible dates for observation whose probable fortnights were narrowed down to 1 or 2
  crosswalk[fortnight_prob_filter == 1,
            `:=`(
              date_min = fifelse(date_min < week_1_start, week_1_start, date_min),
              date_max = fifelse(date_max > week_4_end,   week_4_end,   date_max)
            )]

  # Calculating days within fortnights 1 and 2
  crosswalk[fortnight_prob_filter == 1,
            `:=`(
              days_within_fortnight_1 = pmax(0, week_2_end - date_min     + 1),
              days_within_fortnight_2 = pmax(0, date_max   - week_3_start + 1)
            )]
  attributes(crosswalk$days_within_fortnight_1) <- NULL
  attributes(crosswalk$days_within_fortnight_2) <- NULL

  # Calculating share of days within fortnights 1 and 2
  crosswalk[fortnight_prob_filter == 1,
            `:=`(
              prop_fortnight_1 = days_within_fortnight_1/(days_within_fortnight_1 + days_within_fortnight_2 + 1e-10),
              prop_fortnight_2 = days_within_fortnight_2/(days_within_fortnight_1 + days_within_fortnight_2 + 1e-10)
            )]


  # Assigning fortnights
  # ==========================================================================

  # Assigning probable fortnights
  crosswalk[fortnight_prob_filter == 1,

            prob_ref_fortnight_in_month := fcase(prop_fortnight_1 >= confidence_threshold, 1,
                                                 prop_fortnight_2 >= confidence_threshold, 2,
                                                 default = NA)]


  # Flagging if fortnight was determined probabilistically
  crosswalk[, determined_probable_fortnight := fifelse(!is.na(prob_ref_fortnight_in_month), TRUE, FALSE)]


  # Aggregating by household within quarter (not by UPA-V1014 across quarters)
  crosswalk[fortnight_prob_filter == 1,
            `:=`(
              prob_ref_fortnight_in_month_min = min(prob_ref_fortnight_in_month, na.rm = T),
              prob_ref_fortnight_in_month_max = max(prob_ref_fortnight_in_month, na.rm = T)
            ),
            by = c("Ano", "Trimestre", "UPA", "V1008")]


  crosswalk[fortnight_prob_filter == 1 & prob_ref_fortnight_in_month_min == prob_ref_fortnight_in_month_max,

            `:=`(ref_fortnight_in_month = prob_ref_fortnight_in_month_min,
                 determined_fortnight = TRUE,
                 probabilistic_assignment = TRUE)]


  crosswalk[fortnight_prob_filter == 1 & !is.na(ref_fortnight_in_month),
            `:=`(
              ref_fortnight_in_quarter = ref_fortnight_in_month + (Trimestre - 1)*2
            )]



  # Cleaning up
  # ==========================================================================

  crosswalk[,
            `:=`(
              days_within_fortnight_1 = NULL,
              days_within_fortnight_2 = NULL,
              prop_fortnight_1 = NULL,
              prop_fortnight_2 = NULL,
              prob_ref_fortnight_in_month     = NULL,
              prob_ref_fortnight_in_month_min = NULL,
              prob_ref_fortnight_in_month_max = NULL,

              fortnight_prob_filter = NULL)]

  gc()


  # Stats
  # ==========================================================================

  n_total          <- crosswalk[, .N]
  n_fortnight_prob <- sum(crosswalk$determined_probable_fortnight, na.rm = T)

  if (verbose) {
    cat(sprintf("    Assigned fortnights to %s observations, representing %.1f%% of the total (confidence >= %.0f%%)\n",
                format(n_fortnight_prob, big.mark = ","),
                n_fortnight_prob/n_total * 100,
                confidence_threshold * 100))
  }


  # ==========================================================================
  # STEP 3: WEEK PROBABILISTIC (using pre-computed integer bounds)
  # ==========================================================================

  if (verbose) cat("  Phase 3: Week probabilistic identification (nested)...\n")

  # For range == 2, sequential, not strictly determined: calculate probabilistic assignment
  crosswalk[, week_prob_filter :=
              !is.na(ref_fortnight_in_month) &
              #probabilistic_assignment == T &
              is.na(ref_week_in_month) &
              !is.na(date_min) &
              !is.na(date_max)
  ]

  # NESTING: Only process observations with identified fortnight

  # When possible weeks are weeks 1 and 2 within month
  # ==========================================================================

  # Adjusting minimum and maximum possible dates for observation whose probable weeks within month were narrowed down to 1 or 2
  crosswalk[week_prob_filter == 1 & ref_fortnight_in_month == 1,
            `:=`(
              date_min = fifelse(date_min < week_1_start, week_1_start, date_min),
              date_max = fifelse(date_max > week_2_end,   week_2_end,   date_max)
            )]

  # Calculating days within weeks 1 and 2
  crosswalk[week_prob_filter == 1 & ref_fortnight_in_month == 1,
            `:=`(
              days_within_week_1 = pmax(0, week_1_end - date_min     + 1),
              days_within_week_2 = pmax(0, date_max   - week_2_start + 1)
            )]
  attributes(crosswalk$days_within_week_1) <- NULL
  attributes(crosswalk$days_within_week_2) <- NULL

  # Calculating share of days within weeks 1 and 2
  crosswalk[week_prob_filter == 1 & ref_fortnight_in_month == 1,
            `:=`(
              prop_week_1 = days_within_week_1/(days_within_week_1 + days_within_week_2 + 1e-10),
              prop_week_2 = days_within_week_2/(days_within_week_1 + days_within_week_2 + 1e-10)
            )]


  crosswalk[week_prob_filter == 1 & ref_fortnight_in_month == 1,
            `:=`(
              prob_week_1_2 = fcase(prop_week_1 >= confidence_threshold, 1,
                                    prop_week_2 >= confidence_threshold, 2,
                                    default = NA))]


  # When possible weeks are weeks 3 and 4 within month
  # ==========================================================================

  # Adjusting minimum and maximum possible dates for observation whose probable weeks within month were narrowed down to 3 or 4
  crosswalk[week_prob_filter == 1 & ref_fortnight_in_month == 2,
            `:=`(
              date_min = fifelse(date_min < week_3_start, week_3_start, date_min),
              date_max = fifelse(date_max > week_4_end,   week_4_end,   date_max)
            )]

  # Calculating days within weeks 3 and 4
  crosswalk[week_prob_filter == 1 & ref_fortnight_in_month == 2,
            `:=`(
              days_within_week_3 = pmax(0, week_1_end - date_min     + 1),
              days_within_week_4 = pmax(0, date_max   - week_2_start + 1)
            )]
  attributes(crosswalk$days_within_week_3) <- NULL
  attributes(crosswalk$days_within_week_4) <- NULL

  # Calculating share of days within weeks 3 and 4
  crosswalk[week_prob_filter == 1 & ref_fortnight_in_month == 2,
            `:=`(
              prop_week_3 = days_within_week_3/(days_within_week_3 + days_within_week_4 + 1e-10),
              prop_week_4 = days_within_week_4/(days_within_week_3 + days_within_week_4 + 1e-10)
            )]


  # Assigning probable weeks
  crosswalk[week_prob_filter == 1 & ref_fortnight_in_month == 2,
            `:=`(
              prob_week_3_4 = fcase(prop_week_3 >= confidence_threshold, 3,
                                    prop_week_4 >= confidence_threshold, 4,
                                    default = NA))]


  # Assigning Probable weeks
  # ==========================================================================

  crosswalk[week_prob_filter == 1 & determined_week == F,
            `:=`(
              prob_ref_week_in_month = fcase(!is.na(prob_week_1_2), prob_week_1_2,
                                             !is.na(prob_week_3_4), prob_week_3_4,
                                             default = NA)
            )]

  # Flagging if week was determined probabilistically
  crosswalk[, determined_probable_week := fifelse(!is.na(prob_ref_week_in_month), TRUE, FALSE)]

  # Aggregating by households within quarters (not by UPA-V1014 across quarters)
  crosswalk[week_prob_filter == 1,
            `:=`(
              prob_ref_week_in_month_min = min(prob_ref_week_in_month, na.rm = T),
              prob_ref_week_in_month_max = max(prob_ref_week_in_month, na.rm = T)
            ),
            by = c("Ano", "Trimestre", "UPA", "V1008")]


  crosswalk[week_prob_filter == 1 & prob_ref_week_in_month_min == prob_ref_week_in_month_max,

            `:=`(ref_week_in_month = prob_ref_week_in_month_min,
                 determined_week  = TRUE,
                 probabilistic_assignment = TRUE)]


  crosswalk[week_prob_filter == 1 & !is.na(ref_week_in_quarter),
            `:=`(
              ref_week_in_quarter = ref_week_in_month + (Trimestre - 1)*4
            )]

  # Cleaning up
  # ==========================================================================

  crosswalk[,
            `:=`(
              days_within_week_1 = NULL,
              days_within_week_2 = NULL,
              days_within_week_3 = NULL,
              days_within_week_4 = NULL,

              prop_week_1 = NULL,
              prop_week_2 = NULL,
              prop_week_3 = NULL,
              prop_week_4 = NULL,

              prob_week_1_2 = NULL,
              prob_week_3_4 = NULL,

              week_prob_filter = NULL,

              prob_ref_week_in_month     = NULL,
              prob_ref_week_in_month_min = NULL,
              prob_ref_week_in_month_max = NULL)]

  gc()


  # Stats
  # ==========================================================================

  n_week_prob <- sum(crosswalk$determined_probable_week, na.rm = T)

  if (verbose) {
    cat(sprintf("    Assigned weeks to %s observations, representing %.1f%% of the total (confidence >= %.0f%%)\n",
                format(n_week_prob, big.mark = ","),
                n_week_prob/n_total * 100,
                confidence_threshold * 100))
  }


  # ==========================================================================
  # Re-organizing the crosswalk
  # ==========================================================================

  crosswalk[, `:=`(
    ref_month_yyyymm     = Ano * 100 + ref_month_in_year,
    ref_fortnight_yyyyff = Ano * 100 + ref_fortnight_in_quarter + (Trimestre - 1)*6,
    ref_week_yyyyww      = Ano * 100 + ref_week_in_month + (ref_month_in_year - 1)*4

  )]


  crosswalk <- crosswalk[, .(Ano, Trimestre, UPA, V1008, V1014,

                             ref_month_in_quarter,     ref_month_in_year,
                             ref_fortnight_in_month,   ref_fortnight_in_quarter,
                             ref_week_in_month,        ref_week_in_quarter,

                             date_max,                 date_min,
                             month_max_upa,            month_min_upa,
                             fortnight_max_hh,         fortnight_min_hh,
                             week_min_hh,              week_max_hh,

                             week_1_start, week_1_end,   week_2_start,  week_2_end,
                             week_3_start, week_3_end,   week_4_start,  week_4_end,

                             ref_month_yyyymm, ref_fortnight_yyyyff, ref_week_yyyyww,

                             determined_month, determined_fortnight, determined_week,

                             determined_probable_fortnight,
                             determined_probable_week,
                             determined_probable_month,
                             probabilistic_assignment)]

  gc()

  if (verbose) cat("  Probabilistic strategy complete.\n")

  crosswalk
}


# =============================================================================
# UPA AGGREGATION STRATEGY (Nested)
# =============================================================================

#' Apply UPA Aggregation Strategy with Proper Nesting
#'
#' Extends strictly identified periods to unidentified observations based on
#' consensus within geographic/sampling groups:
#' \itemize{
#'   \item \strong{Months}: Aggregates at UPA-V1014 level (panel design ensures
#'     same month position across quarters)
#'   \item \strong{Fortnights/Weeks}: Aggregates at UPA level (all households in
#'     same UPA are interviewed in same fortnight/week within a quarter,
#'     regardless of panel rotation V1014)
#' }
#'
#' @param crosswalk Crosswalk data.table
#' @param threshold Minimum proportion of observations with strict identification
#'   required before extending to unidentified observations
#' @param verbose Print progress
#' @return Modified crosswalk with UPA-aggregated columns
#' @keywords internal
#' @noRd
.apply_upa_aggregation <- function(crosswalk, threshold, verbose) {

  if (verbose) cat("Applying UPA aggregation strategy (nested)...\n")

  # ==========================================================================
  # STEP 1: MONTH AGGREGATION
  # ==========================================================================

  if (verbose) cat("  Phase 1: Month UPA aggregation...\n")

  # tmp columns
  crosswalk[, `:=`(
    ref_month_in_quarter_aggreg            = NA_real_,
    ref_month_in_quarter_aggreg_confidence = NA_real_)]


  # Calculate proportion of UPA within-quarter with identified month
  upa_month_stats <- crosswalk[, .(
    n_total = .N,
    n_month = sum(!is.na(ref_month_in_quarter)),
    consensus_month = fifelse(
      uniqueN(ref_month_in_quarter[!is.na(ref_month_in_quarter)]) == 1L,
      ref_month_in_quarter[!is.na(ref_month_in_quarter)][1L],
      NA_integer_
    )
  ), by = .(Ano, Trimestre, UPA)]

  upa_month_stats[, prop_month := n_month / n_total]

  # Only extend if proportion >= threshold AND there's a consensus
  upa_month_qualify <- upa_month_stats[prop_month >= threshold & !is.na(consensus_month)]

  if (nrow(upa_month_qualify) > 0) {
    # Join and assign to observations without strict month
    crosswalk[upa_month_qualify, on = .(Ano, Trimestre, UPA), `:=`(
      ref_month_in_quarter_aggreg            = fifelse(is.na(ref_month_in_quarter), i.consensus_month, ref_month_in_quarter_aggreg),
      ref_month_in_quarter_aggreg_confidence = fifelse(is.na(ref_month_in_quarter), 1.0, ref_month_in_quarter_aggreg_confidence)
    )]
  }

  n_month_upa <- sum(!is.na(crosswalk$ref_month_in_quarter_aggreg) & is.na(crosswalk$ref_month_in_quarter))
  if (verbose) {
    cat(sprintf("    Extended to %s observations via UPA aggregation\n",
                format(n_month_upa, big.mark = ",")))
  }

  # ==========================================================================
  # STEP 2: FORTNIGHT AGGREGATION (NESTED)
  # ==========================================================================

  if (verbose) cat("  Phase 2: Fortnight UPA aggregation (nested)...\n")

  crosswalk[, `:=`(
    ref_fortnight_in_month_aggreg            = NA_real_,
    ref_fortnight_in_month_aggreg_confidence = NA_real_)]

  # Calculate proportion of UPA (not UPA-V1014) within-quarter with identified fortnight
  upa_fortnight_stats <- crosswalk[, .(
    n_total = .N,
    n_fortnight = sum(!is.na(ref_fortnight_in_month)),
    consensus_fortnight = fifelse(
      uniqueN(ref_fortnight_in_month[!is.na(ref_fortnight_in_month)]) == 1L,
      ref_fortnight_in_month[!is.na(ref_fortnight_in_month)][1L],
      NA_integer_
    )
  ), by = .(Ano, Trimestre, UPA)]

  upa_fortnight_stats[, prop_fortnight := n_fortnight / n_total]

  # Only extend if proportion >= threshold AND there's a consensus
  upa_fortnight_qualify <- upa_fortnight_stats[prop_fortnight >= threshold & !is.na(consensus_fortnight)]

  if (nrow(upa_fortnight_qualify) > 0) {
    # NESTING: Only assign to observations with identified month
    # AND verify consensus_fortnight falls within that month's bounds
    crosswalk[, month_identified := !is.na(ref_month_in_quarter) | !is.na(ref_month_in_quarter_aggreg)]
    crosswalk[month_identified == TRUE, `:=`(
      effective_month =  fifelse(!is.na(ref_month_in_quarter), ref_month_in_quarter, ref_month_in_quarter_aggreg),
      fortnight_lower = (fifelse(!is.na(ref_month_in_quarter), ref_month_in_quarter, ref_month_in_quarter_aggreg) - 1L) + 1L,
      fortnight_upper =  fifelse(!is.na(ref_month_in_quarter), ref_month_in_quarter, ref_month_in_quarter_aggreg)
    )]

    # Join consensus_fortnight and validate it's within the month's fortnight bounds
    # INVARIANT: Also validate consensus_fortnight is in [1, 6] range
    crosswalk[upa_fortnight_qualify, on = .(Ano, Trimestre, UPA), `:=`(
      ref_fortnight_in_month_aggreg = fifelse(
        is.na(ref_fortnight_in_quarter) & month_identified &
          i.consensus_fortnight >= fortnight_lower &
          i.consensus_fortnight <= fortnight_upper &
          i.consensus_fortnight >= 1L & i.consensus_fortnight <= 6L,
        i.consensus_fortnight,
        ref_fortnight_in_month_aggreg
      ),
      ref_fortnight_in_month_aggreg_confidence = fifelse(
        is.na(ref_fortnight_in_quarter) & month_identified &
          i.consensus_fortnight >= fortnight_lower &
          i.consensus_fortnight <= fortnight_upper,
        1.0,
        ref_fortnight_in_month_aggreg_confidence
      )
    )]

    crosswalk[, c("month_identified", "effective_month", "fortnight_lower", "fortnight_upper") := NULL]
  }

  n_fortnight_upa <- sum(!is.na(crosswalk$ref_fortnight_in_month_aggreg) & is.na(crosswalk$ref_fortnight_in_quarter))
  if (verbose) {
    cat(sprintf("    Extended to %s observations via UPA aggregation\n",
                format(n_fortnight_upa, big.mark = ",")))
  }

  # ==========================================================================
  # STEP 3: WEEK AGGREGATION (NESTED)
  # ==========================================================================

  if (verbose) cat("  Phase 3: Week UPA aggregation (nested)...\n")

  crosswalk[, `:=`(
    ref_week_in_month_aggreg            = NA_real_,
    ref_week_in_month_aggreg_confidence = NA_real_)]

  # Calculate proportion of UPA within-quarter with identified week
  upa_week_stats <- crosswalk[, .(
    n_total = .N,
    n_week = sum(!is.na(ref_week_in_month)),
    consensus_week = fifelse(
      uniqueN(ref_week_in_month[!is.na(ref_week_in_month)]) == 1L,
      ref_week_in_month[!is.na(ref_week_in_month)][1L],
      NA_integer_
    )
  ), by = .(Ano, Trimestre, UPA)]

  upa_week_stats[, prop_week := n_week / n_total]

  # Only extend if proportion >= threshold AND there's a consensus
  upa_week_qualify <- upa_week_stats[prop_week >= threshold & !is.na(consensus_week)]

  if (nrow(upa_week_qualify) > 0) {
    # NESTING: Only assign to observations with identified fortnight
    # Note: Full week-within-fortnight validation would require complex date calculations.
    # Since consensus_week comes from strictly identified weeks (which were already
    # constrained to their fortnight bounds during strict identification), and we require
    # fortnight_identified, the assignment should be valid. We rely on the strict
    # algorithm's nesting enforcement for the source consensus values.
    crosswalk[, fortnight_identified := !is.na(ref_fortnight_in_quarter) | !is.na(ref_fortnight_in_month_aggreg)]

    # INVARIANT: Validate consensus_week is in [1, 4] range
    crosswalk[upa_week_qualify, on = .(Ano, Trimestre, UPA), `:=`(
      ref_week_in_month_aggreg = fifelse(
        is.na(ref_week_in_quarter) & fortnight_identified &
          i.consensus_week >= 1L & i.consensus_week <= 4L,
        i.consensus_week,
        ref_week_in_month_aggreg
      ),
      ref_week_in_month_aggreg_confidence = fifelse(
        is.na(ref_week_in_quarter) & fortnight_identified &
          i.consensus_week >= 1L & i.consensus_week <= 4L,
        1.0,
        ref_week_in_month_aggreg_confidence
      )
    )]

    crosswalk[, fortnight_identified := NULL]
  }

  n_week_upa <- sum(!is.na(crosswalk$ref_week_in_month_aggreg) & is.na(crosswalk$ref_week_in_quarter))
  if (verbose) {
    cat(sprintf("    Extended to %s observations via UPA aggregation\n",
                format(n_week_upa, big.mark = ",")))
  }




  crosswalk[ ,
             `:=`(

               ref_month_in_quarter = fifelse(is.na(ref_month_in_quarter) & !is.na(ref_month_in_quarter_aggreg),
                                              ref_month_in_quarter_aggreg,
                                              ref_month_in_quarter),

               ref_fortnight_in_month = fifelse(is.na(ref_fortnight_in_month) & !is.na(ref_fortnight_in_month_aggreg),
                                                ref_fortnight_in_month_aggreg,
                                                ref_fortnight_in_month),

               ref_week_in_month = fifelse(is.na(ref_week_in_month) & !is.na(ref_week_in_month_aggreg),
                                           ref_week_in_month_aggreg,
                                           ref_week_in_month),

               determined_aggreg_month = fifelse(!is.na(ref_month_in_quarter_aggreg), TRUE, FALSE),
               determined_month        = fifelse(!is.na(ref_month_in_quarter_aggreg), TRUE, determined_month),

               determined_aggreg_fortnight = fifelse(!is.na(ref_fortnight_in_month_aggreg), TRUE, FALSE),
               determined_fortnight        = fifelse(!is.na(ref_fortnight_in_month_aggreg), TRUE, determined_fortnight),

               determined_aggreg_week = fifelse(!is.na(ref_week_in_month_aggreg), TRUE, FALSE),
               determined_week        = fifelse(!is.na(ref_week_in_month_aggreg), TRUE, determined_week)
    )]

  crosswalk[, `:=`(
            ref_month_in_quarter_aggreg               = NULL,
            ref_month_in_quarter_aggreg_confidence    = NULL,
            ref_fortnight_in_month_aggreg             = NULL,
            ref_fortnight_in_month_aggreg_confidence  = NULL,
            ref_week_in_month_aggreg                  = NULL,
            ref_week_in_month_aggreg_confidence       = NULL)]

  crosswalk[, `:=`(
    ref_month_in_year        = ref_month_in_year + (Trimestre - 1)*3,
    ref_fortnight_in_quarter = ref_fortnight_in_month + (ref_month_in_quarter - 1)*2,
    ref_week_in_quarter      = ref_week_in_month + (ref_month_in_quarter - 1)*4

    )]

  crosswalk[, `:=`(
    ref_month_yyyymm     = Ano * 100 + ref_month_in_year,
    ref_fortnight_yyyyff = Ano * 100 + (ref_fortnight_in_quarter + (Trimestre - 1)*6),
    ref_week_yyyyww      = Ano * 100 + (ref_week_in_month + (ref_month_in_year - 1)*4)

  )]


  crosswalk[, probabilistic_assignment := determined_aggreg_month | determined_aggreg_fortnight | determined_aggreg_week]


  if (verbose) {
    cat("  UPA aggregation strategy complete.\n")
  }

  crosswalk
}


# =============================================================================
# COMBINED STRATEGY (Nested)
# =============================================================================

#' Apply Combined Strategy (Probabilistic + UPA Aggregation) with Proper Nesting
#'
#' Sequentially applies both strategies: probabilistic first, then UPA aggregation.
#' This guarantees that "both" identifies at least as many periods as either
#' individual strategy alone.
#'
#' The previous implementation had an extra `prop_narrow` filter that made it
#' more restrictive than probabilistic alone. This refactored version ensures
#' the union behavior users expect from the "both" option.
#'
#' @param crosswalk Crosswalk data.table
#' @param data Original PNADC data
#' @param confidence_threshold Minimum confidence to assign
#' @param upa_threshold Minimum proportion of UPA with narrow ranges
#' @param verbose Print progress
#' @return Modified crosswalk
#' @keywords internal
#' @noRd
.apply_combined_strategy <- function(crosswalk, confidence_threshold, upa_threshold, verbose) {

  if (verbose) cat("Applying combined strategy (probabilistic + UPA aggregation, nested)...\n")

  # ==========================================================================
  # STEP 1: Apply probabilistic strategy first
  # ==========================================================================

  crosswalk <- .apply_probabilistic(crosswalk = crosswalk,
                                    confidence_threshold = confidence_threshold,
                                    verbose = verbose)

  # ==========================================================================
  # STEP 2: Apply UPA aggregation on top
  # ==========================================================================

  crosswalk <- .apply_upa_aggregation(crosswalk = crosswalk,
                                      threshold = upa_threshold,
                                      verbose   = verbose)


  if (verbose) {
    cat("  Combined strategy complete.\n")
  }

  crosswalk
}


