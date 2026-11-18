# Align completed rules without forward-filling order instructions. Intervals
# govern state visibility, event emission and pending entry expiry.
.align_daily_rules <- function(execution, rules, state_cols, event_cols,
                               marker_times = NULL) {
  x <- data.table::copy(data.table::as.data.table(execution))
  r <- data.table::copy(data.table::as.data.table(rules))
  keys <- c("RuleTime", "AvailableAt", "ValidUntil")
  finite_time <- function(v) inherits(v, "POSIXct") && all(is.finite(v))
  if (!finite_time(x$date) || anyDuplicated(x$date))
    stop("Execution timestamps must be finite, unique POSIXct values.")
  if (!all(keys %in% names(r)) || !all(vapply(r[, ..keys], finite_time, logical(1))))
    stop("Rules require finite POSIXct RuleTime, AvailableAt and ValidUntil.")
  if (any(r$RuleTime > r$AvailableAt) || any(r$AvailableAt >= r$ValidUntil))
    stop("Rules require RuleTime <= AvailableAt < ValidUntil.")
  data.table::setorder(x, date)
  data.table::setorder(r, AvailableAt)
  if (anyDuplicated(r$AvailableAt) || (nrow(r) > 1L &&
      any(head(r$ValidUntil, -1L) > tail(r$AvailableAt, -1L))))
    stop("Rule validity intervals must not overlap.")
  fields <- c(state_cols, event_cols)
  if (anyDuplicated(fields) || !all(fields %in% names(r)) ||
      any(c(fields, "StateRuleTime", "OrderRuleTime", "NextOpen") %in% names(x)))
    stop("State/event columns must be distinct, present in rules and absent from execution.")
  if (!"OrderSize" %in% event_cols) stop("event_cols must include OrderSize.")
  for (nm in fields) x[, (nm) := r[[nm]][rep(NA_integer_, .N)]]
  x[, `:=`(StateRuleTime = as.POSIXct(NA, tz="UTC"),
           OrderRuleTime = as.POSIXct(NA, tz="UTC"),
           EntryValidUntil = as.POSIXct(NA, tz="UTC"), NextOpen = NA_real_)]
  x[, OrderSize := 0]
  k <- findInterval(as.numeric(x$date), as.numeric(r$AvailableAt))
  eligible <- which(k > 0L)
  eligible <- eligible[x$date[eligible] < r$ValidUntil[k[eligible]]]
  for (nm in state_cols) data.table::set(x, i=eligible, j=nm, value=r[[nm]][k[eligible]])
  data.table::set(x, i=eligible, j="StateRuleTime", value=r$RuleTime[k[eligible]])
  events <- r[!is.na(OrderSize) & OrderSize != 0]
  if (nrow(events)) {
    activation <- events$AvailableAt
    if ("EntryTime" %in% event_cols) {
      if (!finite_time(events$EntryTime)) stop("Order EntryTime must be finite POSIXct.")
      activation <- pmax(activation, events$EntryTime)
    }
    # First observed execution bar at or after activation, within this interval.
    j <- findInterval(as.numeric(activation), as.numeric(x$date))
    exact <- j > 0L
    exact[exact] <- x$date[j[exact]] == activation[exact]
    j <- j + !exact
    keep <- j <= nrow(x)
    keep[keep] <- x$date[j[keep]] < events$ValidUntil[keep]
    if (anyDuplicated(j[keep])) stop("Multiple orders would be emitted on the same bar.")
    for (nm in event_cols) data.table::set(x, i=j[keep], j=nm, value=events[[nm]][keep])
    data.table::set(x, i=j[keep], j="OrderRuleTime", value=events$RuleTime[keep])
    data.table::set(x, i=j[keep], j="EntryValidUntil", value=events$ValidUntil[keep])
    # Market orders need the actual eligible timestamp if their scheduled bar is absent.
    if ("EntryTime" %in% event_cols)
      data.table::set(x, i=j[keep], j="EntryTime", value=x$date[j[keep]])
    unplaced <- events[!keep]
  } else unplaced <- events
  if (!is.null(marker_times)) {
    if (!finite_time(marker_times) || anyDuplicated(marker_times))
      stop("Marker timestamps must be finite and unique.")
    # A marker is a clock tick, not a future opening price. Exact positions keep
    # the supplied aggregation clock; missing marker bars are reported.
    m <- match(marker_times, x$date)
    data.table::set(x, i=m[!is.na(m)], j="NextOpen", value=1)
    missing_markers <- marker_times[is.na(m)]
  } else missing_markers <- as.POSIXct(character(), tz="UTC")
  list(data=x, audit=list(unplaced_events=unplaced, missing_markers=missing_markers,
                         emitted_events=sum(x$OrderSize != 0), state_rows=length(eligible)))
}
