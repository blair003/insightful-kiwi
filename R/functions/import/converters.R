# converters.R — registry mapping a manifest raw$converter name to its function.
# The import pipeline (ik_convert_raw) looks up the converter here. Add one entry
# per source export format; the converter itself lives in converters/<name>.R.
ik_converters <- list(
  wkt_trapping = convert_wkt_trapping
)
