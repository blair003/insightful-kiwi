# converters.R — register raw-dataset converters in ik_registry. A manifest
# raw$converter value is looked up via ik_registered("converter", name) in
# ik_convert_raw(). Add one registration per source export format; the converter
# itself lives in converters/<name>.R.
ik_register("converter", "wkt_trapping", convert_wkt_trapping)
