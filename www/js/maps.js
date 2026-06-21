// maps.js — Leaflet can't compute its size while its tab is hidden, so when any nav tab
// becomes visible we fire a window resize event; Leaflet listens for that and calls
// invalidateSize(), so the Maps tab draws at the correct size on first view. Harmless on
// other tabs. Paired with suspendWhenHidden = FALSE on the map output (R/modules/maps.R).
$(document).on('shown.bs.tab', function () {
  setTimeout(function () { window.dispatchEvent(new Event('resize')); }, 10);
});
