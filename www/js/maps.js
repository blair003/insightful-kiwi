// maps.js — Leaflet can't compute its size while its tab is hidden, so when any nav tab
// becomes visible we fire a window resize event; Leaflet listens for that and calls
// invalidateSize(), so the Maps tab draws at the correct size on first view. Harmless on
// other tabs. Paired with suspendWhenHidden = FALSE on the map output (R/modules/maps.R).
$(document).on('shown.bs.tab', function () {
  setTimeout(function () { window.dispatchEvent(new Event('resize')); }, 10);
});

// Clicking a map marker filters the records table below. Scroll the map to the top of the viewport
// so the controls above it roll off-screen and the table comes into view, without losing the map
// (it's ~70vh, so the top of the table shows beneath it). Triggered from the marker-click observer.
// The records area has a reserved min-height (maps.css) so drilling to a sparse location can't
// shrink the page below this scroll target and make the browser clamp it back up.
Shiny.addCustomMessageHandler('ik-maps-scroll', function (id) {
  var el = document.getElementById(id);
  if (el) el.scrollIntoView({ behavior: 'smooth', block: 'start' });
});
