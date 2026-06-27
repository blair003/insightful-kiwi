// maps.js — Leaflet can't compute its size while its tab is hidden, so when any nav tab becomes
// visible we (1) fire a window resize so Leaflet calls invalidateSize() and draws at the right size,
// and (2) if a map is still stuck zoomed right out — the symptom when fitBounds ran at zero size
// (e.g. launched from the RStudio runner): it shows the world map until you poke a control — re-fit
// it to its own layers. The zoom guard means a map already fitted (or one you've zoomed) is left alone.
$(document).on('shown.bs.tab', function () {
  setTimeout(function () { window.dispatchEvent(new Event('resize')); }, 10);
  setTimeout(function () {
    document.querySelectorAll('.html-widget.leaflet, .leaflet-container').forEach(function (el) {
      try {
        var id = el.id || (el.closest('.html-widget') || {}).id;
        var w  = (window.HTMLWidgets && HTMLWidgets.find && id) ? HTMLWidgets.find('#' + id) : null;
        var m  = (w && w.getMap) ? w.getMap() : null;
        if (!m) return;
        m.invalidateSize();
        // Hit-area "slop": the canvas renderer adds its `tolerance` (px) to every layer's hover/click
        // target, so a marker is grabbable a few px beyond its drawn edge — easier to hover/click
        // without changing how it looks. Set once per map (existing + future-added layers).
        if (!m._ikTol) {
          m._ikTol = true;
          var TOL = 8;
          var setTol = function (l) {
            if (l && l._renderer && l._renderer.options && (l._renderer.options.tolerance || 0) < TOL) {
              l._renderer.options.tolerance = TOL;
            }
          };
          m.eachLayer(setTol);
          m.on('layeradd', function (e) { setTol(e.layer); });
        }
        if (m.getZoom() > 4) return;                       // already fitted / user-zoomed — don't disturb
        var b = L.latLngBounds([]);
        m.eachLayer(function (l) {
          if (l.getLatLng) { b.extend(l.getLatLng()); }
          else if (l.getBounds) { try { b.extend(l.getBounds()); } catch (e) {} }
        });
        if (b.isValid()) m.fitBounds(b, { padding: [30, 30] });
      } catch (e) {}
    });
  }, 140);
});

// The species pages share one sidebar, but each page's tabset is its own (namespaced) input — so the
// active page's server bridges its current tab to this one top-level input, which the sidebar's Period
// control reads (hidden on the Trend tab, which spans all time). Set globally → no namespace.
Shiny.addCustomMessageHandler('ik-species-tab', function (tab) {
  Shiny.setInputValue('ik_species_tab', tab);
});
