// spatial_explorer.js — client glue for the Spatial explorer's side-by-side display mode.
//
// Two jobs, both driven by the server's "spex-sync" custom message (fired when the Display mode or
// Link toggle changes):
//   1. invalidateSize() both leaflet panes — a conditionalPanel reveal doesn't fire a map resize, so
//      a freshly-shown pane B (and the now-narrower pane A) would tile wrong without this.
//   2. wire / unwire pan+zoom LINKING between the two panes (the "Link views" toggle), with a guard
//      so the two setView calls don't ping-pong into a feedback loop.
(function () {
  var links = {};  // key "aId|bId" -> {a, b, fa, fb}

  function getMap(id) { try { var w = HTMLWidgets.find('#' + id); return w ? w.getMap() : null; } catch (e) { return null; } }
  function invalidate(id) { var m = getMap(id); if (m) setTimeout(function () { m.invalidateSize(); }, 60); }

  function unlink(key) {
    var L = links[key]; if (!L) return;
    L.a.off('move zoom', L.fa); L.b.off('move zoom', L.fb);
    delete links[key];
  }

  function seed(aId, bId) {                       // put pane B where pane A is looking
    var a = getMap(aId), b = getMap(bId);
    if (a && b) b.setView(a.getCenter(), a.getZoom(), { animate: false });
  }

  function link(aId, bId) {
    var a = getMap(aId), b = getMap(bId); if (!a || !b) return;
    var key = aId + '|' + bId; unlink(key);
    var syncing = false;
    function mk(from, to) {
      return function () {
        if (syncing) return;
        syncing = true;
        to.setView(from.getCenter(), from.getZoom(), { animate: false });
        setTimeout(function () { syncing = false; }, 0);  // clear AFTER the synchronous move events settle
      };
    }
    var fa = mk(a, b), fb = mk(b, a);
    a.on('move zoom', fa); b.on('move zoom', fb);
    links[key] = { a: a, b: b, fa: fa, fb: fb };
    b.setView(a.getCenter(), a.getZoom(), { animate: false });
  }

  // The SWIPE/curtain: the two panes overlap (CSS), and a draggable divider sets `--swipe` (a %)
  // which clips the comparison map. One-time wiring per divider.
  function initSwipe() {
    var row = document.querySelector('.ik-spex-row'); if (!row) return;
    var divider = row.querySelector('.ik-spex-swipe-divider'); if (!divider || divider._spex) return;
    divider._spex = true;
    if (!row.style.getPropertyValue('--swipe')) row.style.setProperty('--swipe', '50%');
    var dragging = false;
    function setPct(clientX) {
      var r = row.getBoundingClientRect();
      var pct = Math.max(4, Math.min(96, ((clientX - r.left) / r.width) * 100));
      row.style.setProperty('--swipe', pct + '%');
    }
    divider.addEventListener('mousedown', function (e) { dragging = true; e.preventDefault(); });
    window.addEventListener('mousemove', function (e) { if (dragging) setPct(e.clientX); });
    window.addEventListener('mouseup', function () { dragging = false; });
    divider.addEventListener('touchstart', function () { dragging = true; }, { passive: true });
    window.addEventListener('touchmove', function (e) { if (dragging && e.touches[0]) setPct(e.touches[0].clientX); }, { passive: true });
    window.addEventListener('touchend', function () { dragging = false; });
  }

  if (window.Shiny) {
    Shiny.addCustomMessageHandler('spex-sync', function (msg) {
      var key = msg.a + '|' + msg.b;
      var row = document.querySelector('.ik-spex-row');
      if (row) row.classList.toggle('ik-spex-swipe', msg.display === 'swipe');
      invalidate(msg.a); invalidate(msg.b);
      setTimeout(function () { invalidate(msg.a); invalidate(msg.b); }, 220);  // re-fit after the layout change settles
      unlink(key);
      if (msg.link) { setTimeout(function () { link(msg.a, msg.b); }, 220); }
      else if (msg.twoPane) { setTimeout(function () { seed(msg.a, msg.b); }, 220); }
      if (msg.swipe) setTimeout(initSwipe, 250);
    });
  }
})();
