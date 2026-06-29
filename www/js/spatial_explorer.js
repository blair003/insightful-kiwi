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

  if (window.Shiny) {
    Shiny.addCustomMessageHandler('spex-sync', function (msg) {
      var key = msg.a + '|' + msg.b;
      invalidate(msg.a); invalidate(msg.b);
      unlink(key);
      if (msg.link) { setTimeout(function () { link(msg.a, msg.b); }, 140); }
      else if (msg.sbs) { setTimeout(function () { seed(msg.a, msg.b); }, 140); }
    });
  }
})();
