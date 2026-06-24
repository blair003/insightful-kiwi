// highlights.js — initialise the Highlights hero carousel. Bootstrap auto-inits carousels with
// data-bs-ride only at page load; this one arrives via Shiny renderUI (and lives in a lazily-shown
// nav tab), so we start it ourselves once it's in the DOM. Idempotent (data-ik-init guard).
function ikInitHeroCarousels() {
  if (!(window.bootstrap && bootstrap.Carousel)) return;
  document.querySelectorAll('.ik-hero-carousel:not([data-ik-init])').forEach(function (el) {
    el.setAttribute('data-ik-init', '1');
    var interval = parseInt(el.getAttribute('data-bs-interval'), 10) || 5000;
    var c = bootstrap.Carousel.getOrCreateInstance(el, { interval: interval, ride: 'carousel', pause: 'hover' });
    if (c && c.cycle) c.cycle();                       // ensure it auto-advances immediately
  });
}
$(document).on('shiny:value shown.bs.tab', function () { setTimeout(ikInitHeroCarousels, 60); });
$(function () { setTimeout(ikInitHeroCarousels, 200); });
