/* ovw-lightbox.js — a tiny, dependency-free sequence viewer for the observation Photos burst.
 * Click any thumbnail in a `.ovw-media` grid → a full-screen overlay shows that image large,
 * with ◂ ▸ to step through the WHOLE sequence (the images in the same grid), a caption with its
 * timestamp + position, and Esc / arrow-key / backdrop-click controls. One overlay is reused for
 * all galleries; clicks are delegated so it works for modals rendered after page load. */
(function () {
  var lb, imgEl, capEl, items = [], idx = 0;

  function ensure() {
    if (lb) return;
    lb = document.createElement("div");
    lb.className = "ovw-lightbox";
    lb.innerHTML =
      '<button class="ovw-lb-close" type="button" aria-label="Close">&times;</button>' +
      '<button class="ovw-lb-nav ovw-lb-prev" type="button" aria-label="Previous">&#8249;</button>' +
      '<figure class="ovw-lb-fig">' +
        '<img class="ovw-lb-img" alt="">' +
        '<figcaption class="ovw-lb-cap"></figcaption>' +
      "</figure>" +
      '<button class="ovw-lb-nav ovw-lb-next" type="button" aria-label="Next">&#8250;</button>';
    document.body.appendChild(lb);
    imgEl = lb.querySelector(".ovw-lb-img");
    capEl = lb.querySelector(".ovw-lb-cap");
    lb.querySelector(".ovw-lb-close").addEventListener("click", close);
    lb.querySelector(".ovw-lb-prev").addEventListener("click", function (e) { e.stopPropagation(); step(-1); });
    lb.querySelector(".ovw-lb-next").addEventListener("click", function (e) { e.stopPropagation(); step(1); });
    lb.addEventListener("click", function (e) { if (e.target === lb) close(); });   // backdrop
  }

  function render() {
    if (!items.length) return;
    var it = items[idx];
    imgEl.src = it.src;
    capEl.textContent = (it.cap ? it.cap + "  ·  " : "") + (idx + 1) + " / " + items.length;
    lb.classList.toggle("ovw-lb-single", items.length < 2);   // hide arrows for a lone image
  }
  function step(d) { idx = (idx + d + items.length) % items.length; render(); }
  function close() { if (lb) lb.classList.remove("open"); }

  function open(gallery, startSrc) {
    ensure();
    items = Array.prototype.map.call(gallery.querySelectorAll("img.ovw-img"), function (im) {
      return { src: im.getAttribute("src"), cap: im.getAttribute("data-cap") || "" };
    });
    var start = items.findIndex(function (it) { return it.src === startSrc; });
    idx = start < 0 ? 0 : start;
    render();
    lb.classList.add("open");
  }

  document.addEventListener("click", function (e) {
    var im = e.target.closest && e.target.closest(".ovw-media img.ovw-img");
    if (!im) return;
    open(im.closest(".ovw-media"), im.getAttribute("src"));
  });
  // CAPTURE phase: run BEFORE Bootstrap's modal Esc handler (which lives on the modal element,
  // bubbling) and stop the event there — otherwise Esc closes the underlying modal, not the
  // lightbox. Only intercept while the lightbox is actually open.
  document.addEventListener("keydown", function (e) {
    if (!lb || !lb.classList.contains("open")) return;
    if (e.key === "Escape" || e.key === "ArrowLeft" || e.key === "ArrowRight") {
      e.preventDefault();
      e.stopPropagation();
    }
    if (e.key === "Escape") close();
    else if (e.key === "ArrowLeft") step(-1);
    else if (e.key === "ArrowRight") step(1);
  }, true);
})();
