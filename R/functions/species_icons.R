# species_icons.R — small, playful, single-colour SVG icons for the species groups, shown on the
# Overview cards (and the "other species" entries). Hand-drawn silhouettes on a 0–24 viewBox with
# `fill = currentColor`, so each icon inherits the surrounding text colour and scales by font-size.
#
# Keyed by the project.R species-group KEY by convention (mustelid, rat, cat, …). An unknown key —
# or the literal "other" — falls back to a generic paw print. Add an animal by adding a key here.

#' Inline SVG for a species-group icon.
#' @param key   The species-group key (project.R `species_groups` name), case-insensitive.
#' @param class Extra CSS class(es) for the <svg>.
#' @return An htmltools::HTML() inline <svg>, or the generic paw for an unknown key.
ik_species_icon <- function(key, class = NULL) {
  key <- key %||% "other"; if (length(key) == 1L && is.na(key)) key <- "other"  # unconfigured species → generic paw
  inner <- .IK_SPP_ICONS[[tolower(key)]] %||% .IK_SPP_ICONS[["other"]]
  htmltools::HTML(sprintf(
    '<svg class="ik-spp-icon%s" viewBox="0 0 24 24" fill="currentColor" aria-hidden="true" focusable="false">%s</svg>',
    if (is.null(class)) "" else paste0(" ", class), inner))
}

# Each entry is the INNER markup of the <svg>. Thin lines (whiskers, tails, legs) use
# stroke="currentColor" with no fill; bodies use the svg-level fill.
.IK_SPP_ICONS <- list(

  # Cat — round face, two sharp ears, whiskers.
  cat = paste0(
    '<polygon points="5,2 10,9 4,11"/><polygon points="19,2 14,9 20,11"/>',
    '<circle cx="12" cy="13" r="7"/>',
    '<g stroke="currentColor" stroke-width="0.8" stroke-linecap="round" fill="none">',
    '<line x1="2.5" y1="12.5" x2="8" y2="13"/><line x1="2.5" y1="15" x2="8" y2="14.5"/>',
    '<line x1="21.5" y1="12.5" x2="16" y2="13"/><line x1="21.5" y1="15" x2="16" y2="14.5"/></g>'),

  # Mouse — compact head, two big round ears, curly tail.
  mouse = paste0(
    '<circle cx="6" cy="7" r="4.3"/><circle cx="18" cy="7" r="4.3"/>',
    '<circle cx="12" cy="14" r="6"/>',
    '<path d="M13 19 q6 2 9 -3" fill="none" stroke="currentColor" stroke-width="1.3" stroke-linecap="round"/>'),

  # Rat — body, pointed snout (left), small ears, long curling tail.
  rat = paste0(
    '<ellipse cx="14" cy="14" rx="6" ry="4.6"/>',
    '<path d="M9 14 q-5 -1.5 -7.5 1 q2.5 2.5 7.5 1 z"/>',
    '<circle cx="12" cy="8.6" r="2.5"/><circle cx="17.5" cy="9.2" r="2.5"/>',
    '<path d="M20 15 q4 0 4 4 q0 3 -3 3" fill="none" stroke="currentColor" stroke-width="1.3" stroke-linecap="round"/>'),

  # Mustelid (stoat/ferret) — long low body, small eared head (left), long curling tail (right), stubby legs.
  mustelid = paste0(
    '<rect x="4" y="8.5" width="14" height="5.5" rx="2.6"/>',
    '<circle cx="5.5" cy="8" r="3"/>',
    '<polygon points="3,4.6 4.7,7.6 6.4,5.8"/>',
    '<path d="M17.5 10.3 C21 9.3 23 7.2 22.4 4.2" fill="none" stroke="currentColor" stroke-width="1.7" stroke-linecap="round"/>',
    '<g stroke="currentColor" stroke-width="1.4" stroke-linecap="round" fill="none">',
    '<line x1="7" y1="13.8" x2="7" y2="17"/><line x1="11" y1="13.8" x2="11" y2="17"/><line x1="15" y1="13.8" x2="15" y2="17"/></g>'),

  # Dog — round head, two floppy ears, snout.
  dog = paste0(
    '<path d="M6 6 Q1.5 7.5 3 15 Q6.5 15 7.5 10 Z"/>',
    '<path d="M18 6 Q22.5 7.5 21 15 Q17.5 15 16.5 10 Z"/>',
    '<ellipse cx="12" cy="12" rx="6.2" ry="6"/>',
    '<ellipse cx="12" cy="16.5" rx="3.2" ry="2.6"/><circle cx="12" cy="15.6" r="0.9"/>'),

  # Possum — big rounded ears, face narrowing to a pointed muzzle, thick BUSHY curled tail
  # (the filled tail + muzzle point set it apart from the mouse).
  possum = paste0(
    '<circle cx="7.5" cy="7.5" r="3.4"/><circle cx="14.5" cy="7.5" r="3.4"/>',
    '<path d="M4.5 11 Q11 8.5 17.5 11 Q17.5 15 13 16 L11 19.5 L9 16 Q4.5 15 4.5 11 Z"/>',
    '<path d="M17 12 q5 0.5 4.2 4.8 q-0.8 3 -3.8 1.8 q2.8 -0.8 2 -3.8 z"/>'),

  # Hedgehog — spiky dome with a flat base, little nose poking out the front.
  hedgehog = paste0(
    '<path d="M2 18 L4 10.5 L6 14.5 L8 8.5 L10.5 13.5 L12 7.5 L13.5 13.5 L16 8.5 ',
    'L18 14.5 L20 10.5 L22 18 Z"/>',
    '<circle cx="3.4" cy="16.8" r="1"/>'),

  # Pig — round head, triangle ears, big snout, curly tail.
  pig = paste0(
    '<polygon points="5.5,6 9.5,5.5 7.5,11"/><polygon points="18.5,6 14.5,5.5 16.5,11"/>',
    '<ellipse cx="12" cy="13" rx="7" ry="6"/>',
    '<ellipse cx="12" cy="15" rx="3.6" ry="2.9"/>',
    '<path d="M19 11 q3 -1.5 2.5 1.5 q-0.5 2.5 1.5 2" fill="none" stroke="currentColor" stroke-width="1.3" stroke-linecap="round"/>'),

  # Kiwi — round fluffy body, long thin curved beak, two stick legs.
  kiwi = paste0(
    '<ellipse cx="13.5" cy="11" rx="7" ry="6"/>',
    '<path d="M7 10 Q2 11 0.5 14.5" fill="none" stroke="currentColor" stroke-width="1.7" stroke-linecap="round"/>',
    '<g stroke="currentColor" stroke-width="1.4" stroke-linecap="round" fill="none">',
    '<line x1="11" y1="16.5" x2="11" y2="21"/><line x1="15.5" y1="16.5" x2="15.5" y2="21"/></g>'),

  # Weka — plump rail/bird body, short beak, stubby tail, legs.
  weka = paste0(
    '<path d="M5 13 Q5 7 12 7 Q19 7 19 13 Q19 17.5 12.5 17.5 Q6 17.5 5 13 Z"/>',
    '<polygon points="18.5,9.5 23.5,11 18.5,12"/>',
    '<path d="M5.5 11 L1.5 9.5 L3 13 Z"/>',
    '<g stroke="currentColor" stroke-width="1.2" stroke-linecap="round" fill="none">',
    '<line x1="10" y1="17.5" x2="10" y2="21"/><line x1="14" y1="17.5" x2="14" y2="21"/></g>'),

  # Other / unknown — a generic paw print.
  other = paste0(
    '<ellipse cx="12" cy="16" rx="4.8" ry="4"/>',
    '<ellipse cx="5.5" cy="10" rx="2.1" ry="2.6"/><ellipse cx="10" cy="6.5" rx="2.1" ry="2.7"/>',
    '<ellipse cx="14" cy="6.5" rx="2.1" ry="2.7"/><ellipse cx="18.5" cy="10" rx="2.1" ry="2.6"/>')
)
