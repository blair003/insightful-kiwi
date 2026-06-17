
      // For dynamically updating card content when toggling full-screen
      $(document).on('click', '.bslib-full-screen-enter', function() {
      //  console.log("Full-screen toggle button clicked");  // Debugging line
      
        var cardElement = $(this).closest('.card'); // The closest card element
      
        // Check if the observer is already set to avoid multiple observers
        if (!cardElement.data('observerSet')) {
      
          // Use a MutationObserver to listen for changes to the 'data-full-screen' attribute
          var observer = new MutationObserver(function(mutations) {
            mutations.forEach(function(mutation) {
              if (mutation.attributeName === 'data-full-screen') {
                var isFullScreen = cardElement.attr('data-full-screen') === 'true';
            //    console.log("Is full screen:", isFullScreen);  // Debugging line
      
                // Update the Shiny input to inform about the full-screen state
                Shiny.setInputValue('is_fullscreen', isFullScreen);
              }
            });
          });
      
          // Start observing the card element for attribute changes
          observer.observe(cardElement[0], { attributes: true });
      
          // Set a data attribute to indicate that the observer has been set
          cardElement.data('observerSet', true);
        }
      
        // Trigger the initial check when button is clicked
        var isFullScreenInitial = cardElement.attr('data-full-screen') === 'true';
        Shiny.setInputValue('is_fullscreen', isFullScreenInitial);
      });



      // Sends the field click back to shiny for the observeEvent 
      function customInfoButtonClickHandler(field, btn_id) {
        // Stop propogation so the click doesn't also trigger a re-sort by that column
         event.stopPropagation();
       // console.log('Clicked: ' + btn_id); // Logs the unique button ID
        Shiny.setInputValue('info_field_clicked', field, {priority: 'event'}); // Sends only the field back to Shiny
      }
      
      
    
      $(document).on('click', '.observation-link', function() {
        var observationID = $(this).data('observationid') || "";  // Observation ID
        var sequenceID = $(this).data('sequenceid') || "";        // Sequence ID (fallback to empty string)
        var actionType = $(this).data('action-type') || "";       // Action type
        var timestamp = new Date().getTime();                    // Ensure uniqueness for reactivity
        
        console.log("observationID:", observationID);
        console.log("sequenceID:", sequenceID);
        console.log("actionType:", actionType);
      
        // Concatenate observationID, sequenceID, actionType, and timestamp
        Shiny.setInputValue(
          'observation_click',
          observationID + '|' + sequenceID + '|' + actionType + '|' + timestamp,
          { priority: 'event' }
        );
      });

      $(document).on('click', '.trap-observation-link', function() {
        var observationID = $(this).data('observationid') || "";
        Shiny.setInputValue(
          'trap_observation_click',
          observationID + '|' + new Date().getTime(),
          { priority: 'event' }
        );
      });
      
    // Send obsevationID from link back to shiny for determining sequence_id and getting images
//      $(document).on('click', '.observation-link', function() {
//        var observationID = $(this).data('observationid');
//        var actionType = $(this).data('action-type'); // Capture the action type
//        var timestamp = new Date().getTime(); // Ensure uniqueness for reactivity
//        // Concatenate observationID, actionType, and timestamp, separated by '|'
//        Shiny.setInputValue('observationID_click', observationID + '|' + actionType + '|' + timestamp, {priority: 'event'});
//      });

      // Set focus on the slick slider dots for modal to work
      $(document).on('shiny:connected', function(event) {
        Shiny.addCustomMessageHandler('refreshCarousel', function(message) {
          function carouselById(id) {
            var escapedId = $.escapeSelector ? $.escapeSelector(id) : id.replace(/(:|\.|\[|\]|,|=|@)/g, "\\$1");
            return $('#' + escapedId);
          }

          function refreshCarouselPosition(attemptsLeft) {
            var carousel = carouselById(message.carouselId);

            if (carousel.length && carousel.hasClass('slick-initialized')) {
              carousel.slick('setPosition');
              carousel.find('.slick-dots li:first-child button').focus().click();
              console.log('setPosition on: ' + message.carouselId);
              return;
            }

            if (attemptsLeft > 0) {
              setTimeout(function() {
                refreshCarouselPosition(attemptsLeft - 1);
              }, 250);
            }
          }

          refreshCarouselPosition(8);
        });
      });

      (function() {
        function cssEscape(value) {
          if (window.CSS && window.CSS.escape) {
            return window.CSS.escape(value);
          }

          return String(value).replace(/(:|\.|\[|\]|,|=|@)/g, "\\$1");
        }

        function leafletMapById(id) {
          if (!window.HTMLWidgets || !HTMLWidgets.find || !id) {
            return null;
          }

          try {
            var widget = HTMLWidgets.find('#' + cssEscape(id));
            if (widget && widget.getMap) {
              return widget.getMap();
            }
          } catch (error) {
            console.warn('Map comparison could not find Leaflet widget map:', error);
          }

          return null;
        }

        function setSwipePosition(container, position) {
          var primaryLayer = container.querySelector('.map-swipe-primary-map');
          var comparativeLayer = container.querySelector('.map-swipe-comparative-map');
          var divider = container.querySelector('.map-swipe-divider');
          var constrained = Math.max(0, Math.min(100, position));

          if (primaryLayer) {
            primaryLayer.style.clipPath = 'inset(0 ' + (100 - constrained) + '% 0 0)';
            primaryLayer.style.webkitClipPath = 'inset(0 ' + (100 - constrained) + '% 0 0)';
          }

          if (comparativeLayer) {
            comparativeLayer.style.clipPath = 'inset(0 0 0 ' + constrained + '%)';
            comparativeLayer.style.webkitClipPath = 'inset(0 0 0 ' + constrained + '%)';
          }

          if (divider) {
            divider.style.left = constrained + '%';
            divider.setAttribute('aria-valuenow', String(Math.round(constrained)));
          }

          container.dataset.swipePosition = String(constrained);
        }

        function positionFromEvent(container, event) {
          var point = event.touches && event.touches.length ? event.touches[0] : event;
          var rect = container.getBoundingClientRect();
          if (!rect.width) {
            return 50;
          }

          return ((point.clientX - rect.left) / rect.width) * 100;
        }

        function syncLeafletMaps(primaryMap, comparativeMap) {
          if (!primaryMap || !comparativeMap || primaryMap._insightfulKiwiSwipeSync || comparativeMap._insightfulKiwiSwipeSync) {
            return;
          }

          var syncing = false;
          function syncFrom(source, target) {
            if (syncing || !source || !target) {
              return;
            }

            syncing = true;
            target.setView(source.getCenter(), source.getZoom(), { animate: false, reset: true });
            syncing = false;
          }

          primaryMap.on('moveend zoomend', function() {
            syncFrom(primaryMap, comparativeMap);
          });
          comparativeMap.on('moveend zoomend', function() {
            syncFrom(comparativeMap, primaryMap);
          });

          comparativeMap.setView(primaryMap.getCenter(), primaryMap.getZoom(), { animate: false, reset: true });
          primaryMap._insightfulKiwiSwipeSync = true;
          comparativeMap._insightfulKiwiSwipeSync = true;
        }

        window.initMapSwipeComparison = function(config) {
          var container = config && config.containerId ? document.getElementById(config.containerId) : null;
          if (!container) {
            return;
          }

          var divider = container.querySelector('.map-swipe-divider');
          var position = Number(container.dataset.swipePosition || 50);

          setSwipePosition(container, isFinite(position) ? position : 50);

          function updateFromPointer(event) {
            if (container.dataset.swipeDragging !== 'true') {
              return;
            }

            event.preventDefault();
            setSwipePosition(container, positionFromEvent(container, event));
          }

          if (divider && !divider.dataset.swipeBound) {
            divider.dataset.swipeBound = 'true';
            divider.addEventListener('mousedown', function(event) {
              container.dataset.swipeDragging = 'true';
              updateFromPointer(event);
            });
            divider.addEventListener('touchstart', function(event) {
              container.dataset.swipeDragging = 'true';
              updateFromPointer(event);
            }, { passive: false });
            divider.addEventListener('keydown', function(event) {
              var current = Number(container.dataset.swipePosition || 50);
              var step = event.shiftKey ? 10 : 2;

              if (event.key === 'ArrowLeft') {
                event.preventDefault();
                setSwipePosition(container, current - step);
              } else if (event.key === 'ArrowRight') {
                event.preventDefault();
                setSwipePosition(container, current + step);
              } else if (event.key === 'Home') {
                event.preventDefault();
                setSwipePosition(container, 0);
              } else if (event.key === 'End') {
                event.preventDefault();
                setSwipePosition(container, 100);
              }
            });
          }

          if (!container.dataset.swipeDocumentBound) {
            container.dataset.swipeDocumentBound = 'true';
            document.addEventListener('mousemove', updateFromPointer);
            document.addEventListener('touchmove', updateFromPointer, { passive: false });
            document.addEventListener('mouseup', function() { container.dataset.swipeDragging = 'false'; });
            document.addEventListener('touchend', function() { container.dataset.swipeDragging = 'false'; });
            document.addEventListener('touchcancel', function() { container.dataset.swipeDragging = 'false'; });
            document.addEventListener('shown.bs.tab', function() {
              if (!document.body.contains(container)) {
                return;
              }

              window.setTimeout(function() {
                var rect = container.getBoundingClientRect();
                if (rect.width <= 0 || rect.height <= 0) {
                  return;
                }

                var primaryMap = leafletMapById(config.primaryMapId);
                var comparativeMap = leafletMapById(config.comparativeMapId);
                if (primaryMap && comparativeMap) {
                  primaryMap.invalidateSize(false);
                  comparativeMap.invalidateSize(false);
                  comparativeMap.setView(primaryMap.getCenter(), primaryMap.getZoom(), { animate: false, reset: true });
                }
              }, 60);
            });
          }

          function connectMaps(attemptsLeft) {
            var primaryMap = leafletMapById(config.primaryMapId);
            var comparativeMap = leafletMapById(config.comparativeMapId);

            if (primaryMap && comparativeMap) {
              try {
                primaryMap.invalidateSize(false);
                comparativeMap.invalidateSize(false);
                syncLeafletMaps(primaryMap, comparativeMap);
              } catch (error) {
                console.warn('Map comparison could not initialise map sync:', error);
              }
              return;
            }

            if (attemptsLeft > 0) {
              window.setTimeout(function() {
                connectMaps(attemptsLeft - 1);
              }, 250);
            }
          }

          connectMaps(40);
        };
      })();

      $(document).on('shiny:connected', function(event) {
        function shinyInputValues() {
          return (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$inputValues) || {};
        }

        function inputValue(id) {
          return shinyInputValues()[id];
        }

        function dataTableForOutput(tableId) {
          var table = $('#' + tableId);
          var dataTable = table;

          if (dataTable.length && !dataTable.is('table')) {
            dataTable = table.find('table.dataTable');
          }

          if (!dataTable.length || !$.fn.DataTable || !$.fn.DataTable.isDataTable(dataTable)) {
            return null;
          }

          return dataTable.DataTable();
        }

        function getDataTableSearch(tableId) {
          var dataTable = dataTableForOutput(tableId);
          return dataTable ? (dataTable.search() || "") : "";
        }

        function getDataTableShareState(tableId) {
          var dataTable = dataTableForOutput(tableId);
          if (!dataTable) {
            return null;
          }

          return {
            search: dataTable.search() || "",
            length: dataTable.page.len(),
            page: dataTable.page(),
            order: dataTable.order()
          };
        }

        function applyDataTableSearch(tableId, searchTerm, attemptsLeft) {
          if (!searchTerm) {
            return;
          }

          var dataTable = dataTableForOutput(tableId);
          if (dataTable) {
            dataTable.search(searchTerm).draw();
            return;
          }

          if (attemptsLeft > 0) {
            setTimeout(function() {
              applyDataTableSearch(tableId, searchTerm, attemptsLeft - 1);
            }, 250);
          }
        }

        function applyDataTableShareState(tableId, tableState, attemptsLeft) {
          if (!tableState) {
            return;
          }

          var dataTable = dataTableForOutput(tableId);
          if (dataTable) {
            if (isMeaningfulValue(tableState.length)) {
              dataTable.page.len(tableState.length);
            }
            if (Array.isArray(tableState.order)) {
              dataTable.order(tableState.order);
            }
            if (isMeaningfulValue(tableState.search)) {
              dataTable.search(tableState.search);
            }
            dataTable.draw(false);
            if (isMeaningfulValue(tableState.page)) {
              dataTable.page(tableState.page).draw(false);
            }
            return;
          }

          if (attemptsLeft > 0) {
            setTimeout(function() {
              applyDataTableShareState(tableId, tableState, attemptsLeft - 1);
            }, 250);
          }
        }

        function isMeaningfulValue(value) {
          if (value === null || value === undefined || value === "") {
            return false;
          }
          return !(Array.isArray(value) && value.length === 0);
        }

        function setPageStateValue(id, value, attemptsLeft) {
          var escapedId = $.escapeSelector ? $.escapeSelector(id) : id.replace(/(:|\.|\[|\]|,|=|@)/g, "\\$1");
          var element = $('#' + escapedId);

          if (element.length) {
            if (element.is(':checkbox')) {
              element.prop('checked', value === true || value === 'true').trigger('change');
              return;
            }

            if (element[0].selectize) {
              element[0].selectize.setValue(value, false);
              return;
            }

            element.val(value).trigger('change');
            return;
          }

          if (attemptsLeft > 0) {
            setTimeout(function() {
              setPageStateValue(id, value, attemptsLeft - 1);
            }, 250);
          }
        }

        function currentTabForNav(nav) {
          if (nav && nav.indexOf('species_overview_') === 0) {
            return inputValue(nav + '-overview_tabs');
          }

          var tabInputs = {
            overview: 'overview-main_overview_tabs',
            reporting: 'reporting_tabs',
            activity_patterns: 'activity_patterns-activity_patterns_tabs',
            records: 'records_tabs'
          };

          return inputValue(tabInputs[nav]);
        }

        function shouldShareInput(id, nav) {
          if (!id || id === 'nav' || id === 'share_view_state') {
            return false;
          }

          if (nav === 'overview') {
            return /^main_overview_(current|prior|last_year)_period-period_selection$/.test(id) ||
              /^overview_rai_plot-/.test(id);
          }

          if (nav === 'reporting') {
            return id === 'primary_period-period_selection' ||
              id === 'report_format' ||
              /^current_tables-reporting_(results_summary|spp_sum)_tabsetpanel$/.test(id);
          }

          if (nav === 'plots') {
            return /^spp_obs_plot_visualisations-/.test(id);
          }

          if (nav === 'activity_patterns') {
            return /^activity_patterns_map-(selected_species|selected_localities)$/.test(id) ||
              /^main_overview_(current|prior|last_year)_period-period_selection$/.test(id);
          }

          if (nav && nav.indexOf('species_overview_') === 0) {
            return id.indexOf(nav + '-') === 0 &&
              id !== nav + '-overview_tabs' &&
              !/_rows_|_cell_|_state$|_columns_|_cells_/.test(id);
          }

          return false;
        }

        function collectCurrentPageState(nav) {
          var values = shinyInputValues();
          var state = {};

          Object.keys(values).forEach(function(id) {
            if (shouldShareInput(id, nav) && isMeaningfulValue(values[id])) {
              state[id] = values[id];
            }
          });

          if (nav === 'records') {
            state.rawdata_observations_browse_dt = getDataTableShareState('rawdata_observations_browse');
            state.rawdata_deployments_browse_dt = getDataTableShareState('rawdata_deployments_browse');
          }

          return state;
        }

        window.buildSharedViewUrl = function(extraParams) {
          var nav = inputValue('nav');
          var tab = currentTabForNav(nav);
          var pageState = collectCurrentPageState(nav);
          var baseUrl = window.location.protocol + "//" + window.location.host + window.location.pathname;
          var params = new URLSearchParams();

          if (isMeaningfulValue(nav)) {
            params.set('nav', nav);
          }
          if (isMeaningfulValue(tab)) {
            params.set('tab', tab);
          }
          if (Object.keys(pageState).length > 0) {
            params.set('state', JSON.stringify(pageState));
          }

          Object.keys(extraParams || {}).forEach(function(key) {
            if (isMeaningfulValue(extraParams[key])) {
              params.set(key, extraParams[key]);
            }
          });

          var query = params.toString();
          return query ? baseUrl + '?' + query : baseUrl;
        };

        Shiny.addCustomMessageHandler('collectShareViewState', function(message) {
          var delay = message && message.delay ? Number(message.delay) : 0;

          setTimeout(function() {
            Shiny.setInputValue('share_view_state', {
              base_url: window.location.protocol + "//" + window.location.host + window.location.pathname,
              page_state: collectCurrentPageState(inputValue('nav')),
              rawdata_observations_search: getDataTableSearch('rawdata_observations_browse'),
              rawdata_deployments_search: getDataTableSearch('rawdata_deployments_browse'),
              nonce: new Date().getTime()
            }, { priority: 'event' });
          }, delay);
        });

        Shiny.addCustomMessageHandler('collectPdfExportViewState', function(message) {
          var nav = inputValue('nav');

          function sendPdfExportState(screenshotDataUrl, screenshotWidth, screenshotHeight) {
            Shiny.setInputValue('pdf_export_view_state', {
              base_url: window.location.protocol + "//" + window.location.host + window.location.pathname,
              nav: nav,
              tab: currentTabForNav(nav),
              page_state: collectCurrentPageState(nav),
              rawdata_observations_search: getDataTableSearch('rawdata_observations_browse'),
              rawdata_deployments_search: getDataTableSearch('rawdata_deployments_browse'),
              screenshot_data_url: screenshotDataUrl,
              screenshot_width: screenshotWidth,
              screenshot_height: screenshotHeight,
              nonce: new Date().getTime()
            }, { priority: 'event' });
          }

          if (!window.html2canvas) {
            sendPdfExportState(null, null, null);
            return;
          }

          $('.modal.show').modal('hide');
          $('.modal-backdrop').remove();
          document.body.classList.remove('modal-open');
          document.body.style.removeProperty('padding-right');

          function findExportElement() {
            return document.body;
          }

          var exportElement = findExportElement();
          var originalWindowScrollX = window.scrollX;
          var originalWindowScrollY = window.scrollY;
          var originalStyles = [];

          function saveAndSetStyle(element, styles) {
            var original = {};
            Object.keys(styles).forEach(function(key) {
              original[key] = element.style[key];
              element.style[key] = styles[key];
            });
            originalStyles.push({ element: element, styles: original });
          }

          function addTemporaryElement(element) {
            originalStyles.push({ element: element, remove: true });
          }

          function addExportCaptureStyle() {
            var style = document.createElement('style');
            style.setAttribute('data-pdf-export-style', 'true');
            style.textContent = [
              '.pdf-export-capture .recalculating,',
              '.pdf-export-capture .shiny-bound-output,',
              '.pdf-export-capture .shiny-plot-output,',
              '.pdf-export-capture .html-widget {',
              '  opacity: 1 !important;',
              '  filter: none !important;',
              '}',
              '.pdf-export-capture .shiny-plot-output img {',
              '  opacity: 1 !important;',
              '  filter: none !important;',
              '}'
            ].join('\n');
            document.head.appendChild(style);
            addTemporaryElement(style);
            document.body.classList.add('pdf-export-capture');
          }

          function waitForShinyOutputs() {
            var deadline = Date.now() + 2500;

            return new Promise(function(resolve) {
              function check() {
                var recalculating = document.querySelectorAll('.recalculating').length;

                if (recalculating === 0 || Date.now() >= deadline) {
                  window.setTimeout(resolve, 150);
                  return;
                }

                window.setTimeout(check, 100);
              }

              check();
            });
          }

          function hidePdfExcludedImageCarousels() {
            if (!message || !message.exclude_image_carousel) {
              return;
            }

            var carouselElements = Array.prototype.slice.call(document.querySelectorAll(
              '.overview-favourites-hero, #slickSlider'
            ));
            var hiddenElements = [];

            carouselElements.forEach(function(element) {
              if (!element || hiddenElements.indexOf(element) !== -1) {
                return;
              }

              var heading = element.previousElementSibling;
              if (heading && heading.classList && heading.classList.contains('overview-section-heading')) {
                saveAndSetStyle(heading, { display: 'none' });
                hiddenElements.push(heading);
              }

              saveAndSetStyle(element, { display: 'none' });
              hiddenElements.push(element);
            });
          }

          function visibleLeafletElements() {
            return Array.prototype.slice.call(document.querySelectorAll('.leaflet')).filter(function(element) {
              var rect = element.getBoundingClientRect();
              return element.id && rect.width > 0 && rect.height > 0;
            });
          }

          function ensurePdfExportMapImageHandler() {
            window.pdfExportMapImageResolvers = window.pdfExportMapImageResolvers || {};

            if (window.pdfExportMapImageHandlerRegistered) {
              return;
            }

            Shiny.addCustomMessageHandler('pdfExportMapImages', function(message) {
              var nonce = message && message.nonce;
              var resolver = nonce && window.pdfExportMapImageResolvers[nonce];

              if (!resolver) {
                return;
              }

              delete window.pdfExportMapImageResolvers[nonce];
              resolver((message && message.maps) || []);
            });

            window.pdfExportMapImageHandlerRegistered = true;
          }

          function requestServerLeafletImages(leafletElements) {
            if (!window.Shiny || !leafletElements.length) {
              return Promise.resolve([]);
            }

            ensurePdfExportMapImageHandler();

            return new Promise(function(resolve) {
              var nonce = String(new Date().getTime()) + '-' + String(Math.random()).slice(2);
              var timeout = window.setTimeout(function() {
                if (window.pdfExportMapImageResolvers && window.pdfExportMapImageResolvers[nonce]) {
                  delete window.pdfExportMapImageResolvers[nonce];
                }
                resolve([]);
              }, 45000);

              window.pdfExportMapImageResolvers[nonce] = function(maps) {
                window.clearTimeout(timeout);
                resolve(maps || []);
              };

              Shiny.setInputValue('pdf_export_map_request', {
                nonce: nonce,
                maps: leafletElements.map(function(element) {
                  var rect = element.getBoundingClientRect();
                  return {
                    id: element.id,
                    width: Math.round(rect.width),
                    height: Math.round(rect.height)
                  };
                })
              }, { priority: 'event' });
            });
          }

          function replaceLeafletWithStaticImage(element, src) {
            var rect = element.getBoundingClientRect();

            if (!src || rect.width <= 0 || rect.height <= 0) {
              return false;
            }

            var image = document.createElement('img');
            image.src = src + (src.indexOf('?') === -1 ? '?' : '&') + 'pdf_export_cache_bust=' + encodeURIComponent(String(new Date().getTime()));
            image.alt = element.getAttribute('aria-label') || 'Map';
            image.style.display = 'block';
            image.style.width = rect.width + 'px';
            image.style.height = rect.height + 'px';
            image.style.maxWidth = rect.width + 'px';
            image.style.maxHeight = rect.height + 'px';

            element.parentNode.insertBefore(image, element);
            addTemporaryElement(image);
            saveAndSetStyle(element, { display: 'none' });
            return true;
          }

          function prepareServerLeafletImages() {
            var leafletElements = visibleLeafletElements();

            return requestServerLeafletImages(leafletElements).then(function(maps) {
              var replacedCount = 0;

              maps.forEach(function(mapInfo) {
                var element = mapInfo && mapInfo.id ? document.getElementById(mapInfo.id) : null;
                if (element && replaceLeafletWithStaticImage(element, mapInfo.src)) {
                  replacedCount += 1;
                }
              });

              return replacedCount;
            });
          }

          function loadLeafletImage() {
            if (window.leafletImage) {
              return Promise.resolve(true);
            }

            return new Promise(function(resolve) {
              var existingScript = document.querySelector('script[data-pdf-export-leaflet-image]');

              if (existingScript) {
                existingScript.addEventListener('load', function() {
                  resolve(!!window.leafletImage);
                }, { once: true });
                existingScript.addEventListener('error', function() {
                  resolve(false);
                }, { once: true });
                return;
              }

              var script = document.createElement('script');
              script.src = 'https://unpkg.com/leaflet-image@0.4.0/leaflet-image.js';
              script.async = true;
              script.setAttribute('data-pdf-export-leaflet-image', 'true');
              script.onload = function() {
                resolve(!!window.leafletImage);
              };
              script.onerror = function() {
                resolve(false);
              };
              document.head.appendChild(script);
            });
          }

          function leafletMapForElement(element) {
            if (!window.HTMLWidgets || !HTMLWidgets.find || !element.id) {
              return null;
            }

            try {
              var widget = HTMLWidgets.find('#' + element.id);
              if (widget && widget.getMap) {
                return widget.getMap();
              }
            } catch (error) {
              console.warn('PDF export could not find Leaflet widget map:', error);
            }

            return null;
          }

          function replaceLeafletWithImage(element, map) {
            return new Promise(function(resolve) {
              var rect = element.getBoundingClientRect();

              if (!window.leafletImage || !map || rect.width <= 0 || rect.height <= 0) {
                resolve(false);
                return;
              }

              try {
                map.invalidateSize(false);
              } catch (error) {
                console.warn('PDF export could not refresh Leaflet size:', error);
              }

              window.leafletImage(map, function(error, canvas) {
                if (error || !canvas) {
                  console.warn('PDF export Leaflet image render failed:', error);
                  resolve(false);
                  return;
                }

                var image = document.createElement('img');
                image.src = canvas.toDataURL('image/png');
                image.alt = element.getAttribute('aria-label') || 'Map';
                image.style.display = 'block';
                image.style.width = rect.width + 'px';
                image.style.height = rect.height + 'px';
                image.style.maxWidth = rect.width + 'px';
                image.style.maxHeight = rect.height + 'px';

                element.parentNode.insertBefore(image, element);
                addTemporaryElement(image);
                saveAndSetStyle(element, { display: 'none' });
                resolve(true);
              });
            });
          }

          function prepareLeafletImages() {
            var leafletElements = visibleLeafletElements();

            if (!leafletElements.length) {
              return Promise.resolve();
            }

            return loadLeafletImage().then(function(loaded) {
              if (!loaded) {
                return;
              }

              return Promise.all(leafletElements.map(function(element) {
                return replaceLeafletWithImage(element, leafletMapForElement(element));
              }));
            });
          }

          function waitForTemporaryImages() {
            var temporaryImages = originalStyles
              .filter(function(item) {
                return item.remove && item.element && item.element.tagName === 'IMG';
              })
              .map(function(item) {
                return item.element;
              });

            if (!temporaryImages.length) {
              return Promise.resolve();
            }

            return Promise.all(temporaryImages.map(function(image) {
              if (image.complete && image.naturalWidth > 0) {
                return Promise.resolve();
              }

              return new Promise(function(resolve) {
                var timeout = window.setTimeout(resolve, 5000);
                image.onload = function() {
                  window.clearTimeout(timeout);
                  resolve();
                };
                image.onerror = function() {
                  window.clearTimeout(timeout);
                  resolve();
                };
              });
            }));
          }

          [document.documentElement, document.body, exportElement].forEach(function(element) {
            saveAndSetStyle(element, {
              height: 'auto',
              maxHeight: 'none',
              overflow: 'visible'
            });
          });

          addExportCaptureStyle();
          hidePdfExcludedImageCarousels();

          Array.prototype.slice.call(document.querySelectorAll('.leaflet, .html-widget, .shiny-plot-output')).forEach(function(element) {
            var rect = element.getBoundingClientRect();
            if (rect.width > 0 && rect.height > 0) {
              saveAndSetStyle(element, {
                width: rect.width + 'px',
                height: rect.height + 'px',
                maxWidth: rect.width + 'px',
                maxHeight: rect.height + 'px'
              });
            }
          });

          window.scrollTo(0, 0);

          setTimeout(function() {
            prepareServerLeafletImages().then(function(replacedCount) {
              if (replacedCount > 0) {
                return;
              }

              return prepareLeafletImages();
            }).catch(function(error) {
              console.warn('PDF export Leaflet preparation failed; falling back to direct page capture:', error);
            }).then(function() {
              return waitForTemporaryImages();
            }).then(function() {
              return waitForShinyOutputs();
            }).then(function() {
              var exportWidth = Math.max(exportElement.scrollWidth, document.documentElement.scrollWidth, document.body.scrollWidth, window.innerWidth);
              var exportHeight = Math.max(exportElement.scrollHeight, document.documentElement.scrollHeight, document.body.scrollHeight, window.innerHeight);
              var viewportHeight = window.innerHeight;

              html2canvas(exportElement, {
                backgroundColor: '#ffffff',
                scale: 2,
                useCORS: true,
                logging: false,
                scrollX: 0,
                scrollY: 0,
                windowWidth: exportWidth,
                windowHeight: viewportHeight,
                width: exportWidth,
                height: exportHeight
              }).then(function(canvas) {
                sendPdfExportState(canvas.toDataURL('image/png'), canvas.width, canvas.height);
            }).catch(function(error) {
              console.error('PDF export capture failed:', error);
              sendPdfExportState(null, null, null);
            }).finally(function() {
              originalStyles.reverse().forEach(function(item) {
                if (item.remove) {
                  if (item.element && item.element.parentNode) {
                    item.element.parentNode.removeChild(item.element);
                  }
                  return;
                }
                Object.keys(item.styles).forEach(function(key) {
                  item.element.style[key] = item.styles[key];
                });
              });
              document.body.classList.remove('pdf-export-capture');
              window.scrollTo(originalWindowScrollX, originalWindowScrollY);
            });
            });
          }, 100);

        });

        Shiny.addCustomMessageHandler('restoreRawdataSearch', function(message) {
          applyDataTableSearch('rawdata_observations_browse', message.rawdata_observations_search, 20);
          applyDataTableSearch('rawdata_deployments_browse', message.rawdata_deployments_search, 20);
        });

        Shiny.addCustomMessageHandler('restorePageState', function(message) {
          if (!message || !message.state) {
            return;
          }

          var state = {};
          try {
            state = JSON.parse(message.state);
          } catch (e) {
            console.error('Could not parse shared page state:', e);
            return;
          }

          Object.keys(state).forEach(function(id) {
            if (id === 'rawdata_observations_browse_dt') {
              applyDataTableShareState('rawdata_observations_browse', state[id], 24);
            } else if (id === 'rawdata_deployments_browse_dt') {
              applyDataTableShareState('rawdata_deployments_browse', state[id], 24);
            } else {
              setPageStateValue(id, state[id], 24);
            }
          });
        });

        function closeOpenNavbarMenus() {
          $('.navbar .dropdown-menu.show, .navbar .dropdown.show').removeClass('show');
          $('.navbar [aria-expanded="true"]').attr('aria-expanded', 'false');

          document.querySelectorAll('.navbar .navbar-collapse.show, .navbar .collapse.show').forEach(function(element) {
            if (window.bootstrap && bootstrap.Collapse) {
              bootstrap.Collapse.getOrCreateInstance(element, { toggle: false }).hide();
            } else {
              element.classList.remove('show');
            }
          });
        }

        $(document).on('click', '.navbar .nav-link:not(.dropdown-toggle), .navbar .dropdown-item', function() {
          setTimeout(function() {
            if (window.innerWidth < 992) {
              closeOpenNavbarMenus();
            }
          }, 150);
        });

        $(document).on('shiny:inputchanged', function(event) {
          if (event.name === 'nav' && window.innerWidth < 992) {
            setTimeout(closeOpenNavbarMenus, 150);
          }
        });

        Shiny.addCustomMessageHandler('closeNavbarMenus', function(message) {
          setTimeout(closeOpenNavbarMenus, 100);
        });

        Shiny.addCustomMessageHandler('cleanShareQueryParams', function(message) {
          if (!window.history || !window.history.replaceState || !window.URLSearchParams) {
            return;
          }

          var paramsToRemove = (message && message.params) || [];
          if (!paramsToRemove.length || !window.location.search) {
            return;
          }

          var queryParams = new URLSearchParams(window.location.search);
          var changed = false;

          paramsToRemove.forEach(function(param) {
            if (queryParams.has(param)) {
              queryParams.delete(param);
              changed = true;
            }
          });

          if (!changed) {
            return;
          }

          var query = queryParams.toString();
          var cleanUrl = window.location.pathname +
            (query ? '?' + query : '') +
            window.location.hash;

          window.history.replaceState(window.history.state, document.title, cleanUrl);
        });
      });
      
      
    // Detection of window dimensions, used for map zoom level calculation
    $(document).on('shiny:connected', function(event) {
      
      var sendWindowSize = _.debounce(function() {
        Shiny.setInputValue('window_width', $(window).width());
        Shiny.setInputValue('window_height', $(window).height());
        //console.log('Window width: ' + $(window).width() + ', Window height: ' + $(window).height());
      }, 250); // Debounce period in milliseconds

      // Send initial size
      sendWindowSize();

      // Update dimensions on window resize, with debouncing
      $(window).resize(function() {
        sendWindowSize();
      });
    });

function toggleSidebar(navValue, defaultSidebarState) {
  if (window.innerWidth > 768) {
    if (navValue && navValue.indexOf('species_overview_') === 0) {
      if ($('.collapse-toggle').attr('aria-expanded') === 'true') {
        console.log('Collapsing the sidebar for species overview.');
        $('.collapse-toggle').click();
      }
      return;
    }

    if (defaultSidebarState[navValue] !== undefined) {
      console.log('Default sidebar state for ' + navValue + ' is: ' + defaultSidebarState[navValue]);
      if (defaultSidebarState[navValue]) {
        if (!$('.collapse-toggle').attr('aria-expanded') || $('.collapse-toggle').attr('aria-expanded') === 'false') {
          console.log('Opening the sidebar by clicking the toggle button.');
          $('.collapse-toggle').click();
        }
      } else {
        if ($('.collapse-toggle').attr('aria-expanded') === 'true') {
          console.log('Collapsing the sidebar by clicking the toggle button.');
          $('.collapse-toggle').click();
        }
      }
    }
  } else {
    console.log('Skipping sidebar toggle logic on mobile devices.');
  }
}

function initImageSlider(sliderId) {
  $(document).ready(function() {
    $("#" + sliderId).slick({
      lazyLoad: "progressive",
      slidesToShow: 2,
      slidesToScroll: 2,
      infinite: true,
      dots: true,
      arrows: false,
      autoplay: true,
      autoplaySpeed: 3000,
      speed: 1000,
      responsive: [
        {
          breakpoint: 1024,
          settings: {
            slidesToShow: 2,
            slidesToScroll: 1
          }
        },
        {
          breakpoint: 600,
          settings: {
            slidesToShow: 1,
            slidesToScroll: 1
          }
        }
      ]
    });
  });
}

function overviewHeroSliderById(sliderId) {
  var escapedId = $.escapeSelector ? $.escapeSelector(sliderId) : sliderId.replace(/(:|\.|\[|\]|,|=|@)/g, "\\$1");
  return $("#" + escapedId);
}

function initOverviewHeroSlider(sliderId) {
  function initialize() {
    var slider = overviewHeroSliderById(sliderId);

    if (!slider.length || !$.fn.slick) {
      return;
    }

    if (slider.hasClass("slick-initialized")) {
      slider.slick("unslick");
    }

    var slideCount = slider.children().length;
    if (slideCount === 0) {
      return;
    }

    slider.slick({
      autoplay: slideCount > 1,
      autoplaySpeed: 3600,
      arrows: slideCount > 1,
      centerMode: slideCount > 1,
      centerPadding: "18vw",
      dots: false,
      infinite: slideCount > 1,
      pauseOnFocus: true,
      pauseOnHover: true,
      slidesToScroll: 1,
      slidesToShow: 1,
      speed: 650,
      responsive: [
        {
          breakpoint: 900,
          settings: {
            centerMode: slideCount > 1,
            centerPadding: "12vw",
            slidesToShow: 1
          }
        },
        {
          breakpoint: 560,
          settings: {
            arrows: slideCount > 1,
            centerMode: slideCount > 1,
            centerPadding: "34px",
            slidesToShow: 1
          }
        }
      ]
    });

    window.setTimeout(function() {
      if (slider.hasClass("slick-initialized")) {
        slider.slick("setPosition");
      }
    }, 250);
  }

  $(document).ready(function() {
    window.setTimeout(initialize, 0);
    $(document).off("shown.bs.tab.overviewHero").on("shown.bs.tab.overviewHero", function() {
      var sliders = $(".overview-favourites-slider.slick-initialized");
      if (sliders.length) {
        window.setTimeout(function() {
          sliders.slick("setPosition");
        }, 100);
      }
    });
  });
}

function pauseOverviewHeroSlider(sliderId) {
  var slider = overviewHeroSliderById(sliderId);
  if (slider.length && slider.hasClass("slick-initialized")) {
    slider.slick("slickPause");
    slider.data("pausedByReviewModal", true);
  }
}

function resumeOverviewHeroSlider(sliderId) {
  var slider = overviewHeroSliderById(sliderId);
  if (slider.length && slider.hasClass("slick-initialized") && slider.data("pausedByReviewModal")) {
    slider.slick("slickPlay");
    slider.data("pausedByReviewModal", false);
  }
}

function resumePausedOverviewHeroSliders() {
  $(".overview-favourites-slider.slick-initialized").each(function() {
    var slider = $(this);
    if (slider.data("pausedByReviewModal")) {
      slider.slick("slickPlay");
      slider.data("pausedByReviewModal", false);
    }
  });
}

$(document).on("hidden.bs.modal", function() {
  resumePausedOverviewHeroSliders();
});

function copyToClipboard(text, btn) {
  navigator.clipboard.writeText(text).then(function() {
    var originalHTML = btn.innerHTML;
    btn.innerHTML = "<i class='fa fa-check'></i> Copied!";
    setTimeout(function() {
      btn.innerHTML = originalHTML;
    }, 2000);
  }).catch(function(err) {
    console.error('Could not copy text: ', err);
  });
}

function copyObservationUrl(observationId, btn) {
  var shareUrl = window.buildSharedViewUrl ?
    window.buildSharedViewUrl({ observation_id: observationId, view_mode: 'modal' }) :
    window.location.protocol + "//" + window.location.host + window.location.pathname + "?observation_id=" + encodeURIComponent(observationId) + "&view_mode=modal";
  copyToClipboard(shareUrl, btn);
}

function copyCurrentViewUrl(btn) {
  var shareUrl = window.buildSharedViewUrl ?
    window.buildSharedViewUrl({}) :
    window.location.protocol + "//" + window.location.host + window.location.pathname;
  copyToClipboard(shareUrl, btn);
}

function copyRaiBasisUrl(detailToken, btn) {
  var shareUrl = window.buildSharedViewUrl ?
    window.buildSharedViewUrl({ rai_detail: detailToken }) :
    window.location.protocol + "//" + window.location.host + window.location.pathname + "?rai_detail=" + encodeURIComponent(detailToken);
  copyToClipboard(shareUrl, btn);
}

function copySpeciesRaiBasisUrl(detailToken, btn) {
  var shareUrl = window.buildSharedViewUrl ?
    window.buildSharedViewUrl({ species_rai_detail: detailToken }) :
    window.location.protocol + "//" + window.location.host + window.location.pathname + "?species_rai_detail=" + encodeURIComponent(detailToken);
  copyToClipboard(shareUrl, btn);
}

function getRaiProofText(btn) {
  var container = btn.closest('.modal-content') || document;
  var proof = container.querySelector('.rai-calculation-trace');
  return proof ? proof.textContent.trim() : '';
}

function buildRaiVerificationPrompt(proofText) {
  return [
    'Please independently verify these RAI calculations.',
    'Report any discrepancy with the exact line or subtotal that appears wrong.',
    'Show your workings, then produce a table with the results',
    '',
    proofText
  ].join('\n');
}

function copyRaiProof(btn) {
  var proofText = getRaiProofText(btn);
  if (!proofText) {
    return;
  }

  copyToClipboard(buildRaiVerificationPrompt(proofText), btn);
}

function chatbotVerificationUrl(provider, promptText) {
  var encodedPrompt = encodeURIComponent(promptText);
  var maxQueryLength = 6500;

  if (provider === 'chatgpt') {
    return encodedPrompt.length <= maxQueryLength ?
      'https://chatgpt.com/?q=' + encodedPrompt :
      'https://chatgpt.com/';
  }

  return '#';
}

function verifyRaiProof(provider, btn) {
  var proofText = getRaiProofText(btn);
  if (!proofText) {
    return;
  }

  var promptText = buildRaiVerificationPrompt(proofText);
  var url = chatbotVerificationUrl(provider, promptText);
  var originalHTML = btn.innerHTML;

  window.open(url, '_blank', 'noopener');

  navigator.clipboard.writeText(promptText).then(function() {
    btn.innerHTML = "<i class='fa fa-check'></i> Opened + copied";
    setTimeout(function() {
      btn.innerHTML = originalHTML;
    }, 2500);
  }).catch(function(err) {
    console.error('Could not copy RAI proof text: ', err);
  });
}

(function() {
  var densityPlaybackTimers = {};
  var densityPlaybackHandlerRegistered = false;

  function stopDensityPlaybackTimer(id) {
    if (densityPlaybackTimers[id]) {
      window.clearInterval(densityPlaybackTimers[id]);
      delete densityPlaybackTimers[id];
    }
  }

  function registerDensityPlaybackHandler() {
    if (densityPlaybackHandlerRegistered || !window.Shiny || !Shiny.addCustomMessageHandler) {
      return;
    }

    densityPlaybackHandlerRegistered = true;
    Shiny.addCustomMessageHandler("densityPlaybackTimer", function(message) {
      if (!message || !message.id) {
        return;
      }

      stopDensityPlaybackTimer(message.id);

      if (!message.enabled) {
        return;
      }

      var interval = Math.max(50, Number(message.interval));
      if (!Number.isFinite(interval)) {
        interval = 50;
      }
      densityPlaybackTimers[message.id] = window.setInterval(function() {
        if (window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue(message.id, Date.now(), { priority: "event" });
        }
      }, interval);
    });
  }

  if (window.jQuery) {
    $(document).on("shiny:connected", registerDensityPlaybackHandler);
  }

  registerDensityPlaybackHandler();
})();
