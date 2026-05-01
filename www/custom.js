
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
          // Introduce a delay before calling setPosition
          setTimeout(function() {
            $('#' + message.carouselId).slick('setPosition');
            $('.slick-dots li:first-child button').focus().click();
            console.log('setPosition on: ' + message.carouselId); 
          }, 500); // Delay in milliseconds, adjust as needed
        });
      });

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
          if (nav && nav.indexOf('species_dashboard_') === 0) {
            return inputValue(nav + '-dashboard_tabs');
          }

          var tabInputs = {
            dashboard: 'main_dashboard_tabs',
            reporting: 'reporting_tabs',
            density_map: 'density_map_tabs',
            observation_map: 'observation_map-observation_map_tabs',
            activity_patterns: 'activity_patterns_tabs',
            raw_data: 'raw_data_tabs'
          };

          return inputValue(tabInputs[nav]);
        }

        function shouldShareInput(id, nav) {
          if (!id || id === 'nav' || id === 'share_view_state') {
            return false;
          }

          if (nav === 'dashboard') {
            return /^main_dashboard_(current|prior|last_year)_period-period_selection$/.test(id) ||
              /^dashboard_rai_plot-/.test(id);
          }

          if (nav === 'reporting') {
            return id === 'primary_period-period_selection' ||
              id === 'report_format' ||
              /^current_tables-reporting_(results_summary|spp_sum)_tabsetpanel$/.test(id);
          }

          if (nav === 'plots') {
            return /^spp_obs_plot_visualisations-/.test(id);
          }

          if (nav === 'density_map') {
            return /^density_map_primary-(selected_species|selected_localities)$/.test(id) ||
              /^(primary_period|comparative_period)-period_selection$/.test(id);
          }

          if (nav === 'observation_map') {
            return /^observation_map-(selected_species|selected_localities|enhance_map_details)$/.test(id) ||
              id === 'primary_period-period_selection';
          }

          if (nav === 'activity_patterns') {
            return /^activity_patterns_map-(selected_species|selected_localities)$/.test(id) ||
              /^main_dashboard_(current|prior|last_year)_period-period_selection$/.test(id);
          }

          if (nav && nav.indexOf('species_dashboard_') === 0) {
            return id.indexOf(nav + '-') === 0 &&
              id !== nav + '-dashboard_tabs' &&
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

          if (nav === 'raw_data') {
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
          Shiny.setInputValue('share_view_state', {
            base_url: window.location.protocol + "//" + window.location.host + window.location.pathname,
            page_state: collectCurrentPageState(inputValue('nav')),
            rawdata_observations_search: getDataTableSearch('rawdata_observations_browse'),
            rawdata_deployments_search: getDataTableSearch('rawdata_deployments_browse'),
            nonce: new Date().getTime()
          }, { priority: 'event' });
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

        Shiny.addCustomMessageHandler('closeNavbarMenus', function(message) {
          setTimeout(function() {
            $('.navbar .dropdown-menu.show, .navbar .dropdown.show').removeClass('show');
            $('.navbar [aria-expanded="true"]').attr('aria-expanded', 'false');
          }, 100);
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
    if (navValue && navValue.indexOf('species_dashboard_') === 0) {
      if ($('.collapse-toggle').attr('aria-expanded') === 'true') {
        console.log('Collapsing the sidebar for species dashboard.');
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
