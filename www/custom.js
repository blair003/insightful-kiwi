
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
  // Get the current URL without any search parameters or hash
  var url = window.location.protocol + "//" + window.location.host + window.location.pathname;
  // Construct the new URL
  var shareUrl = url + "?observation_id=" + observationId;
  copyToClipboard(shareUrl, btn);
}
