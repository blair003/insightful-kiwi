
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