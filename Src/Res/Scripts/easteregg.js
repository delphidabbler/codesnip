/*
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * JavaScript code used to perform animations in program's easter egg.
 * Requires jQuery and lite version of jQuery Cycle plug-in.
 */


// Main function called when DOM has loaded. Runs prelinary animations up to
// when introductory page is shown on unfolded screen
$(document).ready(function(){
  
  //! Fix vulnerability using patch suggested in dependabot alert
  //! https://github.com/delphidabbler/codesnip/security/dependabot/1 
  jQuery.htmlPrefilter = function( html ) {
    return html;
  };

  var spt = null;  // showPrompt timeout

  $("#screen-top").click(function(){
    // screen top click => open screen
    // prevent or stop any prompt animations
    if ( spt != null )
      clearTimeout(spt);
    stopPrompt();
    // disable click on screen top: this is a one time only event
    $("#screen-top").off();
    $("#screen-top").css("cursor", "default");
    // unfolds out the screen: moves top and middle elements simultaneously,
    // then displays intro page on unfolded screen
    $("#screen-middle").animate({top: 48, height: 304}, 2000);
    $("#screen-top").animate(
      {top: 26},
      2000,
      '',
      function() {
        showIntro();
        showCancelButton();
      }
    );
  });
  //
  // show a prompt if user doesn't click anything in specified time
  spt = setTimeout(showPrompt, 3500);
});

// Display introductory page on unfloded screen, along with slideshow start
// button.
function showIntro() {
  var pulse = 150;

  setRollover("#start", "play-btn.png", "play-btn-hover.png");
  $("#start").click(function() {
    // Start button clicked:
    // disable further clicks: this is a one-time action
    $("#start").off();
    // switch of title text in pop-up window
    $("#start").attr("title", "");
    $("#start").css("cursor", "default");
    // fade out intro text and start slide show
    $("#intro").fadeOut(500, slideShow);
  });
  $("#intro").fadeIn(1200);
}

// Prepare and display cancel button in unfolded screen
function showCancelButton() {
  setRollover("#cancel-btn", "cancel-btn.png", "cancel-btn-hover.png");
  $("#cancel-btn").click(function() {
    // clear events and switch of title (may be popped up)
    $(this).off();
    $(this).attr("title", "");
    // return true to allow event to bubble up to dialogue box to permit it to
    // close
    return true;
  });
  $("#cancel-btn").fadeIn(1200);
}

// Runs slide show
function slideShow() {
  $("#slideshow").fadeIn(1000).cycle({
    fx: "fade",
    timeout: 4000,
    speed: 2500
  });
}

// Runs prompt animation telling user where to click to display content
function showPrompt() {
  var text = $("#click-prompt");
  var arrow = $("#click-arrow");
  var jigHi = 245;
  var jigLo = 265;
  var jigTime = 150;
  // display arrow pointing at screen top and jiggle it, then display promt
  // text
  arrow.css("top", jigLo);
  arrow
    .fadeIn(200)
    // first jiggle
    .animate({top: jigHi}, jigTime)
    .animate({top: jigLo}, jigTime)
    .animate({top: jigHi}, jigTime)
    .animate({top: jigLo}, jigTime)
    .animate({top: jigHi}, jigTime)
    .animate({top: jigLo}, jigTime)
    .animate({top: jigHi}, jigTime)
    .animate({top: jigLo}, jigTime)
    .delay(1500)
    // second jiggle
    .animate({top: jigHi}, jigTime)
    .animate({top: jigLo}, jigTime)
    .animate({top: jigHi}, jigTime)
    .animate({top: jigLo}, jigTime)
    .animate({top: jigHi}, jigTime)
    .animate({top: jigLo}, jigTime)
    .animate({top: jigHi}, jigTime)
    .animate(
      {top: jigLo},
      jigTime,
      function(){
        // show prompt text above arrow
        text.css(
          "top",
          parseInt(arrow.css("top")) - parseInt(text.css("height")) - 6
        );
        text.delay(1000).fadeIn(1000);
      }
    );
}

// Stops any running prompt animation and hides related elements
function stopPrompt() {
  $("#click-prompt").stop();
  $("#click-arrow").stop();
  $("#click-prompt").hide();
  $("#click-arrow").hide();
  // following is required to stop arrow from appearing when screen is
  // expanding: don't understand why this happens
  $("#click-arrow").css("width", "0");
}

// Sets rollover images for elements matching given CSS selector.
function setRollover(selector, normalImg, hoverImg) {
  $(selector).on(
    "mouseout",
    function () {
      $(this).attr("src", normalImg);
    }
  );
  $(selector).on(
    "mousemove",
    function () {
      $(this).attr("src", hoverImg);
    }
  );
}

