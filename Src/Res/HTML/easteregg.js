/*
 * easteregg.js
 *
 * JavaScript code used to perform animations in program's easter egg.
 *
 * $Rev$
 * $Date$
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is easteregg.js
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributors:
 *    NONE
 *
 * ***** END LICENSE BLOCK *****
 */


// -----------------------------------------------------------------------------
// Initialisation
// -----------------------------------------------------------------------------

/*
 * Performs initialisation. Called when document has loaded.
 * return void.
 */
function init() {
  preload();
  beginAnim();
}


// -----------------------------------------------------------------------------
// Support functions
// -----------------------------------------------------------------------------

/*
 * Preloads image required for rollover button.
 * @return void.
 */
function preload() {
  img = new Image();
  img.src = 'cancel-glow.png';
}

/*
 * Sets source URL of an image.
 * @param id ID of image.
 * @param src URL of required image.
 * @return void.
 */
function setImage(id, src) {
  document.getElementById(id).src = src;
}

/*
 * Displays or hides an HTML element.
 * @param id ID of element.
 * @param show Flag true to show element or false to hide it.
 * @return void.
 */
function showElem(id, show) {
  if ( show )
    document.getElementById(id).style.display = 'block';
  else
    document.getElementById(id).style.display = 'none';
}

/*
 * Sets the top of an HTML element in pixels.
 * @param id ID of element.
 * @param top Top of element in pixels.
 * @return void.
 */
function setElemTop(id, top) {
  document.getElementById(id).style.top = top + 'px';
}


// -----------------------------------------------------------------------------
// Begin animation
// -----------------------------------------------------------------------------

/*
 * Begins animation by displaying "click me" image after a pause.
 * @return void.
 */
function beginAnim() {
  window.setTimeout('showClickMe(true)', 2500);
}


// -----------------------------------------------------------------------------
// Animation that displays and hides the "click me" div
// -----------------------------------------------------------------------------

var clickMeHidden = false;    // inhibits display of "click me" image when true

/*
 * Shows or hides "click me" image providing display not inhibited.
 * @param show Shows image if true and hides if false. Image always hidden if
 *   clickMeHidden is true.
 * @return void.
 */
function showClickMe(show) {
  if ( show && !clickMeHidden )
    showElem('click-me', true);
  else
    showElem('click-me', false);
}

/*
 * Permanently hides "click me" image.
 * @return void.
 */
function hideClickMe() {
  clickMeHidden = true;
  showClickMe(false);
}


// -----------------------------------------------------------------------------
// Animation that breaks the easter egg apart and reveals cancel button
// -----------------------------------------------------------------------------

// values controlling movement of egg part images
var topEggTop = 50;           // current position of top egg image
var topEggTopEnd = 0;         // final position of top egg image
var bottomEggTop = 50;        // current position of bottom egg image
var bottomEggTopEnd = 100;    // final position of bottom egg image
var eggMoveStep = 5;          // increment used to move egg images
var eggMoveDelay = 25;        // delay between moving egg images

/*
 * Break egg apart. Hide "click me" image and begins the animation.
 * @return void.
 */
function breakEgg() {
  hideClickMe();
  moveEggImages();
}

/*
 * Animation that moves images showing two halves of easter egg apart then
 * starts next animation
 * @return void.
*/
function moveEggImages() {
  // increment / decroment top position of images
  if ( topEggTop - eggMoveStep > topEggTopEnd )
    topEggTop -= eggMoveStep;
  else
    topEggTop = topEggTopEnd;
  if ( bottomEggTop + eggMoveStep < bottomEggTopEnd )
    bottomEggTop += eggMoveStep;
  else
    bottomEggTop = bottomEggTopEnd;
  // move the images
  setElemTop('egg-top', topEggTop);
  setElemTop('egg-bottom', bottomEggTop);
  if ( (topEggTop > topEggTopEnd) || (bottomEggTop < bottomEggTopEnd) )
    // there's further to go, call this routine again after delay
    window.setTimeout('moveEggImages()', eggMoveDelay);
  else {
    // finished: change cursor to normal and start next animation
    document.getElementById('egg-top').style.cursor  = 'auto';
    document.getElementById('egg-bottom').style.cursor  = 'auto';
    stretchBlurb();
  }
}


// -----------------------------------------------------------------------------
// Animation that appears to roll "paper" for blurb out of bottom half of egg
// -----------------------------------------------------------------------------

// values controlling stretch of blurb div
var blurbTopStart = 360;      // initial top of div
var blurbTopFinal = 100;      // final top of div
var blurbStartHeight = 20;    // height of div before stretch
var blurbTop = blurbTopStart; // current top of div
var stretchDelay = 15;        // delay between stretching steps
var stretchStep = 2;          // increment used for stretch steps

/*
 * Animation that stretch div that displays the blurb by moving its top
 * up the screen then starts next animation.
 * @return void.
 */
function stretchBlurb() {
  // move to new top position
  if ( blurbTop > blurbTopFinal )
    blurbTop -= stretchStep;
  else
    blurbTop = blurbTopFinal;
  setElemTop('blurb', blurbTop);
  document.getElementById('blurb').style.height
    = (blurbStartHeight + blurbTopStart - blurbTop) + 'px';
  if ( blurbTop > blurbTopFinal )
    // more to do, call this routine again after delay
    window.setTimeout('stretchBlurb()', stretchDelay);
  else {
    // finished: start next animation
    showElem('cancel', true);
    setElemTop('cancel', blurbTop + 4);
    scrollBlurb();
  }
}


// -----------------------------------------------------------------------------
// Animation scrolls the blurb up its "paper". Continues forever.// -----------------------------------------------------------------------------

// values controlling scrolling of blurb div
var scrollTop = -1;       // current position of scroll top
var scrollDelay = 40;     // delay between scrolling steps

/*
 * Continually scrolls blurb contained in blurb div. Also displays links
 * when all blurb has scrolled.
 * @return void.
 */
function scrollBlurb() {
  document.getElementById('blurb').scrollTop += 1;
  var curTop = document.getElementById('blurb').scrollTop;
  if ( curTop != scrollTop ) {
    // not reached end of scroll yet: keep going after delay
    scrollTop = curTop;
    window.setTimeout('scrollBlurb()', scrollDelay);
  }
  else {
    // read end of scroll: display additional info then restart scroll
    showElem('more-info', true);
    document.getElementById('blurb').scrollTop = 0;
    scrollTop = -1;
    window.setTimeout('scrollBlurb()', scrollDelay);
  }
}

