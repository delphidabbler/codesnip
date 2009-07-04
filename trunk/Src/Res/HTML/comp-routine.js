/*
 * comp-routine.js
 *
 * JavaScript code used by Compiler Check pages that display compile results
 * for routines
 *
 * v0.1 of 27 Jan 2005 - Original script (compcheck.js).
 * v0.2 of 20 Jan 2005 - Renamed as comp-routine.js.
 * v1.0 of 04 Jun 2006 - Reformatted update log comments.
 *                     - Changed to Mozilla Public License.
 * v1.1 of 24 Oct 2006 - Changed to get hint text from variables from
 *                       detail-consts.js.
 *                     - Added code to update hint when "view unit" link text
 *                       changes.
 * v1.2 of 24 Nov 2006 - Changed ToggleUnitView() to simply call new
 *                       external.ToggleTestUnit() instead of showing or hiding
 *                       unit.
 * v1.3 of 26 Nov 2006 - Replaced direct calls to external object with calls to
 *                       functions in external.js.
 *                     - Renamed ShowSrc() function as showTestUnit().
 *                     - Removed ToggleUnitView() function. Replaced by routine
 *                       external.js.
 * v1.4 of 03 Dec 2006 - Added showTestCompileHint() function.
 * v1.5 of 04 Dec 2006 - Modified showTestUnit() so that it only re-displays
 *                       hint when the mouse is over the action link. Added
 *                       inTestUnitLink global variable and enterTestUnitLink()
 *                       and exitTestUnitLink() functions to do this.
 *                     - Modified showTestUnit() to change image when unit view
 *                       state changes.
 * v1.6 of 25 Jan 2009 - Changed showTestUnit() to change image title.
 * v2.0 of 07 Jun 2009 - Removed support for showing/hiding test unit:
 *                       - vShowHideHint and inTestUnitLink variables removed.
 *                       - Removed enterTestUnitLink(), exitTestUnitLink() and
 *                         showTestUnit() functions.
 *
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
 * The Original Code is comp-routine.js
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
 */


/*
 * Displays hint information that a test compilation can be performed. Hint
 * contains name of current routine.
 */
function showTestCompileHint() {
  showHint("Compile \"" + getInnerText("routinename") + "\"");
}

