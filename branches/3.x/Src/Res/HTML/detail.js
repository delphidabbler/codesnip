/*
 * detail.js
 *
 * JavaScript code for use by HTML displayed in a detail pane.
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
 * The Original Code is detail.js
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributors:
 *    NONE
 *
 * ***** END LICENSE BLOCK *****
 */

 
/*
 * Trims leading and trailing whitespace from a string.
 *  @param string str [in] String to be trimmed.
 *  @return string Trimmed string.
 */
function trim(str) {
	// see http://developer.loftdigital.com/blog/trim-a-string-in-javascript
	return str.replace(/^\s+|\s+$/g, ''); 
}

/*
 * Retrieves inner text of an HTML element.
 *  @param string id [in] Id of element whose text we wanted.
 *  @return string Required text.
 */
function getInnerText(id) {
  tagObj = document.getElementById(id);
  return tagObj.innerText;
}

/*
 * Gets the name of the displayed snippet from the HTML element with id of
 * "routinename".
 *  @return Required snippet name.
 */
function getSnippetName() {
	return trim(getInnerText("routinename"));
}

/*
 * Displays hint information that a test compilation can be performed. Hint
 * contains name of current snippet.
 *  @return void.
 */
function showTestCompileHint() {
  showHint("Compile \"" + getSnippetName() + "\"");
}

/*
 * Displays hint information that a snippet can be edited. Hint contains name of
 * current snippet.
 *  @return void.
 */
function showEditSnippetHint() {
	showHint("Edit \"" + getSnippetName() + "\"");
}
