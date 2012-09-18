/*
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * JavaScript code for use by HTML displayed in a detail pane.
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
 * "snippetname".
 *  @return Required snippet name.
 */
function getSnippetName() {
  return trim(getInnerText("snippetname"));
}

/*
 * Displays hint information that a snippet can be edited. Hint contains name of
 * current snippet.
 *  @return void.
 */
function showEditSnippetHint() {
  showHint("Edit \"" + getSnippetName() + "\"");
}

