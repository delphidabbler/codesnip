/*
 * detail.js
 *
 * JavaScript code for use by any detail page.
 *
 * v1.0 of 03 Dec 2006  - Original version that exposes routines called from
 *                        both JavaScript and Delphi code.
 * v1.1 of 14 Sep 2008  - Added setOnClick function to set a tag's onclick
 *                        event handler.
 * v2.0 of 25 Jan 2009  - Removed all except getInnerText() routine. This script
 *                        is no longer accessed from Delphi code.
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
 * The Original Code is detail.js
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
 */


/*
 * Retrieves inner text of an HTML element.
 *  @param string id [in] Id of element whose text we wanted.
 *  @return string Required text.
 */
function getInnerText(id) {
  tagObj = document.getElementById(id);
  return tagObj.innerText;
}

