/*
 * external.js
 *
 * JavaScript code used call into host application via external object. All
 * HTML and JavaScript code should call these routines rather than making
 * direct calls to the external object.
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
 * The Original Code is external.js
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributors:
 *    NONE
 *
 * ***** END LICENSE BLOCK *****
 */


/*
 * Calls external object to get host application to display a compiler log.
 *  @param integer compId [in] Id of compiler to be displayed.
 *  @return False.
 */
function viewCompilerLog(compId) {
  external.ViewCompilerLog(compId);
  return false;
}

/*
 * Calls external object to get host application to clear any current hint from
 * the status bar.
 */
function clearHint() {
  external.ShowHint('');
}

/*
 * Calls external object to get host application to display a hint in the status
 * bar.
 *  @param string hint [in] Hint to be displayed.
 */
function showHint(hint) {
  external.ShowHint(hint);
}

/*
 * Calls external object to get host application to display Configure Compilers
 * dialog box.
 *  @return False.
 */
function configCompilers() {
  external.ConfigCompilers();
  return false;
}

/*
 * Calls external object to get host application to compile currently selected
 * snippet.
 *  @return False.
 */
function compileRoutine() {
  external.CompileSnippet();
  return false;
}

/*
 * Calls external object to get host application to display Update Database
 * dialog box.
 *  @return False.
 */
function updateDbase() {
  external.UpdateDbase();
  return false;
}

/*
 * Calls external object to get host application to display test unit.
 *  @return False.
 */
function showTestUnit() {
  external.ShowTestUnit();
  return false;
}

/*
 * Calls external object to get host application to display a named snippet.
 *  @param string snippet [in] Name of snippet to be displayed.
 *  @param boolean userdefined [in] Whether snippet is user defined.
 *  @return False.
 */
function displayRoutine(snippet, userdefined) {
  external.DisplaySnippet(snippet, userdefined);
  return false;
}

/*
 * Calls external object to get host application to display a category.
 *  @param string catid [in] ID of category to be displayed.
 *  @return False.
 */
function displayCategory(catid) {
  external.DisplayCategory(catid);
  return false;
}

/*
 * Calls external object to get host application to edit a named snippet.
 *  @param string snippet [in] Name of snippet to be edited. Must be user
 *    defined.
 *  @return False.
 */
function editRoutine(snippet) {
  external.EditSnippet(snippet);
  return false;
}

/*
 * Calls external object to get host application to display Donate dialog box.
 *  @return False.
 */
function donate() {
  external.Donate();
  return false;
}

