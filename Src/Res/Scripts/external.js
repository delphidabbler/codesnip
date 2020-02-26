/*
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * JavaScript code used call into host application via external object. All
 * HTML and JavaScript code should call these routines rather than making
 * direct calls to the external object.
 */


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
 * Calls external object to get host application to display Update Database
 * dialog box.
 *  @return False.
 */
function updateDbase() {
  external.UpdateDbase();
  return false;
}

/*
 * Calls external object to get host application to display a named snippet.
 *  @param string snippet [in] Name of snippet to be displayed.
 *  @param boolean userdefined [in] Whether snippet is user defined.
 *  @return False.
 */
function displaySnippet(snippet, userdefined) {
  var e = window.event;
  external.DisplaySnippet(snippet, userdefined, e.ctrlKey);
  return false;
}

/*
 * Calls external object to get host application to display a category.
 *  @param string catid [in] ID of category to be displayed.
 *  @return False.
 */
function displayCategory(catid) {
  var e = window.event;
  external.DisplayCategory(catid, e.ctrlKey);
  return false;
}

/*
 * Calls external object to get host application to edit a named snippet.
 *  @param string snippet [in] Name of snippet to be edited. Must be user
 *    defined.
 *  @return False.
 */
function editSnippet(snippet) {
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

/*
 * Calls external object to get host application to start Snippets Editor ready
 * for a new snippet to be entered.
 *  @return False.
 */
function newSnippet() {
  external.NewSnippet();
  return false;
}

/*
 * Calls external object to get host application to display the CodeSnip news
 * feed.
 *  @return False.
 */
function showNews() {
  external.ShowNews();
  return false;
}

/*
 * Calls external object to get host application to check for program updates.
 *  @return False.
 */
function checkForUpdates() {
  external.CheckForUpdates();
  return false;
}

/*
 * Calls external object to get host application to display About box.
 *  @return False,
 */
function showAboutBox() {
  external.ShowAboutBox();
  return false;
}

/*
 * Calls external object to display the tab with the given frame class in the
 * Preferences dialogue box.
 *  @param string tabCls [in] Name of class of frame hosting required
 *    preferences page.
 *  @return False.
 */
function showPrefsPage(tabCls) {
  external.ShowPrefsPage(tabCls);
  return false;
}
