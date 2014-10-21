/*
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * JavaScript code used call into host application via external object. All
 * HTML and JavaScript code should call these routines rather than making
 * direct calls to the external object.
 */


/*
 * Calls external object to get host application to display Configure Compilers
 * dialogue box.
 *  @return False.
 */
function configCompilers() {
  external.ConfigCompilers();
  return false;
}

/*
 * Calls external object to get host application to display Update Database
 * dialogue box.
 *  @return False.
 */
function updateDbase() {
  external.UpdateDbase();
  return false;
}

/*
 * Calls external object to get host application to display a given snippet.
 *  @param string snippetID [in] ID of snippet to be displayed.
 *  @return False.
 */
function displaySnippet(snippetID) {
  var e = window.event;
  external.DisplaySnippet(snippetID, e.ctrlKey);
  return false;
}

/*
 * Calls external object to get host application to display Donate dialogue box.
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

/*
 * Calls external object to get the host application to display a specified tag.
 *  @param string tag [in] Tag to be displayed.
 *  @return False.
 */
function displayTag(tag) {
  var e = window.event;
  external.DisplayTag(tag, e.ctrlKey);
  return false;
}

/*
 * Calls external object to get host application to remove a tag from a
 * snippet's tag list.
 *  @param string snippetID [in] ID of snippet.
 *  @param string tag [in] Tag to be removed.
 *  @return False.
 */
function removeTag(snippetID, tag) {
  external.RemoveTag(snippetID, tag);
  return false;
}

/*
 * Calls external object to get the host application to display a specified
 * source code language.
 *  @param string langId [in] ID of language to be displayed.
 *  @return False.
 */
function displayLanguage(langId) {
  var e = window.event;
  external.DisplayLanguage(langId, e.ctrlKey);
  return false;
}

/*
 * Calls external object to get the host application to change the starred state
 * of a snippet.
 *  @param string snippetID [in] ID of snippet.
 *  @param Boolean state [in] New star state: True if snippet is to be starred,
 *    False if not.
 *  @return False.
 */
function changeSnippetStar(snippetID, state) {
  external.ChangeSnippetStar(snippetID, state);
  return false;
}
