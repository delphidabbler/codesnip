/*
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2021, Peter Johnson (gravatar.com/delphidabbler).
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
 * Calls external object to get host application to display a named snippet.
 *  @param string snippet [in] Key of snippet to be displayed.
 *  @param string vaultId [in] Hex string representation of the vault to which
 *    the snippet belongs.
 *  @return False.
 */
function displaySnippet(snippet, vaultId) {
  var e = window.event;
  external.DisplaySnippet(snippet, vaultId, e.ctrlKey);
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
 *  @param string snippet [in] Key of snippet to be edited. Must be user
 *    defined.
 *  @param string vaultId [in] Hex string representation of the vault to which
 *  the snippet belongs.
 *  @return False.
 */
function editSnippet(snippet, vaultId) {
  external.EditSnippet(snippet, vaultId);
  return false;
}

/*
 * Calls external object to get host application to display the CodeSnip news
 * blog.
 *  @return False.
 */
function showNews() {
  external.ShowNews();
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
