{
 * UColours.pas
 *
 * Defines some special colour constants that record various program colours.
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
 * The Original Code is UColours.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UColours;


interface


uses
  // Delphi
  Graphics;


const
  // Colour constants defining special program colours

  // general
  clBorder              = clGrayText;   // borders used in HTML
  clWarningText         = clRed;        // warning message text

  // backgrounds
  clSourceBg            = $CCFFFF;      // background of displayed source code
  clCompTblHeadBg       = cl3DLight;    // background of compiler table heading

  // links
  clLinkText            = clBlue;       // a-links
  clHelpLinkText        = clGreen;      // help a-links

  // news pane
  clNewsHeading         = clHotLight;   // headings in news frames
  clNewsHighlight       = clHotLight;   // hilited text in news frames
  clNewsBodyBorder      = clBorder;     // border round news frame
  clNewsDateText        = clGrayText;   // dates in news frame

  // text search
  clTextSearchHighlight = $77FFFF;      // background of info pane search text
  clTextSearchText      = clNone;       // search text in info pane

  // splash screen
  clSplashPlainText     = $B52F20;      // splash screen plain text
  clSplashShadowText    = clWhite;      // splash screen text shadow

  // new tab
  clNewTabText          = cl3DLight;    // color of text displayed in new tabs

  // colours of snippet titles / heading
  clMainSnippet         = clWindowText; // main database snippets
  clUserSnippet         = clBlue;       // user database snippets

  // markup tags
  clVarText             = clPurple;     // <var> tag text

  // easter egg
  clEggBlurbBg          = clWindow;     // blurb div background
  clEggBlurbHeadingBg   = $DDDDDD;      // blurb div heading background
  clEggBlurbBorder      = clBorder;     // blurb div border
  clEggMoreInfoBg       = $EEFFEE;      // more info box background
  clEggMoreInfoBorder   = clSilver;     // more info box border
  clEggLinkText         = clNavy;       // a-links used in more info box


implementation

end.

