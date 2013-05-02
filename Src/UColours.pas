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
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
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
  clBorder              = clGrayText;   // colour of borders used in HTML
  clWarningText         = clRed;        // colour of warning message text

  // backgrounds
  clSourceBg            = $CCFFFF;      // bg colour of displayed source code
  clCompTblHeadBg       = cl3DLight;    // bg colour of compiler table heading

  // links
  clLinkText            = clBlue;       // colour of a-links
  clHelpLinkText        = clGreen;      // colour of help a-links

  // news pane
  clNewsHeading         = clHotLight;   // colour of headings in news frames
  clNewsHighlight       = clHotLight;   // colour of hilited text in news frames
  clNewsBodyBorder      = clBorder;     // colour of border round news frame
  clNewsDateText        = clGrayText;   // colour of dates in news frame

  // text search
  clTextSearchHighlight = $77FFFF;      // bg colour of search text in info pane
  clTextSearchText      = clNone;       // colour of search text in info pane

  // splash screen
  clSplashPlainText     = clBlack;      // colour of splash screen plain text
  clSplashShadowText    = $DDDDDD;      // colour of splash screen text shadow

  // user routines
  clUserRoutine         = clBlue;       // colour of names of user routines

  // REML markup tags
  clREMLTags            = clPurple;     // colour of highlighted REML tags
  clVarText             = clPurple;     // colour of <var> tag text

  // easter egg
  clEggBlurbBg          = clWindow;     // blurb div background
  clEggBlurbHeadingBg   = $DDDDDD;      // blurb div heading background
  clEggBlurbBorder      = clBorder;     // blurb div border
  clEggMoreInfoBg       = $EEFFEE;      // more info box background
  clEggMoreInfoBorder   = clSilver;     // more info box border
  clEggLinkText         = clNavy;       // a-link colour (used in more info box)


implementation

end.

