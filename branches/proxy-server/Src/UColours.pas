{
 * UColours.pas
 *
 * Defines some special colour constants that record various program colours.
 *
 * v1.0 of 25 Nov 2006  - Original version.
 * v1.1 of 15 Dec 2006  - Updated some constants to use system colours instead
 *                        of hard wired colours.
 *                      - Added some new constants for update dialog news pane's
 *                        heading and highlight colours.
 *                      - Removed clHiliteText constants since now unused.
 * v1.2 of 17 Feb 2007  - Added new clTextSearchHighlight and clTextSearchText
 *                        constants that define default background and
 *                        foreground colours of search text in info pane. These
 *                        replace hard-wired colours from UWBHighlighter. A
 *                        paler background colour was used.
 * v1.3 of 25 Feb 2007  - Added new constants relating to new splash screen.
 * v1.4 of 26 Aug 2008  - Added new constant that specifies colour used to
 *                        display names of routines from user database.
 * v1.5 of 31 Dec 2008  - Added new constant that specifies colour used to
 *                        display REML markup tags in example code.
 * v1.6 of 20 Jun 2009  - Added new constants relating to new easter egg.
 *                      - Removed constants not now required for new splash
 *                        screen and changed others.
 *                      - Added new constant for colour of rendered REML var
 *                        elements.
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
 * The Original Code is UColours.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
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

