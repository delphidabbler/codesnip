{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines some special colour constants that record various program colours.
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
  clDefaultLink         = clWindowText; // un-styled links
  clExternalLink        = clBlue;       // external links
  clHelpLink            = clGreen;      // help links
  clDBLink              = clNavy;       // database links (cat. and snippet)
  clCommandLink         = clMaroon;     // links that execute program commands
  clTagLinkFG           = clWindowText; // tag links foreground
  clTagLinkHoverFG      = clWHite;      // tag links hover foreground
  clTagLinkBG           = clBtnFace;    // tag link foreground
  clTagLinkHoverBG      = $999999;      // tag links hover background
  clTagBorder           = clBorder;     // border of tag links

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
  clNewTabText          = cl3DLight;    // colour of text displayed in new tabs

  // colours of snippet titles / heading
  { TODO -cSyncSpaces : rethink following colours in light of synch-spaces }
  clMainSnippet         = clWindowText; // main database snippets
  clUserSnippet         = clBlue;       // user database snippets

  // markup tags
  clVarText             = clPurple;     // <var> tag text

implementation

end.

