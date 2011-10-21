{
 * FrInfo.pas
 *
 * Frame that displays and manages user interaction with the pane that displays
 * details of snippets and other selected view items.
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
 * The Original Code is FrInfo.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributors:
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FrInfo;


interface


uses
  // Delphi
  OleCtrls, SHDocVw, Classes, Controls, ExtCtrls,
  // Project
  FrDetailView, IntfFrameMgrs, IntfHTMLDocHostInfo, UActiveTextHTML,
  UCSSBuilder, UDetailPageLoader, USearch;


type

  {
  TInfoFrame:
    Frame that displays and manages user interaction with the pane that displays
    details of snippets and other selected view items. It implements display
    manager, clipboard manager and selection manager interfaces. It also manages
    the contained web browser control and provides an extension to the browser's
    "external" object via its ISetWBExternal interface.
  }
  TInfoFrame = class(TDetailViewFrame,
    IWBDisplayMgr,                         // support for hosted browser control
    IViewItemDisplayMgr,                                 // displays a view item
    IPaneInfo,                                // provides information about pane
    IWBCustomiser,                             // customises web browser control
    IClipboardMgr,                                          // clipboard manager
    ISelectionMgr,                                          // selection manager
    IHTMLDocHostInfo                        // info for use in HTML manipulation
  )
  end;


implementation


{$R *.dfm}


end.

