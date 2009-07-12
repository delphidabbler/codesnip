{
 * IntfFrameMgrs.pas
 *
 * Declares interfaces, constants and enumerations that are supported or needed
 * by frames and other classes that manage display of, and user interaction
 * with, various parts of CodeSnip's UI.
 *
 * v0.1 of 30 Jan 2005  - Original version.
 * v0.2 of 19 Feb 2005  - Refactoring: renamed all *CommandMgr interfaces as
 *                        *ActionMgr.
 * v0.3 of 20 Feb 2005  - Added methods to add actions for download database and
 *                        quick start help ICompCheckActionMgr action manager.
 * v0.4 of 22 Feb 2005  - Removed all I***ActionMgr interfaces - no longer
 *                        required.
 *                      - Added new ISetWBExternal interface used to pass web
 *                        browser extender object to frames.
 * v0.5 of 28 Jan 2006  - Added new parameter to TCompCheckFrame.Display to
 *                        force redisplay of view item even if already
 *                        displayed.
 * v0.6 of 16 Apr 2006  - Removed ISetWBExternal interface and replaced with
 *                        IWBCustomiser that contains ISetWBExternal's former
 *                        SetExternalObj method plus one new method to set
 *                        browser's drag drop handler.
 * v1.0 of 24 May 2006  - Completed, improved and corrected comments.
 *                        Removed warnings about un-fixed interfaces - now
 *                        fixed.
 * v1.1 of 23 Nov 2006  - Added TestUnitVisible property and accessors to
 *                        ICompCheckDisplayMgr.
 * v1.2 of 02 Dec 2006  - Updated re change to single compiler check display
 *                        that is updated dynamically:
 *                        - Deleted TCompCheckDisplayStyle enumeration.
 *                        - Deleted TCompCheckDisplayStyle parameter from
 *                          Display function prototypes.
 *                        - Added new DisplayCompileResults() method to
 *                          ICompCheckDisplayMgr.
 * v1.3 of 04 Feb 2007  - Changed IInfoDisplayMgr.Display by adding extra Force
 *                        Boolean parameter.
 *                      - Replaced redundant TDetailView class references with
 *                        TViewItem.
 * v1.4 of 11 Feb 2007  - Added IsActive, NextTab and PreviousTab methods to
 *                        ITabFrameDisplayMgr.
 *                      - Added new IWBInfo interface that gets information
 *                        about a browser control.
 * v2.0 of 16 Feb 2007  - Heavily revised interfaces used by main display:
 *                        - Renamed ITabFrameDisplayMgr as ITabbedDisplayMgr,
 *                          gave new GUI and descended from IInterface rather
 *                          than IFrameDisplayMgr. Deleted IsActive method - the
 *                          equivalent is now in IPaneInfo.
 *                        - Changed IOverviewDisplayMgr to derive from
 *                          IInterface rather than ITabFrameDisplayMgr, added
 *                          new Clear method and gave new GUID.
 *                        - Changed IWBDisplayMgr to derive from IInterface
 *                          rather than IFrameDisplayMgr. Gave new GUID.
 *                        - Added new IViewItemDisplayMgr interface.
 *                        - Changed ICompCheckDisplayMgr to derive from
 *                          IInterface rather than IWBDisplayMgr. Deleted
 *                          Display method. Gave new GUID.
 *                        - Deleted IFrameDisplayMgr, IInfoDisplayMgr and
 *                          IDetailDisplayMgr interfaces.
 *                        - Added new IPaneInfo interface.
 *                        - Deleted IWBInfo interface - we now use IPaneInfo
 *                          instead.
 * v2.1 of 17 Feb 2007  - Removed redundant IWBDisplayMgr WebBrowser property
 *                        and associated accessor method.
 * v2.2 of 09 Jan 2009  - Added new parameter toICompCheckDisplayMgr's
 *                        DisplayCompileResults method that passes instance of
 *                        compilers object whose results are to be displayed.
 * v2.3 of 06 Jun 2009  - Modified tab index constants for revised 2nd tab and
 *                        new 3rd tab.
 *                      - Added methods to IOverviewDisplayMgr to support
 *                        expanding / collapsing of nodes in tree view.
 *                      - Removed methods from ICompCheckDisplayMgr that
 *                        supported toggling visibility of test units in
 *                        compiler check pane.
 *                      - Added constants to identify command bars in overview
 *                        frame.
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
 * The Original Code is IntfFrameMgrs.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit IntfFrameMgrs;


interface


uses
  // Delphi
  SHDocVw, ActiveX,
  // Project
  IntfCompilers, UCommandBars, USnippets, UView;


const
  // Constants that index tabs in overview pane
  cCategorisedTab = 0;    // categorised tab
  cAlphabeticTab = 1;     // alphabetical tab
  cKindTab = 2;           // snippet kind tab

  // Constants that index tabs in detail pane
  cInfoTab = 0;           // detail information tab
  cCompCheckTab = 1;      // compiler check tab

  // Constants that identify command bars in overview pane
  cOverviewToolBar: TCommandBarID =  1;     // toolbar
  cOverviewPopupMenu: TCommandBarID = 2;    // popup menu


type

  //////////////////////////////////////////////////////////////////////////////
  // Display manager interfaces
  //
  // These interfaces define methods for managing the display of the various
  // panes in the user interface.
  //
  // ITabbedDisplayMgr
  //   Implemented by panes that support tab sets.
  //
  // IOverviewDisplayMgr
  //   Implemented by panes that can display and select snippets.
  //
  // IViewItemDisplayMgr
  //   Implemented by panes that display a view item in detail
  //
  // ICompCheckDisplayMgr
  //   Implemented by panes that can display test compile related information.
  //
  // IWBDisplayMgr
  //   Implemented by panes that host a web browser control.
  //
  //////////////////////////////////////////////////////////////////////////////

  {
  ITabbedDisplayMgr:
    Interface implemented by panes that support tab sets.
  }
  ITabbedDisplayMgr = interface(IInterface)
    ['{5445BF4F-0A02-48E6-A6FA-AE0FFC2F9939}']
    procedure SelectTab(const TabIdx: Integer);
      {Selects tab with specified index.
        @param TabIdx [in] Index of tab to be selected.
      }
    function SelectedTab: Integer;
      {Returns index of currently selected tab.
        @return Required tab index.
      }
    procedure NextTab;
      {Switches to next tab, or return to first tab if current tab is last.
      }
    procedure PreviousTab;
      {Switches to previous tab, or return to last tab if current tab is first.
      }
  end;

  {
  TTreeNodeAction:
    Enumeration that describes the different expand/collapse operations of tree
    views.
  }
  TTreeNodeAction = (
    taExpandNode,     // expand selected node
    taExpandAll,      // expand all nodes
    taCollapseNode,   // collapse selected node
    taCollapseAll     // collapse all nodes
  );

  {
  IOverviewDisplayMgr:
    Interface implemented by panes that can display and select snippets.
  }
  IOverviewDisplayMgr = interface(IInterface)
    ['{AD5D5A0F-E7D3-4173-A4F9-04D43909B0F5}']
    procedure Display(const RoutineList: TRoutineList);
      {Displays the snippets in the current overview tab.
        @param RoutineList [in] List of snippets to be displayed.
      }
    procedure Clear;
      {Clears the display.
      }
    procedure SelectItem(const ViewItem: TViewItem);
      {Select a view item in the overview pane.
        @param ViewItem [in] Item to be selected. Pass nil to deselect current
          item.
      }
    procedure UpdateTreeState(const State: TTreeNodeAction);
      {Update expand / collapse state of nodes in a treeview.
        @param State [in] Required expand / collapse state.
      }
    function CanUpdateTreeState(const State: TTreeNodeAction): Boolean;
      {Checks if a tree view can support a node expand / collapse request.
        @param State [in] Expand / collapse state being queried.
        @return True if request can be fulfilled by treeview, False if not.
      }
  end;

  {
  IWBDisplayMgr:
    Interface implemented by panes that host a web browser control.
  }
  IWBDisplayMgr = interface(IInterface)
    ['{DE875803-D856-402C-BC3D-DBF79A0670D7}']
    procedure Activate;
      {Activates the frame (when it is shown).
      }
    procedure Deactivate;
      {Deactivates the frame (when it is hidden).
      }
  end;

  {
  IViewItemDisplayMgr:
    Interface implemented by panes that display a view item in detail.
  }
  IViewItemDisplayMgr = interface(IInterface)
    ['{1FE68233-1AD7-44C3-A6C0-3974E0C0455E}']
    procedure Display(const View: TViewItem; const Force: Boolean = False);
      {Displays detailed information for a view. Pass nil to clear.
        @param View [in] Information about item to view.
        @param Force [in] Forces view item to be re-displayed even if not
          changed.
      }
  end;

  {
  ICompCheckDisplayMgr:
    Interface implemented by panes that can display test compile related
    information.
  }
  ICompCheckDisplayMgr = interface(IInterface)
    ['{E4E9B05D-E53C-4448-946D-750BDA45328C}']
    procedure DisplayCompileResults(const ACompilers: ICompilers);
      {Displays results of test compilation in pane.
        @param ACompilers [in] Compilers object containing required results.
      }
  end;


  //////////////////////////////////////////////////////////////////////////////
  // Other pane-related interfaces
  //////////////////////////////////////////////////////////////////////////////

  {
  IClipboardMgr:
    Defines methods implemented by frames that can copy to the Windows
    clipboard.
  }
  IClipboardMgr = interface(IInterface)
    ['{328EF9CA-5510-4D77-A570-7F499C0CFA79}']
    function CanCopy: Boolean;
      {Checks whether text can be copied to clipboard from frame.
        @return True if text can be copied.
      }
    procedure CopyToClipboard;
      {Copies selected text to clipboard.
      }
  end;

  {
  ISelectionMgr:
    Defines methods implemented by frames that can select text.
  }
  ISelectionMgr = interface(IInterface)
    ['{70FEFF07-75B5-4A82-A903-3A1B096E1D7C}']
    function CanSelectAll: Boolean;
      {Checks whether text can be selected in frame.
        @return True if text can be selected.
      }
    procedure SelectAll;
      {Selects all text in active control of frame.
      }
  end;

  {
  IWBCustomiser:
    Defines methods implemented by frames that customise the behaviour of the
    web browser control.
  }
  IWBCustomiser = interface(IInterface)
    ['{B46CDC61-EC43-43E3-838C-73AB8F150E46}']
    procedure SetExternalObj(const Obj: IDispatch);
      {Provides an object that is used to extend a web browser's external
      object.
        @param Obj [in] External browser object extender.
      }
    procedure SetDragDropHandler(const Obj: IDropTarget);
      {Provides an object used by web browser control to handle drag-drop
      operations.
        @param Obj [in] Drag-drop handler.
      }
  end;

  {
  IPaneInfo:
    Interface supported by pane objects to provide information about the state
    of the GUI.
  }
  IPaneInfo = interface(IInterface)
    ['{AB826763-0589-440D-B849-BE70C6E152D0}']
    function IsInteractive: Boolean;
      {Checks if pane, or one of its child controls, is currently interactive
      with user. This can mean a child control has input focus or a browser
      control's UI is active.
        @return True if pane or a child control is interactive, False if not.
      }
  end;


implementation

end.

