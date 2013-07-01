{
 * IntfFrameMgrs.pas
 *
 * Declares interfaces, constants and enumerations that are supported or needed
 * by frames and other classes that manage display of, and user interaction
 * with, various parts of CodeSnip's UI.
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
 * The Original Code is IntfFrameMgrs.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit IntfFrameMgrs;


interface


uses
  // Delphi
  SHDocVw, ActiveX,
  // Project
  Browser.IntfDocHostUI, Compilers.UGlobals, UCommandBars, USnippets, UView;


const
  // Constants that index tabs in overview pane
  cCategorisedTab = 0;    // categorised tab
  cAlphabeticTab = 1;     // alphabetical tab
  cKindTab = 2;           // snippet kind tab

  // Constants that index tabs in detail pane
  cInfoTab = 0;           // detail information tab
  cCompCheckTab = 1;      // compiler check tab

  // Constants that identify command bars in overview pane
  // toolbar
  cOverviewToolBar: TCommandBarID =  1;
  // popup menu
  cOverviewPopupMenu: TCommandBarID = 2;

  // Constants that identify command bars in detail pane
  // default menu
  cDetailPopupMenuDefault: TCommandBarID = CONTEXT_MENU_DEFAULT;
  // mouse over image control
  cDetailPopupMenuImage: TCommandBarID = CONTEXT_MENU_IMAGE;
  // mouse over HTML control
  cDetailPopupMenuControl: TCommandBarID = CONTEXT_MENU_CONTROL;
  // mouse over HTML table
  cDetailPopupMenuTable: TCommandBarID = CONTEXT_MENU_TABLE;
  // mouse over selected table
  cDetailPopupMenuTextSelect: TCommandBarID = CONTEXT_MENU_TEXTSELECT;
  // mouse over HTML anchor
  cDetailPopupMenuAnchor: TCommandBarID = CONTEXT_MENU_ANCHOR;
  // unknown menu
  cDetailPopupMenuUnknown: TCommandBarID = CONTEXT_MENU_UNKNOWN;
  // first and last ids
  cDetailPopupMenuFirst = CONTEXT_MENU_DEFAULT;
  cDetailPopupMenuLast = CONTEXT_MENU_UNKNOWN;


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
    procedure Initialise(const TabIdx: Integer);
      {Initialise frame with specified tab selected.
        @param TabIdx [in] Index of required tab.
      }
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
    procedure SaveTreeState;
      {Saves current expansion state of treeview in memory.
      }
    procedure RestoreTreeState;
      {Restores last saved treeview expansion state from memory.
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

