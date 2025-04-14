{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2025, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Declares interfaces, constants and enumerations required to manage various
 * parts of CodeSnip's UI.
}


unit IntfFrameMgrs;


interface


uses
  // Delphi
  SHDocVw, ActiveX,
  Classes, // !! For HACK
  // Project
  Browser.IntfDocHostUI, DB.USnippet, Compilers.UGlobals, UCommandBars, UView;


const
  // Indexes of tabs in Overview Pane
  cCategorisedTab = 0;
  cAlphabeticTab = 1;
  cKindTab = 2; // snippet kind tab

  // Identifiers for Overview Pane's command bars
  cOverviewToolBar: TCommandBarID = 1;
  cOverviewPopupMenu: TCommandBarID = 2;

  // Identifiers for Detail Pane's main view command bars
  cDetailPopupMenuDefault: TCommandBarID = CONTEXT_MENU_DEFAULT;
  cDetailPopupMenuImage: TCommandBarID = CONTEXT_MENU_IMAGE;
  cDetailPopupMenuControl: TCommandBarID = CONTEXT_MENU_CONTROL;
  cDetailPopupMenuTable: TCommandBarID = CONTEXT_MENU_TABLE;
  cDetailPopupMenuTextSelect: TCommandBarID = CONTEXT_MENU_TEXTSELECT;
  cDetailPopupMenuAnchor: TCommandBarID = CONTEXT_MENU_ANCHOR;
  cDetailPopupMenuUnknown: TCommandBarID = CONTEXT_MENU_UNKNOWN;
  cDetailPopupMenuFirst = CONTEXT_MENU_DEFAULT;
  cDetailPopupMenuLast = CONTEXT_MENU_UNKNOWN;

  // Set of all Detail Pane popup menu command bars
  cDetailPopupMenuIDs: TCommandBarIDs =
    [cDetailPopupMenuFirst..cDetailPopupMenuLast];

  // Identifier for Detail Pane's tab set command bar
  // (must be different to all cDetailPopupMenuXXX constants)
  cDetailTabSetPopupMenu = Succ(cDetailPopupMenuLast);


////////////////////////////////////////////////////////////////////////////////
// Interfaces that deal with display of snippets and other views              //
////////////////////////////////////////////////////////////////////////////////

type
  ///  <summary>Interface that defines methods for use in managing tab sets.
  ///  </summary>
  ITabbedDisplayMgr = interface(IInterface)
    ['{5445BF4F-0A02-48E6-A6FA-AE0FFC2F9939}']
    ///  <summary>Select tab with given index in set.</summary>
    procedure SelectTab(const TabIdx: Integer);
    ///  <summary>Get index of currently selected tab.</summary>
    function SelectedTab: Integer;
    ///  <summary>Switch to next tab in sequence or go to first tab if current
    ///  tab is last.</summary>
    procedure NextTab;
    ///  <summary>Switch to previous tab in sequence or go to last tab if
    ///  current tab is first.</summary>
    procedure PreviousTab;
  end;

type
  ///  <summary>Interface that defines operations on Detail Pane relating to
  ///  display of views in one or more tabs.</summary>
  IDetailPaneDisplayMgr = interface(IInterface)
    ['{79F8BCAB-4C3F-4935-B5BF-9E5B9B32D88A}']
    /// <summary>Return currently selected view.</summary>
    function SelectedView: IView;
    ///  <summary>Select detail pane tab with given index.</summary>
    procedure SelectTab(const TabIdx: Integer);
    ///  <summary>Find index of tab displaying given view or -1 if no such tab.
    ///  </summary>
    function FindTab(ViewKey: IViewKey): Integer;
    ///  <summary>Display given view in tab with given index.</summary>
    procedure Display(View: IView; const TabIdx: Integer);
    ///  <summary>Reload the currently displayed views.</summary>
    procedure Reload;
    ///  <summary>Close all tabs and delete all views.</summary>
    procedure Clear;
    ///  <summary>Create a new tab displaying given view and return its index.
    ///  </summary>
    function CreateTab(View: IView): Integer;
    ///  <summary>Check if tab set is empty, i.e. there are no tabs displayed.
    ///  </summary>
    function IsEmptyTabSet: Boolean;
    ///  <summary>Close tab at given index.</summary>
    procedure CloseTab(const TabIdx: Integer);
    ///  <summary>Close all tabs, or all except selected tabs, depending on
    ///  whether KeepSelected is False or True, respectively.</summary>
    procedure CloseMultipleTabs(const KeepSelected: Boolean);
  end;

type
  ///  <summary>Enumeration describing the various expand / collapse operations
  ///  on the nodes of a tree view.</summary>
  TTreeNodeAction = (
    taExpandNode,   // expand selected node
    taExpandAll,    // expand all nodes
    taCollapseNode, // collapse selected node
    taCollapseAll   // collapse all nodes
    );

type
  ///  <summary>Interface that defines operations on Overview Pane relating to
  ///  manipulation of, and interaction with, tree view that lists and groups
  ///  snippets.</summary>
  IOverviewDisplayMgr = interface(IInterface)
    ['{AD5D5A0F-E7D3-4173-A4F9-04D43909B0F5}']
    ///  <summary>Initialise frame with given tab selected.</summary>
    procedure Initialise(const TabIdx: Integer);
    ///  <summary>Display given list of snippets in the current overview tab.
    ///  </summary>
    ///  <remarks>If given snippet list is same as that displayed it may not
    ///  be redisplayed unless Force parameter is True.</remarks>
    procedure Display(const SnippetList: TSnippetList; const Force: Boolean);
    ///  <summary>Clear the display.</summary>
    procedure Clear;
    ///  <summary>Select given view item in tree view.</summary>
    ///  <remarks>Pass a nil parameter to deselect current selection.</remarks>
    procedure SelectItem(ViewItem: IView);
    ///  <summary>Update expand / collapse state of tree nodes as specified by
    ///  State.</summary>
    procedure UpdateTreeState(const State: TTreeNodeAction);
    ///  <summary>Check if tree view can support the expand / collapse request
    ///  specified in State.</summary>
    function CanUpdateTreeState(const State: TTreeNodeAction): Boolean;
    ///  <summary>Save current expand / collapse state of tree view.
    ///  </summary>
    procedure SaveTreeState;
    ///  <summary>Restore expand / collapse state of treeview to last save
    ///  state.</summary>
    procedure RestoreTreeState;
    ///  <summary>!! HACK: Sets an event handler on the tree view to work
    ///  around a bug that can occur after resuming from hibernation.</summary>
    procedure _HACK_SetHibernateHandler(const AHandler: TNotifyEvent);
  end;

type
  ///  <summary>Interface that defines operations on Detail Pane relating to
  ///  display of a view item in detail.</summary>
  IViewItemDisplayMgr = interface(IInterface)
    ['{1FE68233-1AD7-44C3-A6C0-3974E0C0455E}']
    ///  <summary>Display detailed information about given view.</summary>
    procedure Display(View: IView);
  end;


////////////////////////////////////////////////////////////////////////////////
// Other pane-related interfaces                                              //
////////////////////////////////////////////////////////////////////////////////

type
  ///  <summary>Interface that defines methods implemented by frames that
  ///  support copying to the clipboard.</summary>
  IClipboardMgr = interface(IInterface)
    ['{328EF9CA-5510-4D77-A570-7F499C0CFA79}']
    ///  <summary>Check whether anything can currently be copied to the
    ///  clipboard.</summary>
    function CanCopy: Boolean;
    ///  <summary>Copy data to clipboard.</summary>
    ///  <remarks>Behaviour when there is no available data is not defined.
    ///  </remarks>
    procedure CopyToClipboard;
  end;

type
  ///  <summary>Interface that defines methods implemented by frames that
  ///  support selection of text.</summary>
  ISelectionMgr = interface(IInterface)
    ['{70FEFF07-75B5-4A82-A903-3A1B096E1D7C}']
    ///  <summary>Check whether any text can currently be selected.</summary>
    function CanSelectAll: Boolean;
    ///  <summary>Selects all text.</summary>
    ///  <remarks>Behaviour when no text can be selected is not defined.
    ///  </remarks>
    procedure SelectAll;
  end;

type
  ///  <summary>Interface that defines operations supported by frames that
  ///  can customise the behaviour of a hosted web browser control.</summary>
  IWBCustomiser = interface(IInterface)
    ['{B46CDC61-EC43-43E3-838C-73AB8F150E46}']
    ///  <summary>Use the given object to extend the browser control's 'external
    ///  object'.</summary>
    procedure SetExternalObj(Obj: IDispatch);
    ///  <summary>Use the given object to handle drag-drop operations for the
    ///  browser control.</summary>
    procedure SetDragDropHandler(Obj: IDropTarget);
  end;

type
  ///  <summary>Interface the defines operations supported by frames to provide
  ///  information about the state of the UI.</summary>
  IPaneInfo = interface(IInterface)
    ['{AB826763-0589-440D-B849-BE70C6E152D0}']
    ///  <summary>Check if the frame, or one of its child controls, is currently
    ///  interactive with the user.</summary>
    ///  <remarks>This can mean a child control has input focus or a browser
    ///  control's UI is active.</remarks>
    function IsInteractive: Boolean;
  end;

implementation

end.

