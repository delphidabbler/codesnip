{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that enables the user to the select the snippets
 * that are to be displayed.
}


unit FmSelectionSearchDlg;


interface


uses
  // Delphi
  Forms,
  StdCtrls,
  Controls,
  ExtCtrls,
  Classes,
  Buttons,
  Menus,
  // Project
  DB.USnippet,
  DB.Vaults,
  FmGenericOKDlg,
  FrCheckedTV,
  FrSelectSnippets,
  FrSelectSnippetsBase,
  UBaseObjects,
  USearch;


type

  {
  TSelectionSearchDlg:
    Defines a dialog box that is used to select snippets manually by placing
    check marks next to the required snippet names. The dialog creates search
    criteria based on the selected snippets.
  }
  TSelectionSearchDlg = class(TGenericOKDlg, INoPublicConstruct)
    btnClearAll: TButton;
    btnSelectAll: TButton;
    frmSelect: TSelectSnippetsFrame;
    btnExpandAll: TButton;
    btnCollapseAll: TButton;
    lblOverwriteSearch: TLabel;
    btnVaults: TBitBtn;
    mnuVaults: TPopupMenu;
    procedure btnClearAllClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnVaultsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnExpandAllClick(Sender: TObject);
    procedure btnCollapseAllClick(Sender: TObject);
  strict private
    fSearch: ISearch; // Search corresponding to snippets selected by the user
    procedure SetSelectedSnippets(const Value: TSnippetList);
      {Stores a list of snippets in snippet selection frame. Frame then selects
      all listed snippets in its treeview.
        @param Value [in] List of snippets.
      }
    procedure SelectionChanged(Sender: TObject);
      {OnChange event handler for snippet selection frame. Disables OK button if
      no snippet selected.
        @param Sender [in] Not used.
      }

    ///  <summary>Selects all snippets from the given vault.</summary>
    ///  <param name="AVaultID"><c>TVaultID</c> ID of the required vault.
    ///  </param>
    procedure SelectDB(const AVaultID: TVaultID);

    ///  <summary>Populates vault pop-up menu with menu items.</summary>
    procedure PopulateVaultsMenu;

    ///  <summary>Handles clicks on vault menu items. Selects snippets belonging
    ///  to the selected vault.</summary>
    procedure VaultMenuClick(Sender: TObject);

  strict protected

    procedure ConfigForm; override;

    ///  <summary>Initialises form. Populates vaults menu and collapses
    ///  treeview.</summary>
    procedure InitForm; override;

    procedure AfterShowForm; override;
      {Restores default cursor after form shown.
      }
  public
    class function Execute(const AOwner: TComponent;
      const SelectedSnippets: TSnippetList; out ASearch: ISearch): Boolean;
      {Displays dialog and returns search object based on entered criteria.
        @param AOwner [in] Component that owns this dialog.
        @param SelectedSnippets [in] Default list of selected snippets.
        @param ASearch [out] Search to be performed if user OKs. Has filter
          that causes all snippets selected by user to be returned by search.
          Set to nil if user cancels.
        @return True if user OKs and false if user cancels.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  Types,
  // Project
  DB.UMain,
  UCtrlArranger,
  UQuery;


{$R *.dfm}


type
  ///  <summary>Custom menu item with additional property to store a compiler
  ///  version.</summary>
  TVaultMenuItem = class(TMenuItem)
  strict private
    var
      ///  <summary>Value of <c>Vault</c> property</summary>
      fVault: TVault;
  public
    ///  <summary>Constructs a menu item with all required properties and event
    ///  handlers.</summary>
    ///  <param name="AOwner">TComponent [in] Menu item's owner.</param>
    ///  <param name="AVault"><c>TVault</c> [in] Vault whose name is
    ///  displayed in menu item.</param>
    ///  <param name="AClickHandler">TNotifyEvent [in] Reference to an event
    ///  handler for menu item's OnClick event.</param>
    constructor Create(AOwner: TComponent; const AVault: TVault;
      const AClickHandler: TNotifyEvent); reintroduce;
    ///  <summary>Vault whose name is displayed in the menu item.</summary>
    property Vault: TVault read fVault write fVault;
  end;

{ TSelectionSearchDlg }

procedure TSelectionSearchDlg.AfterShowForm;
  {Restores default cursor after form shown.
  }
begin
  inherited;
  Screen.Cursor := crDefault;
end;

procedure TSelectionSearchDlg.btnClearAllClick(Sender: TObject);
  {Clear All button click handler. Deselects all snippets.
    @param Sender [in] Not used.
  }
begin
  // Assigning nil to snippet selection frame's SelectedSnippets property clears
  // the list.
  frmSelect.SelectedSnippets := nil;
end;

procedure TSelectionSearchDlg.btnCollapseAllClick(Sender: TObject);
begin
  frmSelect.CollapseTree;
end;

procedure TSelectionSearchDlg.btnVaultsClick(Sender: TObject);
var
  PopupPos: TPoint; // place where menu pops up
begin
  PopupPos := ClientToScreen(
    Point(btnVaults.Left, btnVaults.Top + btnVaults.Height)
  );
  mnuVaults.Popup(PopupPos.X, PopupPos.Y);
end;

procedure TSelectionSearchDlg.btnExpandAllClick(Sender: TObject);
begin
  frmSelect.ExpandTree;
end;

procedure TSelectionSearchDlg.btnOKClick(Sender: TObject);
  {OK button click handler.
    @param Sender [in] Not used.
  }
var
  Filter: ISelectionSearchFilter; // search filter
begin
  inherited;
  // Create search filter for all selected snippets
  Filter := TSearchFilterFactory.CreateManualSelectionSearchFilter(
    frmSelect.SelectedSnippets
  );
  // Create search object from the entered criteria
  fSearch := TSearchFactory.CreateSearch(Filter);
end;

procedure TSelectionSearchDlg.btnSelectAllClick(Sender: TObject);
  {Select All button click handler. Selects all snippets in database.
    @param Sender [in] Not used.
  }
begin
  // Storing all snippets in database in snippet selection frame's
  // SelectedSnippets property causes all snippets to be selected
  frmSelect.SelectedSnippets := Database.Snippets;
end;

procedure TSelectionSearchDlg.ConfigForm;
begin
  inherited;
  frmSelect.CanCollapse := True;
  lblOverwriteSearch.Visible := Query.IsSearchActive;
  if lblOverwriteSearch.Visible then
    TCtrlArranger.SetLabelHeight(lblOverwriteSearch);
end;

class function TSelectionSearchDlg.Execute(const AOwner: TComponent;
  const SelectedSnippets: TSnippetList; out ASearch: ISearch): Boolean;
  {Displays dialog and returns search object based on entered criteria.
    @param AOwner [in] Component that owns this dialog.
    @param SelectedSnippets [in] Default list of selected snippets.
    @param ASearch [out] Search to be performed if user OKs. Has filter that
      causes all snippets selected by user to be returned by search. Set to nil
      if user cancels.
    @return True if user OKs and false if user cancels.
  }
var
  Dlg: TSelectionSearchDlg;
begin
  Dlg := InternalCreate(AOwner);
  try
    Dlg.SetSelectedSnippets(SelectedSnippets);
    Result := (Dlg.ShowModal = mrOK);
    ASearch := Dlg.fSearch;
  finally
    Dlg.Free;
  end;
end;

procedure TSelectionSearchDlg.FormCreate(Sender: TObject);
  {Form creation event handler. Sets up form and displays hourglass cursor while
  dialog box loads.
    @param Sender [in] Not used.
  }
begin
  inherited;
  frmSelect.OnChange := SelectionChanged;
  Screen.Cursor := crHourGlass;
end;

procedure TSelectionSearchDlg.InitForm;
begin
  inherited;
  frmSelect.CollapseTree;
  PopulateVaultsMenu;
end;

procedure TSelectionSearchDlg.PopulateVaultsMenu;

  ///  Adds a menu item for given vault to the pop-up menu.
  procedure AddMenuItem(const AVault: TVault);
  begin
    mnuVaults.Items.Add(
      TVaultMenuItem.Create(mnuVaults, AVault, VaultMenuClick)
    );
  end;

var
  Vault: TVault;
begin
  for Vault in TVaults.Instance do
    AddMenuItem(Vault);
end;

procedure TSelectionSearchDlg.SelectDB(const AVaultID: TVaultID);
var
  Snippet: TSnippet;          // references each snippet in database
  SnippetList: TSnippetList;  // list of selected snippets
begin
  SnippetList := TSnippetList.Create;
  try
    for Snippet in Database.Snippets do
      if Snippet.VaultID = AVaultID then
        SnippetList.Add(Snippet);
    frmSelect.SelectedSnippets := SnippetList;
  finally
    FreeAndNil(SnippetList);
  end;
end;

procedure TSelectionSearchDlg.SelectionChanged(Sender: TObject);
  {OnChange event handler for snippet selection frame. Disables OK button if no
  snippet selected.
    @param Sender [in] Not used.
  }
begin
  btnOK.Enabled := not frmSelect.SelectedSnippets.IsEmpty;
end;

procedure TSelectionSearchDlg.SetSelectedSnippets(const Value: TSnippetList);
  {Stores a list of snippets in snippet selection frame. Frame then selects all
  listed snippets in its treeview.
    @param Value [in] List of snippets.
  }
begin
  frmSelect.SelectedSnippets := Value;
end;

procedure TSelectionSearchDlg.VaultMenuClick(Sender: TObject);
begin
  SelectDB((Sender as TVaultMenuItem).Vault.UID);
end;

{ TVaultMenuItem }

constructor TVaultMenuItem.Create(AOwner: TComponent; const AVault: TVault;
  const AClickHandler: TNotifyEvent);
begin
  inherited Create(AOwner);
  Caption := AVault.Name;
  Vault := AVault;
  OnClick := AClickHandler;
end;

end.

