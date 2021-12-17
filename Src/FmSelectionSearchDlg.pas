{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that enables the user to the select the snippets
 * that are to be displayed.
}


unit FmSelectionSearchDlg;


interface


uses
  // Delphi
  Forms, StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  DB.USnippet, FmGenericOKDlg, FrCheckedTV, FrSelectSnippets,
  FrSelectSnippetsBase, UBaseObjects, USearch;


type

  {
  TSelectionSearchDlg:
    Defines a dialog box that is used to select snippets manually by placing
    check marks next to the required snippet names. The dialog creates search
    criteria based on the selected snippets.
  }
  TSelectionSearchDlg = class(TGenericOKDlg, INoPublicConstruct)
    btnClearAll: TButton;
    btnMainDB: TButton;
    btnSelectAll: TButton;
    btnUserDB: TButton;
    frmSelect: TSelectSnippetsFrame;
    btnExpandAll: TButton;
    btnCollapseAll: TButton;
    lblOverwriteSearch: TLabel;
    procedure btnClearAllClick(Sender: TObject);
    procedure btnMainDBClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnUserDBClick(Sender: TObject);
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
    procedure SelectDB(const UserDefined: Boolean);
      {Selects all snippets from either main or user defined database.
        @param UserDefined [in] Flag true if user-defined snippets are to be
          selected, False if main database snippets are to be selected.
      }
  strict protected
    procedure ConfigForm; override;
    procedure InitForm; override;
      {Initialises form. Disables User Defined button if there are no user
      defined snippets in database.
      }
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
  // Project
  DB.UMain, UCtrlArranger, UQuery;


{$R *.dfm}


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

procedure TSelectionSearchDlg.btnExpandAllClick(Sender: TObject);
begin
  frmSelect.ExpandTree;
end;

procedure TSelectionSearchDlg.btnMainDBClick(Sender: TObject);
  {Main button click handler. Selects all snippets in main database.
    @param Sender [in] Not used.
  }
begin
  SelectDB(False);
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

procedure TSelectionSearchDlg.btnUserDBClick(Sender: TObject);
  {User Defined button click handler. Selects all user defined snippets in
  database.
    @param Sender [in] Not used.
  }
begin
  SelectDB(True);
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
begin
  with InternalCreate(AOwner) do
    try
      SetSelectedSnippets(SelectedSnippets);
      Result := (ShowModal = mrOK);
      ASearch := fSearch;
    finally
      Free;
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
  {Initialises form. Disables User Defined button if there are no user defined
  snippets in database.
  }
begin
  inherited;
  frmSelect.CollapseTree;
  btnUserDB.Enabled := Database.Snippets.Count(True) > 0;
end;

procedure TSelectionSearchDlg.SelectDB(const UserDefined: Boolean);
  {Selects all snippets from either main or user defined database.
    @param UserDefined [in] Flag true if user-defined snippets are to be
      selected, False if main database snippets are to be selected.
  }
var
  Snippet: TSnippet;          // references each snippet in database
  SnippetList: TSnippetList;  // list of selected snippets
begin
  SnippetList := TSnippetList.Create;
  try
    for Snippet in Database.Snippets do
      if Snippet.UserDefined = UserDefined then
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

end.

