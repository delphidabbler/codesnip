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
  // Project
  CS.Database.Types,
  FmGenericOKDlg,
  FrCheckedTV,
  FrSelectSnippets,
  UBaseObjects,
  USearch;


type

  {
  TSelectionSearchDlg:
    Defines a dialogue box that is used to select snippets manually by placing
    check marks next to the required snippet names. The dialogue creates search
    criteria based on the selected snippets.
  }
  TSelectionSearchDlg = class(TGenericOKDlg, INoPublicConstruct)
    btnClearAll: TButton;
    btnSelectAll: TButton;
    frmSelect: TSelectSnippetsFrame;
    btnExpandAll: TButton;
    btnCollapseAll: TButton;
    lblOverwriteSearch: TLabel;
    procedure btnClearAllClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnExpandAllClick(Sender: TObject);
    procedure btnCollapseAllClick(Sender: TObject);
  strict private
    fSearch: ISearch; // Search corresponding to snippets selected by the user
    procedure SetSelectedSnippets(Value: ISnippetIDList);
      {Stores a list of snippets in snippet selection frame. Frame then selects
      all listed snippets in its treeview.
        @param Value [in] List of snippet IDs.
      }
    procedure SelectionChanged(Sender: TObject);
      {OnChange event handler for snippet selection frame. Disables OK button if
      no snippet selected.
        @param Sender [in] Not used.
      }
  strict protected
    procedure ConfigForm; override;
    procedure InitForm; override;
      {Initialises form by ensuring treeview is collapsed.
      }
    procedure AfterShowForm; override;
      {Restores default cursor after form shown.
      }
  public
    class function Execute(const AOwner: TComponent;
      SelectedSnippets: ISnippetIDList; out ASearch: ISearch): Boolean;
      {Displays dialogue and returns search object based on entered criteria.
        @param AOwner [in] Component that owns this dialogue.
        @param SelectedSnippets [in] Default list of IDs of selected snippets.
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
  DB.UMain,
  UCtrlArranger,
  UQuery;


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
  frmSelect.Clear;
end;

procedure TSelectionSearchDlg.btnCollapseAllClick(Sender: TObject);
begin
  frmSelect.CollapseTree;
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
    frmSelect.GetSelection
  );
  // Create search object from the entered criteria
  fSearch := TSearchFactory.CreateSearch(Filter);
end;

procedure TSelectionSearchDlg.btnSelectAllClick(Sender: TObject);
  {Select All button click handler. Selects all snippets in database.
    @param Sender [in] Not used.
  }
begin
  frmSelect.SelectSnippets(Database.GetAllSnippets);
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
  SelectedSnippets: ISnippetIDList; out ASearch: ISearch): Boolean;
  {Displays dialogue and returns search object based on entered criteria.
    @param AOwner [in] Component that owns this dialogue.
    @param SelectedSnippets [in] Default list of IDs of selected snippets.
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
  dialogue box loads.
    @param Sender [in] Not used.
  }
begin
  inherited;
  frmSelect.OnChange := SelectionChanged;
  Screen.Cursor := crHourGlass;
end;

procedure TSelectionSearchDlg.InitForm;
  {Initialises form by ensuring treeview is collapsed.
  }
begin
  inherited;
  frmSelect.CollapseTree;
end;

procedure TSelectionSearchDlg.SelectionChanged(Sender: TObject);
  {OnChange event handler for snippet selection frame. Disables OK button if no
  snippet selected.
    @param Sender [in] Not used.
  }
begin
  btnOK.Enabled := frmSelect.HasSelection;
end;

procedure TSelectionSearchDlg.SetSelectedSnippets(Value: ISnippetIDList);
  {Stores a list of snippets in snippet selection frame. Frame then selects all
  listed snippets in its treeview.
    @param Value [in] List of snippet IDs.
  }
begin
  if Assigned(Value) then
    frmSelect.SelectSnippets(Value)
  else
    frmSelect.Clear;
end;

end.

