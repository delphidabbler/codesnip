{
 * FmSelectionSearchDlg.pas
 *
 * Dialog box that is used to select routines manually by placing check marks
 * next to the required routine names. The dialog creates search criteria based
 * on the selected routines.
 *
 * v1.0 of 29 Oct 2006  - Original version.
 * v1.1 of 12 Nov 2006  - Updated form file re tree view name change in hosted
 *                        frame.
 * v1.2 of 08 Feb 2007  - Re-assigned form's OnCreate event handler that was
 *                        uncoupled when handler in ancestor class was deleted.
 * v1.3 of 03 Jan 2009  - Added buttons to select all main database or user
 *                        defined routines.
 * v1.4 of 16 May 2009  - Changed caption to Select Snippets from Select
 *                        Routines.
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
 * The Original Code is FmSelectionSearchDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FmSelectionSearchDlg;


interface


uses
  // Delphi
  Forms, StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericOKDlg, FrCheckedTV, FrSelectSnippets, USearch, USnippets;


type

  {
  TSelectionSearchDlg:
    Defines a dialog box that is used to select snippets manually by placing
    check marks next to the required snippet names. The dialog creates search
    criteria based on the selected snippets.
  }
  TSelectionSearchDlg = class(TGenericOKDlg)
    btnClearAll: TButton;
    btnMainDB: TButton;
    btnSelectAll: TButton;
    btnUserDB: TButton;
    frmSelect: TSelectSnippetsFrame;
    procedure btnClearAllClick(Sender: TObject);
    procedure btnMainDBClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnUserDBClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  strict private
    fSearch: ISearch;
      {Search that corresponds to snippets selected by the user}
    procedure SetSelectedRoutines(const Value: TRoutineList);
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
    procedure InitForm; override;
      {Initialises form. Disables User Defined button if there are no user
      defined snippets in database.
      }
  public
    class function Execute(const AOwner: TComponent;
      const SelectedRoutines: TRoutineList; out ASearch: ISearch): Boolean;
      {Displays dialog and returns search object based on entered criteria.
        @param AOwner [in] Component that owns this dialog.
        @param SelectedRoutines [in] Default list of selected snippets.
        @param ASearch [out] Search to be performed if user OKs. Has criteria
          that causes all snippets selected by user to be returned by search.
          Set to nil if user cancels.
        @return True if user OKs and false if user cancels.
      }
    property Search: ISearch
      read fSearch;
      {Search object that corresponds to criteria entered in dialog box. Set to
      nil if users cancels}
  end;


implementation


uses
  // Delphi
  SysUtils;


{$R *.dfm}


{ TSelectionSearchDlg }

procedure TSelectionSearchDlg.btnClearAllClick(Sender: TObject);
  {Clear All button click handler. Deselects all snippets.
    @param Sender [in] Not used.
  }
begin
  // Assigning nil to snippet selection frame's SelectedRoutines property clears
  // the list.
  frmSelect.SelectedRoutines := nil;
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
  SearchCriteria: ISelectionSearchCriteria; // user's search criteria
  Routines: TStringList;                    // list of selected snippets
  RoutineIdx: Integer;                      // loops thru all selected snippets
begin
  inherited;
  // Create search criteria for all selected snippets
  Routines := TStringList.Create;
  try
    for RoutineIdx := 0 to Pred(frmSelect.SelectedRoutines.Count) do
      Routines.Add(frmSelect.SelectedRoutines.Items[RoutineIdx].Name);
    SearchCriteria := TSearchCriteriaFactory.CreateSelectionSearchCriteria(
      Routines
    );
  finally
    FreeAndNil(Routines);
  end;
  // Create search object from the entered criteria
  fSearch := TSearchFactory.CreateSelectionSearch(SearchCriteria);
end;

procedure TSelectionSearchDlg.btnSelectAllClick(Sender: TObject);
  {Select All button click handler. Selects all snippets in database.
    @param Sender [in] Not used.
  }
begin
  // Storing all snippets in database in snippet selection frame's
  // SelectedRoutines property causes all snippets to be selected
  frmSelect.SelectedRoutines := Snippets.Routines;
end;

procedure TSelectionSearchDlg.btnUserDBClick(Sender: TObject);
  {User Defined button click handler. Selects all user defined snippets in
  database.
    @param Sender [in] Not used.
  }
begin
  SelectDB(True);
end;

class function TSelectionSearchDlg.Execute(const AOwner: TComponent;
  const SelectedRoutines: TRoutineList; out ASearch: ISearch): Boolean;
  {Displays dialog and returns search object based on entered criteria.
    @param AOwner [in] Component that owns this dialog.
    @param SelectedRoutines [in] Default list of selected snippets.
    @param ASearch [out] Search to be performed if user OKs. Has criteria that
      causes all snippets selected by user to be returned by search. Set to nil
      if user cancels.
    @return True if user OKs and false if user cancels.
  }
begin    
  with TSelectionSearchDlg.Create(AOwner) do
    try
      SetSelectedRoutines(SelectedRoutines);
      Result := (ShowModal = mrOK);
      ASearch := Search;
    finally
      Free;
    end;
end;

procedure TSelectionSearchDlg.FormCreate(Sender: TObject);
  {Form creation event handler. Sets up contained snippet selection frame.
    @param Sender [in] Not used.
  }
begin
  inherited;
  frmSelect.OnChange := SelectionChanged;
end;

procedure TSelectionSearchDlg.InitForm;
  {Initialises form. Disables User Defined button if there are no user defined
  snippets in database.
  }
begin
  inherited;
  btnUserDB.Enabled := Snippets.Routines.Count(True) > 0;
end;

procedure TSelectionSearchDlg.SelectDB(const UserDefined: Boolean);
  {Selects all snippets from either main or user defined database.
    @param UserDefined [in] Flag true if user-defined snippets are to be
      selected, False if main database snippets are to be selected.
  }
var
  Routine: TRoutine;          // references each snippet in database
  RoutineList: TRoutineList;  // list of selected snippets
begin
  RoutineList := TRoutineList.Create;
  try
    for Routine in Snippets.Routines do
      if Routine.UserDefined = UserDefined then
        RoutineList.Add(Routine);
    frmSelect.SelectedRoutines := RoutineList;
  finally
    FreeAndNil(RoutineList);
  end;
end;

procedure TSelectionSearchDlg.SelectionChanged(Sender: TObject);
  {OnChange event handler for snippet selection frame. Disables OK button if no
  snippet selected.
    @param Sender [in] Not used.
  }
begin
  btnOK.Enabled := frmSelect.SelectedRoutines.Count > 0;
end;

procedure TSelectionSearchDlg.SetSelectedRoutines(const Value: TRoutineList);
  {Stores a list of snippets in snippet selection frame. Frame then selects all
  listed snippets in its treeview.
    @param Value [in] List of snippets.
  }
begin
  frmSelect.SelectedRoutines := Value;
end;

end.

