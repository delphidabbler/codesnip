{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that is used to select criteria for searches for
 * snippets that compile or don't compile with selected compilers.
 *
 * Also defines a class that is used to persist the last chosen search criteria.
}


unit FmFindCompilerDlg;


interface


uses
  // Delphi
  StdCtrls, ExtCtrls, Controls, CheckLst, Classes,
  // Project
  Compilers.UGlobals, FmGenericOKDlg, UBaseObjects, USearch;


type

  TCompilerSearchParams = class;

  {
  TFindCompilerDlg:
    Defines a dialog box that is used to select criteria for searches for
    snippets that compile or don't compile with selected compilers.
  }
  TFindCompilerDlg = class(TGenericOKDlg, INoPublicConstruct)
    btnClearAll: TButton;
    btnSelectAll: TButton;
    cbCriteria: TComboBox;
    lbCompilerVers: TCheckListBox;
    lblCompilerVer: TLabel;
    lblCriteria: TLabel;
    rgLogic: TRadioGroup;
    rgScope: TRadioGroup;
    procedure btnClearAllClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure cbCriteriaSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbCompilerVersClickCheck(Sender: TObject);
  strict private
    fCompilers: ICompilers;               // Provides info about compilers
    fSearchParams: TCompilerSearchParams; // Persistent compiler search options
    fSearch: ISearch;                     // Search entered by user
    fRefinePreviousSearch: Boolean;       // Whether to refine previous search

    procedure UpdateOKBtn;
      {Updates state of OK button according to whether valid entries made in
      dialog.
      }
    function CheckedCompilerCount: Integer;
      {Returns number of compilers selected in check list box.
        @return Number of checked list items.
      }
  strict protected
    procedure InitForm; override;
      {Populates and initialises controls.
      }
    procedure ArrangeForm; override;
  public
    class function Execute(const AOwner: TComponent;
      out ASearch: ISearch; out RefineExisting: Boolean): Boolean;
      {Displays dialog and returns search object based on entered criteria.
        @param AOwner [in] Component that owns this dialog.
        @param ASearch [out] Set to value of Search property: nil if user
          cancels.
        @param RefineExisting [out] Set to flag indicating if any existing
          search is to be refined (True) or if this search is to apply to whole
          database (False).
        @return True if user OKs and search object created or false if user
          cancels and search object is nil.
      }
  end;

  {
  TCompilerSearchParams:
    Object used to store persistent parameters for compiler searches.
    Format within ini file is
    + 'Logic' entry with value Ord(slAnd) or Ord(slOr)
    + entry for each supported compiler with value 1 if in search and 0 if not.
    + 'Option' entry with value Ord(one of TCompilerSearchOption values)
  }
  TCompilerSearchParams = class(TObject)
  strict private
    fCompilersObj: ICompilers;            // Provides info about compilers
    fUpdated: Boolean;                    // Flags if properties updated
    fCompilers: TCompilerSearchCompilers; // Value of Compilers property
    fOption: TCompilerSearchOption;       // Value of Option property
    fLogic: TSearchLogic;                 // Value of Logic property
    procedure ReadValues;
      {Reads search parameters from persistent storage.
      }
    procedure WriteValues;
      {Writes search parameters to persistent storage.
      }
    procedure SetCompilers(const Value: TCompilerSearchCompilers);
      {Write access method for Compilers property. Records value and flags
      updated.
        @param Value The new property value.
      }
    procedure SetLogic(const Value: TSearchLogic);
      {Write access method for Logic property. Records value and flags updated.
        @param Value The new property value.
      }
    procedure SetOption(const Value: TCompilerSearchOption);
      {Write access method for Option property. Records value and flags updated.
        @param Value The new property value.
      }
  public
    constructor Create(const Compilers: ICompilers);
      {Class constructor. Sets up object with parameters from persistent
      storage.
        @param Compilers [in] Reference to object that provides information
          about compilers.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object and saves current values to
      persistent storage if they have changed.
      }
    property Option: TCompilerSearchOption
      read fOption write SetOption;
      {Compiler search option. Specifies type of search we are running: see
      TCompilerOption definition for possible values}
    property Logic: TSearchLogic
      read fLogic write SetLogic;
      {Search logic. Either AND or OR}
    property Compilers: TCompilerSearchCompilers
      read fCompilers write SetCompilers;
      {Set of compilers to include in search}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  Compilers.UCompilers, UCtrlArranger, UQuery, USettings;


{$R *.dfm}

resourcestring
  // Text that appears in option combo box
  sCompileOK      = 'Compiles';
  sCompileNoWarn  = 'Compiles with no warnings';
  sCompileWarn    = 'Compiles with warnings';
  sCompileFail    = 'Does not compile';
  sUntested       = 'Not tested';


const
  // Map of option combo text to compiler search options
  cCompileMap: array[TCompilerSearchOption] of string = (
    sCompileOK, sCompileNoWarn, sCompileWarn, sCompileFail, sUntested
  );


{ TFindCompilerDlg }

procedure TFindCompilerDlg.ArrangeForm;
begin
  // Arrange columns horizontally
  TCtrlArranger.AlignLefts([lblCompilerVer, lbCompilerVers, btnSelectAll], 0);
  TCtrlArranger.AlignRights([lbCompilerVers, btnClearAll]);
  TCtrlArranger.AlignLefts(
    [lblCriteria, cbCriteria, rgLogic, rgScope],
    TCtrlArranger.RightOf(
      [lblCompilerVer, lbCompilerVers, btnSelectAll, btnClearAll], 24
    )
  );
  // Arrange vertically
  TCtrlArranger.AlignTops([lblCompilerVer, lblCriteria], 0);
  TCtrlArranger.AlignTops(
    [lbCompilerVers, cbCriteria],
    TCtrlArranger.BottomOf([lblCompilerVer, lblCriteria], 4)
  );
  TCtrlArranger.AlignTops(
    [btnClearAll, btnSelectAll], TCtrlArranger.BottomOf(lbCompilerVers, 4)
  );
  TCtrlArranger.AlignBottoms([btnClearAll, btnSelectAll, rgScope]);
  rgLogic.Top :=
    (rgScope.Top + TCtrlArranger.BottomOf(cbCriteria) - rgLogic.Height) div 2;
  // Size body panel
  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody) + 4;
  pnlBody.ClientWidth := TCtrlArranger.TotalControlWidth(pnlBody);
  inherited;
end;

procedure TFindCompilerDlg.btnClearAllClick(Sender: TObject);
  {Deselects all compilers when user clicks "Clear All" button.
    @param Sender Not used.
  }
var
  Idx: Integer; // loops thru all items in compiler list box
begin
  // Uncheck all compilers in list box and update state of OK button
  for Idx := 0 to Pred(lbCompilerVers.Items.Count) do
    lbCompilerVers.Checked[Idx] := False;
  UpdateOKBtn;
end;

procedure TFindCompilerDlg.btnOKClick(Sender: TObject);
  {OK button click event handler. Creates and records search criteria.
    @param Sender Not used.
  }

  // ---------------------------------------------------------------------------
  function GetCompilerVersions: TCompilerSearchCompilers;
    {Returns set of compilers selected by user.
      @return Set of selected compilers.
    }
  var
    Idx: Integer; // loops thru all compiler list items
  begin
    Result := [];
    // Loop thru items, storing version associated with checked items in set
    for Idx := 0 to Pred(lbCompilerVers.Items.Count) do
      if lbCompilerVers.Checked[Idx] then
        Include(Result, TCompilerID(lbCompilerVers.Items.Objects[Idx]));
  end;

  function GetLogic: TSearchLogic;
    {Returns search logic selected by user using search logic radio buttons.
      @return Required search logic.
    }
  begin
    if rgLogic.ItemIndex = 0 then
      Result := slAnd   // first radio button
    else
      Result := slOr;   // second radio button
  end;

  function GetOption: TCompilerSearchOption;
    {Returns compiler search option selected by user in combo box.
      @return Selected search option
    }
  begin
    Result := TCompilerSearchOption(
      cbCriteria.Items.Objects[cbCriteria.ItemIndex]
    );
  end;
  // ---------------------------------------------------------------------------

var
  Filter: ICompilerSearchFilter;  // search filter
begin
  // Create search filter from entries made in dialog box
  Filter := TSearchFilterFactory.CreateCompilerSearchFilter(
    GetCompilerVersions, GetLogic, GetOption
  );
  // Persist the search criteria
  fSearchParams.Option := Filter.Option;
  fSearchParams.Logic := Filter.Logic;
  fSearchParams.Compilers := Filter.Compilers;
  // Create search object from filter
  fSearch := TSearchFactory.CreateSearch(Filter);
  // Record search scope
  fRefinePreviousSearch := rgScope.ItemIndex = 0
end;

procedure TFindCompilerDlg.btnSelectAllClick(Sender: TObject);
  {Selects all compilers when user clicks Select All button.
    @param Sender Not used.
  }
var
  Idx: Integer; // loops thru all items in compiler list box
begin
  // Check all compilers in list box and update state of OK button
  for Idx := 0 to Pred(lbCompilerVers.Items.Count) do
    lbCompilerVers.Checked[Idx] := True;
  UpdateOKBtn;
end;

procedure TFindCompilerDlg.cbCriteriaSelect(Sender: TObject);
  {Called when a selection is made in Options combo box. Updates state of OK
  button.
    @param Sender Not used.
  }
begin
  UpdateOKBtn;
end;

function TFindCompilerDlg.CheckedCompilerCount: Integer;
  {Returns number of compilers selected in check list box.
    @return Number of checked list items.
  }
var
  Idx: Integer; // loops thru items in compiler version list box
begin
  Result := 0;
  // Count each checked list item
  for Idx := 0 to Pred(lbCompilerVers.Items.Count) do
    if lbCompilerVers.Checked[Idx] then
      Inc(Result);
end;

class function TFindCompilerDlg.Execute(const AOwner: TComponent;
  out ASearch: ISearch; out RefineExisting: Boolean): Boolean;
  {Displays dialog and returns search object based on entered criteria.
    @param AOwner [in] Component that owns this dialog.
    @param ASearch [out] Set to value of Search property: nil if user
      cancels.
    @param RefineExisting [out] Set to flag indicating if any existing
      search is to be refined (True) or if this search is to apply to whole
      database (False).
    @return True if user OKs and search object created or false if user
      cancels and search object is nil.
  }
var
  Dlg: TFindCompilerDlg;
begin
  Dlg := InternalCreate(AOwner);
  try
    Result := (Dlg.ShowModal = mrOK);
    ASearch := Dlg.fSearch;
    RefineExisting := Dlg.fRefinePreviousSearch;
  finally
    Dlg.Free;
  end;
end;

procedure TFindCompilerDlg.FormCreate(Sender: TObject);
  {Called when form created. Creates owned objects.
    @param Sender Not used.
  }
begin
  inherited;
  fCompilers := TCompilersFactory.CreateAndLoadCompilers;
  fSearchParams := TCompilerSearchParams.Create(fCompilers);
  fRefinePreviousSearch := False;
end;

procedure TFindCompilerDlg.FormDestroy(Sender: TObject);
  {Frees owned object when form destroyed.
    @param Sender Not used.
  }
begin
  inherited;
  FreeAndNil(fSearchParams);
  fCompilers := nil;
end;

procedure TFindCompilerDlg.InitForm;
  {Populates and initialises controls.
  }
var
  Idx: Integer;                   // stores insertion index in Options combo
  Option: TCompilerSearchOption;  // loops thru possible compiler search options
  SelOption: Integer;             // selected search option
  Compiler: ICompiler;            // references each compiler
begin
  inherited;
  // Set up list of compilers and check appropriate ones
  // we store compiler ID in listbox's Objects[] property
  for Compiler in fCompilers do
  begin
    Idx := lbCompilerVers.Items.AddObject(
      Compiler.GetName, TObject(Compiler.GetID)
    );
    lbCompilerVers.Checked[Idx] := Compiler.GetID in fSearchParams.Compilers;
  end;

  // Set up list of options and select current one
  // we store search option in combobox's Objects[] property
  SelOption := -1;
  for Option := Low(TCompilerSearchOption) to High(TCompilerSearchOption) do
  begin
    Idx := cbCriteria.Items.AddObject(cCompileMap[Option], TObject(Option));
    if Option = fSearchParams.Option then
      SelOption := Idx;
  end;
  cbCriteria.ItemIndex := SelOption;

  // Select appropriate search logic radio button
  // radio button index is ordinal value of Logic
  rgLogic.ItemIndex := Ord(fSearchParams.Logic);

  // Set search scope enabled state and selected appropriate default button
  rgScope.Enabled := Query.IsSearchActive;
  if Query.IsSearchActive then
    rgScope.ItemIndex := 0
  else
    rgScope.ItemIndex := 1;

  // Update OK button state
  UpdateOKBtn;
end;

procedure TFindCompilerDlg.lbCompilerVersClickCheck(Sender: TObject);
  {Handles click event on list item check box. Updates state of OK button.
    @param Sender [in] Not used.
  }
begin
  UpdateOKBtn;
end;

procedure TFindCompilerDlg.UpdateOKBtn;
  {Updates state of OK button according to whether valid entries made in dialog.
  }
begin
  // OK button enabled if at least one compiler is checked and if a search
  // option is selected
  btnOK.Enabled := (cbCriteria.ItemIndex >= 0) and (CheckedCompilerCount > 0);
end;


{ TCompilerSearchParams }

constructor TCompilerSearchParams.Create(const Compilers: ICompilers);
  {Class constructor. Sets up object with parameters from persistent storage.
    @param Compilers [in] Reference to object that provides information about
      compilers.
  }
begin
  inherited Create;
  fCompilersObj := Compilers;
  ReadValues;
  fUpdated := False;
end;

destructor TCompilerSearchParams.Destroy;
  {Class destructor. Tears down object and saves current values to persistent
  storage if they have changed.
  }
begin
  if fUpdated then
    WriteValues;
  inherited;
end;

procedure TCompilerSearchParams.ReadValues;
  {Reads search parameters from persistent storage.
  }
var
  Storage: ISettingsSection;  // object used to access persistent storage
  Compiler: ICompiler;        // references each compiler
begin
  Storage := Settings.ReadSection(ssFindCompiler);
  // Build list of compilers in search criteria by checking each supported
  // compiler to see if it's value is '1'
  fCompilers := [];
  for Compiler in fCompilersObj do
    if Storage.GetBoolean(Compiler.GetIDString) then
      Include(fCompilers, Compiler.GetID);
  // Get Option and Logic properties
  fOption := TCompilerSearchOption(
    Storage.GetInteger('Option', Ord(Low(TCompilerSearchOption)))
  );
  fLogic := TSearchLogic(Storage.GetInteger('Logic', Ord(slOr)));
end;

procedure TCompilerSearchParams.SetCompilers(
  const Value: TCompilerSearchCompilers);
  {Write access method for Compilers property. Records value and flags updated.
    @param Value The new property value.
  }
begin
  fCompilers := Value;
  fUpdated := True;
end;

procedure TCompilerSearchParams.SetLogic(const Value: TSearchLogic);
  {Write access method for Logic property. Records value and flags updated.
    @param Value The new property value.
  }
begin
  fLogic := Value;
  fUpdated := True;
end;

procedure TCompilerSearchParams.SetOption(
  const Value: TCompilerSearchOption);
  {Write access method for Option property. Records value and flags updated.
    @param Value The new property value.
  }
begin
  fOption := Value;
  fUpdated := True;
end;

procedure TCompilerSearchParams.WriteValues;
  {Writes search parameters to persistent storage.
  }
var
  Storage: ISettingsSection;  // object used to access persistent storage
  Compiler: ICompiler;        // references each compiler
begin
  // Create blank persistent storage object
  Storage := Settings.EmptySection(ssFindCompiler);
  // Record which compilers included in search
  for Compiler in fCompilersObj do
    Storage.SetBoolean(Compiler.GetIDString, Compiler.GetID in fCompilers);
  // Record Option and Logic parameters
  Storage.SetInteger('Option', Ord(fOption));
  Storage.SetInteger('Logic', Ord(fLogic));
  // Store data
  Storage.Save;
end;

end.

