{
 * FmUserDBEditDlg.pas
 *
 * Implements a dialog box that enables the user to create or edit a user-
 * defined routine.
 *
 * v1.0 of 15 Sep 2008  - Original version.
 * v1.1 of 21 Sep 2008  - Added check to ensure that routine does not refer to
 *                        itself in depends or x-ref lists.
 *                      - Removed duplicate routine names and routine's own name
 *                        from dependency and cross-reference check box lists:
 *                        user database takes precedence over main database when
 *                        there is a clash of names.
 * v1.2 of 11 Oct 2008  - Added button to Compiler Results tab to set all
 *                        compile results to success.
 *                      - Modified label text of Compiler Results tab.
 *                      - Changed Add Unit button to use action.
 *                      - Removed class name from constructor in AddNewRoutine
 *                        and EditRoutine methods.
 *                      - Added message to some Assert statements and made
 *                        others get class name from ClassName method.
 * v1.3 of 16 Dec 2008  - Removed EUserDBEdit exception class and replaced usage
 *                        of it with EDataEntry from UExceptions unit.
 *                      - Made TUserDBEditDlg's protected section strict.
 *                      - Modified to use TRectEx record instead of TRect.
 * v1.4 of 31 Dec 2008  - Changed to support new Extra property of TRoutine on
 *                        "Comments" tab and and removed Credits, Credits URL
 *                        and Comments fields.
 *                      - Renamed "Comments" tab as "Extra Information".
 *                      - Added instructions to "Extra Information" tab using
 *                        HTML frame.
 *                      - Deleted Credits URL help label.
 * v1.5 of 05 Jan 2009  - Added facility to get compiler results from a test
 *                        compilation with all installed compilers.
 *                      - Added new button to set all compiler results to query.
 * v1.6 of 06 Jan 2009  - Changed to use local instance of Compiler object to
 *                        avoid corrupting global object's results that may be
 *                        required in main form.
 *                      - Changed to use revised TTestCompileUI.Execute and
 *                        TCompErrorDlg.Execute method signatures.
 *                      - Now centres wait dialog rather than offsetting it from
 *                        top left of form.
 * v1.7 of 10 Jan 2009  - Changed to use new factory class to create local
 *                        compilers object instead of cloning global singleton.
 *                      - Changed to use new TCompileMgr to manage compilations
 *                        instead of local compilers object.
 *                      - Changed name of TCompilerInfo.Compiler property to
 *                        CompilerID.
 *                      - Added Alt+V view compiler errors action (by adding
 *                        actions). Changed Alt key for test compilation to
 *                        Alt+T from Alt+C since Alt+C already used taken.
 * v1.8 of 14 Jan 2009  - Replaced control char literals with constants.
 * v1.9 of 25 Jan 2009  - Revised to use renamed ICompiler.GetGlyph method.
 * v1.10 of 23 Jun 2009 - Removed reference to Snippet item standard format
 *                        property and associated controls and code.
 *                      - Added support for new snippet item Kind property.
 *                      - Moved management of Dependencies and XRef checklist
 *                        boxes to new TSnippetsChkListMgr manager class and
 *                        varied snippets that appear in Dependencies list
 *                        according to snippet kind.
 *                      - Changed to use TSnippetID etc instead of TRoutineID.
 *                      - Replaced Standard Format help link with one that
 *                        explains snippet kinds.
 *                      - Improved dependency checking code to do a deep
 *                        recursive check.
 *                      - Added View Dependencies button that displays dialog
 *                        box that shows deep dependencies.
 *                      - XRefs are no longer checked: anything is allowed.
 *                      - Default list of units re-ordered so that Graphics unit
 *                        comes after Windows.
 *                      - Made extra information edit control bigger.
 *                      - Made extra description HTML use content font and
 *                        removed unused CSS style definitions.
 *                      - Test compilation prevented for freeform snippets.
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
 * The Original Code is FmUserDBEditDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FmUserDBEditDlg;


interface


uses
  // Delphi
  SysUtils, Classes, ActnList, ImgList, Controls, Buttons, StdCtrls, Forms,
  CheckLst, ComCtrls, ExtCtrls, Windows,
  // Project
  FmGenericOKDlg, FrBrowserBase, FrFixedHTMLDlg, FrHTMLDlg,
  IntfCompilers, UActiveText, UCompileMgr, UCSSBuilder, UFontHelper, USnippets,
  USnippetsChkListMgr;


type

  {
  TUserDBEditDlg:
    Dialog box class that enables the user to create or edit a user-defined
    snippet.
  }
  TUserDBEditDlg = class(TGenericOKDlg)
    alMain: TActionList;
    actAddUnit: TAction;
    actCompile: TAction;
    actSetAllQuery: TAction;
    actSetAllSuccess: TAction;
    btnAddUnit: TButton;
    btnCompile: TButton;
    btnSetAllQuery: TBitBtn;
    btnSetAllSuccess: TBitBtn;
    cbCategories: TComboBox;
    clbDepends: TCheckListBox;
    clbUnits: TCheckListBox;
    clbXRefs: TCheckListBox;
    edDescription: TEdit;
    edExtra: TMemo;
    edName: TEdit;
    edSourceCode: TMemo;
    edUnit: TEdit;
    frmExtraInstructions: TFixedHTMLDlgFrame;
    ilLEDs: TImageList;
    lbCompilers: TListBox;
    lbCompRes: TListBox;
    lblCategories: TLabel;
    lblCompilers: TLabel;
    lblCompileShortcuts: TLabel;
    lblCompRes: TLabel;
    lblDepends: TLabel;
    lblDescription: TLabel;
    lblExtra: TLabel;
    lblName: TLabel;
    lblSourceCode: TLabel;
    lblSnippetKindHelp: TLabel;
    lblUnits: TLabel;
    lblXRefs: TLabel;
    pcMain: TPageControl;
    tsCode: TTabSheet;
    tsComments: TTabSheet;
    tsCompileResults: TTabSheet;
    tsReferences: TTabSheet;
    actViewErrors: TAction;
    pnlViewCompErrs: TPanel;
    lblViewCompErrsKey: TLabel;
    lblViewCompErrs: TLabel;
    cbKind: TComboBox;
    lblKind: TLabel;
    btnDependencies: TButton;
    actDependencies: TAction;
    procedure actAddUnitExecute(Sender: TObject);
    procedure actAddUnitUpdate(Sender: TObject);
    procedure actCompileExecute(Sender: TObject);
    procedure actCompileUpdate(Sender: TObject);
    procedure actDependenciesExecute(Sender: TObject);
    procedure actSetAllQueryExecute(Sender: TObject);
    procedure actSetAllSuccessExecute(Sender: TObject);
    procedure actViewErrorsExecute(Sender: TObject);
    procedure actViewErrorsUpdate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure cbKindChange(Sender: TObject);
    procedure CLBRoutineRefsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbCompilersClick(Sender: TObject);
    procedure lbCompilersDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbCompResClick(Sender: TObject);
    procedure lbCompResDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lblSnippetKindHelpClick(Sender: TObject);
    procedure lblViewCompErrsClick(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
  strict private
    fSnippet: TRoutine;         // Snippet being edited (nil for new snippet)
    fCatNames: TStringList;     // List of names of available categories
    fOrigName: string;          // Original name of snippet ('' for new snippet)
    fEditData: TRoutineEditData;// Record storing a snippet's editable data
    fCompileMgr: TCompileMgr;   // Manages compilations and display of results
    fDependsCLBMgr: TSnippetsChkListMgr;// Manages dependencies check list box
    fXRefsCLBMgr: TSnippetsChkListMgr;  // Manages x-refs check list box
    procedure UpdateTabSheetCSS(Sender: TObject; const CSSBuilder: TCSSBuilder);
      {Updates CSS used for HTML displayed in frames on tab sheets.
        @param Sender [in] Not used.
        @param CSSBuilder [in] Object used to update CSS.
      }
    procedure PopulateControls;
      {Populates controls with dynamic data.
      }
    procedure InitControls;
      {Initialises controls to default values.
      }
    procedure FocusCtrl(const Ctrl: TWinControl);
      {Displays and focusses a control, selecting its parent tab sheet if
      necessary.
        @param Ctrl [in] Control to be focussed.
      }
    procedure HandleException(const E: Exception);
      {Handles trapped exceptions. EDataEntry exceptions are caught, an error
      message is displayed and the control causing the exception is focussed.
      Other exceptions are re-raised.
        @param E [in] Exception to be handled.
        @except Exceptions re-raised if not EDataEntry.
      }
    function BuildExtraActiveText: IActiveText;
      {Creates an active text object from the REML text entered in the extra
      information memo control.
        @return Required active text object.
      }
    procedure SetAllCompilerResults(const CompRes: TCompileResult);
      {Sets all compiler results to same value.
        @param CompRes [in] Required compiler result.
      }
    procedure ValidateData;
      {Checks all user-entered data in all tabs of the form.
        @except EDataEntry raised if data is not valid.
      }
    function UpdateData: TRoutineEditData;
      {Updates snippet's data from user entries. Assumes data has been
      validated.
        @return Record containing snippet's data.
      }
    function SelectedSnippetKind(out Kind: TSnippetKind): Boolean;
      {Gets snippet kind selected in drop-down list.
        @param Kind [out] Set to selected snippet kind. Not defined if False
          returned.
        @return True if a snippeet kind has been selected, False if not.
      }
    procedure UpdateReferences;
      {Updates dependencies and cross-references check lists for snippet being
      edited, depending on kind.
      }
    function CreateTempSnippet: TRoutine;
      {Creates a temporary snippet from data entered in dialog box.
        @return Required snippet instance.
        @except EDataEntry raised if any of entered data is invalid.
      }
    procedure DisplayCompileResults(const Compilers: ICompilers);
      {Displays results of a test compilation. Used as callback method for
      compile manager.
        @param Compilers [in] Object containing compilation results.
      }
  strict protected
    procedure InitForm; override;
      {Performs initialisation of form fields and controls.
      }
  public
    class function AddNewRoutine(AOwner: TComponent): Boolean;
      {Displays dialog box to enable user to enter a new snippet.
        @param AOwner [in] Control that owns the dialog box, over which the
          dialog is aligned. May be nil.
        @return True if user OKs, False if cancels.
      }
    class function EditRoutine(AOwner: TComponent;
      const Routine: TRoutine): Boolean;
      {Displays dialog box to enable user to edit a snippet.
        @param AOwner [in] Control that owns the dialog box, over which the
          dialog is aligned. May be nil.
        @param Routine [in] Reference to snippet to be edited.
        @return True if user OKs, False if cancels.
      }
  end;


implementation


uses
  // Delphi
  StrUtils, Graphics, Menus,
  // Project
  FmDependenciesDlg, IntfCommon, UColours, UConsts, UCSSUtils, UExceptions,
  URoutineExtraHelper, USnippetKindInfo, USnippetValidator, UIStringList,
  UMessageBox, USnippetIDs, UStructs, UThemesEx, UUtils;


type

  {
  TCompilerInfo:
    Helper class that records information about a snippet's compilation results.
  }
  TCompilerInfo = class(TObject)
  strict private
    fCompilerID: TCompilerID;       // Value of Compiler property
    fCompileResult: TCompileResult; // Value of CompileResult property
  public
    constructor Create(const CompilerID: TCompilerID;
      const CompileResult: TCompileResult);
      {Class constructor. Sets up and initialises object.
        @param CompilerID [in] Id of compiler that result applies to.
        @param CompileResult [in] Compiler result for compiler.
      }
    property CompilerID: TCompilerID read fCompilerID;
      {Id of compiler to which CompileResult applies}
    property CompileResult: TCompileResult
      read fCompileResult write fCompileResult;
      {Result of compilation with associated compiler}
  end;


{$R *.dfm}


{ TUserDBEditDlg }

procedure TUserDBEditDlg.actAddUnitExecute(Sender: TObject);
  {Adds a unit to the list of required units and selects it.
    @param Sender [in] Not used.
  }
var
  UnitName: string;     // name of new unit from edit control
  InsertIdx: Integer;   // index at which unit name is inserted in list box
resourcestring
  // Error messages
  sBadUnitName = 'Unit name is not a valid Pascal identifier';
  sUnitNameExists = 'Unit name already in list';
begin
  UnitName := Trim(edUnit.Text);
  Assert(UnitName <> '',
    ClassName + '.actAddUnitExecute: UnitName is empty string');
  if not IsValidIdent(UnitName) then
    raise ECodeSnip.Create(sBadUnitName);
  if clbUnits.Items.IndexOf(UnitName) >= 0 then
    raise ECodeSnip.Create(sUnitNameExists);
  InsertIdx := clbUnits.Items.Add(Trim(UnitName));
  clbUnits.Checked[InsertIdx] := True;
  edUnit.Text := '';
end;

procedure TUserDBEditDlg.actAddUnitUpdate(Sender: TObject);
  {Updates Add Unit action according to whether any unit name is entered in
  associated edit control.
    @param Sender [in] Action triggering this event.
  }
begin
  (Sender as TAction).Enabled := edUnit.Text <> '';
end;

procedure TUserDBEditDlg.actCompileExecute(Sender: TObject);
  {Test compiles edited snippet and updates compilers from test result.
    @param Sender [in] Not used.
  }
var
  TempSnippet: TRoutine;  // temp snippet object for compilation
begin
  // Hide view compile errors link
  pnlViewCompErrs.Hide;
  // Disable dialog box and all its controls and actions
  Enabled := False;
  try
    try
      TempSnippet := CreateTempSnippet;
    except
      on E: Exception do
      begin
        Enabled := True;
        HandleException(E);
        Exit;
      end;
    end;
    // Test compile snippet
    try
      fCompileMgr.Compile(tsCompileResults, TempSnippet, DisplayCompileResults);
    finally
      FreeAndNil(TempSnippet);
    end;
  finally
    // Re-enable dialog and controls
    Enabled := True;
  end;
end;

procedure TUserDBEditDlg.actCompileUpdate(Sender: TObject);
  {Updates Test Compile action according to whether any compilers are available
  and if a snippet kind is selected that is not freeform.
    @param Sender [in] Action triggering this event.
  }
var
  Kind: TSnippetKind; // selected snippet kind
begin
  (Sender as TAction).Enabled := fCompileMgr.HaveCompilers
    and SelectedSnippetKind(Kind) and (Kind <> skFreeform);
end;

procedure TUserDBEditDlg.actDependenciesExecute(Sender: TObject);
  {Displays dependencies for currently edited snippet, per entries in
  "Dependencies" check list box.
    @param Sender [in] Not used.
  }
var
  DependsList: TRoutineList;
begin
  DependsList := TRoutineList.Create;
  try
    fDependsCLBMgr.GetCheckedSnippets(DependsList);
    TDependenciesDlg.Execute(
      Self, TSnippetID.Create(Trim(edName.Text), True), DependsList
    );
  finally
    FreeAndNil(DependsList);
  end;
end;

procedure TUserDBEditDlg.actSetAllQueryExecute(Sender: TObject);
  {Sets all compiler results to "query".
    @param Sender [in] Not used.
  }
begin
  SetAllCompilerResults(crQuery);
end;

procedure TUserDBEditDlg.actSetAllSuccessExecute(Sender: TObject);
  {Sets all compiler results to "success".
    @param Sender [in] Not used.
  }
begin
  SetAllCompilerResults(crSuccess);
end;

procedure TUserDBEditDlg.actViewErrorsExecute(Sender: TObject);
  {Displays compiler errors.
    @param Sender [in] Not used.
  }
begin
  fCompileMgr.ShowErrors;
end;

procedure TUserDBEditDlg.actViewErrorsUpdate(Sender: TObject);
  {Disables view compiler errors action if panel containing link is not
  displayed.
    @param Sender [in] Reference to action to be updated.
  }
begin
  (Sender as TAction).Enabled := pnlViewCompErrs.Visible;
end;

class function TUserDBEditDlg.AddNewRoutine(AOwner: TComponent): Boolean;
  {Displays dialog box to enable user to enter a new snippet.
    @param AOwner [in] Control that owns the dialog box, over which the dialog
      is aligned. May be nil.
    @return True if user OKs, False if cancels.
  }
resourcestring
  sCaption = 'Add a Snippet';   // dialog box caption
begin
  with Create(AOwner) do
    try
      Caption := sCaption;
      fSnippet := nil;
      Result := ShowModal = mrOK;
    finally
      Free;
    end;
end;

procedure TUserDBEditDlg.btnOKClick(Sender: TObject);
  {OnClick event handler for OK button. Validates entries and updates / adds
  snippet if all is well.
    @param Sender [in] Not used.
  }
var
  RoutineName: string;  // name of snippet being edited / added
begin
  inherited;
  try
    // Validate and record entered data
    ValidateData;

    fEditData.Assign(UpdateData);
    RoutineName := Trim(edName.Text);
    // Add or update snippet
    if Assigned(fSnippet) then
      fSnippet := (Snippets as ISnippetsEdit).UpdateRoutine(
        fSnippet, fEditData, RoutineName
      )
    else
    begin
      fSnippet := (Snippets as ISnippetsEdit).AddRoutine(
        RoutineName, fEditData
      )
    end;
  except
    on E: Exception do
      HandleException(E);
  end;
end;

function TUserDBEditDlg.BuildExtraActiveText: IActiveText;
  {Creates an active text object from the REML text entered in the extra
  information memo control.
    @return Required active text object.
  }
begin
  Result := TRoutineExtraHelper.BuildActiveText(
    Trim(CompressWhiteSpace(ReplaceStr(edExtra.Text, EOL, ' ')))
  );
end;

procedure TUserDBEditDlg.cbKindChange(Sender: TObject);
  {Handles change events on snippet kind drop down list. Updates list of
  references depending on new kind.
    @param Sender [in] Not used.
  }
begin
  UpdateReferences;
end;

procedure TUserDBEditDlg.CLBRoutineRefsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
  {OnDrawItem event handler for check list boxes that display snippet names.
  Draws user defined snippet names in a special colour.
    @param Control [in] Check list box that triggered the event.
    @param Index [in] Index if item being drawn.
    @param Rect [in] Rectangle in check list box's canvas where item is to be
      drawn.
    @param State [in] State of list item.
  }
var
  CLB: TCheckListBox;   // reference to check list box
  Canvas: TCanvas;      // check list box's canvas
begin
  inherited;
  CLB := Control as TCheckListBox;
  Canvas := CLB.Canvas;
  if not (odSelected in State)
    and (CLB.Items.Objects[Index] as TRoutine).UserDefined then
    Canvas.Font.Color := clUserRoutine;
  Canvas.TextRect(
    Rect,
    Rect.Left + 2,
    (Rect.Top + Rect.Bottom - Canvas.TextHeight(CLB.Items[Index])) div 2,
    CLB.Items[Index]
  );
end;

function TUserDBEditDlg.CreateTempSnippet: TRoutine;
  {Creates a temporary snippet from data entered in dialog box.
    @return Required snippet instance.
    @except EDataEntry raised if any of entered data is invalid.
  }
var
  EditData: TRoutineEditData; // stores snippet's properties and references
begin
  ValidateData;
  // Create snippet object from entered data
  EditData.Init;
  EditData.Assign(UpdateData);
  Result := (Snippets as ISnippetsEdit).CreateTempRoutine(
    Trim(edName.Text), EditData
  );
end;

procedure TUserDBEditDlg.DisplayCompileResults(const Compilers: ICompilers);
  {Displays results of a test compilation. Used as callback method for compile
  manager.
    @param Compilers [in] Object containing compilation results.
  }
var
  CompilerIdx: Integer;           // loops through each compiler
  CompilerInfo: TCompilerInfo;    // provides information about a compiler
begin
  // Update compiler results from test compilation
  for CompilerIdx := 0 to Pred(lbCompilers.Count) do
  begin
    CompilerInfo := lbCompilers.Items.Objects[CompilerIdx] as TCompilerInfo;
    CompilerInfo.CompileResult :=
      Compilers[CompilerInfo.CompilerID].GetLastCompileResult;
  end;
  // Redisplay compilers list to reflect change
  lbCompilers.Invalidate;
  // Update visibility of show errors link
  pnlViewCompErrs.Visible := fCompileMgr.HaveErrors;
end;

class function TUserDBEditDlg.EditRoutine(AOwner: TComponent;
  const Routine: TRoutine): Boolean;
  {Displays dialog box to enable user to edit a snippet.
    @param AOwner [in] Control that owns the dialog box, over which the dialog
      is aligned. May be nil.
    @param Routine [in] Reference to snippet to be edited.
    @return True if user OKs, False if cancels.
  }
resourcestring
  sCaption = 'Edit %s';   // dialog box caption
begin
  with Create(AOwner) do
    try
      Caption := Format(sCaption, [Routine.Name]);
      fSnippet := Routine;
      Result := ShowModal = mrOK;
    finally
      Free;
    end;
end;

procedure TUserDBEditDlg.FocusCtrl(const Ctrl: TWinControl);
  {Displays and focusses a control, selecting its parent tab sheet if necessary.
    @param Ctrl [in] Control to be focussed.
  }
var
  ParentTab: TTabSheet;       // tab sheet on which Ctrl sits
  ParentCtrl: TWinControl;    // walks up parents of Ctrl
begin
  // Find tab sheet that contains edit control
  ParentCtrl := Ctrl.Parent;
  while Assigned(ParentCtrl) and not (ParentCtrl is TTabSheet) do
    ParentCtrl := ParentCtrl.Parent;
  if not Assigned(ParentCtrl) then
    Exit;
  // Display correct tab sheet
  ParentTab := ParentCtrl as TTabSheet;
  pcMain.ActivePage := ParentTab;
  // Focus control
  if Ctrl.Enabled then
    Ctrl.SetFocus;
end;

procedure TUserDBEditDlg.FormCreate(Sender: TObject);
  {Form creation event handler. Creates owned objects.
    @param Sender [in] Not used.
  }
begin
  inherited;
  fCatNames := TStringList.Create;
  fCompileMgr := TCompileMgr.Create(Self);  // auto-freed
  fDependsCLBMgr := TSnippetsChkListMgr.Create(clbDepends);
  fXRefsCLBMgr := TSnippetsChkListMgr.Create(clbXRefs);
end;

procedure TUserDBEditDlg.FormDestroy(Sender: TObject);
  {Form destruction event handler. Frees owned objects.
    @param Sender [in] Not used.
  }
var
  Idx: Integer; // loops through items in compiler list box
begin
  inherited;
  FreeAndNil(fXRefsCLBMgr);
  FreeAndNil(fDependsCLBMgr);
  FreeAndNil(fCatNames);
  // Free TCompilerInfo objects associated with Compilers list box
  for Idx := 0 to Pred(lbCompilers.Count) do
    lbCompilers.Items.Objects[Idx].Free;
end;

procedure TUserDBEditDlg.HandleException(const E: Exception);
  {Handles trapped exceptions. EDataEntry exceptions are caught, an error
  message is displayed and the control causing the exception is focussed. Other
  exceptions are re-raised.
    @param E [in] Exception to be handled.
    @except Exceptions re-raised if not EDataEntry.
  }
begin
  if E is EDataEntry then
  begin
    TMessageBox.Error(Self, E.Message);
    FocusCtrl((E as EDataEntry).Ctrl);
    ModalResult := mrNone;
  end
  else
    raise E;
end;

procedure TUserDBEditDlg.InitControls;
  {Initialises controls to default values.
  }

  // ---------------------------------------------------------------------------
  procedure CheckEntry(const Entry: string; const CLB: TCheckListBox);
    {Checks an item in a check list box that has specified text.
      @param Entry [in] Text of item to be checked.
      @param CLB [in] Reference to check list box.
    }
  var
    Idx: Integer;   // index of Entry in CLB
  begin
    Idx := CLB.Items.IndexOf(Entry);
    if Idx >= 0 then
      CLB.Checked[Idx] := True;
  end;

  procedure InitUnitCheckListBox;
    {Checks all units in units check list box that are referenced by current
    snippet. If unit is not in list box it is added.
    }
  var
    AUnit: string;  // name of each referenced unit
  begin
    Assert(Assigned(fSnippet),
      ClassName + '.InitControls.InitUnitCheckListBox: fSnippet is nil');
    for AUnit in fSnippet.Units do
    begin
      if clbUnits.Items.IndexOf(AUnit) = -1 then
        clbUnits.Items.Add(AUnit);
      CheckEntry(AUnit, clbUnits);
    end;
  end;

  procedure SelectKind(const Kind: TSnippetKind);
    {Selects a snippet kind in drop down list.
      @param Kind [in] Snippet kind to be selected.
    }
  var
    Idx: Integer; // loops through drop down list entries
  begin
    cbKind.ItemIndex := -1;
    for Idx := 0 to Pred(cbKind.Items.Count) do
      if TSnippetKind(cbKind.Items.Objects[Idx]) = Kind then
      begin
        cbKind.ItemIndex := Idx;
        Break;
      end;
    UpdateReferences; // update references list to match kind
  end;

  procedure InitKindDropDownList;
    {Initialises snippet kind drop down list by selecting kind corresponding to
    snippet being edited.
    }
  begin
    Assert(Assigned(fSnippet));
    SelectKind(fSnippet.Kind);
  end;
  // ---------------------------------------------------------------------------

begin
  if Assigned(fSnippet) then
  begin
    // We are editing a snippet: initialise controls from snippet's properties
    edSourceCode.Text := fSnippet.SourceCode;
    edDescription.Text := fSnippet.Description;
    edName.Text := fSnippet.Name;
    cbCategories.ItemIndex := fCatNames.IndexOf(fSnippet.Category);
    edExtra.Text := TRoutineExtraHelper.BuildREMLMarkup(fSnippet.Extra);
    InitKindDropDownList;
    // check required items in references check list boxes
    InitUnitCheckListBox;
    fDependsCLBMgr.CheckSnippets(fSnippet.Depends);
    fXRefsCLBMgr.CheckSnippets(fSnippet.XRef);
  end
  else
  begin
    // We are adding a new snippet: clear all controls
    edSourceCode.Clear;
    edDescription.Clear;
    edName.Clear;
    cbCategories.ItemIndex := -1;
    edExtra.Clear;
    SelectKind(skFreeform); // update references check boxes
  end;
  // Select first compiler and update compiler result list
  lbCompilers.ItemIndex := 0;
  lbCompilersClick(lbCompilers);
  // Set colour and actions of link labels
  lblSnippetKindHelp.Font.Color := clHelpLinkText;
  lblViewCompErrs.Font.Color := clLinkText;
  lblViewCompErrs.Caption := actViewErrors.Caption;
  lblViewCompErrsKey.Caption :=
    '(' + ShortcutToText(actViewErrors.ShortCut) + ')';
end;

procedure TUserDBEditDlg.InitForm;
  {Performs initialisation of form fields and controls.
  }
begin
  inherited;
  // Get data associated with snippet, or blank / default data if adding a new
  // snippet
  fEditData := (Snippets as ISnippetsEdit).GetEditableRoutineInfo(fSnippet);
  // Record snippet's original name, if any
  if Assigned(fSnippet) then
    fOrigName := fSnippet.Name
  else
    fOrigName := '';
  // Populate controls with dynamic data
  PopulateControls;
  // Initialise controls to default values
  InitControls;
  // Set up CSS builder for extra instructions frame (HTML loading is delayed
  // until appropriate tab is displayed)
  frmExtraInstructions.OnBuildCSS := UpdateTabSheetCSS;
  // Select first tab sheet
  pcMain.ActivePageIndex := 0;
end;

procedure TUserDBEditDlg.lbCompilersClick(Sender: TObject);
  {OnClick event handler for Compilers list box. Selects item in Compile Result
  list box that corresponds to result for the selected compiler.
    @param Sender [in] Not used.
  }
var
  CompInfo: TCompilerInfo;  // info about selected compiler
begin
  inherited;
  CompInfo := lbCompilers.Items.Objects[lbCompilers.ItemIndex] as TCompilerInfo;
  lbCompRes.ItemIndex := Ord(CompInfo.CompileResult);
end;

procedure TUserDBEditDlg.lbCompilersDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
  {OnDrawItem event handler for Compilers list box. Custom draws compiler list
  item to show compiler glyph, name of compilers and current compile result as
  glyph.
    @param Control [in] Reference to list box that triggered event.
    @param Index [in] Index of list item being drawn.
    @param Rect [in] Rectangle in list box canvas where item being drawn.
    @param State [in] State of list item (not used).
  }
var
  LB: TListBox;             // reference to list box
  Cvs: TCanvas;             // list box's canvas
  CompInfo: TCompilerInfo;  // info about compile result associated with item
  Text: string;             // text to be displayed
  TextRect: TRect;          // rectangle in which to display text
  CompGlyph: TBitmap;       // gylph associated with compiler
  GlyphRect: TRect;         // rectangle in which to display compiler glyph
begin
  // Get reference to list box, its canvas and associated compiler info
  LB := Control as TListBox;
  Cvs := LB.Canvas;
  CompInfo := LB.Items.Objects[Index] as TCompilerInfo;
  // Clear item's rectangle
  Cvs.FillRect(Rect);
  // Display text
  Text := LB.Items[Index];
  TextRect := TRectEx.Create(
    Rect.Left + 24,
    (Rect.Bottom + Rect.Top - Cvs.TextHeight(Text)) div 2,
    Rect.Right - 24,
    Rect.Bottom
  );
  if odDisabled in State then
    Cvs.Font.Color := clGrayText
  else
    Cvs.Font.Color := LB.Font.Color;
  Cvs.TextRect(TextRect, Text, [tfLeft, tfNoPrefix, tfEndEllipsis, tfTop]);
  // Display any compiler glyph
  CompGlyph := fCompileMgr.Compilers[CompInfo.CompilerID].GetGlyph;
  if Assigned(CompGlyph) then
  begin
    GlyphRect := TRectEx.CreateBounds(
      Rect.Left + 2,
      (Rect.Bottom + Rect.Top - CompGlyph.Height) div 2,
      CompGlyph.Width,
      CompGlyph.Height
    );
    Cvs.BrushCopy(
      GlyphRect,
      CompGlyph,
      TRectEx.Create(0, 0, CompGlyph.Width, CompGlyph.Height),
      clFuchsia
    );
  end;
  // Display compile result "LED": assumes image index = Ord(CompileResult)
  ilLEDs.Draw(
    Cvs,
    Rect.Right - 2 - ilLEDs.Width,
    (Rect.Bottom + Rect.Top - ilLEDs.Height) div 2,
    Ord(CompInfo.CompileResult)
  );
end;

procedure TUserDBEditDlg.lbCompResClick(Sender: TObject);
  {OnClick event handler for Compiler Result list box. Updates Compilers list
  to reflect chosen result.
    @param Sender [in] Not used.
  }
var
  CompilerIdx: Integer;         // index of selected compiler in list
  CompResIdx: Integer;          // index of selected compile result in list
  CompilerInfo: TCompilerInfo;  // information about compiler / result
begin
  // Get selected compiler and result
  CompResIdx := lbCompRes.ItemIndex;
  CompilerIdx := lbCompilers.ItemIndex;
  if (CompilerIdx = -1) or (CompResIdx = -1) then
    Exit;
  // Update compiler with new result
  CompilerInfo := lbCompilers.Items.Objects[CompilerIdx] as TCompilerInfo;
  CompilerInfo.CompileResult :=
    TCompileResult(lbCompRes.Items.Objects[CompResIdx]);
  // Redisplay compilers list to reflect change
  lbCompilers.Invalidate;
end;

procedure TUserDBEditDlg.lbCompResDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
  {OnDrawItem event handler for Compiler Result list box. Custom draws compiler
  result list item to description of result and its "LED".
    @param Control [in] Reference to list box that triggered event.
    @param Index [in] Index of list item being drawn.
    @param Rect [in] Rectangle in list box canvas where item being drawn.
    @param State [in] State of list item (not used).
  }
var
  LB: TListBox;             // reference to list box
  Cvs: TCanvas;             // list box's canvas
  CompRes: TCompileResult;  // compile result for this item
  Text: string;             // text to be displayed
  TextRect: TRect;          // rectangle in which to draw text
begin
  // Get reference to list box, its canvas and associated compile result
  LB := Control as TListBox;
  Cvs := LB.Canvas;
  CompRes := TCompileResult(LB.Items.Objects[Index]);
  // Clear item's rectangle
  Cvs.FillRect(Rect);
  // Draw text
  Text := LB.Items[Index];
  TextRect := TRectEx.Create(
    Rect.Left + 4 + ilLEDs.Width,
    (Rect.Bottom + Rect.Top - Cvs.TextHeight(Text)) div 2,
    Rect.Right,
    Rect.Bottom
  );
  if odDisabled in State then
    Cvs.Font.Color := clGrayText
  else
    Cvs.Font.Color := LB.Font.Color;
  Cvs.TextRect(TextRect, Text, [tfLeft, tfNoPrefix, tfEndEllipsis, tfTop]);
  // Draw compiler result: assumes image index = Ord(CompRes)
  ilLEDs.Draw(
    Cvs,
    Rect.Left + 2,
    (Rect.Bottom + Rect.Top - ilLEDs.Height) div 2,
    Ord(CompRes)
  );
end;

procedure TUserDBEditDlg.lblSnippetKindHelpClick(Sender: TObject);
  {OnClick event handler for Snippet Kind help link label. Displays help topic
  that informs what a Snippet Kind is.
    @param Sender [in] Not used.
  }
begin
  DisplayHelp('SnippetKinds');
end;

procedure TUserDBEditDlg.lblViewCompErrsClick(Sender: TObject);
  {OnClick event handler for compiler errors link label. Displays compiler
  warnings and errors in a dialog box.
    @param Sender [in] Not used.
  }
begin
  actViewErrors.Execute;
end;

procedure TUserDBEditDlg.pcMainChange(Sender: TObject);
  {Handler for OnChange event for page control. Used to load content into
  instructions HTML frame for comments tab.
    @param Sender [in] Not used.
  }
begin
  inherited;
  if pcMain.ActivePage = tsComments then
  begin
    // We have to load instructions here, since loading them when comments tab
    // is hidden, causes dialog box to freeze and not be displayed. We may only
    // load the content when tab is visible. The HTML is loaded each time the
    // tab is displayed, but there is no noticable lag as a result.
    frmExtraInstructions.Initialise('dlg-userdb-extra.html');
    frmExtraInstructions.Height := frmExtraInstructions.DocHeight;
  end;
  // We always hide "view errors" link whenever page changes since snippet
  // properties may have changed since page was last accessed
  pnlViewCompErrs.Hide;
end;

procedure TUserDBEditDlg.PopulateControls;
  {Populates controls with dynamic data.
  }
var
  Cat: TCategory;             // loops through all categories
  Compiler: ICompiler;        // loops thru all compilers
  CompRes: TCompileResult;    // loops thru all compile results
  Kind: TSnippetKind;         // loops thru all supported snippet kinds
resourcestring
  // Text for list items in Compiler Result list box
  sSuccess = 'Success';
  sWarning = 'Warning';
  sError = 'Error';
  sQuery = 'Unknown';
const
  // Map of compiler results onto descriptions
  cCompRes: array[TCompileResult] of string = (
    sSuccess, sWarning, sError, sQuery
  );
begin
  // Display all kinds in drop down list
  for Kind := Low(TSnippetKind) to High(TSnippetKind) do
    cbKind.Items.AddObject(
      TSnippetKindInfoList.Instance[Kind].Description,
      TObject(Kind)
    );
  // Display all available categories in drop down list
  for Cat in Snippets.Categories do
  begin
    cbCategories.Items.Add(Cat.Description);
    fCatNames.Add(Cat.Category);
  end;
  // Display all compilers
  for Compiler in fCompileMgr.Compilers do
    lbCompilers.Items.AddObject(
      Compiler.GetName,
      TCompilerInfo.Create(
        Compiler.GetID, fEditData.Props.CompilerResults[Compiler.GetID]
      )
    );
  // Display all compiler results
  for CompRes := Low(TCompileResult) to High(TCompileResult) do
    lbCompRes.Items.AddObject(cCompRes[CompRes], TObject(CompRes));
end;

function TUserDBEditDlg.SelectedSnippetKind(out Kind: TSnippetKind): Boolean;
  {Gets snippet kind selected in drop-down list.
    @param Kind [out] Set to selected snippet kind. Not defined if False
      returned.
    @return True if a snippeet kind has been selected, False if not.
  }
begin
  Result := (cbKind.ItemIndex >= 0);
  if Result then
    Kind := TSnippetKind(cbKind.Items.Objects[cbKind.ItemIndex]);
end;

procedure TUserDBEditDlg.SetAllCompilerResults(const CompRes: TCompileResult);
  {Sets all compiler results to same value.
    @param CompRes [in] Required compiler result.
  }
var
  CompilerIdx: Integer;         // loops thru all compilers in list box
  CompilerInfo: TCompilerInfo;  // information about compiler / result
begin
  // Update compile result of all compilers
  for CompilerIdx := 0 to Pred(lbCompilers.Count) do
  begin
    CompilerInfo := lbCompilers.Items.Objects[CompilerIdx] as TCompilerInfo;
    CompilerInfo.CompileResult := CompRes
  end;
  // Redisplay compilers list to reflect change
  lbCompilers.Invalidate;
end;

function TUserDBEditDlg.UpdateData: TRoutineEditData;
  {Updates snippet's data from user entries. Assumes data has been validated.
    @return Record containing snippet's data.
  }

  // ---------------------------------------------------------------------------
  function GetCompileResults: TCompileResults;
    {Gets list of compiler results from compilers list box.
      @return Array of compiler results.
    }
  var
    Idx: Integer;             // loops through all compilers in list box
    CompInfo: TCompilerInfo;  // information about a compiler
  begin
    for Idx := 0 to Pred(lbCompilers.Count) do
    begin
      CompInfo := lbCompilers.Items.Objects[Idx] as TCompilerInfo;
      Result[CompInfo.CompilerID] := CompInfo.CompileResult;
    end;
  end;

  procedure CheckedListItemsToStrings(const CLB: TCheckListBox;
    const Strings: IStringList);
    {Sets a string list to contain the text of all check items in a check list
    box.
      @param CLB [in] Reference to check list box.
      @param Strings [in] String list to receive checked items.
    }
  var
    Idx: Integer; // loops thru all items in check list box
  begin
    Strings.Clear;
    for Idx := 0 to Pred(CLB.Items.Count) do
      if CLB.Checked[Idx] then
        Strings.Add(CLB.Items[Idx]);
  end;
  // ---------------------------------------------------------------------------

begin
  Result.Init;
  with Result do
  begin
    Props.Cat := fCatNames[cbCategories.ItemIndex];
    SelectedSnippetKind(Props.Kind);
    Props.Desc := Trim(edDescription.Text);
    Props.SourceCode := TrimRight(edSourceCode.Text);
    (Props.Extra as IAssignable).Assign(BuildExtraActiveText);
    Props.CompilerResults := GetCompileResults;
    CheckedListItemsToStrings(clbUnits, Refs.Units);
    fDependsCLBMgr.GetCheckedSnippets(Refs.Depends);
    fXRefsCLBMgr.GetCheckedSnippets(Refs.XRef);
  end;
end;

procedure TUserDBEditDlg.UpdateReferences;
  {Updates dependencies and cross-references check lists for snippet being
  edited, depending on kind.
  }
var
  EditSnippetID: TSnippetID;      // id of snippet being edited
  EditSnippetKind: TSnippetKind;  // kind of snippet being edited
  Snippet: TRoutine;              // each snippet in database
begin
  // Save state of dependencies and x-ref check list boxes and clear them
  fDependsCLBMgr.Save;
  fDependsCLBMgr.Clear;
  fXRefsCLBMgr.Save;
  fXRefsCLBMgr.Clear;
  EditSnippetID := TSnippetID.Create(fOrigName, True);
  if SelectedSnippetKind(EditSnippetKind) then
  begin
    for Snippet in Snippets.Routines do
    begin
      // We ignore snippet being edited and main database snippets if there is
      // a user-defined one with same name
      if (Snippet.ID <> EditSnippetID) and
        (
          Snippet.UserDefined or
          not Assigned(Snippets.Routines.Find(Snippet.Name, True))
        ) then
      begin
        case EditSnippetKind of
          skFreeform, skRoutine:
          begin
            // For freeform and snippet's depends list can be anything except
            // freeform
            if Snippet.Kind in [skRoutine, skConstant, skTypeDef] then
              fDependsCLBMgr.AddSnippet(Snippet);
          end;
          skTypeDef, skConstant:
          begin
            // For typedefs and constants depends list can only be other
            // typedefs and consts
            if Snippet.Kind in [skConstant, skTypeDef] then
              fDependsCLBMgr.AddSnippet(Snippet);
          end;
        end;
        // Anything can be in XRefs list
        fXRefsCLBMgr.AddSnippet(Snippet);
      end;
    end;
  end;
  // Restore checks to any saved checked item that still exist in new list
  fDependsCLBMgr.Restore;
  fXRefsCLBMgr.Restore;
end;

procedure TUserDBEditDlg.UpdateTabSheetCSS(Sender: TObject;
  const CSSBuilder: TCSSBuilder);
  {Updates CSS used for HTML displayed in frames on tab sheets.
    @param Sender [in] Not used.
    @param CSSBuilder [in] Object used to update CSS.
  }
var
  ContentFont: TFont; // font used for dialog box content (not controls)
begin
  // Build content font and apply to HTML frame
  ContentFont := TFont.Create;
  try
    TFontHelper.SetContentFont(ContentFont, True);
    with CSSBuilder.Selectors['body'] do
    begin
      AddProperty(CSSFontProps(ContentFont));
      if ThemeServicesEx.ThemesEnabled then
        // For themed windows only, modify background colour to suit tab sheet
        // background
        AddProperty(CSSBackgroundColorProp(ThemeServicesEx.GetTabBodyColour));
    end;
    // Add definitions of custom classes used in extra info example frame
    // font style of REML tags
    with CSSBuilder.AddSelector('.elem') do
    begin
      AddProperty(CSSColorProp(clREMLTags));
      AddProperty(CSSFontFamilyProp('Courier New', cfgMonoSpace));
    end;
  finally
    FreeAndNil(ContentFont);
  end;
end;

procedure TUserDBEditDlg.ValidateData;
  {Checks all user-entered data in all tabs of the form.
    @except EDataEntry raised if data is not valid.
  }

  // ---------------------------------------------------------------------------
  procedure Error(const Msg: string; const Ctrl: TWinControl); overload;
    {Raises EDataEntry exception with a specified message and control where
    error occured.
      @param Msg [in] Exception message.
      @param Ctrl [in] Control to which exception relates.
      @except EDataEntry always raised.
    }
  begin
    raise EDataEntry.Create(Msg, Ctrl);
  end;

  procedure Error(const FmtStr: string; const Args: array of const;
    const Ctrl: TWinControl); overload;
    {Raises EDataEntry exception with a message built from a format string and
    parameters, until with a reference to the control where the error occured.
      @param FmtStr [in] Message's format string.
      @param Args [in] Array of data displayed in format string.
      @param Ctrl [in] Control to which exception relates.
      @except EDataEntry always raised.
    }
  begin
    raise EDataEntry.CreateFmt(FmtStr, Args, Ctrl);
  end;

  procedure CheckExtra;
    {Checks the REML text entered in the extra information memo control.
      @except EDataEntry on error.
    }
  var
    ActiveText: IActiveText;            // active text created from text
    Elem: IActiveTextElem;              // each element in active text
    ActionElem: IActiveTextActionElem;  // references action element
  resourcestring
    // Error message
    sActiveTextErr = 'Error parsing extra information markup:' + EOL2 + '%s';
    sLinkErr = 'Hyperlink URL "%s" in extra information must be prefixed by '
      + '"http://"';
    sEmptyLink = 'Hyperlink URL "%s" in extra information does not specify '
      + 'a resource';
  const
    cHTTPProtocol = 'http://';  // http protocal
  begin
    try
      // Try to create active text: this parses the text and raises exception
      // if there is an error in the REML markup
      ActiveText := BuildExtraActiveText;
    except
      // Convert active text parser to data exception
      on E: EActiveTextParserError do
        Error(sActiveTextErr, [E.Message], edExtra);
      else
        raise;
    end;
    // Scan all active text looking of hyperlinks: check that URL has a http://
    // protocol and some url text after it
    for Elem in ActiveText do
    begin
      if Supports(Elem, IActiveTextActionElem, ActionElem)
        and (ActionElem.Kind = ekLink) then
      begin
        if not AnsiStartsText(cHTTPProtocol, ActionElem.Param) then
          Error(sLinkErr, [ActionElem.Param], edExtra);
        if (Length(ActionElem.Param) <= Length(cHTTPProtocol)) then
          Error(sEmptyLink, [ActionElem.Param], edExtra);
      end;
    end;
  end;

  procedure CheckDependencies;
    {Checks a snippet's dependencies for validity.
      @except EDataEntry on error.
    }
  var
    EditData: TRoutineEditData; // data describing edited snippet
    TempSnippet: TRoutine;      // temporary snippet created from user entries
    ErrorMessage: string;       // receives any error message
  resourcestring
    // text added to any error messages
    sPrompt = 'See the dependencies by clicking the View Dependencies button '
      + 'on the References tab.';
  begin
    EditData.Init;
    EditData.Assign(UpdateData);
    TempSnippet := (Snippets as ISnippetsEdit).CreateTempRoutine(
      Trim(edName.Text), EditData
    );
    try
      if not TSnippetValidator.HasValidDependsList(
        TempSnippet, ErrorMessage
      ) then
        Error(ErrorMessage + EOL2 + sPrompt, clbDepends);
    finally
      TempSnippet.Free;
    end;
  end;
  // ---------------------------------------------------------------------------

resourcestring
  // Error messages
  sErrNoDesc = 'A description must be provided';
  sErrDescHasClosingBrace = 'Description must not contain a ''}'' character';
  sErrNoName = 'A name must be provided';
  sErrDupName = '%s is already in the database. Please choose another name';
  sErrBadName = '%s is not a valid Pascal identifier';
  sErrNoSource = 'Some source code must be provided';
  sErrNoCategory = 'A category must be selected';
  sErrNoKind = 'A kind must be selected';
var
  SnippetName: string;  // trimmed snippet name
begin
  // Source code must be provided
  if Trim(edSourceCode.Text) = '' then
    Error(sErrNoSource, edSourceCode);
  // Description is required and must not contain closing brace character
  if AnsiContainsText(edDescription.Text, '}') then
    Error(sErrDescHasClosingBrace, edDescription);
  if Trim(edDescription.Text) = '' then
    Error(sErrNoDesc, edDescription);
  // Unique name required and must be valid Pascal identifier
  SnippetName := Trim(edName.Text);
  if SnippetName = '' then
    Error(sErrNoName, edName);
  if (SnippetName <> fOrigName)
    and (Snippets.Routines.Find(SnippetName, True) <> nil) then
    Error(sErrDupName, [SnippetName], edName);
  if not IsValidIdent(SnippetName) then
    Error(sErrBadName, [SnippetName], edName);
  // Check that a category has been selected
  if cbCategories.ItemIndex = -1 then
    Error(sErrNoCategory, cbCategories);
  // Check that a kind has been selected
  if cbKind.ItemIndex = -1 then
    Error(sErrNoKind, cbKind);
  // Check extra info
  CheckExtra;
  // Check dependencies
  CheckDependencies;
end;

{ TCompilerInfo }

constructor TCompilerInfo.Create(const CompilerID: TCompilerID;
  const CompileResult: TCompileResult);
  {Class constructor. Sets up and initialises object.
    @param CompilerID [in] Id of compiler that result applies to.
    @param CompileResult [in] Compiler result for compiler.
  }
begin
  inherited Create;
  fCompilerID := CompilerID;
  fCompileResult := CompileResult;
end;

end.

