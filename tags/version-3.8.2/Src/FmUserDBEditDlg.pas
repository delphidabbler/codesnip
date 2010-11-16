{
 * FmUserDBEditDlg.pas
 *
 * Implements a dialog box that enables the user to create or edit a user-
 * defined routine.
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
 * The Original Code is FmUserDBEditDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributors:
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmUserDBEditDlg;


interface


uses
  // Delphi
  SysUtils, Classes, ActnList, Buttons, StdCtrls, Forms, Controls, CheckLst,
  ComCtrls, ExtCtrls, StdActns, Menus, ImgList,
  // Project
  FmGenericOKDlg, FrBrowserBase, FrFixedHTMLDlg, FrHTMLDlg, IntfCompilers,
  UActiveText, UBaseObjects, UCategoryListAdapter, UChkListStateMgr,
  UCompileMgr, UCompileResultsLBMgr, UCSSBuilder, ULEDImageList,
  UMemoCaretPosDisplayMgr, UMemoHelper, USnipKindListAdapter, USnippets,
  USnippetsChkListMgr, UUnitsChkListMgr;


type

  {
  TUserDBEditDlg:
    Dialog box class that enables the user to create or edit a user-defined
    snippet.
  }
  TUserDBEditDlg = class(TGenericOKDlg, INoPublicConstruct)
    alMain: TActionList;
    actAddUnit: TAction;
    actCompile: TAction;
    actCopy: TEditCopy;
    actCut: TEditCut;
    actDependencies: TAction;
    actPaste: TEditPaste;
    actSelectAll: TEditSelectAll;
    actSetAllQuery: TAction;
    actSetAllSuccess: TAction;
    actUndo: TEditUndo;
    actViewErrors: TAction;
    actViewExtra: TAction;
    btnAddUnit: TButton;
    btnDependencies: TButton;
    btnCompile: TButton;
    btnSetAllQuery: TBitBtn;
    btnSetAllSuccess: TBitBtn;
    btnViewExtra: TButton;
    cbCategories: TComboBox;
    cbKind: TComboBox;
    clbDepends: TCheckListBox;
    clbUnits: TCheckListBox;
    clbXRefs: TCheckListBox;
    edDescription: TEdit;
    edExtra: TMemo;
    edName: TEdit;
    edSourceCode: TMemo;
    edUnit: TEdit;
    frmExtraInstructions: TFixedHTMLDlgFrame;
    ilMain: TImageList;
    lbCompilers: TListBox;
    lblCategories: TLabel;
    lblCompilers: TLabel;
    lblCompileShortcuts: TLabel;
    lblCompResDesc: TLabel;
    lblDepends: TLabel;
    lblDescription: TLabel;
    lblExtra: TLabel;
    lblExtraCaretPos: TLabel;
    lblName: TLabel;
    lblKind: TLabel;
    lblSourceCaretPos: TLabel;
    lblSourceCode: TLabel;
    lblSnippetKindHelp: TLabel;
    lblUnits: TLabel;
    lblViewCompErrs: TLabel;
    lblViewCompErrsKey: TLabel;
    lblXRefs: TLabel;
    miCopy: TMenuItem;
    miCut: TMenuItem;
    miPaste: TMenuItem;
    miSelectAll: TMenuItem;
    miSpacer1: TMenuItem;
    miSpacer2: TMenuItem;
    miUndo: TMenuItem;
    mnuEditCtrls: TPopupMenu;
    pcMain: TPageControl;
    pnlViewCompErrs: TPanel;
    tsCode: TTabSheet;
    tsComments: TTabSheet;
    tsCompileResults: TTabSheet;
    tsReferences: TTabSheet;
    procedure actAddUnitExecute(Sender: TObject);
    procedure actAddUnitUpdate(Sender: TObject);
    procedure actCompileExecute(Sender: TObject);
    procedure actCompileUpdate(Sender: TObject);
    procedure actDependenciesExecute(Sender: TObject);
    procedure actSetAllQueryExecute(Sender: TObject);
    procedure actSetAllSuccessExecute(Sender: TObject);
    procedure actViewErrorsExecute(Sender: TObject);
    procedure actViewErrorsUpdate(Sender: TObject);
    procedure actViewExtraExecute(Sender: TObject);
    procedure actViewExtraUpdate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure cbKindChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lblSnippetKindHelpClick(Sender: TObject);
    procedure lblViewCompErrsClick(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
  strict private
    fSnippet: TRoutine;             // Snippet being edited: nil for new snippet
    fCatList: TCategoryListAdapter; // Accesses sorted list of categories
    fSnipKindList:
      TSnipKindListAdapter;         // Accesses sorted list of snippet kinds
    fOrigName: string;              // Original name of snippet ('' for new)
    fEditData: TRoutineEditData;    // Record storing a snippet's editable data
    fCompileMgr: TCompileMgr;       // Manages compilation and results display
    fCLBMgrs: array[0..2] of
      TChkListStateMgr;             // Manages check list box clicks
    fDependsCLBMgr:
      TSnippetsChkListMgr;          // Manages dependencies check list box
    fXRefsCLBMgr:
      TSnippetsChkListMgr;          // Manages x-refs check list box
    fUnitsCLBMgr: TUnitsChkListMgr; // Manages units check list box
    fCompilersLBMgr:
      TCompileResultsLBMgr;         // Manages compilers list box
    fImages: TLEDImageList;         // Image list containing LEDs
    fSourceMemoHelper: TMemoHelper; // Helper for working with source code memo
    fMemoCaretPosDisplayMgr: TMemoCaretPosDisplayMgr;
                                    // Manages display of memo caret positions
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
    procedure CheckExtra;
      {Checks the REML text entered in the extra information memo control.
        @except EDataEntry raised on error.
      }
  strict protected
    procedure ArrangeForm; override;
      {Arranges controls on form and sizes it.
      }
    procedure ConfigForm; override;
      {Configures form's controls. Sets font and colors of "link" labels. Also
      sets item height of owner draw check list boxes.
      }
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
  StrUtils, Windows {for inlining}, Graphics,
  // Project
  FmDependenciesDlg, FmViewExtraDlg, IntfCommon, UColours, UConsts, UCSSUtils,
  UCtrlArranger, UExceptions, UFontHelper, UReservedCategories,
  URoutineExtraHelper, USnippetValidator, UMessageBox, USnippetIDs, UStructs,
  UThemesEx, UUtils;


{$R *.dfm}


{ TUserDBEditDlg }

procedure TUserDBEditDlg.actAddUnitExecute(Sender: TObject);
  {Adds a unit to the list of required units and selects it.
    @param Sender [in] Not used.
  }
var
  UnitName: string;     // name of new unit from edit control
resourcestring
  // Error messages
  sBadUnitName = 'Unit name is not a valid Pascal identifier';
  sUnitNameExists = 'Unit name already in list';
begin
  UnitName := Trim(edUnit.Text);
  Assert(UnitName <> '',
    ClassName + '.actAddUnitExecute: UnitName is empty string');
  try
    if not fUnitsCLBMgr.IsValidUnitName(UnitName) then
      raise EDataEntry.Create(
        sBadUnitName, edUnit, TSelection.Create(0, Length(UnitName))
      );
    if fUnitsCLBMgr.ContainsUnit(UnitName) then
      raise EDataEntry.Create(
        sUnitNameExists, edUnit, TSelection.Create(0, Length(UnitName))
      );
    fUnitsCLBMgr.IncludeUnit(UnitName, True);
    edUnit.Text := '';
  except
    on E: Exception do
      HandleException(E);
  end;
end;

procedure TUserDBEditDlg.actAddUnitUpdate(Sender: TObject);
  {Updates Add Unit action according to whether any unit name is entered in
  associated edit control.
    @param Sender [in] Action triggering this event.
  }
begin
  (Sender as TAction).Enabled := Trim(edUnit.Text) <> '';
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
begin
  (Sender as TAction).Enabled := fCompileMgr.HaveCompilers
    and (fSnipKindList.SnippetKind(cbKind.ItemIndex) <> skFreeform);
end;

procedure TUserDBEditDlg.actDependenciesExecute(Sender: TObject);
  {Displays dependencies for currently edited snippet, per entries in
  "Dependencies" check list box.
    @param Sender [in] Not used.
  }
var
  DependsList: TRoutineList;  // list of dependencies
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

procedure TUserDBEditDlg.actViewExtraExecute(Sender: TObject);
  {Validates REML entered in the extra information memo control then displays it
  as it will appear in the main form.
    @param Sender [in] Not used.
  }
begin
  try
    CheckExtra;
    TViewExtraDlg.Execute(Self, BuildExtraActiveText);
  except
    on E: Exception do
      HandleException(E);
  end;
end;

procedure TUserDBEditDlg.actViewExtraUpdate(Sender: TObject);
  {Disables view extra information action if no markup is entered in Extra
  Information tab.
    @param Sender [in] Reference to action to be updated.
  }
begin
  (Sender as TAction).Enabled := Trim(edExtra.Text) <> '';
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
  with InternalCreate(AOwner) do
    try
      Caption := sCaption;
      fSnippet := nil;
      Result := ShowModal = mrOK;
    finally
      Free;
    end;
end;

procedure TUserDBEditDlg.ArrangeForm;
  {Arranges controls on form and sizes it.
  }
begin
  // tsCode
  lblSourceCaretPos.Top := lblSourceCode.Top;
  edSourceCode.Top := TCtrlArranger.BottomOf(lblSourceCode, 4);
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(edSourceCode, 8),
    [cbKind, lblKind, lblSnippetKindHelp]
  );
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([cbKind, lblKind, lblSnippetKindHelp], 8),
    [edDescription, lblDescription]
  );
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([edDescription, lblDescription], 8),
    [edName, lblName]
  );
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([edName, lblName], 8),
    [cbCategories, lblCategories]
  );
  // tsReferences
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(clbXRefs, 6), [btnDependencies, edUnit, btnAddUnit]
  );
  // tsComments
  lblExtraCaretPos.Top := lblExtra.Top;
  frmExtraInstructions.Top := TCtrlArranger.BottomOf(edExtra, 4);
  btnViewExtra.Top := TCtrlArranger.BottomOf(frmExtraInstructions);
  // tsCompileResults
  lblViewCompErrsKey.Top := TCtrlArranger.BottomOf(lblViewCompErrs);
  TCtrlArranger.SetLabelHeight(lblCompResDesc);
  lblCompResDesc.Top := TCtrlArranger.BottomOf(lbCompilers, 8);
  // set body panel size to accommodate controls
  pnlBody.ClientHeight := TCtrlArranger.MaxContainerHeight(
    [tsCode, tsComments, tsCompileResults, tsReferences]
  ) + pnlBody.ClientHeight - tsCode.Height + 8;
  inherited;
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

procedure TUserDBEditDlg.CheckExtra;
  {Checks the REML text entered in the extra information memo control.
    @except EDataEntry raised on error.
  }
var
  ActiveText: IActiveText;  // active text created from text
  ErrorMsg: string;         // error message from validator
  DataError: EDataEntry;    // data entry error exception raised on error
resourcestring
  // parse error message
  sActiveTextErr = 'Error parsing extra information markup:' + EOL2 + '%s';
begin
  try
    // Try to create active text: this parses the text and raises exception
    // if there is an error in the REML markup
    ActiveText := BuildExtraActiveText;
  except
    // Convert active text parser to data exception
    on E: EActiveTextParserError do
    begin
      DataError := EDataEntry.CreateFmt(
        sActiveTextErr, [E.Message], edExtra
      );
      if E.HasSelection then
        DataError.Selection := E.Selection;
      raise DataError;
    end
    else
      raise;
  end;
  // Validate the active text
  if not TSnippetValidator.ValidateExtra(ActiveText, ErrorMsg) then
    raise EDataEntry.Create(ErrorMsg, edExtra); // no selection info available
end;

procedure TUserDBEditDlg.ConfigForm;
  {Configures form's controls. Sets font and colors of "link" labels. Also sets
  item height of owner draw check list boxes.
  }
begin
  inherited;
  // Set colour and actions of link labels
  lblSnippetKindHelp.Font.Color := clHelpLinkText;
  TFontHelper.SetDefaultBaseFont(lblSnippetKindHelp.Font, False);
  lblViewCompErrs.Font.Color := clLinkText;
  TFontHelper.SetDefaultBaseFont(lblViewCompErrs.Font, False);
  lblViewCompErrs.Caption := actViewErrors.Caption;
  lblViewCompErrsKey.Caption :=
    '(' + ShortcutToText(actViewErrors.ShortCut) + ')';
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
  Results: TCompileResults;       // results of compilation
  CompID: TCompilerID;            // loops thru each compiler
begin
  for CompID := Low(TCompilerID) to High(TCompilerID) do
    Results[CompID] := Compilers[CompID].GetLastCompileResult;
  fCompilersLBMgr.SetCompileResults(Results);
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
  with InternalCreate(AOwner) do
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
  fCatList := TCategoryListAdapter.Create(Snippets.Categories);
  fSnipKindList := TSnipKindListAdapter.Create;
  fCompileMgr := TCompileMgr.Create(Self);  // auto-freed
  fMemoCaretPosDisplayMgr := TMemoCaretPosDisplayMgr.Create;
  fDependsCLBMgr := TSnippetsChkListMgr.Create(clbDepends);
  fXRefsCLBMgr := TSnippetsChkListMgr.Create(clbXRefs);
  fUnitsCLBMgr := TUnitsChkListMgr.Create(clbUnits);
  fCLBMgrs[0] := TChkListStateMgr.Create(clbXRefs);
  fCLBMgrs[1] := TChkListStateMgr.Create(clbDepends);
  fCLBMgrs[2] := TChkListStateMgr.Create(clbUnits);
  fCompilersLBMgr := TCompileResultsLBMgr.Create(
    lbCompilers, fCompileMgr.Compilers
  );
  fImages := TLEDImageList.Create(Self);
  fSourceMemoHelper := TMemoHelper.Create(edSourceCode);
  alMain.Images := fImages;
  btnSetAllSuccess.Action := actSetAllSuccess;
  btnSetAllQuery.Action := actSetAllQuery;
end;

procedure TUserDBEditDlg.FormDestroy(Sender: TObject);
  {Form destruction event handler. Frees owned objects.
    @param Sender [in] Not used.
  }
var
  Idx: Integer; // loops through items in compiler list box
begin
  inherited;
  FreeAndNil(fSourceMemoHelper);
  FreeAndNil(fCompilersLBMgr);
  for Idx := Low(fCLBMgrs) to High(fCLBMgrs) do
    FreeAndNil(fCLBMgrs[Idx]);
  FreeAndNil(fUnitsCLBMgr);
  FreeAndNil(fXRefsCLBMgr);
  FreeAndNil(fDependsCLBMgr);
  FreeAndNil(fSnipKindList);
  FreeAndNil(fCatList);
  fMemoCaretPosDisplayMgr.Free;
end;

procedure TUserDBEditDlg.HandleException(const E: Exception);
  {Handles trapped exceptions. EDataEntry exceptions are caught, an error
  message is displayed and the control causing the exception is focussed. Other
  exceptions are re-raised.
    @param E [in] Exception to be handled.
    @except Exceptions re-raised if not EDataEntry.
  }
var
  Error: EDataEntry;
begin
  if E is EDataEntry then
  begin
    Error := E as EDataEntry;
    TMessageBox.Error(Self, Error.Message);
    FocusCtrl(Error.Ctrl);
    if Error.HasSelection and (Error.Ctrl is TCustomEdit) then
    begin
      (Error.Ctrl as TCustomEdit).SelStart := Error.Selection.StartPos;
      (Error.Ctrl as TCustomEdit).SelLength := Error.Selection.Length;
    end;
    ModalResult := mrNone;
  end
  else
    raise E;
end;

procedure TUserDBEditDlg.InitControls;
  {Initialises controls to default values.
  }
begin
  if Assigned(fSnippet) then
  begin
    // We are editing a snippet: initialise controls from snippet's properties
    edSourceCode.Text := fSnippet.SourceCode;
    edDescription.Text := fSnippet.Description;
    edName.Text := fSnippet.Name;
    cbCategories.ItemIndex := fCatList.IndexOf(fSnippet.Category);
    edExtra.Text := TRoutineExtraHelper.BuildREMLMarkup(fSnippet.Extra);
    cbKind.ItemIndex := fSnipKindList.IndexOf(fSnippet.Kind);
    // check required items in references check list boxes
    UpdateReferences;
    fDependsCLBMgr.CheckSnippets(fSnippet.Depends);
    fXRefsCLBMgr.CheckSnippets(fSnippet.XRef);
    // ensure snippet's units are displayed checked in units check list box
    fUnitsCLBMgr.IncludeUnits(fSnippet.Units, True);
  end
  else
  begin
    // We are adding a new snippet: clear all controls or set default values
    edSourceCode.Clear;
    edDescription.Clear;
    edName.Clear;
    cbCategories.ItemIndex := fCatList.IndexOf(TReservedCategories.UserCatName);
    if cbCategories.ItemIndex = -1 then
      cbCategories.ItemIndex := 0;
    cbKind.ItemIndex := fSnipKindList.IndexOf(skFreeform);
    edExtra.Clear;
    UpdateReferences;
  end;
  // Display all compiler results
  fCompilersLBMgr.SetCompileResults(fEditData.Props.CompilerResults);
  Assert(cbKind.ItemIndex >= 0,
    ClassName + '.InitControls: no selection in cbKind');
  Assert(cbCategories.ItemIndex >= 0,
    ClassName + '.InitControls: no selection in cbCategories');
  // Auto-update caret position display for source and extra info memos
  fMemoCaretPosDisplayMgr.Manage(edSourceCode, lblSourceCaretPos);
  fMemoCaretPosDisplayMgr.Manage(edExtra, lblExtraCaretPos);
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
    // is hidden causes dialog box to freeze and not be displayed. We may only
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
begin
  // Display all kinds in drop down list
  fSnipKindList.ToStrings(cbKind.Items);
  // Display all available categories in drop down list
  fCatList.ToStrings(cbCategories.Items);
end;

procedure TUserDBEditDlg.SetAllCompilerResults(const CompRes: TCompileResult);
  {Sets all compiler results to same value.
    @param CompRes [in] Required compiler result.
  }
begin
  fCompilersLBMgr.SetCompileResults(CompRes);
end;

function TUserDBEditDlg.UpdateData: TRoutineEditData;
  {Updates snippet's data from user entries. Assumes data has been validated.
    @return Record containing snippet's data.
  }
begin
  Result.Init;
  with Result do
  begin
    Props.Cat := fCatList.CatName(cbCategories.ItemIndex);
    Props.Kind := fSnipKindList.SnippetKind(cbKind.ItemIndex);
    Props.Desc := Trim(edDescription.Text);
    Props.SourceCode := TrimRight(edSourceCode.Text);
    (Props.Extra as IAssignable).Assign(BuildExtraActiveText);
    Props.CompilerResults := fCompilersLBMgr.GetCompileResults;
    fUnitsCLBMgr.GetCheckedUnits(Refs.Units);
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
  Snippet: TRoutine;              // each snippet in database
  EditSnippetKind: TSnippetKind;  // kind of snippet being edited
begin
  // Save state of dependencies and x-ref check list boxes and clear them
  fDependsCLBMgr.Save;
  fDependsCLBMgr.Clear;
  fXRefsCLBMgr.Save;
  fXRefsCLBMgr.Clear;
  EditSnippetID := TSnippetID.Create(fOrigName, True);
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
      // Decide if snippet can be added to depends list: must be correct kind
      EditSnippetKind := fSnipKindList.SnippetKind(cbKind.ItemIndex);
      if Snippet.Kind in
        TSnippetValidator.ValidDependsKinds(EditSnippetKind) then
        fDependsCLBMgr.AddSnippet(Snippet);
      // Anything can be in XRefs list
      fXRefsCLBMgr.AddSnippet(Snippet);
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
  DefaultFont: TFont; // font used for dialog box content (not controls)
begin
  // Build default font and apply to HTML frame
  DefaultFont := TFont.Create;
  try
    TFontHelper.SetDefaultFont(DefaultFont, True);
    with CSSBuilder.Selectors['body'] do
    begin
      AddProperty(CSSFontProps(DefaultFont));
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
      AddProperty(
        CSSFontFamilyProp(TFontHelper.DefaultMonoFontName, cfgMonoSpace)
      );
    end;
  finally
    FreeAndNil(DefaultFont);
  end;
end;

procedure TUserDBEditDlg.ValidateData;
  {Checks all user-entered data in all tabs of the form.
    @except EDataEntry raised if data is not valid.
  }
resourcestring
  // Messages
  sDependencyPrompt = 'See the dependencies by clicking the View Dependencies '
    + 'button on the References tab.';
var
  ErrorMessage: string;       // receives validation error messages
  ErrorSelection: TSelection; // receives selection containing errors
begin
  if not TSnippetValidator.ValidateSourceCode(
    edSourceCode.Text, ErrorMessage, ErrorSelection
  ) then
    raise EDataEntry.Create(ErrorMessage, edSourceCode, ErrorSelection);
  if not TSnippetValidator.ValidateDescription(
    edDescription.Text, ErrorMessage, ErrorSelection
  ) then
    raise EDataEntry.Create(ErrorMessage, edDescription, ErrorSelection);
  if not TSnippetValidator.ValidateName(
    edName.Text,
    not AnsiSameText(Trim(edName.Text), fOrigName),
    ErrorMessage,
    ErrorSelection
  ) then
    raise EDataEntry.Create(ErrorMessage, edName, ErrorSelection);
  CheckExtra;
  if not TSnippetValidator.ValidateDependsList(
    Trim(edName.Text), UpdateData, ErrorMessage
  ) then
    raise EDataEntry.Create(  // selection not applicable to list boxes
      MakeSentence(ErrorMessage) + EOL2 + sDependencyPrompt, clbDepends
    );
end;

end.

