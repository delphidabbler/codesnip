{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that enables the user to create or edit user
 * defined snippets.
}


unit FmSnippetsEditorDlg;


interface


uses
  // Delphi
  SysUtils,
  Classes,
  ActnList,
  Buttons,
  StdCtrls,
  Forms,
  Controls,
  CheckLst,
  ComCtrls,
  ExtCtrls,
  StdActns,
  Menus,
  ImgList,
  // Project
  ActiveText.UMain,
  Compilers.UGlobals,
  DB.USnippet,
  DB.Vaults,
  FmGenericOKDlg,
  FrBrowserBase,
  FrFixedHTMLDlg,
  FrHTMLDlg,
  UBaseObjects,
  UCategoryListAdapter,
  UCollectionListAdapter,
  UCompileMgr,
  UCompileResultsLBMgr,
  UCSSBuilder,
  UMemoCaretPosDisplayMgr,
  UMemoHelper,
  USnipKindListAdapter,
  USnippetsChkListMgr,
  UUnitsChkListMgr,
  FmSnippetsEditorDlg.FrActiveTextEditor;


type

  {
  TSnippetsEditorDlg:
    Dialog box class that enables the user to create or edit a user defined
    snippet.
  }
  TSnippetsEditorDlg = class(TGenericOKDlg, INoPublicConstruct)
    alMain: TActionList;
    actAddUnit: TAction;
    actCompile: TAction;
    actCopy: TEditCopy;
    actCut: TEditCut;
    actViewDependencies: TAction;
    actPaste: TEditPaste;
    actSelectAll: TEditSelectAll;
    actSetAllQuery: TAction;
    actSetAllSuccess: TAction;
    actUndo: TEditUndo;
    actViewErrors: TAction;
    actViewExtra: TAction;
    actViewTestUnit: TAction;
    btnAddUnit: TButton;
    btnCompile: TButton;
    btnSetAllQuery: TButton;
    btnSetAllSuccess: TButton;
    btnViewExtra: TButton;
    btnViewTestUnit: TButton;
    cbCategories: TComboBox;
    cbKind: TComboBox;
    clbDepends: TCheckListBox;
    clbUnits: TCheckListBox;
    clbXRefs: TCheckListBox;
    edSourceCode: TMemo;
    edUnit: TEdit;
    lbCompilers: TListBox;
    lblCategories: TLabel;
    lblCompilers: TLabel;
    lblCompileShortcuts: TLabel;
    lblCompResDesc: TLabel;
    lblDepends: TLabel;
    lblDescription: TLabel;
    lblExtra: TLabel;
    lblExtraCaretPos: TLabel;
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
    frmDescription: TSnippetsActiveTextEdFrame;
    btnViewDescription: TButton;
    actViewDescription: TAction;
    frmExtra: TSnippetsActiveTextEdFrame;
    lblDisplayName: TLabel;
    edDisplayName: TEdit;
    chkUseHiliter: TCheckBox;
    actClearDependencies: TAction;
    actClearXRefs: TAction;
    mnuDependencies: TPopupMenu;
    mnuXRefs: TPopupMenu;
    miClearXRefs: TMenuItem;
    miClearDependencies: TMenuItem;
    miViewDependencies: TMenuItem;
    mnuUnits: TPopupMenu;
    actDeleteUnit: TAction;
    miDeleteUnit: TMenuItem;
    actRestoreUnits: TAction;
    miRestoreUnits: TMenuItem;
    actClearUnits: TAction;
    miClearUnits: TMenuItem;
    miSpacer3: TMenuItem;
    lblCollection: TLabel;
    cbCollection: TComboBox;
    lblCollectionInfo: TLabel;
    procedure actAddUnitExecute(Sender: TObject);
    procedure actAddUnitUpdate(Sender: TObject);
    procedure actCompileExecute(Sender: TObject);
    procedure actCompileUpdate(Sender: TObject);
    procedure actViewDependenciesExecute(Sender: TObject);
    procedure actSetAllQueryExecute(Sender: TObject);
    procedure actSetAllSuccessExecute(Sender: TObject);
    procedure actViewErrorsExecute(Sender: TObject);
    procedure actViewErrorsUpdate(Sender: TObject);
    procedure actViewExtraExecute(Sender: TObject);
    procedure actViewExtraUpdate(Sender: TObject);
    procedure actViewTestUnitExecute(Sender: TObject);
    procedure actViewTestUnitUpdate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure cbKindChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lblSnippetKindHelpClick(Sender: TObject);
    procedure lblViewCompErrsClick(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    ///  <summary>Handles event triggered when user clicks on one of page
    ///  control tabs. Ensures page control has focus.</summary>
    ///  <remarks>Without this fix, page control does not always get focus when
    ///  a tab is clicked.</remarks>
    procedure pcMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure actViewDescriptionExecute(Sender: TObject);
    procedure actViewDescriptionUpdate(Sender: TObject);
    procedure actClearDependenciesExecute(Sender: TObject);
    procedure actClearXRefsExecute(Sender: TObject);
    procedure actClearDependenciesUpdate(Sender: TObject);
    procedure actClearXRefsUpdate(Sender: TObject);
    procedure actDeleteUnitUpdate(Sender: TObject);
    procedure actDeleteUnitExecute(Sender: TObject);
    procedure actRestoreUnitsExecute(Sender: TObject);
    procedure actClearUnitsExecute(Sender: TObject);
    procedure actClearUnitsUpdate(Sender: TObject);
  strict private
    fSnippet: TSnippet;             // Snippet being edited: nil for new snippet
    fCatList: TCategoryListAdapter; // Accesses sorted list of categories
    fCollList:
      TCollectionListAdapter;       // Accesses sorted list of collections
    fSnipKindList:
      TSnipKindListAdapter;         // Accesses sorted list of snippet kinds
    fEditData: TSnippetEditData;    // Record storing a snippet's editable data
    fCompileMgr: TCompileMgr;       // Manages compilation and results display
    fDependsCLBMgr:
      TSnippetsChkListMgr;          // Manages dependencies check list box
    fXRefsCLBMgr:
      TSnippetsChkListMgr;          // Manages x-refs check list box
    fUnitsCLBMgr: TUnitsChkListMgr; // Manages units check list box
    fCompilersLBMgr:
      TCompileResultsLBMgr;         // Manages compilers list box
    fSourceMemoHelper: TMemoHelper; // Helper for working with source code memo
    fMemoCaretPosDisplayMgr: TMemoCaretPosDisplayMgr;
                                    // Manages display of memo caret positions

    ///  <summary>Returns the ID of the vault that applies to a new or existing
    ///  snippet.</summary>
    ///  <returns><c>TVaultID</c>. The required vault ID.</returns>
    ///  <remarks>For a new snippet the return value is the vault to be applied
    ///  to the snippet. For an existing snippet this is the value to which the
    ///  snippet already belongs.</remarks>
    function SelectedCollectionID: TVaultID;

    ///  <summary>Returns a snippet key that is unique with the current
    ///  snippet collection.</summary>
    ///  <returns><c>string</c>. The required key.</returns>
    ///  <remarks>For a new snippet this key will change depending each time the
    ///  method is called, and will always be unique within the selected
    ///  collection. For an existing snippet the key is always that of the
    ///  snippet and never changes across method calls.</remarks>
    function UniqueSnippetKey: string;

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
    procedure SetAllCompilerResults(const CompRes: TCompileResult);
      {Sets all compiler results to same value.
        @param CompRes [in] Required compiler result.
      }
    procedure ValidateData;
      {Checks all user-entered data in all tabs of the form.
        @except EDataEntry raised if data is not valid.
      }
    function UpdateData: TSnippetEditData;
      {Updates snippet's data from user entries. Assumes data has been
      validated.
        @return Record containing snippet's data.
      }
    procedure UpdateReferences;
      {Updates dependencies and cross-references check lists for snippet being
      edited, depending on kind.
      }
    function CreateTempSnippet: TSnippet;
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
    class function AddNewSnippet(AOwner: TComponent): Boolean;
      {Displays dialog box to enable user to enter a new snippet.
        @param AOwner [in] Control that owns the dialog box, over which the
          dialog is aligned. May be nil.
        @return True if user OKs, False if cancels.
      }
    class function EditSnippet(AOwner: TComponent;
      const Snippet: TSnippet): Boolean;
      {Displays dialog box to enable user to edit a snippet.
        @param AOwner [in] Control that owns the dialog box, over which the
          dialog is aligned. May be nil.
        @param Snippet [in] Reference to snippet to be edited.
        @return True if user OKs, False if cancels.
      }
  end;


implementation


uses
  // Delphi
  Windows {for inlining}, Graphics,
  // Project
  DB.UCategory,
  DB.UMain, DB.USnippetKind, FmDependenciesDlg, IntfCommon, UColours, UConsts,
  UCSSUtils, UCtrlArranger, UExceptions, UFontHelper, UIStringList,
  USnippetExtraHelper, USnippetValidator, UMessageBox,
  USnippetIDs, UStructs, UStrUtils, UTestUnitDlgMgr, UThemesEx, UUtils;


{$R *.dfm}


{ TSnippetsEditorDlg }

procedure TSnippetsEditorDlg.actAddUnitExecute(Sender: TObject);
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
  UnitName := StrTrim(edUnit.Text);
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

procedure TSnippetsEditorDlg.actAddUnitUpdate(Sender: TObject);
  {Updates Add Unit action according to whether any unit name is entered in
  associated edit control.
    @param Sender [in] Action triggering this event.
  }
begin
  (Sender as TAction).Enabled := (StrTrim(edUnit.Text) <> '')
    and clbUnits.Enabled;
end;

procedure TSnippetsEditorDlg.actClearDependenciesExecute(Sender: TObject);
begin
  fDependsCLBMgr.ClearChecks;
end;

procedure TSnippetsEditorDlg.actClearDependenciesUpdate(Sender: TObject);
begin
  actClearDependencies.Enabled := fDependsCLBMgr.HasCheckedItems;
end;

procedure TSnippetsEditorDlg.actClearUnitsExecute(Sender: TObject);
begin
  fUnitsCLBMgr.ClearChecks;
end;

procedure TSnippetsEditorDlg.actClearUnitsUpdate(Sender: TObject);
begin
  actClearUnits.Enabled := fUnitsCLBMgr.HasCheckedItems;
end;

procedure TSnippetsEditorDlg.actClearXRefsExecute(Sender: TObject);
begin
  fXRefsCLBMgr.ClearChecks;
end;

procedure TSnippetsEditorDlg.actClearXRefsUpdate(Sender: TObject);
begin
  actClearXRefs.Enabled := fXRefsCLBMgr.HasCheckedItems;
end;

procedure TSnippetsEditorDlg.actCompileExecute(Sender: TObject);
  {Test compiles edited snippet and updates compilers from test result.
    @param Sender [in] Not used.
  }
var
  TempSnippet: TSnippet;  // temp snippet object for compilation
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

procedure TSnippetsEditorDlg.actCompileUpdate(Sender: TObject);
  {Updates Test Compile action according to whether any compilers are available
  and if a snippet kind is selected that is not freeform.
    @param Sender [in] Action triggering this event.
  }
begin
  (Sender as TAction).Enabled := fCompileMgr.HaveCompilers
    and (fSnipKindList.SnippetKind(cbKind.ItemIndex) <> skFreeform);
end;

procedure TSnippetsEditorDlg.actDeleteUnitExecute(Sender: TObject);
begin
  fUnitsCLBMgr.DeleteSelectedItem;
end;

procedure TSnippetsEditorDlg.actDeleteUnitUpdate(Sender: TObject);
begin
  actDeleteUnit.Enabled := fUnitsCLBMgr.CanDeleteSelectedItem;
end;

procedure TSnippetsEditorDlg.actRestoreUnitsExecute(Sender: TObject);
begin
  fUnitsCLBMgr.RestoreDefaults;
end;

procedure TSnippetsEditorDlg.actSetAllQueryExecute(Sender: TObject);
  {Sets all compiler results to "query".
    @param Sender [in] Not used.
  }
begin
  SetAllCompilerResults(crQuery);
end;

procedure TSnippetsEditorDlg.actSetAllSuccessExecute(Sender: TObject);
  {Sets all compiler results to "success".
    @param Sender [in] Not used.
  }
begin
  SetAllCompilerResults(crSuccess);
end;

procedure TSnippetsEditorDlg.actViewDependenciesExecute(Sender: TObject);
  {Displays dependencies for currently edited snippet, per entries in
  "Dependencies" check list box.
    @param Sender [in] Not used.
  }
var
  DependsList: TSnippetList;  // list of dependencies
begin
  DependsList := TSnippetList.Create;
  try
    fDependsCLBMgr.GetCheckedSnippets(DependsList);
    TDependenciesDlg.Execute(
      Self,
      TSnippetID.Create(UniqueSnippetKey, SelectedCollectionID),
      StrTrim(edDisplayName.Text),
      DependsList,
      [tiDependsUpon],
      'SnippetsEditorDependenciesDlg'
    );
  finally
    FreeAndNil(DependsList);
  end;
end;

procedure TSnippetsEditorDlg.actViewDescriptionExecute(Sender: TObject);
begin
  try
    frmDescription.Preview;
  except
    on E: Exception do
      HandleException(E);
  end;
end;

procedure TSnippetsEditorDlg.actViewDescriptionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := frmDescription.CanPreview;
end;

procedure TSnippetsEditorDlg.actViewErrorsExecute(Sender: TObject);
  {Displays compiler errors.
    @param Sender [in] Not used.
  }
begin
  fCompileMgr.ShowErrors;
end;

procedure TSnippetsEditorDlg.actViewErrorsUpdate(Sender: TObject);
  {Disables view compiler errors action if panel containing link is not
  displayed.
    @param Sender [in] Reference to action to be updated.
  }
begin
  (Sender as TAction).Enabled := pnlViewCompErrs.Visible;
end;

procedure TSnippetsEditorDlg.actViewExtraExecute(Sender: TObject);
  {Validates REML entered in the extra information memo control then displays it
  as it will appear in the main form.
    @param Sender [in] Not used.
  }
begin
  try
    frmExtra.Preview;
  except
    on E: Exception do
      HandleException(E);
  end;
end;

procedure TSnippetsEditorDlg.actViewExtraUpdate(Sender: TObject);
  {Disables view extra information action if no markup is entered in Extra
  Information tab.
    @param Sender [in] Reference to action to be updated.
  }
begin
  (Sender as TAction).Enabled := frmExtra.CanPreview;
end;

procedure TSnippetsEditorDlg.actViewTestUnitExecute(Sender: TObject);
  {Displays test unit for current snippet. Error message displayed if snippet
  is not valid.
    @param Sender [in] Not used.
  }
var
  TempSnippet: TSnippet;  // temp snippet object for compilation
begin
  try
    TempSnippet := CreateTempSnippet;
  except
    on E: Exception do
    begin
      // TempSnippet not created if there is an exception here
      HandleException(E);
      Exit;
    end;
  end;
  try
    TTestUnitDlgMgr.DisplayTestUnit(Self, TempSnippet);
  finally
    TempSnippet.Free;
  end;
end;

procedure TSnippetsEditorDlg.actViewTestUnitUpdate(Sender: TObject);
  {Updates View Test Unit action according to whether a compilable snippet is
  selected.
    @param Sender [in] Action triggering this event.
  }
begin
  (Sender as TAction).Enabled :=
    fSnipKindList.SnippetKind(cbKind.ItemIndex) <> skFreeform;
end;

class function TSnippetsEditorDlg.AddNewSnippet(AOwner: TComponent): Boolean;
  {Displays dialog box to enable user to enter a new snippet.
    @param AOwner [in] Control that owns the dialog box, over which the dialog
      is aligned. May be nil.
    @return True if user OKs, False if cancels.
  }
var
  Dlg: TSnippetsEditorDlg;
resourcestring
  sCaption = 'Add a Snippet';   // dialog box caption
begin
  Dlg := InternalCreate(AOwner);
  try
    Dlg.Caption := sCaption;
    Dlg.fSnippet := nil;
    Result := Dlg.ShowModal = mrOK;
  finally
    Dlg.Free;
  end;
end;

procedure TSnippetsEditorDlg.ArrangeForm;
  {Arranges controls on form and sizes it.
  }
begin
  // tsCode
  edSourceCode.Width := tsCode.ClientWidth - 8;
  // Column 1
  TCtrlArranger.AlignLefts(
    [
      lblCollection, lblDisplayName, lblDescription, lblKind, lblCategories,
      lblSourceCode, edSourceCode
    ],
    3
  );
  // Column 2
  TCtrlArranger.AlignLefts(
    [
      cbCollection, lblCollectionInfo, edDisplayName, frmDescription, cbKind,
      cbCategories
    ],
    TCtrlArranger.RightOf(
      [lblCollection, lblDisplayName, lblDescription, lblKind, lblCategories],
      12
    )
  );
  // Right hand sides
  TCtrlArranger.AlignRights(
    [edSourceCode, lblSourceCaretPos, btnViewDescription]
  );
  frmDescription.Width := btnViewDescription.Left - frmDescription.Left - 8;
  // Row 1
  TCtrlArranger.AlignVCentres(
    3, [lblCollection, cbCollection, lblCollectionInfo]
  );
  // Row 2
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([lblCollection, cbCollection], 8),
    [lblDisplayName, edDisplayName]
  );
  // Row 3
  TCtrlArranger.AlignTops(
    [lblDescription, frmDescription, btnViewDescription],
    TCtrlArranger.BottomOf([lblDisplayName, edDisplayName], 8)
  );
  // Row 4
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(
      [lblDescription, frmDescription, btnViewDescription], 8
    ),
    [lblKind, cbKind, lblSnippetKindHelp]
  );
  // Row 5
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([lblKind, cbKind, lblSnippetKindHelp], 8),
    [lblCategories, cbCategories]
  );
  // Row 6
  TCtrlArranger.MoveToRightOf(cbKind, lblSnippetKindHelp, 12);
  TCtrlArranger.AlignTops(
    [lblSourceCode, lblSourceCaretPos],
    TCtrlArranger.BottomOf([lblCategories, cbCategories], 8)
  );
  // Row 7
  TCtrlArranger.MoveBelow([lblSourceCode, lblSourceCaretPos], edSourceCode, 4);
  // Row 8
  TCtrlArranger.MoveBelow(edSourceCode, chkUseHiliter, 8);

  // tsReferences
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(clbXRefs, 8), [edUnit, btnAddUnit]
  );

  // tsComments
  frmExtra.Width := tsComments.ClientWidth - 8;
  frmExtra.Height := clbDepends.Height;
  TCtrlArranger.AlignLefts([lblExtra, frmExtra, btnViewExtra], 3);
  TCtrlArranger.AlignVCentres(3, [lblExtra, lblExtraCaretPos]);
  TCtrlArranger.AlignRights([frmExtra, lblExtraCaretPos]);
  TCtrlArranger.MoveBelow([lblExtra, lblExtraCaretPos], frmExtra, 4);
  TCtrlArranger.MoveBelow(frmExtra, btnViewExtra, 8);

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

procedure TSnippetsEditorDlg.btnOKClick(Sender: TObject);
  {OnClick event handler for OK button. Validates entries and updates / adds
  snippet if all is well.
    @param Sender [in] Not used.
  }
begin
  inherited;
  try
    // Validate and record entered data
    ValidateData;
    fEditData.Assign(UpdateData);
    // Add or update snippet
    if Assigned(fSnippet) then
      (Database as IDatabaseEdit).UpdateSnippet(fSnippet, fEditData)
    else
    begin
      (Database as IDatabaseEdit).AddSnippet(
        UniqueSnippetKey, SelectedCollectionID, fEditData
      )
    end;
  except
    on E: Exception do
      HandleException(E);
  end;
end;

procedure TSnippetsEditorDlg.cbKindChange(Sender: TObject);
  {Handles change events on snippet kind drop down list. Updates list of
  references depending on new kind.
    @param Sender [in] Not used.
  }
begin
  UpdateReferences;
end;

procedure TSnippetsEditorDlg.ConfigForm;
  {Configures form's controls. Sets font and colors of "link" labels. Also sets
  item height of owner draw check list boxes.
  }
begin
  inherited;
  // Set colour and actions of link labels
  lblSnippetKindHelp.Font.Color := clHelpLink;
  TFontHelper.SetDefaultBaseFont(lblSnippetKindHelp.Font);
  lblViewCompErrs.Font.Color := clCommandLink;
  TFontHelper.SetDefaultBaseFont(lblViewCompErrs.Font);
  lblViewCompErrs.Caption := actViewErrors.Caption;
  lblViewCompErrsKey.Caption :=
    '(' + ShortcutToText(actViewErrors.ShortCut) + ')';
end;

function TSnippetsEditorDlg.CreateTempSnippet: TSnippet;
  {Creates a temporary snippet from data entered in dialog box.
    @return Required snippet instance.
    @except EDataEntry raised if any of entered data is invalid.
  }
var
  EditData: TSnippetEditData; // stores snippet's properties and references
begin
  ValidateData;
  // Create snippet object from entered data
  EditData.Assign(UpdateData);
  Result := (Database as IDatabaseEdit).CreateTempSnippet(
    UniqueSnippetKey, SelectedCollectionID, EditData
  );
end;

procedure TSnippetsEditorDlg.DisplayCompileResults(const Compilers: ICompilers);
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

class function TSnippetsEditorDlg.EditSnippet(AOwner: TComponent;
  const Snippet: TSnippet): Boolean;
  {Displays dialog box to enable user to edit a snippet.
    @param AOwner [in] Control that owns the dialog box, over which the dialog
      is aligned. May be nil.
    @param Snippet [in] Reference to snippet to be edited.
    @return True if user OKs, False if cancels.
  }
var
  Instance: TSnippetsEditorDlg;
resourcestring
  sCaption = 'Edit Snippet';  // dialogue box caption
begin
  Assert(Assigned(Snippet), ClassName + '.EditSnippet: Snippet is nil');
  Instance := InternalCreate(AOwner);
  try
    Instance.Caption := sCaption;
    Instance.fSnippet := Snippet;
    Result := Instance.ShowModal = mrOK;
  finally
    Instance.Free;
  end;
end;

procedure TSnippetsEditorDlg.FocusCtrl(const Ctrl: TWinControl);
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

procedure TSnippetsEditorDlg.FormCreate(Sender: TObject);
  {Form creation event handler. Creates owned objects.
    @param Sender [in] Not used.
  }
begin
  inherited;
  fCatList := TCategoryListAdapter.Create(Database.Categories);
  fCollList := TCollectionListAdapter.Create;
  fSnipKindList := TSnipKindListAdapter.Create;
  fCompileMgr := TCompileMgr.Create(Self);  // auto-freed
  fMemoCaretPosDisplayMgr := TMemoCaretPosDisplayMgr.Create;
  fDependsCLBMgr := TSnippetsChkListMgr.Create(clbDepends);
  fXRefsCLBMgr := TSnippetsChkListMgr.Create(clbXRefs);
  fUnitsCLBMgr := TUnitsChkListMgr.Create(clbUnits);
  fCompilersLBMgr := TCompileResultsLBMgr.Create(
    lbCompilers, fCompileMgr.Compilers
  );
  fSourceMemoHelper := TMemoHelper.Create(edSourceCode);
end;

procedure TSnippetsEditorDlg.FormDestroy(Sender: TObject);
  {Form destruction event handler. Frees owned objects.
    @param Sender [in] Not used.
  }
begin
  inherited;
  FreeAndNil(fSourceMemoHelper);
  FreeAndNil(fCompilersLBMgr);
  FreeAndNil(fUnitsCLBMgr);
  FreeAndNil(fXRefsCLBMgr);
  FreeAndNil(fDependsCLBMgr);
  FreeAndNil(fSnipKindList);
  FreeAndNil(fCollList);
  FreeAndNil(fCatList);
  fMemoCaretPosDisplayMgr.Free;
end;

procedure TSnippetsEditorDlg.HandleException(const E: Exception);
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

procedure TSnippetsEditorDlg.InitControls;
  {Initialises controls to default values.
  }
begin
  if Assigned(fSnippet) then
  begin
    // We are editing a snippet: initialise controls from snippet's properties
    edSourceCode.Text := fSnippet.SourceCode;
    chkUseHiliter.Checked := fSnippet.HiliteSource;
    frmDescription.DefaultEditMode := emAuto;
    frmDescription.ActiveText := fSnippet.Description;
    edDisplayName.Text := fSnippet.DisplayName;
    cbCategories.ItemIndex := fCatList.IndexOf(fSnippet.Category);
    cbCollection.ItemIndex := fCollList.IndexOfUID(fSnippet.VaultID);
    cbCollection.Visible := False;  // can't change existing snippet collection
    lblCollectionInfo.Caption := cbCollection.Text;
    lblCollectionInfo.Visible := True;
    frmExtra.DefaultEditMode := emAuto;
    frmExtra.ActiveText := fSnippet.Extra;
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
    chkUseHiliter.Checked := True;
    frmDescription.DefaultEditMode := emPlainText;
    frmDescription.Clear;
    edDisplayName.Clear;
    cbCategories.ItemIndex := fCatList.IndexOf(TCategory.DefaultID);
    if cbCategories.ItemIndex = -1 then
      cbCategories.ItemIndex := 0;
    cbCollection.ItemIndex := fCollList.IndexOfUID(TVaultID.Default);
    Assert(cbCollection.ItemIndex >= 0,
      ClassName + '.InitControls: No default collection in cbCollection');
    cbCollection.Visible := True; // can select collection of new snippet
    lblCollectionInfo.Visible := False;
    cbKind.ItemIndex := fSnipKindList.IndexOf(skFreeform);
    frmExtra.DefaultEditMode := emPlainText;
    frmExtra.Clear;
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
  fMemoCaretPosDisplayMgr.Manage(frmExtra, lblExtraCaretPos);
end;

procedure TSnippetsEditorDlg.InitForm;
  {Performs initialisation of form fields and controls.
  }
begin
  inherited;
  // Get data associated with snippet, or blank / default data if adding a new
  // snippet
  fEditData := (Database as IDatabaseEdit).GetEditableSnippetInfo(fSnippet);
  // Populate controls with dynamic data
  PopulateControls;
  // Initialise controls to default values
  InitControls;
  // Select first tab sheet
  pcMain.ActivePageIndex := 0;
end;

procedure TSnippetsEditorDlg.lblSnippetKindHelpClick(Sender: TObject);
  {OnClick event handler for Snippet Kind help link label. Displays help topic
  that informs what a Snippet Kind is.
    @param Sender [in] Not used.
  }
begin
  DisplayHelp('SnippetKinds');
end;

procedure TSnippetsEditorDlg.lblViewCompErrsClick(Sender: TObject);
  {OnClick event handler for compiler errors link label. Displays compiler
  warnings and errors in a dialog box.
    @param Sender [in] Not used.
  }
begin
  actViewErrors.Execute;
end;

procedure TSnippetsEditorDlg.pcMainChange(Sender: TObject);
  {Handler for OnChange event for page control. Used to load content into
  instructions HTML frame for comments tab.
    @param Sender [in] Not used.
  }
begin
  inherited;
  // We always hide "view errors" link whenever page changes since snippet
  // properties may have changed since page was last accessed
  pnlViewCompErrs.Hide;
end;

procedure TSnippetsEditorDlg.pcMainMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if htOnItem in pcMain.GetHitTestInfoAt(X, Y) then
    pcMain.SetFocus;
end;

procedure TSnippetsEditorDlg.PopulateControls;
  {Populates controls with dynamic data.
  }
begin
  // Display all kinds in drop down list
  fSnipKindList.ToStrings(cbKind.Items);
  // Display all available categories in drop down list
  fCatList.ToStrings(cbCategories.Items);
  // Display all available collections in drop down list
  fCollList.ToStrings(cbCollection.Items);
end;

function TSnippetsEditorDlg.SelectedCollectionID: TVaultID;
begin
  // If editing existing snippet ID then the collection cannot be edited
  if Assigned(fSnippet) then
    // Editing existing snippet: can't change collection
    Result := fSnippet.VaultID
  else
    // Editing new snippet: chosing collection is permitted
    Result := fCollList.Collection(cbCollection.ItemIndex).UID;
end;

procedure TSnippetsEditorDlg.SetAllCompilerResults(
  const CompRes: TCompileResult);
  {Sets all compiler results to same value.
    @param CompRes [in] Required compiler result.
  }
begin
  fCompilersLBMgr.SetCompileResults(CompRes);
end;

function TSnippetsEditorDlg.UniqueSnippetKey: string;
begin
  if Assigned(fSnippet) then
    Result := fSnippet.Key
  else
    Result := (Database as IDatabaseEdit).GetUniqueSnippetKey(
      SelectedCollectionID
    );
end;

function TSnippetsEditorDlg.UpdateData: TSnippetEditData;
  {Updates snippet's data from user entries. Assumes data has been validated.
    @return Record containing snippet's data.
  }
begin
  Result.Init;
  Result.Props.DisplayName := StrTrim(edDisplayName.Text);
  Result.Props.Cat := fCatList.CatID(cbCategories.ItemIndex);
  Result.Props.Kind := fSnipKindList.SnippetKind(cbKind.ItemIndex);
  (Result.Props.Desc as IAssignable).Assign(frmDescription.ActiveText);
  Result.Props.SourceCode := StrTrimRight(edSourceCode.Text);
  Result.Props.HiliteSource := chkUseHiliter.Checked;
  (Result.Props.Extra as IAssignable).Assign(frmExtra.ActiveText);
  Result.Props.CompilerResults := fCompilersLBMgr.GetCompileResults;
  Result.Refs.Units := fUnitsCLBMgr.GetCheckedUnits;
  Result.Refs.Depends := fDependsCLBMgr.GetCheckedSnippets;
  Result.Refs.XRef := fXRefsCLBMgr.GetCheckedSnippets;
end;

procedure TSnippetsEditorDlg.UpdateReferences;
  {Updates dependencies and cross-references check lists for snippet being
  edited, depending on kind.
  }
var
  EditSnippetID: TSnippetID;      // id of snippet being edited
  Snippet: TSnippet;              // each snippet in database
  EditSnippetKind: TSnippetKind;  // kind of snippet being edited
begin
  // Save state of dependencies and x-ref check list boxes and clear them
  fDependsCLBMgr.Save;
  fDependsCLBMgr.Clear;
  fXRefsCLBMgr.Save;
  fXRefsCLBMgr.Clear;

  EditSnippetID := TSnippetID.Create(UniqueSnippetKey, SelectedCollectionID);
  EditSnippetKind := fSnipKindList.SnippetKind(cbKind.ItemIndex);

  {TODO -cVault: We do following kind of filtering of Database.Snippets so
          often it would be useful to have a Filter or Select method of
          TSnippetList that can be passed a predicate to do this and return a
          filtered snippet list.
  }
  for Snippet in Database.Snippets do
  begin
    if Snippet.VaultID <> SelectedCollectionID then
      Continue;
    if Snippet.ID <> EditSnippetID then
    begin
      // Decide if snippet can be added to depends list: must be correct kind
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
  clbUnits.Enabled := EditSnippetKind <> skUnit;
  edUnit.Enabled := EditSnippetKind <> skUnit;
end;

procedure TSnippetsEditorDlg.ValidateData;
  {Checks all user-entered data in all tabs of the form.
    @except EDataEntry raised if data is not valid.
  }
resourcestring
  sDependencyPrompt = 'See the dependencies by right-clicking in the '
    + 'Dependencies list box on the References tab and selecting "View '
    + 'Dependencies" from the pop-up menu.';
var
  ErrorMessage: string;       // receives validation error messages
  ErrorSelection: TSelection; // receives selection containing errors
begin
  if not TSnippetValidator.ValidateDisplayName(
    edDisplayName.Text, ErrorMessage
  ) then
    raise EDataEntry.Create(ErrorMessage, edDisplayName);
  frmDescription.Validate;
  if not TSnippetValidator.ValidateSourceCode(
    edSourceCode.Text, ErrorMessage, ErrorSelection
  ) then
    raise EDataEntry.Create(ErrorMessage, edSourceCode, ErrorSelection);
  frmExtra.Validate;
  if not TSnippetValidator.ValidateDependsList(
    UniqueSnippetKey, SelectedCollectionID, UpdateData, ErrorMessage
  ) then
    raise EDataEntry.Create(  // selection not applicable to list boxes
      StrMakeSentence(ErrorMessage) + EOL2 + sDependencyPrompt, clbDepends
    );
end;

end.

