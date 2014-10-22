{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a dialogue box that enables the user to create or edit snippets.
}


unit FmSnippetsEditorDlg;


interface


uses
  // Delphi
  SysUtils,
  Classes,
  ActnList,
  StdCtrls,
  Forms,
  Controls,
  CheckLst,
  ComCtrls,
  ExtCtrls,
  StdActns,
  Menus,
  // Project
  CS.Components.EditCtrls,
  CS.Database.Types,
  CS.SourceCode.Languages,
  CS.UI.Frames.CodeEditor,
  CS.UI.Frames.TagsEditor,
  CS.UI.Helper.CollectionCtrlKVMgr,
  Compilers.UGlobals,
  FmGenericOKDlg,
  FmSnippetsEditorDlg.FrActiveTextEditor,
  UBaseObjects,
  UCompileMgr,
  UCompileResultsLBMgr,
  USnippetsChkListMgr,
  UUnitsChkListMgr;


type

  {
  TSnippetsEditorDlg:
    Dialogue box class that enables the user to create or edit a snippet.
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
    actViewNotes: TAction;
    actViewTestUnit: TAction;
    btnAddUnit: TButton;
    btnCompile: TButton;
    btnSetAllQuery: TButton;
    btnSetAllSuccess: TButton;
    btnViewNotes: TButton;
    btnViewTestUnit: TButton;
    cbLanguages: TComboBox;
    cbKind: TComboBox;
    clbDepends: TCheckListBox;
    clbUnits: TCheckListBox;
    clbXRefs: TCheckListBox;
    edUnit: TEdit;
    lbCompilers: TListBox;
    lblLanguages: TLabel;
    lblCompilers: TLabel;
    lblCompileShortcuts: TLabel;
    lblCompResDesc: TLabel;
    lblDepends: TLabel;
    lblDescription: TLabel;
    lblNotes: TLabel;
    lblKind: TLabel;
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
    frmNotes: TSnippetsActiveTextEdFrame;
    lblTitle: TLabel;
    edTitle: TEdit;
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
    frmSourceEditor: TCodeEditorFrame;
    lblTags: TLabel;
    frmTagsEditor: TTagsEditorFrame;
    procedure actAddUnitExecute(Sender: TObject);
    procedure actAddUnitUpdate(Sender: TObject);
    procedure actCompileExecute(Sender: TObject);
    procedure actCompileUpdate(Sender: TObject);
    procedure actViewDependenciesExecute(Sender: TObject);
    procedure actSetAllQueryExecute(Sender: TObject);
    procedure actSetAllSuccessExecute(Sender: TObject);
    procedure actViewErrorsExecute(Sender: TObject);
    procedure actViewErrorsUpdate(Sender: TObject);
    procedure actViewNotesExecute(Sender: TObject);
    procedure actViewNotesUpdate(Sender: TObject);
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
    procedure cbLanguagesChange(Sender: TObject);
  strict private
    fSnippet: ISnippet;             // Snippet being edited: nil for new snippet
    fCompileMgr: TCompileMgr;       // Manages compilation and results display
    fDependsCLBMgr:
      TSnippetsChkListMgr;          // Manages dependencies check list box
    fXRefsCLBMgr:
      TSnippetsChkListMgr;          // Manages x-refs check list box
    fUnitsCLBMgr: TUnitsChkListMgr; // Manages units check list box
    fCompilersLBMgr:
      TCompileResultsLBMgr;         // Manages compilers list box
    fSnippetKindCBMgr: TSortedCollectionCtrlKVMgr<TSnippetKindID>;
                                    // Manages snippet kind combo box
    fLanguageCBMgr: TSortedCollectionCtrlKVMgr<TSourceCodeLanguageID>;
                                    // Manages source code language combo box
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
    procedure ValidateData;
      {Checks all user-entered data in all tabs of the form.
        @except EDataEntry raised if data is not valid.
      }
    ///  <summary>Updates given snippet's properties from data entered by the
    ///  user.</summary>
    ///  <remarks>Assumes the data has been validated.</remarks>
    procedure UpdateSnippet(ASnippet: IEditableSnippet);
    procedure UpdateReferences;
      {Updates dependencies and cross-references check lists for snippet being
      edited, depending on kind.
      }
    function CreateTempSnippet: ISnippet;
      {Creates a temporary snippet from data entered in dialogue box.
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
      {Configures form's controls. Sets font and colours of "link" labels. Also
      sets item height of owner draw check list boxes.
      }
    procedure InitForm; override;
      {Performs initialisation of form fields and controls.
      }
  public
    class function AddNewSnippet(AOwner: TComponent): Boolean;
      {Displays dialogue box to enable user to enter a new snippet.
        @param AOwner [in] Control that owns the dialogue box, over which the
          dialogue is aligned. May be nil.
        @return True if user OKs, False if cancels.
      }
    class function EditSnippet(AOwner: TComponent; Snippet: ISnippet): Boolean;
      {Displays dialogue box to enable user to edit a snippet.
        @param AOwner [in] Control that owns the dialogue box, over which the
          dialogue is aligned. May be nil.
        @param Snippet [in] Reference to snippet to be edited.
        @return True if user OKs, False if cancels.
      }
  end;


implementation


uses
  // Delphi
  Windows {for inlining},
  // Project
  CS.Database.Snippets,
  CS.Database.Tags,
  CS.Config,
  DB.UMain,
  FmDependenciesDlg,
  IntfCommon,
  UColours,
  UConsts,
  UCtrlArranger,
  UExceptions,
  UFontHelper,
  UPreferences,
  USnippetValidator,
  UMessageBox,
  UStructs,
  UStrUtils,
  UTestUnitDlgMgr;


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
  (Sender as TAction).Enabled := not StrIsBlank(edUnit.Text)
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
  TempSnippet: ISnippet;  // temp snippet object for compilation
begin
  // Hide view compile errors link
  pnlViewCompErrs.Hide;
  // Disable dialogue box and all its controls and actions
  Enabled := False;
  try
    try
      TempSnippet := CreateTempSnippet; // this validates data
    except
      on E: Exception do
      begin
        Enabled := True;
        HandleException(E);
        Exit;
      end;
    end;
    // Test compile snippet
    fCompileMgr.Compile(
      tsCompileResults, TempSnippet, DisplayCompileResults
    );
  finally
    // Re-enable dialogue and controls
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
    and (fSnippetKindCBMgr.GetSelected <> skFreeForm);
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
  fCompilersLBMgr.SetCompileResults(crQuery);
end;

procedure TSnippetsEditorDlg.actSetAllSuccessExecute(Sender: TObject);
  {Sets all compiler results to "success".
    @param Sender [in] Not used.
  }
begin
  fCompilersLBMgr.SetCompileResults(crSuccess);
end;

procedure TSnippetsEditorDlg.actViewDependenciesExecute(Sender: TObject);
  {Displays dependencies for currently edited snippet, per entries in
  "Dependencies" check list box.
    @param Sender [in] Not used.
  }
var
  DependsList: ISnippetIDList;  // list of dependencies
  SnippetID: TSnippetID;
begin
  if Assigned(fSnippet) then
    SnippetID := fSnippet.ID
  else
    SnippetID := TSnippetID.CreateNull;
  DependsList := fDependsCLBMgr.GetCheckedSnippets;
  TDependenciesDlg.Execute(
    Self,
    SnippetID,
    StrTrim(edTitle.Text),
    DependsList,
    [tiDependsUpon],
    'SnippetsEditorDependenciesDlg'
  );
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

procedure TSnippetsEditorDlg.actViewNotesExecute(Sender: TObject);
  {Validates REML entered in the Notes memo control then displays it as it will
  appear in the main form.
    @param Sender [in] Not used.
  }
begin
  try
    frmNotes.Preview;
  except
    on E: Exception do
      HandleException(E);
  end;
end;

procedure TSnippetsEditorDlg.actViewNotesUpdate(Sender: TObject);
  {Disables View Notes action if no markup is entered in Notes tab.
    @param Sender [in] Reference to action to be updated.
  }
begin
  (Sender as TAction).Enabled := frmNotes.CanPreview;
end;

procedure TSnippetsEditorDlg.actViewTestUnitExecute(Sender: TObject);
  {Displays test unit for current snippet. Error message displayed if snippet
  is not valid.
    @param Sender [in] Not used.
  }
var
  TempSnippet: ISnippet;  // temp snippet object for compilation
begin
  try
    TempSnippet := CreateTempSnippet; // this validates data
  except
    on E: Exception do
    begin
      // TempSnippet not created if there is an exception here
      HandleException(E);
      Exit;
    end;
  end;
  TTestUnitDlgMgr.DisplayTestUnit(Self, TempSnippet);
end;

procedure TSnippetsEditorDlg.actViewTestUnitUpdate(Sender: TObject);
  {Updates View Test Unit action according to whether a compilable snippet is
  selected.
    @param Sender [in] Action triggering this event.
  }
begin
  (Sender as TAction).Enabled := fSnippetKindCBMgr.GetSelected <> skFreeForm;
end;

class function TSnippetsEditorDlg.AddNewSnippet(AOwner: TComponent): Boolean;
  {Displays dialogue box to enable user to enter a new snippet.
    @param AOwner [in] Control that owns the dialogue box, over which the
      dialogue is aligned. May be nil.
    @return True if user OKs, False if cancels.
  }
resourcestring
  sCaption = 'Add a Snippet';   // dialogue box caption
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

procedure TSnippetsEditorDlg.ArrangeForm;
  {Arranges controls on form and sizes it.
  }
begin
  // tsCode
  frmSourceEditor.Width := tsCode.ClientWidth - 8;
  TCtrlArranger.AlignLefts(
    [
      lblTitle, lblDescription, lblLanguages, lblSourceCode, frmSourceEditor,
      lblTags
    ],
    3
  );
  TCtrlArranger.AlignRights([frmSourceEditor, btnViewDescription]);
  frmDescription.Width := btnViewDescription.Left - frmDescription.Left - 8;
  TCtrlArranger.AlignVCentres(3, [lblTitle, edTitle]);
  TCtrlArranger.AlignTops(
    [lblDescription, frmDescription, btnViewDescription],
    TCtrlArranger.BottomOf([lblTitle, edTitle], 8)
  );
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(
      [lblDescription, frmDescription, btnViewDescription], 8
    ),
    [lblLanguages, cbLanguages, lblKind, cbKind, lblSnippetKindHelp]
  );
  TCtrlArranger.MoveToRightOf(cbLanguages, lblKind, 18);
  TCtrlArranger.MoveToRightOf(lblKind, cbKind, 12);
  TCtrlArranger.MoveToRightOf(cbKind, lblSnippetKindHelp, 6);
  TCtrlArranger.MoveToRightOf(lblTags, frmTagsEditor, 8);
  TCtrlArranger.MoveBelow([lblLanguages, cbLanguages], lblSourceCode, 8);
  TCtrlArranger.MoveBelow(lblSourceCode, frmSourceEditor, 4);
  TCtrlArranger.AlignTops(
    [lblTags, frmTagsEditor], TCtrlArranger.BottomOf(frmSourceEditor, 8)
  );

  // tsReferences
  clbDepends.Height := clbXRefs.Height;
  clbUnits.Height := clbDepends.Height;
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(clbXRefs, 8), [edUnit, btnAddUnit]
  );

  // tsComments
  frmNotes.Width := tsComments.ClientWidth - 8;
  frmNotes.Height := clbDepends.Height;
  TCtrlArranger.AlignLefts([lblNotes, frmNotes, btnViewNotes], 3);
  lblNotes.Top := 3;
  TCtrlArranger.MoveBelow(lblNotes, frmNotes, 4);
  TCtrlArranger.MoveBelow(frmNotes, btnViewNotes, 8);

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
var
  EditedSnippet: IEditableSnippet;
begin
  inherited;
  try
    // Validate and record entered data
    ValidateData;
    // Add or update snippet
    if Assigned(fSnippet) then
    begin
      EditedSnippet := TEditableSnippet.Create(fSnippet.ID);
      UpdateSnippet(EditedSnippet);
      Database.UpdateSnippet(EditedSnippet);
    end
    else
    begin
      EditedSnippet := Database.NewSnippet;
      UpdateSnippet(EditedSnippet);
      Database.AddSnippet(EditedSnippet);
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

procedure TSnippetsEditorDlg.cbLanguagesChange(Sender: TObject);
begin
  cbKind.Enabled := TSourceCodeLanguage.HasExtendedSupport(
    fLanguageCBMgr.GetSelected
  );
  if not cbKind.Enabled then
    fSnippetKindCBMgr.Select(skFreeform);
  frmSourceEditor.ApplyLanguage(
    TConfig.Instance.SourceCodeLanguages[fLanguageCBMgr.GetSelected]
  );
end;

procedure TSnippetsEditorDlg.ConfigForm;
  {Configures form's controls. Sets font and colours of "link" labels. Also sets
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
  frmSourceEditor.FontSize := 8;
  frmSourceEditor.Theme := TConfig.Instance.HiliterThemes[
    Preferences.CurrentHiliteThemeIds[htkUI]
  ];
end;

function TSnippetsEditorDlg.CreateTempSnippet: ISnippet;
  {Creates a temporary snippet from data entered in dialogue box.
    @return Required snippet instance.
    @except EDataEntry raised if any of entered data is invalid.
  }
var
  EditableSnippet: IEditableSnippet;
begin
  ValidateData;
  // Create snippet object from entered data
  EditableSnippet := TEditableSnippet.CreateNew;
  UpdateSnippet(EditableSnippet);
  Result := EditableSnippet.CloneAsReadOnly;
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
  Snippet: ISnippet): Boolean;
  {Displays dialogue box to enable user to edit a snippet.
    @param AOwner [in] Control that owns the dialogue box, over which the
      dialogue is aligned. May be nil.
    @param Snippet [in] Reference to snippet to be edited.
    @return True if user OKs, False if cancels.
  }
resourcestring
  sCaption = 'Edit Snippet';  // dialogue box caption
begin
  with InternalCreate(AOwner) do
    try
      Caption := sCaption;
      fSnippet := Snippet;
      Result := ShowModal = mrOK;
    finally
      Free;
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
  fCompileMgr := TCompileMgr.Create(Self);  // auto-freed
  fDependsCLBMgr := TSnippetsChkListMgr.Create(clbDepends);
  fXRefsCLBMgr := TSnippetsChkListMgr.Create(clbXRefs);
  fUnitsCLBMgr := TUnitsChkListMgr.Create(clbUnits);
  fCompilersLBMgr := TCompileResultsLBMgr.Create(
    lbCompilers, fCompileMgr.Compilers
  );
  fSnippetKindCBMgr := TSortedCollectionCtrlKVMgr<TSnippetKindID>.Create(
    TComboBoxAdapter.Create(cbKind),
    True,
    function (const Left, Right: TSnippetKindID): Boolean
    begin
      Result := Left = Right;
    end,
    stIgnoreCase
  );
  fLanguageCBMgr := TSortedCollectionCtrlKVMgr<TSourceCodeLanguageID>.Create(
    TComboBoxAdapter.Create(cbLanguages),
    True,
    function (const Left, Right: TSourceCodeLanguageID): Boolean
    begin
      Result := Left = Right;
    end,
    stIgnoreCase
  );
end;

procedure TSnippetsEditorDlg.FormDestroy(Sender: TObject);
  {Form destruction event handler. Frees owned objects.
    @param Sender [in] Not used.
  }
begin
  inherited;
  fLanguageCBMgr.Free;
  fSnippetKindCBMgr.Free;
  fCompilersLBMgr.Free;
  fUnitsCLBMgr.Free;
  fXRefsCLBMgr.Free;
  fDependsCLBMgr.Free;
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
var
  Language: TSourceCodeLanguage;
resourcestring
  sTitleCueText = 'Snippet title (max %d characters)';
  sUnitCueText = 'New unit name';
  sDescriptionCueText = 'Snippet description';
  sNotesCueText = 'Additional notes about the snippet';
  sSourceCodeCueText = 'Snippet''s source code';
begin
  // Set TextHint properties here, not at design-time, to take advantage of
  // hacks in CS.UI.Components.EditCtrls.
  edTitle.TextHint := Format(sTitleCueText, [TSnippetBase.MaxTitleLength]);
  edUnit.TextHint := sUnitCueText;
  frmDescription.TextHint := sDescriptionCueText;
  frmNotes.TextHint := sNotesCueText;
  frmSourceEditor.TextHint := sSourceCodeCueText;
  // Limits
  edTitle.MaxLength := TSnippetBase.MaxTitleLength;
  if Assigned(fSnippet) then
  begin
    // We are editing a snippet: initialise controls from snippet's properties
    frmSourceEditor.SourceCode := fSnippet.SourceCode;
    Language := TConfig.Instance.SourceCodeLanguages[fSnippet.LanguageID];
    frmDescription.DefaultEditMode := emAuto;
    frmDescription.ActiveText := fSnippet.Description;
    edTitle.Text := fSnippet.Title;
    frmNotes.DefaultEditMode := emAuto;
    frmNotes.ActiveText := fSnippet.Notes;
    fSnippetKindCBMgr.Select(fSnippet.KindID);
    frmTagsEditor.Tags := fSnippet.Tags;
    // check required items in references check list boxes
    UpdateReferences;
    fDependsCLBMgr.CheckSnippets(fSnippet.RequiredSnippets);
    fXRefsCLBMgr.CheckSnippets(fSnippet.XRefs);
    // ensure snippet's units are displayed checked in units check list box
    fUnitsCLBMgr.IncludeUnits(fSnippet.RequiredModules, True);
    fCompilersLBMgr.SetCompileResults(fSnippet.CompileResults);
  end
  else
  begin
    // We are adding a new snippet: clear all controls or set default values
    frmSourceEditor.Clear;
    { TODO: Permit default language to be customisable by user: modify
            Preferences dialogue box and Preferences object ? }
    Language := TConfig.Instance.SourceCodeLanguages[
      TSourceCodeLanguageID.CreatePascal
    ];
    frmDescription.DefaultEditMode := emPlainText;
    frmDescription.Clear;
    edTitle.Clear;
    fSnippetKindCBMgr.Select(skFreeForm);
    frmNotes.DefaultEditMode := emPlainText;
    frmNotes.Clear;
    frmTagsEditor.Clear;
    UpdateReferences;
    fCompilersLBMgr.SetCompileResults(crQuery);
  end;
  cbKind.Enabled := TSourceCodeLanguage.HasExtendedSupport(Language.ID);
  fLanguageCBMgr.Select(Language.ID);
  frmSourceEditor.ApplyLanguage(Language);
  Assert(cbKind.ItemIndex >= 0,
    ClassName + '.InitControls: no selection in cbKind');
  Assert(cbLanguages.ItemIndex >= 0,
    ClassName + '.InitControls: no selection in cbLanguages');
end;

procedure TSnippetsEditorDlg.InitForm;
  {Performs initialisation of form fields and controls.
  }
begin
  inherited;
  PopulateControls;
  InitControls;
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
  warnings and errors in a dialogue box.
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
var
  SnippetKind: TSnippetKind;
  Language: TSourceCodeLanguage;
begin
  for SnippetKind in Database.GetAllSnippetKinds do
    fSnippetKindCBMgr.Add(SnippetKind.ID, SnippetKind.DisplayName);
  for Language in TConfig.Instance.SourceCodeLanguages do
    fLanguageCBMgr.Add(Language.ID, Language.FriendlyName);
end;

procedure TSnippetsEditorDlg.UpdateReferences;
  {Updates dependencies and cross-references check lists for snippet being
  edited, depending on kind.
  }
var
  SnippetID: TSnippetID;
  Snippet: ISnippet;
  AllSnippetKinds: ISnippetKindList;
begin
  // Save state of dependencies and x-ref check list boxes and clear them
  fDependsCLBMgr.Save;
  fDependsCLBMgr.Clear;
  fXRefsCLBMgr.Save;
  fXRefsCLBMgr.Clear;
  AllSnippetKinds := Database.GetAllSnippetKinds;
  for SnippetID in Database.GetAllSnippets do
  begin
    // We ignore any snippet being edited
    if not Assigned(fSnippet) or (SnippetID <> fSnippet.ID) then
    begin
      Snippet := Database.LookupSnippet(SnippetID);
      // Decide if snippet can be added to depends list: must be correct kind
      if Snippet.KindID
        in AllSnippetKinds[fSnippetKindCBMgr.GetSelected].ValidDependIDs then
        fDependsCLBMgr.AddSnippet(Snippet);
      // Anything can be in XRefs list
      fXRefsCLBMgr.AddSnippet(Snippet);
    end;
  end;
  // Restore checks to any saved checked item that still exist in new list
  fDependsCLBMgr.Restore;
  fXRefsCLBMgr.Restore;
  clbUnits.Enabled := fSnippetKindCBMgr.GetSelected <> skUnit;
  edUnit.Enabled := fSnippetKindCBMgr.GetSelected <> skUnit;
end;

procedure TSnippetsEditorDlg.UpdateSnippet(ASnippet: IEditableSnippet);
begin
  ASnippet.Title := StrTrim(edTitle.Text);
  ASnippet.KindID := fSnippetKindCBMgr.GetSelected;
  ASnippet.Description := frmDescription.ActiveText;
  ASnippet.SourceCode := StrTrimRight(frmSourceEditor.SourceCode);
  ASnippet.LanguageID := fLanguageCBMgr.GetSelected;
  ASnippet.Notes := frmNotes.ActiveText;
  ASnippet.Tags := TTagSet.Create(frmTagsEditor.Tags);
  ASnippet.CompileResults := fCompilersLBMgr.GetCompileResults;
  ASnippet.RequiredModules := fUnitsCLBMgr.GetCheckedUnits;
  ASnippet.RequiredSnippets := fDependsCLBMgr.GetCheckedSnippets;
  ASnippet.XRefs := fXRefsCLBMgr.GetCheckedSnippets;
end;

procedure TSnippetsEditorDlg.ValidateData;
  {Checks all user-entered data in all tabs of the form.
    @except EDataEntry raised if data is not valid.
  }
resourcestring
  sDependencyPrompt = 'See the dependencies by right-clicking in the '
    + 'Dependencies list box and selecting "View Dependencies" from the pop-up '
    + 'menu.';
var
  ErrorMessage: string;           // receives validation error messages
  ErrorSelection: TSelection;     // receives selection containing errors
  TempSnippet: IEditableSnippet;  // temporary snippet checked for dependencies
begin
  if not TSnippetValidator.ValidateTitle(
    edTitle.Text, ErrorMessage
  ) then
    raise EDataEntry.Create(ErrorMessage, edTitle);
  frmDescription.Validate;
  if not TSnippetValidator.ValidateSourceCode(
    frmSourceEditor.SourceCode, ErrorMessage, ErrorSelection
  ) then
    raise EDataEntry.Create(ErrorMessage, frmSourceEditor, ErrorSelection);
  frmNotes.Validate;
  if Assigned(fSnippet) then
  begin
    // NOTE: Don't use CreateTempSnippet method of this class here since it
    // calls this method and will create an endless loop.
    TempSnippet := TEditableSnippet.Create(fSnippet.ID);
    UpdateSnippet(TempSnippet);
    if not TSnippetValidator.ValidateDependsList(
      TempSnippet.CloneAsReadOnly, ErrorMessage
    ) then
      raise EDataEntry.Create(  // selection not applicable to list boxes
        StrMakeSentence(ErrorMessage) + EOL2 + sDependencyPrompt, clbDepends
      );
    end;
end;

end.

