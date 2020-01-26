{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a wizard dialogue box that lets the user select and import
 * snippets from the DelphiDabbler implementation of the SWAG Pascal archive.
}

// TODO -cSWAG: change to import SWAG from local file - user must download and extract

unit FmSWAGImportDlg;

interface

uses
  // Delphi
  SysUtils,
  ComCtrls,
  CheckLst,
  Controls,
  StdCtrls,
  Forms,
  ExtCtrls,
  Classes,
  Generics.Collections,
  // 3rd party
  Collections.Base,
  Collections.Lists,
  // Project
  FmWizardDlg,
  FrBrowserBase,
  FrFixedHTMLDlg,
  FrHTMLDlg,
  FrHTMLTpltDlg,
  UBaseObjects,
  SWAG.UCommon,
  SWAG.UImporter,
  SWAG.UReader, ActnList;


type
  ///  <summary>Class that implements a wizard dialogue box that lets the user
  ///  select and import snippets from the DelphiDabbler implementation of the
  ///  SWAG Pascal archive.</summary>
  TSWAGImportDlg = class(TWizardDlg, INoPublicConstruct)
    tsIntro: TTabSheet;
    tsCategories: TTabSheet;
    lblCategories: TLabel;
    frmIntro: TFixedHTMLDlgFrame;
    lbCategories: TListBox;
    lblCategoriesDesc: TLabel;
    lblSelectSnippets: TLabel;
    clbSelectSnippets: TCheckListBox;
    tsUpdate: TTabSheet;
    lvImports: TListView;
    lblUpdateDesc: TLabel;
    tsFinish: TTabSheet;
    frmOutro: THTMLTpltDlgFrame;
    btnDisplayCategory: TButton;
    alWizard: TActionList;
    actDisplayCategory: TAction;
    actDisplaySnippet: TAction;
    btnDisplaySnippet: TButton;
    ///  <summary>Handles clicks on the check boxes next to snippets in the
    ///  snippet selection list box by selecting and deselecting snippets for
    ///  inclusion in the import.</summary>
    procedure clbSelectSnippetsClickCheck(Sender: TObject);
    ///  <summary>Handles double clicks on snippets in the snippet selection
    ///  list box by causing the selected snippet to be previewed.</summary>
    procedure clbSelectSnippetsDblClick(Sender: TObject);
    ///  <summary>Handles key down events on the snippet selection list box by
    ///  causing the selected snippet to be previewed when the user presses
    ///  Enter.</summary>
    procedure clbSelectSnippetsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    ///  <summary>Handles double clicks on categories in the SWAG categories
    ///  list box by displaying the category's snippets in the snippet selection
    ///  list box.</summary>
    procedure lbCategoriesDblClick(Sender: TObject);
    ///  <summary>Handles key down events on categories in the SWAG categories
    ///  list box by displaying the category's snippets in the snippet selection
    ///  list box when the user presses enter.</summary>
    procedure lbCategoriesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure actDisplayCategoryUpdate(Sender: TObject);
    procedure actDisplayCategoryExecute(Sender: TObject);
    procedure actDisplaySnippetExecute(Sender: TObject);
    procedure actDisplaySnippetUpdate(Sender: TObject);
  strict private
    const
      ///  <summary>Index of introductory page in wizard.</summary>
      cIntroPage = 0;
      ///  <summary>Index of snippet selection page in wizard.</summary>
      cSelectionPage = 1;
      ///  <summary>Index of import page in wizard.</summary>
      cUpdatePage = 2;
      ///  <summary>Index of finish page in wizard.</summary>
      cFinishPage = 3;
    var
      ///  <summary>Object that provides cached access to the SWAG database.
      ///  </summary>
      fSWAGReader: TSWAGReader;
      ///  <summary>List of all categories in SWAG database, sorted by title.
      ///  </summary>
      fSortedCategories: TSortedList<TSWAGCategory>;
      ///  <summary>List of snippets in the current category, sorted by title.
      ///  </summary>
      fCurrentCatSnippets: TSortedList<TSWAGSnippet>;
      ///  <summary>List of snippets selected for import, sorted by ID.
      ///  </summary>
      fSelectedSnippets: TSortedList<TSWAGSnippet>;
      ///  <summary>Object that imports selected SWAG snippets into CodeSnip's
      ///  database.</summary>
      fImporter: TSWAGImporter;
      ///  <summary>ID of currently selected category.</summary>
      ///  <remarks>Set to empty string if no category is selected.</remarks>
      fCurrentCatID: string;
    ///  <summary>Validates entries on the wizard page identified by the given
    ///  page index.</summary>
    procedure ValidatePage(const PageIdx: Integer);
    ///  <summary>Displays snippets selected for import in list view on Update
    ///  page.</summary>
    procedure PopulateImportsLV;
    ///  <summary>Initialises Selection page by populating its list of SWAG
    ///  categories, if necessary.</summary>
    ///  <remarks>May display a wait dialogue box if the categories have to be
    ///  downloaded from the SWAG database.</remarks>
    procedure InitSelectionPage;
    ///  <summary>Initialises Update page by retrieving all the selected
    ///  snippets, preparing them for import and displaying them in the page's
    ///  list view.</summary>
    ///  <remarks>May display a wait dialogue box if any of the snippets to be
    ///  imported have to be downloaded from the SWAG database.</remarks>
    procedure InitUpdatePage;
    ///  <summary>Gets the snippets contained is any selected category and
    ///  displays them in the snippet selection list box on the Selection page.
    ///  </summary>
    ///  <remarks>May display a wait dialogue box if the snippets have to be
    ///  downloaded from the SWAG database.</remarks>
    procedure DisplaySnippetsForCategory;
    ///  <summary>Creates and displays a preview of the currently selected
    ///  snippet in the Selection page's snippet selection list box.</summary>
    ///  <remarks>May display a wait dialogue box if the selected snippet has to
    ///  be downloaded from the SWAG database.</remarks>
    procedure PreviewSelectedSnippet;
    ///  <summary>Gets the complete information for each snippet selected for
    ///  import and stores in the given list.</summary>
    procedure GetImportSnippets(const SnipList:
      Generics.Collections.TList<TSWAGSnippet>);
    ///  <summary>Performs the import of the selected snippets into CodeSnip's
    ///  database.</summary>
    ///  <remarks>Displays a wait dialogue box while the import is proceeding.
    ///  </remarks>
    procedure UpdateDatabase;
    ///  <summary>Executes a given callback while displaying a wait dialogue
    ///  box.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns the wait
    ///  dialogue box over which the dialogue is aligned.</param>
    ///  <param name="CallProc">TProc [in] Callback closure to be executed while
    ///  wait dialogue box is displayed.</param>
    ///  <param name="WaitMsg">string [in] Message to be displayed in wait
    ///  dialogue box.</param>
    procedure WaitWrapper(AOwner: TComponent; const CallProc: TProc;
      const WaitMsg: string);
  strict protected
    ///  <summary>Constructs and intialises a wizard instance.</summary>
    constructor InternalCreate(AOwner: TComponent); override;
    ///  <summary>Aligns and arranges controls in each tab sheet and sizes
    ///  dialogue box to accomodate controls.</summary>
    ///  <remarks>Overridden method called from ancestor class.</remarks>
    procedure ArrangeForm; override;
    ///  <summary>Initialises wizard pages that display HTML content.</summary>
    ///  <remarks>Overridden method called from ancestor class.</remarks>
    procedure ConfigForm; override;
    ///  <summary>Returns text of heading on page indexed by PageIdx.</summary>
    ///  <remarks>Overridden method called from ancestor class.</remarks>
    function HeadingText(const PageIdx: Integer): string; override;
    ///  <summary>Updates state and caption of buttons on page index by
    ///  PageIdx.</summary>
    ///  <remarks>Implementation of abstract method called from ancestor class.
    ///  </remarks>
    procedure UpdateButtons(const PageIdx: Integer); override;
    ///  <summary>Initialises page indexed by PageIdx.</summary>
    ///  <remarks>Overridden method called from ancestor class.</remarks>
    procedure BeginPage(const PageIdx: Integer); override;
    ///  <summary>Validates page specified by PageIdx then performs any action
    ///  required before next page is displayed.</summary>
    ///  <remarks>Overridden method called from ancestor class.</remarks>
    procedure MoveForward(const PageIdx: Integer; var CanMove: Boolean);
      override;
  public
    ///  <summary>Destroys wizard dialogue box instance.</summary>
    destructor Destroy; override;
    ///  <summary>Displays SWAG import wizard aligned over given owner control
    ///  and returns True if the user performs an import or False if the user
    ///  cancels.</summary>
    class function Execute(const AOwner: TComponent): Boolean;
  end;


implementation


uses
  // Delphi
  Generics.Defaults,
  Windows,
  // Project
  CS.Utils.Hashes,
  FmPreviewDlg,
  FmWaitDlg,
  UComparers,
  UConsts,
  UCtrlArranger,
  UEncodings,
  UExceptions,
  UHTMLTemplate,
  UMessageBox,
  UStrUtils,
  UWaitForThreadUI;

{$R *.dfm}


{ TSWAGImportDlg }

procedure TSWAGImportDlg.actDisplayCategoryExecute(Sender: TObject);
begin
  DisplaySnippetsForCategory;
end;

procedure TSWAGImportDlg.actDisplayCategoryUpdate(Sender: TObject);
begin
  actDisplayCategory.Enabled := lbCategories.ItemIndex >= 0;
end;

procedure TSWAGImportDlg.actDisplaySnippetExecute(Sender: TObject);
begin
  PreviewSelectedSnippet;
end;

procedure TSWAGImportDlg.actDisplaySnippetUpdate(Sender: TObject);
begin
  actDisplaySnippet.Enabled := clbSelectSnippets.ItemIndex >= 0;
end;

procedure TSWAGImportDlg.ArrangeForm;
begin
  TCtrlArranger.SetLabelHeights(Self);
  // Arrange controls on tab sheets

  // tsIntro
  frmIntro.Height := frmIntro.DocHeight;

  // tsCategories
  lblCategoriesDesc.Width := tsCategories.ClientWidth;
  lblCategoriesDesc.Top := 3;
  TCtrlArranger.AlignLefts(
    [lblCategoriesDesc, lblCategories, lbCategories], 0
  );
  TCtrlArranger.AlignTops(
    [lblCategories, lblSelectSnippets],
    TCtrlArranger.BottomOf(lblCategoriesDesc, 12)
  );
  TCtrlArranger.AlignTops(
    [lbCategories, clbSelectSnippets],
    TCtrlArranger.BottomOf([lblCategories, lblSelectSnippets], 6)
  );
  TCtrlArranger.AlignTops(
    [btnDisplayCategory, btnDisplaySnippet],
    TCtrlArranger.BottomOf([lbCategories, clbSelectSnippets], 8)
  );
  TCtrlArranger.AlignHCentresTo([lbCategories], [btnDisplayCategory]);
  TCtrlArranger.AlignHCentresTo([clbSelectSnippets], [btnDisplaySnippet]);

  // tsUpdate
  lblUpdateDesc.Width := tsUpdate.ClientWidth;
  lblUpdateDesc.Top := 3;
  lvImports.Width := tsUpdate.ClientWidth;
  TCtrlArranger.AlignLefts([lblUpdateDesc, lvImports], 0);
  TCtrlArranger.MoveBelow(lblUpdateDesc, lvImports, 12);

  // tsFinish
  frmOutro.Height := frmOutro.DocHeight;

  // set required height
  pnlBody.ClientHeight := TCtrlArranger.MaxContainerHeight(
    [tsIntro, tsCategories, tsUpdate, tsFinish]
  ) + pnlBody.ClientHeight - tsFinish.Height;

  // re-size controls to fit height
  lvImports.Height := tsUpdate.ClientHeight - lvImports.Top;

  inherited;
end;

procedure TSWAGImportDlg.BeginPage(const PageIdx: Integer);
begin
  case PageIdx of
    cSelectionPage: InitSelectionPage;
    cUpdatePage: InitUpdatePage;
  end;
end;

procedure TSWAGImportDlg.clbSelectSnippetsClickCheck(Sender: TObject);
var
  SelIdx: Integer;
  DelIdx: Integer;
begin
  SelIdx := clbSelectSnippets.ItemIndex;
  if SelIdx = -1 then
    Exit;
  if clbSelectSnippets.Checked[SelIdx] then
  begin
    if not fSelectedSnippets.Contains(fCurrentCatSnippets[SelIdx]) then
      fSelectedSnippets.Add(fCurrentCatSnippets[SelIdx]);
  end
  else
  begin
    DelIdx := fSelectedSnippets.IndexOf(fCurrentCatSnippets[SelIdx]);
    if DelIdx >= 0 then
      fSelectedSnippets.RemoveAt(DelIdx);
  end;
end;

procedure TSWAGImportDlg.clbSelectSnippetsDblClick(Sender: TObject);
begin
  PreviewSelectedSnippet;
end;

procedure TSWAGImportDlg.clbSelectSnippetsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    PreviewSelectedSnippet;
end;

procedure TSWAGImportDlg.ConfigForm;
begin
  inherited;
  pcWizard.ActivePage := tsFinish;
  frmOutro.Initialise(
    'dlg-swag-import-outro-tplt.html',
    procedure (Tplt: THTMLTemplate)
    begin
      Tplt.ResolvePlaceholderText('SWAGTag', TSWAGImporter.SWAGTagName);
    end
  );
  pcWizard.ActivePage := tsIntro;
  frmIntro.Initialise('dlg-swag-import-intro.html');
end;

destructor TSWAGImportDlg.Destroy;
begin
  fSWAGReader.Free;
  fImporter.Free;
  fSelectedSnippets.Free;
  fCurrentCatSnippets.Free;
  fSortedCategories.Free;
  inherited;
end;

procedure TSWAGImportDlg.DisplaySnippetsForCategory;
resourcestring
  sSnippetListCaption = '&Select snippets from "%s"';
var
  CatIdx: Integer;
  Idx: Integer;
  N: Integer;
  Snippets: Generics.Collections.TList<TSWAGSnippet>;
  Snippet: TSWAGSnippet;
begin
  CatIdx := lbCategories.ItemIndex;
  if CatIdx = -1 then
  begin
    fCurrentCatID := '';
    Exit;
  end;
  if fCurrentCatID = fSortedCategories[CatIdx].ID then
    // nothing to do if current category selected again
    Exit;
  fCurrentCatID := fSortedCategories[CatIdx].ID;
  Snippets := Generics.Collections.TList<TSWAGSnippet>.Create;
  lblSelectSnippets.Caption := Format(
    sSnippetListCaption, [fSortedCategories[CatIdx].Title]
  );
  try
    fSWAGReader.GetPartialSnippets(fCurrentCatID, Snippets);
    clbSelectSnippets.Items.BeginUpdate;
    try
      fCurrentCatSnippets.Clear;
      clbSelectSnippets.Clear;
      // We set fCurrentCatSnippets first because it is a sorted list which
      // means indices of new items added are not sequential, and we must have
      // displayed title at same index in clbSelectSnippets as its snippet is in
      // fCurrentCatSnippets.
      { TODO -cRefactor: When conversion to Delphi Collections complete, try to assign
              Snippets to fCurrentCatSnippets in one operation. }
      for Snippet in Snippets do
        fCurrentCatSnippets.Add(Snippet);
      for Idx := 0 to Pred(fCurrentCatSnippets.Count) do
      begin
        N := clbSelectSnippets.Items.Add(fCurrentCatSnippets[Idx].Title);
        Assert(Idx = N, 'Idx <> N');
        clbSelectSnippets.Checked[Idx] := fSelectedSnippets.Contains(
          fCurrentCatSnippets[Idx]
        );
      end;
    finally
      clbSelectSnippets.Items.EndUpdate;
    end;
  finally
    Snippets.Free;
  end;
end;

class function TSWAGImportDlg.Execute(const AOwner: TComponent): Boolean;
begin
  with InternalCreate(AOwner) do
    try
      Result := ShowModal = mrOK;
    finally
      Free;
    end;
end;

procedure TSWAGImportDlg.GetImportSnippets(const SnipList:
  Generics.Collections.TList<TSWAGSnippet>);
var
  SnipIDs: Generics.Collections.TList<Cardinal>;
  PartialSnippet: TSWAGSnippet;
resourcestring
  sWaitMsg = 'Downloading Snippets From SWAG...';
begin
  SnipIDs := Generics.Collections.TList<Cardinal>.Create;
  try
    for PartialSnippet in fSelectedSnippets do
      SnipIDs.Add(PartialSnippet.ID);
    fSWAGReader.GetCompleteSnippets(
      SnipIDs,
      SnipList,
      procedure (CallProc: TProc)
      begin
        WaitWrapper(Self, CallProc, sWaitMsg);
      end
    );
  finally
    SnipIDs.Free;
  end;
end;

function TSWAGImportDlg.HeadingText(const PageIdx: Integer): string;
resourcestring
  sIntroPageHeading = 'Import snippets from SWAG';
  sSelectionPageHeading = 'Select required snippets';
  sUpdatePage = 'Ready to import';
  sFinishPage = 'Import complete';
begin
  case PageIdx of
    cIntroPage:     Result := sIntroPageHeading;
    cSelectionPage: Result := sSelectionPageHeading;
    cUpdatePage:    Result := sUpdatePage;
    cFinishPage:    Result := sFinishPage;
  end;
end;

procedure TSWAGImportDlg.InitSelectionPage;
var
  Cats: Generics.Collections.TList<TSWAGCategory>;
  Cat: TSWAGCategory;
  Idx: Integer;
begin
  Application.ProcessMessages;
  if lbCategories.Count > 0 then
    Exit;

  Cats := Generics.Collections.TList<TSWAGCategory>.Create;
  try
    fSWAGReader.GetCategories(Cats);
    lbCategories.Items.BeginUpdate;
    try
      // We set fSortedCategories first because it is a sorted list which means
      // indices of new items added are not sequential, and we must have
      // displayed title at same index in lbCategories as its category is in
      // fSortedCategories.
      { TODO -cRefactor: When conversion to Delphi Collections complete, try to assign
              Cats to fSortedCategories in one operation. }
      for Cat in Cats do
        fSortedCategories.Add(Cat);
      for Idx := 0 to Pred(fSortedCategories.Count) do
        lbCategories.Items.Add(fSortedCategories[Idx].Title);
      lbCategories.ItemIndex := -1;
    finally
      lbCategories.Items.EndUpdate;
    end;
  finally
    Cats.Free;
  end;
end;

procedure TSWAGImportDlg.InitUpdatePage;
var
  FullSnippets: Generics.Collections.TList<TSWAGSnippet>;
  Snippet: TSWAGSnippet;
begin
  Application.ProcessMessages;
  FullSnippets := Generics.Collections.TList<TSWAGSnippet>.Create;
  try
    GetImportSnippets(FullSnippets);
    fImporter.Reset;
    for Snippet in FullSnippets do
      fImporter.IncludeSnippet(Snippet);
  finally
    FullSnippets.Free;
  end;
  PopulateImportsLV;
end;

constructor TSWAGImportDlg.InternalCreate(AOwner: TComponent);
resourcestring
  sDefaultWaitMsg = 'Accessing SWAG...';
begin
  inherited;
  fSortedCategories := TSortedList<TSWAGCategory>.Create(
    TRulesFactory<TSWAGCategory>.Construct(
      function (const Left, Right: TSWAGCategory): Integer
      begin
        Result := StrCompareStr(Left.Title, Right.Title);
      end,
      function (const Cat: TSWAGCategory): Integer
      begin
        Result := StrHash(Cat.Title);
      end
    )
  );

  fCurrentCatSnippets := TSortedList<TSWAGSnippet>.Create(
    TRulesFactory<TSWAGSnippet>.Construct(
      function (const Left, Right: TSWAGSnippet): Integer
      begin
        Result := StrCompareStr(Left.Title, Right.Title);
      end,
      function (const Snippet: TSWAGSnippet): Integer
      begin
        Result := StrHash(Snippet.Title);
      end
    )
  );

  // TODO -cSWAG: change to some structure that doesn't permit duplicates.
  fSelectedSnippets := TSortedList<TSWAGSnippet>.Create(
    TRulesFactory<TSWAGSnippet>.Construct(
      function (const Left, Right: TSWAGSnippet): Integer
      begin
        Result := Left.ID - Right.ID;
      end,
      function (const Left, Right: TSWAGSnippet): Boolean
      begin
        Result := Left.ID = Right.ID;
      end,
      function (const Snippet: TSWAGSnippet): Integer
      begin
        Result := Integer(Snippet.ID);
      end
    )
  );

  fImporter := TSWAGImporter.Create;

  fSWAGReader := TSWAGReader.Create(
    procedure (CallProc: TProc)
    begin
      WaitWrapper(Self, CallProc, sDefaultWaitMsg);
    end
  );
end;

procedure TSWAGImportDlg.lbCategoriesDblClick(Sender: TObject);
begin
  DisplaySnippetsForCategory;
end;

procedure TSWAGImportDlg.lbCategoriesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    DisplaySnippetsForCategory;
end;

procedure TSWAGImportDlg.MoveForward(const PageIdx: Integer;
  var CanMove: Boolean);
begin
  CanMove := False;
  try
    ValidatePage(PageIdx);
    case PageIdx of
      cUpdatePage: UpdateDatabase;
    end;
    CanMove := True;
  except
    on E: EDataEntry do
    begin
      TMessageBox.Error(Self, E.Message);
      if Assigned(E.Ctrl) then
        E.Ctrl.SetFocus;
    end;
  end;
end;

procedure TSWAGImportDlg.PopulateImportsLV;
var
  SWAGSnippet: TSWAGSnippet;
  LI: TListItem;
begin
  lvImports.Items.BeginUpdate;
  try
    lvImports.Clear;
    for SWAGSnippet in fSelectedSnippets do
    begin
      LI := lvImports.Items.Add;
      LI.Caption := SWAGSnippet.Title;
      LI.SubItems.Add(TSWAGImporter.MakeValidSnippetIDString(SWAGSnippet.ID));
    end;
  finally
    lvImports.Items.EndUpdate;
  end;
end;

procedure TSWAGImportDlg.PreviewSelectedSnippet;
var
  PartialSnippet: TSWAGSnippet;
  FullSnippet: TSWAGSnippet;
  SelIdx: Integer;
  Content: string;
resourcestring
  sWaitMsg = 'Downloading Snippet From SWAG...';
  sContentTplt = 'ID: %0:d' + EOL +
    'Category: "%1:s"' + EOL +
    'File Name: "%2:s"' + EOL +
    'Title: "%3:s"' + EOL +
    'Author: "%4:s"' + EOL2 +
    'Source Code:' + EOL + '%5:s' + EOL + '%6:s';
begin
  SelIdx := clbSelectSnippets.ItemIndex;
  if SelIdx = -1 then
    Exit;
  PartialSnippet := fCurrentCatSnippets[SelIdx];
  FullSnippet := fSWAGReader.GetCompleteSnippet(
    PartialSnippet.ID,
    procedure (CallProc: TProc)
    begin
      WaitWrapper(Self, CallProc, sWaitMsg);
    end
  );
  // TODO -cLater: Display snippet as HTML, highlighted if necessary
  Content := Format(
    sContentTplt,
    [
      FullSnippet.ID,
      FullSnippet.Category,
      FullSnippet.FileName,
      FullSnippet.Title,
      FullSnippet.Author,
      StringOfChar('-', 80),
      StrWindowsLineBreaks(FullSnippet.SourceCode)
    ]
  );
  TPreviewDlg.Execute(
    Self,
    TEncodedData.Create(Content, etUTF8),
    dtPlainText
  );
end;

procedure TSWAGImportDlg.UpdateButtons(const PageIdx: Integer);
resourcestring
  // button caption for update page
  sUpdateCaption = 'Import';
begin
  inherited;
  case PageIdx of
    cUpdatePage: btnNext.Caption := sUpdateCaption;
    cFinishPage: btnBack.Enabled := False;
  end;
end;

procedure TSWAGImportDlg.UpdateDatabase;

  procedure SetBtnVisibility(const Show: Boolean);
  begin
    btnNext.Visible := Show;
    btnBack.Visible := Show;
    btnCancel.Visible := Show;
    btnHelp.Visible := Show;
    Application.ProcessMessages;
  end;

begin
  SetBtnVisibility(False);
  try
    WaitWrapper(
      Self,
      procedure
      begin
        fImporter.Import(
          procedure (const Snippet: TSWAGSnippet)
          begin
            Application.ProcessMessages;
          end
        );
      end,
      'Importing Snippets Into Database...'
    );
  finally
    SetBtnVisibility(True);
  end;
end;

procedure TSWAGImportDlg.ValidatePage(const PageIdx: Integer);
resourcestring
  sEmptySelection = 'You must select one or more snippets to import.';
begin
  case PageIdx of
    cSelectionPage:
    begin
      if fSelectedSnippets.Empty then
        raise EDataEntry.Create(sEmptySelection);
    end;
  end;
end;

procedure TSWAGImportDlg.WaitWrapper(AOwner: TComponent; const CallProc: TProc;
  const WaitMsg: string);
const
  PauseBeforeDisplay = 0;
  MinDisplayTime = 1000;
begin
  // Perform the operation
  TWaitForThreadUI.Run( // this blocks until thread completes
    CallProc,
    False,
    TWaitDlg.CreateAutoFree(AOwner, WaitMsg),
    PauseBeforeDisplay,
    MinDisplayTime
  );
end;

end.

