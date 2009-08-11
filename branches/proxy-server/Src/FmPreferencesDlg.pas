{
 * FmPreferencesDlg.pas
 *
 * Implements a dialog box that is used to set user preferences.
 *
 * v0.1 of 06 Jan 2006  - Original version.
 * v0.2 of 10 Jan 2006  - Removed unsupported properties from form on reversion
 *                        to Delphi 7.
 * v1.0 of 25 May 2006  - Improved and corrected comments.
 *                      - Removed unused unit reference.
 *                      - Localised file type and comment style descriptions.
 * v1.1 of 29 Oct 2006  - Changed to use renamed IPreferences properties.
 *                      - Changed logic to deal with change in
 *                        SourceDefaultFileType to return enumeration rather
 *                        than file extension.
 *                      - Changed descriptions of supported file types, removing
 *                        file extensions.
 * v2.0 of 09 Nov 2006  - Major revision.
 *                      - Changed form so that each tab in page control now
 *                        simply hosts a frame that in turn lets user edit a
 *                        group of preferences. The frames expose methods to
 *                        load and save relevant preferences. All other controls
 *                        removed.
 *                      - Source Code tab changed to host TSourcePrefsFrame that
 *                        provides same functionality.
 *                      - Added new Syntax Highlighter tab that hosts
 *                        THiliterPrefsFrame that in turn permits customisation
 *                        of highlighter.
 * v2.1 of 08 Feb 2007  - Moved control initialisation code from FormCreate
 *                        event handler to new overridden InitForm method and
 *                        deleted FormCreate method.
 * v3.0 of 07 Sep 2007  - Major revision.
 *                      - Added new General and Printing tabs
 *                      - Added ability to display a subset of them preference
 *                        pages.
 *                      - Dialog now always displayed left-most visible tab
 *                        sheet when it opens.
 *                      - Changed so that help button now displays a help topic
 *                        relating to displayed page.
 *                      - Renamed tab sheets and frames. Frame names may be are
 *                        used for help A-Link names.
 *                      - Changed to use a temporary preferences object to
 *                        received updates and to update main preferences when
 *                        user OKs.
 *                      - Each tab sheet is now activated and deactivated via
 *                        revised interface. On deactivation the sheets update
 *                        the temp preferences object. On activation sheets
 *                        re-initialise controls.
 * v3.1 of 04 Nov 2007  - Removed the IAssignable cast from the parameter to
 *                        IAssignable.Assign method calls.
 * v3.2 of 21 May 2009  - Resized body panel and frame to accomodate larger
 *                        highlighter preferences frame.
 * v3.3 of 19 Jul 2009  - Resized form to accommodate revised frames. Also sets
 *                        width and height of frames and gets them to arrange
 *                        their own controls.
 *                      - Changed assertions to use ClassName.
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
 * The Original Code is FmPreferencesDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
  * ***** END LICENSE BLOCK *****
}


unit FmPreferencesDlg;


interface


uses
  // Delphi
  Classes, ComCtrls, StdCtrls, Controls, ExtCtrls, Forms,
  // Project
  FmGenericOKDlg, FrGeneralPrefs, FrHiliterPrefs, FrPrefsBase, FrPrintingPrefs,
  FrSourcePrefs, UPreferences;

type

  {
  TPreferencePage:
    Enumeration of IDs of preference pages.
  }
  TPreferencePage = (
    ppGeneral,            // General preferences
    ppSourceCode,         // Source code formatting
    ppSyntaxHighlighter,  // Syntax highlighter customisation
    ppPrinting            // Printing options
  );

  {
  TPreferencePages:
    Sets of preference pages. Used to define which preference pages are
    displayed in dialog box.
  }
  TPreferencePages = set of TPreferencePage;

  {
  TPreferencesDlg:
    Dialog box that sets user preferences.
  }
  TPreferencesDlg = class(TGenericOKDlg)
    pcMain: TPageControl;
    tsSourceCodePrefs: TTabSheet;
    frmSourceCodePrefs: TSourcePrefsFrame;
    tsHiliterPrefs: TTabSheet;
    frmHiliterPrefs: THiliterPrefsFrame;
    tsPrintingPrefs: TTabSheet;
    frmPrintingPrefs: TPrintingPrefsFrame;
    tsGeneralPrefs: TTabSheet;
    frmGeneralPrefs: TGeneralPrefsFrame;
    procedure btnOKClick(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure pcMainChanging(Sender: TObject; var AllowChange: Boolean);
  private
    fLocalPrefs: IPreferences;
      {Object that stores local copy of preferences. Use to pass changes between
      pages. Main preferences object is only updated if user OKs the dialog box}
    fVisiblePages: TPreferencePages;
      {Set of IDs of preferences pages displayed in dialog box}
    fPageMap: array[TPreferencePage] of TTabSheet;
      {Maps page IDs onto to tab sheets storing the pages}
    procedure SetVisiblePages(const Pages: TPreferencePages);
      {Record ids of visible preferences pages.
        @param Pages [in] Set of visible pages. If Pages=[] then all pages are
          visible.
      }
    function MapTabSheetToPage(const TabSheet: TTabSheet): TPrefsBaseFrame;
      {Gets reference to preferences page frame associated with a tab sheet.
        @param TabSheet [in] Tab sheet for which page frame required.
        @return Reference to associated page frame.
      }
    function GetPrefPage(const Page: TPreferencePage): TPrefsBaseFrame;
      {Gets reference to a specified preferences frame.
        @param Page [in] Id of required frame.
        @return Reference to required frame.
      }
    function GetSelectedPage: TPrefsBaseFrame;
      {Gets refence to preferences frame on currently selected tab.
        @return Reference to required frame.
      }
  protected
    function CustomHelpKeyword: string; override;
      {Gets the help A-link keyword to be used when help button clicked. Keyword
      depends on which preferences page is displayed. This permits each
      preferences page to have its own help topic.
        @return Required frame-specific A-link keyword.
      }
    procedure ConfigForm; override;
      {Sets up map of pages to frames that display on them.
      }
    procedure ArrangeForm; override;
      {Sizes frames providing content of pages of dialog and gets each to
      arrange its controls.
      }
    procedure InitForm; override;
      {Displays and initialises frames used to display pages of dialog.
      }
  public
    class function Execute(AOwner: TComponent;
      const Pages: TPreferencePages = []): Boolean;
      {Displays dialog with requested pages displayed and informs whether dialog
      was OKed or cancelled.
        @param AOwner [in] Component that owns dialog.
        @param Pages [in] Pages to be displayed. If Pages=[] then all pages are
          displayed.
        @return True if user accepts changes or false if cancels.
      }
  end;


implementation


{
  Design notes
  ------------

  This dialog box is a multi-page preferences dialog that provides access to
  each page via a tab. The dialog box does not provide an implementation of
  each page of the dialog. This representation must be provided by a frame
  descended from TPrefsBaseFrame that

  (a) Displays all required controls
  (b) Manages input via the controls
  (c) Can load the required data from local copy of preferences object
  (d) Can save updated data to local copy of preferences object

  Each tab sheet is associated with a value from the TPreferencePage
  enumeration. TPreferencesDlg contains a field that maps TPreferencePage
  values to tab sheets.

  When tab sheets are added two things are required:
  (1) An entry must be added to the TPreference enumeration.
  (2) An entry must be made in TPreferenceDlg's fPageMap[] array that maps the
      new TPreferences entry to the new tab sheet (see the InitForm method).

}


uses
  // Delphi
  StrUtils, Windows,
  // Project
  IntfCommon;


{$R *.dfm}

procedure TPreferencesDlg.ArrangeForm;
  {Sizes frames providing content of pages of dialog and gets each to arrange
  its controls.
  }
var
  Page: TPreferencePage;  // loops through all preferences pages
begin
  inherited;
  for Page := Low(TPreferencePage) to High(TPreferencePage) do
  begin
    Assert(Assigned(fPageMap[Page]),
      ClassName + '.ArrangeForm: Page map incomplete');
    MapTabSheetToPage(fPageMap[Page]).Width := fPageMap[Page].Width - 8;
    MapTabSheetToPage(fPageMap[Page]).Height := fPageMap[Page].Height - 8;
    MapTabSheetToPage(fPageMap[Page]).ArrangeControls;
  end;
end;

procedure TPreferencesDlg.btnOKClick(Sender: TObject);
  {OK button click event handler. Stores preference data to persistent storage.
    @param Sender [in] Not used.
  }
var
  Page: TPreferencePage;  // loops through all preferences pages
begin
  inherited;
  // Get each visible page to update local preferences
  for Page := Low(TPreferencePage) to High(TPreferencePage) do
    if fPageMap[Page].TabVisible then
      GetPrefPage(Page).SavePrefs(fLocalPrefs);
  // Update global preferences with data from local preferences object
  (Preferences as IAssignable).Assign(fLocalPrefs);
end;

procedure TPreferencesDlg.ConfigForm;
  {Sets up map of pages to frames that display on them.
  }
begin
  inherited;
  // Set up page map ** IMPORTANT - add entry here for each tabsheet
  fPageMap[ppGeneral] := tsGeneralPrefs;
  fPageMap[ppSourceCode] := tsSourceCodePrefs;
  fPageMap[ppSyntaxHighlighter] := tsHiliterPrefs;
  fPageMap[ppPrinting] := tsPrintingPrefs;
end;

function TPreferencesDlg.CustomHelpKeyword: string;
  {Gets the help A-link keyword to be used when help button clicked. Keyword
  depends on which preferences page is displayed. This permits each preferences
  page to have its own help topic.
    @return Required frame-specific A-link keyword.
  }
var
  SelPage: TPrefsBaseFrame; // reference to frame on selected tab sheet
begin
  SelPage := GetSelectedPage;
  // We use any keyword assigned to selected frame in preference
  Result := SelPage.HelpKeyword;
  if Result = '' then
  begin
    // No specified keyword: use a keyword based on frame name
    Result := SelPage.Name;
    if AnsiStartsStr('frm', Result) then
      // chop off any leading "frm" from frame name
      Result := AnsiRightStr(Result, Length(Result) - 3);
  end;
end;

class function TPreferencesDlg.Execute(AOwner: TComponent;
  const Pages: TPreferencePages = []): Boolean;
  {Displays dialog with requested pages displayed and informs whether dialog was
  OKed or cancelled.
    @param AOwner [in] Component that owns dialog.
    @param Pages [in] Pages to be displayed. If Pages=[] then all pages are
      displayed.
    @return True if user accepts changes or false if cancels.
  }
begin
  with TPreferencesDlg.Create(AOwner) do
    try
      SetVisiblePages(Pages);
      Result := ShowModal = mrOK;
    finally
      Free;
    end;
end;

function TPreferencesDlg.GetPrefPage(
  const Page: TPreferencePage): TPrefsBaseFrame;
  {Gets reference to a specified preferences frame.
    @param Page [in] Id of required frame.
    @return Reference to required frame.
  }
begin
  Result := MapTabSheetToPage(fPageMap[Page]);
end;

function TPreferencesDlg.GetSelectedPage: TPrefsBaseFrame;
  {Gets refence to preferences frame on currently selected tab.
    @return Reference to required frame.
  }
begin
  Result := MapTabSheetToPage(pcMain.ActivePage);
end;

procedure TPreferencesDlg.InitForm;
  {Displays and initialises frames used to display pages of dialog.
  }
var
  TabIdx: Integer;        // loops thru tabs in page control
  Page: TPreferencePage;  // loops through all preferences pages
begin
  inherited;
  // Take local copy of global preferences. This local copy will be updated as
  // user enters data and global object is only updated if user OKs - local
  // copy is discarded if user cancels.
  fLocalPrefs := (Preferences as IClonable).Clone as IPreferences;
  // Display and initialise required pages
  for Page := Low(TPreferencePage) to High(TPreferencePage) do
  begin
    Assert(Assigned(fPageMap[Page]),
      ClassName + '.InitForm: Page map incomplete');
    fPageMap[Page].TabVisible := Page in fVisiblePages;
    if fPageMap[Page].TabVisible then
      // get page to set up controls from preferences
      GetPrefPage(Page).LoadPrefs(fLocalPrefs);
  end;
  // Select lowest indexed visible tab
  for TabIdx := 0 to Pred(pcMain.PageCount) do
    if pcMain.Pages[TabIdx].TabVisible then
    begin
      pcMain.ActivePageIndex := TabIdx;
      Break;
    end;
end;

function TPreferencesDlg.MapTabSheetToPage(
  const TabSheet: TTabSheet): TPrefsBaseFrame;
  {Gets reference to preferences page frame associated with a tab sheet.
    @param TabSheet [in] Tab sheet for which page frame required.
    @return Reference to associated page frame.
  }
var
  CtrlIdx: Integer; // loops through all controls on tab sheet
begin
  Result := nil;
  for CtrlIdx := 0 to Pred(TabSheet.ControlCount) do
  begin
    if (TabSheet.Controls[CtrlIdx] is TPrefsBaseFrame) then
    begin
      Result := TabSheet.Controls[CtrlIdx] as TPrefsBaseFrame;
      Break;
    end;
  end;
  Assert(Assigned(Result), ClassName + '.MapTabSheetToPage: Frame not found');
end;

procedure TPreferencesDlg.pcMainChange(Sender: TObject);
  {Called when current tab sheet has changed. Gets newly selected page to re-
  initialise its controls from local preferences. This enables any pages that
  depend on preferences that may have been changed in other pages to update
  appropriately.
    @param Sender [in] Not used.
  }
begin
  GetSelectedPage.Activate(fLocalPrefs);
end;

procedure TPreferencesDlg.pcMainChanging(Sender: TObject;
  var AllowChange: Boolean);
  {Called just before active tab sheet is changed. Causes page about to be
  deselected to update local preferences with any changes. We do this in case
  another page needs to update due to changes made on current page.
    @param Sender [in] Not used.
  }
begin
  GetSelectedPage.Deactivate(fLocalPrefs);
end;

procedure TPreferencesDlg.SetVisiblePages(const Pages: TPreferencePages);
  {Record ids of visible preferences pages.
    @param Pages [in] Set of visible pages. If Pages=[] then all pages are
      visible.
  }
var
  Page: TPreferencePage;  // loops through all preferences pages
begin
  fVisiblePages := [];
  for Page := Low(TPreferencePage) to High(TPreferencePage) do
    if (Page in Pages) or (Pages = []) then
      Include(fVisiblePages, Page);
end;

end.

