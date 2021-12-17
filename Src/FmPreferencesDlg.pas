{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that is used to set user preferences.
}


unit FmPreferencesDlg;


interface


uses
  // Delphi
  ComCtrls, StdCtrls, Controls, ExtCtrls, Classes, Generics.Collections,
  // Project
  FmGenericOKDlg, FrPrefsBase, UBaseObjects, UPreferences;


type
  ///  <summary>
  ///  Dialog box that sets user preferences.
  ///  </summary>
  ///  <remarks>
  ///  This dialog box displays tabs for preferences frames registered with the
  ///  dialog box.
  ///  </remarks>
  TPreferencesDlg = class(TGenericOKDlg, INoPublicConstruct)
    pcMain: TPageControl;
    ///  <summary>OK button click event handler. Writes preference data to
    ///  persistent storage.</summary>
    procedure btnOKClick(Sender: TObject);
    ///  <param>Called when current tab sheet has changed. Gets newly selected
    ///  page to re-initialise its controls from local preferences.</param>
    ///  <remarks>This enables any pages that depend on preferences that may
    ///  have been changed in other pages to update appropriately.</remarks>
    procedure pcMainChange(Sender: TObject);
    ///  <summary>Called just before active tab sheet is changed. Causes page
    ///  about to be deselected to update local preferences with any changes.
    ///  </summary>
    ///  <remarks>We do this in case another page needs to update due to changes
    ///  made on current page.</remarks>
    procedure pcMainChanging(Sender: TObject; var AllowChange: Boolean);
    ///  <summary>Handles event triggered when user clicks on one of page
    ///  control tabs. Ensures page control has focus.</summary>
    ///  <remarks>Without this fix, page control does not always get focus when
    ///  a tab is clicked.</remarks>
    procedure pcMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  strict private
    class var
      ///  <summary>List of registered page frames</summary>
      fPages: TList<TPrefsFrameClass>;
    var
      ///  <summary>Local copy of preferences.</summary>
      fLocalPrefs: IPreferences;
      ///  <summary>Records if main UI needs to be updated to reflect changed
      ///  preferences.</summary>
      fUpdateUI: Boolean;
    ///  <summary>Creates the required frames and displays each in a tab sheet
    ///  within the page control.</summary>
    ///  <param name="FrameClasses">array of TPrefsFrameClass [in] Class
    ///  references for each required frame.</param>
    procedure CreatePages(const FrameClasses: array of TPrefsFrameClass);
    ///  <summary>Gets reference to preferences page frame associated with given
    ///  tab sheet.</summary>
    function MapTabSheetToPage(const TabSheet: TTabSheet): TPrefsBaseFrame;
      overload;
    ///  <summary>Gets reference to preferences page frame associated with given
    ///  tab index.</summary>
    function MapTabSheetToPage(const TabIdx: Integer): TPrefsBaseFrame;
      overload;
    ///  <summary>Gets reference to preferences page frame with given class
    ///  name.</summary>
    ///  <remarks>Returns nil if ClsName is not recognised.</remarks>
    class function MapClassNameToPageClass(const ClsName: string):
      TPrefsFrameClass;
    ///  <summary>Gets reference to preferences frame on currently selected tab.
    ///  </summary>
    function GetSelectedPage: TPrefsBaseFrame;
  strict protected
    ///  <summary>Gets the help A-link keyword to be used when help button
    ///  clicked.</summary>
    ///  <remarks>Keyword depends on which preferences page is displayed to
    ///  permit each preferences page to have its own help topic.</remarks>
    function CustomHelpKeyword: string; override;
    ///  <summary>Sizes frames providing content of pages of dialog and gets
    ///  each to arrange its controls.</summary>
    procedure ArrangeForm; override;
    ///  <summary>Displays and initialises frames used to display pages of
    ///  dialog.</summary>
    procedure InitForm; override;
  public
    ///  <summary>Creates empty registered page frame list object.</summary>
    class constructor Create;
    ///  <summary>Frees registered page frame list object.</summary>
    class destructor Destroy;
    ///  <summary>Displays dialog with pages for each specified preferences
    ///  frame.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns dialog.
    ///  </param>
    ///  <param name="Pages">array of TPrefsFrameClass [in] Class references of
    ///  frames to be displayed.</param>
    ///  <param name="UpdateUI">Boolean [out] Indicates if main UI needs to
    ///  be updated as a result of preference changes.</param>
    ///  <returns>Boolean. True if user clicks OK to accept changes or False if
    ///  user cancels and no changes made.</returns>
    class function Execute(AOwner: TComponent;
      const Pages: array of TPrefsFrameClass; out UpdateUI: Boolean): Boolean;
      overload;
    ///  <summary>Displays dialog with pages for each specified preferences
    ///  frame.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns dialog.
    ///  </param>
    ///  <param name="Pages">array of TPrefsFrameClass [in] Class references of
    ///  frames to be displayed.</param>
    ///  <returns>Boolean. True if user clicks OK to accept changes or False if
    ///  user cancels and no changes made.</returns>
    class function Execute(AOwner: TComponent;
      const Pages: array of TPrefsFrameClass): Boolean; overload;
    ///  <summary>Displays preferences dialog with all registered preference
    ///  frames.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns dialog.
    ///  </param>
    ///  <param name="UpdateUI">Boolean [out] Indicates if main UI needs to
    ///  be updated as a result of preference changes.</param>
    ///  <returns>Boolean. True if user clicks OK to accept changes or False if
    ///  user cancels and no changes made.</returns>
    class function Execute(AOwner: TComponent; out UpdateUI: Boolean): Boolean;
      overload;
    ///  <summary>Displays dialogue with showing a single frame, specified by
    ///  its class name.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns dialog.
    ///  </param>
    ///  <param name="PageClsName">string [in] Class name of the frame to be
    ///  displayed.</param>
    ///  <param name="UpdateUI">Boolean [out] Indicates if main UI needs to
    ///  be updated as a result of preference changes.</param>
    ///  <returns>Boolean. True if user clicks OK to accept changes or False if
    ///  user cancels and no changes made.</returns>
    class function Execute(AOwner: TComponent; const PageClsName: string;
      out UpdateUI: Boolean): Boolean; overload;
    ///  <summary>Registers given preferences frame class for inclusion in the
    ///  preferences dialog box.</summary>
    ///  <remarks>Registered frames are created when the dialog box is displayed
    ///  and freed when it closes.</remarks>
    class procedure RegisterPage(const FrameCls: TPrefsFrameClass);
  end;


implementation


{
  Design notes
  ------------

  This dialog box is a multi-page preferences dialog that provides access to
  each page via a tab. The dialog box does not provide an implementation of each
  page of the dialog. This representation must be provided by a frame descended
  from TPrefsBaseFrame. Such frames must:
    (a) register themselves with the dialog box by passing their class to the
        TPreferencesDlg.RegisterPage class method.
    (b) implement all the abstract methods of TPrefsBaseFrame.

  The dialog box will create registered frames when needed and host them within
  a tab sheet in the main page control.

  There is no need to modify this unit when a new frame is to be addded to it.
}


uses
  // Project
  IntfCommon;


{$R *.dfm}

{ TPreferencesDlg }

procedure TPreferencesDlg.ArrangeForm;
var
  Idx: Integer;           // loops through all displayed tab sheets
  Frame: TPrefsBaseFrame; // references each preference frame
  TabSheet: TTabSheet;    // references each tab sheet
begin
  inherited;
  for Idx := 0 to Pred(pcMain.PageCount) do
  begin
    TabSheet := pcMain.Pages[Idx];
    Frame := MapTabSheetToPage(TabSheet);
    Frame.Width := TabSheet.Width - 8;
    Frame.Height := TabSheet.Height - 8;
    Frame.ArrangeControls;
  end;
end;

procedure TPreferencesDlg.btnOKClick(Sender: TObject);
var
  TabIdx: Integer; // loops through all tab sheets on page
begin
  inherited;
  // Get each visible page to update local preferences
  fUpdateUI := False;
  for TabIdx := 0 to Pred(pcMain.PageCount) do
  begin
    MapTabSheetToPage(TabIdx).SavePrefs(fLocalPrefs);
    fUpdateUI := fUpdateUI or MapTabSheetToPage(TabIdx).UIUpdated;
  end;
  // Update global preferences with data from local preferences object
  (Preferences as IAssignable).Assign(fLocalPrefs);
end;

class constructor TPreferencesDlg.Create;
begin
  fPages := TList<TPrefsFrameClass>.Create;
end;

procedure TPreferencesDlg.CreatePages(
  const FrameClasses: array of TPrefsFrameClass);
var
  Idx: Integer;           // loops through FrameClasses array
  TS: TTabSheet;          // references each tabsheet as it is created
  Frame: TPrefsBaseFrame; // references each frame as it is created
begin
  for Idx := Low(FrameClasses) to High(FrameClasses) do
  begin
    // create a tabsheet in page control
    TS := TTabSheet.Create(Self);
    TS.PageControl := pcMain;
    // create a frame parented by tab sheet
    Frame := FrameClasses[Idx].Create(Self);
    Frame.Parent := TS;
    Frame.Left := 4;
    Frame.Top := 4;
    // set tab sheet caption to frame's display name
    TS.Caption := Frame.DisplayName;
  end;
end;

function TPreferencesDlg.CustomHelpKeyword: string;
begin
  // We expect keyword to be stored in frame's HelpKeyword property
  Result := GetSelectedPage.HelpKeyword;
end;

class destructor TPreferencesDlg.Destroy;
begin
  fPages.Free;
end;

class function TPreferencesDlg.Execute(AOwner: TComponent;
  const Pages: array of TPrefsFrameClass; out UpdateUI: Boolean): Boolean;
begin
  with InternalCreate(AOwner) do
    try
      CreatePages(Pages);
      Result := ShowModal = mrOK;
      if Result then
        UpdateUI := fUpdateUI
      else
        UpdateUI := False;
    finally
      Free;
    end;
end;

class function TPreferencesDlg.Execute(AOwner: TComponent;
  out UpdateUI: Boolean): Boolean;
begin
  Result := Execute(AOwner, fPages.ToArray, UpdateUI);
end;

class function TPreferencesDlg.Execute(AOwner: TComponent;
  const Pages: array of TPrefsFrameClass): Boolean;
var
  Dummy: Boolean; // unused UpdateUI parameters
begin
  Result := Execute(AOwner, Pages, Dummy);
end;

class function TPreferencesDlg.Execute(AOwner: TComponent;
  const PageClsName: string; out UpdateUI: Boolean): Boolean;
var
  FrameClass: TPrefsFrameClass;
begin
  FrameClass := MapClassNameToPageClass(PageClsName);
  Assert(Assigned(FrameClass), ClassName + '.Execute: PageClsName not valid');
  Result := Execute(AOwner, [FrameClass], UpdateUI);
end;

function TPreferencesDlg.GetSelectedPage: TPrefsBaseFrame;
begin
  Result := MapTabSheetToPage(pcMain.ActivePage);
end;

procedure TPreferencesDlg.InitForm;
var
  TabIdx: Integer;  // loops thru tabs in page control
begin
  inherited;
  // Take local copy of global preferences. This local copy will be updated as
  // user enters data and global object is only updated if user OKs - local
  // copy is discarded if user cancels.
  fLocalPrefs := (Preferences as IClonable).Clone as IPreferences;
  // Display and initialise required pages
  for TabIdx := 0 to Pred(pcMain.PageCount) do
    MapTabSheetToPage(TabIdx).LoadPrefs(fLocalPrefs);
  // Select first TabSheet
  pcMain.ActivePageIndex := 0;
end;

class function TPreferencesDlg.MapClassNameToPageClass(const ClsName: string):
  TPrefsFrameClass;
var
  Cls: TPrefsFrameClass;
begin
  for Cls in fPages do
    if Cls.ClassNameIs(ClsName) then
      Exit(Cls);
  Result := nil;
end;

function TPreferencesDlg.MapTabSheetToPage(const TabIdx: Integer):
  TPrefsBaseFrame;
begin
  Result := MapTabSheetToPage(pcMain.Pages[TabIdx]);
end;

function TPreferencesDlg.MapTabSheetToPage(
  const TabSheet: TTabSheet): TPrefsBaseFrame;
var
  CtrlIdx: Integer; // loops through all child controls of tab sheet
begin
  Assert(Assigned(TabSheet), ClassName + '.MapTabSheetToPage: TabSheet is nil');
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
begin
  GetSelectedPage.Activate(fLocalPrefs);
end;

procedure TPreferencesDlg.pcMainChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  GetSelectedPage.Deactivate(fLocalPrefs);
end;

procedure TPreferencesDlg.pcMainMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if htOnItem in pcMain.GetHitTestInfoAt(X, Y) then
    pcMain.SetFocus;
end;

class procedure TPreferencesDlg.RegisterPage(const FrameCls: TPrefsFrameClass);
var
  PageIdx: Integer; // loops through all registered frames
  InsIdx: Integer;  // place to insert new frame in list (per Index property)
begin
  // Search for place in frame list to insert new frame (sorted on frame's Index
  // property).
  InsIdx := fPages.Count;
  for PageIdx := 0 to Pred(fPages.Count) do
  begin
    if FrameCls.Index < fPages[PageIdx].Index then
    begin
      InsIdx := PageIdx;
      Break;
    end;
  end;
  // Record the frame's class reference
  fPages.Insert(InsIdx, FrameCls);
end;

end.

