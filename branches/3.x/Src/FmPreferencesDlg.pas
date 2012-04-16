{
 * FmPreferencesDlg.pas
 *
 * Implements a dialog box that is used to set user preferences.
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
 * The Original Code is FmPreferencesDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmPreferencesDlg;


interface


uses
  // Delphi
  Contnrs, ComCtrls, StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericOKDlg, FrPrefsBase, UBaseObjects, UPreferences;


type

  {
  TPreferencesDlg:
    Dialog box that sets user preferences.
  }
  TPreferencesDlg = class(TGenericOKDlg, INoPublicConstruct)
    pcMain: TPageControl;
    procedure btnOKClick(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure pcMainChanging(Sender: TObject; var AllowChange: Boolean);
    procedure pcMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  strict private
    class var fPages: TClassList;   // List of registered page frames
    class var fGC: IInterface;      // Garbage collector for class var
    var fLocalPrefs: IPreferences;  // Llocal copy of preferences.
    procedure CreatePages(const FrameClasses: array of TPrefsFrameClass);
      {Creates the required frames and displays each frame in a tab sheet within
      the page control.
        @param Frames [in] Class references for each required frame.
      }
    function MapTabSheetToPage(const TabSheet: TTabSheet): TPrefsBaseFrame;
      overload;
      {Gets reference to preferences page frame associated with a tab sheet.
        @param TabSheet [in] Tab sheet for which page frame required.
        @return Reference to associated frame.
      }
    function MapTabSheetToPage(const TabIdx: Integer): TPrefsBaseFrame;
      overload;
      {Gets reference to preferences page frame associated with a tab index.
        @param TabIdx [in] Index of tab sheet for which page frame required.
        @return Reference to associated frame.
      }
    function GetSelectedPage: TPrefsBaseFrame;
      {Gets refence to preferences frame on currently selected tab.
        @return Reference to required frame.
      }
  strict protected
    function CustomHelpKeyword: string; override;
      {Gets the help A-link keyword to be used when help button clicked. Keyword
      depends on which preferences page is displayed. This permits each
      preferences page to have its own help topic.
        @return Required frame-specific A-link keyword.
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
      const Pages: array of TPrefsFrameClass): Boolean; overload;
      {Displays dialog with pages for each specified preferences frame.
        @param AOwner [in] Component that owns dialog.
        @param Pages [in] Class references of frames to be displayed.
        @return True if user accepts changes or false if cancels.
      }
    class function Execute(AOwner: TComponent): Boolean; overload;
      {Displays preferences dialog with all pages for all registered preference
      frames.
        @param AOwner [in] Component that owns dialog.
      }
    class procedure RegisterPage(const FrameCls: TPrefsFrameClass);
      {Registers a preferences frame class for inclusion in the preferences
      dialog box. Registered frames are created when the dialog box is displayed
      and freed when it closes.
        @param FrameCls [in] Class of frame to be registered.
      }
  end;


implementation


{
  Design notes
  ------------

  This dialog box is a multi-page preferences dialog that provides access to
  each page via a tab. The dialog box does not provide an implementation of
  each page of the dialog. This representation must be provided by a frames
  descended from TPrefsBaseFrame. Such frames must:
    (a) register themselves with the dialog box by passing their class to the
        TPreferencesDlg.RegisterPage class method.
    (b) implement all the abstract methods of TPrefsBaseFrame.

  The dialog box will create registered frames when needed and host them within
  a tab sheet in the main page control.

  There is no need to modify this unit when a new frame is to be addded to it.
}


uses
  // Project
  IntfCommon, UGC;


{$R *.dfm}

{ TPreferencesDlg }

procedure TPreferencesDlg.ArrangeForm;
  {Sizes frames providing content of pages of dialog and gets each to arrange
  its controls.
  }
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
  {OK button click event handler. Writes preference data to persistent storage.
    @param Sender [in] Not used.
  }
var
  TabIdx: Integer; // loops through all tab sheets on page
begin
  inherited;
  // Get each visible page to update local preferences
  for TabIdx := 0 to Pred(pcMain.PageCount) do
    MapTabSheetToPage(TabIdx).SavePrefs(fLocalPrefs);
  // Update global preferences with data from local preferences object
  (Preferences as IAssignable).Assign(fLocalPrefs);
end;

procedure TPreferencesDlg.CreatePages(
  const FrameClasses: array of TPrefsFrameClass);
  {Creates the required frames and displays each frame in a tab sheet within the
  page control.
    @param Frames [in] Class references for each required frame.
  }
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
  {Gets the help A-link keyword to be used when help button clicked. Keyword
  depends on which preferences page is displayed. This permits each preferences
  page to have its own help topic.
    @return Required frame-specific A-link keyword.
  }
begin
  // We expect keyword to be stored in frame's HelpKeyword property
  Result := GetSelectedPage.HelpKeyword;
end;

class function TPreferencesDlg.Execute(AOwner: TComponent;
  const Pages: array of TPrefsFrameClass): Boolean;
  {Displays dialog with pages for each specified preferences frame.
    @param AOwner [in] Component that owns dialog.
    @param Pages [in] Class references of frames to be displayed.
    @return True if user accepts changes or false if cancels.
  }
begin
  with InternalCreate(AOwner) do
    try
      CreatePages(Pages);
      Result := ShowModal = mrOK;
    finally
      Free;
    end;
end;

class function TPreferencesDlg.Execute(AOwner: TComponent): Boolean;
  {Displays preferences dialog with all pages for all registered preference
  frames.
    @param AOwner [in] Component that owns dialog.
  }
var
  FrameClasses: array of TPrefsFrameClass;  // array of preference frame classes
  Idx: Integer;                             // loops through registered frames
begin
  // Copy registered frame classes into dynamic array and pass this to
  // constructor that accepts a list of frames classes.
  SetLength(FrameClasses, fPages.Count);
  for Idx := 0 to Pred(fPages.Count) do
    FrameClasses[Idx] := TPrefsFrameClass(fPages[Idx]);
  Result := Execute(AOwner, FrameClasses);
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

function TPreferencesDlg.MapTabSheetToPage(
  const TabIdx: Integer): TPrefsBaseFrame;
  {Gets reference to preferences page frame associated with a tab index.
    @param TabIdx [in] Index of tab sheet for which page frame required.
    @return Reference to associated frame.
  }
begin
  Result := MapTabSheetToPage(pcMain.Pages[TabIdx]);
end;

function TPreferencesDlg.MapTabSheetToPage(
  const TabSheet: TTabSheet): TPrefsBaseFrame;
  {Gets reference to preferences page frame associated with a tab sheet.
    @param TabSheet [in] Tab sheet for which page frame required.
    @return Reference to associated frame.
  }
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

procedure TPreferencesDlg.pcMainMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  {Handles event triggered when user clicks on one of page control tabs. Ensures
  page control has focus. This does always happen automatically.
    @param Sender [in] Not used.
    @param Button [in] Not used.
    @param Shift [in] Not used.
    @param X [in] X co-ordinate of mouse in client co-ordinates.
    @param Y [in] Y co-ordinate of mouse in client co-ordinates.
  }
begin
  if htOnItem in pcMain.GetHitTestInfoAt(X, Y) then
    pcMain.SetFocus;
end;

class procedure TPreferencesDlg.RegisterPage(const FrameCls: TPrefsFrameClass);
  {Registers a preferences frame class for inclusion in the preferences dialog
  box. Registered frames are created when the dialog box is displayed and freed
  when it closes.
    @param FrameCls [in] Class of frame to be registered.
  }
var
  PageIdx: Integer; // loops through all registered frames
  InsIdx: Integer;  // place to insert new frame in list (per Index property)
begin
  // Ensure page list has been created and flagged for garbage collection
  if not Assigned(fPages) then
  begin
    fPages := TClassList.Create;
    TGC.GCLocalObj(fGC, fPages);
  end;
  // Search for place in frame list to insert new frame (sorted on frame's Index
  // property).
  InsIdx := fPages.Count;
  for PageIdx := 0 to Pred(fPages.Count) do
  begin
    if FrameCls.Index < TPrefsFrameClass(fPages[PageIdx]).Index then
    begin
      InsIdx := PageIdx;
      Break;
    end;
  end;
  // Record the frame's class reference
  fPages.Insert(InsIdx, FrameCls);
end;

end.

