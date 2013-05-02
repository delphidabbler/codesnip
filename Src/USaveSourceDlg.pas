{
 * USaveSourceDlg.pas
 *
 * Implements customised Save dialog box for source code. Dialog has additional
 * controls to allow user to choose output file format, commenting style and
 * syntax highlighting.
 *
 * Unit originally named USaveSnippetDlg.pas. Renamed as USaveSourceDlg.pas as
 * at v1.1.
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
 * The Original Code is USaveSourceDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit USaveSourceDlg;


interface


uses
  // Delphi
  Classes, StdCtrls, ExtCtrls,
  // Project
  USaveDialogEx, USourceGen;


type

  {
  THiliteQuery:
    Type of handler for event triggered by TSaveSourceDlg to check if a file
    type supports syntax highlighting.
      @param Sender [in] TSaveSourceDlg dialog object triggering event.
      @param Ext [in] Extension of file type being checked.
      @param CanHilite [in/out] Flag that handler sets true if syntax
        highlighting supported (default value is False).
  }
  THiliteQuery = procedure(Sender: TObject; const Ext: string;
    var CanHilite: Boolean) of object;

  {
  TSaveSourceDlg:
    Extended save dialog box used when saving code snippets. It displays
    additional controls used to specify attributes of source code. Requires
    SAVESNIPPETEXT dialog resource. Dialog also adjusts any file name entered
    without extension to include extension associated with any current filter.
  }
  TSaveSourceDlg = class(TSaveDialogEx)
  strict private
    fPanel: TPanel;               // Panel thats hold controls added to dialog
    fLblCommentStyle: TLabel;     // Label for comment style combo
    fCmbCommentStyle: TComboBox;  // Combo box used to select commenting style
    fChkSyntaxHilite: TCheckBox;  // Check box that toggles syntax highlighting
    fHelpBtn: TButton;            // Help button added to dialog
    fPreviewBtn: TButton;         // Preview button added to dialog
    fCommentStyle: TCommentStyle; // Style of commenting to be used source code
    fOnPreview: TNotifyEvent;     // Event handler for OnPreview event
    fOnHiliteQuery: THiliteQuery; // Event handler for OnHiliteQuery event
    fUseSyntaxHiliting: Boolean;  // Flags whether source is syntax highlighted
    fSelectedFilterIdx: Integer;  // Store index of selected file type.
    procedure HelpClickHandler(Sender: TObject);
      {Handles click on help button. Calls help with required keyword.
        @param Sender [in] Not used.
      }
    procedure PreviewClickHandler(Sender: TObject);
      {Handles click on preview button. Updates properties then triggers
      dialog's OnPreview event.
        @param Sender [in] Not used.
      }
    procedure SetOnPreview(const Value: TNotifyEvent);
      {Write accessor for OnPreview event. Enables / disables preview button
      depending on if event handler assigned.
        @param Value [in] Required event handler.
      }
    procedure UpdateSyntaxHiliting;
      {Updates value of UseSyntaxHiliting property per state of dialog box
      controls.
      }
    procedure UpdateCommentStyle;
      {Selects appropriate item in comment style combo box per current comment
      style property.
      }
    procedure CommentStyleChange(Sender: TObject);
      {Comment style combo box OnChange event handler. Updates CommentStyle
      property.
        @param Sender [in] Not used.
      }
    function GetSelectedExt: string;
      {Read accessor for SelectedExt property. Extracts extension for selected
      file type from Filter property.
        @return Selected extension with leading '.'.
      }
    function AdjustFileName(const AFileName: string): string;
      {Adjusts any file name supplied without an extension by appending
      extension associated with the currently selected file type. If the file
      has an extension or if the file type is *.* then the file name is not
      changed.
        @param AFileName [in] File name to be adjusted.
        @return Adjusted file name.
      }
    function GetFilterIndex: Integer;
      {Gets index of selected filter.
        @return Selected filter index.
      }
    procedure SetFilterIndex(const Value: Integer);
      {Records index of selected file filter.
        @param Value [in] New value of selected file filter.
      }
  strict protected
    procedure DoClose; override;
      {Tidies up and sets properties from controls when dialog is about to
      close.
      }
    function DoCanClose: Boolean; override;
      {Called to check if dialog can close. We check for existing file (after
      adding required extension) and inhibit closure if file exists and user
      doesn't want to overwrite.
        @return True if dialog can close and false otherwise.
      }
    procedure DoShow; override;
      {Sets up dialog just before it is displayed.
      }
    procedure DoTypeChange; override;
      {Notifies when a different file type is selected in file type combo box.
      Checks if syntax highlighting supported for selected file type.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Creates dialog box and adds custom controls to it.
        @param AOwner [in] Owning component. Dialog box will be aligned over
          AOwner.
      }
    function Execute: Boolean; override;
      {Displays dialog box. Sets required dialog box template if new style
      dialogs being used. Adjusts any extension-less file name to have extension
      of selected file type.
        @return True if user OKs and false if cancels.
      }
    property SelectedExt: string
      read GetSelectedExt;
      {Extension relating to selected file type}
  published
    property CommentStyle: TCommentStyle
      read fCommentStyle write fCommentStyle;
      {Commenting style selected in dialog box. Determines default comment style
      selection in dialog box when set}
    property UseSyntaxHiliting: Boolean
      read fUseSyntaxHiliting write fUseSyntaxHiliting;
      {Flag true if syntax highlighting is to be used when saving / previewing
      the source code. This is case when check box is checked and selected file
      type supports highlighting. Set the property to check the box}
    property OnPreview: TNotifyEvent
      read fOnPreview write SetOnPreview;
      {Event triggered when preview button clicked. Handlers should display file
      that will be generated when Save button is clicked}
    property OnHiliteQuery: THiliteQuery
      read fOnHiliteQuery write fOnHiliteQuery;
      {Event triggered when a file type is selected. Handlers should determine
      if the file type supports syntax highlighting}
    property FilterIndex: Integer read GetFilterIndex write SetFilterIndex;
      {Re-implementation of inherited property. Used to overcome apparent bug
      where property forgets selected filter when dialog box is closed}
  end;


implementation


uses
  // Delphi
  SysUtils, Forms, Dialogs, Dlgs, Controls, Windows,
  // Project
  UConsts, UMessageBox, UOpenDialogHelper, UStructs;


resourcestring
  // Component captions
  sLblCommentStyle = 'Comment style:';
  sChkSyntaxHilite = 'Use syntax highlighting';
  sBtnPreview = '&Preview...';
  sBtnHelp = '&Help';

  
const
  // Name of dialog box template resource
  cTemplateName = 'SAVESNIPPETEXT';


{ TSaveSourceDlg }

function TSaveSourceDlg.AdjustFileName(const AFileName: string): string;
  {Adjusts any file name supplied without an extension by appending extension
  associated with the currently selected file type. If the file has an extension
  or if the file type is *.* then the file name is not changed.
    @param AFileName [in] File name to be adjusted.
    @return Adjusted file name.
  }
begin
  Result := AFileName;
  if ExtractFileExt(Result) = '' then
  begin
    if SelectedExt <> '.*' then
      Result := Result + SelectedExt;
  end;
end;

procedure TSaveSourceDlg.CommentStyleChange(Sender: TObject);
  {Comment style combo box OnChange event handler. Updates CommentStyle
  property.
    @param Sender [in] Not used.
  }
begin
  fCommentStyle := TCommentStyle(
    fCmbCommentStyle.Items.Objects[fCmbCommentStyle.ItemIndex]
  );
end;

constructor TSaveSourceDlg.Create(AOwner: TComponent);
  {Class constructor. Creates dialog box and adds custom controls to it.
    @param AOwner [in] Owning component. Dialog box will be aligned over AOwner.
  }
begin
  inherited Create(AOwner);

  // Create additional controls
  // (Order of creation is important - tab order is reverse of creation order)

  // panel that stores all extra controls
  fPanel := TPanel.Create(Self);
  fPanel.Caption := '';
  fPanel.BevelOuter := bvNone;
  fPanel.TabStop := True;

  // custom help button
  fHelpBtn := TButton.Create(Self);
  fHelpBtn.Parent := fPanel;
  fHelpBtn.OnClick := HelpClickHandler;

  // preview button
  fPreviewBtn := TButton.Create(Self);
  fPreviewBtn.Parent := fPanel;
  fPreviewBtn.OnClick := PreviewClickHandler;

  // label for comment style combo
  fLblCommentStyle := TLabel.Create(Self);
  fLblCommentStyle.Parent := fPanel;
  fLblCommentStyle.Caption := sLblCommentStyle;

  // combo box used to select commenting style
  fCmbCommentStyle := TComboBox.Create(Self);
  fCmbCommentStyle.Parent := fPanel;
  fCmbCommentStyle.Style := csDropDownList;
  fCmbCommentStyle.OnChange := CommentStyleChange;

  // check box used to determine whether to use syntax highlighting
  fChkSyntaxHilite := TCheckBox.Create(Self);
  fChkSyntaxHilite.Parent := fPanel;
  fChkSyntaxHilite.Caption := sChkSyntaxHilite;

  // Set dialog box properties

  // set default button values and states
  SetOnPreview(nil);  // updates state of preview button

  // set dialog options
  Options := [ofPathMustExist, ofEnableIncludeNotify];

  // inhibit default help processing: we provide own help button and handling
  WantDefaultHelpSupport := False;
end;

function TSaveSourceDlg.DoCanClose: Boolean;
  {Called to check if dialog can close. We check for existing file (after adding
  required extension) and inhibit closure if file exists and user doesn't want
  to overwrite.
    @return True if dialog can close and false otherwise.
  }

  // ---------------------------------------------------------------------------
  function QueryOverwrite(const FileName: string): Boolean;
    {Displays a dialog box asking permission to overwrite a file.
      @param FileName [in] Name of file to be overwritten.
      @return True if user agrees that file can be overwritten, false if not.
    }
  resourcestring
    // Text of query displayed in dialog box
    sQueryMsg = '%s already exists.' + EOL + 'Do you want to replace it?';
  begin
    Result := TMessageBox.Confirm(Self, Format(sQueryMsg, [FileName]));
  end;
  // ---------------------------------------------------------------------------

var
  AFileName: string;  // Current file name, adjusted to ensure it has extension
begin
  // Check if user wants to inhibit closure by triggering OnCanClose event
  Result := inherited DoCanClose;
  if not Result then
    Exit;
  // Ask user for permission to overwrite any existing file
  AFileName := AdjustFileName(FileOpenEditedFileName(Self));
  if FileExists(AFileName) and not QueryOverwrite(AFileName) then
    Result := False;
end;

procedure TSaveSourceDlg.DoClose;
  {Tidies up and sets properties from controls when dialog is about to close.
  }
begin
  // Update value of SyntaxHiliting property
  UpdateSyntaxHiliting;
  inherited DoClose;
  // Hide any hint left on screen
  Application.HideHint;
end;

procedure TSaveSourceDlg.DoShow;
  {Sets up dialog just before it is displayed.
  }

  // ---------------------------------------------------------------------------
  function GetDlgCtrlRect(CtrlID: Integer): TRect;
    {Get bounding rectangle of control with given id.
      @return Bounding rectangle.
    }
  begin
    // Get bounds in screen co-ords (controls are children of parent window)
    GetWindowRect(GetDlgItem(GetParent(Handle), CtrlID), Result);
    // Map co-ords to this window
    MapWindowPoints(0, Handle, Result, 2);
  end;
  // ---------------------------------------------------------------------------

var
  FileTypeCmbBounds: TRectEx; // bounds of dlg's filetype combo box
  FileTypeLblBounds: TRect;   // bounds of dlg's filetype label
  ButtonBounds: TRect;        // bounds of one of dlg's buttons
  StaticBounds: TRect;        // bounds of dlg's hidden ctrl (per dlg resource)
  PanelBounds: TRect;         // bounds of panel we add to dlg
  CSIdx: TCommentStyle;       // loops thru comment styles
begin
  // Get bounding rectangle of various dialog box controls
  // bounds of hidden static text control per our custom dialog resource
  StaticBounds := inherited GetStaticRect;
  // bounds of file type combo and associated text (used to align added ctrls)
  FileTypeCmbBounds := GetDlgCtrlRect(cmb1);
  FileTypeLblBounds := GetDlgCtrlRect(stc2);
  // bounds of OK button (used to size buttons we add)
  ButtonBounds := GetDlgCtrlRect(IDOK);

  // Set up TPanel that holds all newly added controls
  GetClientRect(Handle, PanelBounds);       // first size to whole client area
  PanelBounds.Top := StaticBounds.Bottom;   // set top to below hidden ctrl
  fPanel.BoundsRect := PanelBounds;
  fPanel.ParentWindow := Handle;            // make dlg parent of panel

  // Align syntax highlight check box file type combo
  fChkSyntaxHilite.Left := FileTypeCmbBounds.Left;
  fChkSyntaxHilite.Top := 0;
  fChkSyntaxHilite.Width := FileTypeCmbBounds.Width;
  fChkSyntaxHilite.Checked := fUseSyntaxHiliting;

  // Size and align comment style combo box with file type combo box
  fCmbCommentStyle.Left := FileTypeCmbBounds.Left;
  fCmbCommentStyle.Top := fChkSyntaxHilite.Top + fChkSyntaxHilite.Height + 6;
  fCmbCommentStyle.Width := FileTypeCmbBounds.Width;
  // Set up combo box text (can't be done in constructor since a parent window
  // is required and host panel doesn't have one at that point)
  if fCmbCommentStyle.Items.Count = 0 then
  begin
    // Populate comment style combo
    for CSIdx := Low(TCommentStyle) to High(TCommentStyle) do
      fCmbCommentStyle.Items.AddObject(
        TSourceComments.CommentStyleDesc(CSIdx), TObject(CSIdx)
      );
  end;
  UpdateCommentStyle;

  // Align comment style label within group box
  fLblCommentStyle.Left := FileTypeLblBounds.Left;
  fLblCommentStyle.Top := fCmbCommentStyle.Top +
    (fCmbCommentStyle.Height - fLblCommentStyle.Height) div 2;

  // Size preview button and align under above buttons
  fPreviewBtn.BoundsRect := ButtonBounds;
  fPreviewBtn.Top := 0;
  fPreviewBtn.Caption := sBtnPreview;

  // Size help button, enable/disable as required & align under preview buttons
  fHelpBtn.BoundsRect := ButtonBounds;
  fHelpBtn.Top := fPreviewBtn.Top + fPreviewBtn.Height + 6;
  fHelpBtn.Caption := sBtnHelp;
  fHelpBtn.Enabled := (HelpKeyword <> '');

  // Call this to ensure we trigger type change event for default file type
  DoTypeChange;

  inherited;
end;

procedure TSaveSourceDlg.DoTypeChange;
  {Notifies when a different file type is selected in file type combo box.
  Checks if syntax highlighting supported for selected file type.
  }
var
  CanHilite: Boolean; // flag true if syntax highlighting supported
begin
  // Assume highlighting not supported
  CanHilite := False;
  // Trigger event where user specifies if highlighting supported
  if Assigned(fOnHiliteQuery) then
    fOnHiliteQuery(Self, SelectedExt, CanHilite);
  // Enable / disable highlighting check box as required
  fChkSyntaxHilite.Enabled := CanHilite;
  fSelectedFilterIdx := inherited FilterIndex;
  inherited;
end;

function TSaveSourceDlg.Execute: Boolean;
  {Displays dialog box. Sets required dialog box template if new style dialogs
  being used. Adjusts any extension-less file name to have extension of selected
  file type.
    @return True if user OKs and false if cancels.
  }
begin
  // Set up template for customisation
  if NewStyleControls and not (ofOldStyleDialog in Options) then
    Template := cTemplateName
  else
    Template := nil;
  // Display dialog box
  Result := inherited Execute;
  if Result then
    // Adjust file name, adding any missing extension
    FileName := AdjustFileName(FileName);
end;

function TSaveSourceDlg.GetFilterIndex: Integer;
  {Gets index of selected filter.
    @return Selected filter index.
  }
begin
  if Handle <> 0 then
    // dialog box is open: use inherited FilterIndex property
    Result := inherited FilterIndex
  else
    // dialog box is closed: use recorded index to overcome apparent bug
    Result := fSelectedFilterIdx;
end;

function TSaveSourceDlg.GetSelectedExt: string;
  {Read accessor for SelectedExt property. Extracts extension for selected file
  type from Filter property.
    @return Selected extension with leading '.'.
  }
begin
  Result := FilterIndexToExt(Self);
end;

procedure TSaveSourceDlg.HelpClickHandler(Sender: TObject);
  {Handles click on help button. Calls help with required keyword.
    @param Sender [in] Not used.
  }
begin
  DisplayHelp;
end;

procedure TSaveSourceDlg.PreviewClickHandler(Sender: TObject);
  {Handles click on preview button. Updates properties then triggers OnPreview
  event.
    @param Sender [in] Not used.
  }
begin
  UpdateSyntaxHiliting;
  if Assigned(fOnPreview) then
    fOnPreview(Self);
end;

procedure TSaveSourceDlg.SetFilterIndex(const Value: Integer);
  {Records index of selected file filter.
    @param Value [in] New value of selected file filter.
  }
begin
  // record index in inherited property
  inherited FilterIndex := Value;
  // also record filter in own field: used to overcome apparent bug
  fSelectedFilterIdx := Value;
end;

procedure TSaveSourceDlg.SetOnPreview(const Value: TNotifyEvent);
  {Write accessor for OnPreview event. Enables / disables preview button
  depending on if event handler assigned.
    @param Value [in] Required event handler.
  }
begin
  fOnPreview := Value;
  fPreviewBtn.Enabled := Assigned(Value);
end;

procedure TSaveSourceDlg.UpdateCommentStyle;
  {Selects appropriate item in comment style combo box per current comment style
  property.
  }
var
  Idx: Integer; // loops thru combo box items
begin
  for Idx := 0 to Pred(fCmbCommentStyle.Items.Count) do
  begin
    // comment style stored in Objects[] property
    if TCommentStyle(fCmbCommentStyle.Items.Objects[Idx]) = fCommentStyle then
      fCmbCommentStyle.ItemIndex := Idx;
  end;
end;

procedure TSaveSourceDlg.UpdateSyntaxHiliting;
  {Updates value of UseSyntaxHiliting property per state of dialog box controls.
  }
begin
  fUseSyntaxHiliting := fChkSyntaxHilite.Checked;
end;

end.

