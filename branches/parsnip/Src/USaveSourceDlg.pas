{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements customised Save dialog box for source code. Dialog has additional
 * controls to allow user to choose output file format, commenting style and
 * syntax highlighting.
}


unit USaveSourceDlg;


interface


uses
  // Delphi
  Classes,
  StdCtrls,
  ExtCtrls,
  // Project
  CS.SourceCode.Pascal.SourceGen,
  UEncodings,
  USaveDialogEx,
  USourceFileInfo;

type
  ///  <summary>Type of handler for events triggered by TSaveSourceDlg to check
  ///  if a file type supports syntax highlighting.</summary>
  ///  <param name="Sender">TObject [in] Object triggering event.</param>
  ///  <param name="Ext">string [in] Extension that defines type of file being
  ///  queried.</param>
  ///  <param name="CanHilite">Boolean [in/out] Set to true if file type
  ///  supports syntax highlighting.</param>
  THiliteQuery = procedure(Sender: TObject; const Ext: string;
    var CanHilite: Boolean) of object;

type
  ///  <summary>Type of handler for event triggered by TSaveSourceDlg to get
  ///  list of encodings supported for a file type.</summary>
  ///  <param name="Sender">TObject [in] Object triggering event.</param>
  ///  <param name="Ext">string [in] Extension that defines type of file being
  ///  queried.</param>
  ///  <param name="Encodings">TSourceFileEncodings [in/out] Assigned an array
  ///  of records that specify supported encodings.</param>
  TEncodingQuery = procedure(Sender: TObject; const Ext: string;
    var Encodings: TSourceFileEncodings) of object;

type
  ///  <summary>
  ///  Extended save dialog box used when saving code snippets. It displays
  ///  additional controls used to specify attributes of source code and which
  ///  encoding to use to save file. SAVESNIPPETEXT dialog resource. Dialog also
  ///  adjusts any file name entered without extension to include extension
  ///  associated with any current filter.
  ///  </summary>
  TSaveSourceDlg = class(TSaveDialogEx)
  strict private
    var
      ///  <summary>Panel thats hold controls added to dialog.</summary>
      fPanel: TPanel;
      ///  <summary>Label for comment style combo.</summary>
      fLblCommentStyle: TLabel;
      ///  <summary>Combo box used to select commenting style.</summary>
      fCmbCommentStyle: TComboBox;
      ///  <summary>Check box that determines whether a snippet's comment is
      ///  truncated to first paragraph of description or uses whole
      ///  description.</summary>
      fChkTruncateComment: TCheckBox;
      ///  <summary>Check box that toggles syntax highlighting.</summary>
      fChkSyntaxHilite: TCheckBox;
      ///  <summary>Label for encoding combo box.</summary>
      fLblEncoding: TLabel;
      ///  <summary>Combo box used to select encoding.</summary>
      fCmbEncoding: TComboBox;
      ///  <summary>Custom Help button added to dialog.</summary>
      fHelpBtn: TButton;
      ///  <summary>Preview button added to dialog.</summary>
      fPreviewBtn: TButton;
      ///  <summary>Style of commenting to be used in source code.</summary>
      fCommentStyle: TCommentStyle;
      ///  <summary>Whether snippet comments are to be truncated to a single
      ///  paragraph.</summary>
      fTruncateComments: Boolean;
      ///  <summary>Event handler for OnPreview event.</summary>
      fOnPreview: TNotifyEvent;
      ///  <summary>Event handler for OnHiliteQuery event.</summary>
      fOnHiliteQuery: THiliteQuery;
      ///  <summary>Event handler for OnEncodingQuery event.</summary>
      fOnEncodingQuery: TEncodingQuery;
      ///  <summary>Flags whether source is syntax highlighted.</summary>
      fUseSyntaxHiliting: Boolean;
      ///  <summary>Stores index of selected file type.</summary>
      fSelectedFilterIdx: Integer;
      ///  <summary>Stores type of selected encoding.</summary>
      fSelectedEncoding: TEncodingType;
    ///  <summary>Handles click on Help button.</summary>
    ///  <remarks>Calls help with required keyword.</remarks>
    procedure HelpClickHandler(Sender: TObject);
    ///  <summary>Handles click on preview button.</summary>
    ///  <remarks>Triggers dialog's OnPreview event.</remarks>
    procedure PreviewClickHandler(Sender: TObject);
    ///  <summary>Handles style combo box's OnChange event.</summary>
    ///  <remarks>Updates CommentStyle property per selected item in combo box.
    ///  </remarks>
    procedure CommentStyleChange(Sender: TObject);
    ///  <summary>Handles encoding combo box's OnChange event.</summary>
    ///  <remarks>Updates SelectedEncoding property per selected item in combo
    ///  box.</remarks>
    procedure EncodingChange(Sender: TObject);
    ///  <summary>Updates value of UseSyntaxHiliting property per state of
    ///  dialogue box controls.</summary>
    procedure UpdateSyntaxHiliting;
    ///  <summary>Updates value of TruncateComments property per state of
    ///  dialogue box controls.</summary>
    procedure UpdateCommentTruncation;
    ///  <summary>Selects item in comment style combo box that matches value of
    ///  CommentStyle property.</summary>
    procedure UpdateCommentStyle;
    ///  <summary>Adjusts any file name supplied without an extension by
    ///  appending extension associated with the currently selected file type.
    ///  </summary>
    ///  <remarks>If file has an extension or if file type is *.* then file name
    ///  is not changed.</remarks>
    ///  <param name="AFileName">string [in] File name to be adjusted.</param>
    ///  <returns>Adjusted file name.</returns>
    function AdjustFileName(const AFileName: string): string;
    ///  <summary>Returns index of a specified encoding type in the the encoding
    ///  dialog box.</summary>
    function IndexOfEncodingType(const EncType: TEncodingType): Integer;
    ///  <summary>Write accessor for OnPreview event.</summary>
    ///  <remarks>Enables / disables preview button depending on whether event
    ///  handler assigned.</remarks>
    procedure SetOnPreview(const Value: TNotifyEvent);
    ///  <summary>Read accessor for SelectedExt property.</summary>
    ///  <remarks>Extracts extension for selected file type from Filter
    ///  property.</remarks>
    function GetSelectedExt: string;
    ///  <summary>Read accessor for FilterIndex property.</summary>
    function GetFilterIndex: Integer;
    ///  <summary>Write accessor for FilterIndex property.</summary>
    procedure SetFilterIndex(const Value: Integer);
  strict protected
    ///  <summary>Tidies up and sets properties from controls when dialog is
    ///  about to close.</summary>
    procedure DoClose; override;
    ///  <summary>Called to check if dialog box can close. Returns True if so,
    ///  False if not.</summary>
    ///  <remarks>We check for existing file (after adding required extension)
    ///  and inhibit closure if file exists and user doesn't want to overwrite.
    ///  </remarks>
    function DoCanClose: Boolean; override;
    ///  <summary>Sets up dialog just before it is displayed.</summary>
    procedure DoShow; override;
    ///  <summary>Notifies when a different file type is selected in file type
    ///  combo box.</summary>
    ///  <remarks>We store newly selected filter and updated related controls.
    ///  </remarks>
    procedure DoTypeChange; override;
    ///  <summary>Notifies when a different encoding is selected in encoding
    ///  combo box.</summary>
    ///  <remarks>Records type of newly selected encoding.</remarks>
    procedure DoEncodingChange; virtual;
  public
    ///  <summary>Creates dialog box and adds custom controls to it.</summary>
    constructor Create(AOwner: TComponent); override;
    ///  <summary>Displays dialog box. Returns True if user OKs or False if user
    ///  cancels.</summary>
    ///  <remarks>Sets required dialog box template if new style dialogs being
    ///  used. Adjusts any extension-less file name to have extension of
    ///  selected file type.</remarks>
    function Execute: Boolean; override;
    ///  <summary>Extension relating to selected file type.</summary>
    property SelectedExt: string
      read GetSelectedExt;
    ///  <summary>Encoding type of selected encoding.</summary>
    property SelectedEncoding: TEncodingType
      read fSelectedEncoding;
    ///  <summary>Gets and sets the selected commenting style.</summary>
    property CommentStyle: TCommentStyle
      read fCommentStyle write fCommentStyle;
    ///  <summary>Gets and sets the value of the comment truncation check box.
    ///  </summary>
    property TruncateComments: Boolean
      read fTruncateComments write fTruncateComments;
    ///  <summary>Flag true if syntax highlighting is to be used when saving /
    ///  previewing source code. This is case when check box is checked and
    ///  selected file type supports highlighting. Set the property to check the
    ///  check box.</summary>
    property UseSyntaxHiliting: Boolean
      read fUseSyntaxHiliting write fUseSyntaxHiliting;
    ///  <summary>Event triggered when Preview button is clicked.</summary>
    ///  <remarks>Handlers should display the file that will be generated when
    ///  the Save button is clicked.</remarks>
    property OnPreview: TNotifyEvent
      read fOnPreview write SetOnPreview;
    ///  <summary>Event triggered when a file type is selected to determine
    ///  whether syntax highlighting is supported for the file type.</summary>
    property OnHiliteQuery: THiliteQuery
      read fOnHiliteQuery write fOnHiliteQuery;
    ///  <summary>Event triggered when a file type is selected to get a list of
    ///  encodings supported for the file type.</summary>
    property OnEncodingQuery: TEncodingQuery
      read fOnEncodingQuery write fOnEncodingQuery;
    ///  <summary>Re-implementation of inherited property to overcome apparent
    ///  bug where property forgets selected filter when dialog box is closed.
    ///  </summary>
    property FilterIndex: Integer read GetFilterIndex write SetFilterIndex;
  end;


implementation


uses
  // Delphi
  SysUtils,
  Forms,
  Dialogs,
  Dlgs,
  Controls,
  Windows,
  // Project
  UConsts,
  UMessageBox,
  UOpenDialogHelper,
  UStructs;


resourcestring
  // Component captions
  sLblCommentStyle = 'Comment style:';
  sLblEncoding = 'File Encoding:';
  sChkSyntaxHilite = 'Use syntax highlighting';
  sChkTruncateComment = 'Truncate comments to 1st paragraph';
  sBtnPreview = '&Preview...';
  sBtnHelp = '&Help';
  // Default encoding name
  sANSIEncoding = 'ANSI (Default)';


const
  // Name of dialog box template resource
  cTemplateName = 'SAVESNIPPETEXT';


{ TSaveSourceDlg }

function TSaveSourceDlg.AdjustFileName(const AFileName: string): string;
begin
  Result := AFileName;
  if ExtractFileExt(Result) = '' then
  begin
    if SelectedExt <> '.*' then
      Result := Result + SelectedExt;
  end;
end;

procedure TSaveSourceDlg.CommentStyleChange(Sender: TObject);
begin
  fCommentStyle := TCommentStyle(
    fCmbCommentStyle.Items.Objects[fCmbCommentStyle.ItemIndex]
  );
end;

constructor TSaveSourceDlg.Create(AOwner: TComponent);
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

  // label for encoding combo box
  fLblEncoding := TLabel.Create(Self);
  fLblEncoding.Parent := fPanel;
  fLblEncoding.Caption := sLblEncoding;

  // combo box used to select encoding
  fCmbEncoding := TComboBox.Create(Self);
  fCmbEncoding.Parent := fPanel;
  fCmbEncoding.Style := csDropDownList;
  fCmbEncoding.OnChange := EncodingChange;

  // label for comment style combo
  fLblCommentStyle := TLabel.Create(Self);
  fLblCommentStyle.Parent := fPanel;
  fLblCommentStyle.Caption := sLblCommentStyle;

  // check box that determines if comments are truncated to 1st paragraph
  fChkTruncateComment := TCheckBox.Create(Self);
  fChkTruncateComment.Parent := fPanel;
  fChkTruncateComment.Caption := sChkTruncateComment;

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

  // ---------------------------------------------------------------------------
  ///  <summary>Displays a dialog box asking permission to overwrite a file.
  ///  Returns True if user OKs, False if not.</summary>
  function QueryOverwrite(const FileName: string): Boolean;
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
begin
  UpdateSyntaxHiliting;
  UpdateCommentTruncation;
  inherited DoClose;
  // Hide any hint left on screen
  Application.HideHint;
end;

procedure TSaveSourceDlg.DoEncodingChange;
begin
  if fCmbEncoding.ItemIndex >= 0 then
    fSelectedEncoding := TEncodingType(
      fCmbEncoding.Items.Objects[fCmbEncoding.ItemIndex]
    )
  else
    fSelectedEncoding := etSysDefault;
end;

procedure TSaveSourceDlg.DoShow;

  // ---------------------------------------------------------------------------
  ///  <summary>Gets bounding rectangle of control with given id.</summary>
  function GetDlgCtrlRect(CtrlID: Integer): TRect;
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

  GetClientRect(Handle, PanelBounds);       // first size to whole client area
  PanelBounds.Top := StaticBounds.Bottom;   // set top to below hidden ctrl
  fPanel.BoundsRect := PanelBounds;
  fPanel.ParentWindow := Handle;            // make dlg parent of panel

  fChkSyntaxHilite.Left := FileTypeCmbBounds.Left;
  fChkSyntaxHilite.Top := 0;
  fChkSyntaxHilite.Width := FileTypeCmbBounds.Width;
  fChkSyntaxHilite.Checked := fUseSyntaxHiliting;

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

  fChkTruncateComment.Left := FileTypeCmbBounds.Left;
  fChkTruncateComment.Top := fCmbCommentStyle.Top + fCmbCommentStyle.Height + 8;
  fChkTruncateComment.Width := FileTypeCmbBounds.Width;
  fChkTruncateComment.Checked := fTruncateComments;

  fCmbEncoding.Left := FileTypeCmbBounds.Left;
  fCmbEncoding.Top := fChkTruncateComment.Top + fChkTruncateComment.Height + 8;
  fCmbEncoding.Width := FileTypeCmbBounds.Width * 3 div 4;

  fLblCommentStyle.Left := FileTypeLblBounds.Left;
  fLblCommentStyle.Top := fCmbCommentStyle.Top +
    (fCmbCommentStyle.Height - fLblCommentStyle.Height) div 2;

  fLblEncoding.Left := FileTypeLblBounds.Left;;
  fLblEncoding.Top := fCmbEncoding.Top +
    (fCmbEncoding.Height - fLblEncoding.Height) div 2;;

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
var
  CanHilite: Boolean; // flag true if syntax highlighting supported
  Encodings: TSourceFileEncodings;
  Encoding: TSourceFileEncoding;
begin
  // Update enabled state of syntax highlighter checkbox
  CanHilite := False;
  if Assigned(fOnHiliteQuery) then
    fOnHiliteQuery(Self, SelectedExt, CanHilite);
  fChkSyntaxHilite.Enabled := CanHilite;

  // Store selected type
  fSelectedFilterIdx := inherited FilterIndex;

  // Update list of available encodings (just ANSI default if caller doesn't
  // handle OnEncodingQuery)
  SetLength(Encodings, 0);
  if Assigned(fOnEncodingQuery) then
    fOnEncodingQuery(Self, SelectedExt, Encodings);
  if Length(Encodings) = 0 then
    Encodings := TSourceFileEncodings.Create(
      TSourceFileEncoding.Create(etSysDefault, sANSIEncoding)
    );
  fCmbEncoding.Clear;
  for Encoding in Encodings do
  begin
    fCmbEncoding.Items.AddObject(
      Encoding.DisplayName, TObject(Encoding.EncodingType)
    );
  end;
  fCmbEncoding.ItemIndex := IndexOfEncodingType(fSelectedEncoding);
  if fCmbEncoding.ItemIndex = -1 then
    fCmbEncoding.ItemIndex := 0;
  DoEncodingChange;

  inherited;
end;

procedure TSaveSourceDlg.EncodingChange(Sender: TObject);
begin
  DoEncodingChange;
end;

function TSaveSourceDlg.Execute: Boolean;
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
begin
  if Handle <> 0 then
    // dialog box is open: use inherited FilterIndex property
    Result := inherited FilterIndex
  else
    // dialog box is closed: use recorded index to overcome apparent bug
    Result := fSelectedFilterIdx;
end;

function TSaveSourceDlg.GetSelectedExt: string;
begin
  Result := FilterIndexToExt(Self);
end;

procedure TSaveSourceDlg.HelpClickHandler(Sender: TObject);
begin
  DisplayHelp;
end;

function TSaveSourceDlg.IndexOfEncodingType(
  const EncType: TEncodingType): Integer;
var
  Idx: Integer;
begin
  for Idx := 0 to Pred(fCmbEncoding.Items.Count) do
  begin
    if TEncodingType(fCmbEncoding.Items.Objects[Idx]) = EncType then
      Exit(Idx);
  end;
  Result := -1;
end;

procedure TSaveSourceDlg.PreviewClickHandler(Sender: TObject);
begin
  UpdateSyntaxHiliting;
  UpdateCommentTruncation;
  if Assigned(fOnPreview) then
    fOnPreview(Self);
end;

procedure TSaveSourceDlg.SetFilterIndex(const Value: Integer);
begin
  // record index in inherited property
  inherited FilterIndex := Value;
  // also record filter in own field: used to overcome apparent bug
  fSelectedFilterIdx := Value;
end;

procedure TSaveSourceDlg.SetOnPreview(const Value: TNotifyEvent);
begin
  fOnPreview := Value;
  fPreviewBtn.Enabled := Assigned(Value);
end;

procedure TSaveSourceDlg.UpdateCommentStyle;
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

procedure TSaveSourceDlg.UpdateCommentTruncation;
begin
  fTruncateComments := fChkTruncateComment.Checked;
end;

procedure TSaveSourceDlg.UpdateSyntaxHiliting;
begin
  fUseSyntaxHiliting := fChkSyntaxHilite.Checked;
end;

end.

