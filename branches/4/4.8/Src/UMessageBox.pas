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
 * Implements a static class that can display message and confirmation dialogue
 * boxes.
}


unit UMessageBox;


interface


uses
  // Delphi
  Classes, Controls, Dialogs,
  // Project
  UBaseObjects, UIStringList;


resourcestring
  // Button captions
  sBtnYes = '&Yes';       // Yes button caption
  sBtnNo = '&No';         // No button caption
  sBtnOK = 'OK';          // OK button caption
  sBtnCancel = 'Cancel';  // Cancel button caption


type
  ///  <summary>Attributes of a TMessageBox button.</summary>
  TMessageBoxButton = record
    ///  <summary>Button's caption.</summary>
    Caption: TCaption;
    ///  <summary>Button's modal result.</summary>
    ///  <remarks>Set to 0 to prevent button from closing form.</remarks>
    ModalResult: Integer;
    ///  <summary>Determines if this is default button.</summary>
    Default: Boolean;
    ///  <summary>Determines if this is cancel button.</summary>
    Cancel: Boolean;
    ///  <summary>Creates record with given fields values.</summary>
    constructor Create(const ACaption: TCaption; const AModalResult: Integer;
      const ADefault: Boolean = False; const ACancel: Boolean = False);
  end;

type
  ///  <summary>Static class that can display message and confirmation dialogue
  ///  boxes at an appropriate position on screen.</summary>
  TMessageBox = class sealed(TNoConstructObject)
  public
    const
      ///  <summary>Value used to request default dialogue box title.</summary>
      ///  <remarks>For passing to methods that take a Title parameter when
      ///  default title is required.</remarks>
      DefaultTitle = '';

      ///  <summary>Value used to request default dialogue box icon.</summary>
      ///  <remarks>For passing to methods that take an IconRes parameter when
      ///  default icon is required.</remarks>
      DefaultIcon = nil;

  strict private

    ///  <summary>Displays a message in a customised dialogue box aligned over
    ///  the parent control.</summary>
    ///  <param name="Parent">TComponent [in] Dialogue box's parent control,
    ///  over which dialogue box is aligned. May be nil, when active form is
    ///  used for alignment.</param>
    ///  <param name="MsgLines">IStringList [in] Message displayed in dialogue
    ///  box. Can store mulitple lines. Use empty lines to insert space between
    ///  paragraphs.</param>
    ///  <param name="DlgType">TMsgDlgType [in] Type of dialogue box.</param>
    ///  <param name="Buttons">array of TMessageBoxButton [in] Describes buttons
    ///  to be displayed.</param>
    ///  <param name="Title">string [in] Title of dialogue box. If Title = ''
    ///  the title used depends on DlgType.</param>
    ///  <param name="IconRes">PChar [in] Icon resource identifier. If nil icon
    ///  used depends on DlgType.</param>
    ///  <param name="InhibitCancel">Boolean [in] Flag that, when true, removes
    ///  close button from dialogue box and prevents it responding to the ESC
    ///  key.</param>
    ///  <returns>Word. Value indicating which button was pressed to close
    ///  dialogue box. This will be value of ModalResult field of one of the
    ///  buttons specified in Buttons, or mrCancel if close button in title bar
    ///  was clicked.</returns>
    class function Display(const Parent: TComponent;
      MsgLines: IStringList; const DlgType: TMsgDlgType;
      const Buttons: array of TMessageBoxButton; const Title: string;
      const IconRes: PChar; const InhibitCancel: Boolean): Word; overload;

    ///  <summary>Displays a message in a customised dialogue box aligned over
    ///  the parent control.</summary>
    ///  <param name="Parent">TComponent [in] Dialogue box's parent control,
    ///  over which dialogue box is aligned. May be nil, when active form is
    ///  used for alignment.</param>
    ///  <param name="Msg">string [in] Message displayed in dialogue box.
    ///  Separate lines with LF or CRLF. Separate paragraphs with two line
    ///  breaks.</param>
    ///  <param name="DlgType">TMsgDlgType [in] Type of dialogue box.</param>
    ///  <param name="Buttons">array of TMessageBoxButton [in] Describes buttons
    ///  to be displayed.</param>
    ///  <param name="Title">string [in] Title of dialogue box. If Title = ''
    ///  the title used depends on DlgType.</param>
    ///  <param name="IconRes">PChar [in] Icon resource identifier. If nil icon
    ///  used depends on DlgType.</param>
    ///  <param name="InhibitCancel">Boolean [in] Flag that, when true, removes
    ///  close button from dialogue box and prevents it responding to the ESC
    ///  key.</param>
    ///  <returns>Word. Value indicating which button was pressed to close
    ///  dialogue box. This will be value of ModalResult field of one of the
    ///  buttons specified in Buttons, or mrCancel if close button in title bar
    ///  was clicked.</returns>
    class function Display(const Parent: TComponent; const Msg: string;
      const DlgType: TMsgDlgType; const Buttons: array of TMessageBoxButton;
      const Title: string; const IconRes: PChar; const InhibitCancel: Boolean):
      Word; overload;

  public

    ///  <summary>Displays a message in an information dialogue box aligned over
    ///  the parent control.</summary>
    ///  <param name="Parent">TComponent [in] Dialogue box's parent control,
    ///  over which dialogue box is aligned. May be nil, when active form is
    ///  used for alignment.</param>
    ///  <param name="Msg">string [in] Message displayed in dialogue box.
    ///  Separate lines with LF or CRLF. Separate paragraphs with two line
    ///  breaks.</param>
    class procedure Information(const Parent: TComponent; const Msg: string);

    ///  <summary>Displays a message in an error dialogue box aligned over the
    ///  parent control.</summary>
    ///  <param name="Parent">TComponent [in] Dialogue box's parent control,
    ///  over which dialogue box is aligned. May be nil, when active form is
    ///  used for alignment.</param>
    ///  <param name="Msg">string [in] Message displayed in dialogue box.
    ///  Separate lines with LF or CRLF. Separate paragraphs with two line
    ///  breaks.</param>
    class procedure Error(const Parent: TComponent; const Msg: string);

    ///  <summary>Displays a message in a confirmation dialogue box aligned over
    ///  the parent control.</summary>
    ///  <param name="Parent">TComponent [in] Dialogue box's parent control,
    ///  over which dialogue box is aligned. May be nil, when active form is
    ///  used for alignment.</param>
    ///  <param name="Msg">string [in] Message displayed in dialogue box.
    ///  Separate lines with LF or CRLF. Separate paragraphs with two line
    ///  breaks.</param>
    ///  <returns>Boolean. True if user confirms by clicking Yes button and
    ///  False if No is clicking or dialogue is cancelled.</returns>
    class function Confirm(const Parent: TComponent;
      const Msg: string): Boolean;

    ///  <summary>Displays a message in a dialogue box with custom buttons,
    ///  aligned over the parent control.</summary>
    ///  <remarks>This dialogue box has no default icon.</remarks>
    ///  <param name="Parent">TComponent [in] Dialogue box's parent control,
    ///  over which dialogue box is aligned. May be nil, when active form is
    ///  used for alignment.</param>
    ///  <param name="Msg">string [in] Message displayed in dialogue box.
    ///  Separate lines with LF or CRLF. Separate paragraphs with two line
    ///  breaks.</param>
    ///  <param name="Buttons">array of TMessageBoxButton [in] Describes buttons
    ///  to be displayed.</param>
    ///  <param name="Title">string [in] Title of dialogue box. If Title = ''
    ///  the application's title is used.</param>
    ///  <param name="IconRes">PChar [in] Icon resource identifier. If nil then
    ///  no icon is displayed.</param>
    ///  <param name="InhibitCancel">Boolean [in] Flag that, when true, removes
    ///  close button from dialogue box and prevents it responding to the ESC
    ///  key.</param>
    ///  <returns>Word. Value indicating which button was pressed to close
    ///  dialogue box. This will be value of ModalResult field of one of the
    ///  buttons specified in Buttons, or mrCancel if close button in title bar
    ///  was clicked.</returns>
    class function Custom(const Parent: TComponent; const Msg: string;
      const Buttons: array of TMessageBoxButton;
      const Title: string = DefaultTitle; const IconRes: PChar = DefaultIcon;
      const InhibitCancel: Boolean = False):
      Word;
  end;


implementation


uses
  // Delphi
  SysUtils, Windows, Forms, StdCtrls, ExtCtrls, Consts, Math,
  // Project
  UConsts, UDlgHelper, UFontHelper, UGraphicUtils, UStructs, UStrUtils;


type
  ///  <summary>Implements a customisable message dialogue box.</summary>
  TMessageBoxForm = class sealed(TForm)
  strict private
    var
      ///  <summary>Image control used to display any icon.</summary>
      fImage: TImage;
      ///  <summary>Array of dialogue box's buttons.</summary>
      fButtons: array of TButton;
      ///  <summary>Array of lables that display dialogue box's text.</summary>
      fLabels: array of TLabel;
    const
      ///  <summary>Left and right dialogue box margins.</summary>
      cXPadding = 16;
      ///  <summary>Top and bottom dialogue box margins.</summary>
      cYPadding = 16;
      ///  <summary>Spacing between paragraphs.</summary>
      cParaSpacing = 8;
      ///  <summary>Spacing between buttons.</summary>
      cBtnSpacing = 8;
      ///  <summary>Margin above button strip.</summary>
      cBtnStripTopMargin = 24;
      ///  <summary>Maximum width of text in dialogue box.</summary>
      cMaxTextWidth = 512;
  strict private
    ///  <summary>Calculates maximum horizontal space that can be occupied by
    ///  the dialogue's text.</summary>
    function MaxTextWidth: Integer;
    ///  <summary>Calculates height required to display all labels.</summary>
    function LabelsHeight: Integer;
    ///  <summary>Calculates height of given label.</summary>
    function LabelHeight(const Lbl: TLabel): Integer;
    ///  <summary>Calculates width of text in widest label.</summary>
    function LabelsWidth: Integer;
    ///  <summary>Calculates vertical space required to display button strip.
    ///  </summary>
    function ButtonsHeight: Integer;
    ///  <summary>Calculates horizontal space required to display button strip.
    ///  </summary>
    function ButtonsWidth: Integer;
    ///  <summary>Calculates offset of text from dialogue box's left hand
    ///  margin.</summary>
    ///  <remarks>Depends on whether dialogue box displays an icon.</remarks>
    function TextLeftMargin: Integer;
    ///  <summary>Calculates vertical space required to accommodate text and any
    ///  icon.</summary>
    function TextAndIconHeight: Integer;
    ///  <summary>Calculates horizontal spaces required to accommodate text and
    ///  any icon.</summary>
    function TextAndIconWidth: Integer;
    ///  <summary>Sets text of dialogue box's caption.</summary>
    ///  <param name="Title">string [in] Caption text. If '' then a default
    ///  caption based on DlgType is used.</param>
    ///  <param name="DlgType">TMsgDlgType [in] Type of dialogue box. Determines
    ///  any default title.</param>
    procedure InitCaption(const Title: string; const DlgType: TMsgDlgType);
    ///  <summary>Creates and initialises and locates image control that is
    ///  required to display any icon.</summary>
    ///  <param name="IconRes">PChar [in] Name of icon resource. If nil then
    ///  icon is determined by DlgType.</param>
    ///  <param name="DlgType">TMsgDlgType [in] Type of dialogue box. Used to
    ///  determine default icon resource.</param>
    ///  <remarks>If no icon resource is provided for custom dialogue boxes then
    ///  no image control is created.</remarks>
    procedure InitImage(const IconRes: PChar; const DlgType: TMsgDlgType);
    ///  <summary>Creates and sizes dialogue's buttons.</summary>
    ///  <param name="Buttons">array of TMessageBoxButton [in] Array of records
    ///  that describe buttons to be displayed.</param>
    procedure InitButtons(const Buttons: array of TMessageBoxButton);
    ///  <summary>Creates and sizes labels used to display text in dialogue box.
    ///  </summary>
    ///  <param name="Lines">IStringList [in] List od lines of text to display.
    ///  Blank lines indicate a paragraph break.</param>
    procedure InitLabels(const Lines: IStringList);
    ///  <summary>Sets size of dialogue box to accomodate text, optional icon
    ///  and buttons.</summary>
    procedure SizeDialogBox;
    ///  <summary>Arranges all controls within dialogue box.</summary>
    procedure ArrangeControls;
  public
    ///  <summary>Constructs form dialogue box form and all components required
    ///  to display text, buttons and optional icon.</summary>
    ///  <param name="Owner">TComponent [in] Control that owns this form.
    ///  </param>
    ///  <param name="Text">IStringList [in] List of lines of text to display.
    ///  Blank lines indicate a paragraph spacer.</param>
    ///  <param name="DlgType">TMsgDlgType [in] Type of dialogue box.</param>
    ///  <param name="Buttons">array of TMessageBoxButton [in] Describes buttons
    ///  to be displayed.</param>
    ///  <param name="Title">string [in] Title of dialogue box. If Title = ''
    ///  the title used depends on DlgType.</param>
    ///  <param name="IconRes">PChar [in] Icon resource identifier. If nil icon
    ///  used depends on DlgType.</param>
    ///  <param name="InhibitCancel">Boolean [in] Flag that, when true, removes
    ///  close button from dialogue box and prevents it responding to the ESC
    ///  key.</param>
    constructor Create(const Owner: TComponent; const Text: IStringList;
      const Buttons: array of TMessageBoxButton; const DlgType: TMsgDlgType;
      const Title: string; const IconRes: PChar; const InhibitCancel: Boolean);
      reintroduce;
  end;

{ TMessageBox }

class function TMessageBox.Confirm(const Parent: TComponent;
  const Msg: string): Boolean;
begin
  Result := Display(
    Parent,
    Msg,
    mtConfirmation,
    [
      TMessageBoxButton.Create(sBtnYes, mrYes, True),
      TMessageBoxButton.Create(sBtnNo, mrNo, False, True)
    ],
    DefaultTitle,
    DefaultIcon,
    False
  ) = mrYes;
end;

class function TMessageBox.Custom(const Parent: TComponent; const Msg: string;
  const Buttons: array of TMessageBoxButton; const Title: string;
  const IconRes: PChar; const InhibitCancel: Boolean): Word;
begin
  Result := Display(
    Parent, Msg, mtCustom, Buttons, Title, IconRes, InhibitCancel
  );
end;

class function TMessageBox.Display(const Parent: TComponent;
  MsgLines: IStringList; const DlgType: TMsgDlgType;
  const Buttons: array of TMessageBoxButton; const Title: string;
  const IconRes: PChar; const InhibitCancel: Boolean): Word;
var
  Dlg: TForm; // dialogue box instance
begin
  // Create a dialogue box of required type
  Dlg := TMessageBoxForm.Create(
    Parent, MsgLines, Buttons, DlgType, Title, IconRes, InhibitCancel
  );
  try
    // Make sure "Parent" control is parent of dialogue and align over it
    TDlgHelper.SetDlgParent(Dlg, Parent);
    TDlgAligner.Align(Dlg, Parent);
    // Display the dialogue and return result
    Result := Dlg.ShowModal;
  finally
    FreeAndNil(Dlg);
  end;
end;

class function TMessageBox.Display(const Parent: TComponent; const Msg: string;
  const DlgType: TMsgDlgType; const Buttons: array of TMessageBoxButton;
  const Title: string; const IconRes: PChar; const InhibitCancel: Boolean):
  Word;
begin
  Result := Display(
    Parent,
    // convert line breaks in Msg to LF only, remove leading and trailing LFs,
    // ensure proper sentence and then convert text string list.
    TIStringList.Create(
      StrMakeSentence(StrTrimChars(StrUnixLineBreaks(Msg), LF)),
      LF,
      True
    ),
    DlgType,
    Buttons,
    Title,
    IconRes,
    InhibitCancel
  );
end;

class procedure TMessageBox.Error(const Parent: TComponent; const Msg: string);
begin
  MessageBeep(MB_ICONERROR);
  Display(
    Parent,
    Msg,
    mtError,
    [TMessageBoxButton.Create(sBtnOK, mrOK, True, True)],
    DefaultTitle,
    DefaultIcon,
    False
  );
end;

class procedure TMessageBox.Information(const Parent: TComponent;
  const Msg: string);
begin
  Display(
    Parent,
    Msg,
    mtInformation,
    [TMessageBoxButton.Create(sBtnOK, mrOK, True, True)],
    DefaultTitle,
    DefaultIcon,
    False
  );
end;

{ TMessageBoxButton }

constructor TMessageBoxButton.Create(const ACaption: TCaption;
  const AModalResult: Integer; const ADefault, ACancel: Boolean);
begin
  Caption := ACaption;
  ModalResult := AModalResult;
  Default := ADefault;
  Cancel := ACancel;
end;

{ TMessageBoxForm }

procedure TMessageBoxForm.ArrangeControls;

  ///  Arranges labels within form offset from given top left co-ordinate.
  procedure ArrangeLabels(const Left, Top: Integer);
  var
    Idx: Integer;       // loops through all labels
    NextTop: Integer;   // vertical position of next label
  begin
    NextTop := Top;
    for Idx := Low(fLabels) to High(fLabels) do
    begin
      fLabels[Idx].Left := Left;
      fLabels[Idx].Top := NextTop;
      Inc(NextTop, LabelHeight(fLabels[Idx]));
    end;
  end;

  ///  Centres buttons in a strip of given width with tops at given position.
  procedure ArrangeButtons(const Width, Top: Integer);
  var
    Idx: Integer;       // loops through all buttons
    NextLeft: Integer;  // horizontal position of each button
  begin
    NextLeft := (Width - ButtonsWidth) div 2;
    for Idx := Low(fButtons) to High(fButtons) do
    begin
      fButtons[Idx].Top := Top;
      fButtons[Idx].Left := NextLeft;
      Inc(NextLeft, fButtons[Idx].Width + cBtnSpacing);
    end;
  end;

begin
  ArrangeLabels(TextLeftMargin, cYPadding);
  ArrangeButtons(
    ClientWidth, TextAndIconHeight + cYPadding + cBtnStripTopMargin
  );
end;

function TMessageBoxForm.ButtonsHeight: Integer;
begin
  Result := fButtons[0].Height;
end;

function TMessageBoxForm.ButtonsWidth: Integer;
begin
  Result := Length(fButtons) * fButtons[0].Width
    + (Length(fButtons) - 1) * cBtnSpacing;
end;

constructor TMessageBoxForm.Create(const Owner: TComponent;
  const Text: IStringList; const Buttons: array of TMessageBoxButton;
  const DlgType: TMsgDlgType; const Title: string; const IconRes: PChar;
  const InhibitCancel: Boolean);
begin
  Assert(Length(Buttons) > 0, ClassName + '.Create: Buttons array is empty');
  Assert(Assigned(Text) and (Text.Count > 0),
    ClassName + '.Ctreate: No message text provided');
  inherited CreateNew(Owner);
  Position := poDesigned;   // must be poDesgined to enable alignment
  BorderStyle := bsDialog;  // it's a dialogue box
  if InhibitCancel then
    BorderIcons := BorderIcons - [biSystemMenu];
  TFontHelper.SetDefaultFont(Font);
  InitCaption(Title, DlgType);
  InitImage(IconRes, DlgType);
  InitButtons(Buttons);
  InitLabels(Text);
  SizeDialogBox;
  ArrangeControls;
end;

procedure TMessageBoxForm.InitButtons(
  const Buttons: array of TMessageBoxButton);

  ///  Creates each required button control and stores reference in fButtons.
  procedure CreateButtons;
  var
    Idx: Integer;   // loops through all button definition records
    Btn: TButton;   // reference to a created button control
  begin
    SetLength(fButtons, Length(Buttons));
    for Idx := Low(Buttons) to High(Buttons) do
    begin
      Btn := TButton.Create(Self);
      Btn.Parent := Self;
      Btn.Name := Format('Button%d', [Idx]);
      // set values from button definition record
      Btn.Caption := Buttons[Idx].Caption;
      Btn.ModalResult := Buttons[Idx].ModalResult;
      Btn.Default := Buttons[Idx].Default;
      Btn.Cancel := Buttons[Idx].Cancel;
      fButtons[Idx] := Btn;
    end;
  end;

  ///  Calculates size of largest button caption text.
  ///  MUST be called after CreateButtons
  function ButtonTextExtent: TSize;
  var
    Idx: Integer;           // loops thru all buttons
    Btn: TButton;           // references each button
    BtnTextExtent: TSize;   // text extent of each button caption
  begin
    Result.cx := -1;
    Result.cy := -1;
    for Idx := Low(fButtons) to High(fButtons) do
    begin
      Btn := fButtons[Idx];
      BtnTextExtent := StringExtent(Btn.Caption, Btn.Font);
      Result.cx := Max(Result.cx, BtnTextExtent.cx);
      Result.cy := Max(Result.cy, BtnTextExtent.cy);
    end;
  end;

  ///  Sets size of every button to given size.
  ///  MUST be called after CreateButtons.
  procedure SizeButtons(const Size: TSize);
    {Sets size of every button to a specified size. MUST be called after
    CreateButtons.
      @param Size [in] Required button size.
    }
  var
    Idx: Integer; // loops thru all buttons
  begin
    for Idx := Low(fButtons) to High(fButtons) do
    begin
      fButtons[Idx].Width := Size.cx;
      fButtons[Idx].Height := Size.cy;
    end;
  end;

var
  TextExtent: TSize;  // size of largest button caption
  BtnSize: TSize;     // size of every button
begin
  // Create all required button controls
  CreateButtons;
  // Find size of largest button caption
  TextExtent := ButtonTextExtent;
  // Size of each button to be large enough to hold largest caption, but ensure
  // buttons are no smaller than default size. All buttons have same size.
  BtnSize.cx := Max(fButtons[0].Width, TextExtent.cx + 12);
  BtnSize.cy := Max(fButtons[0].Height, TextExtent.cy + 10);
  SizeButtons(BtnSize);
end;

procedure TMessageBoxForm.InitCaption(const Title: string;
  const DlgType: TMsgDlgType);
const
  // Map of dialogue types onto resource id of default caption
  cCaptions: array[TMsgDlgType] of Pointer = (
    @SMsgDlgWarning, @SMsgDlgError, @SMsgDlgInformation, @SMsgDlgConfirm, nil
  );
begin
  if Title <> TMessageBox.DefaultTitle then
    // Caller specified a title: use it
    Caption := Title
  else
  begin
    // Caller did not specify a caption: use default
    if Assigned(cCaptions[DlgType]) then
      // get default from a resource string
      Caption := LoadResString(cCaptions[DlgType])
    else
      // no default resource string: use application's title
      Caption := Application.Title;
  end;
end;

procedure TMessageBoxForm.InitImage(const IconRes: PChar;
  const DlgType: TMsgDlgType);

  ///  Returns instance handle for given icon resource. If resource is a
  ///  recognised system icon then instance is 0, otherwise resource is assumed
  ///  to be in application's resources.
  function ResourceInstance(const IconId: PChar): THandle;
  var
    ID: PChar;
  const
    cSystemIcons: array[1..7] of PChar = (
      IDI_APPLICATION, IDI_HAND, IDI_QUESTION, IDI_EXCLAMATION, IDI_ASTERISK,
      IDI_WINLOGO, IDI_SHIELD
      // IDI_WARNING, IDI_ERROR & IDI_INFORMATION excluded because they are
      // alternates for above values
    );
  begin
    for ID in cSystemIcons do
      if Assigned(ID) and (IconID = ID) then
        Exit(0);
    Result := HInstance;
  end;

const
  // Map of dialogue types onto icon resources
  cIconIDs: array[TMsgDlgType] of PChar = (
    IDI_EXCLAMATION, IDI_HAND, IDI_ASTERISK, IDI_QUESTION, nil
  );
var
  IconId: PChar;  // resource id of icon (nil if no icon required)
begin
  if IconRes <> TMessageBox.DefaultIcon then
    IconId := IconRes
  else
    IconId := cIconIDs[DlgType];
  if Assigned(IconId) then
  begin
    fImage := TImage.Create(Self);
    fImage.Parent := Self;
    fImage.Name := 'Image';
    fImage.Picture.Icon.Handle := LoadIcon(ResourceInstance(IconID), IconID);
    // icon is positioned at top left of dialogue box, inside dialogue's margins
    fImage.BoundsRect := TRectEx.CreateBounds(
      cXPadding, cYPadding,
      fImage.Picture.Icon.Width, fImage.Picture.Icon.Height
    );
  end;
end;

procedure TMessageBoxForm.InitLabels(const Lines: IStringList);

  ///  Creates all required labels and stores reference in fLabels[].
  procedure CreateLabels;
  var
    Idx: Integer; // loops through all lines of text
    Lbl: TLabel;  // reference to a label
  begin
    SetLength(fLabels, Lines.Count);
    for Idx := 0 to Pred(Lines.Count) do
    begin
      Lbl := TLabel.Create(Self);
      Lbl.Parent := Self;
      Lbl.Name := Format('Label%d', [Idx]);
      Lbl.Caption := Lines[Idx];
      Lbl.AutoSize := False;
      Lbl.WordWrap := True;
      Lbl.ShowAccelChar := False;
      Lbl.Width := 0;      // width and height MUST be initialised to 0 here
      Lbl.Height := 0;
      fLabels[Idx] := Lbl;
    end;
  end;

  ///  Sets the width and height of the labels.
  ///  MUST be called after CreateLabels.
  procedure SizeLabels;
  var
    Idx: Integer;       // loops through all labels
    TextRect: TRectEx;  // bounding rectangle of text in label
  const
    // Text formatting flags
    cTextFlags = DT_EXPANDTABS or DT_WORDBREAK or DT_NOPREFIX;
  begin
    for Idx := Low(fLabels) to High(fLabels) do
    begin
      // Treat empty labels differently: leave width and height at 0
      if fLabels[Idx].Caption = '' then
        Continue;
      // Get size of rectangle required to display a label
      TextRect := GetTextRect(
        fLabels[Idx].Caption,
        Canvas,
        Rect(0, 0, MaxTextWidth, 0),
        cTextFlags or fLabels[Idx].DrawTextBiDiModeFlagsReadingOnly
      );
      // Size the label
      fLabels[Idx].Width := TextRect.Width;
      fLabels[Idx].Height := TextRect.Height;
    end;
  end;

begin
  CreateLabels;
  SizeLabels;
end;

function TMessageBoxForm.LabelHeight(const Lbl: TLabel): Integer;
begin
  // We treat blank labels differently: they represent paragraph breaks
  if Lbl.Caption <> '' then
    Result := Lbl.Height
  else
    Result := cParaSpacing;
end;

function TMessageBoxForm.LabelsHeight: Integer;
var
  Idx: Integer; // loops thru all labels
begin
  Result := 0;
  for Idx := Low(fLabels) to High(fLabels) do
    Inc(Result, LabelHeight(fLabels[Idx]));
end;

function TMessageBoxForm.LabelsWidth: Integer;
var
  Idx: Integer; // loops thru all labels
begin
  Result := 0;
  for Idx := Low(fLabels) to High(fLabels) do
  begin
    if fLabels[Idx].Width > Result then
      Result := fLabels[Idx].Width;
  end;
end;

function TMessageBoxForm.MaxTextWidth: Integer;
begin
  // For smaller screens, max space is half of screen width. For larger screens
  // max space is cMaxDlgWidth pixels.
  Result := Min(cMaxTextWidth, Screen.Width div 2);
end;

procedure TMessageBoxForm.SizeDialogBox;
begin
  ClientWidth := Max(ButtonsWidth, TextAndIconWidth) + 2 * cXPadding;
  ClientHeight := TextAndIconHeight + ButtonsHeight
    + 2 * cyPadding + cBtnStripTopMargin;
end;

function TMessageBoxForm.TextAndIconHeight: Integer;
begin
  if Assigned(fImage) then
    Result := Max(LabelsHeight, fImage.Height)
  else
    Result := LabelsHeight;
end;

function TMessageBoxForm.TextAndIconWidth: Integer;
begin
  Result := LabelsWidth;
  if Assigned(fImage) then
    Inc(Result, fImage.Width + cXPadding);
end;

function TMessageBoxForm.TextLeftMargin: Integer;
begin
  if Assigned(fImage) then
    Result := fImage.Width + 2 * cXPadding
  else
    Result := cXPadding;
end;

end.

