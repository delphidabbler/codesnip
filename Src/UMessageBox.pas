{
 * UMessageBox.pas
 *
 * Implements a static class that can display message and confirmation dialog
 * boxes at an appropriate position on screen.
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
 * The Original Code is UMessageBox.pas
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

  {
  TMessageBoxButton:
    Record that contains attributes of a button that appears in a TMessageBox
    dialog box.
  }
  TMessageBoxButton = record
    Caption: TCaption;      // button's caption
    ModalResult: Integer;   // button's modal result (0 to prevent form closure)
    Default: Boolean;       // true if button is default
    Cancel: Boolean;        // true if button is cancel button
    constructor Create(const ACaption: TCaption; const AModalResult: Integer;
      const ADefault: Boolean = False; const ACancel: Boolean = False);
      {Initialises fields of record.
        @param ACaption [in] Button's caption.
        @param AModalResult [in] Modal result returned when button is clicked.
        @param Default [in] Optional: value of button's Default property.
        @param Cancel [in] Optional: value of button's Cancel property.
      }
  end;

  {
  TMessageBox:
    Static class that can display message and confirmation dialog boxes at an
    appropriate position on screen.
  }
  TMessageBox = class sealed(TNoConstructObject)
  public
    // Constants to pass to methods that require Title or IconRes parameters to
    // indicate default value is to be used.
    const DefaultTitle = '';  // Indicates default title required
    const DefaultIcon = nil;  // Indicates default icon required
  strict private
    class function Display(const Parent: TComponent;
      MsgLines: IStringList; const DlgType: TMsgDlgType;
      const Buttons: array of TMessageBoxButton; const Title: string;
      const IconRes: PChar): Word; overload;
      {Displays a message in a customised dialog box located over the calling
      form.
        @param Parent [in] Component that dialog box is aligned over and that
          becomes parent of dialog box. If Parent is nil then current active
          form is used.
        @param MsgLines [in] Message displayed in dialog. Can store mulitple
          lines. Use empty lines to insert space between paragraphs.
        @param DlgType [in] Type of dialog box.
        @param Buttons [in] Array of records decribing buttons to be displayed.
        @param Title [in] Title of dialog box. If '' title used depends on
          DlgType.
        @param IconRes [in] Icon resource identifier. If nil icon used depends
          on DlgType.
        @return Value indicating which button was pressed to close dialog box.
          This will be value of ModalResult field of one of the buttons
          specified in Buttons.
      }
    class function Display(const Parent: TComponent; const Msg: string;
      const DlgType: TMsgDlgType; const Buttons: array of TMessageBoxButton;
      const Title: string; const IconRes: PChar): Word; overload;
      {Displays a message in a customised dialog box located over the calling
      form.
        @param Parent [in] Component that dialog box is aligned over and that
          becomes parent of dialog box. If Parent is nil then current active
          form is used.
        @param Msg [in] Message displayed in dialog. Separate lines with LF or
          CRLF. Separate paragraphs with two line breaks.
        @param DlgType [in] Type of dialog box.
        @param Buttons [in] Array of records decribing buttons to be displayed.
        @param Title [in] Title of dialog box. If '' title used depends on
          DlgType.
        @param IconRes [in] Icon resource identifier. If nil icon used depends
          on DlgType.
        @return Value indicating which button was pressed to close dialog box.
          This will be value of ModalResult field of one of the buttons
          specified in Buttons.
      }
  public
    class procedure Information(const Parent: TComponent; const Msg: string);
      {Displays a message in an information dialog box located relative to owner
      form.
        @param Parent [in] Component that dialog box is aligned over and that
          becomes parent of dialog box. If Parent is nil then current active
          form is used.
        @param Msg [in] Message displayed in dialog. Multi-line messages should
          be separated by LF or CRLF.
      }
    class procedure Error(const Parent: TComponent; const Msg: string);
      {Displays a message in an error dialog box located relative to owner form.
        @param Parent [in] Component that dialog box is aligned over and that
          becomes parent of dialog box. If Parent is nil then current active
          form is used.
        @param Msg [in] Message displayed in dialog. Multi-line messages should
          be separated by LF or CRLF.
      }
    class function Confirm(const Parent: TComponent;
      const Msg: string): Boolean;
      {Displays a message in a confirmation dialog box located relative to owner
      form.
        @param Parent [in] Component that dialog box is aligned over and that
          becomes parent of dialog box. If Parent is nil then current active
          form is used.
        @param Msg [in] Message displayed in dialog. Multi-line messages should
          be separated by LF or CRLF.
        @return True if user clicks Yes and false otherwise.
      }
    class function Custom(const Parent: TComponent; const Msg: string;
      const Buttons: array of TMessageBoxButton;
      const Title: string = DefaultTitle; const IconRes: PChar = DefaultIcon):
      Word;
      {Displays a message in a dialog box with custom buttons and no icon that
      is located relative to owner form.
        @param Parent [in] Component that dialog box is aligned over and that
          becomes parent of dialog box. If Parent is nil then current active
          form is used.
        @param Msg [in] Message displayed in dialog. Multi-line messages should
          be separated by LF or CRLF.
        @param Buttons [in] Array of records decribing buttons to be displayed.
        @param Title [in] Optional title of dialog box. If Title='' then
          application title is used.
        @param IconRes [in] Optional icon resource. No icon is displayed if not
          provided.
        @return Value indicating which button was pressed to close dialog box.
          This will be value of ModalResult field of one of the buttons
          specified in the Buttons array.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Windows, Forms, StdCtrls, ExtCtrls, Consts, Math,
  // Project
  UConsts, UDlgHelper, UFontHelper, UGraphicUtils, UStructs, UUtils;


type

  {
  TMessageBoxForm:
    Implements a message dialog box that has configurable lines and paragraphs
    of text, buttons and icon.
  }
  TMessageBoxForm = class sealed(TForm)
  strict private
    var
      fImage: TImage;             // Image control: displays any icon
      fButtons: array of TButton; // Buttons displayed in dialog box
      fLabels: array of TLabel;   // Labels that display text in dialog box.
    const
      cXPadding = 16;             // Left and right dialog box margins
      cYPadding = 16;             // Top and bottom dialog box margins
      cParaSpacing = 8;           // Spacing between paragraphs
      cBtnSpacing = 8;            // Spacing between buttons
      cBtnStripTopMargin = 24;    // Margin above button strip
      cMaxTextWidth = 512;        // Maximum width of text in dialog box
    function MaxTextWidth: Integer;
      {Calculates maximum horizontal space that can be occupied by the dialog's
      text.
        @return Required width in pixels.
      }
    function LabelsHeight: Integer;
      {Finds height required to display all labels.
        @return Required height in pixels.
      }
    function LabelHeight(const Lbl: TLabel): Integer;
      {Calculates height of a label.
        @param Lbl [in] Label for which height is required.
        @return Label height in pixels.
      }
    function LabelsWidth: Integer;
      {Finds width of text in widest label.
        @return Required width in pixels.
      }
    function ButtonsHeight: Integer;
      {Calculates vertical space required to display button strip.
        @return Required height in pixels.
      }
    function ButtonsWidth: Integer;
      {Calculates horizontal space required to display strip of all buttons.
        @return Required width in pixels.
      }
    function TextLeftMargin: Integer;
      {Calculates offset of text from dialog box's left hand margin. Depends on
      whether dialog box displays an icon.
        @return Required margin in pixels.
      }
    function TextAndIconHeight: Integer;
      {Calculates vertical space required to accommodate text and any icon.
        @return Required height in pixels.
      }
    function TextAndIconWidth: Integer;
      {Calculates horizontal spaces required to accommodate text and any icon.
        @return Required width in pixels.
      }
    procedure InitCaption(const Title: string; const DlgType: TMsgDlgType);
      {Sets text of dialog's caption.
        @param Title [in] Title to display in dialog's caption. If '' a default
          caption dependent on dialog type is used.
        @param DlgType [in] Type of dialog box. Used to determine default
          caption if Title is ''.
      }
    procedure InitImage(const IconRes: PChar; const DlgType: TMsgDlgType);
      {Creates and initialises and locates image control required to display any
      icon. No image control is created if no icon is required.
        @param IconRes [in] Icon resource. If nil then icon is determined by
          DlgType.
        @param DlgType [in] Type of dialog to be displayed. Determines which, if
          any icon is required when IconRes is nil.
      }
    procedure InitButtons(const Buttons: array of TMessageBoxButton);
      {Creates and sizes dialog's buttons.
        @param Buttons [in] Array of records describing buttons to display.
      }
    procedure InitLabels(const Lines: IStringList);
      {Creates and sizes labels used to display text in dialog box.
        @param Lines [in] String list containing lines of text to display. Blank
          lines indicate a paragraph break.
      }
    procedure SizeDialogBox;
      {Sets size of dialog box to accomodate text, optional icon and buttons.
      }
    procedure ArrangeControls;
      {Arranges all controls within dialog box.
      }
  public
    constructor Create(const Owner: TComponent;
      const Text: IStringList;
      const Buttons: array of TMessageBoxButton;
      const DlgType: TMsgDlgType;
      const Title: string;
      const IconRes: PChar); reintroduce;
      {Object constructor. Reintroduced constructor that creates dialog box form
      with components required to display text, buttons and optional icon.
        @param Owner [in] Control that owns this dialog box.
        @param Text [in] List of lines of text to display. Blank lines indicate
          a paragraph spacer.
        @param Buttons [in] Array of records that define buttons to be
          displayed.
        @param DlgType [in] Type of dialog box to be displayed.
        @param Title [in] Optional dialog box caption text. If Title='' caption
          depends on DlgType.
        @param IconRes [in] Resource to be used for icon displayed. If nil then
          any icon displayed depends on DlgType.
      }
  end;

{ TMessageBox }

class function TMessageBox.Confirm(const Parent: TComponent;
  const Msg: string): Boolean;
  {Displays a message in a confirmation dialog box located relative to owner
  form.
    @param Parent [in] Component that dialog box is aligned over and that
      becomes parent of dialog box. If Parent is nil then current active form
      is used.
    @param Msg [in] Message displayed in dialog. Multi-line messages should be
      separated by LF or CRLF.
    @return True if user clicks Yes and false otherwise.
  }
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
    DefaultIcon
  ) = mrYes;
end;

class function TMessageBox.Custom(const Parent: TComponent; const Msg: string;
  const Buttons: array of TMessageBoxButton; const Title: string;
  const IconRes: PChar): Word;
  {Displays a message in a dialog box with custom buttons and no icon that is
  located relative to owner form.
    @param Parent [in] Component that dialog box is aligned over and that
      becomes parent of dialog box. If Parent is nil then current active form
      is used.
    @param Msg [in] Message displayed in dialog. Multi-line messages should be
      separated by LF or CRLF.
    @param Buttons [in] Array of records decribing buttons to be displayed.
    @param Title [in] Optional title of dialog box. If Title='' then application
      title is used.
    @param IconRes [in] Optional icon resource. No icon is displayed if not
      provided.
    @return Value indicating which button was pressed to close dialog box. This
      will be value of ModalResult field of one of the buttons specified in the
      Buttons array.
  }
begin
  Result := Display(Parent, Msg, mtCustom, Buttons, Title, IconRes);
end;

class function TMessageBox.Display(const Parent: TComponent;
  MsgLines: IStringList; const DlgType: TMsgDlgType;
  const Buttons: array of TMessageBoxButton; const Title: string;
  const IconRes: PChar): Word;
  {Displays a message in a customised dialog box located over the calling form.
    @param Parent [in] Component that dialog box is aligned over and that
      becomes parent of dialog box. If Parent is nil then current active form is
      used.
    @param MsgLines [in] Message displayed in dialog. Can store mulitple lines.
      Use empty lines to insert space between paragraphs.
    @param DlgType [in] Type of dialog box.
    @param Buttons [in] Array of records decribing buttons to be displayed.
    @param Title [in] Title of dialog box. If '' title used depends on DlgType.
    @param IconRes [in] Icon resource identifier. If nil icon used depends on
      DlgType.
    @return Value indicating which button was pressed to close dialog box. This
      will be value of ModalResult field of one of the buttons specified in
      Buttons.
  }
var
  Dlg: TForm; // dialog box instance
begin
  // Create a dialog box of required type
  Dlg := TMessageBoxForm.Create(
    Parent, MsgLines, Buttons, DlgType, Title, IconRes
  );
  try
    // Make sure "Parent" control is parent of dialog and align over it
    TDlgHelper.SetDlgParent(Dlg, Parent);
    TDlgAligner.Align(Dlg, Parent);
    // Display the dialog and return result
    Result := Dlg.ShowModal;
  finally
    FreeAndNil(Dlg);
  end;
end;

class function TMessageBox.Display(const Parent: TComponent; const Msg: string;
  const DlgType: TMsgDlgType; const Buttons: array of TMessageBoxButton;
  const Title: string; const IconRes: PChar): Word;
  {Displays a message in a customised dialog box located over the calling form.
    @param Parent [in] Component that dialog box is aligned over and that
      becomes parent of dialog box. If Parent is nil then current active form is
      used.
    @param Msg [in] Message displayed in dialog. Separate lines with LF or CRLF.
      Separate paragraphs with two line breaks.
    @param DlgType [in] Type of dialog box.
    @param Buttons [in] Array of records decribing buttons to be displayed.
    @param Title [in] Title of dialog box. If '' title used depends on DlgType.
    @param IconRes [in] Icon resource identifier. If nil icon used depends on
      DlgType.
    @return Value indicating which button was pressed to close dialog box. This
      will be value of ModalResult field of one of the buttons specified in
      Buttons.
  }
begin
  Result := Display(
    Parent,
    // convert line breaks in Msg to LF only, remove leading and trailing LFs,
    // ensure proper sentence and then convert text string list.
    TIStringList.Create(
      MakeSentence(TrimChar(UnixLineBreaks(Msg), LF)),
      LF,
      True
    ),
    DlgType,
    Buttons,
    Title,
    IconRes
  );
end;

class procedure TMessageBox.Error(const Parent: TComponent; const Msg: string);
  {Displays a message in an error dialog box located relative to owner form.
    @param Parent [in] Component that dialog box is aligned over and that
      becomes parent of dialog box. If Parent is nil then current active form
      is used.
    @param Msg [in] Message displayed in dialog. Multi-line messages should be
      separated by LF or CRLF.
  }
begin
  MessageBeep(MB_ICONERROR);
  Display(
    Parent,
    Msg,
    mtError,
    [TMessageBoxButton.Create(sBtnOK, mrOK, True, True)],
    DefaultTitle,
    DefaultIcon
  );
end;

class procedure TMessageBox.Information(const Parent: TComponent;
  const Msg: string);
  {Displays a message in an information dialog box located relative to owner
  form.
    @param Parent [in] Component that dialog box is aligned over and that
      becomes parent of dialog box. If Parent is nil then current active form
      is used.
    @param Msg [in] Message displayed in dialog. Multi-line messages should be
      separated by LF or CRLF.
  }
begin
  Display(
    Parent,
    Msg,
    mtInformation,
    [TMessageBoxButton.Create(sBtnOK, mrOK, True, True)],
    DefaultTitle,
    DefaultIcon
  );
end;

{ TMessageBoxButton }

constructor TMessageBoxButton.Create(const ACaption: TCaption;
  const AModalResult: Integer; const ADefault, ACancel: Boolean);
  {Initialises fields of record.
    @param ACaption [in] Button's caption.
    @param AModalResult [in] Modal result returned when button is clicked.
    @param Default [in] Optional: value of button's Default property.
    @param Cancel [in] Optional: value of button's Cancel property.
  }
begin
  Caption := ACaption;
  ModalResult := AModalResult;
  Default := ADefault;
  Cancel := ACancel;
end;

{ TMessageBoxForm }

procedure TMessageBoxForm.ArrangeControls;
  {Arranges all controls within dialog box.
  }

  // ---------------------------------------------------------------------------
  procedure ArrangeLabels(const Left, Top: Integer);
    {Arranges labels within dialog box.
      @param Left [in] Left margin for all labels.
      @param Top [in] Vertical position of first label.
    }
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

  procedure ArrangeButtons(const Width, Top: Integer);
    {Arranges buttons in a centred strip in dialog box.
      @param Width [in] Width of button strip.
      @param Top [in] Top of button strip.
    }
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
  // ---------------------------------------------------------------------------

begin
  ArrangeLabels(TextLeftMargin, cYPadding);
  ArrangeButtons(
    ClientWidth, TextAndIconHeight + cYPadding + cBtnStripTopMargin
  );
end;

function TMessageBoxForm.ButtonsHeight: Integer;
  {Calculates vertical space required to display button strip.
    @return Required height in pixels.
  }
begin
  Result := fButtons[0].Height;
end;

function TMessageBoxForm.ButtonsWidth: Integer;
  {Calculates horizontal space required to display strip of all buttons.
    @return Required width in pixels.
  }
begin
  Result := Length(fButtons) * fButtons[0].Width
    + (Length(fButtons) - 1) * cBtnSpacing;
end;

constructor TMessageBoxForm.Create(const Owner: TComponent;
  const Text: IStringList; const Buttons: array of TMessageBoxButton;
  const DlgType: TMsgDlgType; const Title: string; const IconRes: PChar);
  {Object constructor. Reintroduced constructor that creates dialog box form
  with components required to display text, buttons and optional icon.
    @param Owner [in] Control that owns this dialog box.
    @param Text [in] List of lines of text to display. Blank lines indicate a
      paragraph spacer.
    @param Buttons [in] Array of records that define buttons to be displayed.
    @param DlgType [in] Type of dialog box to be displayed.
    @param Title [in] Optional dialog box caption text. If Title='' caption
      depends on DlgType.
    @param IconRes [in] Resource to be used for icon displayed. If nil then any
      icon displayed depends on DlgType.
  }
begin
  Assert(Length(Buttons) > 0, ClassName + '.Create: Buttons array is empty');
  Assert(Assigned(Text) and (Text.Count > 0),
    ClassName + '.Ctreate: No message text provided');
  inherited CreateNew(Owner);
  Position := poDesigned;   // must be poDesgined to enable alignment
  BorderStyle := bsDialog;  // it's a dialog box
  // Set font to OS default
  TFontHelper.SetDefaultFont(Font, False);
  // Initialise controls
  InitCaption(Title, DlgType);
  InitImage(IconRes, DlgType);
  InitButtons(Buttons);
  InitLabels(Text);
  // Size the dialog box
  SizeDialogBox;
  // Arrange controls within dialog box
  ArrangeControls;
end;

procedure TMessageBoxForm.InitButtons(
  const Buttons: array of TMessageBoxButton);
  {Creates and sizes dialog's buttons.
    @param Buttons [in] Array of records describing buttons to display.
  }

  // ---------------------------------------------------------------------------
  procedure CreateButtons;
    {Creates each required button control and stores reference in fButtons.
    }
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

  function ButtonTextExtent: TSize;
    {Calculates text size of largest button caption. MUST be called after
    CreateButtons.
      @return Size structure that can hold largest caption.
    }
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
  // ---------------------------------------------------------------------------

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
  BtnSize.cx := Max(
    fButtons[0].Width,
    TextExtent.cx + fButtons[0].Margins.Left + fButtons[0].Margins.Right
  );
  BtnSize.cy := Max(
    fButtons[0].Height,
    TextExtent.cy + fButtons[0].Margins.Top + fButtons[0].Margins.Bottom
  );
  SizeButtons(BtnSize);
end;

procedure TMessageBoxForm.InitCaption(const Title: string;
  const DlgType: TMsgDlgType);
  {Sets text of dialog's caption.
    @param Title [in] Title to display in dialog's caption. If '' a default
      caption dependent on dialog type is used.
    @param DlgType [in] Type of dialog box. Used to determine default caption
      if Title is ''.
  }
const
  // Map of dialog types onto resource id of default caption
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
  {Creates and initialises and locates image control required to display any
  icon. No image control is created if no icon is required.
    @param IconRes [in] Icon resource. If nil then icon is determined by
      DlgType.
    @param DlgType [in] Type of dialog to be displayed. Determines which, if any
      icon is required when IconRes is nil.
  }
  // ---------------------------------------------------------------------------
  function ResourceInstance(const IconId: PChar): THandle;
    {Returns instance handle for specified icon resource. If resource is a
    recognised system icon then instance is 0, otherwise resource is assumed to
    be in application's resources.
      @param IconId [in] Icon resource identifier.
      @return Required instance handle.
    }
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
  // ---------------------------------------------------------------------------
const
  // Map of dialog types onto icon resources
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
    // icon is positioned at top left of dialog box, inside dialog's margins
    fImage.BoundsRect := TRectEx.CreateBounds(
      cXPadding, cYPadding,
      fImage.Picture.Icon.Width, fImage.Picture.Icon.Height
    );
  end;
end;

procedure TMessageBoxForm.InitLabels(const Lines: IStringList);
  {Creates and sizes labels used to display text in dialog box.
    @param Lines [in] String list containing lines of text to display. Blank
      lines indicate a paragraph break.
  }

  // ---------------------------------------------------------------------------
  procedure CreateLabels;
    {Creates all required labels and stores reference in fLabels[].
    }
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

  procedure SizeLabels;
    {Sets the width and height of the labels. MUST be called after CreateLabels.
    }
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
  // ---------------------------------------------------------------------------

begin
  CreateLabels;
  SizeLabels;
end;

function TMessageBoxForm.LabelHeight(const Lbl: TLabel): Integer;
  {Calculates height of a label.
    @param Lbl [in] Label for which height is required.
    @return Label height in pixels.
  }
begin
  // We treat blank labels differently: they represent paragraph breaks
  if Lbl.Caption <> '' then
    Result := Lbl.Height
  else
    Result := cParaSpacing;
end;

function TMessageBoxForm.LabelsHeight: Integer;
  {Finds height required to display all labels.
    @return Required height in pixels.
  }
var
  Idx: Integer; // loops thru all labels
begin
  Result := 0;
  for Idx := Low(fLabels) to High(fLabels) do
    Inc(Result, LabelHeight(fLabels[Idx]));
end;

function TMessageBoxForm.LabelsWidth: Integer;
  {Finds width of text in widest label.
    @return Required width in pixels.
  }
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
  {Calculates maximum horizontal space that can be occupied by the dialog's
  text.
    @return Required width in pixels.
  }
begin
  // For smaller screens, max space is half of screen width. For larger screens
  // max space is cMaxDlgWidth pixels.
  Result := Min(cMaxTextWidth, Screen.Width div 2);
end;

procedure TMessageBoxForm.SizeDialogBox;
  {Sets size of dialog box to accomodate text, optional icon and buttons.
  }
begin
  ClientWidth := Max(ButtonsWidth, TextAndIconWidth) + 2 * cXPadding;
  ClientHeight := TextAndIconHeight + ButtonsHeight
    + 2 * cyPadding + cBtnStripTopMargin;
end;

function TMessageBoxForm.TextAndIconHeight: Integer;
  {Calculates vertical space required to accommodate text and any icon.
    @return Required height in pixels.
  }
begin
  if Assigned(fImage) then
    Result := Max(LabelsHeight, fImage.Height)
  else
    Result := LabelsHeight;
end;

function TMessageBoxForm.TextAndIconWidth: Integer;
  {Calculates horizontal spaces required to accommodate text and any icon.
    @return Required width in pixels.
  }
begin
  Result := LabelsWidth;
  if Assigned(fImage) then
    Inc(Result, fImage.Width + cXPadding);
end;

function TMessageBoxForm.TextLeftMargin: Integer;
  {Calculates offset of text from dialog box's left hand margin. Depends on
  whether dialog box displays an icon.
    @return Required margin in pixels.
  }
begin
  if Assigned(fImage) then
    Result := fImage.Width + 2 * cXPadding
  else
    Result := cXPadding;
end;

end.

