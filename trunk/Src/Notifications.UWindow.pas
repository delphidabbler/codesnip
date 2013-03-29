{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a slide-in notification window that appears at the bottom right
 * of its parent window.
}


unit Notifications.UWindow;


interface


uses
  // Delphi
  Classes, StdCtrls, ExtCtrls, Controls, Buttons, Generics.Collections,
  Windows {must come before Graphics}, Graphics, Messages, SyncObjs, ActnList,
  // Project
  UStructs;


type
  ///  <summary>VCL control that encapsulates notification window that appears
  ///  at the bottom right of its parent window and can be animated to slide on
  ///  and off screen.</summary>
  TNotificationWindow = class(TCustomControl)
  public
    type
      ///  <summary>
      ///  <para>Possible states of notification window.</para>
      ///  <para>- nwsOpen - window is fully open.</para>
      ///  <para>- nwsClosed - window is fully closed (hidden).</para>
      ///  <para>- nwsOpening - window is transitioning from closed to open.
      ///  </para>
      ///  <para>- nwsClosing - window is transitioning from open to closed.
      ///  </para>
      ///  </summary>
      TState = (nwsOpen, nwsClosed, nwsOpening, nwsClosing);
  strict private
    const
      ///  <summary>Custom window message used to indicate window has been
      ///  concealed (i.e. slide-out has completed).</summary>
      WM_CONCEALED = WM_USER + 234;
      ///  <summary>Colour that defines starting point (top) of window's
      ///  gradient fill.</summary>
      GradColour1 = clWhite;
      ///  <summary>Colour that defines end point (bottom) of window's gradient
      ///  fill.</summary>
      GradColour2 = $edbc96;
      ///  <summary>Colour of window border.</summary>
      BorderColour = $cc8888;
      ///  <summary>Step size used when slliding window in or out.</summary>
      ///  <remarks>A complete move is taken to range from 0.0 to 1.0.</remarks>
      SlideStep = 0.02;
      ///  <summary>Spacing between paragraphs of text from Contents property.
      ///  </summary>
      ParaSpacing = 4;
      ///  <summary>Character used for close button.</summary>
      ///  <remarks>Character must be rendered in Marlett font.</remarks>
      CloseButtonChar = #$75;
      ///  <summary>Character used for help button.</summary>
      ///  <remarks>Character must be rendered in Marlett font.</remarks>
      HelpButtonChar = #$73;
  strict private
    var
      ///  <summary>Value of DisplayLock property.</summary>
      fDisplayLock: TSimpleEvent;
      ///  <summary>Value of State property.</summary>
      fState: TState;
      ///  <summary>Button used to close (hide) an open window.</summary>
      fCloseBtn: TSpeedButton;
      ///  <summary>Button used to display context sensitive help.</summary>
      fHelpBtn: TSpeedButton;
      ///  <summary>Button used to trigger any action event referenced by
      ///  ButtonAction property.</summary>
      fActionBtn: TButton;
      ///  <summary>Displays window's title text.</summary>
      fTitleLbl: TLabel;
      ///  <summary>Check box used to indicate whether or not notification
      ///  should be displayed again.</summary>
      fDontShowChk: TCheckBox;
      ///  <summary>Timer used to automatically close (hide) an open window
      ///  after a specified time.</summary>
      fHideTimer: TTimer;
      ///  <summary>Bounding rectangle of 'light bulb' glyph in window.
      ///  </summary>
      fGlyphBounds: TRectEx;
      ///  <summary>Maintains a list of labels used to display the paragraphs
      ///  of text from the Contents property.</summary>
      fContentLblList: TObjectList<TLabel>;
      ///  <summary>'Light bulb' glyph displayed in window.</summary>
      fLightBulb: TBitmap;

  strict private
    ///  <summary>Handles custom message sent when the window has been concealed
    ///  after sliding out.</summary>
    ///  <remarks>The window display lock is opened.</remarks>
    procedure WMConcealed(var Msg: TMessage); message WM_CONCEALED;

    ///  <summary>Getter for Title property.</summary>
    function GetTitle: string;
    ///  <summary>Setter for Title property.</summary>
    ///  <remarks>Causes window to be re-arranged.</remarks>
    procedure SetTitle(const ATitle: string);

    ///  <summary>Getter for Content property.</summary>
    function GetContent: TArray<string>;
    ///  <summary>Setter for Content property.</summary>
    ///  <remarks>Causes window to be re-arranged.</remarks>
    procedure SetContent(const AContent: TArray<string>);

    ///  <summary>Getter for ButtonAction property.</summary>
    function GetButtonAction: TCustomAction;
    ///  <summary>Setter for ButtonAction property.</summary>
    ///  <remarks>Causes window to be re-arranged.</remarks>
    procedure SetButtonAction(const Action: TCustomAction);

    ///  <summary>Getter for DontShow property.</summary>
    function GetDontShow: Boolean;
    ///  <summary>Setter for DontShow property.</summary>
    procedure SetDontShow(const Flag: Boolean);

    ///  <summary>Getter for DisplayDontShowChk property.</summary>
    function GetDisplayDontShowChk: Boolean;
    ///  <summary>Setter for DisplayDontShowChk property.</summary>
    ///  <remarks>Causes window to be re-arranged.</remarks>
    procedure SetDisplayDontShowChk(const Flag: Boolean);

    ///  <summary>Getter for HelpKeyword property.</summary>
    function GetHelpKeyword: string;
    ///  <summary>Setter for HelpKeyword property.</summary>
    ///  <remarks>Causes window to be re-arranged.</remarks>
    procedure SetHelpKeyword(const Keyword: string);

    ///  <summary>Getter for DisplayTime property.</summary>
    function GetDisplayTime: Cardinal;
    ///  <summary>Setter for DisplayTime property.</summary>
    procedure SetDisplayTime(const MS: Cardinal);

    ///  <summary>Rearranges and sizes window according to current property
    ///  values.</summary>
    procedure UpdateWindow;

    ///  <summary>Creates and initialises all the window's controls.</summary>
    procedure CreateCtrls;

    ///  <summary>Handles clicks on Close button by hiding (closing) window.
    ///  </summary>
    ///  <remarks>Does nothing if window is not fully open.</remarks>
    procedure CloseBtnClickHandler(Sender: TObject);

    ///  <summary>Handles clicks on Help button by displaying any help topic
    ///  specified by HelpKeyword property.</summary>
    ///  <remarks>Does nothing if HelpKeyword is empty string.</remarks>
    procedure HelpBtnClickHandler(Sender: TObject);

    ///  <summary>Closes (hides) window when timer's event fires.</summary>
    ///  <remarks>The event is ignored if the window is not fully open.
    ///  </remarks>
    procedure TimerTickHandler(Sender: TObject);

  strict protected
    ///  <summary>Sets parent to given control. Ensures that this window is
    ///  aligned at the bottom right of the parent window.</summary>
    procedure SetParent(AParent: TWinControl); override;

    ///  <summary>Paints window background and glyph.</summary>
    procedure Paint; override;

  public
    ///  <summary>Creates and initialises a new component instance.</summary>
    constructor Create(AOwner: TComponent); override;

    ///  <summary>Destroys component instance.</summary>
    destructor Destroy; override;

    ///  <summary>Sizes and locates window. ALeft is ignored and is always set
    ///  so that window appears at right edge of parent window.</summary>
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    ///  <summary>Opens (displays) the window by sliding it on-screen.</summary>
    ///  <remarks>
    ///  <para>Does nothing unless State = nwsClosed.</para>
    ///  <para>State = nwsOpening while window is sliding and changes to
    ///  nwsOpen when it is fully hidden.</para>
    ///  <para>The window display lock is closed iff State = mwsClose when the
    ///  method is called.</para>
    ///  </remarks>
    procedure SlideIn;

    ///  <summary>Closes (hides) the window by sliding it off-screen.</summary>
    ///  <remarks>
    ///  <para>Does nothing unless State = nwsOpen.</para>
    ///  <para>State = nwsClosing while window is sliding and changes to
    ///  nwsClosed when it is fully hidden.</para>
    ///  <para>Posts a WM_CONCEALED message when the window is fully closed.
    ///  </para>
    ///  </remarks>
    procedure SlideOut;

      ///  <summary>Event object used as lock when window is displayed.
      ///  </summary>
      ///  <remarks>This lock is used by calling code to prevent them from
      ///  trying to display the window until it is closed.</remarks>
    property DisplayLock: TSimpleEvent read fDisplayLock;

    ///  <summary>Current state of window.</summary>
    ///  <remarks>State is always nwsClosed when the window is created, i.e. it
    ///  is hidden by default.</remarks>
    property State: TState read fState;

    ///  <summary>Title of window.</summary>
    ///  <remarks>Defaults to 'Notification'.</remarks>
    property Title: string read GetTitle write SetTitle;

    ///  <summary>Array of paragraphs of text to display in window.</summary>
    ///  <remarks>Defaults to empty array.</remarks>
    property Content: TArray<string> read GetContent write SetContent;

    ///  <summary>Action to be associated with the action button.</summary>
    ///  <remarks>
    ///  <para>The action button is only displayed if ButtonAction is not nil.
    ///  </para>
    ///  <para>The action button uses this ButtonActions's caption as its own
    ///  caption.</para>
    ///  <para>Clicking the action button executes ButtonAction.</para>
    ///  </remarks>
    property ButtonAction: TCustomAction
      read GetButtonAction write SetButtonAction;

    ///  <summary>Indicates whether this notification may be shown again.
    ///  </summary>
    ///  <remarks>
    ///  <para>Defaults to False.</para>
    ///  <para>Always returns False when DisplayDontShowChk is False.</para>
    ///  </remarks>
    property DontShow: Boolean read GetDontShow write SetDontShow;

    ///  <summary>Indicates whether or not 'don't display notification' check
    ///  box is displayed.</summary>
    ///  <remarks>Defaults to True.</remarks>
    property DisplayDontShowChk: Boolean
      read GetDisplayDontShowChk write SetDisplayDontShowChk;

    ///  <summary>A-link help keyword associated with notification.</summary>
    ///  <remarks>
    ///  <para>Defaults to empty string.</para>
    ///  <para>A help button is displayed iff this property has a non-empty
    ///  value.</para>
    ///  </remarks>
    property HelpKeyword: string read GetHelpKeyword write SetHelpKeyword;

    ///  <summary>Amount of time, in milliseconds, the notification is to be
    ///  displayed before being hidden automatically.</summary>
    ///  <remarks>
    ///  <para>Defaults to 10000 (10 seconds).</para>
    ///  <para>If DisplayTime is zero the notification is not hidden
    ///  automatically.</para>
    ///  </remarks>
    property DisplayTime: Cardinal read GetDisplayTime write SetDisplayTime;
  end;


implementation


uses
  // Delphi
  GraphUtil, Math,
  // Project
  UCtrlArranger, UFontHelper, UGraphicUtils, UHelpMgr, UUtils;


{ TNotificationWindow }

procedure TNotificationWindow.CloseBtnClickHandler(Sender: TObject);
begin
  SlideOut; // does nothing if State <> nwsOpen
end;

constructor TNotificationWindow.Create(AOwner: TComponent);
resourcestring
  sDefaultTitle = 'Notification';
begin
  inherited Create(AOwner);
  fContentLblList := TObjectList<TLabel>.Create(True);
  Width := 300;
  Height := 150;
  ControlStyle := ControlStyle - [csOpaque];
  Anchors := [akBottom, akRight];
  Color := $FFFFFF;
  Visible := False;
  fState := nwsClosed;
  fLightBulb := TBitmap.Create;
  fLightBulb.LoadFromResourceName(HInstance, 'NOTIFICATION');
  CreateCtrls;
  fDontShowChk.Checked := False;
  fTitleLbl.Caption := sDefaultTitle;
  fHideTimer.Interval := 10000;
  fDisplayLock := TSimpleEvent.Create;
  fDisplayLock.SetEvent;
  UpdateWindow;
end;

procedure TNotificationWindow.CreateCtrls;
resourcestring
  sDontShowText = 'Don''t show this notification again.';
  sHelpBtnHint = 'Help';
  sCloseBtnHint = 'Hide this window';

  // Creates a new speed button with a character from the Marlett font as
  // caption and with given OnClick event handler.
  function CreateSymbolButton(Symbol: Char; ClickHandler: TNotifyEvent;
    const Hint: string):
    TSpeedButton;
  begin
    Result := TSpeedButton.Create(Self);
    Result.Parent := Self;
    Result.Width := 16;
    Result.Height := 20;
    Result.Flat := True;
    Result.OnClick := ClickHandler;
    Result.Font.Name := 'Marlett';
    Result.Font.Size := 9;
    Result.Caption := Symbol;
    Result.ShowHint := Hint <> '';
    Result.Hint := Hint;
  end;

begin
  // create title label
  fTitleLbl := TLabel.Create(Self);
  fTitleLbl.Parent := Self;
  fTitleLbl.Font.Style := [fsBold];
  fTitleLbl.Font.Size := fTitleLbl.Font.Size + 1;
  fTitleLbl.AutoSize := False;
  fTitleLbl.EllipsisPosition := epEndEllipsis;
  TFontHelper.SetDefaultBaseFont(fTitleLbl.Font);

  // create action button with default size (width changed to suit text)
  fActionBtn := TButton.Create(Self);
  fActionBtn.Parent := Self;
  fActionBtn.Visible := False;
  fActionBtn.Enabled := False;    // enabled only when window displayed

  // create close button with down arrow symbol
  fCloseBtn := CreateSymbolButton(
    CloseButtonChar, CloseBtnClickHandler, sCloseBtnHint
  );

  // create help button with "?" symbol
  fHelpBtn := CreateSymbolButton(
    HelpButtonChar, HelpBtnClickHandler, sHelpBtnHint
  );
  fHelpBtn.Visible := False;

  // create "don't show again" check box
  fDontShowChk := TCheckBox.Create(Self);
  fDontShowChk.Parent := Self;
  fDontShowChk.Checked := False;
  fDontShowChk.Visible := True;
  fDontShowChk.Caption := sDontShowText;

  // create timer used to automatically hide window
  fHideTimer := TTimer.Create(Self);
  fHideTimer.Enabled := False;
  fHideTimer.OnTimer := TimerTickHandler;
end;

destructor TNotificationWindow.Destroy;
begin
  fLightBulb.Free;
  fContentLblList.Free;
  fDisplayLock.SetEvent;
  fDisplayLock.Free;
  inherited;
end;

function TNotificationWindow.GetButtonAction: TCustomAction;
begin
  Result := fActionBtn.Action as TCustomAction;
end;

function TNotificationWindow.GetContent: TArray<string>;
var
  I: Integer;
begin
  SetLength(Result, fContentLblList.Count);
  for I := 0 to Pred(fContentLblList.Count) do
    Result[I] := fContentLblList[I].Caption;
end;

function TNotificationWindow.GetDisplayDontShowChk: Boolean;
begin
  Result := fDontShowChk.Visible;
end;

function TNotificationWindow.GetDisplayTime: Cardinal;
begin
  Result := fHideTimer.Interval;
end;

function TNotificationWindow.GetDontShow: Boolean;
begin
  Result := DisplayDontShowChk and fDontShowChk.Checked
end;

function TNotificationWindow.GetHelpKeyword: string;
begin
  Result := inherited HelpKeyword;
end;

function TNotificationWindow.GetTitle: string;
begin
  Result := fTitleLbl.Caption;
end;

procedure TNotificationWindow.HelpBtnClickHandler(Sender: TObject);
begin
  if HelpKeyword <> '' then
    HelpMgr.ShowHelp(HelpKeyword);
end;

procedure TNotificationWindow.Paint;
begin
  GradientFillCanvas(Canvas, GradColour1, GradColour2, ClientRect, gdVertical);
  Canvas.Pen.Color := BorderColour;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(ClientRect);
  Canvas.BrushCopy(
    fGlyphBounds,
    fLightBulb,
    TRectEx.Create(0, 0, fLightBulb.Width, fLightBulb.Height),
    clFuchsia
  );
end;

procedure TNotificationWindow.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  ParentBounds: TRectEx;
begin
  if Assigned(Parent) then
  begin
    // align at bottom right of parent
    ParentBounds := Parent.ClientRect;
    ALeft := ParentBounds.Right - AWidth - 6;
  end;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TNotificationWindow.SetButtonAction(const Action: TCustomAction);
begin
  fActionBtn.Action := Action;
  UpdateWindow;
end;

procedure TNotificationWindow.SetContent(const AContent: TArray<string>);
var
  Para: string;
  Lbl: TLabel;
begin
  fContentLblList.Clear;  // frees any existing labels
  for Para in AContent do
  begin
    Lbl := TLabel.Create(nil);
    Lbl.Parent := Self;
    TFontHelper.SetDefaultFont(Lbl.Font);
    Lbl.Caption := Para;
    Lbl.AutoSize := False;
    Lbl.WordWrap := True;
    fContentLblList.Add(Lbl);
  end;
  UpdateWindow;
end;

procedure TNotificationWindow.SetDisplayDontShowChk(const Flag: Boolean);
begin
  fDontShowChk.Visible := Flag;
  UpdateWindow;
end;

procedure TNotificationWindow.SetDisplayTime(const MS: Cardinal);
begin
  fHideTimer.Interval := MS;
end;

procedure TNotificationWindow.SetDontShow(const Flag: Boolean);
begin
  fDontShowChk.Checked := Flag;
end;

procedure TNotificationWindow.SetHelpKeyword(const Keyword: string);
begin
  inherited HelpKeyword := Keyword;
  fHelpBtn.Visible := Keyword <> '';
  UpdateWindow;
end;

procedure TNotificationWindow.SetParent(AParent: TWinControl);
begin
  inherited;
  SetBounds(0, 0, Width, Height);
end;

procedure TNotificationWindow.SetTitle(const ATitle: string);
begin
  fTitleLbl.Caption := ATitle;
  UpdateWindow;
end;

procedure TNotificationWindow.SlideIn;
var
  ClosedTop: Integer;
  OpenedTop: Integer;
  Range: Integer;
  Scale: Double;
begin
  if State <> nwsClosed then
    Exit;
  fDisplayLock.ResetEvent;
  fState := nwsOpening;
  ClosedTop := Parent.ClientRect.Bottom + 1;
  OpenedTop := Parent.ClientRect.Bottom - Height;// - 4;
  Range := ClosedTop - OpenedTop;
  Top := ClosedTop;
  Visible := True;
  Scale := SlideStep;
  while Scale < 1.0 - SlideStep do
  begin
    Top := ClosedTop - Round(Range * Scale);
    Scale := Scale + SlideStep;
    Pause(2);
  end;
  Top := OpenedTop;
  fState := nwsOpen;
  fActionBtn.Enabled := True;
  fHideTimer.Enabled := DisplayTime > 0;
end;

procedure TNotificationWindow.SlideOut;
var
  ClosedTop: Integer;
  OpenedTop: Integer;
  Range: Integer;
  Scale: Double;
begin
  if State <> nwsOpen then
    Exit;
  fHideTimer.Enabled := False;
  fActionBtn.Enabled := False;
  fState := nwsClosing;
  ClosedTop := Parent.ClientRect.Bottom + 1;
  OpenedTop := Parent.ClientRect.Bottom - Height;// - 4;
  Range := ClosedTop - OpenedTop;
  Top := OpenedTop;
  Scale := SlideStep;
  while Scale < 1.0 - SlideStep do
  begin
    Top := OpenedTop + Round(Range * Scale);
    Scale := Scale + SlideStep;
    Pause(2);
  end;
  Top := ClosedTop;
  Visible := False;
  fState := nwsClosed;
  PostMessage(Handle, WM_CONCEALED, 0, 0);
end;

procedure TNotificationWindow.TimerTickHandler(Sender: TObject);
begin
  fHideTimer.Enabled := False;
  SlideOut; // does nothing if State <> nwsOpen
end;

procedure TNotificationWindow.UpdateWindow;
const
  MarginLR = 8;
  MarginTB = 8;
  CtrlVSpacing = 8;
  GlyphSize = 24;
  TextLeftOffset = MarginLR + GlyphSize + 8;
var
  Lbl: TLabel;
  NextTop: Integer;
  ParaTop: Integer;
  TextWidth: Integer;
  TopRowHeight: Integer;
begin
  // Find height of "top row" of controls - title and close button plus help
  // button if visible: they are all centred in this row.
  TCtrlArranger.SetLabelHeight(fTitleLbl);
  TopRowHeight := Max(fTitleLbl.Height, fCloseBtn.Height);
  if fHelpBtn.Visible then
    TopRowHeight := Max(TopRowHeight, fHelpBtn.Height);

  // Locate left hand "light bulb" glyph
  fGlyphBounds := TRectEx.CreateBounds(
    MarginLR, MarginTB, GlyphSize, GlyphSize
  );

  // Set "close" and "help" button locations
  if fHelpBtn.Visible then
  begin
    fHelpBtn.Left := Width - MarginLR - fHelpBtn.Width;
    fHelpBtn.Top := MarginTB + (TopRowHeight - fHelpBtn.Height) div 2;
    fCloseBtn.Left := fHelpBtn.Left - fCloseBtn.Width;
  end
  else
    fCloseBtn.Left := Width - MarginLR - fCloseBtn.Width;
  fCloseBtn.Top := MarginTB + (TopRowHeight - fCloseBtn.Height) div 2;

  // Set title size and location
  fTitleLbl.Width := fCloseBtn.Left - 4 - TextLeftOffset;
  fTitleLbl.Left := TextLeftOffset;
  fTitleLbl.Top := MarginTB + (TopRowHeight - fTitleLbl.Height) div 2;

  // Get width available for text / check box in main body
  TextWidth := Width - MarginLR - TextLeftOffset;

  // Create labels to display content paragraphs and locate them one below the
  // other.
  ParaTop := MarginTB + TopRowHeight + CtrlVSpacing;
  for Lbl in fContentLblList do
  begin
    TCtrlArranger.SetLabelHeight(Lbl);
    Lbl.Top := ParaTop;
    Lbl.Left := TextLeftOffset;
    Lbl.Width := TextWidth;
    Inc(ParaTop, Lbl.Height + ParaSpacing);
  end;
  NextTop := ParaTop - ParaSpacing + CtrlVSpacing;

  // Set location and size of action button
  fActionBtn.Visible := Assigned(fActionBtn.Action);
  if fActionBtn.Visible then
  begin
    fActionBtn.Left := TextLeftOffset;
    fActionBtn.Width := StringExtent(fActionBtn.Caption, fActionBtn.Font).cx
      + 28;
    fActionBtn.Top := NextTop;
    NextTop := TCtrlArranger.BottomOf(fActionBtn, CtrlVSpacing);
  end;

  // Set location and size of "don't show again" check box if visible
  if fDontShowChk.Visible then
  begin
    fDontShowChk.Left := TextLeftOffset;
    fDontShowChk.Width := TextWidth;
    fDontShowChk.Top := NextTop;
    NextTop := TCtrlArranger.BottomOf(fDontShowChk, CtrlVSpacing);
  end;

  // Set required window height
  Height := NextTop - CtrlVSpacing + MarginTB;
end;

procedure TNotificationWindow.WMConcealed(var Msg: TMessage);
begin
  fDisplayLock.SetEvent;
end;

end.

