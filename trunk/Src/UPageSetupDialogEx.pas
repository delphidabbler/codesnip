{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements an extension of TPageSetupDialog that customises the dialog box.
}


unit UPageSetupDialogEx;


interface


uses
  // Delphi
  Messages, Dialogs;


type

  {
  TPageSetupDialogEx:
    An extension of the TPageSetupDialog common dialog box that customises the
    dialog box. The class aligns the dialog box over a "host" window, provides
    custom help handling and relocates some buttons.
  }
  TPageSetupDialogEx = class(TPageSetupDialog)
  strict private
    fHelpKeyword: string; // Value of HelpKeyword property
  strict protected
    procedure DoShow; override;
      {Sets parent of dialog box, aligns and focuses it and customises the
      dialog box when it is shown.
      }
    function MessageHook(var Msg: TMessage): Boolean; override;
      {Intercepts dialog box messages. Intercepts help messages to perform
      custom help handling.
        @param Msg [in/out] Dialog box message. Not changed in this method, but
          ancestor methods may make changes.
        @return False to pass message on to dilog's window procedure, True to
          prevent this.
      }
    procedure AlignDlg; virtual;
      {Aligns dialog box over another window.
      }
    procedure AdjustParent; virtual;
      {Sets parent of dialog box to window handle of Owner. If Owner has no
      handle then either active form or main form are used as parent.
      }
    procedure RealignCtrls;
      {Moves help button from bottom left to bottom right of window and hides
      printers button only if printers button is disabled. This tidies dialog
      and places help in logical position given its place in tab order.
      }
  published
    property HelpKeyword: string read fHelpKeyword write fHelpKeyword;
      {Help keyword used when Help button is clicked}
  end;


implementation


uses
  // Delphi
  Windows, Dlgs,
  // Project
  UCommonDlg, UDlgHelper, UStructs;


{ TPageSetupDialogEx }

procedure TPageSetupDialogEx.AdjustParent;
  {Sets parent of dialog box to window handle of Owner. If Owner has no handle
  then either active form or main form are used as parent.
  }
begin
  TDlgHelper.SetDlgParentToOwner(Self);
end;

procedure TPageSetupDialogEx.AlignDlg;
  {Aligns dialog box over another window.
  }
begin
  TDlgAligner.AlignToOwner(Self);
end;

procedure TPageSetupDialogEx.DoShow;
  {Sets parent of dialog box, aligns and focuses it and customises the dialog
  box when it is shown.
  }
begin
  inherited;
  // Adjust window's parent to be window handle of owning control
  AdjustParent;
  // Align dialog over "parent" window
  AlignDlg;
  // Tweak the dialog's controls
  RealignCtrls;
  // We need to set focus expicitly because dialog box doesn't handle keyboard
  // input correctly if we don't
  SetFocus(Handle);
end;

function TPageSetupDialogEx.MessageHook(var Msg: TMessage): Boolean;
  {Intercepts dialog box messages. Intercepts help messages to perform custom
  help handling.
    @param Msg [in/out] Dialog box message. Not changed in this method, but
      ancestor methods may make changes.
    @return False to pass message on to dilog's window procedure, True to
      prevent this.
  }
begin
  if TCommonDlgHelper.IsHelpMessage(Msg) then
    Result := TCommonDlgHelper.ShowHelp(HelpKeyword)
  else
    Result := inherited MessageHook(Msg);
end;

procedure TPageSetupDialogEx.RealignCtrls;
  {Moves help button to bottom right of window only if Printers button either
  does not exist or is disabled. OK and cancel buttons are moved as required.
  This tidies the dialog and places help in logical position given its place in
  tab order.
  }
const
  // Ids of buttons to be manipulated. Ids found by experimentation
  cOKBtnId = Windows.IDOK;          // id of OK button
  cCancelBtnId = Windows.IDCancel;  // id of Cancel button
  cHelpBtnId = Dlgs.pshHelp;        // id of Help button
  cPrinterBtnId = Dlgs.psh3;        // id of Printers button

  // ---------------------------------------------------------------------------
  procedure ScreenToClientRect(var R: TRect);
    {Converts a rectangle in screen co-ordinates to dialog box client area
    co-ordinates.
      @param R [in/out] Converted rectangle. Passed in in screen co-ordinates
        and out in client co-ordinates.
    }
  begin
    ScreenToClient(Handle, R.TopLeft);
    ScreenToClient(Handle, R.BottomRight);
  end;

  function GetWindowClientRect(const Wnd: HWND): TRectEx;
    {Gets bounding rectangle of a window in co-ordinates relative to dialog box
    client area.
      @param Wnd [in] Handle of window whose bounds are required.
      @return Required bounding rectangle.
    }
  var
    R: TRect; // required rectangle as a TRect
  begin
    GetWindowRect(Wnd, R);
    ScreenToClientRect(R);
    Result := R;
  end;

  procedure RelocateBtn(const BtnWnd: HWND; const R: TRect);
    {Relocates / resizes a button.
      @param BtnWnd [in] Window handle of button.
      @param R [in] Bounding rectangle of relocated button.
    }
  var
    Pl: TWindowPlacement; // information about placement of window being moved
  begin
    FillChar(Pl, SizeOf(Pl), #0);
    Pl.Length := SizeOf(TWindowPlacement);
    Pl.rcNormalPosition := R;
    Pl.showCmd := SW_SHOW;
    SetWindowPlacement(BtnWnd, @Pl);
  end;

  procedure ReplacePrinterBtnWithHelp(const HPrinterBtn, HHelpBtn: HWND);
    {Replaces dialog's printer button with help button.
      @param HPrinterBtn [in] Window handle of printer button.
      @param HHelpBtn [in] Window handle of help button.
    }
  var
    RPrinterBtn: TRectEx; // printers button's bounding rectangle
  begin
    Assert(HPrinterBtn <> 0,
      ClassName + '.RealignCtrls:ReplacePrinterBtnWithHelp: HPrinterBtn = 0');
    Assert(HHelpBtn <> 0,
      ClassName + '.RealignCtrls:ReplacePrinterBtnWithHelp: HHelpBtn = 0');
    // Get bounds of printer button
    RPrinterBtn := GetWindowClientRect(HPrinterBtn);
    // Hide the printers button
    ShowWindow(HPrinterBtn, SW_HIDE);
    // Move help button to printers button location
    RelocateBtn(HHelpBtn, RPrinterBtn);
  end;

  procedure PlaceHelpBtnAfterCancel(HHelpBtn: HWND);
    {Relocates OK and Cancel buttons left and places help button where cancel
    button was originally placed. Does nothing if OK or Cancel button can't be
    found.
      @param HHelpBtn [in] Handle of help button.
    }
  var
    HOKBtn: HWND;           // window handle of OK button
    HCancelBtn: HWND;       // window handle of Cancel button
    ROKBtn: TRectEx;        // bounding rectangle of OK button
    RCancelBtn: TRectEx;    // bounding rectangle of Cancel button
    RHelpBtn: TRectEx;      // bounding rectangle of Help button
    BtnLeftShift: Integer;  // amount to shift OK cancel buttons left
  begin
    // Get handles to OK and Cancel button: escape if can't be found
    HOKBtn := GetDlgItem(Handle, cOKBtnId);
    HCancelBtn := GetDlgItem(Handle, cCancelBtnId);
    if (HOKBtn = 0) or (HCancelBtn = 0) then
      Exit;
    // Find bounding rectangles of OK and Cancel buttons
    ROKBtn := GetWindowClientRect(HOKBtn);
    RCancelBtn := GetWindowClientRect(HCancelBtn);
    // Find how far we need to shift OK and Cancel left: this is size of a
    // button + distance between them
    BtnLeftShift := Abs(ROKBtn.Left - RCancelBtn.Left);
    // Get bounding rectangle of help button: this is present location of Cancel
    // button
    RHelpBtn := RCancelBtn;
    // Shift bounding rectangles of OK and Cancel button left to make room for
    // help button
    ROKBtn.OffsetBy(-BtnLeftShift, 0);
    RCancelBtn.OffsetBy(-BtnLeftShift, 0);
    // Move all buttons to required positions
    RelocateBtn(HHelpBtn, RHelpBtn);
    RelocateBtn(HCancelBtn, RCancelBtn);
    RelocateBtn(HOKBtn, ROKBtn);
  end;
  // ---------------------------------------------------------------------------

var
  HHelpBtn: HWND;       // help button's window handle
  HPrinterBtn: HWND;    // printers button's window handle
begin
  // Get window handle of help button: exit if there's no help button
  HHelpBtn := GetDlgItem(Handle, cHelpBtnId);
  if HHelpBtn = 0 then
    Exit;
  // Check if printer button exists and move help button there if disabled
  HPrinterBtn := GetDlgItem(Handle, cPrinterBtnId);
  if HPrinterBtn <> 0 then
  begin
    // we have printer button: replace it only if disabled, otherwise do nothing
    if not IsWindowEnabled(HPrinterBtn) then
      ReplacePrinterBtnWithHelp(HPrinterBtn, HHelpBtn);
  end
  else
    // no printer button align help button after cancel
    PlaceHelpBtnAfterCancel(HHelpBtn);
end;

end.

