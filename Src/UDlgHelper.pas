{
 * UDlgHelper.pas
 *
 * Implements "static" classes that help to manipulate dialog boxes.
 *
 * Originally named UDlgAligner.pas. Renamed as UDlgHelper.pas as at v1.1.
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
 * The Original Code is UDlgHelper.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UDlgHelper;


interface


uses
  // Delphi
  Classes, Windows,
  // Project
  UBaseObjects, UStructs;


type


  {
  TDlgHelper:
    Static class that sets parent window of a dialog box.
  }
  TDlgHelper = class(TNoConstructObject)
  public
    class procedure SetDlgParent(const Dlg, Parent: TComponent);
      {Sets parent of a dialog box to window associated with a parent component.
        @param Dlg [in] Dialog box that will have parent set.
        @param Parent [in] Control whose window handle is to be parent of Dlg.
          If Parent is nil or isn't support either active form or main form are
          used.
      }
    class procedure SetDlgParentToOwner(const Dlg: TComponent);
      {Makes owner of a dialog box its parent.
        @param Dlg [in] Dialog box whose parent is to be set to owner. If Dlg's
          Owner is nil or not supported either active form or main form is used.
      }
  end;

  {
  TDlgAligner:
    "Static" class that aligns dialog boxes over host windows.
  }
  TDlgAligner = class(TNoPublicConstructObject)
  strict private
    fDialog: IInterface;  // Object representing window to be aligned
    fHostWdw: IInterface; // Object representing host window for alignment
    procedure GetDialogBounds(out DlgBounds: TRectEx);
      {Gets bounding rectangle of dialog box to be aligned.
        @param DlgBounds [out] Set to required bounding rectangle in screen
          coordinates.
      }
    procedure OffsetDialog(var DlgBounds: TRectEx);
      {Offsets a dialog's bounding rectangle according to whether host is a
      dialog box or not.
        @param DlgBounds [in/out] Current dialog bounds are passed in and offset
          bounding rectangle passed out.
      }
    procedure FitToWorkArea(var DlgBounds: TRectEx);
      {Ensures that a bounding rectangle of a dialog box fits within work area.
        @param DlgBounds [in/out] Bounding rectangle to be adjusted is passed in
          and may be modified if it does not fit work area.
      }
    procedure AdjustWindowPosition(const DlgBounds: TRectEx);
      {Adjusts position (and optionally size) of dialog box being aligned
      according to bounding rectangle.
        @param DlgBounds [in] Required bounding rectangle of dialog box.
      }
    procedure DoAlign(const Dlg, Host: TComponent);
      {Performs alignment of a window over a host window.
        @param Dlg [in] Dialog box to be aligned. Must not be nil.
        @param Host [in] Window over which window is to be aligned. May be nil.
      }
  public
    class procedure Align(const Dlg, Host: TComponent);
      {Aligns a dialog window over a host window. If host window is a dialog box
      the window is offset from dialog's top left corner, otherwise the window
      is "centered" over the host window. If host window is not supported either
      active form or application's main form are used as host.
        @param Dlg [in] Dialog box to be aligned. Must not be nil.
        @param Host [in] Window over which window is to be aligned. May be nil.
      }
    class procedure AlignToOwner(const Dlg: TComponent);
      {Aligns a dialog box window over window represented by dialog's Owner
      property. If Owner window is a dialog box the dialog window is offset from
      dialog's top left corner, otherwise the window is "centered" over the
      owner window. If Owner does not have a suitable associated window either
      active form or application's main form are used in place of owner.
        @param Dlg [in] Dialog box to be aligned to its owner. Dlg must not be
          nil but its owner may be.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Controls, Forms, Dialogs,
  // Project
  UExceptions;


type

  {
  IWindowInfo:
    Abstraction of a window that provides information about the window.
  }
  IWindowInfo = interface(IInterface)
    ['{8E0F5AA6-88AC-4734-99C0-2253E7CF665A}']
    function IsDialog: Boolean;
      {Check if window is a dialog box.
        @return True if dialog box, False if not.
      }
    function BoundsRect: TRectEx;
      {Get bounding rectangle of window.
        @return Required rectangle in screen co-ordinates.
      }
    function Handle: THandle;
      {Get handle of window.
        @return Required handle or 0 if no handle supported.
      }
  end;

  {
  IAlignableWindow:
    Abstraction of a window that can be aligned above another window.
  }
  IAlignableWindow = interface(IInterface)
    ['{E65CAAEE-F782-4489-B2DF-2B8C4121825F}']
    function BoundsRect: TRectEx;
      {Get bounding rectangle of window.
        @return Required rectangle in screen co-ordinates.
      }
    procedure AdjustWindow(const Bounds: TRectEx);
      {Adjust window to have a new bounding rectangle.
        @param Bounds [in] New bounds for window, in screen co-ordinates.
      }
  end;

  {
  TWindowInfoFactory:
    Factory class that creates IWindowInfo instances from appropriate classes.
  }
  TWindowInfoFactory = class(TNoConstructObject)
  public
    class function Instance(const Host: TComponent): IWindowInfo;
      {Creates a suitable IWindowInfo instance for a host component.
        @param Host [in] Component for which we need IWindowInfo instance. Host
          may be any TComponent. If it is not a TWinControl (including
          TCustomForm) or a TCommonDialog then either current active form or
          main form are used as host.
        @return Required instance.
      }
  end;

  {
  TAlignableDialogFactory:
    Factory class that creates IAlignableWindow instances from appropriate
    classes.
  }
  TAlignableDialogFactory = class(TNoConstructObject)
  public
    class function Instance(const Dlg: TComponent): IAlignableWindow;
      {Creates a suitable IAlignableWindow instance for a dialog box component.
        @param Dlg [in] Dialog component for which we need IWindowInfo instance.
          Dlg must be either a TCustomForm or a TCommonDialog.
        @return Required instance.
        @except EBug raised if Dlg is not one of required types.
      }
  end;

  {
  TFormWindow:
    Class that implements IAlignableWindow and IWindowInfo interfaces for a
    form. Provides information about and positions the form.
  }
  TFormWindow = class(TInterfacedObject,
    IAlignableWindow, IWindowInfo
  )
  strict private
    fForm: TCustomForm; // Reference to encapsulated form
  protected
    { IAlignableWindow and IWindowInfo methods }
    function BoundsRect: TRectEx;
      {Get bounding rectangle of form.
        @return Required rectangle in screen co-ordinates.
      }
    procedure AdjustWindow(const Bounds: TRectEx);
      {Adjust form to have a new bounding rectangle. If form is a dialog box
      then top left corner is moved to new position but size is not changed.
        @param Bounds [in] New bounds for form, in screen co-ordinates.
      }
    function IsDialog: Boolean;
      {Check if form is a dialog box.
        @return True if dialog box, False if not.
      }
    function Handle: THandle;
      {Get handle of form.
        @return Required handle.
      }
  public
    constructor Create(const Form: TCustomForm);
      {Class constructor. Sets up object.
        @param Form [in] Reference to form to be encapsulated.
      }
  end;

  {
  TWinControlWindow:
    Class that implements IWindowInfo interface for a TWinControl and
    descendants (except TCustomForm). Provides information about the control.
  }
  TWinControlWindow = class(TInterfacedObject,
    IWindowInfo
  )
  strict private
    fWinCtrl: TWinControl;  // Reference to encapsulated control
  protected
    { IWindowInfo methods }
    function IsDialog: Boolean;
      {Check if control is a dialog box.
        @return False. Control never a dialog box.
      }
    function BoundsRect: TRectEx;
      {Get bounding rectangle of control.
        @return Required rectangle in screen co-ordinates.
      }
    function Handle: THandle;
      {Get handle of window.
        @return Required handle.
      }
  public
    constructor Create(const WinCtrl: TWinControl);
      {Class constructor. Sets up object.
        @param WinCtrl [in] Control to be encapsulated. Must not be a
          TCustomForm.
      }
  end;

  {
  TCommonDialogWindow:
    Class that implements IAlignableWindow and IWindowInfo interfaces for a
    common dialog box (except TOpenDialog). Provides information about and
    positions the dialog.
  }
  TCommonDialogWindow = class(TInterfacedObject,
    IWindowInfo, IAlignableWindow
  )
  strict protected
    fDlg: TCommonDialog;  // Reference to common dialog to be encapsulated
  protected
    { IWindowInfo and IAlignableWindow methods }
    function IsDialog: Boolean;
      {Check if window is a dialog box.
        @return True. Common dialogs are always dialogs.
      }
    function BoundsRect: TRectEx;
      {Get bounding rectangle of common dialog
        @return Required rectangle in screen co-ordinates.
      }
    procedure AdjustWindow(const Bounds: TRectEx);
      {Adjust common dialog to have same top corner as bounding rectangle. Any
      change of size is ignored.
        @param Bounds [in] New bounds for control, in screen co-ordinates.
      }
    function Handle: THandle; virtual;
      {Get dialog box's window handle.
        @return Required window handle.
      }
  public
    constructor Create(const Dlg: TCommonDialog);
      {Class constructor. Sets up object.
        @param Dlg [in] Reference to common dialog to be encapsulated. Must not
          be called directly with a TOpenDialog.
      }
  end;

  {
  TOpenDialogWindow:
    Class that implements IAlignableWindow and IWindowInfo interfaces for a
    TOpenDialog. Provides information about and positions the dialog.
  }
  TOpenDialogWindow = class(TCommonDialogWindow,
    IWindowInfo, IAlignableWindow
  )
  protected
    { IWindowInfo method }
    function Handle: THandle; override;
      {Get dialog box's window handle. Deals correctly with dialogas with
        explorer hooks and/or customisation templates.
        @return Required window handle.
      }
  public
    constructor Create(const Dlg: TOpenDialog);
      {Class constructor. Sets up object.
        @param Dlg [in] Reference to open dialog to be encapsulated.
      }
  end;


{ TDlgHelper }

class procedure TDlgHelper.SetDlgParent(const Dlg, Parent: TComponent);
  {Sets parent of a dialog box to window associated with a parent component.
    @param Dlg [in] Dialog box that will have parent set.
    @param Parent [in] Control whose window handle is to be parent of Dlg. If
      Parent is nil or isn't support either active form or main form are used.
  }
var
  ParentWindow: IWindowInfo;  // encapsulates parent window
  DlgWindow: IWindowInfo;     // encapsulates dialog box window
begin
  Assert(Assigned(Dlg), ClassName + '.SetDlgParent: Dlg is nil');
  DlgWindow := TWindowInfoFactory.Instance(Dlg);
  ParentWindow := TWindowInfoFactory.Instance(Parent);
  SetWindowLong(DlgWindow.Handle, GWL_HWNDPARENT, ParentWindow.Handle);
end;

class procedure TDlgHelper.SetDlgParentToOwner(const Dlg: TComponent);
  {Makes owner of a dialog box its parent.
    @param Dlg [in] Dialog box whose parent is to be set to owner. If Dlg's
      Owner is nil or not supported either active form or main form is used.
  }
begin
  SetDlgParent(Dlg, Dlg.Owner);
end;

{ TDlgAligner }

procedure TDlgAligner.AdjustWindowPosition(const DlgBounds: TRectEx);
  {Adjusts position (and optionally size) of dialog box being aligned according
  to bounding rectangle.
    @param DlgBounds [in] Required bounding rectangle of dialog box.
  }
begin
  (fDialog as IAlignableWindow).AdjustWindow(DlgBounds);
end;

class procedure TDlgAligner.Align(const Dlg, Host: TComponent);
  {Aligns a dialog window over a host window. If host window is a dialog box the
  window is offset from dialog's top left corner, otherwise the window is
  "centered" over the host window. If host window is not supported either active
  form or application's main form are used as host.
    @param Dlg [in] Dialog box to be aligned. Must not be nil.
    @param Host [in] Window over which window is to be aligned. May be nil.
  }
begin
  Assert(Assigned(Dlg), ClassName + '.Align: Dlg is nil');
  with InternalCreate do
  try
    DoAlign(Dlg, Host);
  finally
    Free;
  end;
end;

class procedure TDlgAligner.AlignToOwner(const Dlg: TComponent);
  {Aligns a dialog box window over window represented by dialog's Owner
  property. If Owner window is a dialog box the dialog window is offset from
  dialog's top left corner, otherwise the window is "centered" over the owner
  window. If Owner does not have a suitable associated window either active form
  or application's main form are used in place of owner.
    @param Dlg [in] Dialog box to be aligned to its owner. Dlg must not be nil
      but its owner may be.
  }
begin
  Assert(Assigned(Dlg), ClassName + '.AlignToOwner: Dlg is nil');
  Align(Dlg, Dlg.Owner);
end;

procedure TDlgAligner.DoAlign(const Dlg, Host: TComponent);
  {Performs alignment of a window over a host window.
    @param Dlg [in] Dialog box to be aligned. Must not be nil.
    @param Host [in] Window over which window is to be aligned. May be nil.
  }
var
  DlgBounds: TRectEx; // bounding rectangle of dialog box
begin
  Assert(Assigned(Dlg), ClassName + '.DoAlign: Dlg is nil');
  // Encapsulate window and host window
  fDialog := TAlignableDialogFactory.Instance(Dlg);
  fHostWdw := TWindowInfoFactory.Instance(Host);
  // Do the alignment
  GetDialogBounds(DlgBounds);
  OffsetDialog(DlgBounds);
  FitToWorkArea(DlgBounds);
  AdjustWindowPosition(DlgBounds);
end;

procedure TDlgAligner.FitToWorkArea(var DlgBounds: TRectEx);
  {Ensures that a bounding rectangle of a dialog box fits within work area.
    @param DlgBounds [in/out] Bounding rectangle to be adjusted is passed in and
      may be modified if it does not fit work area.
  }
var
  WorkArea: TRectEx;  // desktop work area
begin
  WorkArea := Screen.MonitorFromRect(DlgBounds).WorkareaRect;
  if DlgBounds.Right > WorkArea.Right then
    DlgBounds.OffsetBy(WorkArea.Right - DlgBounds.Right, 0);
  if DlgBounds.Left < WorkArea.Left then
    DlgBounds.OffsetBy(WorkArea.Left - DlgBounds.Left, 0);
  if DlgBounds.Bottom > WorkArea.Bottom then
    DlgBounds.OffsetBy(0, WorkArea.Bottom - DlgBounds.Bottom);
  if DlgBounds.Top < WorkArea.Top then
    DlgBounds.OffsetBy(0, WorkArea.Top - DlgBounds.Top);
end;

procedure TDlgAligner.GetDialogBounds(out DlgBounds: TRectEx);
  {Gets bounding rectangle of dialog box to be aligned.
    @param DlgBounds [out] Set to required bounding rectangle in screen
      coordinates.
  }
begin
  DlgBounds := (fDialog as IAlignableWindow).BoundsRect;
end;

procedure TDlgAligner.OffsetDialog(var DlgBounds: TRectEx);
  {Offsets a dialog's bounding rectangle according to whether host is a dialog
  box or not.
    @param DlgBounds [in/out] Current dialog bounds are passed in and offset
      bounding rectangle passed out.
  }
const
  // Offsets used when aligning over a dialog box
  cDlgOffset: TPoint = (X: 40; Y: 40);
var
  HostBounds: TRectEx;  // bounding rectangle of AlignCtrl in screen co-ords
begin
  HostBounds := (fHostWdw as IWindowInfo).BoundsRect;
  if (fHostWdw as IWindowInfo).IsDialog then
    // Aligning over dialog box: offset down and to left
    DlgBounds.OffsetBy(
      HostBounds.Left - DlgBounds.Left + cDlgOffset.X,
      HostBounds.Top - DlgBounds.Top + cDlgOffset.Y
    )
  else
    // Aligning over a main window: "centre" dialog over window
    DlgBounds.OffsetBy(
      HostBounds.Left - DlgBounds.Left +
        (HostBounds.Width - DlgBounds.Width) div 2,
      HostBounds.Top - DlgBounds.Top +
        (HostBounds.Height - DlgBounds.Height) div 3
    )
end;

{ TWindowInfoFactory }

class function TWindowInfoFactory.Instance(
  const Host: TComponent): IWindowInfo;
  {Creates a suitable IWindowInfo instance for a host component.
    @param Host [in] Component for which we need IWindowInfo instance. Host may
      be any TComponent. If it is not a TWinControl (including TCustomForm) or a
      TCommonDialog then either current active form or main form are used as
      host.
    @return Required instance.
  }
begin
  Result := nil;
  if Host is TCustomForm then
    Result := TFormWindow.Create(Host as TCustomForm)
  else if Host is TWinControl then
    Result := TWinControlWindow.Create(Host as TWinControl)
  else if Host is TOpenDialog then
    Result := TOpenDialogWindow.Create(Host as TOpenDialog)
  else if Host is TCommonDialog then
    Result := TCommonDialogWindow.Create(Host as TCommonDialog);
  if not Assigned(Result) then
  begin
    if Assigned(Screen.ActiveCustomForm) then
      Result := TFormWindow.Create(Screen.ActiveCustomForm)
    else if Assigned(Application.MainForm) then
      Result := TFormWindow.Create(Application.MainForm)
  end;
  Assert(Assigned(Result), ClassName + '.Instance: Can''t create instance');
end;

{ TAlignableDialogFactory }

class function TAlignableDialogFactory.Instance(
  const Dlg: TComponent): IAlignableWindow;
  {Creates a suitable IAlignableWindow instance for a dialog box component.
    @param Dlg [in] Dialog component for which we need IWindowInfo instance. Dlg
      must be either a TCustomForm or a TCommonDialog.
    @return Required instance.
    @except EBug raised if Dlg is not one of required types.
  }
begin
  Assert(Assigned(Dlg), ClassName + '.Instance: Dlg is nil');
  if Dlg is TCustomForm then
    Result := TFormWindow.Create(Dlg as TCustomForm)
  else if Dlg is TOpenDialog then
    Result := TOpenDialogWindow.Create(Dlg as TOpenDialog)
  else if Dlg is TCommonDialog then
    Result := TCommonDialogWindow.Create(Dlg as TCommonDialog)
  else
    raise EBug.CreateFmt(
      '%0:s.Instance: Unsupported WdwCtrl type %1:s', [ClassName, Dlg.ClassName]
    );
end;

{ TFormWindow }

procedure TFormWindow.AdjustWindow(const Bounds: TRectEx);
  {Adjust form to have a new bounding rectangle. If form is a dialog box then
  top left corner is moved to new position but size is not changed.
    @param Bounds [in] New bounds for form, in screen co-ordinates.
  }
begin
  if IsDialog then
    fForm.SetBounds(Bounds.Left, Bounds.Top, fForm.Width, fForm.Height)
  else
    fForm.BoundsRect := Bounds;
end;

function TFormWindow.BoundsRect: TRectEx;
  {Get bounding rectangle of form.
    @return Required rectangle in screen co-ordinates.
  }
begin
  Result := fForm.BoundsRect;
end;

constructor TFormWindow.Create(const Form: TCustomForm);
  {Class constructor. Sets up object.
    @param Form [in] Reference to form to be encapsulated.
  }
begin
  Assert(Assigned(Form), ClassName + '.Create: Form is nil');
  inherited Create;
  fForm := Form;
end;

function TFormWindow.Handle: THandle;
  {Gets handle of form.
    @return Required handle.
  }
begin
  Result := fForm.Handle;
end;

function TFormWindow.IsDialog: Boolean;
  {Check if form is a dialog box.
    @return True if dialog box, False if not.
  }
begin
  Result := fForm.BorderStyle in [bsDialog, bsSizeToolWin, bsToolWindow];
end;

{ TCommonDialogWindow }

procedure TCommonDialogWindow.AdjustWindow(const Bounds: TRectEx);
  {Adjust common dialog to have same top corner as bounding rectangle. Any
  change of size is ignored.
    @param Bounds [in] New bounds for control, in screen co-ordinates.
  }
begin
  SetWindowPos(
    Handle,                     // window to position
    0,                          // only required if setting z-order
    Bounds.Left, Bounds.Top,    // X and Y co-ords of window
    0, 0,                       // only required if setting size of window
    SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER  // flags saying what to do
  );
end;

function TCommonDialogWindow.BoundsRect: TRectEx;
  {Get bounding rectangle of common dialog
    @return Required rectangle in screen co-ordinates.
  }
var
  Rect: TRect;  // bounding rectangle
begin
  GetWindowRect(Handle, Rect);
  Result := Rect;
end;

constructor TCommonDialogWindow.Create(const Dlg: TCommonDialog);
  {Class constructor. Sets up object.
    @param Dlg [in] Reference to common dialog to be encapsulated. Must not be
      called directly with a TOpenDialog.
  }
begin
  Assert(Assigned(Dlg), ClassName + '.Create: Dlg is nil');
  Assert((ClassType <> TCommonDialogWindow) or not (Dlg is TOpenDialog),
    ClassName + '.Create: Dlg cannot be a TOpenDialog');
  inherited Create;
  fDlg := Dlg;
end;

function TCommonDialogWindow.Handle: THandle;
  {Gets dialog box's window handle.
    @return Required window handle.
  }
begin
  Result := fDlg.Handle;
end;

function TCommonDialogWindow.IsDialog: Boolean;
  {Check if window is a dialog box.
    @return True. Common dialogs are always dialogs!
  }
begin
  Result := True;
end;

{ TOpenDialogWindow }

constructor TOpenDialogWindow.Create(const Dlg: TOpenDialog);
  {Class constructor. Sets up object.
    @param Dlg [in] Reference to open dialog to be encapsulated.
  }
begin
  inherited Create(Dlg);
end;

function TOpenDialogWindow.Handle: THandle;
  {Get dialog box's window handle. Deals correctly with dialogas with explorer
  hooks and/or customisation templates.
    @return Required window handle.
  }
begin
  if NewStyleControls and
    not (ofOldStyleDialog in (fDlg as TOpenDialog).Options)  then
    // For explorer style dialogs with explorer hooks and / or customisation
    // templates the main window handle is the parent of the handle returned
    // from dialog's Handle property. Delphi always provides an explorer hook
    // for new style dialog boxes, so we can assume we need the parent handle
    Result := GetParent(inherited Handle)
  else
    Result := inherited Handle;
end;

{ TWinControlWindow }

function TWinControlWindow.BoundsRect: TRectEx;
  {Get bounding rectangle of control.
    @return Required rectangle in screen co-ordinates.
  }
begin
  if fWinCtrl.Parent = nil then
    // No parent: bounds are already in screen coords
    Result := fWinCtrl.BoundsRect
  else
  begin
    // Parented control: bounds are relative to parent => map to screen coords
    Result.TopLeft := fWinCtrl.ClientToScreen(
      fWinCtrl.BoundsRect.TopLeft
    );
    Result.BottomRight := fWinCtrl.ClientToScreen(
      fWinCtrl.BoundsRect.BottomRight
    );
  end;
end;

constructor TWinControlWindow.Create(const WinCtrl: TWinControl);
  {Class constructor. Sets up object.
    @param WinCtrl [in] Control to be encapsulated. Must not be a TCustomForm.
  }
begin
  Assert(Assigned(WinCtrl), ClassName + '.Create: WinCtrl is nil');
  Assert(not (WinCtrl is TCustomForm),
    ClassName + '.Create: WinCtrl cannot be a TCustomForm');
  inherited Create;
  fWinCtrl := WinCtrl;
end;

function TWinControlWindow.Handle: THandle;
  {Gets handle of window.
    @return Required handle.
  }
begin
  Result := fWinCtrl.Handle;
end;

function TWinControlWindow.IsDialog: Boolean;
  {Check if control is a dialog box.
    @return False. Control never a dialog box.
  }
begin
  Result := False;
end;

end.

