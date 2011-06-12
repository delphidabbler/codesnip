{
 * UDlgHelper.pas
 *
 * Implements "static" classes that help to manipulate dialog boxes:
 *  + TDlgHelper sets a dialpg box's parent window.
 *  + TDlgAligner aligns a dialog box over a host window.
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
 * Portions created by the Initial Developer are Copyright (C) 2007-2011 Peter
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
  ///  <summary>
  ///  Static class that sets parent window of a dialog box.
  ///  </summary>
  TDlgHelper = class(TNoConstructObject)
  public
    ///  <summary>Sets parent window of dialog Dlg to the window associated with
    ///  Parent.</summary>
    ///  <remarks>If Parent is nil or is not recognised as a component that has
    ///  an associated window then either the active form or, failing that, the
    ///  application's main form are used.</remarks>
    class procedure SetDlgParent(const Dlg, Parent: TComponent);
    ///  <summary>Makes the window associated with the owner of dialog box Dlg
    ///  its parent window.</summary>
    ///  <remarks>If Dlg's owner is nil or is not recognised as a component that
    ///  has an associated window then either the active form or, failing that,
    ///  the application's main form are used.</remarks>
    class procedure SetDlgParentToOwner(const Dlg: TComponent);
  end;

type
  ///  <summary>
  ///  Class that aligns dialog boxes over host windows.
  ///  </summary>
  TDlgAligner = class(TNoPublicConstructObject)
  strict private
    var
      ///  <summary>Object representing window to be aligned.</summary>
      fDialog: IInterface;
      ///  <summary>Object representing host window for alignment.</summary>
      fHostWdw: IInterface;
    ///  <summary>Gets bounding rectangle of dialog box to be aligned.</summary>
    procedure GetDialogBounds(out DlgBounds: TRectEx);
    ///  <summary>Offsets a dialog's bounding rectangle relative to host window.
    ///  </summary>
    ///  <remarks>How rectangle is offset depends on whether host is a dialog
    ///  box or not.</remarks>
    procedure OffsetDialog(var DlgBounds: TRectEx);
    ///  <summary>Ensures that a bounding rectangle of a dialog box fits within
    ///  screen work area.</summary>
    procedure FitToWorkArea(var DlgBounds: TRectEx);
    ///  <summary>Adjusts position (and possibly size) of dialog box being
    ///  aligned according to bounding rectangle.</summary>
    procedure AdjustWindowPosition(const DlgBounds: TRectEx);
    ///  <summary>Aligns dialog box relative to host.</summary>
    procedure PerformAlignment;
  strict protected
    ///  <summary>Parameterless constructor. Must not be called.</summary>
    ///  <remarks>Raises ENoConstructException if called.</remarks>
    constructor InternalCreate; overload;
    ///  <summary>Constructs object for dialog boxes and host windows that both
    ///  descend from TComponent.</summary>
    ///  <remarks>Creates appropriate window and aligner instances.</remarks>
    constructor InternalCreate(const Dlg, Host: TComponent); overload;
    ///  <summary>Constructs object for dialog boxes defined by window handle
    ///  and hosts that descend from TComponent.</summary>
    ///  <remarks>Creates appropriate window and aligner instances.</remarks>
    constructor InternalCreate(const DlgHandle: THandle;
      const Host: TComponent); overload;
  public
    ///  <summary>Aligns a dialog box over a host window.</summary>
    ///  <remarks>
    ///  <para>If host window is also a dialog box then the dialog box being
    ///  aligned is offset from top left corner of host. If host window is not
    ///  a dialog box then dialog box is centred over the host window.</para>
    ///  <para>If Host is nil or does not have an associated window then the
    ///  dialog box is aligned over the active form or, failing that the
    ///  application's main form.</para>
    ///  <para>Both dialog box and host must descend from TComponent.</para>
    ///  </remarks>
    class procedure Align(const Dlg, Host: TComponent); overload;
    ///  <summary>Aligns a dialog box over a host window.</summary>
    ///  <remarks>
    ///  <para>If host window is also a dialog box then the dialog box being
    ///  aligned is offset from top left corner of host. If host window is not
    ///  a dialog box then dialog box is centred over the host window.</para>
    ///  <para>If Host is nil or does not have an associated window then the
    ///  dialog box is aligned over the active form or, failing that the
    ///  application's main form.</para>
    ///  <para>Dialog box is identified by its window handle and host must be a
    ///  TComponent descendant.</para>
    ///  </remarks>
    class procedure Align(const DlgHandle: THandle; const Host: TComponent);
      overload;
    ///  <summary>Aligns a dialog box window over the window associated with the
    ///  dialog's Owner component.</summary>
    ///  <remarks>
    ///  <para>If owner window is also a dialog box then the dialog box being
    ///  aligned is offset from top left corner of owner window. If owner window
    ///  is not a dialog box then dialog box is centred over the owner window.
    ///  </para>
    ///  <para>If dialog's Owner is nil or does not have an associated window
    ///  then the dialog box is aligned over the active form or, failing that
    ///  the application's main form.</para>
    ///  </remarks>
    class procedure AlignToOwner(const Dlg: TComponent);
  end;


implementation


uses
  // Delphi
  SysUtils, Controls, Forms, Dialogs,
  // Project
  UExceptions;


type
  ///  <summary>
  ///  Abstraction of a window that provides information about the window.
  ///  </summary>
  IWindowInfo = interface(IInterface)
    ['{8E0F5AA6-88AC-4734-99C0-2253E7CF665A}']
    ///  <summary>Checks if window is a dialog box or not.</summary>
    function IsDialog: Boolean;
    ///  <summary>Get window's bounding rectangle.</summary>
    function BoundsRect: TRectEx;
    ///  <summary>Gets window's handle.</summary>
    function Handle: THandle;
  end;

type
  ///  <summary>
  ///  Abstraction of a window that can be aligned above another window.
  ///  </summary>
  IAlignableWindow = interface(IInterface)
    ['{E65CAAEE-F782-4489-B2DF-2B8C4121825F}']
    ///  <summary>Gets window's bounding rectangle.</summary>
    function BoundsRect: TRectEx;
    ///  <summary>Adjusts window's bounding rectangle to given value, in screen
    ///  co-ordinates.</summary>
    ///  <remarks>May ignore any change of size.</remarks>
    procedure AdjustWindow(const Bounds: TRectEx);
  end;

type
  ///  <summary>
  ///  Factory class that creates IWindowInfo instances from appropriate
  ///  classes.
  ///  </summary>
  TWindowInfoFactory = class(TNoConstructObject)
  public
    ///  <summary>Creates a suitable IWindowInfo instance for given host
    ///  component.</summary>
    ///  <remarks>If host is nil or not a TWinControl or TCommonDialog
    ///  descendant then active form or, failing that, application's main form
    ///  are substituted.</remarks>
    class function Instance(const Host: TComponent): IWindowInfo;
  end;

type
  ///  <summary>
  ///  Factory class that creates IAlignableWindow instances from appropriate
  ///  classes.
  ///  </summary>
  TAlignableDialogFactory = class(TNoConstructObject)
  public
    ///  <summary>Creates a suitable IAlignableWindow instance for a dialog box
    ///  component.</summary>
    ///  <remarks>Dlg must be either a TCustomForm or TCommonDialog descendant.
    ///  </remarks>
    class function Instance(const Dlg: TComponent): IAlignableWindow; overload;
    ///  <summary>Creates a suitable IAlignableWindow instance for a dialog box
    ///  specified by its window handle.</summary>
    ///  <remarks>Handle must be a valid window handle.</remarks>
    class function Instance(const Handle: THandle): IAlignableWindow; overload;
  end;

type
  ///  <summary>
  ///  Class that implements IAlignableWindow and IWindowInfo interfaces for a
  ///  form. Provides information about and positions the form.
  ///  </summary>
  TFormWindow = class(TInterfacedObject,
    IAlignableWindow, IWindowInfo
  )
  strict private
    var
      ///  <summary>Reference to encapsulated form.</summary>
      fForm: TCustomForm;
  protected
    ///  <summary>Gets form's bounding rectangle.</summary>
    ///  <remarks>Method of both IWindowInfo and IAlignableWindow.</remarks>
    function BoundsRect: TRectEx;
    ///  <summary>Adjusts form to have a new bounding rectangle. If form is a
    ///  dialog box then top left corner is moved to new position but size is
    ///  not changed.</summary>
    ///  <remarks>Method of IAlignableWindow.</remarks>
    procedure AdjustWindow(const Bounds: TRectEx);
    ///  <summary>Checks if form is a dialog box.</summary>
    ///  <remarks>Method of IWindowInfo.</remarks>
    function IsDialog: Boolean;
    ///  <summary>Gets form's window handle.</summary>
    ///  <remarks>Method of IWindowInfo.</remarks>
    function Handle: THandle;
  public
    ///  <summary>Sets up object for given form.</summary>
    constructor Create(const Form: TCustomForm);
  end;

type
  ///  <summary>
  ///  Class that implements IWindowInfo interface for a TWinControl and
  ///  descendants (except TCustomForm). Provides information about the control.
  ///  </summary>
  TWinControlWindow = class(TInterfacedObject,
    IWindowInfo
  )
  strict private
    var
      ///  <summary>Reference to encapsulated control.</summary>
      fWinCtrl: TWinControl;
  protected
    ///  <summary>Checks if control is a dialog box.</summary>
    ///  <remarks>
    ///  <para>Always returns False because a TWinControl is never a dialog box.
    ///  </para>
    ///  <para>Method of IWindowInfo.</para>
    ///  </remarks>
    function IsDialog: Boolean;
    ///  <summary>Gets bound rectangle rectangle of control.</summary>
    ///  <remarks>Method of IWindowInfo.</remarks>
    function BoundsRect: TRectEx;
    ///  <summary>Gets window handle associated with control.</summary>
    ///  <remarks>Method of IWindowInfo.</remarks>
    function Handle: THandle;
  public
    ///  <summary>Sets up object for given TWinControl.</summary>
    constructor Create(const WinCtrl: TWinControl);
  end;

type
  ///  <summary>
  ///  Class that implements IAlignableWindow and IWindowInfo interfaces for a
  ///  common dialog box (except TOpenDialog). Provides information about and
  ///  positions the dialog.
  ///  </summary>
  TCommonDialogWindow = class(TInterfacedObject,
    IWindowInfo, IAlignableWindow
  )
  strict protected
    var
      ///  <summary>Reference to common dialog to be encapsulated.</summary>
      fDlg: TCommonDialog;
  protected
    ///  <summary>Gets dialog's bounding rectangle.</summary>
    ///  <remarks>Method of both IWindowInfo and IAlignableWindow.</remarks>
    function BoundsRect: TRectEx;
    ///  <summary>Adjust common dialog to have same top corner as bounding
    ///  rectangle. Any change of size is ignored.</summary>
    ///  <remarks>Method of IAlignableWindow.</remarks>
    procedure AdjustWindow(const Bounds: TRectEx);
    ///  <summary>Checks if component is a dialog box.</summary>
    ///  <remarks>
    ///  <para>Always returns True because a common dialogs are always dialogs.
    ///  </para>
    ///  <para>Method of IWindowInfo.</para>
    ///  </remarks>
    function IsDialog: Boolean;
    ///  <summary>Gets dialog box's window handle.</summary>
    ///  <remarks>Method of IWindowInfo.</remarks>
    function Handle: THandle; virtual;
  public
    ///  <summary>Sets up object for given common dialog component.</summary>
    constructor Create(const Dlg: TCommonDialog);
  end;

type
  ///  <summary>
  ///  Class that implements IAlignableWindow and IWindowInfo interfaces for a
  ///  TOpenDialog. Provides information about and positions the dialog.
  ///  </summary>
  TOpenDialogWindow = class(TCommonDialogWindow,
    IWindowInfo, IAlignableWindow
  )
  protected
    ///  <summary>Gets dialog box's window handle.</summary>
    ///  <remarks>
    ///  <para>Deals correctly with dialogs with explorer hooks and/or
    ///  customisation templates.</para>
    ///  <para>Method of IWindowInfo.</para>
    ///  </remarks>
    function Handle: THandle; override;
  public
    ///  <summary>Sets up object for given open dialog component.</summary>
    constructor Create(const Dlg: TOpenDialog);
  end;

type
  ///  <summary>
  ///  Class that implements IAlignableWindow for a window specified by its
  ///  window handle. Positions the dialog.
  ///  </summary>
  THandleWindow = class(TInterfacedObject,
    IAlignableWindow
  )
  strict private
    var
      ///  <summary>Handle of dialog box window.</summary>
      fHandle: THandle;
  public
    ///  <summary>Sets up object for given window handle.</summary>
    ///  <remarks>Handle must be a valid window handle.</remarks>
    constructor Create(const Handle: THandle);
    ///  <summary>Gets window's bounding rectangle.</summary>
    ///  <remarks>Method of IAlignableWindow.</remarks>
    function BoundsRect: TRectEx;
    ///  <summary>Offsets window to top left of given bouding rectangle. Size of
    ///  window is not changed.</summary>
    ///  <remarks>Method of IAlignableWindow.</remarks>
    procedure AdjustWindow(const Bounds: TRectEx);
  end;


{ TDlgHelper }

class procedure TDlgHelper.SetDlgParent(const Dlg, Parent: TComponent);
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
begin
  SetDlgParent(Dlg, Dlg.Owner);
end;

{ TDlgAligner }

procedure TDlgAligner.AdjustWindowPosition(const DlgBounds: TRectEx);
begin
  (fDialog as IAlignableWindow).AdjustWindow(DlgBounds);
end;

class procedure TDlgAligner.Align(const Dlg, Host: TComponent);
begin
  Assert(Assigned(Dlg), ClassName + '.Align: Dlg is nil');
  with InternalCreate(Dlg, Host) do
  try
    PerformAlignment;
  finally
    Free;
  end;
end;

class procedure TDlgAligner.Align(const DlgHandle: THandle;
  const Host: TComponent);
begin
  Assert(IsWindow(DlgHandle), ClassName + '.Align: DlgHandle is not a window');
  with InternalCreate(DlgHandle, Host) do
  try
    PerformAlignment;
  finally
    Free;
  end;
end;

class procedure TDlgAligner.AlignToOwner(const Dlg: TComponent);
begin
  Align(Dlg, Dlg.Owner);
end;

procedure TDlgAligner.FitToWorkArea(var DlgBounds: TRectEx);
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
begin
  DlgBounds := (fDialog as IAlignableWindow).BoundsRect;
end;

constructor TDlgAligner.InternalCreate;
begin
  raise ENoConstructException.Create(
    ClassName + '.InternalCreate: Constructor required parameters'
  );
end;

constructor TDlgAligner.InternalCreate(const Dlg, Host: TComponent);
begin
  inherited InternalCreate;
  fDialog := TAlignableDialogFactory.Instance(Dlg);
  fHostWdw := TWindowInfoFactory.Instance(Host);
end;

constructor TDlgAligner.InternalCreate(const DlgHandle: THandle;
  const Host: TComponent);
begin
  inherited InternalCreate;
  fDialog := TAlignableDialogFactory.Instance(DlgHandle);
  fHostWdw := TWindowInfoFactory.Instance(Host);
end;

procedure TDlgAligner.OffsetDialog(var DlgBounds: TRectEx);
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

procedure TDlgAligner.PerformAlignment;
var
  DlgBounds: TRectEx; // bounding rectangle of dialog box
begin
  GetDialogBounds(DlgBounds);
  OffsetDialog(DlgBounds);
  FitToWorkArea(DlgBounds);
  AdjustWindowPosition(DlgBounds);
end;

{ TWindowInfoFactory }

class function TWindowInfoFactory.Instance(
  const Host: TComponent): IWindowInfo;
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

class function TAlignableDialogFactory.Instance(
  const Handle: THandle): IAlignableWindow;
begin
  Result := THandleWindow.Create(Handle);
end;

{ TFormWindow }

procedure TFormWindow.AdjustWindow(const Bounds: TRectEx);
begin
  if IsDialog then
    fForm.SetBounds(Bounds.Left, Bounds.Top, fForm.Width, fForm.Height)
  else
    fForm.BoundsRect := Bounds;
end;

function TFormWindow.BoundsRect: TRectEx;
begin
  Result := fForm.BoundsRect;
end;

constructor TFormWindow.Create(const Form: TCustomForm);
begin
  Assert(Assigned(Form), ClassName + '.Create: Form is nil');
  inherited Create;
  fForm := Form;
end;

function TFormWindow.Handle: THandle;
begin
  Result := fForm.Handle;
end;

function TFormWindow.IsDialog: Boolean;
begin
  Result := fForm.BorderStyle in [bsDialog, bsSizeToolWin, bsToolWindow];
end;

{ TCommonDialogWindow }

procedure TCommonDialogWindow.AdjustWindow(const Bounds: TRectEx);
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
var
  Rect: TRect;  // bounding rectangle
begin
  GetWindowRect(Handle, Rect);
  Result := Rect;
end;

constructor TCommonDialogWindow.Create(const Dlg: TCommonDialog);
begin
  Assert(Assigned(Dlg), ClassName + '.Create: Dlg is nil');
  Assert((ClassType <> TCommonDialogWindow) or not (Dlg is TOpenDialog),
    ClassName + '.Create: Dlg cannot be a TOpenDialog');
  inherited Create;
  fDlg := Dlg;
end;

function TCommonDialogWindow.Handle: THandle;
begin
  Result := fDlg.Handle;
end;

function TCommonDialogWindow.IsDialog: Boolean;
begin
  Result := True;
end;

{ TOpenDialogWindow }

constructor TOpenDialogWindow.Create(const Dlg: TOpenDialog);
begin
  inherited Create(Dlg);
end;

function TOpenDialogWindow.Handle: THandle;
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
begin
  Assert(Assigned(WinCtrl), ClassName + '.Create: WinCtrl is nil');
  Assert(not (WinCtrl is TCustomForm),
    ClassName + '.Create: WinCtrl cannot be a TCustomForm');
  inherited Create;
  fWinCtrl := WinCtrl;
end;

function TWinControlWindow.Handle: THandle;
begin
  Result := fWinCtrl.Handle;
end;

function TWinControlWindow.IsDialog: Boolean;
begin
  Result := False;
end;

{ THandleWindow }

procedure THandleWindow.AdjustWindow(const Bounds: TRectEx);
begin
  SetWindowPos(
    fHandle,                    // window to position
    0,                          // only required if setting z-order
    Bounds.Left, Bounds.Top,    // X and Y co-ords of window
    0, 0,                       // only required if setting size of window
    SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER  // flags saying what to do
  );
end;

function THandleWindow.BoundsRect: TRectEx;
var
  Rect: TRect;  // bounding rectangle
begin
  GetWindowRect(fHandle, Rect);
  Result := Rect;
end;

constructor THandleWindow.Create(const Handle: THandle);
begin
  Assert(IsWindow(Handle), ClassName + '.Create: Handle is not a window.');
  inherited Create;
  fHandle := Handle;
end;

end.

