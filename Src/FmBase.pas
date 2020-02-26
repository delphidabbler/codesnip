{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a form that provides the ancestor of all forms in the application.
 * Provides default names for form window classes along with various operations
 * that are common to all forms in application.
}


unit FmBase;


interface


uses
  // Delphi
  Classes, Forms, Controls, Messages,
  // Project
  IntfAligner, UControlStateMgr;


type
  ///  <summary>Base class for all forms in application.</summary>
  ///  <remarks>Sets a unique window class name for all derived forms and
  ///  provides various operations that are common to all forms in application.
  ///  </remarks>
  TBaseForm = class(TForm)
    ///  <summary>Handles form's OnDestroy event. Frees owned objects.</summary>
    procedure FormDestroy(Sender: TObject);
    ///  <summary>Handles form's OnShow event. Aligns form on screen using
    ///  aligner object. Calls virtual methods that sub-classes override to
    ///  perform pre- and post- alignment initialisation.</summary>
    procedure FormShow(Sender: TObject);
    ///  <summary>Handles form's OnCreate event to perform initialisations
    ///  required for every form.</summary>
    ///  <remarks>In addition to creation of owned objects this method also
    ///  sets form's font in OS dependent way.</remarks>
    procedure FormCreate(Sender: TObject);
    ///  <summary>Handles form's OnKeyDown event. Intercepts Alt+F10 key press
    ///  and displays any available context menu.</summary>
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  strict private
    var
      ///  <summary>Object used to enable / disable all form's controls when.
      ///  form's enabled state changes.</summary>
      fCtrlStateMgr: TControlStateMgr;
    const
      ///  <summary>Custom message used to call AfterShow method after form
      ///  appears on screen.</summary>
      WM_AFTERSHOW = WM_USER + 1; // Custom message used to call AfterShow
  strict private
    ///  <summary>Aligns form on screen using an IAligner instance.</summary>
    ///  <remarks>Called from OnShow event after form is customised and before
    ///  it is initialised.</remarks>
    procedure AlignForm;
    ///  <summary>Message handler that responds to changes in form's enabled
    ///  state by updating state of all controls according to whether form is
    ///  enabled or disabled.</summary>
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    ///  <summary>Message handler that responds to custom message sent from
    ///  OnShow event handler that arrives after form has been displayed. Calls
    ///  virtual AfterShow method.</summary>
    procedure WMAfterShow(var Msg: TMessage); message WM_AFTERSHOW;
    ///  <summary>Activates any context menu associated with the active control
    ///  or any of its parent controls.</summary>
    procedure ActivateContextMenu;
  strict protected
    ///  <summary>Overrides window creation parameters to set window class name
    ///  to that provided by virtual WindowClassName method.</summary>
    procedure CreateParams(var Params: TCreateParams); override;
    ///  <summary>Returns a window class name comprised of company, program and
    ///  form class names.</summary>
    function WindowClassName: string; virtual;
    ///  <summary>Returns new instance of aligner object used to align form to
    ///  owner.</summary>
    ///  <remarks>This implementation returns a null aligner. Subclasses that
    ///  require alignment should return a suitable IAligner instance.</remarks>
    function GetAligner: IFormAligner; virtual;
    ///  <summary>Customises form.</summary>
    ///  <remarks>
    ///  <para>This method is called during the form's OnShow event before the
    ///  form is aligned.</para>
    ///  <para>In this implementation the method does nothing. Subclasses should
    ///  overrride to perform any customisation and to change the default size
    ///  of form if necessary instead handling the OnShow event.</para>
    ///  </remarks>
    procedure CustomiseForm; virtual;
    ///  <summary>Initialises form content.</summary>
    ///  <remarks>
    ///  <para>This method is called during the form's OnShow event after the
    ///  form is aligned.</para>
    ///  <para>This implementation does nothing. Subclasses should override to
    ///  initialise the form instead of handling the OnShow event.</para>
    ///  <para>The form size should not be changed in this method since it will
    ///  interfere with the aligment.</para>
    ///  </remarks>
    procedure InitForm; virtual;
    ///  <summary>Performs any actions needed after the form is visible on
    ///  screen.</summary>
    ///  <remarks>This implementation does nothing. Subclasses that need this
    ///  functionality should override this method.</remarks>
    procedure AfterShowForm; virtual;
    ///  <summary>Protected constructor. Does nothing but call the inherited
    ///  constructor.</summary>
    ///  <remarks>
    ///  <para>This constructor is provided for use in derived form classes that
    ///  implement the INoPublicConstruct interface where the public Create
    ///  constructor can't be called.</para>
    ///  <para>Such classes must instantiate the form from a class method that
    ///  must call InternalCreate instead of Create.</para>
    ///  </remarks>
    constructor InternalCreate(AOwner: TComponent); virtual;
  public
    ///  <summary>Public constructor. Does nothing but call the inherited
    ///  constructor.</summary>
    ///  <remarks>
    ///  <para>This constructor can be called directly or from class methods in
    ///  a descendant class, providing that class does not support the
    ///  INoPublicConstruct interface.</para>
    ///  <para>In cases where INoPublicConstruct is supported the protected
    ///  InternalCreate constructor must be called instead.</para>
    ///  </remarks>
    constructor Create(AOwner: TComponent); override;
  end;


implementation


uses
  // Delphi
  SysUtils, Windows, Menus,
  // Project
  UAppInfo, UBaseObjects, UClassHelpers, UFontHelper, UKeysHelper, UMenus,
  UNulFormAligner, UStrUtils;

{$R *.dfm}

{ TBaseForm }

procedure TBaseForm.ActivateContextMenu;
var
  Ctrl: TControl;       // active control or a parent that supports pop-up menu
  MenuIntf: IPopupMenu; // interface reference to controls supporting IPopupMenu
  PopupPos: TPoint;     // pop-up position of menu in screen co-ords
begin
  // search active control parents to try to find if pop-up menu supported
  Ctrl := ActiveControl;
  if not Assigned(Ctrl) then
    Ctrl := Self;
  while Assigned(Ctrl)
    and (Ctrl <> Self)
    and not Ctrl.HasPopupMenu
    and not (Supports(Ctrl, IPopupMenu, MenuIntf) and MenuIntf.HasPopup) do
    Ctrl := Ctrl.Parent;
  if not Assigned(Ctrl) then
    Exit;
  // we use an arbitrary pop-up position: may be able to improve on this
  PopupPos := Ctrl.ClientToScreen(
    Point(40, 40)
  );
  // show pop-up menu, either via PopupMenu property or via IPopupMenu interface
  if Ctrl.HasPopupMenu then
    Ctrl.GetPopupMenu.Popup(PopupPos.X, PopupPos.Y)
  else if Supports(Ctrl, IPopupMenu, MenuIntf) and MenuIntf.HasPopup then
    MenuIntf.Popup(PopupPos);
end;

procedure TBaseForm.AfterShowForm;
begin
  // Do nothing
end;

procedure TBaseForm.AlignForm;
begin
  // Align the control. This does nothing by default, since default aligner is
  // a do-nothing instance
  GetAligner.AlignForm(Self);
end;

procedure TBaseForm.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  // We update state of all controls, menu items and actions if possible
  if Assigned(fCtrlStateMgr) then
    fCtrlStateMgr.Update;
end;

constructor TBaseForm.Create(AOwner: TComponent);
begin
  Assert(not Supports(Self, INoPublicConstruct),
    ClassName + '.Create: Form''s public constructor can''t be called');
  inherited;
end;

procedure TBaseForm.CreateParams(var Params: TCreateParams);
var
  ClassName: string;  // window class name
begin
  inherited;
  ClassName := WindowClassName;
  if ClassName <> '' then
    StrLCopy(
      Params.WinClassName,
      PChar(ClassName),
      SizeOf(Params.WinClassName) div SizeOf(Char) - 1
    );
end;

procedure TBaseForm.CustomiseForm;
begin
  // Do nothing
end;

procedure TBaseForm.FormCreate(Sender: TObject);
begin
  inherited;
  fCtrlStateMgr := TControlStateMgr.Create(Self);
  // Set form font to OS default font. This will be used by all descendants and
  // controls that have ParentFont true.
  TFontHelper.SetDefaultFont(Self.Font);
end;

procedure TBaseForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fCtrlStateMgr);
end;

procedure TBaseForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if (Key = VK_F10) and (ExtractShiftKeys(Shift) = [ssAlt]) then
    ActivateContextMenu;
end;

procedure TBaseForm.FormShow(Sender: TObject);
begin
  CustomiseForm;
  AlignForm;
  InitForm;
  // Post message that causes AfterShowForm to be called after form has appeared
  // on screen
  PostMessage(Handle, WM_AFTERSHOW, 0, 0);
end;

function TBaseForm.GetAligner: IFormAligner;
begin
  Result := TNulAligner.Create;
end;

procedure TBaseForm.InitForm;
begin
  // Do nothing
end;

constructor TBaseForm.InternalCreate(AOwner: TComponent);
begin
  Assert(Supports(Self, INoPublicConstruct), ClassName + '.InternalCreate: '
    + 'Form''s protected constructor can''t be called');
  inherited Create(AOwner);
end;

function TBaseForm.WindowClassName: string;
var
  PostfixName: string;  // Postfix to name, based on form's class name
begin
  // Calculate window class name postfix. This is form class name, stripped of
  // any preceeding 'T'
  if StrStartsStr('T', ClassName) then
    PostfixName := StrSliceRight(ClassName, Length(ClassName) - 1)
  else
    PostfixName := ClassName;
  // Build window class name
  Result := TAppInfo.CompanyName
    + '.' + TAppInfo.ProgramName
    + '.' + PostfixName;
end;

procedure TBaseForm.WMAfterShow(var Msg: TMessage);
begin
  AfterShowForm;
end;

end.

