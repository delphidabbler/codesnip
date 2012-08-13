{
 * FmBase.pas
 *
 * Implements a form that provides the ancestor of all forms in the application.
 * Provides default names for form window classes along with various operations
 * that are common to all forms in application.
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
 * The Original Code is FmBase.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmBase;


interface


uses
  // Delphi
  Classes, Forms, Controls, Messages,
  // Project
  IntfAligner, UControlStateMgr;


type

  {
  TBaseForm:
    Base class for all forms in application. Sets a unique window class name for
    all derived forms and provides various operations that are common to all
    forms in application.
  }
  TBaseForm = class(TForm)
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  strict private
    var
      fCtrlStateMgr: TControlStateMgr;  // Enables/disables all form's controls
    const
      WM_AFTERSHOW = WM_USER + 1; // Custom message used to call AfterShow
    procedure AlignForm;
      {Optionally aligns form to a "parent" window, using an IAligner instance.
      Called from Form's OnShow event after form is customised and before it is
      initialised.
      }
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
      {Called when enabled state of form changes. Updates state of all controls.
        @param Msg [in/out] Unused.
      }
    procedure WMAfterShow(var Msg: TMessage); message WM_AFTERSHOW;
      {Handles custom method that is posted just before form is shown and
      handled just after form is shown.
        @param Msg [in/out] Unused.
      }
    procedure ActivateContextMenu;
      {Activates any context menu associated with active control or any of its
      parents.
      }
  strict protected
    procedure CreateParams(var Params: TCreateParams); override;
      {Sets window class name to that provided by WindowClassName method if not
      empty string.
        @param Params [in/out] Parameters used in underlying call to
          CreateWindowEx API function. Window class name member field is set.
      }
    function WindowClassName: string; virtual;
      {Returns name of form's window class. This is a name comprised of company,
      program and form class names. Subclasses may override.
        @return Required window class name.
      }
    function GetAligner: IFormAligner; virtual;
      {Gets object to be used to align form to owner.
        @return Nul aligner that does nothing. Subclasses that require alignment
          should override to return a suitable IAligner instance.
      }
    procedure CustomiseForm; virtual;
      {Used to customise form. This method is called during the form's OnShow
      event before the form is aligned. This implementation does nothing.
      Subclasses should overrride to perform any customisation and to change
      default size of form if necessary rather than handling the OnShow event.
      }
    procedure InitForm; virtual;
      {Used to initialise form content. This method is called during the form's
      OnShow event after the form is aligned. This implementation does nothing.
      Subclasses should override to initialise the form rather than handling the
      OnShow event. Form size must not be changed in this method.
      }
    procedure AfterShowForm; virtual;
      {Used to perform any actions that need to occur after the form has been
      shown and is visible on-screen. This implementation does nothing.
      Subclasses that need this functionality should override this method.
      }
    constructor InternalCreate(AOwner: TComponent); virtual;
      {Protected constructor. Does nothing but call inherited constructor.
      Must be called by class methods of derived classes instead of inherited
      Create if and only if the form supports the INoPublicConstruct interface.
        @param AOwner [in] Component that owns form. May be nil.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Public constructor. Does nothing but call inherited constructor. Can be
      called from descendant classes if necessary to override the constructor.
      Must not be called, directly or indirectly if the descendant form supports
      the INoPublicConstruct interface.
        @param AOwner [in] Component that owns form. May be nil.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, StrUtils, Windows, Menus,
  // Project
  UAppInfo, UBaseObjects, UFontHelper, UKeysHelper, UMenuHelper,
  UNulFormAligner;


{$R *.dfm}


type
  ///  <summary>Class helper that provides information about, and access to, the
  ///  protected PopupMenu property of TControl.</summary>
  TControlHelper = class helper for TControl
  public
    ///  <summary>Gets reference to pop-up menu assigned to protected PopupMenu
    ///  property.</summary>
    function GetPopupMenu: TPopupMenu;
    ///  <summary>Checks if protected PopupMenu property is assigned.</summary>
    function HasPopupMenu: Boolean;
  end;

{ TBaseForm }

procedure TBaseForm.ActivateContextMenu;
  {Activates any context menu associated with active control or any of its
  parents.
  }
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
  {Used to perform any actions that need to occur after the form has been shown
  and is visible on-screen. This implementation does nothing. Subclasses that
  need this functionality should override this method.
  }
begin
  // Do nothing
end;

procedure TBaseForm.AlignForm;
  {Optionally aligns form to a "parent" window, using an IAligner instance.
  Called from Form's OnShow event after form is customised and before it is
  initialised.
  }
begin
  // Align the control. This does nothing by default, since default aligner is
  // a do-nothing instance
  GetAligner.AlignForm(Self);
end;

procedure TBaseForm.CMEnabledChanged(var Msg: TMessage);
  {Called when enabled state of form changes. Updates state of all controls.
    @param Msg [in/out] Unused.
  }
begin
  inherited;
  // We update state of all controls, menu items and actions if possible
  if Assigned(fCtrlStateMgr) then
    fCtrlStateMgr.Update;
end;

constructor TBaseForm.Create(AOwner: TComponent);
  {Public constructor. Does nothing but call inherited constructor. Can be
  called from descendant classes if necessary to override the constructor. Must
  not be called, directly or indirectly if the descendant form supports the
  INoPublicConstruct interface.
    @param AOwner [in] Component that owns form. May be nil.
  }
begin
  Assert(not Supports(Self, INoPublicConstruct),
    ClassName + '.Create: Form''s public constructor can''t be called');
  inherited;
end;

procedure TBaseForm.CreateParams(var Params: TCreateParams);
  {Sets window class name to that provided by WindowClassName method if not
  empty string.
    @param Params [in/out] Parameters used in underlying call to
      CreateWindowEx API function. Window class name member field is set.
  }
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
  {Used to customise form. This method is called during the form's OnShow
  event before the form is aligned. This implementation does nothing.
  Subclasses should overrride to perform any customisation and to change
  default size of form if necessary rather than handling the OnShow event.
  }
begin
  // Do nothing
end;

procedure TBaseForm.FormCreate(Sender: TObject);
  {Handles form's OnCreate event. Creates owned objects.
    @param Sender [in] Not used.
  }
begin
  inherited;
  fCtrlStateMgr := TControlStateMgr.Create(Self);
  // Set form font to OS default font. This will be used by all descendants and
  // controls that have ParentFont true.
  TFontHelper.SetDefaultFont(Self.Font, False);
end;

procedure TBaseForm.FormDestroy(Sender: TObject);
  {Handles form's OnDestroy event. Unregisters form with object that works
  around Delphi's Alt Key bug and frees control state manager.
    @param Sender [in] Not used.
  }
begin
  FreeAndNil(fCtrlStateMgr);
end;

procedure TBaseForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  {Handles form's OnKeyDown event. Traps Alt+F10 keypress and displays any
  suitable context menu in response.
  }
begin
  inherited;
  if (Key = VK_F10) and (ExtractShiftKeys(Shift) = [ssAlt]) then
    ActivateContextMenu;
end;

procedure TBaseForm.FormShow(Sender: TObject);
  {Handles form's OnShow event. Calls a virtual method to customise form before
  aligning it. A further virtual method is then called to initialise the form.
  Finally a message is posted to the form that results in the AfterShowForm
  method being called after the form has been displayed. Subclasses should
  override the CustomiseForm, AlignForm and InitForm methods rather than
  handling this event.
    @param Sender [in] Not used.
  }
begin
  // Call virtual methods
  CustomiseForm;  // customise form: override if form size needs to be changed
  AlignForm;      // optionally align form using provided IAligner object
  InitForm;       // initialise form: do not change size of form in overrides
  // Post message that causes AfterShowForm to be called after form has appeared
  // on screen
  PostMessage(Handle, WM_AFTERSHOW, 0, 0);
end;

function TBaseForm.GetAligner: IFormAligner;
  {Gets object to be used to align form to owner.
    @return Nul aligner that does nothing. Subclasses that require alignment
      should override to return a suitable IAligner instance.
  }
begin
  Result := TNulAligner.Create;
end;

procedure TBaseForm.InitForm;
  {Used to initialise form content. This method is called during the form's
  OnShow event after the form is aligned. This implementation does nothing.
  Subclasses should override to initialise the form rather than handling the
  OnShow event. Form size must not be changed in this method.
  }
begin
  // Do nothing
end;

constructor TBaseForm.InternalCreate(AOwner: TComponent);
  {Protected constructor. Does nothing but call inherited constructor. Must be
  called by class methods of derived classes instead of inherited Create if and
  only if the form supports the INoPublicConstruct interface.
    @param AOwner [in] Component that owns form. May be nil.
  }
begin
  Assert(Supports(Self, INoPublicConstruct), ClassName + '.InternalCreate: '
    + 'Form''s protected constructor can''t be called');
  inherited Create(AOwner);
end;

function TBaseForm.WindowClassName: string;
  {Returns name of form's window class. This is a name comprised of company,
  program and form class names. Subclasses may override.
    @return Required window class name.
  }
var
  PostfixName: string;  // Postfix to name, based on form's class name
begin
  // Calculate window class name postfix. This is form class name, stripped of
  // any preceeding 'T'
  if AnsiStartsStr('T', ClassName) then
    PostfixName := AnsiRightStr(ClassName, Length(ClassName) - 1)
  else
    PostfixName := ClassName;
  // Build window class name
  Result := TAppInfo.CompanyName
    + '.' + TAppInfo.ProgramName
    + '.' + PostfixName;
end;

procedure TBaseForm.WMAfterShow(var Msg: TMessage);
  {Handles custom method that is posted just before form is shown and handled
  just after form is shown.
    @param Msg [in/out] Unused.
  }
begin
  AfterShowForm;
end;

{ TControlHelper }

function TControlHelper.GetPopupMenu: TPopupMenu;
begin
  Result := PopupMenu;
end;

function TControlHelper.HasPopupMenu: Boolean;
begin
  Result := Assigned(PopupMenu);
end;

end.

