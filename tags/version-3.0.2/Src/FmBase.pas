{
 * FmBase.pas
 *
 * Implements a form that provides the ancestor of all forms in the application.
 * Provides default names for form window classes along with various operations
 * that are common to all forms in application.
 *
 * v0.1 of 30 Jan 2005  - Original version.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 *                      - Relocated and rationalised $WARN directives.
 *                      - Prefixed global constants with UGlobals unit name.
 * v1.1 of 31 Oct 2006  - Added handler for CM_WININICHANGE that updates theme
 *                        manager.
 * v2.0 of 08 Feb 2007  - Major update.
 *                      - Added support for form aligment to class:
 *                        - Added code that enables form to be aligned over
 *                          another form via a provided IAligner instance.
 *                        - Added virtual methods that be overridden to
 *                          configure form before aligment and initialise it
 *                          after alignment.
 *                      - WindowClassName now only strips leading character of
 *                        class name if it starts with a 'T'.
 * v2.1 of 26 Sep 2007  - Changed to use renamed IFormAligner interface.
 * v2.2 of 14 Jun 2008  - Registered form with object that fixes Delphi's Alt
 *                        key bug.
 * v2.3 of 11 Aug 2008  - Removed handler for CM_WININICHANGE that updated theme
 *                        manager. Not required now that theme manager can
 *                        detect changes itself.
 * v2.4 of 05 Oct 2008  - Alt key bug fix was not being unregistered correctly.
 *                        Fixed by moving unregistration of form from OnHide
 *                        event handler (which is not always called) to the
 *                        OnDestroy event handler.
 * v2.5 of 04 Jan 2009  - Added code that detects form's enabled state changing
 *                        and causes all controls and actions to be enabled /
 *                        disabled. Uses separate object to perform updating.
 * v2.6 of 13 May 2009  - Now gets company and program name from TAppInfo
 *                        instead of UGlobals unit.
 *
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
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FmBase;


interface


uses
  // Delphi
  Forms, Controls, Messages,
  // Project
  IntfAligner, UAltBugFix, UControlStateMgr;


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
  strict private
    fCtrlStateMgr: TControlStateMgr;
      {Object used to enable disable all controls on form}
    procedure AlignForm;
      {Optionally aligns form to a "parent" window, using an IAligner instance.
      Called from Form's OnShow event after form is customised and before it is
      initialised.
      }
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
      {Called when enabled state of form changes. Updates state of all controls.
        @param Msg [in/out] Unused.
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
  end;


implementation


uses
  // Delphi
  SysUtils, StrUtils,
  // Project
  UAppInfo, UNulFormAligner;


{$R *.dfm}


{ TBaseForm }

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
    StrLCopy(Params.WinClassName, PChar(ClassName), 62);
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
end;

procedure TBaseForm.FormDestroy(Sender: TObject);
  {Handles form's OnDestroy event. Unregisters form with object that works
  around Delphi's Alt Key bug and frees control state manager.
    @param Sender [in] Not used.
  }
begin
  FreeAndNil(fCtrlStateMgr);
  // NOTE: We can't use OnHide since that is not always called, causing form to
  // remain registered and therefore any later form with same object address not
  // to be handled correctly
  AltBugFix.UnRegisterCtrl(Self);
end;

procedure TBaseForm.FormShow(Sender: TObject);
  {Handles form's OnShow event. Calls a virtual method to customise form before
  aligning it. A further virtual method is then called to initialise the form.
  Subclasses should override the CustomiseForm and InitForm methods rather than
  handling this event. Also registers form with object that works around
  Delphi's Alt Key bug.
    @param Sender [in] Not used.
  }
begin
  // Register form with Alt Key bug fixer to be updated in idle state
  AltBugFix.RegisterCtrl(Self, True);
  // Call virtual methods
  CustomiseForm;  // customise form: override if form size needs to be changed
  AlignForm;      // optionally align form using provided IAligner object
  InitForm;       // initialise form: do not change size of form in overrides
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

end.

