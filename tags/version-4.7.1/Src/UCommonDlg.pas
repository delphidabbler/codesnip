{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * A set of classes that assist in hooking and handling common dialog box
 * messages. Also provides helper methods for handling dialog box messages.
}


unit UCommonDlg;


interface


uses
  // Delphi
  Messages, Dialogs, CommDlg, Windows,
  // Project
  UBaseObjects;


type

  {
  TCommonDlgHelper:
    Static class that provides methods to help with common dialog tasks not
    handled via the dialog hook function.
  }
  TCommonDlgHelper = class(TNoConstructObject)
  public
    class function IsHelpMessage(const Msg: TMessage): Boolean;
      {Checks if a message is a common dialog help messages.
        @param Msg [in] Message to be checked.
        @return True if help message, False if not.
      }
    class function ShowHelp(const Keyword: string): Boolean;
      {Displays a help topic based on an a-link keyword.
        @param Keyword [in] A-link keyword that specifies help topic. No topic
          displayed if Keyword is empty string.
      }
  end;

  {
  TCommonDlgHookFn:
    Type of common dialog box hook (callback). These procedures receive messages
    and notifications intended for a dialog box and can handle them or pass them
    on to the dialog box procedure.
      @param Wnd [in] Handle to the dialog box for which message is intended.
      @param Msg [in] Message id.
      @param WParam [in] Additional information depending on value of Msg.
      @param LParam [in] Additional information depending on value of Msg.
      @return 0 to pass message to dialog box procedure or non-zero to cause the
        dialog box procedure to ignore the message.
  }
  TCommonDlgHookFn = function(Wnd: HWnd; Msg: UINT; WParam: WPARAM;
    LParam: LPARAM): UINT_PTR; stdcall;

  {
  TCommonDlgHook:
    Abstract base class for classes that hooks and handles dialog hook messages
    on behalf of an associated common dialog box.
  }
  TCommonDlgHook = class abstract(TObject)
  strict private
    fDlg: TCommonDialog;          // Reference to related dialog box
    fOldHookFn: TCommonDlgHookFn; // Value of OldHookFn property
  strict protected
    property OldHookFn: TCommonDlgHookFn read fOldHookFn write fOldHookFn;
      {Dialog's previous (default?) hook function}
  public
    constructor Create(const Dlg: TCommonDialog);
      {Class constructor. Sets up object and associates with a common dialog
        box.
        @param Dlg [in] Related dialog box. Must not be nil.
      }
    procedure Initialise(var DialogData); virtual; abstract;
      {Applies new dialog box hook function and ensures dialog box has reference
      back to this object.
        @param DialogData [in/out] Contains data about dialog box. Updated with
          new hook function and reference to associated hook object.
      }
    function CallHookFn(Wnd: HWnd; Msg: UINT; WParam, LParam: Integer): UINT;
      {Calls associated dialog hook function with the required parameters. A
      reference to the dialog function must be recorded in the OldHookFn
      property.
        @param Wnd [in] Handle to the dialog box for which message is intended.
          Passed to hook function.
        @param Msg [in] Message id. Passed to hook function.
        @param WParam [in] Additional information depending on value of Msg.
          Passed to hook function.
        @param LParam [in] Additional information depending on value of Msg.
          Passed to hook function.
        @return Value returned from hook function.
      }
    procedure AlignDlg;
      {Aligns associated dialog box to its owner.
      }
  end;

  {
  TFileDlgHook:
    Class that hooks and handles dialog hook messages on behalf of file open /
    save common dialog dailogs using a custom hook function. Aligns dialog box
    over owner.
  }
  TFileDlgHook = class(TCommonDlgHook)
  public
    procedure Initialise(var DialogData); override;
      {Applies new dialog box hook function and ensures file dialog box has
      reference back to this object.
        @param DialogData [in/out] Contains address of file dialog's current
          hook function. Receives new hook function and reference to this hook
          object.
      }
    class function RecoverInstance(P: POpenFileName): TFileDlgHook;
      overload;
      {Recovers an instance an object of this class from TOpenFileName data.
        @param P [in] Pointer to TOpenFilename structure that contains reference
          to a TFileDlgHook instance in one of its field.
      }
    class function RecoverInstance(P: POFNotify): TFileDlgHook;
      overload;
      {Recovers an instance an object of this class from TOFNotify data.
        @param P [in] Pointer to TOFNotify structure that contains reference to
          a TFileDlgHook instance in one of its field.
      }
  end;

  {
  TColorDlgHook:
    Class that hooks and handles dialog hook messages on behalf of color common
    dialog dailogs using a custom hook function. Aligns dialog box over owner.
  }
  TColorDlgHook = class(TCommonDlgHook)
  public
    procedure Initialise(var DialogData); override;
      {Applies new dialog box hook function and ensures colour dialog box has
      reference back to this object.
        @param DialogData [in/out] Contains address of colour dialog's current
          hook function. Receives new hook function and reference to this hook
          object.
      }
    class function RecoverInstance(P: PChooseColor): TColorDlgHook;
      {Recovers an instance an object of this class from TChooseColor data.
        @param P [in] Pointer to TChooseColor structure that contains reference
          to a TColorDlgHook instance in one of its field.
      }
  end;


implementation


uses
  // Project
  UDlgHelper, UHelpMgr;


{ TCommonDlgHelper }

class function TCommonDlgHelper.IsHelpMessage(const Msg: TMessage): Boolean;
  {Checks if a message is a common dialog help messages.
    @param Msg [in] Message to be checked.
    @return True if help message, False if not.
  }
begin
  Result := Msg.Msg = RegisterWindowMessage(CommDlg.HELPMSGSTRING);
end;

class function TCommonDlgHelper.ShowHelp(const Keyword: string): Boolean;
  {Displays a help topic based on an a-link keyword.
    @param Keyword [in] A-link keyword that specifies help topic. No topic
      displayed if Keyword is empty string.
  }
begin
  Result := Keyword <> '';
  if Result then
    HelpMgr.ShowHelp(Keyword);
end;

{ TCommonDlgHook }

function CallHookFunc(Fn: TCommonDlgHookFn; Wnd: HWnd; Msg: UINT;
  WParam, LParam: Integer): UINT;
  {Calls a dialog hook function with the required parameters. Must be a standard
  function, not a method.
    @param Fn [in] Hook function to be called. Must be non nil.
    @param Wnd [in] Handle to the dialog box for which message is intended.
      Passed to Fn.
    @param Msg [in] Message id. Passed to Fn.
    @param WParam [in] Additional information depending on value of Msg. Passed
      to Fn.
    @param LParam [in] Additional information depending on value of Msg. Passed
      to Fn.
    @return Value returned from Fn.
  }
begin
  Assert(Assigned(Fn), 'CallHookFunc: Fn is nil');
  Result := Fn(Wnd, Msg, LParam, WParam)
end;

procedure TCommonDlgHook.AlignDlg;
  {Aligns associated dialog box to its owner.
  }
begin
  TDlgAligner.AlignToOwner(fDlg);
end;

function TCommonDlgHook.CallHookFn(Wnd: HWnd; Msg: UINT; WParam,
  LParam: Integer): UINT;
  {Calls associated dialog hook function with the required parameters. A
  reference to the dialog function must be recorded in the OldHookFn property.
    @param Wnd [in] Handle to the dialog box for which message is intended.
      Passed to hook function.
    @param Msg [in] Message id. Passed to hook function.
    @param WParam [in] Additional information depending on value of Msg. Passed
      to hook function.
    @param LParam [in] Additional information depending on value of Msg. Passed
      to hook function.
    @return Value returned from hook function.
  }
begin
  Assert(Assigned(OldHookFn), ClassName + '.CallHookFn: OldHookFn is nil');
  Result := CallHookFunc(OldHookFn, Wnd, Msg, LParam, WParam);
end;

constructor TCommonDlgHook.Create(const Dlg: TCommonDialog);
  {Class constructor. Sets up object and associates with a common dialog box.
    @param Dlg [in] Related dialog box. Must not be nil.
  }
begin
  Assert(Assigned(Dlg), ClassName + '.Create: Dlg is nil');
  inherited Create;
  fDlg := Dlg;
end;

{ TFileDlgHook }

function NewExplorerHook(Wnd: HWnd; Msg: UINT; WParam: WPARAM;
  LParam: LPARAM): UINT_PTR; stdcall;
  {Replacement explorer hook function called by Windows to process dialog box
  messages. Original hook handles only WM_INITDIALOG message and CDN_INITDONE
  notification. This replacement passes WM_INITDIALOG messages to original
  hook but handles CDN_INITDONE to align the dialog box (original hook centres
  it). All other messages are passed to original hook. This function is used by
  and is associated with TFileDlgHook objects.
    @param Wnd [in] Window handle of dialog box.
    @param Msg [in] Identifies message being passed to hook.
    @param WParam [in] Message parameter. Usage depends on message.
    @param LParam [in] Message parameter. Usage depends on message.
    @return Message specific return value.
  }
var
  Hook: TFileDlgHook; // object that handles hook messages
begin
  // Set default result passed back to windows
  Result := 0;
  if Msg = WM_INITDIALOG then
  begin
    // Dialog initialising: pass on to original hook function
    Hook := TFileDlgHook.RecoverInstance(POpenFileName(LParam))
      as TFileDlgHook;
    Result := Hook.CallHookFn(Wnd, Msg, LParam, WParam);
  end
  else if Msg = WM_NOTIFY then
  begin
    // Get reference to dialog box object from data structure
    Hook := TFileDlgHook.RecoverInstance(POFNotify(LParam))
      as TFileDlgHook;
    if POFNotify(LParam)^.hdr.code = CDN_INITDONE then
      // Dialog intialization complete: align the dialog box. We don't call old
      // hook function since all this does is centre the dialog box!
      // Windows ignores return value (we leave as default 0)
      Hook.AlignDlg
    else
      // Other notification: pass on to original hook function
      Result := Hook.CallHookFn(Wnd, Msg, WParam, LParam);
  end;
end;

procedure TFileDlgHook.Initialise(var DialogData);
  {Applies new dialog box hook function and ensures file dialog box has
  reference back to this object.
    @param DialogData [in/out] Contains address of file dialog's current hook
      function. Receives new hook function and reference to this hook object.
  }
begin
  OldHookFn := TOpenFilename(DialogData).lpfnHook;
  TOpenFilename(DialogData).lpfnHook := NewExplorerHook;
  TOpenFilename(DialogData).lCustData := Integer(Self);
end;

class function TFileDlgHook.RecoverInstance(P: POpenFileName): TFileDlgHook;
  {Recovers an instance an object of this class from TOpenFileName data.
    @param P [in] Pointer to TOpenFilename structure that contains reference to
      a TFileDlgHook instance in one of its field.
  }
begin
  Result := TFileDlgHook(P^.lCustData)
end;

class function TFileDlgHook.RecoverInstance(P: POFNotify): TFileDlgHook;
  {Recovers an instance an object of this class from TOFNotify data.
    @param P [in] Pointer to TOFNotify structure that contains reference to a
      TFileDlgHook instance in one of its field.
  }
begin
  Result := RecoverInstance(P^.lpOFN);
end;

{ TColorDlgHook }

function NewCCHook(Wnd: HWnd; Msg: UINT; WParam: WPARAM;
  LParam: LPARAM): UINT_PTR; stdcall;
  {Replacement hook function called by Windows to process dialog box messages.
  Original hook handles only WM_INITDIALOG message. This replacement passes
  WM_INITDIALOG messages to original hook and then aligns dialog box as
  required. No other messages are handled by this hook or by original hook. This
  function is used by and is associated with TFileDlgHook objects.
    @param Wnd [in] Window handle of dialog box.
    @param Msg [in] Identifies message being passed to hook.
    @param WParam [in] Message parameter. Usage depends on message.
    @param LParam [in] Message parameter. Usage depends on message.
    @return Message specific return value.
  }
var
  Hook: TColorDlgHook;  // object that handles hook messages
begin
  // Set default result passed back to Windows
  Result := 0;
  if Msg = WM_INITDIALOG then
  begin
    // Dialog initialising: pass on to original hook function which centres
    // dialog box (amongst other work), then re-align dialog.
    Hook := TColorDlgHook.RecoverInstance(PChooseColor(LParam))
      as TColorDlgHook;
    Result := Hook.CallHookFn(Wnd, Msg, WParam, LParam);
    Hook.AlignDlg;
  end;
end;

procedure TColorDlgHook.Initialise(var DialogData);
  {Applies new dialog box hook function and ensures colour dialog box has
  reference back to this object.
    @param DialogData [in/out] Contains address of colour dialog's current hook
      function. Receives new hook function and reference to this hook object.
  }
begin
  OldHookFn := TChooseColor(DialogData).lpfnHook;
  TChooseColor(DialogData).lpfnHook := NewCCHook;
  TChooseColor(DialogData).lCustData := Integer(Self);
end;

class function TColorDlgHook.RecoverInstance(P: PChooseColor): TColorDlgHook;
  {Recovers an instance an object of this class from TChooseColor data.
    @param P [in] Pointer to TChooseColor structure that contains reference to
      a TColorDlgHook instance in one of its field.
  }
begin
  Result := TColorDlgHook(P^.lCustData)
end;

initialization

Dialogs.UseLatestCommonDialogs := False;

{$IFNDEF PORTABLE}
// Default to My Docs for file dialogues in standard version
Dialogs.ForceCurrentDirectory := False;
{$ELSE}
// Default to program working directory for file dialogues in portable version
Dialogs.ForceCurrentDirectory := True;
{$ENDIF}

end.

