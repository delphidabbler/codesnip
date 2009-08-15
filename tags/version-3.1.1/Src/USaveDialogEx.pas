{
 * USaveDialogEx.pas
 *
 * Implements a subclass of the Save dialog box to enable the dialog to align
 * itself over its owner and to work correctly with the Vista task bar. Also
 * adds support for help keywords and help button.
 *
 * Requires that Application.ModalPopupMode <> pmNone.
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
 * The Original Code is USaveDialogEx.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit USaveDialogEx;


interface

{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}

uses
  // Delphi
  Classes, Dialogs, Messages, Windows;


type

  {
  TSaveDialogEx:
    Subclasses the Save dialog box to enable the dialog to align itself over its
    owner and to work correctly with the Vista task bar. Also adds support for
    help keywords and help button.
  }
  TSaveDialogEx = class(TSaveDialog)
  strict private
    fHelpKeyword: string;
      {Value of HelpKeyword property}
    fWantDefaultHelpSupport: Boolean;
      {Value of WantDefaultHelpSupport property}
  protected
    fOldExplorerHook: Pointer;
      {Reference to original explorer hook function provided by Delphi}
    function TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool;
      override;
      {Overridden method that updates the DialogData structure to route message
      processing through a custom explorer hook function while storing the
      original explorer hook function for later use.
        @param DialogFunc [in] Windows function to be called to execute dialog
          box (GetOpenFileName() in this case).
        @param DialogData [in] Data describing dialog box to be passed to
          DialogFunc (in this case of type TOpenFileName).
      }
    function MessageHook(var Msg: TMessage): Boolean; override;
      {Intercepts messages sent to the dialog window before the dialog�s window
      procedure. This implementation changes default support for the help button
      to include the new HelpKeyword property and to use the program's own help
      manager.
        @param Msg [in/out] Specifies message. Unchanged by this method. May be
          modified by inherited implementation(s).
        @return False to pass message on to dilog's window procedure, True to
          prevent this.
      }
    procedure DoShow; override;
      {Sets up dialog just before it is displayed.
      }
    procedure AlignDlg; virtual;
      {Aligns dialog box to owner control.
      }
    function DisplayHelp: Boolean; virtual;
      {Calls program's help manager to display help if HelpKeyword or
      HelpContext properties are set. HelpKeyword is used in preference.
        @return True if help manager was called or False if not (i.e. neither
          HelpKeyword nor HelpContext were set).
      }
    property WantDefaultHelpSupport: Boolean
      read fWantDefaultHelpSupport write fWantDefaultHelpSupport;
      {Indicates if support for default help processing is required. When false
      the dialog never displays the help button. When true display depends on
      state of HelpKeyword or HelpContext properties}
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Creates dialog box.
        @param AOwner [in] Owning component. Dialog box will be aligned over
          AOwner.
      }
    function Execute: Boolean; override;
      {Displays dialog box. Ensures help button is displayed if HelpKeyword or
      HelpContext properties are set.
        @return True if user OKs and False if cancels.
      }
  published
    property HelpKeyword: string
      read fHelpKeyword write fHelpKeyword;
      {ALink help keyword used to access help topic when help button clicked.
      When set this property is used in preference to HelpContext}
  end;


implementation


uses
  // Delphi
  Controls, CommDlg,
  // Project
  UDlgHelper, UHelpMgr;


var
  HelpMsgID: DWORD = 0; // ID of common dialog box help message


function NewExplorerHook(Wnd: HWnd; Msg: UINT; WParam: WPARAM;
  LParam: LPARAM): UINT; stdcall;
  {Replacement explorer hook function called by Windows to process dialog box
  messages. Original hook handles only WM_INITDIALOG message and CDN_INITDONE
  notification. This replacement passes WM_INITDIALOG messages to original
  hook but handles CDN_INITDONE to align the dialog box (original hook centres
  it). All other messages are passed to original hook.
    @param Wnd [in] Window handle of dialog box.
    @param Msg [in] Identifies message being passed to hook.
    @param WParam [in] Message parameter. Usage depends on message.
    @param LParam [in] Message parameter. Usage depends on message.
    @return Message specific return value.
  }

  //----------------------------------------------------------------------------
  function OFNToDlg(const OFN: TOpenFilename): TSaveDialogEx;
    {Gets reference to save dialog from custom data in save dialog data
    structure.
      @param OFN [in] Structure containing reference to dialog box object.
      @return Required dialog box reference.
    }
  begin
    Result := TSaveDialogEx(OFN.lCustData);
  end;

  function CallHookFn(const HookFn: Pointer): UINT;
    {Calls explorer hook function with parameters passed to outer method.
      @param HookFn [in] Pointer to hook function to be called.
      @return Return value from called hook function.
    }
  type
    // Hook function prototype
    THookFn = function(Wnd: HWnd; Msg: UINT; WParam: Integer;
      LParam: Integer): UINT; stdcall;
  begin
    Result := THookFn(HookFn)(Wnd, Msg, WParam, LParam);
  end;
  //----------------------------------------------------------------------------

var
  Dlg: TSaveDialogEx;  // reference to dialog box object
begin
  // Set default result passed back to windows
  Result := 0;
  if Msg = WM_INITDIALOG then
    // Dialog initialising: pass on to original hook function
    Result := CallHookFn(OFNToDlg(POpenFileName(LParam)^).fOldExplorerHook)
  else if Msg = WM_NOTIFY then
  begin
    // Get reference to dialog box object from data structure
    Dlg := OFNToDlg(POFNotify(LParam)^.lpOFN^);
    if POFNotify(LParam)^.hdr.code = CDN_INITDONE then
      // Dialog intialization complete: align the dialog box. We don't call old
      // hook function since all this does is centre the dialog box!
      // Windows ignores return value (we leave as default 0)
      Dlg.AlignDlg
    else
      // Other notification: pass on to original hook function
      Result := CallHookFn(Dlg.fOldExplorerHook);
  end;
end;

{ TSaveDialogEx }

procedure TSaveDialogEx.AlignDlg;
  {Aligns dialog box to owner control.
  }
begin
  TDlgAligner.AlignToOwner(Self);
end;

constructor TSaveDialogEx.Create(AOwner: TComponent);
  {Class constructor. Creates dialog box.
    @param AOwner [in] Owning component. Dialog box will be aligned over AOwner.
  }
begin
  inherited;
  // Enable default help processing
  fWantDefaultHelpSupport := True;
end;

function TSaveDialogEx.DisplayHelp: Boolean;
  {Calls program's help manager to display help if HelpKeyword or HelpContext
  properties are set. HelpKeyword is used in preference.
    @return True if help manager was called or False if not (i.e. neither
      HelpKeyword nor HelpContext were set).
  }
begin
  Result := True;
  if HelpKeyword <> '' then
    HelpMgr.ShowHelp(HelpKeyword)
  else if HelpContext <> 0 then
    HelpMgr.ShowHelp(HelpContext)
  else
    Result := False;
end;

procedure TSaveDialogEx.DoShow;
  {Sets up dialog just before it is displayed.
  }
begin
  // Prevent task bar button press bringing owner window to foreground
  TDlgHelper.SetDlgParentToOwner(Self);
  inherited;
end;

function TSaveDialogEx.Execute: Boolean;
  {Displays dialog box. Ensures help button is displayed if HelpKeyword or
  HelpContext properties are set.
    @return True if user OKs and False if cancels.
  }
begin
  if WantDefaultHelpSupport and ((HelpKeyword <> '') or (HelpContext <> 0)) then
    Options := Options + [ofShowHelp]
  else
    Options := Options - [ofShowHelp];
  Result := inherited Execute;
end;

function TSaveDialogEx.MessageHook(var Msg: TMessage): Boolean;
  {Intercepts messages sent to the dialog window before the dialog�s window
  procedure. This implementation changes default support for the help button
  to include the new HelpKeyword property and to use the program's own help
  manager.
    @param Msg [in/out] Specifies message. Unchanged by this method. May be
      modified by inherited implementation(s).
    @return False to pass message on to dilog's window procedure, True to
      prevent this.
  }
begin
  if (Msg.Msg = HelpMsgID) and WantDefaultHelpSupport then
    Result := DisplayHelp
  else
    Result := inherited MessageHook(Msg);
end;

function TSaveDialogEx.TaskModalDialog(DialogFunc: Pointer;
  var DialogData): Bool;
  {Overridden method that updates the DialogData structure to route message
  processing through a custom explorer hook function while storing the original
  explorer hook function for later use.
    @param DialogFunc [in] Windows function to be called to execute dialog box
      (GetOpenFileName() in this case).
    @param DialogData [in] Data describing dialog box to be passed to DialogFunc
      (in this case of type TOpenFilename).
  }
begin
  if NewStyleControls and not (ofOldStyleDialog in Options) then
  begin
    // Record previous explorer hook function for later use
    fOldExplorerHook := @TOpenFilename(DialogData).lpfnHook;
    // Store reference to our new explorer hook function in DialogData
    TOpenFilename(DialogData).lpfnHook := NewExplorerHook;
    // Store reference to this object in DialogData
    TOpenFilename(DialogData).lCustData := Integer(Self);
  end;
  // Call inherited function with (modified) data structure
  Result := inherited TaskModalDialog(DialogFunc, DialogData);
end;


initialization

// Get ID of common dialog help message
HelpMsgID := RegisterWindowMessage(CommDlg.HELPMSGSTRING);


end.

