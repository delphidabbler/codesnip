{
 * UColorDialogEx.pas
 *
 * Implements a colour dialog box subclass that aligns itself over its owner,
 * works correctly with the Vista task bar, provides a Title property and uses
 * UK English. Dialog also supports help keywords.
 *
 * v1.0 of 10 Aug 2008  - Original version.
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
 * The Original Code is UColorDialogEx.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
 *
 * ***** END LICENSE BLOCK *****
}


unit UColorDialogEx;

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}

interface


uses
  // Delphi
  Dialogs, Messages, Controls, Windows;


type

  {
  TColorDialogEx:
    Subclass of the colour dialog box that aligns itself over its owner, works
    correctly with the Vista task bar, provides a Title property and uses UK
    English. Also adds support for help keywords and help button.
  }
  TColorDialogEx = class(TColorDialog)
  private
    fTitle: TCaption;
      {Value of Title property}
    fHelpKeyword: string;
      {Value of HelpKeyword property}
  protected
    fOldHook: Pointer;
      {Reference to original hook function provided by Delphi}
    function TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool;
      override;
      {Overridden method that updates the DialogData structure to route message
      processing through a custom hook function while storing the original hook
      function for later use.
        @param DialogFunc [in] Windows function to be called to execute dialog
          box (ChooseColor() in this case).
        @param DialogData [in] Data describing dialog box to be passed to
          DialogFunc (in this case of type TChooseColour).
      }
    function MessageHook(var Msg: TMessage): Boolean; override;
      {Intercepts messages sent to the dialog window before the dialog’s window
      procedure. This implementation changes default support for the help button
      to include the new HelpKeyword property and to use the program's own help
      manager.
        @param Msg [in/out] Specifies message. Unchanged by this method. May be
          modified by inherited implementation(s).
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
  public
    function Execute: Boolean; override;
      {Displays dialog box. Esnures help button is displayed if HelpKeyword or
      HelpContext properties are set.
        @return True if user OKs and False if cancels.
      }
  published
    property Title: TCaption read fTitle write fTitle;
      {Enables dialog box title to be customised. If Title is left blank the
      default dialog box title is used}
    property HelpKeyword: string
      read fHelpKeyword write fHelpKeyword;
      {ALink help keyword used to access help topic when help button clicked.
      When set this property is used in preference to HelpContext}
  end;


implementation


uses
  // Delphi
  CommDlg,
  // Project
  UDlgHelper, UHelpMgr;


var
  HelpMsgID: DWORD = 0; // ID of dialog box help message


function NewCCHook(Wnd: HWnd; Msg: UINT; WParam: WPARAM;
  LParam: LPARAM): UINT; stdcall;
  {Replacement hook function called by Windows to process dialog box messages.
  Original hook handles only WM_INITDIALOG message. This replacement passes
  WM_INITDIALOG messages to original hook and then aligns dialog box as
  required. No other messages are handled by this hook or by original hook.
    @param Wnd [in] Window handle of dialog box.
    @param Msg [in] Identifies message being passed to hook.
    @param WParam [in] Message parameter. Usage depends on message.
    @param LParam [in] Message parameter. Usage depends on message.
    @return Message specific return value.
  }

  //----------------------------------------------------------------------------
  function CCToDlg(const CC: TChooseColor): TColorDialogEx;
    {Gets reference to color dialog from custom data in dialog data
    structure.
      @param OFN [in] Structure containing reference to dialog box object.
      @return Required dialog box reference.
    }
  begin
    Result := TColorDialogEx(CC.lCustData);
  end;

  function CallHookFn(const HookFn: Pointer): UINT;
    {Calls hook function with parameters passed to outer method.
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

begin
  // Set default result passed back to Windows
  Result := 0;
  if Msg = WM_INITDIALOG then
  begin
    // Dialog initialising: pass on to original hook function which centres
    // dialog box (amongst other work), then re-align dialog.
    Result := CallHookFn(CCToDlg(PChooseColor(LParam)^).fOldHook);
    CCToDlg(PChooseColor(LParam)^).AlignDlg;
  end;
end;

{ TColorDialogEx }

procedure TColorDialogEx.AlignDlg;
  {Aligns dialog box to owner control.
  }
begin
  TDlgAligner.AlignToOwner(Self);
end;

function TColorDialogEx.DisplayHelp: Boolean;
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

procedure TColorDialogEx.DoShow;
  {Sets up dialog just before it is displayed.
  }

  // ---------------------------------------------------------------------------
  function PrimaryLangID(LangID: Word): Word;
    {Gets primary language from a language ID. Based on Windows SDK macro in
    WinNT.h.
      @param LangID [in] Language ID.
      @return Primary language ID.
    }
  begin
    Result := LangID and $3FF;
  end;
  // ---------------------------------------------------------------------------

const
  // Identifiers of controls using American spelling of "colour" (per ColorDlg.h
  // from Windows SDK).
  COLOR_ADD        = 712;
  COLOR_MIX        = 719;
  COLOR_SOLID_LEFT = 730;
resourcestring
  // UK English strings to replace American in dialog box
  sAddToCustomColors = '&Add to Custom Colours';
  sDefineCustomColors = '&Define Custom Colours >>';
  sBasicColors = '&Basic colours:';
  sCustomColors = '&Custom colours:';
  sColorSolidLeft = 'Colour';
  sDefaultTitle = 'Colour';
begin
  // Prevent task bar button press bringing owner window to foreground
  TDlgHelper.SetDlgParentToOwner(Self);
  // Set dialog title
  if Title <> '' then
    SetWindowText(Handle, PChar(Title));
  // Replace all occurences of "Color" with "Colour" if using English
  if PrimaryLangID(GetUserDefaultLangID) = LANG_ENGLISH then
  begin
    // All info used in this section gleaned from Color.dlg and ColorDlg.h from
    // Windows SDK
    SetWindowText(GetDlgItem(Handle, COLOR_ADD), PChar(sAddToCustomColors));
    SetWindowText(GetDlgItem(Handle, COLOR_MIX), PChar(sDefineCustomColors));
    SetWindowText(GetDlgItem(Handle, COLOR_SOLID_LEFT), PChar(sColorSolidLeft));
    // next controls have no ID, so we need to find them from caption
    SetWindowText(
      FindWindowEx(Handle, 0, 'Static', '&Basic colors:'),
      PChar(sBasicColors)
    );
    SetWindowText(
      FindWindowEx(Handle, 0, 'Static', '&Custom colors:'),
      PChar(sCustomColors)
    );
    if Title = '' then
      // no Title property set: change default title
      SetWindowText(Handle, PChar(sDefaultTitle));
  end;
  inherited;
end;

function TColorDialogEx.Execute: Boolean;
  {Displays dialog box. Esnures help button is displayed if HelpKeyword or
  HelpContext properties are set.
    @return True if user OKs and False if cancels.
  }
begin
  if (fHelpKeyword <> '') or (HelpContext <> 0) then
    Options := Options + [cdShowHelp];
  Result := inherited Execute;
end;

function TColorDialogEx.MessageHook(var Msg: TMessage): Boolean;
  {Intercepts messages sent to the dialog window before the dialog’s window
  procedure. This implementation changes default support for the help button
  to include the new HelpKeyword property and to use the program's own help
  manager.
    @param Msg [in/out] Specifies message. Unchanged by this method. May be
      modified by inherited implementation(s).
  }
begin
  if Msg.Msg = HelpMsgID then
    Result := DisplayHelp
  else
    Result := inherited MessageHook(Msg);
end;

function TColorDialogEx.TaskModalDialog(DialogFunc: Pointer;
  var DialogData): Bool;
  {Overridden method that updates the DialogData structure to route message
  processing through a custom hook function while storing the original hook
  function for later use.
    @param DialogFunc [in] Windows function to be called to execute dialog box
      (ChooseColor() in this case).
    @param DialogData [in] Data describing dialog box to be passed to DialogFunc
      (in this case of type TChooseColour).
  }
begin
  if NewStyleControls then
  begin
    fOldHook := @TChooseColor(DialogData).lpfnHook;
    TChooseColor(DialogData).lpfnHook := NewCCHook;
    TChooseColor(DialogData).lCustData := Integer(Self);
  end;
  // Call inherited function with (possibly modified) data structure
  Result := inherited TaskModalDialog(DialogFunc, DialogData);
end;


initialization

// Get ID of common dialog help message
HelpMsgID := RegisterWindowMessage(CommDlg.HELPMSGSTRING);

end.

