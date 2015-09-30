{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a colour dialogue box subclass that aligns itself over its owner,
 * works correctly with the Vista task bar, provides a Title property and uses
 * UK English. Dialogue also supports help keywords.
}


unit UColorDialogEx;


interface


uses
  // Delphi
  Classes, Dialogs, Messages, Controls, Windows,
  // Project
  UCommonDlg;


type

  {
  TColorDialogEx:
    Subclass of the colour dialogue box that aligns itself over its owner, works
    correctly with the Vista task bar, provides a Title property and uses UK
    English. Also adds support for help keywords and help button.
  }
  TColorDialogEx = class(TColorDialog)
  strict private
    var fTitle: TCaption;     // Value of Title property
    var fHelpKeyword: string; // Value of HelpKeyword property
    var fHook: TColorDlgHook; // Object that wraps dialogue and hook function
  strict protected
    function TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool;
      override;
      {Overridden method that updates the DialogData structure to route message
      processing through a custom hook object.
        @param DialogFunc [in] Windows function to be called to execute dialogue
          box (ChooseColor() in this case).
        @param DialogData [in] Data describing dialogue box to be passed to
          DialogFunc (in this case of type TChooseColour).
      }
    function MessageHook(var Msg: TMessage): Boolean; override;
      {Intercepts messages sent to the dialogue window before the dialogue's
      window procedure. This implementation changes default support for the help
      button to include the new HelpKeyword property and to use the program's
      own help manager.
        @param Msg [in/out] Specifies message. Unchanged by this method. May be
          modified by inherited implementation(s).
        @return False to pass message on to dilog's window procedure, True to
          prevent this.
      }
    procedure DoShow; override;
      {Sets up dialogue just before it is displayed.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Object constructor. Creates dialogue box.
        @param AOwner [in] Owning component. Dialogue box will be aligned over
          AOwner.
      }
    destructor Destroy; override;
      {Object desctructor. Tears down object.
      }
    function Execute: Boolean; override;
      {Displays dialogue box. Ensures help button is displayed if HelpKeyword
      property is not set.
        @return True if user OKs and False if cancels.
      }
  published
    property Title: TCaption read fTitle write fTitle;
      {Enables dialogue box title to be customised. If Title is left blank the
      default dialogue box title is used}
    property HelpKeyword: string
      read fHelpKeyword write fHelpKeyword;
      {Help keyword used to access help topic when help button clicked}
  end;


implementation


uses
  // Project
  UDlgHelper;


{ TColorDialogEx }

constructor TColorDialogEx.Create(AOwner: TComponent);
  {Object constructor. Creates dialogue box.
    @param AOwner [in] Owning component. Dialogue box will be aligned over
      AOwner.
  }
begin
  inherited;
  fHook := TColorDlgHook.Create(Self);
end;

destructor TColorDialogEx.Destroy;
  {Object desctructor. Tears down object.
  }
begin
  fHook.Free;
  inherited;
end;

procedure TColorDialogEx.DoShow;
  {Sets up dialogue just before it is displayed.
  }
const
  // Identifiers of controls using American spelling of "colour" (per ColorDlg.h
  // from Windows SDK).
  COLOR_ADD        = 712;
  COLOR_MIX        = 719;
  COLOR_SOLID_LEFT = 730;
resourcestring
  // UK English strings to replace American in dialogue box
  sAddToCustomColors = '&Add to Custom Colours';
  sDefineCustomColors = '&Define Custom Colours >>';
  sBasicColors = '&Basic colours:';
  sCustomColors = '&Custom colours:';
  sColorSolidLeft = 'Colour';
  sDefaultTitle = 'Colour';
begin
  // Prevent task bar button press bringing owner window to foreground
  TDlgHelper.SetDlgParentToOwner(Self);
  // Set dialogue title
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
  {Displays dialogue box. Ensures help button is displayed if HelpKeyword
  property is not set.
    @return True if user OKs and False if cancels.
  }
begin
  if HelpKeyword <> '' then
    Options := Options + [cdShowHelp]
  else
    Options := Options - [cdShowHelp];
  Result := inherited Execute;
end;

function TColorDialogEx.MessageHook(var Msg: TMessage): Boolean;
  {Intercepts messages sent to the dialogue window before the dialogue's window
  procedure. This implementation changes default support for the help button
  to include the new HelpKeyword property and to use the program's own help
  manager.
    @param Msg [in/out] Specifies message. Unchanged by this method. May be
      modified by inherited implementation(s).
    @return False to pass message on to dilog's window procedure, True to
      prevent this.
  }
begin
  if TCommonDlgHelper.IsHelpMessage(Msg) then
    Result := TCommonDlgHelper.ShowHelp(HelpKeyword)
  else
    Result := inherited MessageHook(Msg);
end;

function TColorDialogEx.TaskModalDialog(DialogFunc: Pointer;
  var DialogData): Bool;
  {Overridden method that updates the DialogData structure to route message
  processing through a custom hook object.
    @param DialogFunc [in] Windows function to be called to execute dialogue box
      (ChooseColor() in this case).
    @param DialogData [in] Data describing dialogue box to be passed to
      DialogFunc (in this case of type TChooseColour).
  }
begin
  if NewStyleControls then
    fHook.Initialise(DialogData);
  // Call inherited function with (possibly modified) data structure
  Result := inherited TaskModalDialog(DialogFunc, DialogData);
end;

end.

