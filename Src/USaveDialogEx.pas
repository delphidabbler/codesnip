{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a subclass of the Save dialog box to enable the dialog to align
 * itself over its owner and to work correctly with the Vista task bar. Also
 * adds support for help keywords and help button.
 *
 * Requires that Application.ModalPopupMode <> pmNone.
}


unit USaveDialogEx;


interface


uses
  // Delphi
  Classes, Dialogs, Messages, Windows,
  // Project
  UCommonDlg;


type

  {
  TSaveDialogEx:
    Subclasses the Save dialog box to enable the dialog to align itself over its
    owner and to work correctly with the Vista task bar. Also adds support for
    help keywords and help button.
  }
  TSaveDialogEx = class(TSaveDialog)
  strict private
    fHelpKeyword: string;             // Value of HelpKeyword property
    fWantDefaultHelpSupport: Boolean; // Value of WantDefaultHelpSupport prop
    fHook: TFileDlgHook;              // Object that wraps dlg and hook function
  strict protected
    function TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool;
      override;
      {Overridden method that updates the DialogData structure to route message
      processing through a custom explorer hook object.
        @param DialogFunc [in] Windows function to be called to execute dialog
          box (GetOpenFileName() in this case).
        @param DialogData [in] Data describing dialog box to be passed to
          DialogFunc (in this case of type TOpenFileName).
      }
    function MessageHook(var Msg: TMessage): Boolean; override;
      {Intercepts messages sent to the dialog window before the dialog’s window
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
    function DisplayHelp: Boolean; virtual;
      {Calls program's help manager to display help if HelpKeyword property is
      set.
        @return True if help manager was called or False if not (i.e.HelpKeyword
          not set).
      }
    property WantDefaultHelpSupport: Boolean
      read fWantDefaultHelpSupport write fWantDefaultHelpSupport;
      {Indicates if support for default help processing is required. When false
      the dialog never displays the help button. When true display depends on
      state of HelpKeyword property}
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Creates dialog box.
        @param AOwner [in] Owning component. Dialog box will be aligned over
          AOwner.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    function Execute: Boolean; override;
      {Displays dialog box. Ensures help button is displayed if HelpKeyword
      property is set.
        @return True if user OKs and False if cancels.
      }
  published
    property HelpKeyword: string
      read fHelpKeyword write fHelpKeyword;
      {Help keyword used to access help topic when help button clicked}
  end;


implementation


uses
  // Delphi
  Sysutils, Controls, CommDlg,
  // Project
  UDlgHelper, UHelpMgr;


{ TSaveDialogEx }

constructor TSaveDialogEx.Create(AOwner: TComponent);
  {Class constructor. Creates dialog box.
    @param AOwner [in] Owning component. Dialog box will be aligned over AOwner.
  }
begin
  inherited;
  fHook := TFileDlgHook.Create(Self);
  // Enable default help processing
  fWantDefaultHelpSupport := True;
end;

destructor TSaveDialogEx.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fHook);
  inherited;
end;

function TSaveDialogEx.DisplayHelp: Boolean;
  {Calls program's help manager to display help if HelpKeyword property is set.
    @return True if help manager was called or False if not (HelpKeyword not
      set).
  }
begin
  Result := TCommonDlgHelper.ShowHelp(HelpKeyword);
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
  {Displays dialog box. Ensures help button is displayed if HelpKeyword property
  is set.
    @return True if user OKs and False if cancels.
  }
begin
  if WantDefaultHelpSupport and (HelpKeyword <> '') then
    Options := Options + [ofShowHelp]
  else
    Options := Options - [ofShowHelp];
  Result := inherited Execute;
end;

function TSaveDialogEx.MessageHook(var Msg: TMessage): Boolean;
  {Intercepts messages sent to the dialog window before the dialog’s window
  procedure. This implementation changes default support for the help button
  to include the new HelpKeyword property and to use the program's own help
  manager.
    @param Msg [in/out] Specifies message. Unchanged by this method. May be
      modified by inherited implementation(s).
    @return False to pass message on to dilog's window procedure, True to
      prevent this.
  }
begin
  if TCommonDlgHelper.IsHelpMessage(Msg) and WantDefaultHelpSupport then
    Result := DisplayHelp
  else
    Result := inherited MessageHook(Msg);
end;

function TSaveDialogEx.TaskModalDialog(DialogFunc: Pointer;
  var DialogData): Bool;
  {Overridden method that updates the DialogData structure to route message
  processing through a custom explorer hook object.
    @param DialogFunc [in] Windows function to be called to execute dialog box
      (GetOpenFileName() in this case).
    @param DialogData [in] Data describing dialog box to be passed to DialogFunc
      (in this case of type TOpenFilename).
  }
begin
  if NewStyleControls and not (ofOldStyleDialog in Options) then
    fHook.Initialise(DialogData);
  // Call inherited function with (modified) data structure
  Result := inherited TaskModalDialog(DialogFunc, DialogData);
end;

end.

