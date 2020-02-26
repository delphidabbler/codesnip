{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements an open dialog box subclass that aligns itself over its owner and
 * works correctly with the Vista task bar. Dialog also supports help keywords.
}


unit UOpenDialogEx;



interface


uses
  // Delphi
  Classes, Dialogs, Messages, Windows,
  // Project
  UCommonDlg;


type

  {
  TOpenDialogEx:
    Subclasses the Open dialog box to enable the dialog to align itself over its
    owner and to work correctly with the Vista task bar. Also adds support for
    help keywords and help button.
  }
  TOpenDialogEx = class(TOpenDialog)
  strict private
    fHelpKeyword: string;       // Value of HelpKeyword property
    fHook: TFileDlgHook;        // Object that wraps dlg and hook function
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
  SysUtils, Controls,
  // Project
  UDlgHelper;


{ TOpenDialogEx }

constructor TOpenDialogEx.Create(AOwner: TComponent);
  {Class constructor. Creates dialog box.
    @param AOwner [in] Owning component. Dialog box will be aligned over AOwner.
  }
begin
  inherited;
  fHook := TFileDlgHook.Create(Self);
end;

destructor TOpenDialogEx.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fHook);
  inherited;
end;

procedure TOpenDialogEx.DoShow;
  {Sets up dialog just before it is displayed.
  }
begin
  // Prevent task bar button press bringing owner window to foreground
  TDlgHelper.SetDlgParentToOwner(Self);
  inherited;
end;

function TOpenDialogEx.Execute: Boolean;
  {Displays dialog box. Ensures help button is displayed if HelpKeyword property
  is set.
    @return True if user OKs and False if cancels.
  }
begin
  if HelpKeyword <> '' then
    Options := Options + [ofShowHelp]
  else
    Options := Options - [ofShowHelp];
  Result := inherited Execute;
end;

function TOpenDialogEx.MessageHook(var Msg: TMessage): Boolean;
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
  if TCommonDlgHelper.IsHelpMessage(Msg) then
    Result := TCommonDlgHelper.ShowHelp(HelpKeyword)
  else
    Result := inherited MessageHook(Msg);
end;

function TOpenDialogEx.TaskModalDialog(DialogFunc: Pointer;
  var DialogData): Bool;
  {Overridden method that updates the DialogData structure to route message
  processing through a custom explorer hook object.
    @param DialogFunc [in] Windows function to be called to execute dialog box
      (GetOpenFileName() in this case).
    @param DialogData [in] Data describing dialog box to be passed to DialogFunc
      (in this case of type TOpenFileName).
  }
begin
  if NewStyleControls and not (ofOldStyleDialog in Options) then
    fHook.Initialise(DialogData);
  // Call inherited function with (modified) data structure
  Result := inherited TaskModalDialog(DialogFunc, DialogData);
end;

end.

