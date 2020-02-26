{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a base class for forms that can access topics in the help file.
}


unit FmHelpAware;


interface


uses
  // Delphi
  Classes,
  // Projct
  FmBase;


type

  {
  THelpAwareForm:
    Form class that provides help functionality and F1 key awareness to
    descendant forms.
  }
  THelpAwareForm = class(TBaseForm)
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  strict private
    fDisableHelp: Boolean;  // Value of DisableHelp property
  strict protected
    procedure DisplayHelp; overload; virtual;
      {Displays help specified by a keyword. Form's HelpKeyword is used if set
      otherwise the keyword returned by CustomHelpKeyword method is used. Help
      context numbers are ignored. Help is not called if DisableHelp is true or
      if no keyword can be found.
      }
    procedure DisplayHelp(const AKeyword: string); overload; virtual;
      {Displays help using a keyword. Does nothing if DisableHelp property is
      true.
        @param AKeyword [in] Keyword to be used.
      }
    function CustomHelpKeyword: string; virtual;
      {Gets a help keyword to be used if form's HelpKeyword property is not set.
      Default is to use name of form. Subclasses can override this behaviour.
        @return Name of form.
      }
    property DisableHelp: Boolean
      read fDisableHelp write fDisableHelp default False;
      {Determines whether help is enabled or disabled. When true form is not
      "help-enabled"}
  end;


implementation


uses
  // Delphi
  Windows,
  // Project
  UHelpMgr;


{$R *.dfm}

{ THelpAwareForm }

function THelpAwareForm.CustomHelpKeyword: string;
  {Gets a help keyword to be used if form's HelpKeyword property is not set.
  Default is to use name of form. Subclasses can override this behaviour.
    @return Name of form.
  }
begin
  Result := Name;
end;

procedure THelpAwareForm.DisplayHelp;
  {Displays help specified by a keyword. Form's HelpKeyword is used if set
  otherwise the keyword returned by CustomHelpKeyword method is used. Help
  context numbers are ignored. Help is not called if DisableHelp is true or if
  no keyword can be found.
  }
var
  AKeyword: string;   // A-link keyword
begin
  if DisableHelp then
    Exit;
  if HelpKeyword <> '' then
    AKeyword := HelpKeyword
  else
    AKeyword := CustomHelpKeyword;
  if AKeyword <> '' then
    DisplayHelp(AKeyword);
end;

procedure THelpAwareForm.DisplayHelp(const AKeyword: string);
  {Displays help using a keyword. Does nothing if DisableHelp property is true.
    @param AKeyword [in] Keyword to be used.
  }
begin
  if not fDisableHelp then
    HelpMgr.ShowHelp(AKeyword);
end;

procedure THelpAwareForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  {Traps form key-down event. If FI key pressed with no modifiers we display
  help.
    @param Sender [in] Not used.
    @param Key [in/out] Key pressed by user. Set to 0 if F1 to inhibit further
      processing.
    @param Shift [in] Modifier keys pressed.
  }
begin
  inherited;
  if (Key = VK_F1) and (Shift = []) then
  begin
    DisplayHelp;
    Key := 0;
  end;
end;

end.

