{
 * UExceptions.pas
 *
 * Defines application's exception classes and a handler for untrapped
 * exceptions.
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
 * The Original Code is UExceptions.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UExceptions;


interface


{
  NOTES:

  Exceptions are treated as bugs unless they derive from ECodeSnip. This means
  that the application needs to trap any expected exceptions that are not
  derived from ECodeSnip.

  The EBug exception class is also provided for explicitly generating bug
  exceptions.

  Untrapped ECodeSnip exceptions result in the display of an error dialog box
  while all others lead to display of a bug report dialog box.
}


uses
  // Delphi
  SysUtils, Controls,
  // Project
  UBaseObjects;


type

  {
  EBug:
    Base class for exceptions that are treated as bugs.
  }
  EBug = class(Exception);

  {
  ECodeSnip:
    Base class for exceptions that are treated as "expected" and are not bugs.
    Such exception must support an Assign method that copies the properties of
    a given exception to the current exception. Descendants that add extra
    properties should extend Assign to copy the extra properties. Descendants
    may limit the exception classes that they accept.
  }
  ECodeSnip = class(Exception)
  public
    constructor Create(const E: Exception); overload;
      {Create the exception object with same properties as a given exception.
        @param E [in] Exception to copy.
      }
    procedure Assign(const E: Exception); virtual;
      {Sets this exception object's properties to be the same as another
      exception.
        @param E [in] Exception whose properties are to be copied.
      }
  end;

  {
  EDataEntry:
    Class for exception raised during data entry in forms. Carries a reference
    to the control where the error was made. Enables control with erroneous
    entry to be focussed.
  }
  EDataEntry = class(ECodeSnip)
  strict private
    fCtrl: TWinControl;
      {Value of Ctrl property}
  public
    constructor Create(const Msg: string; const Ctrl: TWinControl);
      {Creates an exception with a specified message that refers to a specified
      control.
        @param Msg [in] Exception message.
        @param Ctrl [in] Control to which exception relates.
      }
    constructor CreateFmt(const Msg: string; const Args: array of const;
      const Ctrl: TWinControl);
      {Creates an exception with a message built from a format string and
      parameters that refers to a specified control.
        @param Msg [in] Format string for exception message.
        @param Args [in] Array of arguments for format string.
        @param Ctrl [in] Control to which exception relates.
      }
    procedure Assign(const E: Exception); override;
      {Sets this exception object's properties to be the same as another
      exception.
        @param E [in] Exception whose properties are to be copied. Must be
          another EDataEntry.
      }
    property Ctrl: TWinControl read fCtrl;
      {Reference to control to which exception relates}
  end;

  {
  TExceptionHandler:
    Static class providing exception handling support method.
  }
  TExceptionHandler = class(TNoConstructObject)
  public
    class procedure Handler(Sender: TObject; E: Exception);
      {An event handler for the Application.OnException event. Provides custom
      handling for un-trapped exceptions. A message box is displayed for an
      expected exception while an unexpected exception cause a bug report dialog
      to be displayed.
        @param Sender [in] Not used.
        @param E [in] Exception to be handled.
      }
  end;


implementation


uses
  // Delphi
  Classes,
  // Project
  UMessageBox, FmTrappedBugReportDlg;


{ TExceptionHandler }

class procedure TExceptionHandler.Handler(Sender: TObject; E: Exception);
  {An event handler for the Application.OnException event. Provides custom
  handling for un-trapped exceptions. A message box is displayed for an expected
  exception while an unexpected exception cause a bug report dialog to be
  displayed.
    @param Sender [in] Not used.
    @param E [in] Exception to be handled.
  }
begin
  if (E is ECodeSnip) or (E is EFileStreamError) then
    TMessageBox.Error(nil, E.Message)
  else
    TTrappedBugReportDlg.Execute(nil, E);
end;

{ ECodeSnip }

procedure ECodeSnip.Assign(const E: Exception);
  {Sets this exception object's properties to be the same as another
  exception.
    @param E [in] Exception whose properties are to be copied.
  }
begin
  Self.Message := E.Message;
end;

constructor ECodeSnip.Create(const E: Exception);
  {Create the exception object with same properties as a given exception.
    @param E [in] Exception to copy.
  }
begin
  inherited Create('');
  Assign(E);  // we call assign so that descendants can copy extra properties
end;

{ EDataEntry }

procedure EDataEntry.Assign(const E: Exception);
  {Sets this exception object's properties to be the same as another
  exception.
    @param E [in] Exception whose properties are to be copied. Must be another
      EDataEntry.
  }
begin
  Assert(E is EDataEntry,                                  // ** do not localise
    ClassName + '.Assign: E must be a EDataEntry');
  inherited;
  fCtrl := (E as EDataEntry).Ctrl;
end;

constructor EDataEntry.Create(const Msg: string; const Ctrl: TWinControl);
  {Creates an exception with a specified message that refers to a specified
  control.
    @param Msg [in] Exception message.
    @param Ctrl [in] Control to which exception relates.
  }
begin
  inherited Create(Msg);
  fCtrl := Ctrl;
end;

constructor EDataEntry.CreateFmt(const Msg: string; const Args: array of const;
  const Ctrl: TWinControl);
  {Creates an exception with a message built from a format string and parameters
  that refers to a specified control.
    @param Msg [in] Format string for exception message.
    @param Args [in] Array of arguments for format string.
    @param Ctrl [in] Control to which exception relates.
  }
begin
  inherited CreateFmt(Msg, Args);
  fCtrl := Ctrl;
end;

end.

