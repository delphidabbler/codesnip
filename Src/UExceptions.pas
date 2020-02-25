{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Defines application's exception classes and a handler for untrapped
 * exceptions.
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
  UBaseObjects, UStructs;


type

  TExceptionHelper = class(TNoConstructObject)
  public
    class function Clone(const ExObj: Exception): Exception;
      {Creates a new exception that is an approximate copy of a given exception.
      Any inner exception information is lost. Only the message is copied if the
      exception is not an EAssignable descendant.
        @param ExObj [in] Exception to be cloned. Must be an Exception
          descendant.
        @return New cloned exception.
      }
  end;

  {
  EAssignable:
    Base class for exceptions that can be assigned the properties of another
    exception. Descendants that add extra properties should override Assign to
    copy the extra properties. Descendants may limit the exception classes that
    they accept for assignment.
  }
  EAssignable = class(Exception)
  public
    constructor Create(const E: Exception); overload;
      {Create the exception object with same properties as a given exception.
        @param E [in] Exception to copy.
      }
    procedure Assign(const E: Exception); virtual;
      {Sets this exception object's properties to be the same as another
      exception. Any inner exception information is lost.
        @param E [in] Exception whose properties are to be copied.
      }
  end;

  {
  EBug:
    Base class for exceptions that are treated as bugs.
  }
  EBug = class(EAssignable);

  {
  ECodeSnip:
    Base class for exceptions that are treated as "expected" and are not bugs.
    Such exception must support an Assign method that copies the properties of
    a given exception to the current exception. Descendants that add extra
    properties should extend Assign to copy the extra properties. Descendants
    may limit the exception classes that they accept.
  }
  ECodeSnip = class(EAssignable);

  {
  EValidation:
    Base class fo exceptions that are raised when some kind of validation fails.
    Has a Selection property that may indicate the position and, optionally,
    length of the error, where this makes sense. The HasSelection property is
    set to False when there is no selection information available.
  }
  EValidation = class(ECodeSnip)
  strict private
    var fSelection: TSelection; // Value of Selection property
    var fHasSelection: Boolean; // Value of HasSelection property
    function GetSelection: TSelection;
      {Read accessor for Selection property. Must not be called when
      HasSelection is false.
        @return Value of property.
      }
    procedure SetSelection(const ASelection: TSelection);
      {Write accessor for Selection property. Sets HasSelection to True.
        @param Value [in] New property value.
      }
  public
    constructor Create(const Msg: string; const ASelection: TSelection);
      overload;
      {Creates an exception with a specified message along with selection info.
        @param Msg [in] Exception message.
        @param ASelection [in] Selection information.
      }
    constructor CreateFmt(const Msg: string; const Args: array of const;
      const ASelection: TSelection); overload;
      {Creates an exception with a formatted message along with selection info.
        @param Msg [in] Exception message format string.
        @param Args [in] Message format arguments.
        @param ASelection [in] Selection information.
      }
    procedure Assign(const E: Exception); override;
      {Sets this exception object's properties to be the same as another
      exception.
        @param E [in] Exception whose properties are to be copied. Must be
          another EValidation.
      }
    property HasSelection: Boolean read fHasSelection;
      {Informs if the exception has selection information. When False the
      Selection property cannot be read}
    property Selection: TSelection read GetSelection write SetSelection;
      {Provides information about a selection associated with the exception.
      Cannot be read if HasSelection is false}
  end;

  {
  EDataEntry:
    Class for exception raised during data entry in forms. Carries a reference
    to the control where the error was made. Enables control with erroneous
    entry to be focussed.
  }
  EDataEntry = class(EValidation)
  strict private
    fCtrl: TWinControl; // Value of Ctrl property
  public
    constructor Create(const Msg: string; const Ctrl: TWinControl); overload;
      {Creates an exception with a specified message that refers to a specified
      control.
        @param Msg [in] Exception message.
        @param Ctrl [in] Control to which exception relates.
      }
    constructor Create(const Msg: string; const Ctrl: TWinControl;
      const Selection: TSelection); overload;
      {Creates an exception with a specified message that refers to a specified
      control and selection within it.
        @param Msg [in] Exception message.
        @param Ctrl [in] Control to which exception relates.
        @param Selection [in] Selection containing error in Ctrl.
      }
    constructor CreateFmt(const Msg: string; const Args: array of const;
      const Ctrl: TWinControl; const Selection: TSelection); overload;
      {Creates an exception with a message built from a format string and
      parameters that refers to a specified control and selection within it.
        @param Msg [in] Format string for exception message.
        @param Args [in] Array of arguments for format string.
        @param Ctrl [in] Control to which exception relates.
        @param Selection [in] Selection containing error in Ctrl.
      }
    constructor CreateFmt(const Msg: string; const Args: array of const;
      const Ctrl: TWinControl); overload;
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


implementation


uses
  // Delphi
  Classes;


{ EAssignable }

procedure EAssignable.Assign(const E: Exception);
  {Sets this exception object's properties to be the same as another exception.
  Any inner exception information is lost.
    @param E [in] Exception whose properties are to be copied.
  }
begin
  Self.Message := E.Message;
end;

constructor EAssignable.Create(const E: Exception);
  {Create the exception object with same properties as a given exception.
    @param E [in] Exception to copy.
  }
begin
  inherited Create('');
  Assign(E);  // we call assign so that descendants can copy extra properties
end;

{ EValidation }

procedure EValidation.Assign(const E: Exception);
  {Sets this exception object's properties to be the same as another exception.
    @param E [in] Exception whose properties are to be copied. Must be  another
    EValidation.
  }
begin
  Assert(E is EValidation, ClassName + '.Assign: E must be a EValidation');
  inherited;
  fHasSelection := (E as EValidation).fHasSelection;
  fSelection := (E as EValidation).fSelection;
end;

constructor EValidation.Create(const Msg: string; const ASelection: TSelection);
  {Creates an exception with a specified message along with selection info.
    @param Msg [in] Exception message.
    @param ASelection [in] Selection information.
  }
begin
  inherited Create(Msg);
  SetSelection(ASelection);
end;

constructor EValidation.CreateFmt(const Msg: string; const Args: array of const;
  const ASelection: TSelection);
  {Creates an exception with a formatted message along with selection info.
    @param Msg [in] Exception message format string.
    @param Args [in] Message format arguments.
    @param ASelection [in] Selection information.
  }
begin
  inherited CreateFmt(Msg, Args);
  SetSelection(ASelection);
end;

function EValidation.GetSelection: TSelection;
  {Read accessor for Selection property. Must not be called when
  HasSelection is false.
    @return Value of property.
  }
begin
  Assert(HasSelection,
    ClassName + '.GetPosition: Can''t read Selection property');
  Result := fSelection;
end;

procedure EValidation.SetSelection(const ASelection: TSelection);
  {Write accessor for Selection property. Sets HasSelection to True.
    @param Value [in] New property value.
  }
begin
  fHasSelection := True;
  fSelection := ASelection;
end;

{ EDataEntry }

procedure EDataEntry.Assign(const E: Exception);
  {Sets this exception object's properties to be the same as another
  exception.
    @param E [in] Exception whose properties are to be copied. Must be another
      EDataEntry.
  }
begin
  Assert(E is EDataEntry, ClassName + '.Assign: E must be a EDataEntry');
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

constructor EDataEntry.Create(const Msg: string; const Ctrl: TWinControl;
  const Selection: TSelection);
  {Creates an exception with a specified message that refers to a specified
  control and selection within it.
    @param Msg [in] Exception message.
    @param Ctrl [in] Control to which exception relates.
    @param Selection [in] Selection containing error in Ctrl.
  }
begin
  inherited Create(Msg, Selection);
  fCtrl := Ctrl;
end;

constructor EDataEntry.CreateFmt(const Msg: string; const Args: array of const;
  const Ctrl: TWinControl; const Selection: TSelection);
  {Creates an exception with a message built from a format string and parameters
  that refers to a specified control and selection within it.
    @param Msg [in] Format string for exception message.
    @param Args [in] Array of arguments for format string.
    @param Ctrl [in] Control to which exception relates.
    @param Selection [in] Selection containing error in Ctrl.
  }
begin
  inherited CreateFmt(Msg, Args, Selection);
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

{ TExceptionHelper }

class function TExceptionHelper.Clone(const ExObj: Exception): Exception;
  {Creates a new exception that is an approximate copy of a given exception. Any
  inner exception information is lost. Only the message is copied if the
  exception is not an EAssignable descendant.
    @param ExObj [in] Exception to be cloned. Must be an Exception
      descendant.
    @return New cloned exception.
  }
begin
  if ExObj is EAssignable then
  begin
    Result := ExObj.ClassType.Create as EAssignable;
    (Result as EAssignable).Assign(ExObj as EAssignable);
  end
  else
  begin
    Result := ExObj.ClassType.Create as Exception;
    Result.Message := ExObj.Message;
  end;
end;

end.

