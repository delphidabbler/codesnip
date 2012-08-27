{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements extension of TThread class that adds property that indicates when
 * thread has completed.
}


unit UThreadEx;


interface


uses
  // Delphi
  Classes;


type
  {
  TThreadEx:
    Subclass of TThread that adds property that indicates when thread has
    completed execution.
  }
  TThreadEx = class(TThread)
  strict private
    fCompleted: Boolean;
      {Value of Completed property}
  protected
    procedure DoTerminate; override;
      {Sets Completed property to indicate that thread has terminated.
      }
  public
    property Completed: Boolean read fCompleted;
      {Indicates when threas has completed execution}
  end;


implementation


{ TThreadEx }

procedure TThreadEx.DoTerminate;
  {Sets Completed property to indicate that thread has terminated.
  }
begin
  inherited;
  fCompleted := True;
end;
         
end.

