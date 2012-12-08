{
 * UThreadEx.pas
 *
 * Implements extension of TThread class that adds property that indicates when
 * thread has completed.
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
 * The Original Code is UThreadEx.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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
  private
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

