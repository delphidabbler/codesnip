{
 * UHiddenWindow.pas
 *
 * Implements a class that provides a hidden window.
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
 * The Original Code is UHiddenWindow.pas
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


unit UHiddenWindow;


interface


uses
  // Delphi
  Messages;


type

  {
  THiddenWindow:
    Class that provides a hidden window that performs only default message
    handling.
  }
  THiddenWindow = class(TObject)
  private
    fHandle: THandle;
      {Value of Handle property}
  protected
    procedure WndProc(var Msg: TMessage); virtual;
      {Window procedure for hidden window. Performs only default message
      processing.
        @param Msg [in/out] Message being process. May be modified by default
          processing.
      }
  public
    constructor Create;
      {Class constructor. Creates hidden window.
      }
    destructor Destroy; override;
      {Class destructor. Destroys hidden window.
      }
    property Handle: THandle read fHandle;
      {Handle to hidden window}
  end;


implementation


uses
  // Delphi
  Classes, Windows;


{ THiddenWindow }

constructor THiddenWindow.Create;
  {Class constructor. Creates hidden window.
  }
begin
  inherited;
  fHandle := AllocateHWnd(WndProc);
end;

destructor THiddenWindow.Destroy;
  {Class destructor. Destroys hidden window.
  }
begin
  DeallocateHWnd(Handle);
  inherited;
end;

procedure THiddenWindow.WndProc(var Msg: TMessage);
  {Window procedure for hidden window. Performs only default message processing.
    @param Msg [in/out] Message being process. May be modified by default
      processing.
  }
begin
  with Msg do
    Result := DefWindowProc(Handle, Msg, WParam, LParam);
end;

end.

