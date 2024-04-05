{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that provides a hidden window.
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
  strict private
    fHandle: THandle;
      {Value of Handle property}
  strict protected
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
  Msg.Result := DefWindowProc(Handle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

end.

