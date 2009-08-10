{
 * UStacks.pas
 *
 * Provides an implementation of a string stack.
 *
 * v1.0 of 30 Dec 2008  - Original version.
 *
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
 * The Original Code is UStacks.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UStacks;


interface


uses
  // Delphi
  SysUtils, Classes;


type

  {
  EStackError:
    Class of exception raised when stack objects report an error.
  }
  EStackError = class(Exception);


  {
  TStringStack:
    Implements a stack of strings and associated objects.
  }
  TStringStack = class(TObject)
  strict private
    fStack: TStringList;
      {String list object used to store strings and objects on stack}
    function GetCount: Integer;
      {Gets number of items in stack.
        @return Required count.
      }
  public
    constructor Create;
      {Class constructor. Sets up object.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure Clear;
      {Clears the stack.
      }
    function IsEmpty: Boolean;
      {Checks if the stack is empty.
        @return True if stack is empty, False if not.
      }
    function Top: string; overload;
      {Get string at top of stack without removing it.
        @return String at top of stack.
      }
    function Top(out Obj: TObject): string; overload;
      {Gets string and associated object at top of stack without removing them.
        @param Obj [out] Set to object at top of stack.
        @return String at top of stack.
      }
    function Pop: string; overload;
      {Gets and removes string at top of stack. Any associated object is lost.
        @return String at top of stack.
      }
    function Pop(out Obj: TObject): string; overload;
      {Gets and removes string and associated object at top of stack.
        @param Obj [out] Set to object at top of stack.
        @return String at top of stack.
      }
    procedure Push(const S: string); overload;
      {Pushes a string onto the stack.
        @param S [in] String to be pushed onto stack.
      }
    procedure Push(const S: string; const Obj: TObject); overload;
      {Pushes string and associated object onto the stack.
        @param S [in] String to be pushed onto stack.
        @param Obj [in] Associated object to be pushed onto stack.
      }
    property Count: Integer read GetCount;
      {Number of items in the stack}
  end;


implementation


resourcestring
  // Error message
  sEmptyStack = 'Stack is empty';


{ TStringStack }

procedure TStringStack.Clear;
  {Clears the stack.
  }
begin
  fStack.Clear;
end;

constructor TStringStack.Create;
  {Class constructor. Sets up object.
  }
begin
  inherited;
  fStack := TStringList.Create;
end;

destructor TStringStack.Destroy;
  {Class destructor. Tears down object.
  }
begin
  fStack.Free;
  inherited;
end;

function TStringStack.GetCount: Integer;
  {Gets number of items in stack.
    @return Required count.
  }
begin
  Result := fStack.Count;
end;

function TStringStack.IsEmpty: Boolean;
  {Checks if the stack is empty.
    @return True if stack is empty, False if not.
  }
begin
  Result := Count = 0;
end;

function TStringStack.Pop: string;
  {Gets and removes string at top of stack. Any associated object is lost.
    @return String at top of stack.
  }
var
  Dummy: TObject; // stores unused object associated with string
begin
  Result := Pop(Dummy);
end;

function TStringStack.Pop(out Obj: TObject): string;
  {Get and removes string and associated object at top of stack.
    @param Obj [out] Set to object at top of stack.
    @return String at top of stack.
  }
begin
  Result := Top(Obj);
  fStack.Delete(Pred(Count));
end;

procedure TStringStack.Push(const S: string);
  {Pushes a string onto the stack.
    @param S [in] String to be pushed onto stack.
  }
begin
  fStack.Add(S);
end;

procedure TStringStack.Push(const S: string; const Obj: TObject);
  {Pushes string and associated object onto the stack.
    @param S [in] String to be pushed onto stack.
    @param Obj [in] Associated object to be pushed onto stack.
  }
begin
  fStack.AddObject(S, Obj);
end;

function TStringStack.Top: string;
  {Get string at top of stack without removing it.
    @return String at top of stack.
  }
begin
  if IsEmpty then
    raise EStackError.Create(sEmptyStack);
  Result := fStack[Pred(Count)];
end;

function TStringStack.Top(out Obj: TObject): string;
  {Gets string and associated object at top of stack without removing them.
    @param Obj [out] Set to object at top of stack.
    @return String at top of stack.
  }
begin
  Result := Top;
  Obj := fStack.Objects[Pred(Count)];
end;


end.

