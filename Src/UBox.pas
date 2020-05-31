{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a generic class that can wrap any type in an object. Designed for
 * use in wrapping value types and strings in objects.
}


unit UBox;


interface


type
  ///  <summary>Generic class that wraps a type in an object.</summary>
  ///  <remarks>Although this type can be used to wrap any type it is aimed at
  ///  wrapping value types and strings.</remarks>
  TBox<T> = class(TObject)
  strict private
    var
      ///  <summary>Value of Value property.</summary>
      fValue: T;
  public
    ///  <summary>Constructs an object that wraps the specified value.</summary>
    constructor Create(Value: T);
    ///  <summary>Value wrapped by object.</summary>
    property Value: T read fValue;
  end;


implementation


{ TBox<T> }

constructor TBox<T>.Create(Value: T);
begin
  fValue := Value;
end;

end.
