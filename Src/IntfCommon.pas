{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Common general purpose interfaces.
}


unit IntfCommon;


interface


type
  ///  <summary>Interface that defines a method that clones a copy of the
  ///  implementing object.</summary>
  ///  <remarks>Any object that supports cloning should implement this
  ///  interface.</remarks>
  IClonable = interface(IInterface)
    ['{5718FBDF-C307-4E28-94A2-76D672486324}']
    ///  <summary>Creates and returns a new object that is an exact copy of the
    ///  current object.</summary>
    ///  <returns>IInterface reference to cloned object.</returns>
    function Clone: IInterface;
  end;

type
  ///  <summary>Interface that defines a method that changes the implementing
  ///  object to be an exact copy of a given object of a compatible type.
  ///  </summary>
  ///  <remarks>Any object that supports assignment should implement this
  ///  interface.</remarks>
  IAssignable = interface(IInterface)
    ['{AB1EC037-FB19-4E6F-ACA6-8113891D9089}']
    ///  <summary>Modifies the current object to be an exact copy of another
    ///  object.</summary>
    ///  <param name="Src">IInterface [in] Reference to IInterface of object to
    ///  be assigned.</param>
    ///  <exception>An EBug exception should be raised if Src is not type
    ///  compatible with this object.</exception>
    procedure Assign(const Src: IInterface);
  end;


implementation

end.

