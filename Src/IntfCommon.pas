{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Common general purpose interfaces.
}


unit IntfCommon;


interface


type

  {
  IClonable:
    Interface that defines a method that clones a copy of the implementing
    object. Any object that supports cloning should implement this interface.
  }
  IClonable = interface(IInterface)
    ['{5718FBDF-C307-4E28-94A2-76D672486324}']
    function Clone: IInterface;
      {Create a new instance of the object that is an extact copy and return it.
        @return New object's IInterface interface.
      }
  end;

  {
  IAssignable:
    Interface that defines a method that sets the implementing object to be
    equal to the assigned object. Any object that supports assignment should
    implement this interface.
  }
  IAssignable = interface(IInterface)
    ['{AB1EC037-FB19-4E6F-ACA6-8113891D9089}']
    procedure Assign(const Src: IInterface);
      {Assigns properties of a given object to this object.
        @param Src [in] Object whose properties are to be copied.
        @except EBug should be raised if Src is incompatible with this object.
      }
  end;


implementation

end.

