{
 * IntfCommon.pas
 *
 * Contains common general purpose interfaces.
 *
 * v0.1 of 05 May 2005  - Original version.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 *                      - Removed warnings about un-fixed interfaces - now
 *                        fixed.
 * v1.1 of 04 Nov 2007  - Changed parameter type of IAssignable.Assign from
 *                        IAssignable to IInterface.
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
 * The Original Code is IntfCommon.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2007 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
 *
 * ***** END LICENSE BLOCK *****
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

