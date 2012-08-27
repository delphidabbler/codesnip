{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Declares an interface that provides information about an HTML document
 * contained in a document host.
 *
 * Used by HTML generators and dynamic HTML manipulators to get a reference to
 * an HTML document.
}


unit IntfHTMLDocHostInfo;


interface


type

  {
  IDHTMLHostInfo:
    Interface that provides information about an HTML document contained in a
    document host.
  }
  IHTMLDocHostInfo = interface(IInterface)
    ['{84C861A9-C752-4D24-95F3-970252EF3F88}']
    function HTMLDocument: IDispatch;
      {Gets reference to IDispatch interface of HTML document that HTML host
      contains.
        @return Document's IDispatch interface.
      }
  end;


implementation

end.

