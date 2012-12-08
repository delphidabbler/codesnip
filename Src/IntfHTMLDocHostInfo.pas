{
 * IntfHTMLDocHostInfo.pas
 *
 * Interface that provide information about an HTML document contained in a
 * document host. Used by HTML generators and dynamic HTML manipulators to get
 * a reference to an HTML document.
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
 * The Original Code is IntfHTMLDocHostInfo.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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

