{
 * UHTMLTemplate.pas
 *
 * Implements a class that loads a HTML template from resources and permits
 * replacing of placeholders with values.
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
 * The Original Code is UHTMLTemplate.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UHTMLTemplate;


interface


uses
  // Delphi
  Classes, Windows,
  // Project
  UConsts;


type

  {
  THTMLTemplate:
    Loads a HTML template from resources and permits replacing of placeholders
    in form <%PlaceHolder%> with values.
  }
  THTMLTemplate = class(TObject)
  strict private
    fHTML: string; // HTML code from template and updated by object's methods
  public
    constructor Create(const Inst: THandle; const ResName: string;
      ResType: PChar = RT_HTML);
      {Class constructor. Creates object that uses a given HTML template.
        @param Inst [in] Instance of module containing HTML template resource.
        @param ResName [in] Name of resource containing template.
        @param ResType [in] Type of resource containing template - assumes
          RT_HTML if parameter omitted.
      }
    procedure ResolvePlaceholderText(const Placeholder, Text: string);
      {Replaces all instances of a placeholder with plain text. The plain text
      is converted to valid HTML.
        @param Placeholder [in] Name of placeholder to be replaced. Should not
          include <% and %> placeholder delimiters.
        @param Text [in] Plain text to replace placeholder.
      }
    procedure ResolvePlaceholderHTML(const Placeholder, HTML: string);
      {Replaces all instances of a placeholder with given HTML. The HTML must be
      valid.
        @param Placeholder [in] Name of placeholder to be replaced. Should not
          include <% and %> placeholder delimiters.
        @param HTML [in] Valid HTML to replace placeholder.
      }
    property HTML: string
      read fHTML;
      {HTML loaded from resources and manipulated by object's Resolve***
      methods}
  end;


implementation


uses
  // Project
  UEncodings, UHTMLUtils, UResourceUtils, UStrUtils;


{ THTMLTemplate }

constructor THTMLTemplate.Create(const Inst: THandle; const ResName: string;
  ResType: PChar);
  {Class constructor. Creates object that uses a given HTML template.
    @param Inst [in] Instance of module containing HTML template resource.
    @param ResName [in] Name of resource containing template.
    @param ResType [in] Type of resource containing template - assumes RT_HTML
      if parameter omitted.
  }
begin
  inherited Create;
  fHTML := LoadResourceAsString(Inst, ResName, ResType, etWindows1252);
end;

procedure THTMLTemplate.ResolvePlaceholderHTML(const Placeholder, HTML: string);
  {Replaces all instances of a placeholder with given HTML. The HTML must be
  valid.
    @param Placeholder [in] Name of placeholder to be replaced. Should not
      include <% and %> placeholder delimiters.
    @param HTML [in] Valid HTML to replace placeholder.
  }
begin
  fHTML := StrReplace(fHTML, '<%' + Placeholder + '%>', HTML);
end;

procedure THTMLTemplate.ResolvePlaceholderText(const Placeholder, Text: string);
  {Replaces all instances of a placeholder with plain text. The plain text is
  converted to valid HTML.
    @param Placeholder [in] Name of placeholder to be replaced. Should not
      include <% and %> placeholder delimiters.
    @param Text [in] Plain text to replace placeholder.
  }
begin
  ResolvePlaceholderHTML(Placeholder, MakeSafeHTMLText(Text));
end;

end.

