{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that loads a HTML template from resources and permits
 * replacing of placeholders with values.
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
  ResolvePlaceholderHTML(Placeholder, THTML.Entities(Text));
end;

end.

