{
 * UPrintDocuments.pas
 *
 * Provides interface and classes that can generate output suitable for printing
 * using print engine.
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
 * The Original Code is UPrintDocuments.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UPrintDocuments;


interface


uses
  // Delphi
  Classes,
  // Project
  DB.UCategory, DB.USnippet, Hiliter.UGlobals, URTFUtils;


type
  ///  <summary>Interface supported by classes that can generate a print
  ///  document suitable for processing by the print engine.</summary>
  ///  <remarks>The print engine prints documents rendered in rich text format.
  ///  </remarks>
  IPrintDocument = interface(IInterface)
    ['{56E4CA97-7F04-427A-A95F-03CE55910DC0}']
    ///  <summary>Generates and returns print document.</summary>
    function Generate: TRTF;
  end;

type
  ///  <summary>Class that generates a print document that describes a snippet.
  ///  </summary>
  TSnippetPrintDocument = class(TInterfacedObject,
    IPrintDocument
  )
  strict private
    var
      ///  <summary>Reference to snippet described by print document.</summary>
      fSnippet: TSnippet;
    ///  <summary>Gets highlighter attributes required to render source code,
    ///  depending on printer properties.</summary>
    function GetHiliteAttrs: IHiliteAttrs;
  public
    ///  <summary>Constructs object to create print document for given snippet.
    ///  </summary>
    constructor Create(const Snippet: TSnippet);
    ///  <summary>Generates and returns print document.</summary>
    ///  <remarks>Method of IPrintDocument.</remarks>
    function Generate: TRTF;
  end;

type
  ///  <summary>Class that generates a print document that describes a category.
  ///  </summary>
  TCategoryPrintDocument = class(TInterfacedObject,
    IPrintDocument
  )
  strict private
    var
      ///  <summary>Reference to category described by print document.</summary>
      fCategory: TCategory;
  public
    ///  <summary>Constructs object to create print document for given category.
    ///  </summary>
    constructor Create(const Category: TCategory);
    ///  <summary>Generates and returns print document.</summary>
    ///  <remarks>Method of IPrintDocument.</remarks>
    function Generate: TRTF;
  end;

implementation


uses
  // Project
  Hiliter.UAttrs, URTFCategoryDoc, URTFSnippetDoc, UPrintInfo;


{ TSnippetPrintDocument }

constructor TSnippetPrintDocument.Create(const Snippet: TSnippet);
begin
  inherited Create;
  fSnippet := Snippet;
end;

function TSnippetPrintDocument.Generate: TRTF;
var
  Doc: TRTFSnippetDoc;  // object that renders snippet document in RTF
begin
  Doc := TRTFSnippetDoc.Create(
    GetHiliteAttrs, poUseColor in PrintInfo.PrintOptions
  );
  try
    Result := TRTF.Create(Doc.Generate(fSnippet));
  finally
    Doc.Free;
  end;
end;

function TSnippetPrintDocument.GetHiliteAttrs: IHiliteAttrs;
begin
  if fSnippet.HiliteSource then
    if not (poSyntaxPrint in PrintInfo.PrintOptions) then
      Result := THiliteAttrsFactory.CreatePrintAttrs(nil, False)
    else
      Result := THiliteAttrsFactory.CreatePrintAttrs(
        THiliteAttrsFactory.CreateUserAttrs,
        poUseColor in PrintInfo.PrintOptions
      )
  else
    Result := THiliteAttrsFactory.CreateNulAttrs;
end;

{ TCategoryPrintDocument }

constructor TCategoryPrintDocument.Create(const Category: TCategory);
begin
  inherited Create;
  fCategory := Category;
end;

function TCategoryPrintDocument.Generate: TRTF;
var
  Doc: TRTFCategoryDoc; // object that renders category document in RTF
begin
  Doc := TRTFCategoryDoc.Create(poUseColor in PrintInfo.PrintOptions);
  try
    Result := TRTF.Create(Doc.Generate(fCategory));
  finally
    Doc.Free;
  end;
end;

end.

