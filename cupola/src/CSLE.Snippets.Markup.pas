{
  This Source Code Form is subject to the terms of the Mozilla Public License,
  v. 2.0. If a copy of the MPL was not distributed with this file, You can
  obtain one at https://mozilla.org/MPL/2.0/

  Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).

  Data types that encapsulate different styles of text markup.
}

unit CSLE.Snippets.Markup;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils,
  CSLE.TextData;

type
  TSnippetMarkupKind = (
    Plain = 0,  // plain text: content must be UTF-8
    REML = 1,   // REML code: content must be UTF-8
    RTF  = 2    // RTF code: content must be ASCII
  );

  TSnippetMarkup = record
  strict private
    var
      fKind: TSnippetMarkupKind;
      fContent: TTextData;
      fExtra: UInt32;
  public

    ///  <summary>Constructs new record instance.</summary>
    ///  <param name="AText">[in] Markup content. Format must be valid for the
    ///  kind specified by the <c>AKind</c> parameter.</param>
    ///  <param name="AKind">[in] Type of markup.</param>
    ///  <param name="AExtra">[in] Optional extra information about the markup.
    ///  </param>
    ///  <remarks>See the remarks of the <c>Extra</c> property for details of
    ///  when <c>AExtra</c> is required.</remarks>
    constructor Create(const AText: string; const AKind: TSnippetMarkupKind;
      const AExtra: UInt32 = 0);

    ///  <summary>The type of markup.</summary>
    property Kind: TSnippetMarkupKind read fKind;

    ///  <summary>Any extra information about the markup.</summary>
    ///  <remarks><c>Extra</c> is only significant when <c>AKind</c> is
    ///  <c>TSnippetMarkupKind.REML</c>, in which case <c>AExtra</c> specifies
    ///  the REML version of the markup.</remarks>
    property Extra: UInt32 read fExtra;

    ///  <summary>Markup content. Must be in the correct format specified by
    ///  <c>Kind</c> and <c>Extra</c>.</summary>
    property Content: TTextData read fContent;

    // Initialisation, assignment & (in)equality operators
    class operator Initialize(out Dest: TSnippetMarkup);
    class operator Assign(var Dest: TSnippetMarkup;
      const [ref] Src: TSnippetMarkup);
    class operator Equal(const Left, Right: TSnippetMarkup): Boolean; inline;
    class operator NotEqual(const Left, Right: TSnippetMarkup): Boolean; inline;
  end;

implementation

{ TSnippetMarkup }

class operator TSnippetMarkup.Assign(var Dest: TSnippetMarkup;
  const [ref] Src: TSnippetMarkup);
begin
  Dest.fKind := Src.fKind;
  Dest.fContent := Src.fContent;
  Dest.fExtra := Src.fExtra;
end;

constructor TSnippetMarkup.Create(const AText: string;
  const AKind: TSnippetMarkupKind; const AExtra: UInt32);
begin
  fKind := AKind;
  fExtra := AExtra;
  case fKind of
    TSnippetMarkupKind.Plain, TSnippetMarkupKind.REML:
      fContent := TTextData.Create(AText, TTextDataType.UTF8);
    TSnippetMarkupKind.RTF:
      fContent := TTextData.Create(AText, TTextDataType.ASCII);
  end;
end;

class operator TSnippetMarkup.Equal(const Left, Right: TSnippetMarkup): Boolean;
begin
  Result := (Left.fKind = Right.fKind)
    and (Left.fExtra = Right.fExtra)
    and (Left.fContent = Right.fContent);
end;

class operator TSnippetMarkup.Initialize(out Dest: TSnippetMarkup);
begin
  // Can't assign directly to Dest - causes repeated assignment
  Dest.fContent := TTextData.Create('', TTextDataType.UTF8);
  Dest.fKind := TSnippetMarkupKind.Plain;
  Dest.fExtra := 0;
end;

class operator TSnippetMarkup.NotEqual(const Left,
  Right: TSnippetMarkup): Boolean;
begin
  Result := (Left.fKind <> Right.fKind)
    or (Left.fExtra <> Right.fExtra)
    or (Left.fContent <> Right.fContent);
end;

end.
