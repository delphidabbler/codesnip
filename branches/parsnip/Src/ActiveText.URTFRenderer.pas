{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Provides an object that can be passed to IActiveText.Render to render active
 * text in rich text format.
}


unit ActiveText.URTFRenderer;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  ActiveText.UMain, UBaseObjects, URTFBuilder, URTFStyles;


type
  TActiveTextRTFStyleMap = class(TObject)
  strict private
    var
      fMap: TDictionary<TActiveTextActionElemKind,TRTFStyle>;
    function GetStyle(ElemKind: TActiveTextActionElemKind): TRTFStyle;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const ElemKind: TActiveTextActionElemKind;
      const Style: TRTFStyle);
    procedure Assign(const Src: TActiveTextRTFStyleMap);
    procedure MakeMonochrome;
    // enumerator enumerates styles, not pair or action element kind
    function GetEnumerator: TEnumerator<TRTFStyle>;
    property Styles[ElemKInd: TActiveTextActionElemKind]: TRTFStyle
      read GetStyle; default;
  end;

type
  TActiveTextRTF = class(TInterfacedObject, IActiveTextRenderer)
  public
    type
      TOptions = class(TObject)
      strict private
        var
          fElemStyleMap: TActiveTextRTFStyleMap;
          fDisplayURLs: Boolean;
          fIgnoreInterBlockText: Boolean;
          fURLStyle: TRTFStyle;
        procedure SetElemStyleMap(const ElemStyleMap: TActiveTextRTFStyleMap);
      public
        constructor Create;
        destructor Destroy; override;
        property ElemStyleMap: TActiveTextRTFStyleMap
          read fElemStyleMap write SetElemStyleMap;
        property DisplayURLs: Boolean read fDisplayURLs write fDisplayURLs
          default False;
        property IgnoreInterBlockText: Boolean read fIgnoreInterBlockText
          write fIgnoreInterBlockText default True;
        property URLStyle: TRTFStyle read fURLStyle write fURLStyle;
      end;
  strict private
    var
      fInBlock: Boolean;
      fOptions: TOptions;
      fBuilder: TRTFBuilder;
    function SkipOutput: Boolean;
  public
    constructor Create(const Builder: TRTFBuilder);
    destructor Destroy; override;
    property Options: TOptions read fOptions;

    ///  <summary>Called before active text is processed.</summary>
    ///  <remarks>Method of IActiveTextRenderer.</remarks>
    procedure Initialise;

    ///  <summary>Called after last active text element has been processed.
    ///  </summary>
    ///  <remarks>Method of IActiveTextRenderer.</remarks>
    procedure Finalise;

    ///  <summary>Called when plain text should be output.</summary>
    ///  <param name="AText">string. Text to be output.</param>
    ///  <remarks>Method of IActiveTextRenderer.</remarks>
    procedure OutputText(const AText: string);

    ///  <summary>Called at the start of a new block of active text.</summary>
    ///  <param name="Kind">TActiveTextActionElemKind [in] Kind of block being
    ///  opened. This will be an active text element with DisplayStyle =
    ///  dsBlock.</param>
    ///  <remarks>Method of IActiveTextRenderer.</remarks>
    procedure BeginBlock(const Kind: TActiveTextActionElemKind);

    ///  <summary>Called when the current active text block ends.</summary>
    ///  <param name="Kind">TActiveTextActionElemKind [in] Kind of block being
    ///  opened. This will be an active text element with DisplayStyle =
    ///  dsBlock.</param>
    ///  <remarks>Method of IActiveTextRenderer.</remarks>
    procedure EndBlock(const Kind: TActiveTextActionElemKind);

    ///  <summary>Called at the start of a new active text styling element.
    ///  </summary>
    ///  <param name="Kind">TActiveTextActionElemKind [in] Kind of styling
    ///  element. The kind indicates the type of styling to be applied. This
    ///  will be an active text element with DisplayStyle = dsInline that is
    ///  not ekLink.</param>
    ///  <remarks>Method of IActiveTextRenderer.</remarks>
    procedure BeginInlineStyle(const Kind: TActiveTextActionElemKind);

    ///  <summary>Called the current active text styling element ends.</summary>
    ///  <param name="Kind">TActiveTextActionElemKind [in] Kind of styling
    ///  element. The kind indicates the type of styling to be applied. This
    ///  will be an active text element with DisplayStyle = dsInline that is
    ///  not ekLink.</param>
    ///  <remarks>Method of IActiveTextRenderer.</remarks>
    procedure EndInlineStyle(const Kind: TActiveTextActionElemKind);

    ///  <summary>Called at the start of a new active text link element.
    ///  </summary>
    ///  <param name="URL">string. URL to accessed from the link.</param>
    ///  <remarks>Method of IActiveTextRenderer.</remarks>
    procedure BeginLink(const URL: string);

    ///  <summary>Called when the current active text link element ends.
    ///  </summary>
    ///  <param name="URL">string. URL to accessed from the link.</param>
    ///  <remarks>Method of IActiveTextRenderer.</remarks>
    procedure EndLink(const URL: string);
  end;


implementation


uses
  // Project
  SysUtils, Generics.Defaults;


{ TActiveTextRTFStyleMap }

procedure TActiveTextRTFStyleMap.Add(const ElemKind: TActiveTextActionElemKind;
  const Style: TRTFStyle);
begin
  Assert(not fMap.ContainsKey(ElemKind),
    ClassName + '.Add: ElemKind already in map');
  fMap.Add(ElemKind, Style);
end;

procedure TActiveTextRTFStyleMap.Assign(const Src: TActiveTextRTFStyleMap);
var
  Entry: TPair<TActiveTextActionElemKind, TRTFStyle>;
begin
  fMap.Clear;
  for Entry in Src.fMap do
    fMap.Add(Entry.Key, Entry.Value);
end;

constructor TActiveTextRTFStyleMap.Create;
begin
  inherited Create;
  fMap := TDictionary<TActiveTextActionElemKind,TRTFStyle>.Create(
    TDelegatedEqualityComparer<TActiveTextActionElemKind>.Create(
      function (const Left, Right: TActiveTextActionElemKind): Boolean
      begin
        Result := Left = Right;
      end,
      function (const ElemKind: TActiveTextActionElemKind): Integer
      begin
        Result := Ord(ElemKind);
      end
    )
  )
end;

destructor TActiveTextRTFStyleMap.Destroy;
begin
  fMap.Free;
  inherited;
end;

function TActiveTextRTFStyleMap.GetEnumerator: TEnumerator<TRTFStyle>;
begin
  Result := fMap.Values.GetEnumerator;
end;

function TActiveTextRTFStyleMap.GetStyle(ElemKind: TActiveTextActionElemKind):
  TRTFStyle;
begin
  if fMap.ContainsKey(ElemKind) then
    Result := fMap[ElemKind]
  else
    Result := TRTFStyle.CreateNull;
end;

procedure TActiveTextRTFStyleMap.MakeMonochrome;
var
  Style: TRTFStyle;
  Kind: TActiveTextActionElemKind;
begin
  for Kind := Low(TActiveTextActionElemKind)
    to High(TActiveTextActionElemKind) do
  begin
    if fMap.ContainsKey(Kind) then
    begin
      Style := fMap[Kind];
      Style.MakeMonochrome;
      fMap[Kind] := Style;
    end;
  end;
end;

{ TActiveTextRTF }

procedure TActiveTextRTF.BeginBlock(const Kind: TActiveTextActionElemKind);
begin
  fInBlock := True;
  fBuilder.BeginGroup;
  fBuilder.ApplyStyle(fOptions.ElemStyleMap[Kind]);
end;

procedure TActiveTextRTF.BeginInlineStyle(
  const Kind: TActiveTextActionElemKind);
begin
  if SkipOutput then
    Exit;
  fBuilder.BeginGroup;
  fBuilder.ApplyStyle(fOptions.ElemStyleMap[Kind]);
end;

procedure TActiveTextRTF.BeginLink(const URL: string);
begin
  // do nothing
end;

constructor TActiveTextRTF.Create(const Builder: TRTFBuilder);
begin
  Assert(not Assigned(fBuilder), ClassName + '.Create: Builder is nil');
  inherited Create;
  fBuilder := Builder;
  fOptions := TOptions.Create;
end;

destructor TActiveTextRTF.Destroy;
begin
  fOptions.Free;
  inherited;
end;

procedure TActiveTextRTF.EndBlock(const Kind: TActiveTextActionElemKind);
begin
  fBuilder.EndPara;
  fBuilder.EndGroup;
  fInBlock := False;
end;

procedure TActiveTextRTF.EndInlineStyle(const Kind: TActiveTextActionElemKind);
begin
  if SkipOutput then
    Exit;
  fBuilder.EndGroup;
end;

procedure TActiveTextRTF.EndLink(const URL: string);
resourcestring
  sURL = ' (%s)';                     // formatting for URLs from hyperlinks
begin
  if SkipOutput then
    Exit;
  fBuilder.BeginGroup;
  fBuilder.ApplyStyle(fOptions.URLStyle);
  fBuilder.AddText(Format(sURL, [URL]));
  fBuilder.EndGroup;
end;

procedure TActiveTextRTF.Finalise;
begin
  // do nothing
end;

procedure TActiveTextRTF.Initialise;
var
  Style: TRTFStyle;
begin
  fInBlock := False;
  for Style in fOptions.ElemStyleMap do
  begin
    fBuilder.FontTable.AddFromStyle(Style);
    fBuilder.ColourTable.AddFromStyle(Style);
  end;
  fBuilder.FontTable.AddFromStyle(fOptions.URLStyle);
  fBuilder.ColourTable.AddFromStyle(fOptions.URLStyle);
end;

procedure TActiveTextRTF.OutputText(const AText: string);
begin
  if SkipOutput then
    Exit;
  fBuilder.AddText(AText);
end;

function TActiveTextRTF.SkipOutput: Boolean;
begin
  Result := not fInBlock and fOptions.IgnoreInterBlockText;
end;

{ TActiveTextRTF.TOptions }

constructor TActiveTextRTF.TOptions.Create;
begin
  inherited Create;
  fElemStyleMap := TActiveTextRTFStyleMap.Create;
  fDisplayURLs := False;
  fIgnoreInterBlockText := True;
  fURLStyle := TRTFStyle.CreateNull;
end;

destructor TActiveTextRTF.TOptions.Destroy;
begin
  fElemStyleMap.Free;
  inherited;
end;

procedure TActiveTextRTF.TOptions.SetElemStyleMap(
  const ElemStyleMap: TActiveTextRTFStyleMap);
begin
  fElemStyleMap.Assign(ElemStyleMap);
end;

end.

