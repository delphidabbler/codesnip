unit Test.IniData;

interface

uses
  DUnitX.TestFramework,

  System.SysUtils,
  System.Classes,
  System.Generics.Defaults,
  System.Generics.Collections,

  CSLE.IniData;

type
  [TestFixture]
  TTestIniData = class
  strict private
    var
      Ini: TIniData;
    type
      TIniSectionInfo = record
        Name: string;
        KVCount: Integer;
      end;
      TIniKVPairInfo = record
        Section: string;
        Key: string;
        Value: string;
      end;
    const
      IniData1 =
        '''
        ; Comment

        [   Top section  ]
        Alice = FOO
        Bob = "   BAR with spaces "
        Charlie = "" quoted string""
        [empty]

        [©]
        Date=2024-10-17 23:12:00

              ;
              ; Indented comment
        [$last_section$]

        ;Ignore=This value
          Alice    = Baz is here
        MissingValue    =
        question? = 42
        the_answer=56
        quoted_num="666"
        ''';
      IniData1Sections: array[0..3] of TIniSectionInfo = (
        (Name: 'Top Section'; KVCount: 2),
        (Name: 'empty'; KVCount: 0),
        (Name: '©'; KVCount: 1),
        (Name: '$last_section$'; KVCount: 4)
      );
      IniData1KVInfo: array[0..8] of TIniKVPairInfo = (
        (Section: 'Top Section'; Key: 'Alice'; Value: 'FOO'),
        (Section: 'Top Section'; Key: 'Bob'; Value: '   BAR with spaces '),
        (Section: 'Top Section'; Key: 'Charlie'; Value: '" quoted string"'),
        (Section: '©'; Key: 'Date'; Value: '2024-10-17 23:12:00'),
        (Section: '$last_section$'; Key: 'Alice'; Value: 'Baz is here'),
        (Section: '$last_section$'; Key: 'MissingValue'; Value: ''),
        (Section: '$last_section$'; Key: 'question?'; Value: '42'),
        (Section: '$last_section$'; Key: 'the_answer'; Value: '56'),
        (Section: '$last_section$'; Key: 'quoted_num'; Value: '666')
      );
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    [TestCase('Empty',',False')]
    [TestCase('Spaces only','    ,False')]
    [TestCase('Surrounded by spaces', '  Foo  ,False')]
    [TestCase('Contains ctrl chars 1', 'Foo'+#8+'Bar,False')]
    [TestCase('Contains ctrl chars 2', #127+'Bar,False')]
    [TestCase('Valid', 'Valid name †№,True')]
    [TestCase('Starts with ;', ';Foo,True')]
    procedure IsValidSectionName_returns_expected_value(const ASection: string; const Expected: Boolean);

    [Test]
    [TestCase('Empty',',False')]
    [TestCase('Spaces only','    ,False')]
    [TestCase('Surrounded by spaces', '  Foo  ,False')]
    [TestCase('Contains ctrl chars 1', 'Foo'+#8+'Bar,False')]
    [TestCase('Contains ctrl chars 2', #127+'Bar,False')]
    [TestCase('Valid', 'Valid name †№,True')]
    [TestCase('Starts with ;', ';Foo,False')]
    procedure IsValidKeyName_returns_expected_value(const AKey: string; const Expected: Boolean);

    [Test]
    [TestCase('Read existing section/key','©,Date,2024-10-17 23:12:00')]
    [TestCase('Read quoted quoted string','Top section,Charlie," quoted string"')]
    [TestCase('Read quoted whitespace string','Top section,Bob,   BAR with spaces ')]
    procedure ReadString_returns_correct_values_for_existing_sections_and_keys(const ASection, AKey, AExpectedValue: string);
    [TestCase('Read existing section, bad key','Top section,Betty,Boop')]
    [TestCase('Read bad section & key','Popeye,The Sailor,man')]
    [TestCase('Read empty section','empty,unknown,')]   // default is empty string
    [TestCase('Read existing section/key, empty value','$last_section$,MissingValue,Found it!')]
    procedure ReadString_returns_default_values_for_bad_sections_or_keys(const ASection, AKey, ADefaultValue: string);

    [Test]
    [TestCase('#1','$last_section$,question?,42')]
    [TestCase('#2','$last_section$,the_answer,56')]
    [TestCase('#3 (quoted)','$last_section$,quoted_num,666')]
    procedure ReadInteger_returns_correct_values_for_existing_sections_and_keys(const ASection, AKey: string; AExpectedValue: Integer);

    [Test]
    [TestCase('Read existing section, bad key','Top section,Betty,0')]
    [TestCase('Read bad section & key','Popeye,The Sailor,1')]
    [TestCase('Read empty section','empty,unknown,2')]
    [TestCase('Read existing section/key, empty value','$last_section$,MissingValue,3')]
    procedure ReadInteger_returns_default_values_for_bad_sections_or_keys(const ASection, AKey: string; ADefaultValue: Integer);

    // LoadFromString tests also test ReadString, GetSectionNames and
    // GetSectionKeys
    [Test]
    procedure LoadFromString_loads_section_names_correctly;
    [Test]
    procedure LoadFromString_loads_section_keys_and_values_correctly;
    [Test]
    procedure LoadFromString_loads_empty_string_correctly;
    [Test]
    procedure LoadFromString_loads_string_with_only_blank_lines_and_comments_correctly;
    [Test]
    procedure LoadFromString_fails_when_KV_pairs_precede_1st_section;
    [Test]
    procedure LoadFromString_fails_on_invalid_section_id;
    [Test]
    procedure LoadFromString_fails_on_white_space_section_id;
    [Test]
    procedure LoadFromString_fails_on_section_id_containing_ctrl_chars;
    [Test]
    procedure LoadFromString_fails_on_empty_section_id;
    [Test]
    procedure LoadFromString_fails_on_empty_key_name;
    [Test]
    procedure LoadFromString_fails_on_white_space_key_name;
    [Test]
    procedure LoadFromString_fails_on_key_name_containing_ctrl_chars;

    [Test]
    procedure IsEmpty_returns_true_for_newly_created_object;
    [Test]
    procedure IsEmpty_returns_true_for_loaded_non_empty_ini_data;

  end;

implementation

procedure TTestIniData.IsEmpty_returns_true_for_loaded_non_empty_ini_data;
begin
  Ini.LoadFromString(IniData1);
  Assert.IsFalse(Ini.IsEmpty);
end;

procedure TTestIniData.IsEmpty_returns_true_for_newly_created_object;
begin
  Assert.IsTrue(Ini.IsEmpty);
end;

procedure TTestIniData.IsValidKeyName_returns_expected_value(
  const AKey: string; const Expected: Boolean);
begin
  Assert.AreEqual(Expected, TIniData.IsValidKeyName(AKey), AKey);
end;

procedure TTestIniData.IsValidSectionName_returns_expected_value(
  const ASection: string; const Expected: Boolean);
begin
  Assert.AreEqual(Expected, TIniData.IsValidSectionName(ASection), ASection);
end;

procedure TTestIniData.LoadFromString_fails_on_empty_key_name;
begin
  const EmptyKeyName =
    '''
    [section]
    =Value
    ''';
  Assert.WillRaise(
    procedure
    begin
      Ini.LoadFromString(EmptyKeyName);
    end,
    EIniData
  );
end;

procedure TTestIniData.LoadFromString_fails_on_empty_section_id;
begin
  const EmptySectionID =
    '''
    []
    Key=Value
    ''';
  Assert.WillRaise(
    procedure
    begin
      Ini.LoadFromString(EmptySectionID);
    end,
    EIniData
  );
end;

procedure TTestIniData.LoadFromString_fails_on_invalid_section_id;
begin
  const BadIniData =
    '''
    [  Incomplete-section-name
    Alice=42
    Bob=56
    ''';
  Assert.WillRaise(
    procedure
    begin
      Ini.LoadFromString(BadIniData);
    end,
    EIniData
  );
end;

procedure TTestIniData.LoadFromString_fails_on_key_name_containing_ctrl_chars;
begin
  const CtrlCharKeyName =
    '''
    [section]
    Foo%sBar=Value
    ''';
  var Fmt := Format(CtrlCharKeyName, [#127]);
  Assert.WillRaise(
    procedure
    begin
      Ini.LoadFromString(Fmt);
    end,
    EIniData
  );
end;

procedure TTestIniData.LoadFromString_fails_on_section_id_containing_ctrl_chars;
begin
  const CtrlCharSectionID =
    '''
    [Foo%sBar]
    Key=Value
    ''';
  var Fmt := Format(CtrlCharSectionID, [#127]);
  Assert.WillRaise(
    procedure
    begin
      Ini.LoadFromString(Fmt);
    end,
    EIniData
  );
end;

procedure TTestIniData.LoadFromString_fails_on_white_space_key_name;
begin
  const WhitespaceKeyName =
    '''
    [section]
         =Value
    ''';
  Assert.WillRaise(
    procedure
    begin
      Ini.LoadFromString(WhitespaceKeyName);
    end,
    EIniData
  );
end;

procedure TTestIniData.LoadFromString_fails_on_white_space_section_id;
begin
  const WhitespaceSectionID =
    '''
    [    ]
    Key=Value
    ''';
  Assert.WillRaise(
    procedure
    begin
      Ini.LoadFromString(WhitespaceSectionID);
    end,
    EIniData
  );
end;

procedure TTestIniData.LoadFromString_fails_when_KV_pairs_precede_1st_section;
begin
  const BadIniData =
    '''
    Foo=42
    Bar=56
    [Alice]
    A=1
    B=2
    ''';
  Assert.WillRaise(
    procedure
    begin
      Ini.LoadFromString(BadIniData);
    end,
    EIniData
  );
end;

procedure TTestIniData.LoadFromString_loads_empty_string_correctly;
begin
  Ini.LoadFromString(string.Empty);
  var Keys := Ini.GetSectionNames;
  Assert.AreEqual(NativeInt(0), Length(Keys));
end;

procedure TTestIniData.LoadFromString_loads_section_keys_and_values_correctly;
begin
  Ini.LoadFromString(IniData1);
  var SLG := TStringList.Create;
  var SLE: TStringList := nil;
  try
    SLE := TStringList.Create;
    for var Section in Ini.GetSectionNames do
    begin
      var KA := Ini.GetSectionKeys(Section);
      for var Key in KA do
        SLG.Add(Section + '|' + Key + '|' + Ini.ReadString(Section, Key, ''));
    end;
    for var I in IniData1KVInfo do
      SLE.Add(I.Section + '|' + I.Key + '|' + I.Value);
    SLG.Sort;
    SLE.Sort;
    Assert.AreEqual(SLE.Text, SLG.Text);
  finally
    SLE.Free;
    SLG.Free;
  end;
end;

procedure TTestIniData.LoadFromString_loads_section_names_correctly;
begin
  Ini.LoadFromString(IniData1);
  var SA := Ini.GetSectionNames;
  var SLG := TStringList.Create;
  var SLE: TStringList := nil;
  try
    SLE := TStringList.Create;
    for var SN in SA do
      SLG.Add(SN);
    for var SN in IniData1Sections do
      SLE.Add(SN.Name);
    SLG.Sort;
    SLE.Sort;
    Assert.AreEqual(SLE.Text, SLG.Text);
  finally
    SLE.Free;
    SLG.Free;
  end;
end;

procedure TTestIniData.LoadFromString_loads_string_with_only_blank_lines_and_comments_correctly;
begin
  const Blank =
    '''


    ; Comment 1

            ; Comment 2


    ''';
  Ini.LoadFromString(Blank);
  var Keys := Ini.GetSectionNames;
  Assert.AreEqual(NativeInt(0), Length(Keys));
end;

procedure TTestIniData.ReadInteger_returns_correct_values_for_existing_sections_and_keys(
  const ASection, AKey: string; AExpectedValue: Integer);
begin
  Ini.LoadFromString(IniData1);
  var Value := Ini.ReadInteger(ASection, AKey, 999);
  Assert.AreEqual(AExpectedValue, Value);
end;

procedure TTestIniData.ReadInteger_returns_default_values_for_bad_sections_or_keys(
  const ASection, AKey: string; ADefaultValue: Integer);
begin
  Ini.LoadFromString(IniData1);
  var Value := Ini.ReadInteger(ASection, AKey, ADefaultValue);
  Assert.AreEqual(ADefaultValue, Value);
end;

procedure TTestIniData.ReadString_returns_correct_values_for_existing_sections_and_keys(
  const ASection, AKey, AExpectedValue: string);
begin
  Ini.LoadFromString(IniData1);
  var Value := Ini.ReadString(ASection, AKey, 'default value');
  Assert.AreEqual(AExpectedValue, Value);
end;

procedure TTestIniData.ReadString_returns_default_values_for_bad_sections_or_keys(
  const ASection, AKey, ADefaultValue: string);
begin
  Ini.LoadFromString(IniData1);
  var Value := Ini.ReadString(ASection, AKey, ADefaultValue);
  Assert.AreEqual(ADefaultValue, Value);
end;

procedure TTestIniData.Setup;
begin
  Ini := TIniData.Create;
end;

procedure TTestIniData.TearDown;
begin
  Ini.Free;
end;

initialization

  TDUnitX.RegisterTestFixture(TTestIniData);

end.
