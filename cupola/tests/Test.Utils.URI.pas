{
  This unit is dedicated to public domain under the CC0 license.
  See https://creativecommons.org/public-domain/cc0/
}

unit Test.Utils.URI;

interface

uses
  DUnitX.TestFramework,

  System.SysUtils,
  System.Net.URLClient,

  CSLE.Utils.URI;

type
  [TestFixture]
  TTestTImmutableURI = class
  private
    const
      SimpleURI = 'https://www.example.com';
      SimpleURIEncoded = SimpleURI + '/';
      SimpleURIScheme = 'https';
      SimpleURIUsername = '';
      SimpleURIPassword = '';
      SimpleURIHost = 'www.example.com';
      SimpleURIPort = '443';    // default port for https
      SimpleURIPath = '/';
      SimpleURIQuery = '';
      SimpleURIFragment = '';

      ComplexURI = 'https://username:password@example.com:9876/index?question=84×½&answer=56&Σ=98#part-2';
      ComplexURIEncoded = 'https://username:password@example.com:9876/index?question=84%C3%97%C2%BD&answer=56&%CE%A3=98#part-2';
      ComplexURIScheme = 'https';
      ComplexURIUsername = 'username';
      ComplexURIPassword = 'password';
      ComplexURIHost = 'example.com';
      ComplexURIPort = '9876';
      ComplexURIPath = '/index';
      ComplexURIQuery = 'question=84%C3%97%C2%BD&answer=56&%CE%A3=98';
      ComplexURIFragment = 'part-2';

      RFCEg1 = 'ftp://ftp.is.co.za/rfc/rfc1808.txt';
      RFCEg1Encoded = RFCEg1;
      RFCEg1Scheme = 'ftp';
      RFCEg1Port = '-1';  // -1 => no port & no default port known
      RFCEg1Host = 'ftp.is.co.za';
      RFCEg1Path = '/rfc/rfc1808.txt';
      RFCEg1Query = '';
      RFCEg1Fragment = '';

      RFCEg2 = 'http://www.ietf.org/rfc/rfc2396.txt';
      RFCEg2Encoded = RFCEg2;
      RFCEg2Scheme = 'http';
      RFCEg2Host = 'www.ietf.org';
      RFCEg2Port = '80';  // default port for http
      RFCEg2Path = '/rfc/rfc2396.txt';
      RFCEg2Query = '';
      RFCEg2Fragment = '';

      RFCEg3 = 'ldap://[2001:db8::7]/c=GB?objectClass?one';
      RFCEg3Encoded = RFCEg3;
      RFCEg3Scheme = 'ldap';
      RFCEg3Username = '';
      RFCEg3Password = '';
      RFCEg3Host = '[2001:db8::7]';
      RFCEg3Port = '-1';  // -1 => no port & no default port known
      RFCEg3Path = '/c=GB';
      RFCEg3Query = 'objectClass?one';
      RFCEg3Fragment = '';

      RFCEg4 = 'mailto:John.Doe@example.com';
      RFCEg4Encoded = RFCEg4;
      RFCEg4Scheme = 'mailto';
      RFCEg4Username = 'John.Doe';
      RFCEg4Host = 'example.com';
      RFCEg4Port = '-1';  // -1 => no port & no default port known
      RFCEg4Path = '';
      RFCEg4Query = '';
      RFCEg4Fragment = '';

      RFCEg5 = 'news:comp.infosystems.www.servers.unix';
      RFCEg5Encoded = RFCEg5;
      RFCEg5Scheme = 'news';
      RFCEg5Host = '';    // no authority part, since no '//' following 'news:'
      RFCEg5Port = '-1';  // -1 => no port & no default port known
      RFCEg5Path = 'comp.infosystems.www.servers.unix';
      RFCEg5Query = '';
      RFCEg5Fragment = '';

      RFCEg6 = 'tel:+1-816-555-1212';
      RFCEg6Encoded = RFCEg6;
      RFCEg6Scheme = 'tel';
      RFCEg6Host ='';     // no authority part, since no '//' following 'tel:'
      RFCEg6Port = '-1';  // -1 => no port & no default port known
      RFCEg6Path = '+1-816-555-1212';
      RFCEg6Query = '';
      RFCEg6Fragment = '';

      RFCEg7 = 'telnet://192.0.2.16:80/';
      RFCEg7Encoded = RFCEg7;
      RFCEg7Scheme = 'telnet';
      RFCEg7Host = '192.0.2.16';
      RFCEg7Port = '80';
      RFCEg7Path = '/';
      RFCEg7Query = '';
      RFCEg7Fragment = '';

      RFCEg8 = 'urn:oasis:names:specification:docbook:dtd:xml:4.1.2';
      RFCEg8Encoded = RFCEg8;
      RFCEg8Scheme = 'urn';
      RFCEg8Host = '';    // no authority part, since no '//' following 'urn:'
      RFCEg8Port = '-1';  // -1 => no port & no default port known
      RFCEg8Path = 'oasis:names:specification:docbook:dtd:xml:4.1.2';
      RFCEg8Query = '';
      RFCEg8Fragment = '';

      RFCEg9 = 'foo://example.com:8042/over/there?name=ferret#nose';
      RFCEg9Encoded = RFCEg9;
      RFCEg9Scheme = 'foo';
      RFCEg9Host = 'example.com';
      RFCEg9Port = '8042';
      RFCEg9Path = '/over/there';
      RFCEg9Query = 'name=ferret';
      RFCEg9Fragment = 'nose';

      // Source of file URI format info:
      // https://en.wikipedia.org/wiki/File_URI_scheme
      // Per wikipedia, the form file:/path/to/file is acceptable and is
      // equivalent to file:///path/to/file, but the Delphi RTL code that
      // TImmutableURI depends upon does not support this format
      FileURI2 = 'file://localhost/path/to/file';
      FileURI2Encoded = FileURI2;
      FileURI2Scheme = 'file';
      FileURI2Host = 'localhost';
      FileURI2Port = '-1';
      FileURI2Path = '/path/to/file';
      FileURI2Query = '';
      FileURI2Fragment = '';

      FileURI3 = 'file:///path/to/file';
      FileURI3Encoded = FileURI3;
      FileURI3Scheme = 'file';
      FileURI3Host = '';
      FileURI3Port = '-1';
      FileURI3Path = '/path/to/file';
      FileURI3Query = '';
      FileURI3Fragment = '';

      MailtoURI = 'mailto:Fred:hidden@www.example.com';
      MailtoURIEncoded = MailtoURI;
      MailtoURIUsername = 'Fred';
      MailtoURIPassword = 'hidden';
      MailtoURIHost = 'www.example.com';
      MailtoURIPort = '-1'; // -1 => no port & no default port known
      MailtoURIPath = '';
      MailtoURIQuery = '';
      MailtoURIFragment = '';

      HttpURI = 'http://example.com/#§temp';
      HttpURIEncoded = 'http://example.com/#%C2%A7temp';
      HttpURIPath = '/';
      HttpURIQuery = '';
      HttpURIFragment = '%C2%A7temp';

      BadURI = 'example.com/';
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    [TestCase('SimpleURI',SimpleURI)]
    [TestCase('ComplexURI',ComplexURI)]
    [TestCase('RFCEg1', RFCEg1)]
    [TestCase('RFCEg2', RFCEg2)]
    [TestCase('RFCEg3', RFCEg3)]
    [TestCase('RFCEg4', RFCEg4)]
    [TestCase('RFCEg5', RFCEg5)]
    [TestCase('RFCEg6', RFCEg6)]
    [TestCase('RFCEg7', RFCEg7)]
    [TestCase('RFCEg8', RFCEg8)]
    [TestCase('FileURI2', FileURI2)]
    [TestCase('FileURI3', FileURI3)]
    [TestCase('Empty URI', string.Empty)]
    procedure ctor_succeeds_on_valid_and_empty_uri_strings_when_empty_strings_permitted(const AURIStr: string);
    [Test]
    [TestCase('SimpleURI',SimpleURI)]
    [TestCase('ComplexURI',ComplexURI)]
    [TestCase('RFCEg1', RFCEg1)]
    [TestCase('RFCEg2', RFCEg2)]
    [TestCase('RFCEg3', RFCEg3)]
    [TestCase('RFCEg4', RFCEg4)]
    [TestCase('RFCEg5', RFCEg5)]
    [TestCase('RFCEg6', RFCEg6)]
    [TestCase('RFCEg7', RFCEg7)]
    [TestCase('RFCEg8', RFCEg8)]
    [TestCase('FileURI2', FileURI2)]
    [TestCase('FileURI3', FileURI3)]
    procedure ctor_succeeds_on_valid_uri_strings_when_empty_strings_not_permitted(const AURIStr: string);
    [Test]
    procedure ctor_raises_exception_for_bad_uri;
    [Test]
    procedure ctor_raises_exception_for_empty_string_when_not_permitted;

    [Test]
    [TestCase('Empty URI - empty not permitted',string.Empty+',False,False')]
    [TestCase('Empty URI - empty permitted',string.Empty+',True,True')]
    [TestCase('Valid URI - empty not permitted',ComplexURI+',False,True')]
    [TestCase('Valid URI - empty permitted',RFCeg7+',True,True')]
    [TestCase('Bad URI - empty not permitted',BadURI+',False,False')]
    [TestCase('Bad URI - empty permitted',BadURI+',True,False')]
    procedure IsValidURIString_returns_expected_result_depending_whether_empty_strings_permitted(const AURIStr: string; const APermitEmpty, Expected: Boolean);

    [Test]
    [TestCase('IsEmpty => False', 'https://example.com,False')]
    [TestCase('IsEmpty => True',',True')]
    procedure IsEmpty_returns_expected_value_for_empty_and_non_empty_uris(AURIString: string; Expected: Boolean);

    [Test]
    [TestCase('SimpleURI',SimpleURI+','+SimpleURIEncoded)]
    [TestCase('ComplexURI',ComplexURI+','+ComplexURIEncoded)]
    [TestCase('RFCEg1',RFCEg1+','+RFCEg1Encoded)]
    [TestCase('RFCEg2',RFCEg2+','+RFCEg2Encoded)]
    [TestCase('RFCEg3',RFCEg3+','+RFCEg3Encoded)]
    [TestCase('RFCEg4',RFCEg4+','+RFCEg4Encoded)]
    [TestCase('RFCEg5',RFCEg5+','+RFCEg5Encoded)]
    [TestCase('RFCEg6',RFCEg6+','+RFCEg6Encoded)]
    [TestCase('RFCEg7',RFCEg7+','+RFCEg7Encoded)]
    [TestCase('RFCEg8',RFCEg8+','+RFCEg8Encoded)]
    [TestCase('RFCEg9',RFCEg9+','+RFCEg9Encoded)]
    [TestCase('FileURI2', FileURI2+','+FileURI2Encoded)]
    [TestCase('FileURI3', FileURI3+','+FileURI3Encoded)]
    [TestCase('MailtoURI',MailtoURI+','+MailtoURIEncoded)]
    [TestCase('HttpURI',HttpURI+','+HttpURIEncoded)]
    procedure ToString_returns_URI_unchanged(const AURIStr: string; const Expected: string);
    [Test]
    procedure ToString_returns_empty_string_for_empty_uri;

    [Test]
    [TestCase('SimpleURI',SimpleURI+','+SimpleURIScheme)]
    [TestCase('ComplexURI',ComplexURI+','+ComplexURIScheme)]
    [TestCase('RFCEg1',RFCEg1+','+RFCEg1Scheme)]
    [TestCase('RFCEg2',RFCEg2+','+RFCEg2Scheme)]
    [TestCase('RFCEg3',RFCEg3+','+RFCEg3Scheme)]
    [TestCase('RFCEg4',RFCEg4+','+RFCEg4Scheme)]
    [TestCase('RFCEg5',RFCEg5+','+RFCEg5Scheme)]
    [TestCase('RFCEg6',RFCEg6+','+RFCEg6Scheme)]
    [TestCase('RFCEg7',RFCEg7+','+RFCEg7Scheme)]
    [TestCase('RFCEg8',RFCEg8+','+RFCEg8Scheme)]
    [TestCase('RFCEg9',RFCEg9+','+RFCEg9Scheme)]
    [TestCase('FileURI2', FileURI2+','+FileURI2Scheme)]
    [TestCase('FileURI3', FileURI3+','+FileURI3Scheme)]
    [TestCase('Empty URI', string.Empty+','+string.Empty)]
    procedure Scheme_prop_returns_expected_value(const AURIStr, Expected: string);

    [Test]
    [TestCase('SimpleURI',SimpleURI+','+SimpleURIUsername)]
    [TestCase('ComplexURI',ComplexURI+','+ComplexURIUsername)]
    [TestCase('RFCEg3',RFCEg3+','+RFCEg3Username)]
    [TestCase('RFCEg4',RFCEg4+','+RFCEg4Username)]
    [TestCase('MailtoURI',MailtoURI+','+MailtoURIUsername)]
    [TestCase('Empty URI', string.Empty+','+string.Empty)]
    procedure Username_prop_returns_expected_value(const AURIStr, Expected: string);

    [Test]
    [TestCase('SimpleURI',SimpleURI+','+SimpleURIPassword)]
    [TestCase('ComplexURI',ComplexURI+','+ComplexURIPassword)]
    [TestCase('RFCEg3',RFCEg3+','+RFCEg3Password)]
    [TestCase('MailtoURI',MailtoURI+','+MailtoURIPassword)]
    [TestCase('Empty URI', string.Empty+','+string.Empty)]
    procedure Password_prop_returns_expected_value(const AURIStr, Expected: string);

    [Test]
    [TestCase('SimpleURI',SimpleURI+','+SimpleURIHost)]
    [TestCase('ComplexURI',ComplexURI+','+ComplexURIHost)]
    [TestCase('RFCEg1',RFCEg1+','+RFCEg1Host)]
    [TestCase('RFCEg2',RFCEg2+','+RFCEg2Host)]
    [TestCase('RFCEg3',RFCEg3+','+RFCEg3Host)]
    [TestCase('RFCEg4',RFCEg4+','+RFCEg4Host)]
    [TestCase('RFCEg5',RFCEg5+','+RFCEg5Host)]
    [TestCase('RFCEg6',RFCEg6+','+RFCEg6Host)]
    [TestCase('RFCEg7',RFCEg7+','+RFCEg7Host)]
    [TestCase('RFCEg8',RFCEg8+','+RFCEg8Host)]
    [TestCase('RFCEg9',RFCEg9+','+RFCEg9Host)]
    [TestCase('FileURI2', FileURI2+','+FileURI2Host)]
    [TestCase('FileURI3', FileURI3+','+FileURI3Host)]
    [TestCase('MailtoURI',MailtoURI+','+MailtoURIHost)]
    [TestCase('Empty URI', string.Empty+','+string.Empty)]
    procedure Host_prop_returns_expected_value(const AURIStr, Expected: string);

    [Test]
    [TestCase('SimpleURI',SimpleURI+','+SimpleURIPort)]
    [TestCase('ComplexURI',ComplexURI+','+ComplexURIPort)]
    [TestCase('RFCEg1',RFCEg1+','+RFCEg1Port)]
    [TestCase('RFCEg2',RFCEg2+','+RFCEg2Port)]
    [TestCase('RFCEg3',RFCEg3+','+RFCEg3Port)]
    [TestCase('RFCEg4',RFCEg4+','+RFCEg4Port)]
    [TestCase('RFCEg5',RFCEg5+','+RFCEg5Port)]
    [TestCase('RFCEg6',RFCEg6+','+RFCEg6Port)]
    [TestCase('RFCEg7',RFCEg7+','+RFCEg7Port)]
    [TestCase('RFCEg8',RFCEg8+','+RFCEg8Port)]
    [TestCase('RFCEg9',RFCEg9+','+RFCEg9Port)]
    [TestCase('FileURI2', FileURI2+','+FileURI2Port)]
    [TestCase('FileURI3', FileURI3+','+FileURI3Port)]
    [TestCase('MailtoURI',MailtoURI+','+MailtoURIPort)]
    [TestCase('Empty URI', string.Empty+',0')]
    procedure Port_prop_returns_expected_value(const AURIStr, Expected: string);

    [Test]
    [TestCase('SimpleURI',SimpleURI+','+SimpleURIPath)]
    [TestCase('ComplexURI',ComplexURI+','+ComplexURIPath)]
    [TestCase('RFCEg1',RFCEg1+','+RFCEg1Path)]
    [TestCase('RFCEg2',RFCEg2+','+RFCEg2Path)]
    [TestCase('RFCEg3',RFCEg3+','+RFCEg3Path)]
    [TestCase('RFCEg4',RFCEg4+','+RFCEg4Path)]
    [TestCase('RFCEg5',RFCEg5+','+RFCEg5Path)]
    [TestCase('RFCEg6',RFCEg6+','+RFCEg6Path)]
    [TestCase('RFCEg7',RFCEg7+','+RFCEg7Path)]
    [TestCase('RFCEg8',RFCEg8+','+RFCEg8Path)]
    [TestCase('RFCEg9',RFCEg9+','+RFCEg9Path)]
    [TestCase('FileURI2', FileURI2+','+FileURI2Path)]
    [TestCase('FileURI3', FileURI3+','+FileURI3Path)]
    [TestCase('MailtoURI',MailtoURI+','+MailtoURIPath)]
    [TestCase('HttpURI',HttpURI+','+HttpURIPath)]
    procedure Path_prop_returns_expected_value(const AURIStr, Expected: string);

    [Test]
    [TestCase('SimpleURI',SimpleURI+','+SimpleURIQuery)]
    [TestCase('ComplexURI',ComplexURI+','+ComplexURIQuery)]
    [TestCase('RFCEg1',RFCEg1+','+RFCEg1Query)]
    [TestCase('RFCEg2',RFCEg2+','+RFCEg2Query)]
    [TestCase('RFCEg3',RFCEg3+','+RFCEg3Query)]
    [TestCase('RFCEg4',RFCEg4+','+RFCEg4Query)]
    [TestCase('RFCEg5',RFCEg5+','+RFCEg5Query)]
    [TestCase('RFCEg6',RFCEg6+','+RFCEg6Query)]
    [TestCase('RFCEg7',RFCEg7+','+RFCEg7Query)]
    [TestCase('RFCEg8',RFCEg8+','+RFCEg8Query)]
    [TestCase('RFCEg9',RFCEg9+','+RFCEg9Query)]
    [TestCase('FileURI2', FileURI2+','+FileURI2Query)]
    [TestCase('FileURI3', FileURI3+','+FileURI3Query)]
    [TestCase('MailtoURI',MailtoURI+','+MailtoURIQuery)]
    [TestCase('HttpURI',HttpURI+','+HttpURIQuery)]
    procedure Query_prop_returns_expected_value(const AURIStr, Expected: string);

    [Test]
    [TestCase('SimpleURI',SimpleURI+','+SimpleURIQuery)]
    [TestCase('ComplexURI',ComplexURI+','+ComplexURIQuery)]
    [TestCase('RFCEg1',RFCEg1+','+RFCEg1Query)]
    [TestCase('RFCEg2',RFCEg2+','+RFCEg2Query)]
    [TestCase('RFCEg3',RFCEg3+','+RFCEg3Query)]
    [TestCase('RFCEg4',RFCEg4+','+RFCEg4Query)]
    [TestCase('RFCEg5',RFCEg5+','+RFCEg5Query)]
    [TestCase('RFCEg6',RFCEg6+','+RFCEg6Query)]
    [TestCase('RFCEg7',RFCEg7+','+RFCEg7Query)]
    [TestCase('RFCEg8',RFCEg8+','+RFCEg8Query)]
    [TestCase('RFCEg9',RFCEg9+','+RFCEg9Query)]
    [TestCase('FileURI2', FileURI2+','+FileURI2Query)]
    [TestCase('FileURI3', FileURI3+','+FileURI3Query)]
    [TestCase('MailtoURI',MailtoURI+','+MailtoURIQuery)]
    [TestCase('HttpURI',HttpURI+','+HttpURIQuery)]
    procedure Params_prop_returns_expected_value(const AURIStr, Expected: string);

    [Test]
    [TestCase('SimpleURI',SimpleURI+','+SimpleURIFragment)]
    [TestCase('ComplexURI',ComplexURI+','+ComplexURIFragment)]
    [TestCase('RFCEg1',RFCEg1+','+RFCEg1Fragment)]
    [TestCase('RFCEg2',RFCEg2+','+RFCEg2Fragment)]
    [TestCase('RFCEg3',RFCEg3+','+RFCEg3Fragment)]
    [TestCase('RFCEg4',RFCEg4+','+RFCEg4Fragment)]
    [TestCase('RFCEg5',RFCEg5+','+RFCEg5Fragment)]
    [TestCase('RFCEg6',RFCEg6+','+RFCEg6Fragment)]
    [TestCase('RFCEg7',RFCEg7+','+RFCEg7Fragment)]
    [TestCase('RFCEg8',RFCEg8+','+RFCEg8Fragment)]
    [TestCase('RFCEg9',RFCEg9+','+RFCEg9Fragment)]
    [TestCase('FileURI2', FileURI2+','+FileURI2Fragment)]
    [TestCase('FileURI3', FileURI3+','+FileURI3Fragment)]
    [TestCase('MailtoURI',MailtoURI+','+MailtoURIFragment)]
    [TestCase('HttpURI',HttpURI+','+HttpURIFragment)]
    procedure Fragment_prop_returns_expected_value(const AURIStr, Expected: string);

    [Test]
    [TestCase('Both non-empty =',ComplexURI+','+ComplexURI+',0')]
    [TestCase('Both non-empty <',RFCEg7+','+RFCEg8+',-1')]
    [TestCase('Both non-empty >',RFCEg7+','+SimpleURI+',1')]
    [TestCase('empty < non-empty',string.Empty + ','+ComplexURI+',-1')]
    [TestCase('non-empty > empty',SimpleURI+','+string.Empty+',1')]
    [TestCase('empty = empty', string.Empty+','+string.Empty+',0')]
    procedure Compare_returns_correct_value_for_uri_ordering(const Left, Right: string; const Expected: Integer);

    [Test]
    [TestCase('Both non-empty =',ComplexURI+','+ComplexURI+',True')]
    [TestCase('Both non-empty <',RFCEg7+','+RFCEg8+',False')]
    [TestCase('Both non-empty >',RFCEg7+','+SimpleURI+',False')]
    [TestCase('empty = empty', string.Empty+','+string.Empty+',True')]
    [TestCase('empty < non-empty',string.Empty + ','+ComplexURI+',False')]
    [TestCase('non-empty > empty',SimpleURI+','+string.Empty+',False')]
    procedure Equals_op_returns_expected_value_for_uri_equality(const Left, Right: string; const Expected: Boolean);

    [Test]
    [TestCase('Both non-empty =',ComplexURI+','+ComplexURI+',False')]
    [TestCase('Both non-empty <',RFCEg7+','+RFCEg8+',True')]
    [TestCase('Both non-empty >',RFCEg7+','+SimpleURI+',True')]
    [TestCase('empty = empty', string.Empty+','+string.Empty+',False')]
    [TestCase('empty < non-empty',string.Empty + ','+ComplexURI+',True')]
    [TestCase('non-empty > empty',SimpleURI+','+string.Empty+',True')]
    procedure NotEquals_op_returns_expected_value_for_uri_equality(const Left, Right: string; const Expected: Boolean);

    [Test]
    [TestCase('native := ComplexURIL:foreign',ComplexURI+','+ComplexURIEncoded)]
    [TestCase('native := RFC8Eg:foreign',RFCEg8+','+RFCEg8Encoded)]
    procedure ImplictCast_op_works_with_assignment(const AURIStr, Expected: string);

  end;

implementation

uses
  System.Classes,
  System.Math;

procedure TTestTImmutableURI.Compare_returns_correct_value_for_uri_ordering(const Left,
  Right: string; const Expected: Integer);
begin
  var L := TImmutableURI.Create(Left, True);
  var R := TImmutableURI.Create(Right, True);
  Assert.AreEqual(Sign(Expected), Sign(TImmutableURI.Compare(L, R)));
end;

procedure TTestTImmutableURI.ctor_raises_exception_for_bad_uri;
begin
  // Exceptions raised for bad URI string regardless of whether empty strings are permitted
  for var AllowEmptyStr: Boolean in [False, True] do
    Assert.WillRaise(
      procedure
      begin
        var URI := TImmutableURI.Create(BadURI, AllowEmptyStr);
      end,
      EURI
    );
end;

procedure TTestTImmutableURI.ctor_raises_exception_for_empty_string_when_not_permitted;
begin
  Assert.WillRaise(
    procedure
    begin
      // pass empty string when empty strings are not permitted
      var URI := TImmutableURI.Create(string.Empty, False);
    end,
    EURI
  );
end;

procedure TTestTImmutableURI.ctor_succeeds_on_valid_and_empty_uri_strings_when_empty_strings_permitted(
  const AURIStr: string);
begin
  Assert.WillNotRaise(
    procedure
    begin
      var URI := TImmutableURI.Create(AURIStr, True);
    end
  );
end;

procedure TTestTImmutableURI.ctor_succeeds_on_valid_uri_strings_when_empty_strings_not_permitted(
  const AURIStr: string);
begin
  Assert.WillNotRaise(
    procedure
    begin
      var URI := TImmutableURI.Create(AURIStr, False);
    end
  );
end;

procedure TTestTImmutableURI.Equals_op_returns_expected_value_for_uri_equality(
  const Left, Right: string; const Expected: Boolean);
begin
  var L := TImmutableURI.Create(Left, True);
  var R := TImmutableURI.Create(Right, True);
  Assert.AreEqual(Expected, L = R);
end;

procedure TTestTImmutableURI.Fragment_prop_returns_expected_value(const AURIStr,
  Expected: string);
begin
  var U := TImmutableURI.Create(AURIStr, True);
  Assert.AreEqual(Expected, U.Fragment);
end;

procedure TTestTImmutableURI.Host_prop_returns_expected_value(const AURIStr,
  Expected: string);
begin
  var U := TImmutableURI.Create(AURIStr, True);
  Assert.AreEqual(Expected, U.Host);
end;

procedure TTestTImmutableURI.ImplictCast_op_works_with_assignment(const AURIStr, Expected: string);
begin
  var UF := System.Net.URLClient.TURI.Create(AURIStr);
  var U: TImmutableURI := UF;
  Assert.AreEqual(UF.ToString, U.ToString, 'Check native same as foreign');
  Assert.IsFalse(U.IsEmpty, 'Check native not empty');
end;

procedure TTestTImmutableURI.IsEmpty_returns_expected_value_for_empty_and_non_empty_uris(
  AURIString: string; Expected: Boolean);
begin
  var URI := TImmutableURI.Create(AURIString, True);
  Assert.AreEqual(Expected, URI.IsEmpty);
end;

procedure TTestTImmutableURI.IsValidURIString_returns_expected_result_depending_whether_empty_strings_permitted(
  const AURIStr: string; const APermitEmpty, Expected: Boolean);
begin
  Assert.AreEqual(Expected, TImmutableURI.IsValidURIString(AURIStr, APermitEmpty));
end;

procedure TTestTImmutableURI.NotEquals_op_returns_expected_value_for_uri_equality(
  const Left, Right: string; const Expected: Boolean);
begin
  var L := TImmutableURI.Create(Left, True);
  var R := TImmutableURI.Create(Right, True);
  Assert.AreEqual(Expected, L <> R);
end;

procedure TTestTImmutableURI.Params_prop_returns_expected_value(const AURIStr,
  Expected: string);
begin
  var U := TImmutableURI.Create(AURIStr, True);
  var Q := Expected.Split(['&']);
  var ExpectedParams := TStringList.Create;
  for var I in Q do
  begin
    var NV := I.Split(['=']);
    if Length(NV) >= 2 then
      ExpectedParams.AddPair(NV[0], NV[1])
    else if Length(NV) = 1 then
      ExpectedParams.AddPair(NV[0], '');
  end;
  ExpectedParams.Sort;
  var GotParams := TStringList.Create;
  for var P in U.Params do
    GotParams.AddPair(P.Name, P.Value);
  GotParams.Sort;
  Assert.AreEqual(GotParams.Count, ExpectedParams.Count, 'Check param count');
  Assert.AreEqual(ExpectedParams, GotParams, 'Check parameter content');
end;

procedure TTestTImmutableURI.Password_prop_returns_expected_value(const AURIStr,
  Expected: string);
begin
  var U := TImmutableURI.Create(AURIStr, True);
  Assert.AreEqual(Expected, U.Password);
end;

procedure TTestTImmutableURI.Path_prop_returns_expected_value(const AURIStr,
  Expected: string);
begin
  var U := TImmutableURI.Create(AURIStr, True);
  Assert.AreEqual(Expected, U.Path);
end;

procedure TTestTImmutableURI.Port_prop_returns_expected_value(const AURIStr,
  Expected: string);
begin
  var U := TImmutableURI.Create(AURIStr, True);
  Assert.AreEqual(Expected.ToInteger, U.Port);
end;

procedure TTestTImmutableURI.Query_prop_returns_expected_value(const AURIStr,
  Expected: string);
begin
  var U := TImmutableURI.Create(AURIStr, True);
  Assert.AreEqual(Expected, U.Query);
end;

procedure TTestTImmutableURI.Scheme_prop_returns_expected_value(const AURIStr,
  Expected: string);
begin
  var U := TImmutableURI.Create(AURIStr, True);
  Assert.AreEqual(Expected, U.Scheme);
end;

procedure TTestTImmutableURI.Setup;
begin
end;

procedure TTestTImmutableURI.TearDown;
begin
end;

procedure TTestTImmutableURI.ToString_returns_empty_string_for_empty_uri;
begin
  var URI := TImmutableURI.Create(string.Empty, True);
  Assert.AreEqual(string.Empty, URI.ToString);
end;

procedure TTestTImmutableURI.ToString_returns_URI_unchanged(const AURIStr: string; const Expected: string);
begin
  var URI := TImmutableURI.Create(AURIStr, False);
  Assert.AreEqual(Expected, URI.ToString);
end;

procedure TTestTImmutableURI.Username_prop_returns_expected_value(const AURIStr,
  Expected: string);
begin
  var U := TImmutableURI.Create(AURIStr, True);
  Assert.AreEqual(Expected, U.Username);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestTImmutableURI);

end.
