{
  This unit is dedicated to public domain under the CC0 license.
  See https://creativecommons.org/public-domain/cc0/
}

unit Test.Utils.Dates;

interface

uses
  DUnitX.TestFramework,

  System.SysUtils,
  System.DateUtils,

  CSLE.Utils.Dates;

type
  [TestFixture]
  TTestUTCDateTime = class
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // NOTE: TUTCDateTime.ToDateTime is called in the ctor and Now method tests
    //       so if those tests pass we can assume that ToDateTime also passes.

    [Test]
    procedure TDateTime_ctor_for_UTC_date_leaves_date_unchanged;
    [Test]
    procedure TDateTime_ctor_for_local_date_adjusts_date_correctly;
    [Test]
    procedure TDateTime_ctor_for_UTC_date_rounds_to_second_correctly;
    [Test]
    procedure TDateTime_ctor_for_local_date_rounds_to_second_correctly;

    [Test]
    procedure Now_is_approximately_correct_when_unrounded;
    [Test]
    procedure Now_is_approximately_correct_when_rounded;

    [Test]
    procedure CreateNull_returns_value_for_which_IsNull_is_true;
    [Test]
    procedure IsNull_returns_false_for_valid_date;

    [Test]
    [TestCase('#1 (= Local)', 'True,2000,01,01,00,00,00,000,2000,01,01,00,00,00,000,False')]
    [TestCase('#2 (= UTC)',   'True,2020,12,31,23,59,59,999,2020,12,31,23,59,59,999,True')]
    [TestCase('#3 (< Local)', 'False,1959,01,03,18,25,04,123,1959,01,03,18,25,04,124,False')]
    [TestCase('#4 (< UTC)',   'False,1949,08,20,14,45,56,678,1959,01,03,18,25,04,124,True')]
    [TestCase('#5 (> Local)', 'False,2023,07,01,00,00,00,000,2023,06,30,23,59,59,999,False')]
    [TestCase('#6 (> UTC)',   'False,2013,06,02,03,09,47,849,2013,04,26,21,30,20,839,True')]
    procedure Equal_op(Expected: Boolean; YL, ML, DL, HL, NL, SL, MSL, YR, MR, DR, HR, NR, SR, MSR: UInt16; IsUTC: Boolean);

    [Test]
    [TestCase('#1 (= Local)', 'False,2000,01,01,00,00,00,000,2000,01,01,00,00,00,000,False')]
    [TestCase('#2 (= UTC)',   'False,2020,12,31,23,59,59,999,2020,12,31,23,59,59,999,True')]
    [TestCase('#3 (< Local)', 'True,1959,01,03,18,25,04,123,1959,01,03,18,25,04,124,False')]
    [TestCase('#4 (< UTC)',   'True,1949,08,20,14,45,56,678,1959,01,03,18,25,04,124,True')]
    [TestCase('#5 (> Local)', 'True,2023,07,01,00,00,00,000,2023,06,30,23,59,59,999,False')]
    [TestCase('#6 (> UTC)',   'True,2013,06,02,03,09,47,849,2013,04,26,21,30,20,839,True')]
    procedure NotEqual_op(Expected: Boolean; YL, ML, DL, HL, NL, SL, MSL, YR, MR, DR, HR, NR, SR, MSR: UInt16; IsUTC: Boolean);

    [Test]
    [TestCase('#1 (= Local)', 'False,2000,01,01,00,00,00,000,2000,01,01,00,00,00,000,False')]
    [TestCase('#2 (= UTC)',   'False,2020,12,31,23,59,59,999,2020,12,31,23,59,59,999,True')]
    [TestCase('#3 (< Local)', 'False,1959,01,03,18,25,04,123,1959,01,03,18,25,04,124,False')]
    [TestCase('#4 (< UTC)',   'False,1949,08,20,14,45,56,678,1959,01,03,18,25,04,124,True')]
    [TestCase('#5 (> Local)', 'True,2023,07,01,00,00,00,000,2023,06,30,23,59,59,999,False')]
    [TestCase('#6 (> UTC)',   'True,2013,06,02,03,09,47,849,2013,04,26,21,30,20,839,True')]
    procedure GreaterThan_op(Expected: Boolean; YL, ML, DL, HL, NL, SL, MSL, YR, MR, DR, HR, NR, SR, MSR: UInt16; IsUTC: Boolean);

    [Test]
    [TestCase('#1 (= Local)', 'True,2000,01,01,00,00,00,000,2000,01,01,00,00,00,000,False')]
    [TestCase('#2 (= UTC)',   'True,2020,12,31,23,59,59,999,2020,12,31,23,59,59,999,True')]
    [TestCase('#3 (< Local)', 'False,1959,01,03,18,25,04,123,1959,01,03,18,25,04,124,False')]
    [TestCase('#4 (< UTC)',   'False,1949,08,20,14,45,56,678,1959,01,03,18,25,04,124,True')]
    [TestCase('#5 (> Local)', 'True,2023,07,01,00,00,00,000,2023,06,30,23,59,59,999,False')]
    [TestCase('#6 (> UTC)',   'True,2013,06,02,03,09,47,849,2013,04,26,21,30,20,839,True')]
    procedure GreaterThanOrEqual_op(Expected: Boolean; YL, ML, DL, HL, NL, SL, MSL, YR, MR, DR, HR, NR, SR, MSR: UInt16; IsUTC: Boolean);

    [Test]
    [TestCase('#1 (= Local)', 'False,2000,01,01,00,00,00,000,2000,01,01,00,00,00,000,False')]
    [TestCase('#2 (= UTC)',   'False,2020,12,31,23,59,59,999,2020,12,31,23,59,59,999,True')]
    [TestCase('#3 (< Local)', 'True,1959,01,03,18,25,04,123,1959,01,03,18,25,04,124,False')]
    [TestCase('#4 (< UTC)',   'True,1949,08,20,14,45,56,678,1959,01,03,18,25,04,124,True')]
    [TestCase('#5 (> Local)', 'False,2023,07,01,00,00,00,000,2023,06,30,23,59,59,999,False')]
    [TestCase('#6 (> UTC)',   'False,2013,06,02,03,09,47,849,2013,04,26,21,30,20,839,True')]
    procedure LessThan_op(Expected: Boolean; YL, ML, DL, HL, NL, SL, MSL, YR, MR, DR, HR, NR, SR, MSR: UInt16; IsUTC: Boolean);

    [Test]
    [TestCase('#1 (= Local)', 'True,2000,01,01,00,00,00,000,2000,01,01,00,00,00,000,False')]
    [TestCase('#2 (= UTC)',   'True,2020,12,31,23,59,59,999,2020,12,31,23,59,59,999,True')]
    [TestCase('#3 (< Local)', 'True,1959,01,03,18,25,04,123,1959,01,03,18,25,04,124,False')]
    [TestCase('#4 (< UTC)',   'True,1949,08,20,14,45,56,678,1959,01,03,18,25,04,124,True')]
    [TestCase('#5 (> Local)', 'False,2023,07,01,00,00,00,000,2023,06,30,23,59,59,999,False')]
    [TestCase('#6 (> UTC)',   'False,2013,06,02,03,09,47,849,2013,04,26,21,30,20,839,True')]
    procedure LessThanOrEqual_op(Expected: Boolean; YL, ML, DL, HL, NL, SL, MSL, YR, MR, DR, HR, NR, SR, MSR: UInt16; IsUTC: Boolean);

    [Test]
    [TestCase('#1','1949,08,20,14,45,56,678,1949-08-20T14:45:56.678Z')]
    [TestCase('#2','2000,01,01,00,00,00,000,2000-01-01T00:00:00.000Z')]
    [TestCase('#3','1999,12,31,23,59,59,999,1999-12-31T23:59:59.999Z')]
    [TestCase('#4','2023,07,01,09,04,23,823,2023-07-01T09:04:23.823Z')]
    procedure ToISO8601String_unrounded(const Y, M, D, H, N, S, MS: UInt16; Expected: string);
    [Test]
    [TestCase('#1 (round up)','1949,08,20,14,45,56,678,1949-08-20T14:45:57Z')]
    [TestCase('#2 (round down)','2000,01,01,00,00,00,000,2000-01-01T00:00:00Z')]
    [TestCase('#3 (round up)','1999,12,31,23,59,59,999,2000-01-01T00:00:00Z')]
    [TestCase('#4(round down)','2023,07,01,09,04,23,123,2023-07-01T09:04:23Z')]
    procedure ToISO8601String_rounded_to_nearest_sec(const Y, M, D, H, N, S, MS: UInt16; Expected: string);

    // Following test depends on ToISO8601String method, tested above
    [Test]
    [TestCase('#A','2023-07-01T08:56:23.456Z,2023-07-01T08:56:23.456Z')]
    [TestCase('#B','2020-02-08,2020-02-08T00:00:00.000Z')]
    [TestCase('#C','2020-02-08T09:00:23Z,2020-02-08T09:00:23.000Z')]
    [TestCase('#D','2020-02-08T09:00:23.456Z,2020-02-08T09:00:23.456Z')]
    [TestCase('#A (short)','20230701T085623.456Z,2023-07-01T08:56:23.456Z')]
    [TestCase('#A (short date, long time)','20230701T08:56:23.456Z,2023-07-01T08:56:23.456Z')]
    [TestCase('#A (long date, short time)','2023-07-01T085623.456Z,2023-07-01T08:56:23.456Z')]
    [TestCase('#A (short, no millis)','20230701T085623Z,2023-07-01T08:56:23.000Z')]
    [TestCase('#E (+03:00)','2023-07-01T08:56:23.456+03:00,2023-07-01T05:56:23.456Z')]
    [TestCase('#E (+0300)','2023-07-01T08:56:23.456+0300,2023-07-01T05:56:23.456Z')]
    [TestCase('#F (-02:30)','2023-07-01T08:56:23.456-02:30,2023-07-01T11:26:23.456Z')]
    [TestCase('#E (-0230)','2023-07-01T08:56:23.456-0230,2023-07-01T11:26:23.456Z')]
    [TestCase('#G (+12:00)','2023-07-01T06:00:00+12:00,2023-06-30T18:00:00.000Z')]
    [TestCase('#H (-12:00)','2023-06-30T12:00:00-12:00,2023-07-01T00:00:00.000Z')]
    procedure CreateFromISO8601String_works_for_valid_date_strings(const DateStr, Expected: string);
    [Test]
    procedure CreateFromISO8601String_raises_exception_for_invalid_date_string;

    // Following test depends on ToISO8601String method, tested above
    [Test]
    [TestCase('#A', '2023-07-03T12:13:14.000Z,2023-07-03T12:13:14.000Z')]
    [TestCase('#B', '1949-04-26T23:34:46.499Z,1949-04-26T23:34:46.000Z')]
    [TestCase('#C', '1999-12-31T23:59:59.999Z,2000-01-01T00:00:00.000Z')]
    [TestCase('#D', '2001-05-06T08:00:00.500Z,2001-05-06T08:00:01.000Z')]
    procedure RoundToNearestSecond_works_for_unrounded_dates(const DateStr, Expected: string);
    [Test]
    procedure RoundToNearestSecond_works_for_already_rounded_date;

    // Avoid using any locale specific characters with following test
    [Test]
    [TestCase('#A','2023-07-03T12:13:14.123Z,"day="d" month="m" year="yy,day=3 month=7 year=23')]
    [TestCase('#B','1939-04-26T12:03:14.123Z,"hr="hh" min="nn" sec="ss,hr=12 min=03 sec=14')]
    procedure ToString_with_default_format_settings(const DateStr, FmtStr, Expected: string);
    [Test]
    [TestCase('#A','2023-07-03T12:13:14.123Z,"D/M/Y="d/m/yy,D/M/Y=3/7/23')]
    [TestCase('#B','1939-04-26T12:03:14.123Z,"H:M:S="hh:nn:ss,H:M:S=12:03:14')]
    procedure ToString_with_invariant_format_settings(const DateStr, FmtStr, Expected: string);
    [Test]
    [TestCase('#A','2023-07-03T12:13:14.123Z,yyyy/mm,2023%07')]
    [TestCase('#B','1939-04-26T12:03:14.123Z,hh:mm:ss,12^03^14')]
    procedure ToString_with_custom_format_settings(const DateStr, FmtStr, Expected: string);

    [Test]
    [TestCase('#A','2023-07-01T08:56:23.456Z')]
    [TestCase('#B','2020-02-08')]
    [TestCase('#B (Trailing T)','2020-02-08T')]
    [TestCase('#C','2020-02-08T09:00:23Z')]
    [TestCase('#D','2020-02-08T09:00:23.456Z')]
    [TestCase('#A (short)','20230701T085623.456Z')]
    [TestCase('#A (short date, long time)','20230701T08:56:23.456Z')]
    [TestCase('#A (long date, short time)','2023-07-01T085623.456Z')]
    [TestCase('#A (short, no millis)','20230701T085623Z,')]
    [TestCase('#E (+03:00)','2023-07-01T08:56:23.456+03:00')]
    [TestCase('#E (+0300)','2023-07-01T08:56:23.456+0300')]
    [TestCase('#F (-02:30)','2023-07-01T08:56:23.456-02:30')]
    [TestCase('#E (-0230)','2023-07-01T08:56:23.456-0230')]
    [TestCase('#G (+12:00)','2023-07-01T06:00:00+12:00')]
    [TestCase('#H (-12:00)','2023-06-30T12:00:00-12:00')]
    [TestCase('#I (-ve timezone with no separators)','20230831T000000-0100')]
    procedure IsValidISO8601String_is_true(const Str: string);
    [Test]
    [TestCase('#A (empty)','')]
    [TestCase('#B (date with trailing Z)','2023-07-01Z')]
    [TestCase('#C (invalid date)','2023-08-32T00:00:00Z')]
    [TestCase('#D (invalid time)','2023-08-31T12:60:00Z')]
    procedure IsValidISO8601String_is_false(const Str: string);

  end;

implementation

procedure TTestUTCDateTime.CreateFromISO8601String_raises_exception_for_invalid_date_string;
begin
  Assert.WillRaise(
    procedure
    begin
      var Date := TUTCDateTime.CreateFromISO8601String('20230630T120000+12:00');
    end,
    EDateTimeException
  );
end;

procedure TTestUTCDateTime.CreateFromISO8601String_works_for_valid_date_strings(
  const DateStr, Expected: string);
begin
  var Date := TUTCDateTime.CreateFromISO8601String(DateStr);
  var Actual := Date.ToISO8601String;
  Assert.AreEqual(Expected, Actual);
end;

procedure TTestUTCDateTime.CreateNull_returns_value_for_which_IsNull_is_true;
begin
  Assert.IsTrue(TUTCDateTime.CreateNull.IsNull);
end;

procedure TTestUTCDateTime.Equal_op(Expected: Boolean; YL, ML, DL, HL, NL, SL,
  MSL, YR, MR, DR, HR, NR, SR, MSR: UInt16; IsUTC: Boolean);
begin
  var Left := TUTCDateTime.Create(EncodeDateTime(YL, ML, DL, HL, NL, SL, MSL), IsUTC);
  var Right := TUTCDateTime.Create(EncodeDateTime(YR, MR, DR, HR, NR, SR, MSR), IsUTC);
  Assert.AreEqual(Expected, Left = Right);
end;

procedure TTestUTCDateTime.GreaterThanOrEqual_op(Expected: Boolean; YL, ML, DL,
  HL, NL, SL, MSL, YR, MR, DR, HR, NR, SR, MSR: UInt16; IsUTC: Boolean);
begin
  var Left := TUTCDateTime.Create(EncodeDateTime(YL, ML, DL, HL, NL, SL, MSL), IsUTC);
  var Right := TUTCDateTime.Create(EncodeDateTime(YR, MR, DR, HR, NR, SR, MSR), IsUTC);
  Assert.AreEqual(Expected, Left >= Right);
end;

procedure TTestUTCDateTime.GreaterThan_op(Expected: Boolean; YL, ML, DL, HL, NL,
  SL, MSL, YR, MR, DR, HR, NR, SR, MSR: UInt16; IsUTC: Boolean);
begin
  var Left := TUTCDateTime.Create(EncodeDateTime(YL, ML, DL, HL, NL, SL, MSL), IsUTC);
  var Right := TUTCDateTime.Create(EncodeDateTime(YR, MR, DR, HR, NR, SR, MSR), IsUTC);
  Assert.AreEqual(Expected, Left > Right);
end;

procedure TTestUTCDateTime.IsNull_returns_false_for_valid_date;
begin
  Assert.IsFalse(TUTCDateTime.Now.IsNull);
end;

procedure TTestUTCDateTime.IsValidISO8601String_is_false(const Str: string);
begin
  Assert.IsFalse(TUTCDateTime.IsValidISO8601String(Str));
end;

procedure TTestUTCDateTime.IsValidISO8601String_is_true(const Str: string);
begin
  Assert.IsTrue(TUTCDateTime.IsValidISO8601String(Str));
end;

procedure TTestUTCDateTime.LessThanOrEqual_op(Expected: Boolean; YL, ML, DL, HL,
  NL, SL, MSL, YR, MR, DR, HR, NR, SR, MSR: UInt16; IsUTC: Boolean);
begin
  var Left := TUTCDateTime.Create(EncodeDateTime(YL, ML, DL, HL, NL, SL, MSL), IsUTC);
  var Right := TUTCDateTime.Create(EncodeDateTime(YR, MR, DR, HR, NR, SR, MSR), IsUTC);
  Assert.AreEqual(Expected, Left <= Right);
end;

procedure TTestUTCDateTime.LessThan_op(Expected: Boolean; YL, ML, DL, HL, NL,
  SL, MSL, YR, MR, DR, HR, NR, SR, MSR: UInt16; IsUTC: Boolean);
begin
  var Left := TUTCDateTime.Create(EncodeDateTime(YL, ML, DL, HL, NL, SL, MSL), IsUTC);
  var Right := TUTCDateTime.Create(EncodeDateTime(YR, MR, DR, HR, NR, SR, MSR), IsUTC);
  Assert.AreEqual(Expected, Left < Right);
end;

procedure TTestUTCDateTime.NotEqual_op(Expected: Boolean; YL, ML, DL, HL, NL,
  SL, MSL, YR, MR, DR, HR, NR, SR, MSR: UInt16; IsUTC: Boolean);
begin
  var Left := TUTCDateTime.Create(EncodeDateTime(YL, ML, DL, HL, NL, SL, MSL), IsUTC);
  var Right := TUTCDateTime.Create(EncodeDateTime(YR, MR, DR, HR, NR, SR, MSR), IsUTC);
  Assert.AreEqual(Expected, Left <> Right);
end;

procedure TTestUTCDateTime.Now_is_approximately_correct_when_rounded;
begin
  // WARNING: This test can't be accurate because we have to guess the time when
  // TUTCDate.Now is called. The best way to do that is to take snapshots #
  // before and after calling the method. But note the margin for error is up to
  // 2 seconds after rounding.

  // IMPORTANT: preserve order of following three statements. We must have
  // N0 < time D created < N0
  var N0 := System.SysUtils.Now;    // local time
  var D := TUTCDateTime.Now(True);  // UTC time
  var N1 := System.SysUtils.Now;    // local time

  // Adjust N0 & N1 to UTC
  var N0UTC := TTimeZone.Local.ToUniversalTime(N0);
  var N1UTC := TTimeZone.Local.ToUniversalTime(N1);

  // Let R(x) be a function that rounds date time x to nearest second. Let Dc be
  // the time that D was created.
  // Since N0UTC <= Dc <= N1UTC we must have R(N0UTC) <= R(Dc) <= R(N1UTC)
  // Let Rfloor(x) be a function rounds date time x down to the nearest second
  // and let Rceil(x) round x to the next second up,
  // Observe that Rfloor(x) <= R(x) <= Rceil(x)
  // Hence Rfloor(N0UTC) <= R(N0UTC) <= R(Dc) <= R(N1UTC) <= Rceil(N1UTC)

  var Year, Month, Day, Hour, Min, Sec, MS: UInt16;

  // Find R(Dc)=RDc, Rfloor(N0UTC)=RFloor and Rceil(N1UTC)=RCeil

  var RDc := D.ToDateTime;

  DecodeDateTime(N0UTC, Year, Month, Day, Hour, Min, Sec, MS);
  var RFloor := EncodeDateTime(Year, Month, Day, Hour, Min, Sec, 0);

  DecodeDateTime(N1UTC, Year, Month, Day, Hour, Min, Sec, MS);
  var RCeil := IncSecond(EncodeDateTime(Year, Month, Day, Hour, Min, Sec, 0));

  // Do checks
  Assert.IsTrue((CompareDateTime(RFloor, RDc) <= 0) and (CompareDateTime(RCeil, RDc) >= 0), 'Check date');
  Assert.AreEqual(0, MilliSecondOf(RDc), 'Rounded MS = 0');
end;

procedure TTestUTCDateTime.Now_is_approximately_correct_when_unrounded;
begin
  // IMPORTANT: preserve order of following three statements. We must have
  // N0 < time D created < N0
  var N0 := System.SysUtils.Now;  // local time
  var D := TUTCDateTime.Now;      // UTC time
  var N1 := System.SysUtils.Now;  // local time

  // Adjust N0 to UTC
  var NUTC := TTimeZone.Local.ToUniversalTime(N0);

  // Since N0 < time D created < N1, time D created must be with N1 - N0 ms of
  // N0, so with Delta = N1 - N0, we must have N0 < D < Delta, so that's the
  // best we can make this test
  var Delta := Extended(MilliSecondSpan(N1, N0));
  Assert.AreEqual(Extended(NUTC), Extended(D.ToDateTime), Delta);
end;

procedure TTestUTCDateTime.RoundToNearestSecond_works_for_already_rounded_date;
begin
  var Date := TUTCDateTime.Create(EncodeDateTime(2023,07,01,10,34,25,234), True, True);
  var Expected := Extended(EncodeDateTime(2023,07,01,10,34,25,0));
  Assert.AreEqual(Expected, Extended(Date.ToDateTime), 'Setup check');
  var RDate := Date.RoundToNearestSecond;
  Assert.AreEqual(Expected, Extended(RDate.ToDateTime), 'RoundToNearestSecond check');
end;

procedure TTestUTCDateTime.RoundToNearestSecond_works_for_unrounded_dates(
  const DateStr, Expected: string);
begin
  var Date := TUTCDateTime.CreateFromISO8601String(DateStr);
  var RDate := Date.RoundToNearestSecond;
  Assert.AreEqual(Expected, RDate.ToISO8601String);
end;

procedure TTestUTCDateTime.Setup;
begin
end;

procedure TTestUTCDateTime.TDateTime_ctor_for_local_date_adjusts_date_correctly;
begin
  var Local := System.SysUtils.Now;
  var UTC := TTimeZone.Local.ToUniversalTime(Local);
  var D := TUTCDateTime.Create(Local, False); // create date from local time
  Assert.IsTrue(SameDateTime(UTC, D.ToDateTime));
end;

procedure TTestUTCDateTime.TDateTime_ctor_for_local_date_rounds_to_second_correctly;
begin
  var Year, Month, Day, Hour, Min, Sec, MS: UInt16;

  // Create two local date times, close to Now, one of which will round up MS,
  // the other of which will round MS down.
  var LocalDate := System.SysUtils.Now;
  DecodeDateTime(LocalDate, Year, Month, Day, Hour, Min, Sec, MS);
  var LocalDateLo := EncodeDateTime(Year, Month, Day, Hour, Min, Sec, 123);
  var LocalDateHi := EncodeDateTime(Year, Month, Day, Hour, Min, Sec, 690);

  // Create expected UTC versions of LocalDateLo and LocalDateHi
  var UTCDateLo := TTimeZone.Local.ToUniversalTime(LocalDateLo);
  DecodeDateTime(UTCDateLo, Year, Month, Day, Hour, Min, Sec, MS);
  var ExpectedLo := EncodeDateTime(Year, Month, Day, Hour, Min, Sec, 0);
  var UTCDateHi := TTimeZone.Local.ToUniversalTime(LocalDateHi);
  DecodeDateTime(UTCDateHi, Year, Month, Day, Hour, Min, Sec, MS);
  var ExpectedHi := EncodeDateTime(Year, Month, Day, Hour, Min, Sec, 0);
  ExpectedHi := IncSecond(ExpectedHi);

  // Create UTC date times from adjusted local dates and test
  var DLo := TUTCDateTime.Create(LocalDateLo, False, True);
  var DHi := TUTCDateTime.Create(LocalDateHi, False, True);
  Assert.IsTrue(SameDateTime(ExpectedLo, DLo.ToDateTime), 'Round down');
  Assert.IsTrue(SameDateTime(ExpectedHi, DHi.ToDateTime), 'Round up');
end;

procedure TTestUTCDateTime.TDateTime_ctor_for_UTC_date_leaves_date_unchanged;
begin
  var N := System.SysUtils.Now;
  var D := TUTCDateTime.Create(N, True, False);
  Assert.IsTrue(SameDateTime(N, D.ToDateTime));
end;

procedure TTestUTCDateTime.TDateTime_ctor_for_UTC_date_rounds_to_second_correctly;
begin
  var UTCDate := System.SysUtils.Now;
  var Year, Month, Day, Hour, Min, Sec, MS: UInt16;
  DecodeDateTime(UTCDate, Year, Month, Day, Hour, Min, Sec, MS);
  // Create date times close to UTCDate, one that will round down, one that
  // will round up
  var UTCDateLo := EncodeDateTime(Year, Month, Day, Hour, Min, Sec, 123);
  var UTCDateHi := EncodeDateTime(Year, Month, Day, Hour, Min, Sec, 690);
  // Create expected rounded dates
  var ExpectedLo := EncodeDateTime(Year, Month, Day, Hour, Min, Sec, 0);
  var ExpectedHi := IncSecond(ExpectedLo);
  // Create UTC date time instances for UTCDate dates & test them
  var DLo := TUTCDateTime.Create(UTCDateLo, True, True);
  var DHi := TUTCDateTime.Create(UTCDateHi, True, True);
  Assert.IsTrue(SameDateTime(ExpectedLo, DLo.ToDateTime), 'Round down datetime');
  Assert.IsTrue(SameDateTime(ExpectedHi, DHi.ToDateTime), 'Round up datetime');

  Assert.AreEqual(0, MilliSecondOf(DLo.ToDateTime), 'Round down MS = 0');
  Assert.AreEqual(0, MilliSecondOf(DHi.ToDateTime), 'Round down MS = 0');

end;

procedure TTestUTCDateTime.TearDown;
begin
end;

procedure TTestUTCDateTime.ToISO8601String_rounded_to_nearest_sec(const Y, M, D,
  H, N, S, MS: UInt16; Expected: string);
begin
  var Date := TUTCDateTime.Create(EncodeDateTime(Y, M, D, H, N, S, MS), True);
  var ISOStr := Date.ToISO8601String(True); // rounded
  Assert.AreEqual(Expected, ISOStr);
end;

procedure TTestUTCDateTime.ToISO8601String_unrounded(const Y, M, D, H, N, S,
  MS: UInt16; Expected: string);
begin
  var Date := TUTCDateTime.Create(EncodeDateTime(Y, M, D, H, N, S, MS), True);
  var ISOStr := Date.ToISO8601String; // unrounded
  Assert.AreEqual(Expected, ISOStr);
end;

procedure TTestUTCDateTime.ToString_with_custom_format_settings(const DateStr,
  FmtStr, Expected: string);
begin
  var Date := TUTCDateTime.CreateFromISO8601String(DateStr);
  var Settings := TFormatSettings.Create;
  Settings.DateSeparator := '%';
  Settings.TimeSeparator := '^';
  var OutStr := Date.ToString(FmtStr, Settings);
  Assert.AreEqual(Expected, OutStr);
end;

procedure TTestUTCDateTime.ToString_with_default_format_settings(const DateStr,
  FmtStr, Expected: string);
begin
  var Date := TUTCDateTime.CreateFromISO8601String(DateStr);
  var OutStr := Date.ToString(FmtStr);
  Assert.AreEqual(Expected, OutStr);
end;

procedure TTestUTCDateTime.ToString_with_invariant_format_settings(
  const DateStr, FmtStr, Expected: string);
begin
  var Date := TUTCDateTime.CreateFromISO8601String(DateStr);
  var OutStr := Date.ToString(FmtStr, TFormatSettings.Invariant);
  Assert.AreEqual(Expected, OutStr);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestUTCDateTime);

end.
