program DatabaseTest;

uses
  Forms,
  FmTestMain in 'FmTestMain.pas' {Form1},
  CS.Database.Types in '..\Engine\CS.Database.Types.pas',
  CS.Database.Core.SnippetsTable in '..\Engine\CS.Database.Core.SnippetsTable.pas',
  CS.Utils.Hashes in '..\Extra\CS.Utils.Hashes.pas',
  UConsts in '..\..\TrunkSrc\UConsts.pas',
  UStrUtils in '..\..\TrunkSrc\UStrUtils.pas',
  CS.Markup in '..\Extra\CS.Markup.pas',
  CS.Database.Main in '..\Engine\CS.Database.Main.pas',
  UBaseObjects in '..\..\TrunkSrc\UBaseObjects.pas',
  CS.Database.Snippets in '..\Engine\CS.Database.Snippets.pas',
  UExceptions in '..\..\TrunkSrc\UExceptions.pas',
  UStructs in '..\..\TrunkSrc\UStructs.pas',
  CS.Utils.Dates in '..\Extra\CS.Utils.Dates.pas',
  UUtils in '..\..\TrunkSrc\UUtils.pas',
  CS.Database.Core.Lookups in '..\Engine\CS.Database.Core.Lookups.pas',
  CS.Database.Exceptions in '..\Engine\CS.Database.Exceptions.pas',
  CS.Database.IO.Native in '..\Engine\CS.Database.IO.Native.pas',
  UIStringList in '..\..\TrunkSrc\UIStringList.pas',
  IntfCommon in '..\..\TrunkSrc\IntfCommon.pas',
  UIOUtils in '..\..\TrunkSrc\UIOUtils.pas',
  UDataStreamIO in '..\..\TrunkSrc\UDataStreamIO.pas',
  PJStreamWrapper in '..\..\TrunkSrc\3rdParty\PJStreamWrapper.pas',
  UEncodings in '..\..\TrunkSrc\UEncodings.pas',
  ULocales in '..\..\TrunkSrc\ULocales.pas',
  USystemInfo in '..\..\TrunkSrc\USystemInfo.pas',
  PJSysInfo in '..\..\TrunkSrc\3rdParty\PJSysInfo.pas',
  CS.SourceCode.Languages in '..\..\SyntaxHighlighting\Languages\CS.SourceCode.Languages.pas',
  CS.SourceCode.Hiliter.Brushes in '..\..\SyntaxHighlighting\Highlighters\CS.SourceCode.Hiliter.Brushes.pas',
  SynEdit in '..\..\SyntaxHighlighting\SynEditUnits\SynEdit.pas',
  SynEditHighlighter in '..\..\SyntaxHighlighting\SynEditUnits\SynEditHighlighter.pas',
  SynEditHighlighterOptions in '..\..\SyntaxHighlighting\SynEditUnits\SynEditHighlighterOptions.pas',
  SynEditKbdHandler in '..\..\SyntaxHighlighting\SynEditUnits\SynEditKbdHandler.pas',
  SynEditKeyCmds in '..\..\SyntaxHighlighting\SynEditUnits\SynEditKeyCmds.pas',
  SynEditKeyConst in '..\..\SyntaxHighlighting\SynEditUnits\SynEditKeyConst.pas',
  SynEditMiscClasses in '..\..\SyntaxHighlighting\SynEditUnits\SynEditMiscClasses.pas',
  SynEditMiscProcs in '..\..\SyntaxHighlighting\SynEditUnits\SynEditMiscProcs.pas',
  SynEditStrConst in '..\..\SyntaxHighlighting\SynEditUnits\SynEditStrConst.pas',
  SynEditTextBuffer in '..\..\SyntaxHighlighting\SynEditUnits\SynEditTextBuffer.pas',
  SynEditTypes in '..\..\SyntaxHighlighting\SynEditUnits\SynEditTypes.pas',
  SynEditWordWrap in '..\..\SyntaxHighlighting\SynEditUnits\SynEditWordWrap.pas',
  SynHighlighterHtml in '..\..\SyntaxHighlighting\SynEditUnits\SynHighlighterHtml.pas',
  SynHighlighterJScript in '..\..\SyntaxHighlighting\SynEditUnits\SynHighlighterJScript.pas',
  SynHighlighterMulti in '..\..\SyntaxHighlighting\SynEditUnits\SynHighlighterMulti.pas',
  SynHighlighterPas in '..\..\SyntaxHighlighting\SynEditUnits\SynHighlighterPas.pas',
  SynHighlighterPHP in '..\..\SyntaxHighlighting\SynEditUnits\SynHighlighterPHP.pas',
  SynRegExpr in '..\..\SyntaxHighlighting\SynEditUnits\SynRegExpr.pas',
  SynTextDrawer in '..\..\SyntaxHighlighting\SynEditUnits\SynTextDrawer.pas',
  SynUnicode in '..\..\SyntaxHighlighting\SynEditUnits\SynUnicode.pas',
  Compilers.UGlobals in '..\..\TrunkSrc\Compilers.UGlobals.pas',
  CS.Database.Tags in '..\Engine\CS.Database.Tags.pas',
  Collections.Bags in '..\..\DelphiCol\Collections.Bags.pas',
  Collections.Base in '..\..\DelphiCol\Collections.Base.pas',
  Collections.BidiDictionaries in '..\..\DelphiCol\Collections.BidiDictionaries.pas',
  Collections.BidiMaps in '..\..\DelphiCol\Collections.BidiMaps.pas',
  Collections.Dictionaries in '..\..\DelphiCol\Collections.Dictionaries.pas',
  Collections.Dynamic in '..\..\DelphiCol\Collections.Dynamic.pas',
  Collections.Lists in '..\..\DelphiCol\Collections.Lists.pas',
  Collections.MultiMaps in '..\..\DelphiCol\Collections.MultiMaps.pas',
  Collections.Queues in '..\..\DelphiCol\Collections.Queues.pas',
  Collections.Sets in '..\..\DelphiCol\Collections.Sets.pas',
  Collections.Stacks in '..\..\DelphiCol\Collections.Stacks.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
