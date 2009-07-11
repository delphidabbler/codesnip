{
 * CodeSnip.dpr
 *
 * Application project file.
 *
 * v0.1 of 30 Jan 2005  - Original version.
 * v0.2 of 20 Feb 2005  - Added new UHTMLDetailUtils, UDetailPageLoader and
 *                        UHTMLGenerators units.
 *                      - Removed UCompCheckHTML.pas and UHTMLProducers units.
 * v0.3 of 22 Feb 2005  - Added new IntfNotifier, UWBExternal and UNotifier
 *                        units.
 *                      - Removed UExtCompCheck and UExtInfo units.
 * v0.4 of 23 Feb 2005  - Added new FrBrowserBase, FrDetailView and
 *                        FrHTMLTpltDlg units.
 * v0.5 of 24 Feb 2005  - Added new UMessageBox unit.
 * v0.6 of 26 Feb 2005  - Added new UIniDataReader and USnipData units.
 * v0.7 of 28 Feb 2005  - Added new UWBHighlighter unit.
 * v0.8 of 05 Mar 2005  - Added new IntfCommon, FmCompilersDlg, UCompilers,
 *                        UCompilerBase, UCompilerUtils and UFreePascalCompiler
 *                        units.
 *                      - Added new Images.res resource file.
 * v0.9 of 10 Mar 2005  - Added new UBaseHiliter, UDetailHTMLHiliter,
 *                        UHilitePasLexer, UHilitePasParser and
 *                        UTextStreamReader units.
 * v0.10 of 17 Mar 2005 - Added new USaveSnippetDlg, USaveSnippetMgr,
 *                        FrTextPreview, FrRTFPreview, FrHTMLPreview,
 *                        IntfPreview, FmPreviewDlg, URTFUtils, UHiliteAttrs and
 *                        USyntaxHiliters units.
 *                      - Removed units: UBaseHiliter and UDetailHTMLHiliter
 *                        (replaced by USyntaxHiliters).
 *                      - Added new Dialogs.res resource file.
 * v0.11 of 21 Apr 2005 - Renamed UUnitGen unit as USourceGen.
 *                      - Renamed UCompilerTypes unit as IntfCompilers.
 *                      - Added new IntfHiliter unit.
 * v0.12 of 25 Apr 2005 - Added new UHelpTopicAction unit.
 * v0.13 of 20 Nov 2005 - Added new UWebService and UBugReporter units.
 * v0.14 of 30 Nov 2005 - Added new UHTMLHelp and UHelpMgr units.
 * v0.15 of 04 Jan 2006 - Added new UParams unit.
 * v0.16 of 06 Jan 2006 - Added new FmPreferencesDlg, UCopySnippetMgr,
 *                        UFileHiliter, UPreferences, USnippetMgr and
 *                        USnippetSourceGen units.
 * v0.17 of 06 Jan 2006 - Added new UBDSCompiler and UBorlandCompiler units.
 *                      - Added model view support code on move to Delphi 2006.
 * v0.18 of 10 Jan 2006 - Removed model view support code on reversion to Delphi
 *                        7.
 * v0.19 of 11 Jan 2006 - Added new UConsoleApp, UCompilerRunner and UPipe
 *                        units.
 *                      - Removed UCompilerUtils unit.
 * v0.20 of 12 Jan 2006 - Added new UCheckSum unit.
 * v0.21 of 02 Apr 2006 - Added new UFileUpdater, ULocalFileList,
 *                        URemoteFileList, UObsoleteFileList, UDownloadFileList,
 *                        UUpdateAnalyser and UDataBackupMgr units.
 * v0.22 of 02 Apr 2006 - Deleted resource directive that included CodeSnip.res.
 *                        Icon is now defined in Images.res.
 * v0.23 of 04 Apr 2006 - Added new USettings and UWindowSettings units.
 * v0.24 of 07 Apr 2006 - Added new FmWizardDlg, FmRegistration, URegistrar,
 *                        USystemInfo and UVersionInfo units.
 * v0.25 of 16 Apr 2006 - Added new UWBNulDropTarget unit.
 * v0.26 of 01 May 2006 - Added new FrNews, FrNewsHTML, UDataStreamReader,
 *                        UDOSDateTime, UDownloadMonitor, UMemoInfo,
 *                        UMemoProgBarMgr and UNews units.
 *                      - Removed UDatedFile, UDownloadFileList, ULocalFileList,
 *                        UObsoleteFileList, URemoteFileList and UUpdateAnalyser
 *                        units.
 * v1.0 of 05 Jun 2006  - Added important compiler directives that should not
 *                        be changed along with description directive.
 *                      - Added UCompilerRunner unit. This was created and used
 *                        from v0.19 but not added to project file in error.
 *                      - Added new FmHTMLViewDlg and UExecFileAction units.
 *                      - Renamed CodeSnip_TLB.pas unit as
 *                        AutoGen\IntfExternalObj.pas.
 *                      - Added MD5.pas to project. Has been referenced
 *                        previously, but not included in project files.
 *                      - Changed named of included type library from
 *                        CodeSnip.tlb to ExternalObj.tlb.
 * v1.1 of 25 Oct 2006  - Added new UProtocolHandler unit.
 *                        Renamed UExecFileAction unit as UShellExecAction.
 * v1.2 of 29 Oct 2006  - Added new FmFindXRefsDlg, FmSelectionSearchDlg,
 *                        FrSelectSnippets, USaveUnitMgr, USourceFileInfo and
 *                        USourceFileOutputMgr units.
 * v1.3 of 31 Oct 2006  - Added new UThemesEx and UTVCheckBoxes units.
 * v1.4 of 07 Nov 2006  - Added new UCSSUtils and URTFBuilder units.
 * v1.5 of 09 Nov 2006  - Added new FrHiliterPrefs, FrPrefsBase, FrSourcePrefs,
 *                        UFontHelper and UHiliterPersist units.
 * v1.6 of 12 Nov 2006  - Added new FrSelectSnippets unit.
 * v1.7 of 14 Nov 2006  - Renamed UProtocolHandler unit as UProtocols.
 *                      - Added new UHelpProtocol and UHTTPProtocol units.
 * v1.8 of 18 Nov 2006  - Added new FmMailingListDlg and UMailListSubscriber
 *                        units.
 * v1.9 of 21 Nov 2006  - Added new UGraphicUtils unit.
 * v1.10 of 23 Nov 2006 - Added new UJavaScriptUtils unit.
 * v1.11 of 25 Nov 2006 - Added new UColours and UHiliterCSS units.
 * v1.12 of 01 Dec 2006 - Added new FmWaitDlg and UWaitForActionUI units.
 * v1.13 of 02 Dec 2006 - Added new UDHTML and UCompResHTML units.
 * v1.14 of 03 Dec 2006 - Added new URoutineHTML unit.
 * v1.15 of 04 Dec 2006 - Added new UTestUnit unit.
 * v1.16 of 03 Feb 2007 - Added new UStatusBarMgr unit.
 * v1.17 of 04 Feb 2007 - Added new UQuery unit.
 * v1.18 of 04 Feb 2007 - Added new UHTMLHelpMgr unit.
 * v1.18 of 06 Feb 2007 - Added new IntfAligner, UActionFactory, UDialogMgr,
 *                        UFormAligner and UNulFormAligner units.
 * v1.19 of 11 Feb 2007 - Added new UContributors unit.
 * v1.20 of 17 Feb 2007 - Added UHTMLDocHelper and UWBHelper units.
 * v1.21 of 25 Feb 2007 - Added new FmSplash unit.
 *                      - Added code to create and show splash form.
 * v1.22 of 01 Mar 2007 - Added new UThreadEx, UThreadWrapper and
 *                        UDatabaseLoader units.
 * v1.23 of 13 May 2007 - Renamed UMemoInfo unit as UMemoHelper.
 *                      - Added new FrMemoPreview unit.
 * v1.24 of 02 Jul 2007 - Added new UHTMLBuilder unit.
 * v1.25 of 07 Sep 2007 - Added new FmPrintDlg, FrGeneralPrefs, FrPrintingPrefs,
 *                        UCommonDlgAligner, UHiddenRichEdit, UHiddenWindow,
 *                        UMeasurement, UPageSetupDialogEx, UPageSetupDlgMgr,
 *                        UPrintDocuments, UPrintEngine, UPrinterDocPropsDlg,
 *                        UPrintInfo, UPrintMgr, and URTFMerger units.
 * v1.26 of 27 Sep 2007 - Added new UDlgAligner unit.
 *                      - Removed UCommonDlgAligner unit.
 * v1.27 of 18 Oct 2007 - Added new UHTMLEvents and USimpleDispatch units.
 * v1.28 of 03 Nov 2007 - Added new IntfWBPopupMenus, UAnchors, UDispatchList,
 *                        UIStringList, ULinkAction and UWBPopupMenus units.
 * v1.29 of 04 Nov 2007 - Removed UHelpTopicAction and UShellExecAction units.
 * v1.30 of 22 Apr 2008 - Removed Images.res and Dialogs.res and replaced by
 *                        new Resources.res.
 *                      - Added new FrRTFShowCase and UShowCaseCtrl units.
 * v1.31 of 02 Jun 2008 - Added new UMarquee unit.
 * v1.32 of 14 Jun 2008 - Renamed UDlgAligner unit as UDlgHelper.
 *                      - Added new UAltBugFix and UMultiCastEvents units.
 * v1.33 of 10 Aug 2008 - Added new UOpenDialogEx, UColorBoxEx and
 *                        UColorDialogEx units.
 * v1.34 of 11 Aug 2008 - Added new UMessageWindow unit.
 * v1.35 of 24 Aug 2008 - Added new USystemID unit.
 * v1.36 of 25 Aug 2008 - Added new UTestCompileUI unit.
 * v1.37 of 14 Sep 2008 - Added new FmUserDBEditDlg, UDataStreamWriter,
 *                        UEditRoutineAction, UGIFImageList, UImageTags,
 *                        UNulDataReader, USaveDialogEx, USnippetsIO,
 *                        UUserDBBackup, UUserDBMgr, UXMLDataIO and
 *                        UXMLDocumentEx units.
 *                      - Added 3rd party GIFImage unit to project.
 *                      - Set Application.ModalPopupMode to pmAuto to ensure
 *                        that TCommonDialog's MessageHook (and overrides) get
 *                        called.
 * v1.38 of 05 Oct 2008 - Renamed UIntfObjects as UBaseObjects.
 * v1.39 of 15 Dec 2008 - Added new FmCodeExportDlg, FmCodeSubmitDlg,
 *                        FmEditTextDlg, FrSelectUserSnippets,
 *                        UCodeImportExport, UCodeImportMgr, UCodeShareMgr,
 *                        UCodeSubmitter, UEmailHelper, UOpenDialogHelper and
 *                        UStructs units.
 * v1.40 of 17 Dec 2008 - Added new UXMLDocConsts and UXMLDocHelper units.
 * v1.41 of 30 Dec 2008 - Added new FrFixedHTMLDlg, UActiveText, UREMLDataIO,
 *                        URoutineCreditsParser, URoutineDoc,
 *                        URoutineExtraHelper, URTFRoutineDoc, UStacks and
 *                        UTaggedTextLexer units.
 * v1.42 of 04 Jan 2009 - Added new UClipboardHelper, UControlStateMgr,
 *                        UCopyInfoMgr and UTextRoutineDoc units.
 * v1.43 of 10 Jan 2009 - Added new UCompileMgr unit.
 * v1.44 of 13 Jan 2009 - Added new UConsts and USnippetsTVDraw units.
 * v1.45 of 25 Jan 2009 - Renamed UHTMLGenerators unit as UDetailPageHTML.
 *                      - Added new IntfHTMLDocHostInfo unit.
 * v1.46 of 26 Jan 2009 - Added new ULocales unit.
 * v1.47 of 10 May 2009 - Added new FmDonateDlg and UPaypalDonateAction units.
 * v1.48 of 13 May 2009 - Added new UWebInfo unit.
 *                      - Removed UGlobals and UParams units.
 * v1.49 of 17 Jun 2009 - Added new FmDependenciesDlg, FmEasterEgg, FrEasterEgg,
 *                        FrOverview, UAlphabet, UCommandBars, UCopySourceMgr,
 *                        UIniDataLoader, ULists, UOverviewTreeBuilder,
 *                        UOverviewTreeState, USnippetIDs, USnippetKindInfo,
 *                        USnippetsChkListMgr, USnippetValidator,
 *                        UTestUnitDlgMgr, UToolButtonEx and UViewItemTreeNode
 *                        units.
 * v1.50 of 05 Jul 2009 - Added new UAutoFree, UBrowseProtocol and UFileProtocol
 *                        units.
 * v1.51 of 11 Jul 2009 - Removed UCopySnippetMgr and USnippetMgr units.
 *                      - Added new UCopyViewMgr unit.
 *
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
 * The Original Code is CodeSnip.dpr
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


program CodeSnip;

{$ALIGN 8}
{$APPTYPE GUI}
{$BOOLEVAL ON}
{$DESCRIPTION 'Code Snippets Database Viewer'}
{$EXTENDEDSYNTAX ON}
{$HINTS ON}
{$IOCHECKS ON}
{$LONGSTRINGS ON}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}
{$TYPEDADDRESS OFF}
{$WARNINGS ON}
{$WRITEABLECONST OFF}

{%ToDo 'CodeSnip.todo'}

uses
  Forms,
  GIFImage in '3rdParty\GIFImage.pas',
  MD5 in '3rdParty\MD5.pas',
  IntfExternalObj in 'AutoGen\IntfExternalObj.pas',
  FmAboutDlg in 'FmAboutDlg.pas' {AboutDlg},
  FmBase in 'FmBase.pas' {BaseForm},
  FmBugReportDlg in 'FmBugReportDlg.pas' {BugReportDlg},
  FmCodeExportDlg in 'FmCodeExportDlg.pas' {CodeExportDlg},
  FmCodeSubmitDlg in 'FmCodeSubmitDlg.pas' {CodeSubmitDlg},
  FmCompErrorDlg in 'FmCompErrorDlg.pas' {CompErrorDlg},
  FmCompilersDlg in 'FmCompilersDlg.pas' {CompilersDlg},
  FmDependenciesDlg in 'FmDependenciesDlg.pas' {DependenciesDlg},
  FmDonateDlg in 'FmDonateDlg.pas' {DonateDlg},
  FmEasterEgg in 'FmEasterEgg.pas' {EasterEggForm},
  FmEditTextDlg in 'FmEditTextDlg.pas' {EditTextDlg},
  FmFindCompilerDlg in 'FmFindCompilerDlg.pas' {FindCompilerDlg},
  FmFindTextDlg in 'FmFindTextDlg.pas' {FindTextDlg},
  FmFindXRefsDlg in 'FmFindXRefsDlg.pas' {FindXRefsDlg},
  FmGenericDlg in 'FmGenericDlg.pas' {GenericDlg},
  FmGenericOKDlg in 'FmGenericOKDlg.pas' {GenericOKDlg},
  FmGenericViewDlg in 'FmGenericViewDlg.pas' {GenericViewDlg},
  FmHelpAware in 'FmHelpAware.pas' {HelpAwareForm},
  FmHTMLViewDlg in 'FmHTMLViewDlg.pas' {HTMLViewDlg},
  FmMailingListDlg in 'FmMailingListDlg.pas' {MailingListDlg},
  FmMain in 'FmMain.pas' {MainForm},
  FmPreferencesDlg in 'FmPreferencesDlg.pas' {PreferencesDlg},
  FmPreviewDlg in 'FmPreviewDlg.pas' {PreviewDlg},
  FmPrintDlg in 'FmPrintDlg.pas' {PrintDlg},
  FmRegistrationDlg in 'FmRegistrationDlg.pas' {RegistrationDlg},
  FmSelectionSearchDlg in 'FmSelectionSearchDlg.pas' {SelectionSearchDlg},
  FmSplash in 'FmSplash.pas' {SplashForm},
  FmUpdateDlg in 'FmUpdateDlg.pas' {UpdateDlg},
  FmUserDBEditDlg in 'FmUserDBEditDlg.pas' {UserDBEditDlg},
  FmWaitDlg in 'FmWaitDlg.pas' {WaitDlg},
  FmWizardDlg in 'FmWizardDlg.pas' {WizardDlg},
  FrBrowserBase in 'FrBrowserBase.pas' {BrowserBaseFrame: TFrame},
  FrCompCheck in 'FrCompCheck.pas' {CompCheckFrame: TFrame},
  FrDetail in 'FrDetail.pas' {DetailFrame: TFrame},
  FrDetailView in 'FrDetailView.pas' {DetailViewFrame: TFrame},
  FrEasterEgg in 'FrEasterEgg.pas' {EasterEggFrame: TFrame},
  FrFixedHTMLDlg in 'FrFixedHTMLDlg.pas' {FixedHTMLDlgFrame: TFrame},
  FrGeneralPrefs in 'FrGeneralPrefs.pas' {GeneralPrefsFrame: TFrame},
  FrHiliterPrefs in 'FrHiliterPrefs.pas' {HiliterPrefsFrame: TFrame},
  FrHTMLDlg in 'FrHTMLDlg.pas' {HTMLDlgFrame: TFrame},
  FrHTMLPreview in 'FrHTMLPreview.pas' {HTMLPreviewFrame: TFrame},
  FrHTMLTpltDlg in 'FrHTMLTpltDlg.pas' {HTMLTpltDlgFrame: TFrame},
  FrInfo in 'FrInfo.pas' {InfoFrame: TFrame},
  FrMemoPreview in 'FrMemoPreview.pas' {MemoPreviewFrame: TFrame},
  FrNews in 'FrNews.pas' {NewsFrame: TFrame},
  FrNewsHTML in 'FrNewsHTML.pas' {NewsHTMLFrame: TFrame},
  FrOverview in 'FrOverview.pas' {OverviewFrame: TFrame},
  FrPrefsBase in 'FrPrefsBase.pas' {PrefsBaseFrame: TFrame},
  FrPrintingPrefs in 'FrPrintingPrefs.pas' {PrintingPrefsFrame: TFrame},
  FrRTFPreview in 'FrRTFPreview.pas' {RTFPreviewFrame: TFrame},
  FrRTFShowCase in 'FrRTFShowCase.pas' {RTFShowCaseFrame: TFrame},
  FrCheckedTV in 'FrCheckedTV.pas' {CheckedTVFrame: TFrame},
  FrSelectSnippets in 'FrSelectSnippets.pas' {SelectSnippetsFrame: TFrame},
  FrSelectUserSnippets in 'FrSelectUserSnippets.pas' {SelectUserSnippetsFrame: TFrame},
  FrSourcePrefs in 'FrSourcePrefs.pas' {SourcePrefsFrame: TFrame},
  FrTextPreview in 'FrTextPreview.pas' {TextPreviewFrame: TFrame},
  FrTitled in 'FrTitled.pas' {TitledFrame: TFrame},
  IntfAligner in 'IntfAligner.pas',
  IntfCommon in 'IntfCommon.pas',
  IntfCompilers in 'IntfCompilers.pas',
  IntfFrameMgrs in 'IntfFrameMgrs.pas',
  IntfHiliter in 'IntfHiliter.pas',
  IntfHTMLDocHostInfo in 'IntfHTMLDocHostInfo.pas',
  IntfNotifier in 'IntfNotifier.pas',
  IntfPreview in 'IntfPreview.pas',
  IntfUIHandlers in 'IntfUIHandlers.pas',
  IntfWBPopupMenus in 'IntfWBPopupMenus.pas',
  UActionFactory in 'UActionFactory.pas',
  UActiveText in 'UActiveText.pas',
  UAlphabet in 'UAlphabet.pas',
  UAltBugFix in 'UAltBugFix.pas',
  UAnchors in 'UAnchors.pas',
  UAppInfo in 'UAppInfo.pas',
  UAutoFree in 'UAutoFree.pas',
  UBaseObjects in 'UBaseObjects.pas',
  UBDSCompiler in 'UBDSCompiler.pas',
  UBorlandCompiler in 'UBorlandCompiler.pas',
  UBrowseProtocol in 'UBrowseProtocol.pas',
  UBugReporter in 'UBugReporter.pas',
  UCheckSum in 'UCheckSum.pas',
  UClipboardHelper in 'UClipboardHelper.pas',
  UCodeImportExport in 'UCodeImportExport.pas',
  UCodeImportMgr in 'UCodeImportMgr.pas',
  UCodeShareMgr in 'UCodeShareMgr.pas',
  UCodeSubmitter in 'UCodeSubmitter.pas',
  UColorBoxEx in 'UColorBoxEx.pas',
  UColorDialogEx in 'UColorDialogEx.pas',
  UColours in 'UColours.pas',
  UCommandBars in 'UCommandBars.pas',
  UCompileMgr in 'UCompileMgr.pas',
  UCompilerBase in 'UCompilerBase.pas',
  UCompilerRunner in 'UCompilerRunner.pas',
  UCompilers in 'UCompilers.pas',
  UCompLogAction in 'UCompLogAction.pas',
  UCompResHTML in 'UCompResHTML.pas',
  UConsoleApp in 'UConsoleApp.pas',
  UConsts in 'UConsts.pas',
  UContributors in 'UContributors.pas',
  UControlStateMgr in 'UControlStateMgr.pas',
  UCopyInfoMgr in 'UCopyInfoMgr.pas',
  UCopySourceMgr in 'UCopySourceMgr.pas',
  UCopyViewMgr in 'UCopyViewMgr.pas',
  UCSSBuilder in 'UCSSBuilder.pas',
  UCSSUtils in 'UCSSUtils.pas',
  UDataBackupMgr in 'UDataBackupMgr.pas',
  UDatabaseLoader in 'UDatabaseLoader.pas',
  UDataStreamReader in 'UDataStreamReader.pas',
  UDataStreamWriter in 'UDataStreamWriter.pas',
  UDelphiCompiler in 'UDelphiCompiler.pas',
  UDetailPageHTML in 'UDetailPageHTML.pas',
  UDetailPageLoader in 'UDetailPageLoader.pas',
  UDHTML in 'UDHTML.pas',
  UDialogMgr in 'UDialogMgr.pas',
  UDispatchList in 'UDispatchList.pas',
  UDlgHelper in 'UDlgHelper.pas',
  UDOSDateTime in 'UDOSDateTime.pas',
  UDownloadMgr in 'UDownloadMgr.pas',
  UDownloadMonitor in 'UDownloadMonitor.pas',
  UEditRoutineAction in 'UEditRoutineAction.pas',
  UEmailHelper in 'UEmailHelper.pas',
  UExceptions in 'UExceptions.pas',
  UExeFileType in 'UExeFileType.pas',
  UFileHiliter in 'UFileHiliter.pas',
  UFileProtocol in 'UFileProtocol.pas',
  UFileUpdater in 'UFileUpdater.pas',
  UFontHelper in 'UFontHelper.pas',
  UFormAligner in 'UFormAligner.pas',
  UFreePascalCompiler in 'UFreePascalCompiler.pas',
  UGIFImageList in 'UGIFImageList.pas',
  UGraphicUtils in 'UGraphicUtils.pas',
  UHelpMgr in 'UHelpMgr.pas',
  UHelpProtocol in 'UHelpProtocol.pas',
  UHiddenRichEdit in 'UHiddenRichEdit.pas',
  UHiddenWindow in 'UHiddenWindow.pas',
  UHiliteAttrs in 'UHiliteAttrs.pas',
  UHilitePasLexer in 'UHilitePasLexer.pas',
  UHilitePasParser in 'UHilitePasParser.pas',
  UHiliterCSS in 'UHiliterCSS.pas',
  UHiliterPersist in 'UHiliterPersist.pas',
  UHistory in 'UHistory.pas',
  UHistoryMenus in 'UHistoryMenus.pas',
  UHTMLBuilder in 'UHTMLBuilder.pas',
  UHTMLDetailUtils in 'UHTMLDetailUtils.pas',
  UHTMLDocHelper in 'UHTMLDocHelper.pas',
  UHTMLEvents in 'UHTMLEvents.pas',
  UHTMLHelp in 'UHTMLHelp.pas',
  UHTMLHelpMgr in 'UHTMLHelpMgr.pas',
  UHTMLTemplate in 'UHTMLTemplate.pas',
  UHTMLUtils in 'UHTMLUtils.pas',
  UHTTPProtocol in 'UHTTPProtocol.pas',
  UImageTags in 'UImageTags.pas',
  UIniDataLoader in 'UIniDataLoader.pas',
  UIniDataReader in 'UIniDataReader.pas',
  UIStringList in 'UIStringList.pas',
  UJavaScriptUtils in 'UJavaScriptUtils.pas',
  ULinkAction in 'ULinkAction.pas',
  ULists in 'ULists.pas',
  ULocales in 'ULocales.pas',
  UMailListSubscriber in 'UMailListSubscriber.pas',
  UMainDisplayMgr in 'UMainDisplayMgr.pas',
  UMarquee in 'UMarquee.pas',
  UMeasurement in 'UMeasurement.pas',
  UMemoHelper in 'UMemoHelper.pas',
  UMemoProgBarMgr in 'UMemoProgBarMgr.pas',
  UMessageBox in 'UMessageBox.pas',
  UMessageWindow in 'UMessageWindow.pas',
  UMultiCastEvents in 'UMultiCastEvents.pas',
  UNews in 'UNews.pas',
  UNotifier in 'UNotifier.pas',
  UNulDataReader in 'UNulDataReader.pas',
  UNulFormAligner in 'UNulFormAligner.pas',
  UNulUIHandler in 'UNulUIHandler.pas',
  UOleClientSite in 'UOleClientSite.pas',
  UOpenDialogHelper in 'UOpenDialogHelper.pas',
  UOpenDialogEx in 'UOpenDialogEx.pas',
  UOverviewTreeBuilder in 'UOverviewTreeBuilder.pas',
  UOverviewTreeState in 'UOverviewTreeState.pas',
  UPageSetupDialogEx in 'UPageSetupDialogEx.pas',
  UPageSetupDlgMgr in 'UPageSetupDlgMgr.pas',
  UPaypalDonateAction in 'UPaypalDonateAction.pas',
  UPipe in 'UPipe.pas',
  UPreferences in 'UPreferences.pas',
  UPrintDocuments in 'UPrintDocuments.pas',
  UPrintEngine in 'UPrintEngine.pas',
  UPrinterDocPropsDlg in 'UPrinterDocPropsDlg.pas',
  UPrintInfo in 'UPrintInfo.pas',
  UPrintMgr in 'UPrintMgr.pas',
  UProtocols in 'UProtocols.pas',
  UQuery in 'UQuery.pas',
  URegistrar in 'URegistrar.pas',
  UREMLDataIO in 'UREMLDataIO.pas',
  URoutineAction in 'URoutineAction.pas',
  URoutineCreditsParser in 'URoutineCreditsParser.pas',
  URoutineDoc in 'URoutineDoc.pas',
  URoutineExtraHelper in 'URoutineExtraHelper.pas',
  URoutineHTML in 'URoutineHTML.pas',
  URTFBuilder in 'URTFBuilder.pas',
  URTFMerger in 'URTFMerger.pas',
  URTFRoutineDoc in 'URTFRoutineDoc.pas',
  URTFUtils in 'URTFUtils.pas',
  USaveDialogEx in 'USaveDialogEx.pas',
  USaveSourceDlg in 'USaveSourceDlg.pas',
  USaveSnippetMgr in 'USaveSnippetMgr.pas',
  USaveUnitMgr in 'USaveUnitMgr.pas',
  USearch in 'USearch.pas',
  USettings in 'USettings.pas',
  UShowCaseCtrl in 'UShowCaseCtrl.pas',
  USimpleDispatch in 'USimpleDispatch.pas',
  USnipData in 'USnipData.pas',
  USnippetIDs in 'USnippetIDs.pas',
  USnippetKindInfo in 'USnippetKindInfo.pas',
  USnippets in 'USnippets.pas',
  USnippetsChkListMgr in 'USnippetsChkListMgr.pas',
  USnippetsIO in 'USnippetsIO.pas',
  USnippetSourceGen in 'USnippetSourceGen.pas',
  USnippetsTVDraw in 'USnippetsTVDraw.pas',
  USnippetValidator in 'USnippetValidator.pas',
  USourceFileInfo in 'USourceFileInfo.pas',
  USourceFileOutputMgr in 'USourceFileOutputMgr.pas',
  USourceGen in 'USourceGen.pas',
  UStacks in 'UStacks.pas',
  UStatusBarMgr in 'UStatusBarMgr.pas',
  UStrStreamWriter in 'UStrStreamWriter.pas',
  UStructs in 'UStructs.pas',
  USyntaxHiliters in 'USyntaxHiliters.pas',
  USystemID in 'USystemID.pas',
  USystemInfo in 'USystemInfo.pas',
  UTaggedTextLexer in 'UTaggedTextLexer.pas',
  UTestCompile in 'UTestCompile.pas',
  UTestCompileUI in 'UTestCompileUI.pas',
  UTestUnit in 'UTestUnit.pas',
  UTestUnitDlgMgr in 'UTestUnitDlgMgr.pas',
  UTextRoutineDoc in 'UTextRoutineDoc.pas',
  UTextStreamReader in 'UTextStreamReader.pas',
  UThemesEx in 'UThemesEx.pas',
  UThreadEx in 'UThreadEx.pas',
  UThreadWrapper in 'UThreadWrapper.pas',
  UToolButtonEx in 'UToolButtonEx.pas',
  UTVCheckBoxes in 'UTVCheckBoxes.pas',
  UUpdateMgr in 'UUpdateMgr.pas',
  UUserDBBackup in 'UUserDBBackup.pas',
  UUserDBMgr in 'UUserDBMgr.pas',
  UUtils in 'UUtils.pas',
  UVersionInfo in 'UVersionInfo.pas',
  UView in 'UView.pas',
  UViewItemAction in 'UViewItemAction.pas',
  UViewItemTreeNode in 'UViewItemTreeNode.pas',
  UWaitForActionUI in 'UWaitForActionUI.pas',
  UWBController in 'UWBController.pas',
  UWBExternal in 'UWBExternal.pas',
  UWBHelper in 'UWBHelper.pas',
  UWBHighlighter in 'UWBHighlighter.pas',
  UWBIOMgr in 'UWBIOMgr.pas',
  UWBNulDropTarget in 'UWBNulDropTarget.pas',
  UWBPopupMenus in 'UWBPopupMenus.pas',
  UWBUIMgr in 'UWBUIMgr.pas',
  UWebInfo in 'UWebInfo.pas',
  UWebService in 'UWebService.pas',
  UWindowSettings in 'UWindowSettings.pas',
  UXMLDataIO in 'UXMLDataIO.pas',
  UXMLDocConsts in 'UXMLDocConsts.pas',
  UXMLDocHelper in 'UXMLDocHelper.pas',
  UXMLDocumentEx in 'UXMLDocumentEx.pas';

// Include resources
{$Resource ExternalObj.tlb} // Type library file
{$Resource HTML.res}        // HTML resources
{$Resource VCodeSnip.res}   // Version information resource
{$Resource Resources.res}   // All other resources.

begin
  Application.Initialize;
  SplashForm := TSplashForm.Create(Application);
  SplashForm.Show;
  Application.ModalPopupMode := pmAuto;
  Application.ProcessMessages;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

