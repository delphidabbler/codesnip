{
 * CodeSnip.dpr
 *
 * Application project file.
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
 * The Original Code is CodeSnip.dpr
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributors:
 *   NONE
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
  UEncrypt in '3rdParty\UEncrypt.pas',
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
  FmProxyServerDlg in 'FmProxyServerDlg.pas' {ProxyServerDlg},
  FmRegistrationDlg in 'FmRegistrationDlg.pas' {RegistrationDlg},
  FmSelectionSearchDlg in 'FmSelectionSearchDlg.pas' {SelectionSearchDlg},
  FmSplash in 'FmSplash.pas' {SplashForm},
  FmUpdateDlg in 'FmUpdateDlg.pas' {UpdateDlg},
  FmUserDBEditDlg in 'FmUserDBEditDlg.pas' {UserDBEditDlg},
  FmViewExtraDlg in 'FmViewExtraDlg.pas' {ViewExtraDlg},
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
  FrSelectSnippetsBase in 'FrSelectSnippetsBase.pas' {SelectSnippetsBaseFrame: TFrame},
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
  UActionFactory in 'UActionFactory.pas',
  UActiveText in 'UActiveText.pas',
  UActiveTextHTML in 'UActiveTextHTML.pas',
  UAlphabet in 'UAlphabet.pas',
  UAltBugFix in 'UAltBugFix.pas',
  UAnchors in 'UAnchors.pas',
  UAppInfo in 'UAppInfo.pas',
  UBaseObjects in 'UBaseObjects.pas',
  UBDSCompiler in 'UBDSCompiler.pas',
  UBorlandCompiler in 'UBorlandCompiler.pas',
  UBrowseProtocol in 'UBrowseProtocol.pas',
  UBugReporter in 'UBugReporter.pas',
  UCategoryAction in 'UCategoryAction.pas',
  UCategoryListAdapter in 'UCategoryListAdapter.pas',
  UCheckSum in 'UCheckSum.pas',
  UChkListStateMgr in 'UChkListStateMgr.pas',
  UClipboardHelper in 'UClipboardHelper.pas',
  UCodeImportExport in 'UCodeImportExport.pas',
  UCodeImportMgr in 'UCodeImportMgr.pas',
  UCodeShareMgr in 'UCodeShareMgr.pas',
  UCodeSubmitter in 'UCodeSubmitter.pas',
  UColorBoxEx in 'UColorBoxEx.pas',
  UColorDialogEx in 'UColorDialogEx.pas',
  UColours in 'UColours.pas',
  UCommandBars in 'UCommandBars.pas',
  UCommonDlg in 'UCommonDlg.pas',
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
  UCtrlArranger in 'UCtrlArranger.pas',
  UDataBackupMgr in 'UDataBackupMgr.pas',
  UDatabaseLoader in 'UDatabaseLoader.pas',
  UDataStreamIO in 'UDataStreamIO.pas',
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
  UGC in 'UGC.pas',
  UGIFImageList in 'UGIFImageList.pas',
  UGraphicUtils in 'UGraphicUtils.pas',
  UGroups in 'UGroups.pas',
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
  UMenuHelper in 'UMenuHelper.pas',
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
  USnipKindListAdapter in 'USnipKindListAdapter.pas',
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
  UUnitsChkListMgr in 'UUnitsChkListMgr.pas',
  UUpdateMgr in 'UUpdateMgr.pas',
  UUserDBBackup in 'UUserDBBackup.pas',
  UUserDBMgr in 'UUserDBMgr.pas',
  UUtils in 'UUtils.pas',
  UVersionInfo in 'UVersionInfo.pas',
  UView in 'UView.pas',
  UViewItemAction in 'UViewItemAction.pas',
  UViewItemTreeNode in 'UViewItemTreeNode.pas',
  UWaitForActionUI in 'UWaitForActionUI.pas',
  UWBCommandBars in 'UWBCommandBars.pas',
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

