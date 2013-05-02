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
 * Portions created by the Initial Developer are Copyright (C) 2005-2011 Peter
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
{$DESCRIPTION 'Code Snippets Database Manager and Viewer'}
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


uses
  Forms,
  Windows,
  GIFImage in '3rdParty\GIFImage.pas',
  PJMD5 in '3rdParty\PJMD5.pas',
  PJStreamWrapper in '3rdParty\PJStreamWrapper.pas',
  PJSysInfo in '3rdParty\PJSysInfo.pas',
  PJVersionInfo in '3rdParty\PJVersionInfo.pas',
  PJWdwState in '3rdParty\PJWdwState.pas',
  UEncrypt in '3rdParty\UEncrypt.pas',
  IntfExternalObj in 'AutoGen\IntfExternalObj.pas',
  Browser.IntfDocHostUI in 'Browser.IntfDocHostUI.pas',
  Browser.UControlHelper in 'Browser.UControlHelper.pas',
  Browser.UController in 'Browser.UController.pas',
  Browser.UHighlighter in 'Browser.UHighlighter.pas',
  Browser.UIOMgr in 'Browser.UIOMgr.pas',
  Browser.UNulUIHandler in 'Browser.UNulUIHandler.pas',
  Browser.UUIMgr in 'Browser.UUIMgr.pas',
  Compilers.UBDS in 'Compilers.UBDS.pas',
  Compilers.UBorland in 'Compilers.UBorland.pas',
  Compilers.UCompilerBase in 'Compilers.UCompilerBase.pas',
  Compilers.UCompilers in 'Compilers.UCompilers.pas',
  Compilers.UDelphi in 'Compilers.UDelphi.pas',
  Compilers.UFreePascal in 'Compilers.UFreePascal.pas',
  Compilers.UGlobals in 'Compilers.UGlobals.pas',
  Compilers.URunner in 'Compilers.URunner.pas',
  FmAboutDlg in 'FmAboutDlg.pas' {AboutDlg},
  FmAddCategoryDlg in 'FmAddCategoryDlg.pas' {AddCategoryDlg},
  FmBase in 'FmBase.pas' {BaseForm},
  FmBugReportBaseDlg in 'FmBugReportBaseDlg.pas' {BugReportBaseDlg},
  FmCategoryEditDlg in 'FmCategoryEditDlg.pas' {CategoryEditDlg},
  FmCodeExportDlg in 'FmCodeExportDlg.pas' {CodeExportDlg},
  FmCodeSubmitDlg in 'FmCodeSubmitDlg.pas' {CodeSubmitDlg},
  FmCompErrorDlg in 'FmCompErrorDlg.pas' {CompErrorDlg},
  FmCompilersDlg in 'FmCompilersDlg.pas' {CompilersDlg},
  FmDeleteCategoryDlg in 'FmDeleteCategoryDlg.pas' {DeleteCategoryDlg},
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
  FmMain in 'FmMain.pas' {MainForm},
  FmNewsDlg in 'FmNewsDlg.pas' {NewsDlg},
  FmPreferencesDlg in 'FmPreferencesDlg.pas' {PreferencesDlg},
  FmPreviewDlg in 'FmPreviewDlg.pas' {PreviewDlg},
  FmPrintDlg in 'FmPrintDlg.pas' {PrintDlg},
  FmProxyServerDlg in 'FmProxyServerDlg.pas' {ProxyServerDlg},
  FmRegistrationDlg in 'FmRegistrationDlg.pas' {RegistrationDlg},
  FmRenameCategoryDlg in 'FmRenameCategoryDlg.pas' {RenameCategoryDlg},
  FmSelectionSearchDlg in 'FmSelectionSearchDlg.pas' {SelectionSearchDlg},
  FmSnippetsEditorDlg in 'FmSnippetsEditorDlg.pas' {SnippetsEditorDlg},
  FmSplash in 'FmSplash.pas' {SplashForm},
  FmTrappedBugReportDlg in 'FmTrappedBugReportDlg.pas' {TrappedBugReportDlg},
  FmUpdateDlg in 'FmUpdateDlg.pas' {UpdateDlg},
  FmUserBugReportDlg in 'FmUserBugReportDlg.pas' {UserBugReportDlg},
  FmViewExtraDlg in 'FmViewExtraDlg.pas' {ViewExtraDlg},
  FmWaitDlg in 'FmWaitDlg.pas' {WaitDlg},
  FmWizardDlg in 'FmWizardDlg.pas' {WizardDlg},
  FrBrowserBase in 'FrBrowserBase.pas' {BrowserBaseFrame: TFrame},
  FrCategoryDescEdit in 'FrCategoryDescEdit.pas' {CategoryDescEditFrame: TFrame},
  FrCategoryList in 'FrCategoryList.pas' {CategoryListFrame: TFrame},
  FrCodeGenPrefs in 'FrCodeGenPrefs.pas' {CodeGenPrefsFrame: TFrame},
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
  FrNewsPrefs in 'FrNewsPrefs.pas' {NewsPrefsFrame: TFrame},
  FrOverview in 'FrOverview.pas' {OverviewFrame: TFrame},
  FrPrefsBase in 'FrPrefsBase.pas' {PrefsBaseFrame: TFrame},
  FrPrintingPrefs in 'FrPrintingPrefs.pas' {PrintingPrefsFrame: TFrame},
  FrRSSNews in 'FrRSSNews.pas' {RSSNewsFrame: TFrame},
  FrRTFPreview in 'FrRTFPreview.pas' {RTFPreviewFrame: TFrame},
  FrRTFShowCase in 'FrRTFShowCase.pas' {RTFShowCaseFrame: TFrame},
  FrCheckedTV in 'FrCheckedTV.pas' {CheckedTVFrame: TFrame},
  FrSelectSnippetsBase in 'FrSelectSnippetsBase.pas' {SelectSnippetsBaseFrame: TFrame},
  FrSelectSnippets in 'FrSelectSnippets.pas' {SelectSnippetsFrame: TFrame},
  FrSelectUserSnippets in 'FrSelectUserSnippets.pas' {SelectUserSnippetsFrame: TFrame},
  FrSourcePrefs in 'FrSourcePrefs.pas' {SourcePrefsFrame: TFrame},
  FrTextPreview in 'FrTextPreview.pas' {TextPreviewFrame: TFrame},
  FrTitled in 'FrTitled.pas' {TitledFrame: TFrame},
  Hiliter.UAttrs in 'Hiliter.UAttrs.pas',
  Hiliter.UCSS in 'Hiliter.UCSS.pas',
  Hiliter.UFileHiliter in 'Hiliter.UFileHiliter.pas',
  Hiliter.UGlobals in 'Hiliter.UGlobals.pas',
  Hiliter.UHiliters in 'Hiliter.UHiliters.pas',
  Hiliter.UPasLexer in 'Hiliter.UPasLexer.pas',
  Hiliter.UPasParser in 'Hiliter.UPasParser.pas',
  Hiliter.UPersist in 'Hiliter.UPersist.pas',
  IntfAligner in 'IntfAligner.pas',
  IntfCommon in 'IntfCommon.pas',
  IntfFrameMgrs in 'IntfFrameMgrs.pas',
  IntfHTMLDocHostInfo in 'IntfHTMLDocHostInfo.pas',
  IntfNotifier in 'IntfNotifier.pas',
  IntfPreview in 'IntfPreview.pas',
  UActionFactory in 'UActionFactory.pas',
  UActiveText in 'UActiveText.pas',
  UActiveTextHTML in 'UActiveTextHTML.pas',
  UAlphabet in 'UAlphabet.pas',
  UAnchors in 'UAnchors.pas',
  UAppInfo in 'UAppInfo.pas',
  UBaseObjects in 'UBaseObjects.pas',
  UBrowseProtocol in 'UBrowseProtocol.pas',
  UCategoryAction in 'UCategoryAction.pas',
  UCategoryListAdapter in 'UCategoryListAdapter.pas',
  UClipboardHelper in 'UClipboardHelper.pas',
  UCodeImportExport in 'UCodeImportExport.pas',
  UCodeImportMgr in 'UCodeImportMgr.pas',
  UCodeShareMgr in 'UCodeShareMgr.pas',
  UColorBoxEx in 'UColorBoxEx.pas',
  UColorDialogEx in 'UColorDialogEx.pas',
  UColours in 'UColours.pas',
  UCommandBars in 'UCommandBars.pas',
  UCommonDlg in 'UCommonDlg.pas',
  UComparers in 'UComparers.pas',
  UCompileMgr in 'UCompileMgr.pas',
  UCompileResultsLBMgr in 'UCompileResultsLBMgr.pas',
  UCompLogAction in 'UCompLogAction.pas',
  UCompResHTML in 'UCompResHTML.pas',
  UConsoleApp in 'UConsoleApp.pas',
  UConsts in 'UConsts.pas',
  UContainers in 'UContainers.pas',
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
  UDatabaseLoaderUI in 'UDatabaseLoaderUI.pas',
  UDataStreamIO in 'UDataStreamIO.pas',
  UDetailPageHTML in 'UDetailPageHTML.pas',
  UDetailPageLoader in 'UDetailPageLoader.pas',
  UDHTML in 'UDHTML.pas',
  UDialogMgr in 'UDialogMgr.pas',
  UDispatchList in 'UDispatchList.pas',
  UDlgHelper in 'UDlgHelper.pas',
  UDOSDateTime in 'UDOSDateTime.pas',
  UDropDownButtons in 'UDropDownButtons.pas',
  UEditRoutineAction in 'UEditRoutineAction.pas',
  UEmailHelper in 'UEmailHelper.pas',
  UEncodings in 'UEncodings.pas',
  UEncryptor in 'UEncryptor.pas',
  UExceptions in 'UExceptions.pas',
  UExeFileType in 'UExeFileType.pas',
  UFileProtocol in 'UFileProtocol.pas',
  UFileUpdater in 'UFileUpdater.pas',
  UFolderBackup in 'UFolderBackup.pas',
  UFontHelper in 'UFontHelper.pas',
  UFormAligner in 'UFormAligner.pas',
  UGC in 'UGC.pas',
  UGIFImageList in 'UGIFImageList.pas',
  UGraphicUtils in 'UGraphicUtils.pas',
  UGroups in 'UGroups.pas',
  UHelpMgr in 'UHelpMgr.pas',
  UHelpProtocol in 'UHelpProtocol.pas',
  UHexUtils in 'UHexUtils.pas',
  UHiddenRichEdit in 'UHiddenRichEdit.pas',
  UHiddenWindow in 'UHiddenWindow.pas',
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
  UKeysHelper in 'UKeysHelper.pas',
  ULEDImageList in 'ULEDImageList.pas',
  ULinkAction in 'ULinkAction.pas',
  ULocales in 'ULocales.pas',
  UMainDisplayMgr in 'UMainDisplayMgr.pas',
  UMarquee in 'UMarquee.pas',
  UMeasurement in 'UMeasurement.pas',
  UMemoCaretPosDisplayMgr in 'UMemoCaretPosDisplayMgr.pas',
  UMemoHelper in 'UMemoHelper.pas',
  UMemoProgBarMgr in 'UMemoProgBarMgr.pas',
  UMenuHelper in 'UMenuHelper.pas',
  UMessageBox in 'UMessageBox.pas',
  UMessageWindow in 'UMessageWindow.pas',
  UMultiCastEvents in 'UMultiCastEvents.pas',
  UNotifier in 'UNotifier.pas',
  UNulDataReader in 'UNulDataReader.pas',
  UNulDropTarget in 'UNulDropTarget.pas',
  UNulFormAligner in 'UNulFormAligner.pas',
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
  UPrintInfo in 'UPrintInfo.pas',
  UPrintMgr in 'UPrintMgr.pas',
  UProtocols in 'UProtocols.pas',
  UQuery in 'UQuery.pas',
  UREMLDataIO in 'UREMLDataIO.pas',
  UReservedCategories in 'UReservedCategories.pas',
  UResourceUtils in 'UResourceUtils.pas',
  URFC2822Date in 'URFC2822Date.pas',
  URoutineAction in 'URoutineAction.pas',
  URoutineCreditsParser in 'URoutineCreditsParser.pas',
  URoutineDoc in 'URoutineDoc.pas',
  URoutineExtraHelper in 'URoutineExtraHelper.pas',
  URoutineHTML in 'URoutineHTML.pas',
  URSS20 in 'URSS20.pas',
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
  UStatusBarMgr in 'UStatusBarMgr.pas',
  UStrStreamWriter in 'UStrStreamWriter.pas',
  UStructs in 'UStructs.pas',
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
  UToolButtonEx in 'UToolButtonEx.pas',
  UTVCheckBoxes in 'UTVCheckBoxes.pas',
  UUIWidgetImages in 'UUIWidgetImages.pas',
  UUniqueID in 'UUniqueID.pas',
  UUnitsChkListMgr in 'UUnitsChkListMgr.pas',
  UUpdateMgr in 'UUpdateMgr.pas',
  UURIEncode in 'UURIEncode.pas',
  UURIParams in 'UURIParams.pas',
  UUserDBBackup in 'UUserDBBackup.pas',
  UUserDBMgr in 'UUserDBMgr.pas',
  UUserDetails in 'UUserDetails.pas',
  UUtils in 'UUtils.pas',
  UVersionInfo in 'UVersionInfo.pas',
  UView in 'UView.pas',
  UViewItemAction in 'UViewItemAction.pas',
  UViewItemTreeNode in 'UViewItemTreeNode.pas',
  UWaitForThreadUI in 'UWaitForThreadUI.pas',
  UWarnings in 'UWarnings.pas',
  UWBCommandBars in 'UWBCommandBars.pas',
  UWBExternal in 'UWBExternal.pas',
  UWBPopupMenus in 'UWBPopupMenus.pas',
  UWindowSettings in 'UWindowSettings.pas',
  UXMLDataIO in 'UXMLDataIO.pas',
  UXMLDocConsts in 'UXMLDocConsts.pas',
  UXMLDocHelper in 'UXMLDocHelper.pas',
  UXMLDocumentEx in 'UXMLDocumentEx.pas',
  Web.UBaseWebService in 'Web.UBaseWebService.pas',
  Web.UCharEncodings in 'Web.UCharEncodings.pas',
  Web.UCodeSubmitter in 'Web.UCodeSubmitter.pas',
  Web.UDBDownloadMgr in 'Web.UDBDownloadMgr.pas',
  Web.UDownloadMonitor in 'Web.UDownloadMonitor.pas',
  Web.UExceptions in 'Web.UExceptions.pas',
  Web.UHTTPEx in 'Web.UHTTPEx.pas',
  Web.UInfo in 'Web.UInfo.pas',
  Web.URegistrar in 'Web.URegistrar.pas',
  Web.UStdWebService in 'Web.UStdWebService.pas',
  Web.UXMLRequestor in 'Web.UXMLRequestor.pas';

// Include compiler defines: checks if compiler supported
{$Include CompilerDefines.inc}

// Include resources
{$Resource ExternalObj.tlb} // Type library file
{$Resource HTML.res}        // HTML resources
{$Resource VCodeSnip.res}   // Version information resource
{$Resource Resources.res}   // All other resources.

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.Initialize;
  Application.MainFormOnTaskBar := True;
  SplashForm := TSplashForm.Create(Application);
  SplashForm.Show;
  Application.ModalPopupMode := pmAuto;
  Application.ProcessMessages;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

