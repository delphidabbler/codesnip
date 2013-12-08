{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * CodeSnip application project file.
}


program CodeSnip;

{$Include CompilerChecks.inc}

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
  Browser.IntfDocHostUI in 'Browser.IntfDocHostUI.pas',
  Browser.UControlHelper in 'Browser.UControlHelper.pas',
  Browser.UController in 'Browser.UController.pas',
  Browser.UHighlighter in 'Browser.UHighlighter.pas',
  Browser.UHTMLEvents in 'Browser.UHTMLEvents.pas',
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
  Compilers.USearchDirs in 'Compilers.USearchDirs.pas',
  DB.UCategory in 'DB.UCategory.pas',
  DB.UDatabaseIO in 'DB.UDatabaseIO.pas',
  DB.UMain in 'DB.UMain.pas',
  DB.USnippet in 'DB.USnippet.pas',
  DB.USnippetKind in 'DB.USnippetKind.pas',
  DBIO.UFileIOIntf in 'DBIO.UFileIOIntf.pas',
  DBIO.UNulDataReader in 'DBIO.UNulDataReader.pas',
  DBIO.UXMLDataIO in 'DBIO.UXMLDataIO.pas',
  Favourites.UManager in 'Favourites.UManager.pas',
  Favourites.UFavourites in 'Favourites.UFavourites.pas',
  Favourites.UPersist in 'Favourites.UPersist.pas',
  FirstRun.FmV4ConfigDlg in 'FirstRun.FmV4ConfigDlg.pas' {V4ConfigDlg},
  FirstRun.UConfigFile in 'FirstRun.UConfigFile.pas',
  FirstRun.UDatabase in 'FirstRun.UDatabase.pas',
  FirstRun.UIniFile in 'FirstRun.UIniFile.pas',
  FirstRun.UInstallInfo in 'FirstRun.UInstallInfo.pas',
  FirstRun.UMain in 'FirstRun.UMain.pas',
  FmAboutDlg in 'FmAboutDlg.pas' {AboutDlg},
  FmActiveTextPreviewDlg in 'FmActiveTextPreviewDlg.pas' {ActiveTextPreviewDlg},
  FmBase in 'FmBase.pas' {BaseForm},
  FmBugReportBaseDlg in 'FmBugReportBaseDlg.pas' {BugReportBaseDlg},
  FmCodeExportDlg in 'FmCodeExportDlg.pas' {CodeExportDlg},
  FmCodeImportDlg in 'FmCodeImportDlg.pas' {CodeImportDlg},
  FmCodeSubmitDlg in 'FmCodeSubmitDlg.pas' {CodeSubmitDlg},
  FmCompErrorDlg in 'FmCompErrorDlg.pas' {CompErrorDlg},
  FmCompilersDlg in 'FmCompilersDlg.pas' {CompilersDlg},
  FmCompilersDlg.FrBase in 'FmCompilersDlg.FrBase.pas' {CompilersDlgBaseFrame: TFrame},
  FmCompilersDlg.FrCompiler in 'FmCompilersDlg.FrCompiler.pas' {CompilersDlgCompilerFrame: TFrame},
  FmCompilersDlg.FrLog in 'FmCompilersDlg.FrLog.pas' {CompilersDlgLogFrame: TFrame},
  FmCompilersDlg.FrNamespaces in 'FmCompilersDlg.FrNamespaces.pas' {CompilersDlgNamespacesFrame: TFrame},
  FmCompilersDlg.FrSearchDirs in 'FmCompilersDlg.FrSearchDirs.pas' {CompilersDlgSearchDirsFrame: TFrame},
  FmCompilersDlg.FrSwitches in 'FmCompilersDlg.FrSwitches.pas' {CompilersDlgSwitchesFrame: TFrame},
  FmCompilersDlg.UBannerMgr in 'FmCompilersDlg.UBannerMgr.pas',
  FmCompilersDlg.UCompilerListMgr in 'FmCompilersDlg.UCompilerListMgr.pas',
  FmDependenciesDlg in 'FmDependenciesDlg.pas' {DependenciesDlg},
  FmDonateDlg in 'FmDonateDlg.pas' {DonateDlg},
  FmDuplicateSnippetDlg in 'FmDuplicateSnippetDlg.pas' {DuplicateSnippetDlg},
  FmEasterEgg in 'FmEasterEgg.pas' {EasterEggForm},
  FmFavouritesDlg in 'FmFavouritesDlg.pas' {FavouritesDlg},
  FmFindCompilerDlg in 'FmFindCompilerDlg.pas' {FindCompilerDlg},
  FmFindTextDlg in 'FmFindTextDlg.pas' {FindTextDlg},
  FmFindXRefsDlg in 'FmFindXRefsDlg.pas' {FindXRefsDlg},
  FmGenericDlg in 'FmGenericDlg.pas' {GenericDlg},
  FmGenericModalDlg in 'FmGenericModalDlg.pas' {GenericModalDlg},
  FmGenericNonModalDlg in 'FmGenericNonModalDlg.pas' {GenericNonModalDlg},
  FmGenericOKDlg in 'FmGenericOKDlg.pas' {GenericOKDlg},
  FmGenericViewDlg in 'FmGenericViewDlg.pas' {GenericViewDlg},
  FmHelpAware in 'FmHelpAware.pas' {HelpAwareForm},
  FmMain in 'FmMain.pas' {MainForm},
  FmNewsDlg in 'FmNewsDlg.pas' {NewsDlg},
  FmPreferencesDlg in 'FmPreferencesDlg.pas' {PreferencesDlg},
  FmPreviewDlg in 'FmPreviewDlg.pas' {PreviewDlg},
  FmPrintDlg in 'FmPrintDlg.pas' {PrintDlg},
  FmProgramUpdatesDlg in 'FmProgramUpdatesDlg.pas' {ProgramUpdatesDlg},
  FmProxyServerDlg in 'FmProxyServerDlg.pas' {ProxyServerDlg},
  FmRegistrationDlg in 'FmRegistrationDlg.pas' {RegistrationDlg},
  FmSelectionSearchDlg in 'FmSelectionSearchDlg.pas' {SelectionSearchDlg},
  FmSnippetsEditorDlg in 'FmSnippetsEditorDlg.pas' {SnippetsEditorDlg},
  FmSnippetsEditorDlg.FrActiveTextEditor in 'FmSnippetsEditorDlg.FrActiveTextEditor.pas' {SnippetsActiveTextEdFrame: TFrame},
  FmSplash in 'FmSplash.pas' {SplashForm},
  FmSWAGImportDlg in 'FmSWAGImportDlg.pas' {SWAGImportDlg},
  FmTestCompileDlg in 'FmTestCompileDlg.pas' {TestCompileDlg},
  FmTrappedBugReportDlg in 'FmTrappedBugReportDlg.pas' {TrappedBugReportDlg},
  FmUserBugReportDlg in 'FmUserBugReportDlg.pas' {UserBugReportDlg},
  FmUserDataPathDlg in 'FmUserDataPathDlg.pas' {UserDataPathDlg},
  FmUserDataPathDlg.FrProgress in 'FmUserDataPathDlg.FrProgress.pas' {UserDataPathDlgProgressFrame: TFrame},
  FmWaitDlg in 'FmWaitDlg.pas' {WaitDlg},
  FmWizardDlg in 'FmWizardDlg.pas' {WizardDlg},
  FrBrowserBase in 'FrBrowserBase.pas' {BrowserBaseFrame: TFrame},
  FrCodeGenPrefs in 'FrCodeGenPrefs.pas' {CodeGenPrefsFrame: TFrame},
  FrDetail in 'FrDetail.pas' {DetailFrame: TFrame},
  FrDetailView in 'FrDetailView.pas' {DetailViewFrame: TFrame},
  FrDisplayPrefs in 'FrDisplayPrefs.pas' {DisplayPrefsFrame: TFrame},
  FrEasterEgg in 'FrEasterEgg.pas' {EasterEggFrame: TFrame},
  FrFixedHTMLDlg in 'FrFixedHTMLDlg.pas' {FixedHTMLDlgFrame: TFrame},
  FrGeneralPrefs in 'FrGeneralPrefs.pas' {GeneralPrefsFrame: TFrame},
  FrHiliterPrefs in 'FrHiliterPrefs.pas' {HiliterPrefsFrame: TFrame},
  FrHTMLDlg in 'FrHTMLDlg.pas' {HTMLDlgFrame: TFrame},
  FrHTMLPreview in 'FrHTMLPreview.pas' {HTMLPreviewFrame: TFrame},
  FrHTMLTpltDlg in 'FrHTMLTpltDlg.pas' {HTMLTpltDlgFrame: TFrame},
  FrMemoPreview in 'FrMemoPreview.pas' {MemoPreviewFrame: TFrame},
  FrNewsPrefs in 'FrNewsPrefs.pas' {NewsPrefsFrame: TFrame},
  FrOverview in 'FrOverview.pas' {OverviewFrame: TFrame},
  FrPrefsBase in 'FrPrefsBase.pas' {PrefsBaseFrame: TFrame},
  FrRSSNews in 'FrRSSNews.pas' {RSSNewsFrame: TFrame},
  FrRTFPreview in 'FrRTFPreview.pas' {RTFPreviewFrame: TFrame},
  FrRTFShowCase in 'FrRTFShowCase.pas' {RTFShowCaseFrame: TFrame},
  FrCheckedTV in 'FrCheckedTV.pas' {CheckedTVFrame: TFrame},
  FrSelectSnippets in 'FrSelectSnippets.pas' {SelectSnippetsFrame: TFrame},
  FrSnippetLayoutPrefs in 'FrSnippetLayoutPrefs.pas' {SnippetLayoutPrefsFrame: TFrame},
  FrSourcePrefs in 'FrSourcePrefs.pas' {SourcePrefsFrame: TFrame},
  FrTextPreview in 'FrTextPreview.pas' {TextPreviewFrame: TFrame},
  FrTitled in 'FrTitled.pas' {TitledFrame: TFrame},
  FrUpdatePrefs in 'FrUpdatePrefs.pas' {UpdatePrefsFrame: TFrame},
  IntfAligner in 'IntfAligner.pas',
  IntfCommon in 'IntfCommon.pas',
  IntfFrameMgrs in 'IntfFrameMgrs.pas',
  IntfNotifier in 'IntfNotifier.pas',
  IntfPreview in 'IntfPreview.pas',
  Notifications.UDisplayMgr in 'Notifications.UDisplayMgr.pas',
  Notifications.UData in 'Notifications.UData.pas',
  Notifications.UDisplayThread in 'Notifications.UDisplayThread.pas',
  Notifications.UQueue in 'Notifications.UQueue.pas',
  Notifications.URecorderThread in 'Notifications.URecorderThread.pas',
  Notifications.UWindow in 'Notifications.UWindow.pas',
  SWAG.UCommon in 'SWAG.UCommon.pas',
  SWAG.UImporter in 'SWAG.UImporter.pas',
  SWAG.UReader in 'SWAG.UReader.pas',
  SWAG.USnippetCache in 'SWAG.USnippetCache.pas',
  UActionFactory in 'UActionFactory.pas',
  UAnchors in 'UAnchors.pas',
  UAppInfo in 'UAppInfo.pas',
  UBaseObjects in 'UBaseObjects.pas',
  UBox in 'UBox.pas',
  UBrowseForFolderDlg in 'UBrowseForFolderDlg.pas',
  UBrowseProtocol in 'UBrowseProtocol.pas',
  UCategoryListAdapter in 'UCategoryListAdapter.pas',
  UClassHelpers in 'UClassHelpers.pas',
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
  UCompResHTML in 'UCompResHTML.pas',
  UConsoleApp in 'UConsoleApp.pas',
  UConsts in 'UConsts.pas',
  UContainers in 'UContainers.pas',
  UControlStateMgr in 'UControlStateMgr.pas',
  UCopyInfoMgr in 'UCopyInfoMgr.pas',
  UCopySourceMgr in 'UCopySourceMgr.pas',
  UCopyViewMgr in 'UCopyViewMgr.pas',
  UCSSBuilder in 'UCSSBuilder.pas',
  UCSSUtils in 'UCSSUtils.pas',
  UCtrlArranger in 'UCtrlArranger.pas',
  UDatabaseLoader in 'UDatabaseLoader.pas',
  UDatabaseLoaderUI in 'UDatabaseLoaderUI.pas',
  UDataStreamIO in 'UDataStreamIO.pas',
  UDetailPageHTML in 'UDetailPageHTML.pas',
  UDetailPageLoader in 'UDetailPageLoader.pas',
  UDetailTabAction in 'UDetailTabAction.pas',
  UDialogMgr in 'UDialogMgr.pas',
  UDispatchList in 'UDispatchList.pas',
  UDlgHelper in 'UDlgHelper.pas',
  UDOSDateTime in 'UDOSDateTime.pas',
  UDropDownButtons in 'UDropDownButtons.pas',
  UEditSnippetAction in 'UEditSnippetAction.pas',
  UEmailHelper in 'UEmailHelper.pas',
  UEncodings in 'UEncodings.pas',
  UEncryptor in 'UEncryptor.pas',
  UExceptions in 'UExceptions.pas',
  UExeFileType in 'UExeFileType.pas',
  UFileProtocol in 'UFileProtocol.pas',
  UFolderBackup in 'UFolderBackup.pas',
  UFontHelper in 'UFontHelper.pas',
  UFormAligner in 'UFormAligner.pas',
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
  UHTMLDOMHelper in 'UHTMLDOMHelper.pas',
  UHTMLHelpMgr in 'UHTMLHelpMgr.pas',
  UHTMLTemplate in 'UHTMLTemplate.pas',
  UHTMLUtils in 'UHTMLUtils.pas',
  UHTTPProtocol in 'UHTTPProtocol.pas',
  UImageTags in 'UImageTags.pas',
  UInitialLetter in 'UInitialLetter.pas',
  UIOUtils in 'UIOUtils.pas',
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
  UMenus in 'UMenus.pas',
  UMessageBox in 'UMessageBox.pas',
  UMessageWindow in 'UMessageWindow.pas',
  UMultiCastEvents in 'UMultiCastEvents.pas',
  UNotifier in 'UNotifier.pas',
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
  UProgramUpdateChecker in 'UProgramUpdateChecker.pas',
  UProtocols in 'UProtocols.pas',
  UQuery in 'UQuery.pas',
  UREMLDataIO in 'UREMLDataIO.pas',
  UReservedCategories in 'UReservedCategories.pas',
  UResourceUtils in 'UResourceUtils.pas',
  URFC2822Date in 'URFC2822Date.pas',
  URSS20 in 'URSS20.pas',
  URTFBuilder in 'URTFBuilder.pas',
  URTFSnippetDoc in 'URTFSnippetDoc.pas',
  URTFStyles in 'URTFStyles.pas',
  URTFUtils in 'URTFUtils.pas',
  USaveDialogEx in 'USaveDialogEx.pas',
  USaveSnippetMgr in 'USaveSnippetMgr.pas',
  USaveSourceDlg in 'USaveSourceDlg.pas',
  USaveSourceMgr in 'USaveSourceMgr.pas',
  USaveUnitMgr in 'USaveUnitMgr.pas',
  USearch in 'USearch.pas',
  USelectionIOMgr in 'USelectionIOMgr.pas',
  USettings in 'USettings.pas',
  UShowCaseCtrl in 'UShowCaseCtrl.pas',
  UShowPrefsPageAction in 'UShowPrefsPageAction.pas',
  USimpleDispatch in 'USimpleDispatch.pas',
  USingleton in 'USingleton.pas',
  USnippetAction in 'USnippetAction.pas',
  USnippetDoc in 'USnippetDoc.pas',
  USnippetHTML in 'USnippetHTML.pas',
  USnippetIDListIOHandler in 'USnippetIDListIOHandler.pas',
  USnippetPageHTML in 'USnippetPageHTML.pas',
  USnippetPageStructure in 'USnippetPageStructure.pas',
  USnippetsChkListMgr in 'USnippetsChkListMgr.pas',
  USnippetSourceGen in 'USnippetSourceGen.pas',
  USnippetsTVDraw in 'USnippetsTVDraw.pas',
  USnippetValidator in 'USnippetValidator.pas',
  USourceFileInfo in 'USourceFileInfo.pas',
  UStartUp in 'UStartUp.pas',
  UStatusBarMgr in 'UStatusBarMgr.pas',
  UStringReader in 'UStringReader.pas',
  UStructs in 'UStructs.pas',
  UStrUtils in 'UStrUtils.pas',
  USystemID in 'USystemID.pas',
  USystemInfo in 'USystemInfo.pas',
  UTaggedTextLexer in 'UTaggedTextLexer.pas',
  UTestCompile in 'UTestCompile.pas',
  UTestCompileUI in 'UTestCompileUI.pas',
  UTestUnitDlgMgr in 'UTestUnitDlgMgr.pas',
  UTextSnippetDoc in 'UTextSnippetDoc.pas',
  UThemesEx in 'UThemesEx.pas',
  UThreadGroup in 'UThreadGroup.pas',
  UToolButtonEx in 'UToolButtonEx.pas',
  UTVCheckBoxes in 'UTVCheckBoxes.pas',
  UUIWidgetImages in 'UUIWidgetImages.pas',
  UUniqueID in 'UUniqueID.pas',
  UUnitsChkListMgr in 'UUnitsChkListMgr.pas',
  UUpdateCheckers in 'UUpdateCheckers.pas',
  UURIEncode in 'UURIEncode.pas',
  UURIParams in 'UURIParams.pas',
  UUrlMonEx in 'UUrlMonEx.pas',
  UUserDBBackup in 'UUserDBBackup.pas',
  UUserDBMgr in 'UUserDBMgr.pas',
  UUserDBMove in 'UUserDBMove.pas',
  UUserDetails in 'UUserDetails.pas',
  UUserDetailsPersist in 'UUserDetailsPersist.pas',
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
  UXMLDocConsts in 'UXMLDocConsts.pas',
  UXMLDocHelper in 'UXMLDocHelper.pas',
  UXMLDocumentEx in 'UXMLDocumentEx.pas',
  Web.UBaseWebService in 'Web.UBaseWebService.pas',
  Web.UCharEncodings in 'Web.UCharEncodings.pas',
  Web.UCodeSubmitter in 'Web.UCodeSubmitter.pas',
  Web.UDownloadMonitor in 'Web.UDownloadMonitor.pas',
  Web.UExceptions in 'Web.UExceptions.pas',
  Web.UHTTPEx in 'Web.UHTTPEx.pas',
  Web.UInfo in 'Web.UInfo.pas',
  Web.UProgramUpdateMgr in 'Web.UProgramUpdateMgr.pas',
  Web.URegistrar in 'Web.URegistrar.pas',
  Web.UStdWebService in 'Web.UStdWebService.pas',
  Web.USWAGRESTMgr in 'Web.USWAGRESTMgr.pas',
  Web.UXMLRequestor in 'Web.UXMLRequestor.pas',
  IntfExternalObj in 'Main\AutoGen\IntfExternalObj.pas',
  GIFImage in 'Main\Imports\GIFImage.pas',
  LVEx in 'Main\Imports\LVEx.pas',
  UEncrypt in 'Main\Imports\UEncrypt.pas',
  CS.Actions.DisplayTag in 'Main\CS.Actions.DisplayTag.pas',
  CS.Actions.RemoveTag in 'Main\CS.Actions.RemoveTag.pas',
  CS.ActiveText in 'Main\CS.ActiveText.pas',
  CS.ActiveText.Helper in 'Main\CS.ActiveText.Helper.pas',
  CS.ActiveText.Parsers.Credits in 'Main\CS.ActiveText.Parsers.Credits.pas',
  CS.ActiveText.Parsers.PlainText in 'Main\CS.ActiveText.Parsers.PlainText.pas',
  CS.ActiveText.Parsers.REML in 'Main\CS.ActiveText.Parsers.REML.pas',
  CS.ActiveText.Renderers.HTML in 'Main\CS.ActiveText.Renderers.HTML.pas',
  CS.ActiveText.Renderers.PlainText in 'Main\CS.ActiveText.Renderers.PlainText.pas',
  CS.ActiveText.Renderers.REML in 'Main\CS.ActiveText.Renderers.REML.pas',
  CS.ActiveText.Renderers.RTF in 'Main\CS.ActiveText.Renderers.RTF.pas',
  CS.ActiveText.Validator in 'Main\CS.ActiveText.Validator.pas',
  CS.Config in 'Main\CS.Config.pas',
  CS.Docs.TagInfo.RTF in 'Main\CS.Docs.TagInfo.RTF.pas',
  CS.Database.IO.Legacy in 'Main\CS.Database.IO.Legacy.pas',
  CS.Database.IO.Native in 'Main\CS.Database.IO.Native.pas',
  CS.Database.SnippetLinks in 'Main\CS.Database.SnippetLinks.pas',
  CS.Database.Snippets in 'Main\CS.Database.Snippets.pas',
  CS.Database.SnippetsTable in 'Main\CS.Database.SnippetsTable.pas',
  CS.Database.Tags in 'Main\CS.Database.Tags.pas',
  CS.Database.Types in 'Main\CS.Database.Types.pas',
  CS.ExternalProgs.DiffViewer in 'Main\CS.ExternalProgs.DiffViewer.pas',
  CS.Init.CommandLineOpts in 'Main\CS.Init.CommandLineOpts.pas',
  CS.SourceCode.Hiliter.Brushes in 'Main\CS.SourceCode.Hiliter.Brushes.pas',
  CS.SourceCode.Hiliter.Parser in 'Main\CS.SourceCode.Hiliter.Parser.pas',
  CS.SourceCode.Hiliter.Renderers in 'Main\CS.SourceCode.Hiliter.Renderers.pas',
  CS.SourceCode.Hiliter.Renderers.CSS in 'Main\CS.SourceCode.Hiliter.Renderers.CSS.pas',
  CS.SourceCode.Hiliter.Themes in 'Main\CS.SourceCode.Hiliter.Themes.pas',
  CS.SourceCode.Hiliter.Themes.Persist in 'Main\CS.SourceCode.Hiliter.Themes.Persist.pas',
  CS.SourceCode.Languages in 'Main\CS.SourceCode.Languages.pas',
  CS.SourceCode.Languages.Persist in 'Main\CS.SourceCode.Languages.Persist.pas',
  CS.SourceCode.Pascal.Lexer in 'Main\CS.SourceCode.Pascal.Lexer.pas',
  CS.SourceCode.Pascal.SourceGen in 'Main\CS.SourceCode.Pascal.SourceGen.pas',
  CS.SourceCode.Pascal.TestUnit in 'Main\CS.SourceCode.Pascal.TestUnit.pas',
  CS.UI.Dialogs.ConfigDiffProg in 'Main\CS.UI.Dialogs.ConfigDiffProg.pas' {ConfigDiffProgDlg},
  CS.UI.Dialogs.HiliteThemeName in 'Main\CS.UI.Dialogs.HiliteThemeName.pas' {HiliteThemeNameDlg},
  CS.UI.Dialogs.HiliteThemesEditor in 'Main\CS.UI.Dialogs.HiliteThemesEditor.pas' {HiliteThemesEditorDlg},
  CS.UI.Helper.CollectionCtrlKVMgr in 'Main\CS.UI.Helper.CollectionCtrlKVMgr.pas',
  CS.UI.Frames.CodeEditor in 'Main\CS.UI.Frames.CodeEditor.pas' {CodeEditorFrame: TFrame},
  CS.Utils.Dates in 'Main\CS.Utils.Dates.pas',
  CS.Utils.Hashes in 'Main\CS.Utils.Hashes.pas',
  CS.Utils.Sound in 'Main\CS.Utils.Sound.pas';

// Include resources
{$Resource ExternalObj.tlb}       // Type library file
{$Resource HTML.res}              // HTML resources
{$Resource VCodeSnip.res}         // Version information resource (normal)
{$Resource Resources.res}         // All other resources.

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.Initialize;
  Application.MainFormOnTaskBar := True;
  if not TStartUp.Execute then
  begin
    Application.Terminate;
    Exit;
  end;
  SplashForm := TSplashForm.Create(Application);
  if not TCommandLineOpts.NoSplash then
    SplashForm.Show;
  Application.ModalPopupMode := pmAuto;
  Application.ProcessMessages;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

