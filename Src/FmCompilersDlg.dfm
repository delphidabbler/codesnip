inherited CompilersDlg: TCompilersDlg
  Caption = 'Configure Compilers'
  ClientHeight = 381
  ClientWidth = 588
  ExplicitWidth = 594
  ExplicitHeight = 410
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 539
    ExplicitWidth = 539
    object pbBanner: TPaintBox
      Left = 169
      Top = 0
      Width = 370
      Height = 23
      Color = clActiveCaption
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clCaptionText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object lbCompilers: TListBox
      Left = 0
      Top = 0
      Width = 163
      Height = 292
      Style = lbOwnerDrawFixed
      Ctl3D = True
      IntegralHeight = True
      ItemHeight = 36
      ParentCtl3D = False
      TabOrder = 0
    end
    object pcCompiler: TPageControl
      Left = 169
      Top = 29
      Width = 370
      Height = 263
      ActivePage = tsCompiler
      TabOrder = 1
      OnMouseDown = pcCompilerMouseDown
      object tsCompiler: TTabSheet
        Caption = 'Compiler'
        inline frmCompiler: TCompilersDlgCompilerFrame
          Left = 0
          Top = 0
          Width = 362
          Height = 235
          Align = alClient
          TabOrder = 0
        end
      end
      object tsSwitches: TTabSheet
        Caption = 'Switches'
        ImageIndex = 2
        inline frmSwitches: TCompilersDlgSwitchesFrame
          Left = 0
          Top = 0
          Width = 362
          Height = 235
          Align = alClient
          TabOrder = 0
          inherited btnDefSwitches: TButton
            ExplicitLeft = 293
          end
          inherited btnAdd: TButton
            ExplicitLeft = 222
          end
          inherited btnReplace: TButton
            ExplicitLeft = 293
          end
          inherited btnDelete: TButton
            ExplicitLeft = 222
          end
        end
      end
      object tsNamespaces: TTabSheet
        Caption = 'Namespaces'
        ImageIndex = 4
        inline frmNamespaces: TCompilersDlgNamespacesFrame
          Left = 0
          Top = 0
          Width = 362
          Height = 235
          Align = alClient
          TabOrder = 0
          inherited btnDefSwitches: TButton
            ExplicitLeft = 293
          end
          inherited btnAdd: TButton
            ExplicitLeft = 222
          end
          inherited btnReplace: TButton
            ExplicitLeft = 293
          end
          inherited btnDelete: TButton
            ExplicitLeft = 222
          end
        end
      end
      object tsSearchDirs: TTabSheet
        Caption = 'Search Paths'
        ImageIndex = 3
        inline frmSearchDirs: TCompilersDlgSearchDirsFrame
          Left = 0
          Top = 0
          Width = 362
          Height = 235
          Align = alClient
          TabOrder = 0
          inherited edPath: TEdit
            ExplicitWidth = 322
          end
          inherited btnBrowse: TButton
            ExplicitLeft = 331
          end
        end
      end
      object tsLog: TTabSheet
        Caption = 'Output Log'
        ImageIndex = 1
        inline frmLog: TCompilersDlgLogFrame
          Left = 0
          Top = 0
          Width = 362
          Height = 235
          Align = alClient
          TabOrder = 0
          inherited vleLogPrefixes: TValueListEditor
            ExplicitWidth = 354
          end
        end
      end
    end
  end
  inherited btnHelp: TButton
    TabOrder = 4
  end
  inherited btnCancel: TButton
    TabOrder = 3
  end
  inherited btnOK: TButton
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnDetect: TButton
    Left = 8
    Top = 304
    Width = 153
    Height = 25
    Caption = '&Detect Delphi Compilers'
    TabOrder = 1
    OnClick = btnDetectClick
  end
end
