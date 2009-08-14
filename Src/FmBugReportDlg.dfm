inherited BugReportDlg: TBugReportDlg
  Caption = 'BugReportDlg'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 457
    ExplicitWidth = 457
    inherited pnlHead: TPanel
      Width = 457
      ExplicitWidth = 457
      object imgBug: TImage
        Left = 435
        Top = 9
        Width = 16
        Height = 16
        Picture.Data = {
          07544269746D617036040000424D360400000000000036000000280000001000
          000010000000010020000000000000040000C40E0000C40E0000000000000000
          0000FEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFE
          FEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFE
          FEFFFFFFFFFFFFFFFFFF7F7F7FFFBFBFBFFFFFFFFFFFFFFFFFFF5FDCE2FF0000
          80FF000080FF5FDCE2FFFFFFFFFFFFFFFFFFBFBFBFFF7F7F7FFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF7F7F7FFFFFFFFFFFFFFFFFFF5FDCE2FF000080FF6BE8
          FFFF6BE8FFFF000080FF5FDCE2FFFFFFFFFFFFFFFFFF7F7F7FFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF7F7F7FFF7F7F7FFFFFFFFFFF000080FF6BE8FFFF6060
          A0FF6060A0FF6BE8FFFF000080FFFFFFFFFF7F7F7FFF7F7F7FFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF3F3F3FFF3F3F3FFF000080FF6BE8FFFFAFFF
          FFFFAFFFFFFF6BE8FFFF000080FF3F3F3FFF3F3F3FFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3F3F3FFF000020FF6BE8FFFFA1EA
          EAFFA1EAEAFF6BE8FFFF000020FF3F3F3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3F3F3FFF000040FFA1EA
          EAFFA1EAEAFF000040FF3F3F3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF7F7F7FFFBFBFBFFFFFFFFFFFFFFFFFFFFFFFFFFF000080FFA1EA
          EAFFA1EAEAFF000080FFFFFFFFFFFFFFFFFFFFFFFFFFBFBFBFFF7F7F7FFFFFFF
          FFFFFFFFFFFFFFFFFFFF3F3F3FFFBFBFBFFF3F3F3FFF303050FF000040FF60A0
          A0FF60A0A0FF000040FF3F3F5FFF3F3F3FFFBFBFBFFF3F3F3FFFFFFFFFFFFFFF
          FFFFFFFFFFFF7F7F7FFFBFBFBFFF3F3F3FFFBFBFBFFF3F3F5FFF000040FFA1EA
          EAFFA1EAEAFF000040FF3F3F5FFFBFBFBFFF3F3F3FFFBFBFBFFF7F7F7FFFFFFF
          FFFFFFFFFFFFFFFFFFFF3F3F3FFFBFBFBFFF3F3F3FFF7F7F7FFF000080FFA1EA
          EAFFA1EAEAFF000080FF7F7F9FFF3F3F3FFFBFBFBFFF3F3F3FFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF3F3F3FFFBFBFBFFFFFFFFFFF7F7F7FFF0000
          40FF000040FF7F7F7FFFFFFFFFFFBFBFBFFF3F3F3FFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFBFBFFF000020FF0000
          80FF000080FF000020FFBFBFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3F7F7FFF7FFFFFFF0000
          80FF000080FF7FFFFFFF3F7F7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF7F7F7FFF3F3F3FFFBFBFBFFF3F3F3FFF7F7F
          7FFF7F7F7FFF3F3F3FFFBFBFBFFF3F3F3FFF7F7F7FFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFBFBFFF3F3F3FFF3F3F3FFFFFFF
          FFFFFFFFFFFF3F3F3FFF3F3F3FFFBFBFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF}
        Transparent = True
      end
    end
    inherited pcWizard: TPageControl
      Width = 457
      ActivePage = tsUserInfo
      ExplicitWidth = 457
      object tsIntroBug: TTabSheet
        Caption = 'tsIntroBug'
        TabVisible = False
        object lblHeading: TLabel
          Left = 0
          Top = 8
          Width = 449
          Height = 16
          AutoSize = False
          Caption = 'lblHeading'
          Layout = tlCenter
          WordWrap = True
        end
        object bvlBugDesc: TBevel
          Left = 0
          Top = 34
          Width = 449
          Height = 34
          Shape = bsFrame
        end
        object lblBugMarker: TLabel
          Left = 1
          Top = 38
          Width = 12
          Height = 11
          Caption = '8'
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Marlett'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblBugInfo: TLabel
          Left = 18
          Top = 38
          Width = 423
          Height = 27
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clPurple
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object lblExceptIntro: TLabel
          Left = 0
          Top = 83
          Width = 449
          Height = 65
          AutoSize = False
          Caption = 
            'It will be helpful if you can take the time to report this bug. ' +
            'You can do this by filling in the information in this wizard.'
          WordWrap = True
        end
        object lblBugBegin: TLabel
          Left = 0
          Top = 168
          Width = 143
          Height = 13
          Caption = 'Click the Next button to begin.'
        end
      end
      object tsIntroUser: TTabSheet
        Caption = 'tsIntroUser'
        ImageIndex = 1
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblUserIntro: TLabel
          Left = 0
          Top = 8
          Width = 449
          Height = 57
          AutoSize = False
          Caption = 
            'Thanks for taking the time to report a bug. This wizard collects' +
            ' information about the bug and sends it to the author.'
          WordWrap = True
        end
        object lblUserBegin: TLabel
          Left = 0
          Top = 104
          Width = 143
          Height = 13
          Caption = 'Click the Next button to begin.'
        end
      end
      object tsBugInfo: TTabSheet
        Caption = 'tsBugInfo'
        ImageIndex = 2
        TabVisible = False
        object lblDesc: TLabel
          Left = 0
          Top = 8
          Width = 449
          Height = 13
          AutoSize = False
          Caption = 
            'Please provide a &description of the bug and what you were doing' +
            ' when it happened.'
          FocusControl = memoDesc
          WordWrap = True
        end
        object memoDesc: TMemo
          Left = 0
          Top = 24
          Width = 449
          Height = 214
          BevelInner = bvSpace
          BevelKind = bkFlat
          BevelOuter = bvRaised
          BorderStyle = bsNone
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
      object tsUserInfo: TTabSheet
        Caption = 'tsUserInfo'
        ImageIndex = 3
        TabVisible = False
        object lblEmailRequest: TLabel
          Left = 0
          Top = 8
          Width = 449
          Height = 30
          AutoSize = False
          Caption = 
            'If you wish to get information from the author about the bug, pl' +
            'ease provide your email address below.'
          WordWrap = True
        end
        object lblEmail: TLabel
          Left = 0
          Top = 48
          Width = 68
          Height = 13
          Caption = '&Email address:'
        end
        object lblOS: TLabel
          Left = 0
          Top = 112
          Width = 449
          Height = 13
          AutoSize = False
          Caption = 
            'The following details of your operating system will also be repo' +
            'rted:'
          WordWrap = True
        end
        object edEmail: TEdit
          Left = 72
          Top = 44
          Width = 297
          Height = 21
          BevelInner = bvSpace
          BevelKind = bkFlat
          BevelOuter = bvRaised
          BorderStyle = bsNone
          TabOrder = 0
        end
        object edOS: TEdit
          Left = 0
          Top = 128
          Width = 449
          Height = 21
          TabStop = False
          BevelInner = bvSpace
          BevelKind = bkFlat
          BevelOuter = bvRaised
          BorderStyle = bsNone
          ParentColor = True
          ReadOnly = True
          TabOrder = 1
        end
        inline frmPrivacy: TFixedHTMLDlgFrame
          Left = 0
          Top = 77
          Width = 449
          Height = 23
          TabOrder = 2
          TabStop = True
          ExplicitTop = 77
          ExplicitWidth = 449
          ExplicitHeight = 23
          inherited pnlBrowser: TPanel
            Width = 449
            Height = 23
            ExplicitWidth = 449
            ExplicitHeight = 23
            inherited wbBrowser: TWebBrowser
              Width = 449
              Height = 23
              ExplicitWidth = 17
              ExplicitHeight = 23
              ControlData = {
                4C000000682E0000610200000000000000000000000000000000000000000000
                000000004C000000000000000000000001000000E0D057007335CF11AE690800
                2B2E126208000000000000004C0000000114020000000000C000000000000046
                8000000000000000000000000000000000000000000000000000000000000000
                00000000000000000100000000000000000000000000000000000000}
            end
          end
        end
      end
      object tsSubmit: TTabSheet
        Caption = 'tsSubmit'
        ImageIndex = 4
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblReport: TLabel
          Left = 0
          Top = 8
          Width = 449
          Height = 13
          AutoSize = False
          Caption = 
            'You are now ready to submit the bug report. Here is the informat' +
            'ion that will be sent:'
          WordWrap = True
        end
        object lblSubmit: TLabel
          Left = 0
          Top = 192
          Width = 449
          Height = 27
          AutoSize = False
          Caption = 
            'Please ensure you are connected to the internet and then click t' +
            'he Submit button to send the bug report.'
          WordWrap = True
        end
        object edReport: TMemo
          Left = 0
          Top = 32
          Width = 449
          Height = 153
          TabStop = False
          BevelInner = bvSpace
          BevelKind = bkFlat
          BevelOuter = bvRaised
          BorderStyle = bsNone
          ParentColor = True
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
      object tsDone: TTabSheet
        Caption = 'tsDone'
        ImageIndex = 5
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblDone: TLabel
          Left = 0
          Top = 8
          Width = 449
          Height = 13
          AutoSize = False
          Caption = 'The bug report has been submitted successfully - thank you.'
          WordWrap = True
        end
      end
    end
  end
end
