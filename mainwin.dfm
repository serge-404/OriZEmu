€
 TFRMMAIN 0Б  TPF0TfrmMainfrmMainLeftп TopBorderIconsbiSystemMenu
biMinimize BorderStylebsSingleCaptionOrion/Z emulatorClientHeight3ClientWidth Color	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Heightх	Font.NameMS Sans Serif
Font.Style OldCreateOrder
OnActivateFormActivateOnClose	FormCloseOnCloseQueryFormCloseQueryOnCreate
FormCreate	OnDestroyFormDestroyOnResize
FormResizePixelsPerInch`
TextHeight TBevelBevel1Left TopWidth HeightAlignalTop  TPanelpnDbgLeft TopWidth HeightAlignalClient
BevelOuterbvNoneCaptionDebuggerTabOrderVisible TPanelPanel1Left Topµ Width HeightLAlignalClientCaptionPanel1TabOrder  TMemoMemDumpLeftTopEWidthюHeightAlignalClientFont.CharsetRUSSIAN_CHARSET
Font.ColorclWindowTextFont.Heightф	Font.NameCourier New
Font.PitchfpFixed
Font.Style 
ParentFontReadOnly	
ScrollBars
ssVerticalTabOrder WordWrap  TPanelPanel4LeftTopWidthюHeightDAlignalTopTabOrder TLabelLabel2Left€Top&Width™ HeightCaption"bytes beginning from address above  TLabelLabel1LeftTopWidthьHeightAlignalTop	AlignmenttaCenterCaptionMEMORYFont.CharsetDEFAULT_CHARSET
Font.ColorclPurpleFont.Heightн	Font.NameMS Sans Serif
Font.Style 
ParentFont  TBevelBevel2LeftTopWidthьHeightAlignalTop  	TMaskEdit
MEDumpAddrLeftvTop"Width2HeightEditMaskAA:AAAA;1;_	MaxLengthTabOrderText00:0000OnChangeMEDumpAddrChange  TButtonBtnModyByteLeft≤ Top!Width™ HeightCaptionModify bytes at this addressTabOrderOnClickBtnModyByteClick  TButton
BtnSaveMemLeftgTop!WidthQHeightCaptionSave to FileTabOrderOnClickBtnSaveMemClick  	TMaskEdit	MESaveCntLeftЊTop"Width:HeightEditMask
999999;1; 	MaxLengthTabOrderText0       TButtonbtnPageAddressLeftTop!WidthkHeightCaptionPage : Address	PopupMenuPageAddressMenuTabOrder OnClickbtnPageAddressClick    TPanelPanel2Left Top Width Heightµ AlignalTopTabOrder TLabelLabel5LeftTopWidthюHeightAlignalTop	AlignmenttaCenterCaptionC P U,  PortsFont.CharsetDEFAULT_CHARSET
Font.ColorclPurpleFont.Heightн	Font.NameMS Sans Serif
Font.Style 
ParentFont  TPanelPanel3LeftTopWidthюHeightЩ AlignalBottomBorderWidthBorderStylebsSingleTabOrder  TLabelLabel3LeftБTopWidthHeightCaption<-- PC  TLabelLabel4Left/TopWidth*HeightCaption	< Ports >  TStringGrid	SGRegMainLeftTopWidthiHeightП Hint@Right click for set conditions and modify register in pause modeAlignalLeftColCountDefaultColWidth<DefaultRowHeightRowCount	FixedRows ParentShowHint	PopupMenuDebuggerMenuShowHint	TabOrder 
RowHeights   TStringGrid
SGRegAlterLeftlTopWidthhHeightП Hint@Right click for set conditions and modify register in pause modeAlignalLeftColCountDefaultColWidth<DefaultRowHeightRowCount	FixedRows ParentShowHint	PopupMenuDebuggerMenuShowHint	TabOrder
RowHeights   TStringGrid	SGHistoryLeftЏ Top Width£ HeightOHintPrevious commandsColCountDefaultColWidthODefaultRowHeightRowCount	FixedRows ParentShowHintShowHint	TabOrder
RowHeights   	TCheckBoxcbBreakPointLeftё TopTWidthjHeightCaptionBreakPoint at ->TabOrderOnClickcbBreakPointClick  	TCheckBoxcbConditionsLeft” TopfWidthjHeight	AlignmenttaLeftJustifyCaption <- with conditionsTabOrderOnClickcbConditionsClick  	TMaskEditmeBreakPointLeftJTopUWidth2HeightEditMask
a:aaaa;1;_	MaxLengthTabOrderText :    OnChangecbBreakPointClick  TButtonBtnStepIntoLeftџ Top{WidthNHeightActionActDbgStepIntoParentShowHintShowHint	TabOrder  TButtonBtnStepOverLeft/Top{WidthNHeightActionActDbgStepOverParentShowHintShowHint	TabOrder  TStringGridSGFlagsLeftЂTop Width.HeightЙ HintPrevious commandsColCountDefaultColWidthDefaultRowHeightRowCount	FixedRows ParentShowHintShowHint	TabOrder
RowHeights   TStringGrid
SGPortDumpLeftиTop WidthDHeightК Hint$Right click for modify in pause modeColCountDefaultColWidth#DefaultRowHeightRowCount	FixedRows ParentShowHint	PopupMenuDebuggerMenuShowHint	TabOrder	
RowHeights   TStringGridsgPort1DumpLeft\TopWidthDHeightК Hint$Right click for modify in pause modeColCountDefaultColWidth#DefaultRowHeightRowCount	FixedRows ParentShowHint	PopupMenuDebuggerMenuShowHint	TabOrder

RowHeights   TStringGridsgPort2DumpLeftҐTopWidthCHeightК Hint&Right click for register in pause modeColCountDefaultColWidth#DefaultRowHeightRowCount	FixedRows ParentShowHint	PopupMenuDebuggerMenuShowHint	TabOrder
RowHeights      
TStatusBar	StatusBarLeft Top Width HeightPanelsBevelpbRaisedTextFDD:Width< BevelpbRaisedTextHDD:Width< BevelpbRaisedTextKB:Width< BevelpbRaisedTextCPU:WidthA BevelpbRaisedTextEth:WidthВ  Width2  SimplePanel  TToolBarToolBarLeft Top Width HeightButtonHeightImages
ImageList1TabOrder  TToolButtonToolButtonScrLeft TopActionActScrDown	ParentShowHintShowHint	  TToolButtonToolButton14LeftTopWidthCaptionToolButton14
ImageIndex
StyletbsSeparator  TToolButtonToolButtonDbgLeftTopActionActDbgParentShowHintShowHint	  TToolButtonToolButton13Left3TopWidthCaptionToolButton13
ImageIndex
StyletbsSeparator  TToolButtonToolButtonPauseLeftETopHint)Pause CPU (also keyboard key Pause/Break)CaptionPause
ImageIndex ParentShowHintShowHint	OnClickActPauseExecute  TToolButtonToolButton2Left\TopWidthCaptionToolButton2
ImageIndexStyletbsSeparator  TToolButtonToolButtonResetLeftaTopHint	Reset CPUCaptionReset
ImageIndexParentShowHintShowHint	OnClickActResetExecute  TToolButtonToolButton4LeftxTopWidthCaptionToolButton4
ImageIndexStyletbsSeparator  TToolButtonToolButtonZoomLeftК TopActionActZoomParentShowHintShowHint	  TToolButtonToolButton8Left° TopWidthCaptionToolButton8
ImageIndex
StyletbsSeparator  TToolButtonToolButtonSnapshotLeft¶ TopHintSave / Load  snapshot
ImageIndex
ParentShowHintShowHint	OnClickToolButtonSnapshotClick  TToolButtonToolButton1Leftљ TopWidthCaptionToolButton1
ImageIndexStyletbsSeparator  TToolButtonToolButtonFloppyALeftѕ TopHintSelect ODI file as floppy ACaptionFloppy A
ImageIndexParentShowHintShowHint	OnClickToolButtonFloppyAClick  TToolButtonToolButton5Leftж TopWidthCaptionToolButton5
ImageIndexStyletbsSeparator  TToolButtonToolButtonFloppyBLeftл TopHintSelect ODI file as floppy BCaptionFloppyB
ImageIndexParentShowHintShowHint	OnClickToolButtonFloppyBClick  TToolButtonToolButton7LeftTopWidthCaptionToolButton7
ImageIndexStyletbsSeparator  TToolButtonToolButtonOpenLeftTopActionActOpenSaveParentShowHintShowHint	  TToolButtonToolButton10Left+TopWidthCaptionToolButton10
ImageIndexStyletbsSeparator  TToolButtonToolButtonSettingsLeft>TopActionActSettingsParentShowHintShowHint	  TToolButtonToolButton3LeftUTopWidthCaptionToolButton3
ImageIndexStyletbsSeparator  TToolButtonToolButton6LeftgTopActionActHelp   TPanelpnScrLeft TopWidth HeightAlignalClient
BevelOuterbvNoneTabOrder 	TPaintBoxpbDrawLeft Top Width HeightAlignalClientOnPaintpbDrawPaint   TActionListActionList1Images
ImageList1LeftXTop( TActionActScrCaptionShow ScreenHintShow Screen
ImageIndex	OnExecuteActScrExecute  TActionActDbgCaptionShow DebuggerHintShow Debugger
ImageIndex	OnExecuteActDbgExecute  TActionActPauseCaptionPauseHint	Pause CPU
ImageIndex 	OnExecuteActPauseExecute  TActionActResetCaptionResetHint	Reset CPU
ImageIndex	OnExecuteActResetExecute  TActionActZoomCaptionZoomHintSelect screen zoom mode
ImageIndex	OnExecuteActZoomExecute  TAction
ActFloppyACaptionFloppy AHintSelect ODI file as floppy A
ImageIndex  TAction
ActFloppyBCaptionFloppy BHintSelect ODI file as floppy B
ImageIndex  TActionActOpenSaveCaptionOpen \ SaveHint,Open\Save ORDOS files (*.rko, *.bru, *.ord )
ImageIndex	OnExecuteActOpenSaveExecute  TActionActSettingsCaptionSettingsHintChange settings
ImageIndex	OnExecuteActSettingsExecute  TActionActHelpCaptionHelp
ImageIndex		OnExecuteActHelpExecute  TActionActDbgStepIntoCaptionF7 - Step IntoShortCutv	OnExecuteActDbgStepIntoExecute  TActionActDbgStepOverCaptionF8 - Step OverShortCutw	OnExecuteActDbgStepOverExecute   
TImageList
ImageList1LeftxTop(Bitmap
&S  IL     €€€€€€€€€€€€€BM6       6   (   @   P           P                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      њњњ                                                                                                                                                                                                                                                                                                                                                                                                                                          €               €                   €                      њњњ њњњ                                                                                                                                                                                €           €                       €            €                                                                                                                                                                                                €    €                        €    €                                                    њњњ                                                                                                                                                €   €                               €   €                                                           њњњ     њњњ                                                                                                                                           €   €       €               €       €   €                                                                 њњњ                                                                                                                                            €   €   €   €                       €   €   €   €   €                                              њњњ                                                                                                                                       €       €   €                                   €                                                          њњњ                                                                                                                                               €   €                               €   €                                                                                                                                                                                                  €   €                               €   €                                                                                                                                                                                                                                                           њњњ                                                                                                                                                                                                                                              њњњ     њњњ                                                                                                                                                                                                                                         њњњ      њњњ                                                                                                                                                                                                                                            њњњ                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ААА                                                             €€€    €  €€€                                                                                            А   А   А   А   А   А   А   А   А   А               ААА ААА     ААА ААА                                              €€ €€€  €€   €   €   €  €€ €€€  €€                                                                                          А   €€€ €€€ €€€ €€€ €€€ €€€ €€€ €€€ А               ААА јјј ААА ААА     ААА ААА                                  €€ €€€  €€ €€€    €  €€€  €€ €€€  €€                    €   €                                            ААА  АА ААА  АА ААА А   €€€                         €€€ А           ААА јјј јјј ААА ААА ААА ААА     ААА ААА                      €€ €€€  €€ €€€  €€ €€€  €€ €€€  €€ €€€  €€ €€€  €€                    њњњ њњњ                 њњњ          АА ААА  АА ААА  АА А   €€€ €€€ €€€ €€€ €€€ €€€ €€€ €€€ А   ААА јјј јјј јјј јјј јјј јјј ААА ААА ААА ААА     ААА ААА             €€€  €€ €€€  €€ €€€  €€   €  €€ €€€  €€ €€€  €€ €€€              њњњ њњњ њњњ њњњ њњњ  њњњ њњњ                       ААА  АА ААА  АА ААА А   €€€             €€€ А   А   А   А   јјј €€€ јјј јјј јјј јјј јјј јјј јјј јјј ААА ААА ААА     ААА     €€€  €€ €€€  €€ €€€  €€ €€€   €   €€ €€€  €€ €€€  €€ €€€                          њњњ  њњњ њњњ           њњњ          АА ААА  АА ААА  АА А   €€€ €€€ €€€ €€€ €€€ А   €€€ А           јјј јјј јјј јјј јјј јјј јјј јјј јјј јјј јјј ААА ААА ААА      €€ €€€  €€ €€€  €€ €€€  €€   €   € €€€  €€ €€€  €€ €€€  €€          њњњ њњњ њњњ њњњ њњњ  њњњ њњњ     њњњ     њњњ њњњ         ААА  АА ААА  АА ААА А   €€€ €€€ €€€ €€€ €€€ А   А                   ААА ААА јјј јјј јјј јјј јјј јјј јјј јјј јјј јјј ААА     €€€  €€ €€€  €€ €€€  €€ €€€  €€   €   € €€€  €€ €€€  €€ €€€                њњњ њњњ             њњњ њњњ          АА ААА  АА ААА  АА А   А   А   А   А   А   А                               ААА ААА јјј јјј јјј јјј јјј јјј јјј јјј          €€ €€€  €€ €€€    €€ €€€    €   € €€€  €€ €€€  €€         њњњ њњњ њњњ             њњњ њњњ њњњ                 њњњ         ААА  АА ААА  АА ААА  АА ААА  АА ААА  АА ААА  АА                                     ААА ААА јјј јјј јјј јјј јјј             €€€  €€ €€€  €€   €   € €€€  €€    €   €  €€ €€€  €€ €€€                         њњњ                                              АА ААА                                 ААА ААА                                             ААА ААА јјј јјј                     €€€  €€ €€€   €   €  €€€    €   € €€€  €€ €€€                         њњњ                                                 ААА ААА                                 ААА  АА                                                     ААА ААА                      €€ €€€  €€ €€€   €   €   €   €   € €€€  €€ €€€  €€                     њњњ                                                      АА ААА  АА      €€          €€     ААА  АА ААА                                                                                      €€ €€€  €€ €€€   €   €   € €€€  €€ €€€  €€                 њњњ њњњ                                                                              €€  €€                                                                                                              €€ €€€  €€ €€€  €€ €€€  €€ €€€  €€                         њњњ                                                                                                                                                                                                         €€€  €€ €€€  €€ €€€                                                                                                                                                                                                                                                 €€€ €€€               €   А   €                                                                                                                                             ААА ААА ААА ААА ААА ААА ААА ААА ААА ААА ААА ААА ААА ААА                      €€€ €€€                   €   А   €      АА                                                                                                                                     €€€ јјј јјј јјј јјј јјј јјј јјј јјј јјј јјј јјј јјј ААА          €€€ €€€ €€€ €€€                       €   А   €  АА  АА                  €€ њњњ  €€ њњњ  €€ њњњ  €€ њњњ  €€                                                                                 €€€ јјј јјј јјј ААА ААА ААА ААА ААА ААА јјј јјј јјј ААА         €€€         €€€ €€€                       €   А   €  АА  АА             €€€      €€ њњњ  €€ њњњ  €€ њњњ  €€ њњњ  €€         €€           €€ €€€  €€ €€€  €€                                     €€€  АА  АА  АА  АА  АА  АА  АА  АА  АА  АА  АА  АА ААА                         €€€                       €   А   €  АА                  €€ €€€      €€ њњњ  €€ њњњ  €€ њњњ  €€ њњњ  €€     €€       €€ €€€  €€ €€€                                             €€€ јјј јјј јјј ААА ААА ААА ААА ААА ААА јјј јјј јјј ААА                      €€€                   €   €   €   €   €                 €€€  €€ €€€                                         €€      €€€  €€ €€€  €€ €€€  €€ €€€                                 €€€ јјј јјј јјј јјј јјј   €   €   €   €   €   €  €  ААА                  €€€                    А   А   А   А   А                  €€ €€€  €€ €€€  €€ €€€  €€ €€€  €€                 €€       €€ €€€  €€ €€€                                             €€€ €€€ €€€ €€€ €€€ €€€   €   €   €   €   €   €   €   €                                       АА  АА  АА                      €€     €€€  €€ €€€  €€ €€€  €€ €€€  €€ €€€                 €€      €€€  €€ €€€  €€ €€€  €€ €€€  €€ €€€  €€           €   €                               €   € €€€ €€€ €€€   €   €   €                                      АА  АА  АА                         €€€      €€ €€€  €€ €€€  €€ €€€                             €€       €€ €€€                                           €   €                               €   €               €   €   €                                  АА  АА  АА                              €€ €€€      €€ €€€  €€ €€€      €€                                      €€ €€€  €€                                                                       €   €   €   €   €   €   €                                  АА  АА  АА                                 €€€  €€                   €€€                                                                                                                       €   €   €   €   €   €   €                              АА  АА  АА                                      €€ €€€  €€ €€€  €€ €€€  €€ €€€  €€                                                                                                                       €   €               €   €   €                              АА                                   €€€  €€ €€€  €€ €€€  €€ €€€  €€ €€€                                                                                                                       €   €               €   €   €                                                                                                                                                                                                                             €   €   €   €   €   €   €   €              АА                                                    €€€  €€ €€€  €€                                                                                                                                       €   €   €   €   €   €   €                  АА  АА                                                                                                                                                                                                                                                                                                                                                                                                                                           €€  €€  €€  €€  €€  €€  €€  €€  €€  €€  €€  €€  €€  €€  €€      €€  €€  €€  €€  €€  €€  €€  €€  €€  €€  €€  €€  €€  €€  €€                                                                          ААА ААА ААА ААА ААА ААА ААА ААА ААА ААА ААА ААА ААА ААА     €€                                                      €€      €€                                                      €€                                                                          €€€ јјј јјј јјј јјј јјј јјј јјј јјј јјј јјј јјј јјј ААА     €€      €€€               €€      €€      €€€               €€                                                                          €€€ јјј јјј јјј ААА ААА ААА ААА ААА ААА јјј јјј јјј ААА     €€      €€€                                          €€      €€      €€€                                          €€                                                                          €€€  АА  АА  АА  АА  АА  АА  АА  АА  АА  АА  АА  АА ААА     €€      €€€                                          €€      €€      €€€                                          €€                                     €€                          €€€ јјј јјј јјј ААА ААА ААА ААА ААА ААА јјј јјј јјј ААА     €€      €€€                                          €€      €€      €€€                                          €€                                €€                         €€€   €   €   € јјј јјј јјј   €   €   € јјј  €   €  ААА     €€      €€€                                          €€      €€      €€€                                          €€               €€€ њњњ €€€ њњњ €€€                                  €€€   €   €   € €€€ €€€ €€€   €   €   € €€€ €€€ €€€ ААА     €€      €€€                                          €€      €€      €€€                                          €€        €€€ њњњ €€€   € €€€ њњњ €€€                                 €   €   €   €   €   €   €   €   €                     €€      €€€                                          €€      €€      €€€                                          €€           њњњ €€€ њњњ   € њњњ €€€ њњњ                                        €   €   €   €   €   €   €                         €€      €€€                                          €€      €€      €€€                                          €€           €€€   €   €   €   €   € €€€                                        €   €   €       €   €   €                         €€      €€€                                          €€      €€      €€€                                          €€           њњњ €€€ њњњ   € њњњ €€€ њњњ                                        €   €   €       €   €   €                         €€      €€€                                          €€      €€      €€€                                          €€        €€€ њњњ €€€   € €€€ њњњ €€€                                     €   €   €       €   €   €                         €€      €€€ €€€ €€€ €€€ €€€ €€€ €€€ €€€ €€€ €€€ €€€     €€      €€      €€€ €€€ €€€ €€€ €€€ €€€ €€€ €€€ €€€ €€€ €€€     €€               €€€ њњњ €€€ њњњ €€€                                            €   €   €   €   €   €   €                         €€                                                      €€      €€                                                      €€                                                                         €   €   €   €   €                             €€  €€  €€  €€  €€  €€  €€  €€  €€  €€  €€  €€  €€  €€  €€      €€  €€  €€  €€  €€  €€  €€  €€  €€  €€  €€  €€  €€  €€  €€                                                                                  €   €   €   €   €                             BM>       >   (   @   P         А                      €€€                                                                                                                                 А Бю    UUв      а    vЉа    ґЎр    Ж¬#ј    ќж?ј     ¶гј    Жј"0    Nц      жт      жт b    юю            UU            э€€€ь€€сш?Аь а_а А јј <  АА    А          А     ј     р   ь А< €Аг€√€ѕАЗ€ €€ј€А€€аП€ш€€ш?я€€€€€шb€€€€ААаа€шАај шАај А1бј  |А1Ѕј  <АЅБА А√  юю  юqћ7 €ю®w гью@ч €ьюqг €€юqЅг ?€шюјгБ€€шю»?Б€€€€€€€€э€€  €шА  €сА  €гАсс€«АссаПА11јА11А?А11 ј11 а?11 в?сс в?сс в?  А?а?  јр  а€р                        
TPopupMenuOdiPopupMenuImages
ImageList1LeftTopP 	TMenuItem
ItemBrowseCaptionBrowse for file
ImageIndexOnClickItemBrowseClick  	TMenuItem	ItemClearCaptionClear driveOnClickItemClearClick  	TMenuItemItemInfoCaptionInfo  	TMenuItemN1Caption-  	TMenuItem
ItemRecentCaptionRecent file
ImageIndex   TOpenDialog
OpenDialog
DefaultExtodiFilterOrion Disk Images (*.odi)|*.odiOptionsofEnableSizing Left8Top(  TSaveDialog
SaveDialog
DefaultExtdmpLeftTop(  
TPopupMenuSnapPopupMenuLeft8TopP 	TMenuItemSavescreenpicture1CaptionSave screenshotOnClickSavescreenpicture1Click  	TMenuItemN5Caption-  	TMenuItemSaveshapshot128k1CaptionSave shapshot 128kOnClickSaveshapshot128k1Click  	TMenuItemSaveshapshot256k1CaptionSave shapshot 256kOnClickSaveshapshot256k1Click  	TMenuItemSaveshapshot512k1CaptionSave shapshot 512kOnClickSaveshapshot512k1Click  	TMenuItemN2Caption-  	TMenuItemLoadsnapshot1CaptionLoad snapshotOnClickLoadsnapshot1Click  	TMenuItemN3Caption-  	TMenuItemAutoSnapshot1CaptionAutoSave/Load shapshotOnClickAutoSnapshot1Click   
TPopupMenuZoomPopupMenuImages
ImageList1LeftXTopP 	TMenuItem
x1menuitemCaptionx 1OnClickx1menuitemClick  	TMenuItem
x2menuitemTagCaptionx 2Checked	OnClickx1menuitemClick  	TMenuItemx25menuitemTagCaptionx 2.5OnClickx1menuitemClick  	TMenuItem
x3menuitemTagCaptionx 3OnClickx1menuitemClick   
TPopupMenuOpenPopupMenuLeftyTopP 	TMenuItemItemLoadCaption Load ORDOS file(s) to RAM-disk BOnClickItemLoadClick  	TMenuItemN4Caption-  	TMenuItemItemSaveCaptionSave ORDOS file from RAM-disk BOnClickItemSaveClick   
TPopupMenuDebuggerMenuOnPopupDebuggerMenuPopupLeft† TopP 	TMenuItem	ItemPauseActionActPause  	TMenuItemN7Caption-  	TMenuItem
ItemModifyCaptionModify valueOnClickItemModifyClick  	TMenuItemN6Caption-  	TMenuItemItemSetConditionCaptionSet conditionOnClickItemSetConditionClick  	TMenuItemItemClearConditionCaptionClear conditionOnClickItemClearConditionClick   
TPopupMenuPageAddressMenuLeft  TopQ 	TMenuItemF9HL1Caption	(F9):(HL)OnClickF9Click  	TMenuItemF9HL2TagCaption	(F9):(DE)OnClickF9Click  	TMenuItemF9BC1TagCaption	(F9):(BC)OnClickF9Click  	TMenuItemF9IX1TagCaption	(F9):(IX)OnClickF9Click  	TMenuItemF9IY1TagCaption	(F9):(IY)OnClickF9Click  	TMenuItemF9SP1TagCaption	(F9):(SP)OnClickF9Click  	TMenuItemF9PC1TagCaption	(F9):(PC)OnClickF9Click  	TMenuItemF9Ix2TagCaption	(F9):(Ix)OnClickF9Click    