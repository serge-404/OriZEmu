/////////////////////////////////////////////////////////////////////////
//                                                                     //
//      Orion/Z (Orion-128 + Z80-CARD-II) emulator, version 1.9        //
//                                                                     //
//   Author: Sergey A.  <a-s-m@km.ru>                                  //
//                                                                     //
//   Copyright (C) 2006-2016 Sergey A.                                 //
//                                                                     //
//   This program is free software; you can redistribute it and/or     //
//                  modify it in any ways.                             //
//   This program is distributed "AS IS" in the hope that it will be   //
//   useful, but WITHOUT ANY WARRANTY; without even the implied        //
//   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  //
//                                                                     //
/////////////////////////////////////////////////////////////////////////


unit F600prnp;

interface

uses
  Windows, Messages, SysUtils, Forms,
  Mask, Classes, Controls, Dialogs, ExtCtrls, StdCtrls;

type
  TfrmF600prnp = class(TForm)
    btnFont: TButton;
    edtFontName: TEdit;
    FontDialog: TFontDialog;
    btnOK: TButton;
    btnCancel: TButton;
    rgMode: TRadioGroup;
    rgPrinter: TRadioGroup;
    meFontSize: TMaskEdit;
    rgCodePage: TRadioGroup;
    Label1: TLabel;
    meBorderTop: TMaskEdit;
    meBorderBottom: TMaskEdit;
    meBorderLeft: TMaskEdit;
    meBorderRight: TMaskEdit;
    Label2: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmF600prnp: TfrmF600prnp;

  function pConfigure: LongInt;
  function flush_prnp: LongInt;

implementation

{$R *.DFM}

Uses F600common, modF600, F600printer;

// Âàðèàíò1:
// pB0...pB7 out  - printer data (inverted)
// pC0 out - data strobe (active=1)
// pC7 in  - printer ready (Active=1)

// Âàðèàíò2:
// PA0..PA7 out - DATA
// PC7 - STROBE  - strob (inwersnyj)
// PC3 - BUSY  - gotownostx{
         ;
         ;     drajwer "LPT"
         ; "CENTRONICS" (irpr-m)
         ;
         ;======================
         ;
         ORG 0A800H
         ;
     PPB:EQU 0F601H
     PPC:EQU 0F602H
     PPU:EQU 0F603H
         ;
   PRINT:JMP PRNT       ; whod dr."wywod simwola"
  STATUS:JMP STR0       ; whod dr."gotownostx"
         ;
    STR0:MVI A,98H      ; programmirowanie porta
         STA PPU
         XRA A
         STA PPC        ; ust.ishod.sost.signalow
    STR1:DCR A          ; zadervka
         JNZ STR1
         CALL STTS      ; printer -- gotow?
         RZ             ; esli net - wyhod
         PUSH H
         LXI H,STTS     ; wykl.ustanowo~nu` ~astx
         SHLD STR0+1    ; programmy STATUS
         POP H
    STTS:LDA PPC        ; osnow.~astx progr. STATUS
         ANI 80H        ; prwer.  _BUSY
         RET
         ;
    PRNT:PUSH H
         PUSH D
         PUSH B
         PUSH PSW
    PRT0:JMP PRN
         ;
     PRN:MVI A,98H      ; po primeru podprog.
         STA PPU        ; STATUS programmiru`aq
         XRA A          ; ~astx podrogr. print
         STA PPC
    PRN1:DCR A
         JNZ PRN1
         CALL STTS
         JZ PRT3
         PUSH H
         LXI H,PRT1
         SHLD PRT0+1
         POP H
         ; osnownaq ~astx podprogrm.
         ; wywoda simwola na printer
    PRT1:CALL STTS      ; ovidatx gotownosti printera
         JZ PRT1        ; do priema simwola
         MOV A,C
         CMA            ; inwertirowatx bajt simwola
         STA PPB        ; wywesti na printer
         MVI A,1        ; \
         STA PPC        ;  formirowanie impulxsa
         XRA A          ;        _STROBE
         STA PPC        ;/
    PRT2:CALL STTS      ; ovidatx gotownosti printera
         JZ PRT2        ; posle priema simwola
    PRT3:POP PSW
         POP B
         POP D
         POP H
         RET
         ;
         END

  uproùennaq shema podkl`~eniq printera s interfejsom
  "CENTRONICS".

  port             obozn.             razxem LX-800

  B1 >------------- D0 --------------   2 DATA 1
  B2 >------------- D1 --------------   3 DATA 2
  B3 >------------- D2 --------------   4 DATA 3
  B4 >------------- D3 --------------   5 DATA 4
  B5 >------------- D4 --------------   6 DATA 5
  B6 >------------- D5 --------------   7 DATA 6
  B7 >------------- D6 --------------   8 DATA 7
  B8 >------------- D7 --------------   9 DATA 8
                              *
  C1 >------------ _STROBE -- SC ----   1 _STROBE
  C2 >----------------------- S0 ----

  C7 <----------------------- A0 ----
  C8 <------------  BUSY ---- AC ----  11 BUSY
 }

var
  SelectFont: boolean;
  SelectPrinter: boolean;
  PrnStarted: boolean;
  FlushCount: integer;

function flush_prnp: LongInt;
var mode, ii: integer;
    tmpbuf: TPrnBuffer;
  procedure PStop;
  begin
    FlushCount:=0;
    if PrnStarted then PrnStop;
    PrnStarted:=False;
  end;
begin
  mode:=0;
  Result:=0;
  if (BufCount=0) or (trim(PrnFile)='') then
  begin
    if FlushCount>2 then   // close printer (end job) if 3 seconds idle (end of printing)
      PStop
    else inc(FlushCount);
    exit;
  end;
  if SelectFont then begin
    SetPrinterParams('','Orion/Z emulator printing', PrnFont, PrnFontSize);
    SelectFont:=False;
  end;
  if SelectPrinter then begin
    if PrnPrn<>0 then mode:=PRINTER_DIALOG;
    SelectPrinter:=False;
    PStop;
  end;
  if not PrnStarted then
    PrnStarted:=PrnStart(mode);
  if PrnStarted then begin
    for ii:=0 to BufCount-1 do
    case PrnCP of
      1: begin
           tmpbuf[ii]:=Koi8to866(PrnBuffer[ii]);
         end
    end;
    PrnString(PChar(@tmpbuf[0]), BufCount);
  end;
  FlushCount:=0;
  BufCount:=0;
end;

function pConfigure: LongInt;
begin
  Result:=0;
  frmF600prnp:=nil;
  try
    frmF600prnp:=TfrmF600prnp.Create(Application);
    frmF600prnp.ShowModal;
  finally
    if Assigned(frmF600prnp) then
      frmF600prnp.Free;
  end;
end;

procedure TfrmF600prnp.btnOKClick(Sender: TObject);
begin
  SelectFont:=(AnsiUpperCase(PrnFont)<>AnsiUpperCase(trim(edtFontName.Text)))or
              (PrnFontSize<>StrToIntDef(trim(meFontSize.Text), 8));
  SelectPrinter:=(PrnPrn<>rgPrinter.ItemIndex);
  PrnFont:=trim(edtFontName.Text);
  PrnFontSize:=StrToIntDef(trim(meFontSize.Text), 8);
  PrnPrn:=rgPrinter.ItemIndex;
  PrnMode:=rgMode.ItemIndex;
  PrnCP:=rgCodePage.ItemIndex;
  BorderTop   := StrToIntDef(meBorderTop.Text, 50);
  BorderLeft  := StrToIntDef(meBorderLeft.Text, 50);
  BorderRight := StrToIntDef(meBorderRight.Text, 50);
  BorderBottom:= StrToIntDef(meBorderBottom.Text, 50);
  WritePrivateString(stSectionName, stPrnFontKey, PrnFont);
  WritePrivateInt(stSectionName, stPrnFtSzKey, PrnFontSize);
  WritePrivateInt(stSectionName, stPrnModeKey, PrnMode);
  WritePrivateInt(stSectionName, stPrnCPKey,   PrnCP);
  WritePrivateInt(stSectionName, stPrnPrnKey, PrnPrn);
  WritePrivateInt(stSectionName, stPrnBrdTop,   BorderTop );
  WritePrivateInt(stSectionName, stPrnBrdLeft,  BorderLeft );
  WritePrivateInt(stSectionName, stPrnBrdRight, BorderRight );
  WritePrivateInt(stSectionName, stPrnBrdBot,   BorderBottom );
  ModalResult:=mrOk;
end;

procedure TfrmF600prnp.btnFontClick(Sender: TObject);
begin
  if FontDialog.Execute then
  begin
    edtFontName.Text:=FontDialog.Font.Name;
    meFontSize.Text:=IntToStr(FontDialog.Font.Size);
  end;
end;

procedure TfrmF600prnp.FormActivate(Sender: TObject);
begin
  OnActivate:=nil;
  GetIniSettings;
  edtFontName.Text:=PrnFont;
  meFontSize.Text:=IntToStr(PrnFontSize);
  rgPrinter.ItemIndex:=PrnPrn;
  rgMode.ItemIndex:=PrnMode;
  rgCodePage.ItemIndex:=PrnCP;
  meBorderTop.Text   := IntToStr(BorderTop);
  meBorderLeft.Text  := IntToStr(BorderLeft);
  meBorderRight.Text := IntToStr(BorderRight);
  meBorderBottom.Text:= IntToStr(BorderBottom);
end;

initialization
  SelectPrinter:=True;
  SelectFont:=True;
  PrnStarted:=False;
  FlushCount:=0;

end.
