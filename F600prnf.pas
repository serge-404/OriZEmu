/////////////////////////////////////////////////////////////////////////
//                                                                     //
//   Orion/Z (Orion-128 + Z80-CARD-II) emulator, version 1.07          //
//                                                                     //
//                 http://www.orion-z.hoter.ru                         //
//                                                                     //
//             Author: Sergey A.                                       //
//                                                                     //
//             Copyright (C)2006-2011 Sergey A.                        //
//                                                                     //
//   This program is free software; you can redistribute it and/or     //
//                  modify it in any ways.                             //
//   This program is distributed "AS IS" in the hope that it will be   //
//   useful, but WITHOUT ANY WARRANTY; without even the implied        //
//   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  //
//                                                                     //
/////////////////////////////////////////////////////////////////////////

unit F600prnf;

interface

uses
  Windows, Messages, SysUtils, Forms,
  Classes, Controls, Dialogs, ExtCtrls, StdCtrls;

type
  TfrmF600prnf = class(TForm)
    edtFileName: TEdit;
    btnFileName: TButton;
    btnOk: TButton;
    btnCancel: TButton;
    SaveDialog: TSaveDialog;
    rgMode: TRadioGroup;
    rgFNew: TRadioGroup;
    rgCodePage: TRadioGroup;
    procedure btnFileNameClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TFlushCallback=function: LongInt;

var
  frmF600prnf: TfrmF600prnf;

  function fConfigure: LongInt;
  function flush_prnf: LongInt;
  function prnbuf(fType:LongInt; var fDataPtr: pointer; flush_buf: TFlushCallback): LongInt;

implementation

{$R *.DFM}

Uses F600common, modF600;

var NextFront: boolean;
    FlushCount: integer=0;

// ������� 1:
// pB0...pB7 out  - printer data (inverted)
// pC0 out - data strobe (active=1)
// pC7 in  - printer ready (Active=1)

// ������� 2:
// PA0..PA7 out - DATA
// PC7 - STROBE  - strob (inwersnyj)
         ;
         ;     drajwer "LPT"

function flush_prnf: LongInt;
var ii: integer;
{$IFDEF USE_FILESTREAM}
    fs: TFileStream;
    ss: string;
{$ELSE}
    ff: file of byte;
    bb: byte;
{$ENDIF}
begin
  Result:=0;
  if (BufCount=0) or (trim(PrnFile)='') then
  begin
    inc(FlushCount);
    exit;
  end;
{$IFDEF USE_FILESTREAM}
  fs:=nil;
  try
    if (not FileExists(PrnFile)) or ((PrnFNew<>0) and (FlushCount>2)) then
    begin
      fs:=TFileStream.Create(PrnFile, fmCreate);
      fs.Free;
    end;
    fs:=TFileStream.Create(PrnFile, fmOpenReadWrite or fmShareDenyNone);
    ss:='';
    for ii:=0 to BufCount-1 do
      ss:=ss+chr(PrnBuffer[ii]);
    fs.Seek(0, soFromEnd);
    fs.Write(PrnBuffer, BufCount);
  finally
    if Assigned(fs) then fs.Free;
  end;
{$ELSE}
  AssignFile(ff, PrnFile);
  if (not FileExists(PrnFile)) or ((PrnFNew<>0) and (FlushCount>2)) then
    Rewrite(ff)
  else
  begin
    Reset(ff);
    Seek(ff, FileSize(ff));
  end;
  for ii:=0 to BufCount-1 do
    case PrnCP of
      1: begin
           bb:=Koi8to866(PrnBuffer[ii]);
           write(ff, bb);
         end
      else write(ff, PrnBuffer[ii]);
    end;
  CloseFile(ff);
{$ENDIF}
  FlushCount:=0;
  BufCount:=0;
end;

function prnbuf(fType:LongInt; var fDataPtr: pointer; flush_buf: TFlushCallback): LongInt;
begin
  Result:=0;
  if BufCount=MAX_BUF then flush_buf;
  case fType of
    F600Func_PA_out: begin
                       case PrnMode of
                         0: ;
                         1: PrnBuffer[BufCount]:=PByte(fDataPtr)^;
                       end;
                     end;
    F600Func_PB_out: begin
                       case PrnMode of
                         0: PrnBuffer[BufCount]:=not PByte(fDataPtr)^;   // in this scheme incoming data is inverted
                         1: ;
                       end;
                     end;
    F600Func_PC_in:  begin
                       case PrnMode of
                         0: Result:=$80;                                 // printer allready ready
                         1: Result:=not 8;
                       end;
                     end;
    F600Func_PC_out: begin
                       case PrnMode of
                         0: if (PByte(fDataPtr)^ and 1 = 0) and (prev_pC and 1 <> 0) then      // ����� �� ����� ��������
                            begin
                              inc(BufCount);
                            end;
                         1: if (PByte(fDataPtr)^ and $80 <> 0) and (prev_pC and $80 = 0) then  // ����� �� ������ (��������)
                            begin
                              if NextFront then inc(BufCount);           // block first front (inializing of inv. high level carrier)
                              NextFront:=True;
                            end;
                       end;
                       prev_pC:=PByte(fDataPtr)^;
                     end;
  end;
end;

function fConfigure: LongInt;
begin
  Result:=0;
  frmF600prnf:=nil;
  try
    frmF600prnf:=TfrmF600prnf.Create(Application);
    frmF600prnf.ShowModal;
  finally
    if Assigned(frmF600prnf) then
      frmF600prnf.Free;
  end;
end;

procedure TfrmF600prnf.btnFileNameClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    edtFileName.Text:=SaveDialog.FileName;
end;

procedure TfrmF600prnf.FormActivate(Sender: TObject);
begin
  OnActivate:=nil;
  GetIniSettings;
  edtFileName.Text:=trim(PrnFile);
  rgMode.ItemIndex:=PrnMode;
  rgCodePage.ItemIndex:=PrnCP;
  rgFNew.ItemIndex:=PrnFNew;
end;

procedure TfrmF600prnf.btnOkClick(Sender: TObject);
begin
  PrnFile:=trim(edtFileName.Text);
  PrnMode:=rgMode.ItemIndex;
  PrnFNew:=rgFNew.ItemIndex;
  PrnCP:=rgCodePage.ItemIndex;
  WritePrivateString(stSectionName, stPrnFileKey, PrnFile);
  WritePrivateInt(stSectionName, stPrnModeKey, PrnMode);
  WritePrivateInt(stSectionName, stPrnCPKey,   PrnCP);
  WritePrivateInt(stSectionName, stPrnFNewKey, PrnFNew);
  ModalResult:=mrOk;
end;

initialization
  NextFront:=False;
  FlushCount:=0;

end.