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


unit F600printer;

interface

uses
  Windows, SysUtils, CommDlg, winspool;

const PRINTER_DIALOG  = $1;
      PRINT_OUT_FILE  = $2;
      OUT_FILE_EXIST  = $4;
      NO_INIT         = $8;
      APPEND_OUT_FILE = $10;
      MATRIX_PRINTER  = $20;
      LASER_PRINTER   = $40;
      PREVIEW_OUT_FILE= $80;
      PaperWidth: integer = 80; // 80 символов в строке
      ParerHeight: integer = 60;// 60 строк на листе
      LaserWrap: integer = 0;
      FontWidth: integer = 0;
      FontHeight: integer = 0;
      FontWeight: integer = FW_DONTCARE;
      FontName: string = 'Courier';
      Leading: double = 1;
      BorderTop: integer = 40;
      BorderLeft: integer = 40;
      BorderRight: integer = 40;
      BorderBottom: integer = 40;
      UsePrnPortName: boolean = True;
      WndOwner: HWND = 0;

var   DocName: String;
      PrinterName: String;

procedure ShowLastError;
procedure SetPrinterParams(PrintName, DocumName, LaserFontNm: string; LaserFontSz: integer);
procedure GetPrinterParams(var PName:string; var DName: string; var HPrinter:THandle; var PrnDC:HDC);
function PrnStart(Mode:LongInt):boolean;
function PrnString(Text:PChar; Len:integer):boolean;
function PrnStartPage:boolean;
function PrnEndPage:boolean;
function PrnStop:boolean;

implementation

type
     PrnRec = record
                 Cur: TPoint;
                 Finish: TPoint;         { End of the printable area }
                 Height: Integer;       { Height of the current line }
              end;
const
  MagicLaserValue=300;  {dpi}
  FHPrinter: THandle = 0;
  PrinterDC: HDC = 0;                                // IFDEF    TEdit
  _hFont: HFONT = 0;

var
  RecPrn: PrnRec;
  MatrixPrinter: boolean;

procedure ShowLastError;
var lpMsgBuf: PChar;
begin
  FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or
                FORMAT_MESSAGE_FROM_SYSTEM,
                nil, GetLastError, 0,
                @lpMsgBuf, 0, nil);
  MessageBox(WndOwner, lpMsgBuf, 'System Error', MB_OK+MB_ICONSTOP);
  LocalFree( HLOCAL(lpMsgBuf) );
end;

procedure FormatReportError(st:PChar);
begin
  MessageBox(WndOwner,st,'FormatReport Error',MB_OK+MB_ICONSTOP);
end;

procedure SetLaserFont;
begin
  if PrinterDC<>0 then
    begin
      _hfont:=CreateFont(-MulDiv(FontHeight, GetDeviceCaps(PrinterDC, LOGPIXELSY), 72),
                         -MulDiv(FontWidth, GetDeviceCaps(PrinterDC, LOGPIXELSX), 72),
                         0, 0, (FontWeight mod 10)*1000, 0, 0, 0,
                         {DEFAULT}RUSSIAN_CHARSET, OUT_DEFAULT_PRECIS,
                         CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
                         FIXED_PITCH or FF_DONTCARE, PChar(FontName));
      SelectObject(PrinterDC,_hfont);
      SetBkMode(PrinterDC, TRANSPARENT);
    end;
end;

procedure SetPrinterParams(PrintName, DocumName, LaserFontNm: string; LaserFontSz: integer);
begin
     if PrintName<>'' then
       begin
         PrinterName:=PrintName;
         FHPrinter:=0;
         PrinterDC:=0;
       end;
     DocName:=DocumName;
     if LaserFontNm<>'' then FontName:=LaserFontNm;
     if LaserFontSz<>0 then FontHeight:=LaserFontSz;
     if FontHeight=0 then FontHeight:=10;
     if FontName='' then FontName:='Courier';
     FontWidth:=(FontHeight div 3)*2;
     FontWeight:=4;
     SetLaserFont;
end;

procedure GetPrinterParams(var PName:string; var DName: string;
                           var HPrinter:THandle; var PrnDC:HDC);
begin
  PName:=PrinterName;
  DName:=DocName;
  HPrinter:=FHprinter;
  PrnDC:=PrinterDC;
end;

procedure NewPage(var Prn: PrnRec);
begin
  with Prn do
  begin
    Cur.X := BorderLeft {0};
    Cur.Y := BorderTop  {0};
    EndPage(PrinterDC);
    StartPage(PrinterDC);
  end;
end;

procedure NewLine(var Prn: PrnRec);
  function CharHeight: Word;
  var
    Metrics: TTextMetric;
  begin
    GetTextMetrics(PrinterDC, Metrics);
    Result := Metrics.tmHeight;
  end;
begin
  with Prn do
  begin
    Cur.X := BorderLeft {0};
    if Height = 0 then
      Inc(Cur.Y, trunc(CharHeight*Leading)) else
      Inc(Cur.Y, trunc(Height*Leading));
    if Cur.Y > (Finish.Y - (trunc(Height*Leading) * 2)) then NewPage(Prn);
    Height := 0;
  end;
end;

procedure PrnOutStr(var Prn: PrnRec; Text: PChar; Len: Integer);
var
  Extent: TSize;
  L: Integer;
begin
  with Prn do
  begin
    while Len > 0 do
    begin
      L := Len;
      GetTextExtentPoint32(PrinterDC, Text, L, Extent);
      while (L > 0) and (Extent.cX + (Cur.X + BorderLeft) > Finish.X) do
      begin
        L := CharPrev(Text, Text+L) - Text;
        GetTextExtentPoint32(PrinterDC, Text, L, Extent);
      end;
      if Extent.cY > Height then Height := Extent.cY + 2;
      Windows.TextOut(PrinterDC, Cur.X, Cur.Y, Text, L);
      Dec(Len, L);
      Inc(Text, L);
      if Len > 0 then NewLine(Prn)
      else Inc(Cur.X, Extent.cX);
    end;
  end;
end;

function PrnString(Text:PChar; Len:integer):boolean;
var
  L: Integer;
  TabWidth: Word;
  a: boolean;
  n: DWORD;
  procedure Flush;
  begin
    if L <> 0 then PrnOutStr(RecPrn, Text, L);
    Inc(Text, L + 1);
    Dec(Len, L + 1);
    L := 0;
  end;
  function AvgCharWidth: Word;
  var
    Metrics: TTextMetric;
  begin
    GetTextMetrics(PrinterDC, Metrics);
    Result := Metrics.tmAveCharWidth;
  end;
begin
   PrnString:=False;
   if (FHPrinter or PrinterDC)=0 then
      FormatReportError('Попытка печатать без начала документа.')
    else
      begin
        if (Len=0) and (Text<>nil) and (Text^<>#0) then
          Len:=StrLen(Text);
        If MatrixPrinter then
          begin
            a:=WritePrinter(FHPrinter, Text, Len, n);
            PrnString:=a and (n=Len);
          end
         else
          begin
            PrnString:=True;
            OemToAnsiBuff( Text, Text, Len );
            L := 0;
            with RecPrn do
            begin
              while L < Len do
              begin
                case Text[L] of
                  #9:
                    begin
                      Flush;
                      TabWidth := AvgCharWidth * 8;
                      Inc(Cur.X, TabWidth - ((Cur.X + TabWidth + 1)
                        mod TabWidth) + 1);
                      if Cur.X > Finish.X then NewLine(RecPrn);
                    end;
                  #13: Flush;
                  #10:
                    begin
                      Flush;
                      NewLine(RecPrn);
                    end;
                  ^L:
                    begin
                      Flush;
                      NewPage(RecPrn);
                    end;
                else
                  Inc(L);
                end;
              end;
            end;
            Flush;
          end;
      end;
end;

function PrnStartPage:boolean;
begin
   PrnStartPage:=False;
   if (FHPrinter or PrinterDC)=0 then
      FormatReportError('Попытка начать страницу без начала документа.')
    else
      begin
        If MatrixPrinter then PrnStartPage:=StartPagePrinter(FHPrinter)
          else PrnStartPage:=boolean(StartPage(PrinterDC));
      end;
end;

function PrnEndPage:boolean;
var n: DWORD {integer};
    a: boolean;
begin
   PrnEndPage:=False;
   if (FHPrinter or PrinterDC) = 0 then
      FormatReportError('Попытка завершить страницу без начала документа.')
    else
      begin
        If MatrixPrinter then
          begin
            a:=True;
            n:=1;
            if (GetVersion and $80000000)<>0 then  {IF WIN95}
                a:=WritePrinter(FHPrinter, PChar(#12), 1, n);
            PrnEndPage:=(n=1) and a and EndPagePrinter(FHPrinter);
          end
         else PrnEndPage:=(EndPage(PrinterDC)>0);
      end;
end;

function PrnStart(Mode:LongInt):boolean;
var DocInf:  TDocInfo;
    DocInfo: TDocInfo1;
    a: boolean;
    xDevMode: PDeviceMode;
    DevNames: PDevNames;
    PrnDlg: TPrintDlg;
begin
  a:=False;
  PrnStart:=False;
  if (FHPrinter or PrinterDC)<> 0 then
     FormatReportError('Предыдущий сеанс печати не завершен.')
   else
     begin
       if (NO_INIT and mode)=0 then SetPrinterParams('','','',0);
       with PrnDlg do
         begin
           lStructSize:=sizeof(PrnDlg);
           hWndOwner:=WndOwner;
           hDevMode:=0;
           hDevNames:=0;
           hDC:=0;
           Flags:=PD_HIDEPRINTTOFILE or PD_NOPAGENUMS or PD_RETURNDC or PD_USEDEVMODECOPIESANDCOLLATE or PD_NOSELECTION;
           if (PRINTER_DIALOG and mode)=0 then Flags:=Flags or PD_RETURNDEFAULT;
           nMinPage:=1;
           nMaxPage:=9999;
           nFromPage:=1;
           nToPage:=1;
           nCopies:=1;
           hInstance:=0;
           lCustData:=0;
           lpPrintTemplateName:=nil;
           lpSetupTemplateName:=nil;
           hPrintTemplate:=0;
           hSetupTemplate:=0;
         end;
       if PrintDlg(PrnDlg) then
         begin
           if (NO_INIT and mode)=0 then
             begin
               PrinterDC:=PrnDlg.hDC;
               try
                  xDevMode:=GlobalLock(PrnDlg.hDevMode);
                  PrinterName:=StrPas(xDevMode^.dmDeviceName);
                  case (MATRIX_PRINTER or LASER_PRINTER) and mode of
                    MATRIX_PRINTER: MatrixPrinter:=True;
                    LASER_PRINTER:  MatrixPrinter:=False
                    else MatrixPrinter:=(xDevMode.dmPrintQuality<MagicLaserValue);
                  end;
               finally
                  GlobalUnlock(PrnDlg.hDevMode);
                  GlobalFree(PrnDlg.hDevMode);
               end;
               if (UsePrnPortName) then
               try
                   DevNames := PDevNames(GlobalLock(PrnDlg.hDevNames));
                   PrinterName:=StrPas(PChar(DevNames) + DevNames^.wOutputOffset);
               finally
                   GlobalUnlock(PrnDlg.hDevNames);
                   GlobalFree(PrnDlg.hDevNames);
               end
             end;
{$IFDEF XDEBUG}
  ShowMessage('PrinterName='+PrinterName+'  '+IntToStr(PrinterDC));
{$ENDIF}
           if MatrixPrinter then
             begin
               if not OpenPrinter(PChar(PrinterName), FHPrinter, nil) then
                  FormatReportError(PChar('Нет такого принтера: '+PrinterName))
                else
                  begin
                    docInfo.pDocName := PChar('Orion/Z emulator: '+DocName);
                    docInfo.pOutputFile := nil;
                    docInfo.pDatatype := 'RAW';
                    a:=(StartDocPrinter(FHPrinter, 1, @docInfo)>0);
                  end;
             end
            else
             begin
               SetLaserFont;
               with RecPrn do
                 begin
                   Cur.X := 0;
                   Cur.Y := 0;
                   Finish.X := GetDeviceCaps(PrinterDC, HorzRes)-BorderRight;
                   Finish.Y := GetDeviceCaps(PrinterDC, VertRes)-BorderBottom;
                   Height := 0;
                 end;
               with DocInf do
                 begin
                   cbSize:=sizeof(DocInf);
                   lpszDocName:=PChar('Orion/Z emulator: '+DocName);
                   lpszOutput:=nil;
                   lpszDatatype:='EMF';
                   fwType:=0;
                 end;
               a:=(StartDoc(PrinterDC,DocInf)>0)
             end;
           PrnStart:=a and PrnStartPage;
         end;
     end;
end;

function PrnStop:boolean;
var a,b:boolean;
begin
  PrnStop:=False;
  if (FHPrinter or PrinterDC) = 0 then
      FormatReportError('Попытка завершить печать без начала документа.')
    else
      begin
        a:=PrnEndPage;
        If MatrixPrinter then
           begin
              b:=EndDocPrinter(FHPrinter);
              PrnStop:=a and b and ClosePrinter(FHPrinter);
           end
         else
           begin
             PrnStop:=a and (EndDoc(PrinterDC)>0);
             if _hfont<>0 then DeleteObject(_hfont);
           end;
        FHPrinter:=0;
        PrinterDC:=0;
      end;
end;

end.
