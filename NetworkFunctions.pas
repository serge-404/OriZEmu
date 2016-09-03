{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: NetworkFunctions.PAS, released on 2003-05-13.

The Initial Developer of the Original Code is Olivier Sannier
[obones@altern.org]
Portions created by Olivier Sannier are Copyright (C) 2003 Olivier Sannier.
All Rights Reserved.

Contributor(s): none to date.

You may retrieve the latest version of this file at the Connection Manager
home page, located at http://cnxmanager.sourceforge.net

Known Issues: none to date.

Description: This unit contains some functions to help get information
             on the network configuration of the current computer
-----------------------------------------------------------------------------}
// $Id: NetworkFunctions.pas,v 1.7 2005/05/19 20:23:12 obones Exp $
unit NetworkFunctions;

interface

uses Classes;

// Fills the given List with the names of all installed network adapters
// on the current computer. The List must be created before the call
// and will be cleared by the procedure. The Objects porperty of the
// List will be filled with the index of the adapters, so that you can
// use them in a call to GetUpBytes for instance
// (doing GetUpBytes(Cardinal(List.Objects[i])); )
//
// Parameters:
//    List  a TStrings instance that will be filled with the list of
//          installed network adapters for the current computer
//
procedure GetAdaptersList(List : TStrings);

// Returns the number of uploaded bytes (sent) through the given
// adapter, or 0 if the Adapter is incorrect
//
// Parameters:
//    Adapter   The index of a valid network adapter for this computer
//
// Return value:
//    The number of bytes uploaded (sent) through the given adapter
//
function GetUpBytes(Adapter : Cardinal) : cardinal;

// Returns the number of downloaded bytes (received) through the given
// adapter, or 0 if the Adapter is incorrect
//
// Parameters:
//    Adapter   The index of a valid network adapter for this computer
//
// Return value:
//    The number of bytes downloaded (received) through the given adapter
//
function GetDownBytes(Adapter : Cardinal) : cardinal;

// Returns the string representation of the given value as scientific
// number following the SI recommandations. (eg, 1000 is 1k)
// It can also follow the recommandations for binary numbers, where
// 1k is 1024 and a suffix is added (hence 1024 is 1ki)
//
// Parameters:
//    value         The value to convert
//    MaxDecimals   The maximum number of decimals to show in the string
//                  If the value is 2.10k then 2.1k will be shown
//                  If the value is 2.342k then 2.34k will be shown
//    separator     A string to add between the number representation
//                  and the power prefix. Defaults to '' (empty string)
//    binary        If true, the binary conversion rules are used
//                  Defaults to False
//    binarySuffix  The suffix to added when converting according to
//                  binary rules. Defaults to 'i', the recommanded
//                  value for SI compliant values
//
// Return value:
//    The converted number as a string
//
function BytesToSI(value : Int64;
                   MaxDecimals : Integer = 2;
                   separator : string = '';
                   binary : Boolean=False;
                   binarySuffix : string='i'
                  ): string;

type
  TCallNetShResult = (nsrOk, nsrNotFound, nsrOther);
  TOutputCallback = procedure(Line: string; var Aborted: Boolean);                   
// Calls Netsh with the given parameters and returns the
// result of ShellExecute

{$IFDEF CALL_NET_SH}

function CallNetSh(Parameters : string;
                   OutputCallback : TOutputCallback = nil): TCallNetShResult;

{$ENDIF}

// Returns the names of the available connections, as taken
// from the registry. These names are the ones to use with
// Netsh. The names list must be created by the caller
procedure GetConnectionInfo(Names, Guids: TStrings);
procedure AdaptersInfo(Names: TStrings);

function UrlDecode(S : String) : String;
function UrlEncode(S : String) : String;


implementation

uses Windows, SysUtils, Math, IPHlpApi, IPRtrMIB, IPTypes, IpIfConst, ShellApi, Registry
{$IFDEF CALL_NET_SH}
, CapExec
{$ENDIF}
;

procedure GetAdaptersList(List : TStrings);
var
  Size: Cardinal;
  IntfTable: PMibIfTable;
  I: Integer;
  MibRow: TMibIfRow;
  itemIndex : integer;
begin
  List.Clear;
  
  // ask for size
  Size := 0;
  if GetIfTable(nil, Size, True) = ERROR_INSUFFICIENT_BUFFER then
  begin
    // if we were given the size
    // then we allocate what's needed for it
    IntfTable := AllocMem(Size);
    try
      // asks for Interface information table
      if GetIfTable(IntfTable, Size, True) = NO_ERROR then
      begin
        // and fill the combo box
        for I := 0 to IntfTable^.dwNumEntries - 1 do
        begin
          MibRow := IntfTable.Table[I];
          // add the name
          itemIndex := List.Add(PChar(@MIbRow.bDescr[0]));
          // and store the index
          List.Objects[itemIndex] := TObject(MibRow.dwIndex);
        end;
      end;
    finally
      // always free
      FreeMem(IntfTable);
    end;
  end;
end;

function GetUpBytes(Adapter : Cardinal) : cardinal;
var
  row : TMIBIFRow;
begin
  row.dwIndex := Adapter;
  if GetIfEntry(@row) = NO_ERROR then
  begin
    Result := row.dwOutOctets;
  end
  else
  begin
    Result := 0;
  end;
end;

function GetDownBytes(Adapter : Cardinal) : cardinal;
var
  row : TMIBIFRow;
begin
  row.dwIndex := Adapter;
  if GetIfEntry(@row) = NO_ERROR then
  begin
    Result := row.dwInOctets;
  end
  else
  begin
    Result := 0;
  end;
end;

function BytesToSI(value : int64;
                   MaxDecimals : Integer;
                   separator : string;
                   binary : Boolean;
                   binarySuffix : string
                  ): string;
const prefixes : array[0..8] of string = ('', 'k','M','G','T', 'P', 'E', 'Z', 'Y');
var
  divider : integer;
  prefixIndex : cardinal;
  suffix : string;
  FloatValue : Extended;
  SignPrefix : string;
  Precision : Integer;
  StrRounded : String;
  i : Integer;
begin
  // decide the divider
  if binary then
  begin
    divider := 1024;
    suffix := binarySuffix;
  end
  else
  begin
    divider := 1000;
    suffix := '';
  end;

   // decide the sign prefix
   if Value < 0 then
   begin
    SignPrefix := '-';
    FloatValue := -Value;
   end
   else
   begin
    SignPrefix := '';
    FloatValue := Value;
   end;

  // at first, no prefix
  prefixIndex := 0;
  while (FloatValue / divider) >= 1 do
  begin
    // divide by divider
    FloatValue := FloatValue / divider;
    // use next prefix
    inc(prefixIndex);
  end;

  // calculate precision
  StrRounded := IntToStr(Round(Power(10, MaxDecimals) * FloatValue));
  i := Length(StrRounded);
  Precision := MaxDecimals;
  while StrRounded[i] = '0' do
  begin
    Dec(I);
    Dec(Precision);
  end;

  // return result
  Result := SignPrefix + Format('%.*f', [Precision, FloatValue]) + separator + prefixes[prefixIndex] + suffix;
end;

type
  TOutputCallbackHolder = class(TObject)
  private
    FOutputCallback: TOutputCallback;
    procedure CaptureLine(const Line: string; var Aborted: Boolean);
  public
    constructor Create(OutputCallback: TOutputCallback);
  end;

constructor TOutputCallbackHolder.Create(OutputCallback: TOutputCallback);
begin
  inherited Create;
  FOutputCallback := OutputCallback;
end;

procedure TOutputCallbackHolder.CaptureLine(const Line: string; var Aborted: Boolean);
begin
  if Assigned(FOutputCallback) then
    FOutputCallback(Line, Aborted);
end;

var stbuf:array[0..MAX_PATH] of char;

{$IFDEF CALL_NET_SH}

function CallNetSh(Parameters : string; OutputCallback : TOutputCallback): TCallNetShResult;
var
  szParameters: PChar;
  tmp : Boolean;
  NetshFileName: string;
  CaptureResult: Integer;
  callbackHolder: TOutputCallbackHolder;
begin
  // Test for netsh presence
  NetshFileName := '';
  if GetSystemDirectory(stbuf, MAX_PATH)>0 then
    NetshFileName := string(stbuf) + '\netsh.exe';
  if not FileExists(NetshFileName) then
  begin
    if GetWindowsDirectory(stbuf, MAX_PATH)>0 then
      NetshFileName := string(stbuf) + '\netsh.Exe';
    if not FileExists(NetshFileName) then
      NetshFileName := '';
  end;
  if NetshFileName = '' then
  begin
    Result := nsrNotFound;
  end
  else
  begin
    // Prepare callback holder
    callbackHolder := TOutputCallbackHolder.Create(OutputCallback);
    try
      szParameters := GetMemory(Length(Parameters));
      try
        // Ensure that the parameters are in the OEM page code.
        //CharToOEM(PChar(Parameters), szParameters);
        StrPCopy(szParameters, Parameters);

        tmp := False;
        OutputCallback(NetshFileName+ ' '+ szParameters, tmp);


        // Now call netsh
        CaptureResult := CaptureExecute(NetshFileName,
                                        SzParameters,
                                        GetCurrentDir,
                                        callbackHolder.CaptureLine);
        Result := nsrOk;
        if CaptureResult = -1 then
          Result := nsrOther
      finally
        FreeMemory(szParameters);
      end;
    finally
      callbackHolder.Free;
    end;
  end;
end;

{$ENDIF}

procedure GetConnectionInfo(Names, Guids: TStrings);
const
  StartKey = '\SYSTEM\CurrentControlSet\Control\Network';
  SubKeyNetName = 'Class';
  SubKeyNetValue = 'Net';
  NameValueName = 'Name';
  ShowIconValueName = 'ShowIcon';
var
  Reg: TRegistry;
  KeyNames : TStringList;
  i : Integer;
  KeyFound : Boolean;
  Key : string;
begin
  Names.Clear;

  // Access the registry in read only mode
  Reg := TRegistry.Create(HKEY_LOCAL_MACHINE);
  KeyNames := TStringList.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKeyReadOnly(StartKey);

    // Need to find the child that has a value named
    // 'Class' which value is 'Net'
    Reg.GetKeyNames(KeyNames);
    i := 0;
    KeyFound := False;
    Key := StartKey;
    while (i < KeyNames.Count) and not KeyFound do
    begin
      Reg.OpenKeyReadOnly(Key+'\'+KeyNames[i]);
      if Reg.ReadString(SubKeyNetName) = SubKeyNetValue then
        KeyFound := True;
      Inc(i);
    end;

    // Found a key, open it and read its subkeys
    // which in turn contain the names we are looking for
    // in their Connection subkey
    if KeyFound then
    begin
      Key := '\'+Reg.CurrentPath;
      Reg.OpenKeyReadOnly(KeyNames[i-1]);
      Reg.GetKeyNames(KeyNames);
      for i := 0 to KeyNames.Count - 1 do
      begin
        Reg.OpenKeyReadOnly(Key+'\'+KeyNames[i]+'\Connection');
        if Reg.ValueExists(NameValueName) then
        begin
        // This below has been removed, there must be another way
        // to find if an interface is valid or not
{          // do not add if ShowIcon is 0.
          if not (Reg.ValueExists(ShowIconValueName) and
                 (Reg.ReadInteger(ShowIconValueName) = 0)) then}
            Names.Add(Reg.ReadString(NameValueName));
            Guids.Add(KeyNames[i]);
        end;
      end;
    end;
  finally
    Reg.Free;
    KeyNames.Free;
  end;
end;

function XDigit(Ch : char) : Integer;
begin
    if ch in ['0'..'9'] then
        Result := ord(Ch) - ord('0')
    else
        Result := (ord(Ch) and 15) + 9;
end;

function IsXDigit(Ch : char) : Boolean;
begin
    Result := (ch in ['0'..'9']) or (ch in ['a'..'f']) or (ch in ['A'..'F']);
end;

function htoin(value : PChar; len : Integer) : Integer;
var
    i : Integer;
begin
    Result := 0;
    i      := 0;
    while (i < len) and (Value[i] = ' ') do
        i := i + 1;
    while (i < len) and (isxDigit(Value[i])) do begin
        Result := Result * 16 + xdigit(Value[i]);
        i := i + 1;
    end;
end;

function htoi2(value : PChar) : Integer;
begin
    Result := htoin(value, 2);
end;


function UrlEncode(S : String) : String;
var
    I : Integer;
begin
    Result := '';
    for I := 1 to Length(S) do begin
        if S[I] in ['0'..'9', 'A'..'Z', 'a'..'z'] then
            Result := Result + S[I]
        else
            Result := Result + '%' + IntToHex(Ord(S[I]), 2);
    end;
end;

function UrlDecode(S : String) : String;
var
    I  : Integer;
    Ch : Char;
begin
    Result := '';
    I := 1;
    while (I <= Length(S)) and (S[I] <> '&') do begin
        Ch := S[I];
        if Ch = '%' then begin
            Ch := chr(htoi2(@S[I + 1]));
            Inc(I, 2);
        end
        else if Ch = '+' then
            Ch := ' ';
        Result := Result + Ch;
        Inc(I);
    end;
end;

// serge
procedure AdaptersInfo(Names: TStrings);
var
  pAdapterInfo, pAdapt: PIP_ADAPTER_INFO;
  pAddrStr:PIP_ADDR_STRING;
  Err, AdapterInfoSize: DWORD;
begin
//Очищаю список устройств
  Names.Clear;
//Получить количество устройств
  AdapterInfoSize:=0;
  Err:=GetAdaptersInfo(nil, AdapterInfoSize);
//Если произошла ошибка, то...
  if (Err<>0) and (Err<>ERROR_BUFFER_OVERFLOW) then
  begin
    Names.Add('Error');
    exit;
  end;
//Получить информацию об устройствах.
  pAdapterInfo := PIP_ADAPTER_INFO(GlobalAlloc(GPTR, AdapterInfoSize));
  GetAdaptersInfo(pAdapterInfo, AdapterInfoSize);
  pAdapt := pAdapterInfo;
//Проверяю тип полученного адаптера
  while pAdapt<>nil do
  begin
    case pAdapt.Type_ of
      MIB_IF_TYPE_ETHERNET:
        Names.Add('Ethernet adapter '+pAdapt.AdapterName);
      MIB_IF_TYPE_TOKENRING:
        Names.Add('Token Ring adapter '+pAdapt.AdapterName);
      MIB_IF_TYPE_FDDI:
        Names.Add('FDDI adapter '+pAdapt.AdapterName);
      MIB_IF_TYPE_PPP:
        Names.Add('PPP adapter '+pAdapt.AdapterName);
      MIB_IF_TYPE_LOOPBACK:
        Names.Add('Loopback adapter '+pAdapt.AdapterName);
      MIB_IF_TYPE_SLIP:
        Names.Add('Slip adapter '+pAdapt.AdapterName);
      MIB_IF_TYPE_OTHER:
        Names.Add('Other adapter '+pAdapt.AdapterName);
    end;
    pAdapt := pAdapt.Next;
  end;
//  GlobalFree(Cardinal(pFixedInfo));
end;


end.
