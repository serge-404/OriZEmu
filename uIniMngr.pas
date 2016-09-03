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


unit uIniMngr;

interface

Uses Windows, SysUtils, Classes, Forms, Menus, TypInfo, IniFiles;

const
  BufSize = 32768;
  stSectionRecentFiles = 'RECENT';

type
  TIniBindType = (btInteger, btString, btBoolean, btSmallInt, btChar, btCharBuf, btDouble, btProp);

  TAsofIniFile = TIniFile;

  PString = ^String;
  PInteger = ^Integer;
  PBoolean = ^Boolean;
  PSmallInt = ^SmallInt;
  PDouble = ^Double;

  TIniBindRec = class                  // TObject
    FBindType: TIniBindType;
    FValue: pointer;                   // pointer to variable (or nil if FBindType=btProp)
    FDefaultValue: string;             // INI
    FSectionName: string;              // INI
    FKeyName: string;                  // INI
    FInstance: TObject;                // instance of associated class (or nil if FBindType<>btProp)
    FPropertyName: string;             // name of associated published property
    FPropInfo: PPropInfo;
    FPropType: PTypeInfo;
  private
    function GetPropAsFloat: double;
    function GetPropAsInteger: integer;
    function GetPropAsString: String;
    function GetVarAsFloat: double;
    function GetVarAsInteger: integer;
    function GetVarAsString: String;
    procedure SetPropAsFloat(const Value: double);
    procedure SetPropAsInteger(const Value: integer);
    procedure SetPropAsString(const Value: String);
    procedure SetVarAsFloat(const Value: double);
    procedure SetVarAsInteger(const Value: integer);
    procedure SetVarAsString(const Value: String);
    function FindProperty(Instance: TObject; PropertyName:string; var PropInfo:PPropInfo):boolean;
  public
    constructor Create(BindType:TIniBindType; pValue: pointer; Instance: TObject;
      PropertyName, SectionName, KeyName, DefaultValue:string);
    property pValue:pointer        read FValue;
    property BindType:TIniBindType read FBindType;
    property Instance:TObject      read FInstance;
    property PropertyName:string   read FPropertyName;
    property SectionName:string    read FSectionName;
    property KeyName:string        read FKeyName;
    property DefaultValue:string   read FDefaultValue;
    property VarAsInteger:integer  read GetVarAsInteger  write SetVarAsInteger;
    property VarAsString:String    read GetVarAsString   write SetVarAsString;
    property VarAsFloat:double     read GetVarAsFloat    write SetVarAsFloat;
    property PropAsInteger:integer read GetPropAsInteger write SetPropAsInteger;
    property PropAsString:String   read GetPropAsString  write SetPropAsString;
    property PropAsFloat:double   read GetPropAsFloat   write SetPropAsFloat;
  end;

  TIniManager = class
    FIniFile: TAsofIniFile;    // not creating in TIniManager - passed by external proc !
    FBindList: TList;
    FRecentFilesList: TStringList;
    FBuffer: array [0..BufSize] of char;
    FRecentFilesMenuItem: TMenuItem;
    FRecentFilesSectionName: string;
    FRecentFilesDropDownMax: integer;
    FRecentFilesDropDownCount: integer;
    FRecentFilesItemClick: TNotifyEvent;
  private
    function  CheckIniFile:TAsofIniFile;
    procedure RecentFileItemClick(Sender:TObject);
    procedure SetRecentFilesMenuItem(const Value: TMenuItem);
  public
    constructor Create;
    destructor Destroy; override;
    function GetRecentFilesSection: integer;
    function WriteRecentFilesSection: boolean;
    procedure GetPrivateSection(SectionName:string; SL:TStringList);
    function GetPrivateString(SectionName,KeyName,DefaultValue:string):string;
    function GetPrivateInt(SectionName,KeyName:string;DefaultValue:Integer):integer;
    function WritePrivateString(SectionName,KeyName,Value:string):boolean;
    function WritePrivateInt(SectionName,KeyName:string;Value:Integer):boolean;
    function WritePrivateSection(SectionName:string; SL:TStringList):boolean;
    function GetValueID(pValue: pointer): integer;
    function GetPropID(Instance: TObject; PropertyName:string): integer;
    procedure BindProperty(Instance: TObject; PropertyName,
                           SectionName, KeyName, DefaultValue:string);
    procedure BindVariable(BindType: TIniBindType; pValue: pointer;
                           SectionName, KeyName, DefaultValue:string);
    procedure LinkPropertyToVariable(Instance: TObject; PropertyName:string; pVariable: pointer);
    procedure UnLinkPropertyFromVariable(pVariable: pointer);
    procedure StopBindVariable(pValue: pointer);
    procedure StopBindProperty(Instance: TObject; PropertyName: string);
    procedure WriteValueByID(ID: integer);
    procedure WritePropByID(ID: integer);
    procedure GetValueByID(ID: integer);
    procedure GetPropByID(ID: integer);
    procedure WriteValue(pValue: pointer);
    procedure WriteProp(Instance: TObject; PropertyName:string);
    procedure GetValue(pValue: pointer);
    procedure GetProp(Instance: TObject; PropertyName:string);
    procedure LoadLinkedPropsFromIni;
    procedure LoadLinkedPropsFromVar;
    procedure SaveLinkedPropsToIni;
    procedure SaveLinkedPropsToVar;
    function  IsLinkedPropsEqualToVars:boolean;
    procedure WriteAllValues;
    procedure GetAllValues;
    procedure WriteAllProps;
    procedure GetAllProps;
    procedure WriteAll;
    procedure GetAll;
    procedure Clear;
    procedure Flush;
    procedure RecentFilesAdd(aCaption: String);
{}
    property IniFileObj: TAsofIniFile read FIniFile write FIniFile;
    property RecentFilesMenuItem: TMenuItem read FRecentFilesMenuItem write SetRecentFilesMenuItem;
    property RecentFilesSectionName: string read FRecentFilesSectionName write FRecentFilesSectionName;
    property RecentFilesDropDownList: TStringList read FRecentFilesList;
    property RecentFilesDropDownMax: integer read FRecentFilesDropDownMax write FRecentFilesDropDownMax;
{}
    property OnRecentFilesItemClick: TNotifyEvent read FRecentFilesItemClick write FRecentFilesItemClick;
  end;

implementation

{ TIniBindRec }

constructor TIniBindRec.Create(BindType:TIniBindType; pValue: pointer; Instance: TObject;
  PropertyName, SectionName, KeyName, DefaultValue:string);
begin
  inherited Create;
  FBindType:=BindType;
  FValue:=pValue;
  FInstance:=Instance;
  FPropertyName:=PropertyName;
  FSectionName:=SectionName;
  FKeyName:=KeyName;
  FDefaultValue:=DefaultValue;
end;

function TIniBindRec.FindProperty(Instance: TObject; PropertyName: string;
  var PropInfo: PPropInfo): boolean;
var
  I, Count: Integer;
  PropList: PPropList;
begin
  Result:=False;
  Count := GetTypeData(Instance.ClassInfo)^.PropCount;
  if Count > 0 then
  begin
    GetMem(PropList, Count * SizeOf(Pointer));
    try
      GetPropInfos(Instance.ClassInfo, PropList);
      I:=0;
      while (I<Count)and (not Result) do
      begin
        PropInfo := PropList^[I];
        if PropInfo = nil then i:=Count
        else Result:={(IsStoredProp(Instance, PropInfo))and}
                     (PropInfo^.SetProc <> nil)and
                     (UpperCase(PropInfo^.Name) = UpperCase(PropertyName));
        inc(I);
      end;
    finally
      FreeMem(PropList, Count * SizeOf(Pointer));
    end;
  end;
end;

function TIniBindRec.GetPropAsFloat: double;
var d:double;
    i:integer;
begin
  Result:=0.0;
  if Assigned(FInstance) and (FPropertyName<>'') and
     FindProperty(FInstance, FPropertyName, FPropInfo) then
  begin
     FPropType:=FPropInfo^.PropType^;
     case FPropType^.Kind of
        tkInteger, tkChar, tkEnumeration, tkSet:
           Result:=GetOrdProp(FInstance, FPropInfo);
        tkFloat:
           Result:=GetFloatProp(FInstance, FPropInfo);
        tkString, tkLString, tkWString:
           begin
             val(GetStrProp(FInstance, FPropInfo), d, i);
             if i=0 then Result:=d;
           end;
     end;
  end;
end;

function TIniBindRec.GetPropAsInteger: integer;
begin
  Result:=0;
  if Assigned(FInstance) and (FPropertyName<>'') and
     FindProperty(FInstance, FPropertyName, FPropInfo) then
  begin
     FPropType:=FPropInfo^.PropType^;
     case FPropType^.Kind of
        tkInteger, tkChar, tkEnumeration, tkSet:
           Result:=GetOrdProp(FInstance, FPropInfo);
        tkFloat:
           Result:=Trunc(GetFloatProp(FInstance, FPropInfo));
        tkString, tkLString, tkWString:
           Result:=StrToIntDef(GetStrProp(FInstance, FPropInfo), 0);
     end;
  end;
end;

function TIniBindRec.GetPropAsString: String;
begin
  Result:='';
  if Assigned(FInstance) and (FPropertyName<>'') and
     FindProperty(FInstance, FPropertyName, FPropInfo) then
  begin
     FPropType:=FPropInfo^.PropType^;
     case FPropType^.Kind of
        tkInteger, tkChar, tkEnumeration, tkSet:
           Result:=IntToStr(GetOrdProp(FInstance, FPropInfo));
        tkFloat:
           Result:=FloatToStr(GetFloatProp(FInstance, FPropInfo));
        tkString, tkLString, tkWString:
           Result:=GetStrProp(FInstance, FPropInfo);
     end;
  end;
end;

function TIniBindRec.GetVarAsFloat: double;
var d: double;
    i: integer;
begin
  Result:=0.0;
  if Assigned(FValue) then
     case FBindType of
        btInteger:  Result:=PInteger(FValue)^;
        btSmallInt: Result:=PSmallInt(FValue)^;
        btBoolean:  Result:=Integer(PBoolean(FValue)^);
        btString:   begin
                      val(PString(FValue)^, d, i);
                      if i=0 then Result:=d;
                    end;
        btChar:     Result:=PByte(FValue)^;
        btCharBuf:  begin
                      val(StrPas(Pchar(FValue)), d, i);
                      if i=0 then Result:=d;
                    end;
        btDouble:   Result:=PDouble(FValue)^;
     end;
end;

function TIniBindRec.GetVarAsInteger: integer;
begin
  Result:=0;
  if Assigned(FValue) then
     case FBindType of
        btInteger:  Result:=PInteger(FValue)^;
        btSmallInt: Result:=PSmallInt(FValue)^;
        btBoolean:  Result:=Integer(PBoolean(FValue)^);
        btString:   Result:=StrToIntDef(PString(FValue)^, 0);
        btChar:     Result:=PByte(FValue)^;
        btCharBuf:  Result:=StrToIntDef(StrPas(Pchar(FValue)), 0);
        btDouble:   Result:=Trunc(PDouble(FValue)^);
     end;
end;

function TIniBindRec.GetVarAsString: String;
begin
  Result:='';
  if Assigned(FValue) then
     case FBindType of
        btInteger:  Result:=IntToStr(PInteger(FValue)^);
        btSmallInt: Result:=IntToStr(PSmallInt(FValue)^);
        btBoolean:  Result:=IntToStr(Integer(PBoolean(FValue)^));
        btString:   Result:=PString(FValue)^;
        btChar:     Result:=Char(PByte(FValue)^);
        btCharBuf:  Result:=StrPas(Pchar(FValue));
        btDouble:   Result:=FloatToStr(PDouble(FValue)^);
     end;
end;

procedure TIniBindRec.SetPropAsFloat(const Value: double);
begin
  if Assigned(FInstance) and (FPropertyName<>'') and
     FindProperty(FInstance, FPropertyName, FPropInfo) then
  begin
     FPropType:=FPropInfo^.PropType^;
     case FPropType^.Kind of
        tkInteger, tkChar, tkEnumeration, tkSet:
          SetOrdProp(FInstance, FPropInfo, Trunc(Value));
        tkFloat:
          SetFloatProp(FInstance, FPropInfo, Value);
        tkString, tkLString, tkWString:
          SetStrProp(FInstance, FPropInfo, FloatToStr(Value));
      end;
  end;
end;

procedure TIniBindRec.SetPropAsInteger(const Value: integer);
begin
  if Assigned(FInstance) and (FPropertyName<>'') and
     FindProperty(FInstance, FPropertyName, FPropInfo) then
  begin
     FPropType:=FPropInfo^.PropType^;
     case FPropType^.Kind of
        tkInteger, tkChar, tkEnumeration, tkSet:
          SetOrdProp(FInstance, FPropInfo, Value);
        tkFloat:
          SetFloatProp(FInstance, FPropInfo, Value);
        tkString, tkLString, tkWString:
          SetStrProp(FInstance, FPropInfo, IntToStr(Value));
      end;
  end;
end;

procedure TIniBindRec.SetPropAsString(const Value: String);
var d: double;
    i: integer;
begin
  if Assigned(FInstance) and (FPropertyName<>'') and
     FindProperty(FInstance, FPropertyName, FPropInfo) then
  begin
     FPropType:=FPropInfo^.PropType^;
     case FPropType^.Kind of
        tkInteger, tkChar, tkEnumeration, tkSet:
          SetOrdProp(FInstance, FPropInfo, StrToIntDef(Value,0));
        tkFloat:
          begin
            val(Value, d, i);
            if i>0 then d:=0.0;
            SetFloatProp(FInstance, FPropInfo, d);
          end;
        tkString, tkLString, tkWString:
          SetStrProp(FInstance, FPropInfo, Value);
      end;
  end;
end;

procedure TIniBindRec.SetVarAsFloat(const Value: double);
begin
  case FBindType of
    btInteger:  PInteger(FValue)^  := Trunc(Value);
    btSmallInt: PSmallInt(FValue)^ := LoWord(Trunc(Value));
    btBoolean:  PBoolean(FValue)^  := boolean(Trunc(Value));
    btDouble:   PDouble(FValue)^   := Value;
    btString:   PString(FValue)^   := FloatToStr(Value);
    btChar:     PByte(FValue)^     := Lo(Trunc(Value));
    btCharBuf:  StrPCopy(PChar(FValue), FloatToStr(Value));
  end;
end;

procedure TIniBindRec.SetVarAsInteger(const Value: integer);
begin
  case FBindType of
    btInteger:  PInteger(FValue)^  := Value;
    btSmallInt: PSmallInt(FValue)^ := LoWord(Value);
    btBoolean:  PBoolean(FValue)^  := boolean(Value);
    btDouble:   PDouble(FValue)^   := Value;
    btString:   PString(FValue)^   := IntToStr(Value);
    btChar:     PByte(FValue)^     := Lo(Value);
    btCharBuf:  StrPCopy(PChar(FValue), IntToStr(Value));
  end;
end;

procedure TIniBindRec.SetVarAsString(const Value: String);
var d: double;
    i: integer;
begin
  case FBindType of
    btInteger:  PInteger(FValue)^  := StrToIntDef(Value,0);
    btSmallInt: PSmallInt(FValue)^ := LoWord(StrToIntDef(Value,0));
    btBoolean:  PBoolean(FValue)^  := boolean(StrToIntDef(Value,0));
    btDouble:   begin
                  val(Value,d,i);
                  if i>0 then d:=0.0;
                  PDouble(FValue)^ := d;
                end;
    btString:   PString(FValue)^   := Value;
    btChar:     if Length(Value)>0 then PByte(FValue)^ := Byte(Value[1]);
    btCharBuf:  StrPCopy(Pchar(FValue), Value);
  end;
end;

{ TIniManager }

function NonStandardGetSection(SectionName: string;
  Buffer: PChar; BufferSize: integer; IniName:string): integer;
var FileSL: TStringList;
    i: integer;
    st: string;
begin
     st:='';
     FileSL:=TStringList.Create;
     FileSL.LoadFromFile(IniName);
     i:=0;
     while (i<FileSL.Count)and(pos('['+AnsiUpperCase(SectionName)+']',
                                   AnsiUpperCase(trim(FileSL.Strings[i])))<>1) do
           inc(i);
     if i<FileSL.Count then
     begin
        inc(i);
        while (i<FileSL.Count)and(trim(FileSL.Strings[i])<>'')and
              (trim(FileSL.Strings[i])[1]<>'[') do
        begin
             st:=st+FileSL.Strings[i]+#0;
             inc(i);
        end;
     end;
     if Length(st)>BufferSize-3 then st:=copy(st,1,BufferSize-3);
     st:=st+#0#0;
     CopyMemory(Buffer, PChar(st), Length(st));
     Result:=Length(st);
     FileSL.Free;
end;

procedure TIniManager.GetPrivateSection(SectionName: string; SL:TStringList);
const
  BufSize = 16384;
var
  Buffer, P: PChar;
begin
    GetMem(Buffer, BufSize);
    try
      SL.BeginUpdate;
      try
        SL.Clear;
        Buffer[0]:=#0;
// because inherited TIniFile.ReadSection call GetPrivateProfileString(..,nil,..) and sucks on exotic sections :)
//
        NonStandardGetSection(SectionName, Buffer, BufSize, FIniFile.FileName);
        P := Buffer;
        while P^ <> #0 do
        begin
          SL.Add(P);
          Inc(P, StrLen(P) + 1);
        end;
      finally
        SL.EndUpdate;
      end;
    finally
      FreeMem(Buffer, BufSize);
    end;
end;

procedure TIniManager.BindVariable(BindType: TIniBindType; pValue: pointer;
  SectionName, KeyName, DefaultValue: string);
begin
  FBindList.Add(pointer(TIniBindRec.Create(BindType, pValue, nil, '',
                        SectionName, KeyName, DefaultValue)));
end;

constructor TIniManager.Create;
begin
  inherited Create;
  FIniFile:=nil;
  RecentFilesMenuItem:=nil;
  FRecentFilesItemClick:=nil;
  RecentFilesSectionName:='';
  RecentFilesDropDownMax:=8;
  FBindList:=TList.Create;
  FRecentFilesList:=TStringList.Create;
  FRecentFilesSectionName:=stSectionRecentFiles;
end;

destructor TIniManager.Destroy;
begin
  Clear;
  FBindList.Free;
  FRecentFilesList.Free;
  inherited Destroy;
end;

function TIniManager.GetPrivateString(SectionName,KeyName,DefaultValue:string):string;
begin
   Result:=trim(CheckIniFile.ReadString(SectionName,KeyName,DefaultValue));
end;

function TIniManager.GetPrivateInt(SectionName,KeyName:string;DefaultValue:Integer):integer;
begin
   Result:=CheckIniFile.ReadInteger(SectionName, KeyName, DefaultValue);
end;

function TIniManager.WritePrivateString(SectionName,KeyName,Value:string):boolean;
begin
   Result:=True;
   CheckIniFile.WriteString(SectionName, KeyName, Value);
end;

function TIniManager.WritePrivateInt(SectionName,KeyName:string;Value:Integer):boolean;
begin
   Result:=True;
   CheckIniFile.WriteInteger(SectionName, KeyName, Value);
end;

procedure TIniManager.GetAllValues;
var i: integer;
begin
  for i:=0 to FBindList.Count-1 do
  begin
    if Assigned(FBindList.Items[i]) then GetValueByID(i);
    Application.ProcessMessages;
  end;
end;

procedure TIniManager.GetValue(pValue: pointer);
begin
  GetValueByID(GetValueID(pValue));
end;

procedure TIniManager.WriteAllValues;
var i: integer;
begin
  for i:=0 to FBindList.Count-1 do
    if Assigned(FBindList.Items[i]) then WriteValueByID(i);
end;

procedure TIniManager.WriteValue(pValue: pointer);
begin
   WriteValueByID(GetValueID(pValue));
end;

procedure TIniManager.Clear;
var i:integer;
begin
  for i:=0 to FBindList.Count-1 do
     if Assigned(FBindList.Items[i]) then
        TIniBindRec(FBindList.Items[i]).Free;
  FBindList.Clear;
end;

procedure TIniManager.GetValueByID(ID: integer);
begin
  if (ID>=FBindList.Count)or(not Assigned(FBindList.Items[ID])) then exit;
  with TIniBindRec(FBindList.Items[ID]) do
  begin
    case FBindType of
      btInteger, btSmallInt, btBoolean:
         VarAsInteger:=GetPrivateInt(FSectionName, FKeyName, StrToInt(FDefaultValue));
      btString, btChar, btCharBuf, btDouble:
         VarAsString :=GetPrivateString(FSectionName, FKeyName, FDefaultValue);
    end;
  end;
end;

procedure TIniManager.WriteValueByID(ID: integer);
begin
  if (ID>=FBindList.Count)or(not Assigned(FBindList.Items[ID])) then exit;
  with TIniBindRec(FBindList.Items[ID]) do
  begin
    case FBindType of
      btInteger, btBoolean, btSmallInt:
        WritePrivateInt(FSectionName, FKeyName, VarAsInteger);
      btString, btChar, btCharBuf, btDouble:
        WritePrivateString(FSectionName, FKeyName, VarAsString);
    end;
  end;
end;

procedure TIniManager.StopBindVariable(pValue: pointer);
var i: integer;
begin
  i:=GetValueID(pValue);
  if (i<FBindList.Count) and (i>=0) then
  begin
    if Assigned(FBindList.Items[i]) then
       TIniBindRec(FBindList.Items[i]).Free;
    FBindList.Delete(i);
  end;
end;

function TIniManager.GetValueID(pValue: pointer): integer;
begin
  Result:=0;
  while (Result<FBindList.Count) do
    if Assigned(FBindList.Items[Result]) and
       (TIniBindRec(FBindList.Items[Result]).FValue=pValue) then exit
    else inc(Result);
end;

procedure TIniManager.BindProperty(Instance: TObject; PropertyName,
  SectionName, KeyName, DefaultValue: string);
begin
  if GetPropID(Instance, PropertyName)<0 then
     FBindList.Add(TIniBindRec.Create(btProp, nil, Instance, PropertyName, SectionName, KeyName, DefaultValue));
end;

procedure TIniManager.GetAll;
begin
  GetAllProps;
  GetAllValues;
end;

procedure TIniManager.GetAllProps;
var i: integer;
begin
  for i:=0 to FBindList.Count-1 do
    if Assigned(FBindList.Items[i]) and
      (TIniBindRec(FBindList.Items[i]).FBindType=btProp) and
      Assigned(TIniBindRec(FBindList.Items[i]).FInstance) then
    begin
      GetPropByID(i);
      Application.ProcessMessages;
    end;
end;

procedure TIniManager.StopBindProperty(Instance: TObject;
  PropertyName: string);
var i: integer;
begin
  i:=GetPropID(Instance, PropertyName);
  if (i>=0)and(i<FBindList.Count) then
  begin
    if Assigned(FBindList.Items[i]) then
       TIniBindRec(FBindList.Items[i]).Free;
    FBindList.Delete(i);
  end;
end;

procedure TIniManager.WriteAll;
begin
  WriteAllProps;
  WriteAllValues;
end;

procedure TIniManager.WriteAllProps;
var i: integer;
begin
  for i:=0 to FBindList.Count-1 do
    if Assigned(FBindList.Items[i]) and
       (TIniBindRec(FBindList.Items[i]).FBindType=btProp) and
       Assigned(TIniBindRec(FBindList.Items[i]).FInstance) then WritePropByID(i);
end;

function TIniManager.GetPropID(Instance: TObject;
  PropertyName: string): integer;
begin
  Result:=0;
  while (Result<FBindList.Count) and (not (
         Assigned(FBindList.Items[Result])and
        (TIniBindRec(FBindList.Items[Result]).FBindType=btProp)and
        (TIniBindRec(FBindList.Items[Result]).FInstance=pointer(Instance))and
        (UpperCase(TIniBindRec(FBindList.Items[Result]).FPropertyName)=UpperCase(PropertyName)) ))
    do inc(Result);
  if Result>=FBindList.Count then Result:=-1;
end;

procedure TIniManager.WritePropByID(ID: integer);
var PropInfo: PPropInfo;
    PropType: PTypeInfo;
begin
  if (ID>=0) and (ID<FBindList.Count) and Assigned(FBindList.Items[ID]) and
     (TIniBindRec(FBindList.Items[ID]).FBindType=btProp)  then
    with TIniBindRec(FBindList.Items[ID]) do
      if FindProperty(FInstance, FPropertyName, PropInfo) then
      begin
        PropType:=PropInfo^.PropType^;
        case PropType^.Kind of
          tkInteger, tkChar, tkEnumeration, tkSet:
            WritePrivateInt(FSectionName, FKeyName, GetOrdProp(Instance, PropInfo));
          tkFloat:
            WritePrivateString(FSectionName, FKeyName, FloatToStr(GetFloatProp(Instance, PropInfo)));
          tkString, tkLString, tkWString:
            WritePrivateString(FSectionName, FKeyName, GetStrProp(Instance, PropInfo));
        end;
      end;
end;

procedure TIniManager.GetPropByID(ID: integer);
var PropInfo: PPropInfo;
    PropType: PTypeInfo;
begin
  if (ID>=0) and (ID<FBindList.Count) and Assigned(FBindList.Items[ID]) and
     (TIniBindRec(FBindList.Items[ID]).FBindType=btProp)  then
    with TIniBindRec(FBindList.Items[ID]) do
      if FindProperty(FInstance, FPropertyName, PropInfo) then
      begin
        PropType:=PropInfo^.PropType^;
        case PropType^.Kind of
          tkInteger, tkChar, tkEnumeration, tkSet:
            SetOrdProp(Instance, PropInfo, GetPrivateInt(FSectionName, FKeyName, StrToInt(FDefaultValue)));
          tkFloat:
            SetFloatProp(Instance, PropInfo, StrToFloat(GetPrivateString(FSectionName, FKeyName, FDefaultValue)));
          tkString, tkLString, tkWString:
            SetStrProp(Instance, PropInfo, GetPrivateString(FSectionName, FKeyName, FDefaultValue));
        end;
      end;
end;

procedure TIniManager.GetProp(Instance: TObject; PropertyName: string);
begin
   GetPropByID(GetPropID(Instance, PropertyName));
end;

procedure TIniManager.WriteProp(Instance: TObject; PropertyName: string);
begin
   WritePropByID(GetPropID(Instance, PropertyName));
end;

procedure TIniManager.Flush;
begin
  CheckIniFile.UpdateFile;
end;

procedure TIniManager.RecentFilesAdd(aCaption: String);
var NewItem: TMenuItem;
    i: integer;
begin
  if (aCaption<>'')and(aCaption[Length(aCaption)]<>'\') then
  begin
    NewItem := TMenuItem.Create(FRecentFilesMenuItem);
    NewItem.Caption := aCaption;
    NewItem.Tag := 0;
    NewItem.OnClick := RecentFileItemClick;
    try
      FRecentFilesMenuItem.Insert(0, NewItem);
      FRecentFilesList.Insert(0, aCaption);
    except
      NewItem.Free;
    end;
    i:=1;
    while i<FRecentFilesMenuItem.Count do
      if AnsiUpperCase(FRecentFilesMenuItem.Items[i].Caption)=AnsiUpperCase(aCaption) then
      begin
        FRecentFilesMenuItem.Delete(i);
        FRecentFilesList.Delete(i);
      end
      else
      begin
        FRecentFilesMenuItem.Items[i].Tag:=i;
        inc(i);
      end;
    while (FRecentFilesMenuItem.Count>FRecentFilesDropDownMax) do
    begin
      FRecentFilesMenuItem.Delete(FRecentFilesMenuItem.Count-1);
      FRecentFilesList.Delete(FRecentFilesMenuItem.Count-1);
    end;
  end;
end;                                    

function TIniManager.GetRecentFilesSection: integer;
var NewItem: TMenuItem;
    i: integer;
begin
   Result:=0;
   GetPrivateSection(FRecentFilesSectionName, FRecentFilesList);
   for i:=FRecentFilesList.Count downto FRecentFilesDropDownMax+1 do
       FRecentFilesList.Delete(i-1);
   if not Assigned(FRecentFilesMenuItem) then exit;
   for i:=0 to FRecentFilesList.Count-1 do
   begin
     NewItem := TMenuItem.Create(FRecentFilesMenuItem);
     NewItem.Caption := FRecentFilesList.Strings[i];
     NewItem.Tag := i;
     NewItem.OnClick := FRecentFilesItemClick;
     FRecentFilesMenuItem.Add(NewItem);
   end;
end;

function TIniManager.WriteRecentFilesSection: boolean;
begin
  Result:=WritePrivateSection(FRecentFilesSectionName, FRecentFilesList);
end;

function TIniManager.WritePrivateSection(SectionName:string;
  SL: TStringList): boolean;
 procedure WriteSection(const Section: string;
  Strings: TStrings);
 const
  BufSize = 16384;
 var
  Buffer: PChar;
  i, ii: integer;
  res: boolean;
 begin
  GetMem(Buffer, BufSize);
  try
    i:=0;
    Buffer[0]:=#0; Buffer[1]:=#0;
    WritePrivateProfileSection(PChar(Section), nil, PChar(FIniFile.FileName));  // clear in file
    for ii:=0 to Strings.Count-1 do
    begin
      StrPCopy(@Buffer[i], Strings.Strings[ii]);
      i:=i+Length(Strings.Strings[ii])+1;
    end;
    Buffer[i]:=#0;
    res:=WritePrivateProfileSection(PChar(Section), Buffer, PChar(FIniFile.FileName));
    if not Res then raise Exception.Create('IniFile Write Error: + FIniFile.FileName');
  finally
    FreeMem(Buffer, BufSize);
  end;
 end;
begin
  Result:=True;
  CheckIniFile;
  WriteSection(SectionName, SL);
end;

procedure TIniManager.RecentFileItemClick(Sender: TObject);
var i, tg: integer;
begin
  If Assigned(FRecentFilesItemClick) then FRecentFilesItemClick(Sender);
  if Assigned(Sender)and(Sender is TMenuItem) then
  begin
    tg:=(Sender as TMenuItem).Tag;
    FRecentFilesList.Delete( tg );
    FRecentFilesMenuItem.Delete( tg );
    (Sender as TMenuItem).Free;
    for i:=tg to FRecentFilesMenuItem.Count-1 do
       FRecentFilesMenuItem.Items[i].Tag:=i;
  end;
end;

procedure TIniManager.SetRecentFilesMenuItem(const Value: TMenuItem);
begin
  FRecentFilesMenuItem := Value;
  if Assigned(Value) then Value.AutoHotkeys:=maManual;
end;

function TIniManager.CheckIniFile: TAsofIniFile;
begin
  Result:=nil;
  if Assigned(FIniFile) then Result:=FIniFile
  else raise Exception.Create('IniManager Error'#13#13'IniFileObj not assigned');
end;

procedure TIniManager.LinkPropertyToVariable(Instance: TObject;
  PropertyName: string; pVariable: pointer);
var ID:Integer;
begin
  ID:=GetValueID(pVariable);
  if (ID<0) or (ID>=FBindList.Count) then ID:=GetPropID(Instance, PropertyName);
  if (ID>=0) and (ID<FBindList.Count) then
  begin
    TIniBindRec(FBindList.Items[ID]).FValue:=pVariable;
    TIniBindRec(FBindList.Items[ID]).FInstance:=Instance;
    TIniBindRec(FBindList.Items[ID]).FPropertyName:=PropertyName;
  end;
end;

procedure TIniManager.LoadLinkedPropsFromIni;
var ID:Integer;
    BT: TIniBindType;
begin
  for ID:=0 to FBindList.Count-1 do
    if Assigned(FBindList.Items[ID]) then
    with TIniBindRec(FBindList.Items[ID]) do
      if (FBindType<>btProp) and Assigned(FValue) and
         Assigned(FInstance) and (FPropertyName<>'') then
      begin
        BT:=FBindType;
        try
          FBindType:=btProp;
          GetPropByID(ID);
        finally
          FBindType:=BT;
        end;
      end;
end;

procedure TIniManager.LoadLinkedPropsFromVar;
type TPChar = ^Char;
var ID:Integer;
begin
  for ID:=0 to FBindList.Count-1 do
    if Assigned(FBindList.Items[ID]) then
    with TIniBindRec(FBindList.Items[ID]) do
      if Assigned(FValue) and Assigned(FInstance) and (FPropertyName<>'') then
      begin
        case FBindType of
          btInteger:  PropAsInteger:=PInteger(FValue)^;
          btSmallInt: PropAsInteger:=PSmallInt(FValue)^;
          btBoolean:  PropAsInteger:=Integer(PBoolean(FValue)^);
          btString:   PropAsString:=PString(FValue)^;
          btChar:     PropAsString:=TPChar(FValue)^;
          btCharBuf:  PropAsString:=StrPas(Pchar(FValue));
          btDouble:   PropAsFloat:=PDouble(FValue)^;
        end;
      end;
end;

procedure TIniManager.SaveLinkedPropsToIni;
var ID:Integer;
    BT: TIniBindType;
begin
  for ID:=0 to FBindList.Count-1 do
    if Assigned(FBindList.Items[ID]) then
    with TIniBindRec(FBindList.Items[ID]) do
      if (FBindType<>btProp) and Assigned(FValue) and
         Assigned(FInstance) and (FPropertyName<>'') then
      begin
        BT:=FBindType;
        try
          FBindType:=btProp;
          WritePropByID(ID);
        finally
          FBindType:=BT;
        end;
      end;
end;

procedure TIniManager.SaveLinkedPropsToVar;
type TPChar = ^Char;
var ID:Integer;
begin
  for ID:=0 to FBindList.Count-1 do
    if Assigned(FBindList.Items[ID]) then
    with TIniBindRec(FBindList.Items[ID]) do
      if Assigned(FValue) and Assigned(FInstance) and (FPropertyName<>'') then
      begin
        case FBindType of
          btInteger:  PInteger(FValue)^  := PropAsInteger;
          btSmallInt: PSmallInt(FValue)^ := PropAsInteger;
          btBoolean:  PBoolean(FValue)^  := boolean(PropAsInteger);
          btDouble:   PDouble(FValue)^   :=PropAsFloat;
          btString:   PString(FValue)^   :=PropAsString;
          btChar:     TPChar(FValue)^    :=PropAsString[1];
          btCharBuf:  StrPCopy(Pchar(FValue),PropAsString);
        end;
      end;
end;

function TIniManager.IsLinkedPropsEqualToVars: boolean;
var ID:Integer;
begin
  ID:=0;
  Result:=True;
  while (ID<FBindList.Count) and Result do
    if Assigned(FBindList.Items[ID]) then
    with TIniBindRec(FBindList.Items[ID]) do
    begin
      if (FBindType<>btProp) and Assigned(FValue) and
         Assigned(FInstance) and (FPropertyName<>'') then
         Result:=VarAsString=PropAsString;
      inc(ID);
    end;
end;

procedure TIniManager.UnLinkPropertyFromVariable(pVariable: pointer);
begin

end;

end.

