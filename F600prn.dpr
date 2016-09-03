/////////////////////////////////////////////////////////////////////////
//                                                                     //
//   Printer Plugin for Orion/Z (Orion-128 + Z80-CARD-II) emulator,    //
//                         version 1.0                                 //
//                                                                     //
//                                                                     //
//             Author: Sergey A.        <a-s-m@km.ru>                  //
//                                                                     //
//             Copyright (C)2006-2016 Sergey A.                        //
//                                                                     //
//   This program is free software; you can redistribute it and/or     //
//                  modify it in any ways.                             //
//   This program is distributed "AS IS" in the hope that it will be   //
//   useful, but WITHOUT ANY WARRANTY; without even the implied        //
//   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  //
//                                                                     //
/////////////////////////////////////////////////////////////////////////

library F600prn;

//{$DEFINE ENTIRE_APP}

uses
  Windows,
  Messages,
  SysUtils,
{$IFDEF ENTIRE_APP}
  Forms,
{$ENDIF}
  modF600,
  F600prnf in 'F600prnf.pas' {frmF600prnf},
  F600prnp in 'F600prnp.pas' {frmF600prnp},
  F600common in 'F600common.pas';

// print to prn functions

{$R *.RES}

const
  DllFunctions:string='"Port F600 Printer","0","Port F600 print to file","1"'#0;
  DllFuncCount=2;

{$IFDEF ENTIRE_APP}
var OldApp:Tapplication;
{$ENDIF}

function MainFunc(fIndex:LongInt; fType:LongInt; var fDataPtr: pointer): LongInt; stdcall;
begin
  Result:=0;
  case fType of
    F600Func_Load:      begin        // when module loaded
                          GetIniSettings;
                        end;
    F600Func_Flush,
    F600Func_UnLoad:    begin        // when module unloaded
                          case fIndex of
                            0: Result:=flush_prnp;
                            1: Result:=flush_prnf
                            else result:=0;
                          end;
                        end;
    F600Func_Configure: with PApplicationParams(fDataPtr)^ do
                        begin        // when user press 'configure plugin' button
{$IFDEF ENTIRE_APP}
                          OldApp:=Application;
                          Application.Handle:=AppHandle;
                          Application.Icon.Handle:=aIcon;
{$ENDIF}
                          case fIndex of
                            0: Result:=pConfigure;
                            1: Result:=fConfigure
                            else result:=0;
                          end;
{$IFDEF ENTIRE_APP}
                          Application:=OldApp;
{$ENDIF}
                        end;
    F600Func_Enumerate: begin        // when OrionZEm read plugin for content (functions list - comma separated pairs "Title","index",...)
                          fDataPtr:=@DllFunctions[1];
                          Result:=DllFuncCount;
                        end;
    else case fIndex of              // when printing via parallel port 8255
           0: Result:=prnbuf(fType, fDataPtr, flush_prnp);
           1: Result:=prnbuf(fType, fDataPtr, flush_prnf)
           else result:=0;
         end;
  end;
end;

exports
  MainFunc index 1;

begin
  frmF600prnp:=nil;
  frmF600prnf:=nil;
end.


