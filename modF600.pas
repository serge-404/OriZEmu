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

unit modF600;

{ interface definitions for plugins emulating devices at extension port F600 }

interface

Uses Windows, Sysutils, Forms;

const
  F600FuncName='MainFunc';         // dll common entry point function name
  F600Func_Load     =0;            // when module loaded
  F600Func_UnLoad   =1;            // when module unloaded
  F600Func_Configure=2;            // when user press 'configure plugin' button
  F600Func_Enumerate=3;            // when OrionZEm read plugin for content (functions list - comma separated pairs "Title","index",...)
  F600Func_Flush    =4;            // executed every second
  F600Func_PA_in    =5;            // Port A signal read
  F600Func_PA_out   =6;            // Port A signal write
  F600Func_PB_in    =7;            // Port B signal read
  F600Func_PB_out   =8;            // Port B signal write
  F600Func_PC_in    =9;            // Port C signal read
  F600Func_PC_out  =10;            // Port C signal write
  F600Func_PD_out  =11;            // Port D signal out (8255 cfg register)

type
  TF600Function=function(fIndex:LongInt; fType:LongInt; var fDataPtr: pointer): LongInt; stdcall;  // dll common entry point type function type

  TApplicationParams=packed record
                       AppHandle:THandle;
                       aIcon:HIcon;
                       MainInstance:LongInt;
                       Wnd:HWND;
                     end;
  PApplicationParams=^TApplicationParams;

implementation

end.
