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

unit frNewVal;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask;

const
  strictMask16: string = 'AAAA;1;_';
  freeMask16: string = 'aaaa;1;_';
  strictMask8: string = 'AA;1;_';
  freeMask8: string = 'aa;1;_';

type
  TfrmNewVal = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    meValue: TMaskEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  function GetValue16(title: string; DefaultValue: integer; var Value:string; var Mask: integer): integer;

var
  frmNewVal: TfrmNewVal;

implementation

uses mainwin;

{$R *.DFM}

function GetValue16(title: string; DefaultValue: integer; var Value:string; var Mask: integer): integer;
begin
  frmNewVal:=TfrmNewVal.Create(Application);
  if (Assigned(frmNewVal)) then with frmNewVal do
  try
    Caption:=Caption+title;
    meValue.EditMask:=Value;
    if pos(';', Value)=3 then
      meValue.Text:=IntToHex(DefaultValue and $FF, 2)
    else
      meValue.Text:=IntToHex(DefaultValue and $FFFF, 4);
    if ShowModal()=mrOK then
    begin
      Value:=meValue.Text;
      Result:=GetHexMasked16(meValue.Text, Mask);
    end
    else
    begin
      Value:='';
      Result:=DefaultValue;
      Mask:=0;
    end;
    Free;
  finally
    frmNewVal:=nil;
  end;
end;


end.
