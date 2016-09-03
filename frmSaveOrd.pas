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

unit frmSaveOrd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFrmSave = class(TForm)
    lbOrdFiles: TListBox;
    BtnSave: TButton;
    BtnCancel: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmSave: TFrmSave;

implementation

{$R *.DFM}

end.
