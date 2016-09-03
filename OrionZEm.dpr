/////////////////////////////////////////////////////////////////////////
//                                                                     //
//      Orion/Z (Orion-128 + Z80-CARD-II) emulator, version 1.9        //
//                                                                     //
//   Author: Sergey A.        <a-s-m@km.ru>                            //
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

program OrionZEm;

uses
  Forms,
  mainwin in 'mainwin.pas' {frmMain},
  modWaveOut in 'modWaveOut.pas',
  modZ80 in 'modZ80.pas',
  ScrThrd in 'ScrThrd.pas',
  mod8255 in 'mod8255.pas',
  mod1793 in 'mod1793.pas',
  modOrion in 'modOrion.pas',
  mod146818 in 'mod146818.pas',
  settswin in 'settswin.pas' {frmSetts},
  uIniMngr in 'uIniMngr.pas',
  frmAbout in 'frmAbout.pas' {AboutBox},
  modAY8912 in 'modAY8912.pas',
  modHDD in 'modHDD.pas',
  HDDUtils in 'HDDUtils.pas',
  modF600 in 'modF600.pas',
  frmSaveOrd in 'frmSaveOrd.pas' {FrmSave},
  uPackOdi in 'UTILS\OdiWcx-OhiWcx\uPackOdi.pas',
  wcxhead in 'UTILS\OdiWcx-OhiWcx\wcxhead.pas',
  frNewVal in 'frNewVal.pas' {frmNewVal},
  modSD in 'modSD.pas',
  mod232 in 'mod232.pas',
  EthThrd in 'EthThrd.pas',
  mod8019as in 'mod8019as.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
