/////////////////////////////////////////////////////////////////////////
//                                                                     //
//   Orion/Z (Orion-128 + Z80-CARD-II) emulator.                       //
//                                                                     //
//   Addon: TotalCommander archiver (WCX) plugin for serving OHI files //
//          (Orion HDD Image files). Allow copy/extract CP/M files     //
//          to/from OHI "hdd image" such simple as processing any      //
//          archives in TotalCommander interface. PC MBR partitioning  //
//          scheme supported.    Version 1.01.                         //
//                                                                     //
//   How to install this plugin (32 bit only) in TotalCommander:       //
//          1. Unzip odi.wcx, system.bin to any directory              //
//                (usually c:\wincmd\Plugins)                          //
//          2. In Windows Commander 5.5 (or newer), choose             //
//                Configuration - Options                              //
//          3. Open the 'Packer' page                                  //
//          4. Click 'Configure packer extension DLLs'                 //
//          5. type the "ODI" extension                                //
//          6. Click 'new type', and select the  odi.wcx               //
//          7. Click OK.  Click OK                                     //
//          8. Repeat steps 2-7 for other supported (specified in      //
//               ODI.INI) formats - such as TRD, DSK, etc.             //
//                                                                     //
//   How to install this plugin (32 bit only) in Far Manager:          //
//          0. Install wcx.dll (plugin allowing TC WCX-plugins usage   //
//             in Far) to                                              //
//                   {FAR_DIR}\Plugins\Multiarc\Formats\WCX\           //
//             Install wcx.fmt to                                      //
//                   {FAR_DIR}\Plugins\Multiarc\Formats\               //
//          1. Unzip odi.wcx, system.bin to                            //
//                   {FAR_DIR}\Plugins\Multiarc\Formats\WCX\           //
//          1.2. To Support any other CP/M format (specified in INI),  //
//               just copy ODI.WCX, ODI.INI to files with              //
//               file name correcponding to format extension (for      //
//               example:  DSK.WCX, DSK.INI ;  TRD.WCX, TRD.INI) to    //
//                   {FAR_DIR}\Plugins\Multiarc\Formats\WCX\           //
//          2. Restart Far                                             //
//                                                                     //
//   Author: Sergey A. <a-s-m@km.ru>                                   //
//                                                                     //
//                                                                     //
//   Copyright (C)2016 Sergey A.                                       //
//                                                                     //
//   This program is free software; you can redistribute it and/or     //
//                  modify it in any ways.                             //
//   This program is distributed "AS IS" in the hope that it will be   //
//   useful, but WITHOUT ANY WARRANTY; without even the implied        //
//   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  //
//                                                                     //
/////////////////////////////////////////////////////////////////////////

library Ohi;

//{$APPTYPE CONSOLE}

uses
  uPackOhi in 'uPackOhi.pas';

{$E wcx}

{$R *.res}

exports
	OpenArchive,
	ReadHeader,
	ProcessFile,
	CloseArchive,
	PackFiles,
	DeleteFiles,
	GetPackerCaps,
//	PackSetDefaultParams,
	ConfigurePacker,
	SetProcessDataProc,
	CanYouHandleThisFile,
        SetChangeVolProc;

end.
