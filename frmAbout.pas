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


unit frmAbout;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TAboutBox = class(TForm)
    Button1: TButton;
    Panel1: TPanel;
    Image1: TImage;
    TitleLabel: TLabel;
    Label1: TLabel;
    VersionLabel: TLabel;
    ServerMemo: TMemo;
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox = nil;

implementation

{$R *.DFM}

procedure TAboutBox.FormActivate(Sender: TObject);
begin
  OnActivate:=nil;
  TitleLabel.Caption:=Application.Title;
  Image1.Picture.Icon.Assign(Application.Icon);
end;

end.
