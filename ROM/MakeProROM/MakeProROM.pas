unit MakeProROM;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TForm1 = class(TForm)
    btnMake: TButton;
    btnCancel: TButton;
    btnORDFiles: TButton;
    lbORDFiles: TListBox;
    edROM2File: TEdit;
    bntROM2File: TButton;
    OpenROM2Dialog: TOpenDialog;
    OpenORDDialog: TOpenDialog;
    bbtnUP: TBitBtn;
    bbtnDown: TBitBtn;
    btnClean: TButton;
    procedure btnORDFilesClick(Sender: TObject);
    procedure btnMakeClick(Sender: TObject);
    procedure bntROM2FileClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure bbtnUPClick(Sender: TObject);
    procedure bbtnDownClick(Sender: TObject);
    procedure btnCleanClick(Sender: TObject);
  private
    function MakeCommon(FilesCnt:integer):boolean;
  public
    { Public declarations }
  end;

const
  ROM2Loaded: boolean = false;

var
  Form1: TForm1;
  ROM2BIOS: array [$0..$2000] of char;
  sst: string;
  cnt: integer;

implementation

{$R *.DFM}

procedure TForm1.bntROM2FileClick(Sender: TObject);
begin
 with OpenROM2Dialog do
   if OpenROM2Dialog.Execute then
     If FileExists(filename) then
       edROM2File.Text:=trim(filename);
end;

procedure TForm1.btnORDFilesClick(Sender: TObject);
var n:integer;
begin
 with OpenORDDialog do
   if OpenORDDialog.Execute then
   begin
     lbORDFiles.Items.Clear;
     with OpenORDDialog.Files do
       for n:=0 to Count-1 do
         If FileExists(filename) then
           lbORDFiles.Items.Add(Strings[n]);
   end;
end;

function MIN(a,b:integer):integer;
begin
 if a<b then Result:=a else Result:=b;
end;

function TForm1.MakeCommon(FilesCnt:integer):boolean;
var FStream: TFileStream;
    ROM2BIOS: array [$0..$2000] of char;
    i,next_ordos, datasize:integer;
begin
  cnt:=0;
  sst:='';
  next_ordos:=0;
  Result:=False;
  FillChar(ROM2BIOS, SizeOf(ROM2BIOS), $FF);
  i:=0;
  while i<FilesCnt do begin
    if FileExists(lbORDFiles.Items[i]) and (next_ordos<sizeof(ROM2BIOS)-33) then
    begin
      FStream:=nil;
      FStream:=TFileStream.Create(lbORDFiles.Items[i], fmOpenRead or fmShareDenyWrite);
      if FStream.Read(ROM2BIOS[next_ordos], 16)=16 then         // read ordos header
      begin
        datasize:=PWord(@ROM2BIOS[next_ordos+10])^;
        if (datasize<sizeof(ROM2BIOS)-next_ordos-17) then
        begin
          sst:=sst+lbORDFiles.Items[i]+#13#10;
          FStream.Read(ROM2BIOS[next_ordos+16], datasize);
          next_ordos:=next_ordos + datasize + 16;                         // 16 = ordos_header size
          inc(cnt);
        end
        else ROM2BIOS[next_ordos]:=char($FF);
      end;
      FStream.Free;
    end;
    inc(i);
  end;
  if FileExists(trim(edROM2File.Text)) then begin
    FStream:=TFileStream.Create(trim(edROM2File.Text), fmOpenReadWrite or fmShareDenyWrite);
    FStream.Seek($4D80, soFromBeginning	);
    FStream.WriteBuffer(ROM2BIOS[0], $1270);
    FStream.Seek($1270, soFromBeginning	);
    FStream.WriteBuffer(ROM2BIOS[$1270], $D80);
    FStream.Free;
    Result:=True;
  end
  else
    Application.MessageBox(PChar('Not found ROM2 PRO BIOS file: '+edROM2File.Text), 'Error', MB_OK+MB_ICONWARNING);
end;

procedure TForm1.btnMakeClick(Sender: TObject);
begin
  if MakeCommon(lbORDFiles.Items.Count) then
    Application.MessageBox(StrFmt(ROM2BIOS,'%d Ordos files -'#13#10'%s'#13#10'writted to ROM2 PRO BIOS file'#13#10'%s'#13#10#10'at positions 4D80h..5FEFh, 1270h..1FEFh', [cnt, PChar(sst), PChar(edROM2File.Text)]), 'Done', MB_OK);
end;

procedure TForm1.btnCleanClick(Sender: TObject);
begin
  if MakeCommon(0) then
    Application.MessageBox(StrFmt(ROM2BIOS,'ROM2 PRO BIOS file'#13#10'%s'#13#10#10'cleaned(FF) at positions 4D80h..5FEFh, 1270h..1FEFh', [PChar(edROM2File.Text)]), 'Done', MB_OK);
end;

procedure TForm1.btnCancelClick(Sender: TObject);
begin
  Form1.Close;
end;

procedure TForm1.bbtnUPClick(Sender: TObject);
var st:string;
begin
  with lbORDFiles do if (ItemIndex>0) then
  begin
    st:=Items[ItemIndex-1];
    Items[ItemIndex-1]:=Items[ItemIndex];
    Items[ItemIndex]:=st;
    ItemIndex:=ItemIndex-1;
    Update;
  end;
end;

procedure TForm1.bbtnDownClick(Sender: TObject);
var st:string;
begin
  with lbORDFiles do if (ItemIndex>=0)and(ItemIndex<Items.Count-1) then
  begin
    st:=Items[ItemIndex];
    Items[ItemIndex]:=Items[ItemIndex+1];
    Items[ItemIndex+1]:=st;
    ItemIndex:=ItemIndex+1;
    Update;
  end;
end;

end.
