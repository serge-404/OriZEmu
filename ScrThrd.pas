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


unit ScrThrd;

interface

{
               1.2. ОРГАНИЗАЦИЯ ЭКРАННОЙ ПАМЯТИ
               --------------------------------

     Экранная память располагается в 0 и 1 страницах ОЗУ,  при-
чем количество экранов и распределение сегментов в них  зависит
от текущего цветового режима, задаваемого разрядами порта 0F8H:

     D4  D3  D2  D1  D0
     ------------------
     0   x   0   0   0   - монохромный, палитра 1
     0   x   0   0   1   - монохромный, палитра 2
     0   x   0   1   x   - запрет видеосигнала
     0   x   1   0   0   - 2-битный (4-цветный), палитра 1
     0   x   1   0   1   - 2-битный (4-цветный), палитра 2
     0   x   1   1   x   - 16-цветный с групповым кодированием
     0   1   1   1   x   - псевдоцветной (цвет -  в порт 0FCH)
     1   x   0   x   x   - 3-битный (8-цветный RGB)
     1   x   1   x   x   - 4-битный (16-цветный RGBI)

     В монохромном  режиме  палитре  1 соответствует комбинация
цветов - (черный,  зеленый),  палитре 2 - (белый,  зеленый).  В
4-цветном  (2-х битовом) режиме палитре 1 соответствуют цвета -
(черный,  синий,  зеленый, красный), палитре 2 - (белый, синий,
зеленый, красный).
     Код палитры для псевдоцветного режима записывается в  порт
с адресом 0FCH.
     Выбор на отображение одного из 4-х экранов выполняется пу-
тем записи номера экрана в порт 0FAH:

     D0 \ номер экрана
     D1 /
     D6 - выключение регенерации ОЗУ
     D7 - включение широкого экрана

     Разряды D2-D5 являются резервными.

     Если разряд D7 установлен в единицу, то ширина экрана сос-
тавляет 512 точек (64 байта), что при высоте 256 байт соответс-
твует объему памяти 16 Кбайт. В противном случае экранная плос-
кость  ОЗУ имеет ширину 384 точки (48 байт) и занимает объем 12
Кбайт.
     В 3-х битном и 4-х битном (EGA-режим) цветовых режимах до-
пускается использование только двух экранов,  поэтому разряд D0
порта 0FAH игнорируется.
     Рассмотрим распределение сегментов экранного ОЗУ в различ-
ных цветовых режимах.

           1.2.3 МОНОХРОМНЫЙ И ПСЕВДОЦВЕТНОЙ РЕЖИМЫ
           ----------------------------------------

     В монохромном и псевдоцветном режимах возможно использова-
ние  до  4-х  экранов,  занимающих только сегменты 0-й страницы
ОЗУ:

                 Стр.0         Экран 12 К      Экран 16 К
               --------¬      ------------    ------------
     Экран 0 ->¦   3   ¦      C000H..EFFFH    C000H..FFFFH
               ¦=======¦
     Экран 1 ->¦   2   ¦      8000H..AFFFH    8000H..BFFFH
               ¦=======¦
     Экран 2 ->¦   1   ¦      4000H..6FFFH    4000H..7FFFH
               ¦=======¦
     Экран 3 ->¦   0   ¦      0000H..2FFFH    0000H..3FFFH
               L--------

     В монохромном  режиме  единичному значению некоторого бита
экранного сегмента ОЗУ соответствует засветка изображаемой точ-
ки, нулевому - гашение.
     В псевдоцветном  режиме цвет отображаемых точек зависит от
кода палитры,  записанного в порт 0FCH. Старшие 4 бита значения
этого  порта  определяют один из 16 цветов фона (для погашенных
точек), младшие 4 бита - один из 16 цветов переднего плана (для
засвеченных точек).
     Заметим, что при широком экране-0  область  0F000H..0FFFFH
экрана (не путать с системной областью 0F000H..0FFFFH) доступна
только через окно.  Прямой доступ к экрану возможен  только  по
адресам  0C000-0EFFFH.  Это относится ко всем цветовым режимам.


                    1.2.4.  4-ЦВЕТНЫЙ РЕЖИМ
                    -----------------------

     В 4-цветном  (2-битном)  режиме  цвет  каждой отображаемой
точки зависит от соответствующих битов двух экранных плоскостей
(сегментов), находящихся в страницах 0 и 1 ОЗУ:


                 Стр.0   Стр.1
               --------T-------¬
     Экран 0 ->¦   3   ¦   7   ¦
               ¦=======+=======¦
     Экран 1 ->¦   2   ¦   6   ¦
               ¦=======+=======¦
     Экран 2 ->¦   1   ¦   5   ¦
               ¦=======+=======¦
     Экран 3 ->¦   0   ¦   4   ¦
               L-------+--------
                   L--¬ ----

                      0 0  ->  черный (белый)
                      0 1  ->  красный
                      1 0  ->  зеленый
                      1 1  ->  синий


             1.2.5.  8-ЦВЕТНЫЙ и 16-ЦВЕТНЫЙ РЕЖИМЫ
             -------------------------------------

     Это новый графический режим.  Функционально он  тождествен
EGA  режиму на IBM PC AT (был широко распространен на 286 моде-
лях).  В 8-цветном (3-битном) и 16-цветном  (4-битном)  режимах
для  формирования  отображаемой  точки в каждом из двух экранов
используются соответственно 3 и 4 плоскости экранного ОЗУ:

                 Стр.0   Стр.1
               --------T-------¬
               ¦  3 (G)¦  7 (I)¦
     Экран 0 ->+-------+-------+
               ¦  2 (R)¦  6 (B)¦
               ¦=======+=======¦
               ¦  1 (G)¦  5 (I)¦
     Экран 1 ->+-------+-------+
               ¦  0 (R)¦  4 (B)¦
               L-------+--------

     Сегментам 3  и  1 соответствует зеленый цвет (G),  2 и 0 -
красный (R), 6 и 4 - синий (B), 7 и 5 (в 3-битном режиме не ис-
пользуются) - управление яркостью (I).

     Путем записи  комбинации  битов в соответствующие сегменты
экрана можно получить точку заданного цвета.


          1.2.6. РЕЖИМ С ГРУППОВЫМ КОДИРОВАНИЕМ ЦВЕТА
          -------------------------------------------

     В 16-цветном режиме с групповым кодированием каждый из 4-х
экранов  формируется  из содержимого двух сегментов памяти:  из
плоскости изображения (0 страница ОЗУ) и плоскости цветовых ат-
рибутов  (1 страница ОЗУ),  причем восьми соседним точкам плос-
кости изображения, расположенным в пределах одного байта, соот-
ветствует один байт из плоскости цветовых атрибутов.

     Старшие 4  бита в байте цветового атрибута определяют цвет
фона (для погашенных точек),  младшие 4 бита -  цвет  переднего
плана  (для засвеченных точек) в пределах одного экранного бай-
та.

                 Стр.0    Стр.1
                (изобр)  (цвет)
               --------T-------¬
     Экран 0 ->¦   3   ¦   7   ¦
               ¦=======+=======¦
     Экран 1 ->¦   2   ¦   6   ¦
               ¦=======+=======¦
     Экран 2 ->¦   1   ¦   5   ¦
               ¦=======+=======¦
     Экран 3 ->¦   0   ¦   4   ¦
               L-------+--------

     Для всех цветовых режимов действует ограничение на исполь-
зование широкого экрана с номером 0, описанное в П.1.2.3.
     Следует помнить, что экраны аппаратно привязаны к конкрет-
ным сегментам ОЗУ,  а не к окнам,  т.е.  отображение информации
экрана не зависит от рабочей страницы ОЗУ и включения /  выклю-
чения окон.

}

{$I 'OrionZEm.inc'}


uses
  Windows, Messages, SysUtils, Classes;

type
  TScrThread = class(TThread)
  private
    FScrMode, FScrAddr, FSX, FSY, FMX, FMode: Integer;
    procedure DrawScreen;
    procedure BlankScreen;
  protected
    procedure Execute; override;
  public
    constructor Create(SX, SY, MX, ScrMode: Integer);
    destructor Destroy; override;
  end;

implementation

Uses modOrion, MainWin;

constructor TScrThread.Create(SX, SY, MX, ScrMode: Integer);
begin
  FSX:=SX;
  FSY:=SY;
  FMX:=MX;
  FMode:=ScrMode;
  inherited Create(True);         // Create Suspended
end;

destructor TScrThread.Destroy;
begin
  inherited;
end;

procedure TScrThread.BlankScreen;
begin
  frmMain.BlankOrionScreen;
end;

procedure TScrThread.DrawScreen;
begin
  frmMain.DrawOrionScreen;
end;

procedure TScrThread.Execute;
var
  x, y, b, tmpx, x_3, ty1, ty2, ty3: integer;
  RamCell0, RamCell1, RamCell2, RamCell3, ci, cr, cg, cb: byte;
  Color, c0, c1, c2, c3: COLORREF;
begin
  repeat
    if not DoNotUpdateScr then
    begin
      if Z80CardMode>=Z80_ORIONPRO_v2 then tmpx:=31 else tmpx:=7;
      FScrMode:=MainPort[$F8] and tmpx;
      if FScrMode and 16 = 0 then
        FScrAddr:=ScrBase[(MainPort[$FA]) and 3]
      else
        FScrAddr:=ScrBase[(MainPort[$FA]) and 3 or 1];              // Orion-Pro mode
      case FScrMode of
        0:   begin c0:=RGB(0,0,0);      c1:=RGB(0,255,0);    end;
        1:   begin c0:=RGB(200,180,40); c1:=RGB(50,250,250); end;
        2,3: begin
               DoNotUpdateScr:=True;
               Synchronize(BlankScreen);
             end;
        4: begin c0:=RGB(0,0,0);       c1:=RGB(0,0,192); c2:=RGB(0,192,0); c3:=RGB(192,0,0); end;
        5: begin c0:=RGB(192,192,192); c1:=RGB(0,0,192); c2:=RGB(0,192,0); c3:=RGB(192,0,0); end;
      end;
      if not (FScrMode in [2,3]) then
        for x := 0 to (FSX div 8) - 1 do
        begin
          x_3:=x shl 3;                                             // вычисляем левый верхний угол точки
          for y := 0 to FSY - 1 do
          begin
            RamCell0:=RAMarr[0, FScrAddr];       // страница 0
            if (FScrMode in [14,15]) and (Z80CardMode>=Z80_ORIONPRO_v2) then
              RamCell1:=MainPort[$FC]                               // Orion-Pro pseudocolor mode
            else
              RamCell1:=RAMarr[1, FScrAddr];     // страница 1
            RamCell2:=RAMarr[0, FScrAddr+$4000]; // страница 0
            RamCell3:=RAMarr[1, FScrAddr+$4000]; // страница 1
            case FMode of                                           // вычисляем левый верхний угол точки
              SCR_ZOOM_X1:
                ty1:=y * FMX;
              SCR_ZOOM_X2:
                ty1:=y * FMX *2;                                    // (y*384) *4             ( *4 = horz*2 + vert*2 )
              SCR_ZOOM_X25: 
                ty1:=((y * 5) shr 1) * FMX ;
              SCR_ZOOM_X3:
                ty1:=y * FMX *3;
            end;
            ty2:=ty1 + FMX;                                         // (y*384)*4 + 384*2      ( shift to second row )
            ty3:=ty2 + FMX;                                         // shift to third row
            case FScrMode of
              6,7,14,15:
                   begin
                     cr:=0; cg:=0; cb:=0;
                     ci:=((RamCell1 and $80) shr 1) and $FE;        // фон - старший ниббл
                     if (RamCell1 and $40 <>0) then cr:=191+ci;
                     if (RamCell1 and $20 <>0) then cg:=191+ci;
                     if (RamCell1 and $10 <>0) then cb:=191+ci;
                     c0:=RGB(cb, cg, cr);
                     cr:=0; cg:=0; cb:=0;
                     ci:=((RamCell1 and 8) shl 3) and $FE;          // цвет - младший ниббл
                     if (RamCell1 and 4 <>0) then cr:=191+ci;
                     if (RamCell1 and 2 <>0) then cg:=191+ci;
                     if (RamCell1 and 1 <>0) then cb:=191+ci;
                     c1:=RGB(cb, cg, cr);
                   end;
            end;
            for b:=7 downto 0 do
            begin
              case FScrMode of
                0,1,6,7,14,15: if (RamCell0 and 1)=0 then Color:=c0 else Color:=c1;
                4,5: begin
                       case ((RamCell0 and 1) shl 1) or (RamCell1 and 1) of
                         0: Color:=c0;
                         1: Color:=c1;
                         2: Color:=c2
                         else Color:=c3;
                       end;
                       RamCell1 := RamCell1 shr 1;
                     end
                else begin
                       case FScrMode and 20 of
                         16: begin                                   // Orion-Pro 3-bit color mode
                               if (RamCell0 and 1 = 0) then cr:=0 else cr:=191;
                               if (RamCell1 and 1 = 0) then cb:=0 else cb:=191;
                               if (RamCell2 and 1 = 0) then cg:=0 else cg:=191;
                               RamCell1 := RamCell1 shr 1;
                               RamCell2 := RamCell2 shr 1;
                               Color:=RGB(cb, cg, cr);
                             end;
                         20: begin                                   // Orion-Pro 4-bit color mode
                               if (RamCell3 and 1 = 0) then ci:=0 else ci:=63;
                               if (RamCell0 and 1 = 0) then cr:=0 else cr:=191+ci;
                               if (RamCell1 and 1 = 0) then cb:=0 else cb:=191+ci;
                               if (RamCell2 and 1 = 0) then cg:=0 else cg:=191+ci;
                               RamCell1 := RamCell1 shr 1;
                               RamCell2 := RamCell2 shr 1;
                               RamCell3 := RamCell3 shr 1;
                               Color:=RGB(cb, cg, cr);
                             end;
                       end;
                     end;
              end;
              RamCell0 := RamCell0 shr 1;
              case FMode of
                SCR_ZOOM_X1:
                   begin
                     TBig(Scr^)[x_3 + b + ty1] := Color;
                   end;
                SCR_ZOOM_X2:
                   begin                                           // x=0..384/8, y=0..255
                     tmpx:=(x_3 + b) shl 1;                        // (x*8 + 0..7) *2
                     TBig(Scr^)[tmpx +     ty1] := Color;          // left half point    (first row)
                     TBig(Scr^)[tmpx + 1 + ty1] := Color;          // right half point
                     TBig(Scr^)[tmpx +     ty2] := Color;          // left half point    (second row)
                     TBig(Scr^)[tmpx + 1 + ty2] := Color;          // right half point
                   end;
                SCR_ZOOM_X25:
                   begin
                     tmpx:=((x_3 + b) * 5) shr 1;                  //  0, 2, 5, 7, 10, 12, 15, ...
                     TBig(Scr^)[tmpx +     ty1] := Color;          // line 1
                     TBig(Scr^)[tmpx + 1 + ty1] := Color;
                     TBig(Scr^)[tmpx +     ty2] := Color;          // line 2
                     TBig(Scr^)[tmpx + 1 + ty2] := Color;
                     if boolean(b and 1) then
                     begin
                       TBig(Scr^)[tmpx + 2 + ty1] := Color;
                       TBig(Scr^)[tmpx + 2 + ty2] := Color;
                       if boolean(y and 1) then
                         TBig(Scr^)[tmpx + 2 + ty3] := Color;
                     end;
                     if boolean(y and 1) then
                     begin
                       TBig(Scr^)[tmpx +     ty3] := Color;        // line 3
                       TBig(Scr^)[tmpx + 1 + ty3] := Color;
                     end;
                   end;
                SCR_ZOOM_X3:
                   begin
                     tmpx:=(x_3 + b) *3 ;
                     TBig(Scr^)[tmpx +     ty1] := Color;          // line 1
                     TBig(Scr^)[tmpx + 1 + ty1] := Color;
                     TBig(Scr^)[tmpx + 2 + ty1] := Color;
                     TBig(Scr^)[tmpx +     ty2] := Color;          // line 2
                     TBig(Scr^)[tmpx + 1 + ty2] := Color;
                     TBig(Scr^)[tmpx + 2 + ty2] := Color;
                     TBig(Scr^)[tmpx +     ty3] := Color;          // line 3
                     TBig(Scr^)[tmpx + 1 + ty3] := Color;
                     TBig(Scr^)[tmpx + 2 + ty3] := Color;
                   end;
              end;
            end;
            inc(FScrAddr);
          end;
        end;
      Synchronize(DrawScreen);
    end;
    sleep(19);
  until Terminated;
end;

end.
