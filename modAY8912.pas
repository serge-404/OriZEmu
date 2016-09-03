unit modAY8912;


{*******************************************************************************
'   modAY8912.bas within DelphiSpec.dpr
'
'   Routines for emulating the 128K Spectrum's AY-3-8912 sound generator
'
'   Author: James Bagg <chipmunk_uk_1@hotmail.com>
'
'   With minor optimisations and mods by
'           Chris Cowley <ccowley@grok.co.uk>
'
'   Translation to Delphi Object Pascal by
'           Jari Korhonen <jarit.korhonen@luukku.com>
'
'
'   Copyright (C)1999-2000 Grok Developments Ltd  and James Bagg
'   http://www.grok.co.uk/      http://www.chipmunks-corner.co.uk
'
'   This program is free software; you can redistribute it and/or
'   modify it under the terms of the GNU General Public License
'   as published by the Free Software Foundation; either version 2
'   of the License, or (at your option) any later version.
'   This program is distributed in the hope that it will be useful,
'   but WITHOUT ANY WARRANTY; without even the implied warranty of
'   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'   GNU General Public License for more details.
'
'   You should have received a copy of the GNU General Public License
'   along with this program; if not, write to the Free Software
'   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
'
' *******************************************************************************/
}


interface


{$I 'OrionZEm.inc'}


const
  MAX_OUTPUT = 63;
  AY_STEP: integer = 32768;
  MAXVOL  = $1F;

  // AY register ID's
  AY_AFINE = 0;
  AY_ACOARSE = 1;
  AY_BFINE = 2;
  AY_BCOARSE = 3;
  AY_CFINE = 4;
  AY_CCOARSE = 5;
  AY_NOISEPER = 6;
  AY_ENABLE = 7;
  AY_AVOL = 8;
  AY_BVOL = 9;
  AY_CVOL = 10;
  AY_EFINE = 11;
  AY_ECOARSE = 12;
  AY_ESHAPE = 13;
  AY_PORTA = 14;
  AY_PORTB = 15;

type
  RegArray = array[0..15] of integer;
  VolTableArray = array[0..63] of integer;

  AY8912 = packed record
    sampleRate: integer;
    register_latch: integer;
    Regs: RegArray;
    UpdateStep: Double;
    PeriodA: integer;
    PeriodB: integer;
    PeriodC: integer;
    PeriodN: integer;
    PeriodE: integer;
    CountA: integer;
    CountB: integer;
    CountC: integer;
    CountN: integer;
    CountE: integer;
    VolA: integer;
    VolB: integer;
    VolC: integer;
    VolE: integer;
    EnvelopeA: integer;
    EnvelopeB: integer;
    EnvelopeC: integer;
    OutputA: integer;
    OutputB: integer;
    OutputC: integer;
    OutputN: integer;
    CountEnv: integer;
    Hold: integer;
    Alternate: integer;
    Attack: integer;
    Holding: integer;
    VolTable2: VolTableArray;
  end;

var
  AYPSG: AY8912;
  AY_OutNoise: integer;
  VolA: integer;
  VolB: integer;
  VolC: integer;
  lOut1: integer;
  lOut2: integer;
  lOut3: integer;
  AY_Left: integer;
  AY_NextEvent: integer;
//  Buffer_Length: integer;


procedure AY8912Update_8;
procedure AYWriteReg(r: integer; v: integer);
procedure AY8912_reset;
procedure AY8912_set_clock(clock: Double);
procedure AY8912_set_volume(volume: integer; gain: integer);

function AYReadReg(r: integer): Byte;
function AY8912_init(clock: Double; sample_rate: integer;
  sample_bits: integer): integer;
function RenderByte: integer;


implementation

procedure AY8912_reset;
  var i: integer;
begin
  with AYPSG do
  begin
    register_latch := 0;
    OutputA := 0;
    OutputB := 0;
    OutputC := 0;
    OutputN := $FF;
    PeriodA := 0;
    PeriodB := 0;
    PeriodC := 0;
    PeriodN := 0;
    PeriodE := 0;
    CountA := 0;
    CountB := 0;
    CountC := 0;
    CountN := 0;
    CountE := 0;
    VolA := 0;
    VolB := 0;
    VolC := 0;
    VolE := 0;
    EnvelopeA := 0;
    EnvelopeB := 0;
    EnvelopeC := 0;
    CountEnv := 0;
    Hold := 0;
    Alternate := 0;
    Holding := 0;
    Attack := 0;
  end;

  Randomize;
  For i := 0 To AY_PORTA do
  begin
    AYWriteReg(i, 0);     //* AYWriteReg() uses the timer system; we cannot
  end;                    //* call it at this time because the timer system
                          //* has not been initialized.
end;

procedure AY8912_set_clock(clock: Double);
  var t1: Double;
begin

    {' /* the AY_STEP clock for the tone and noise generators is the chip clock    */
    ' /* divided by 8; for the envelope generator of the AY-3-8912, it is half */
    ' /* that much (clock/16), but the envelope of the YM2149 goes twice as    */
    ' /* fast, therefore again clock/8.                                        */
    ' /* Here we calculate the number of AY_STEPs which happen during one sample  */
    ' /* at the given sample rate. No. of events = sample rate / (clock/8).    */
    ' /* AY_STEP is a multiplier used to turn the fraction into a fixed point     */
    ' /* number.}
    t1 := AY_STEP * AYPSG.sampleRate * 8.0;
    AYPSG.UpdateStep := t1 / clock
end;


{' /*
' ** set output gain
' **
' ** The gain is expressed in 0.2dB increments, e.g. a gain of 10 is an increase
' ** of 2dB. Note that the gain only affects sounds not playing at full volume,
' ** since the ones at full volume are already played at the maximum intensity
' ** allowed by the sound card.
' ** 0x00 is the default.
' ** 0xff is the maximum allowed value.
' */ }
procedure AY8912_set_volume(volume: integer; gain: integer);
  var
    i: integer;
    out1: Double;
    out2: Double;
begin
    gain := gain and $FF;

    // increase max output basing on gain (0.2 dB per AY_STEP) */
    out1 := MAX_OUTPUT;
    out2 := MAX_OUTPUT;

    while (gain > 0) do
    begin
        gain := gain - 1;
        out1 := out1 * 1.023292992;  ///* = (10 ^ (0.2/20)) */
        out2 := out2 * 1.023292992;
    end;

    {' /* calculate the volume.voltage conversion table */
    ' /* The AY-3-8912 has 16 levels, in a logarithmic scale (3dB per AY_STEP) */
    ' /* The YM2149 still has 16 levels for the tone generators, but 32 for */
    ' /* the envelope generator (1.5dB per AY_STEP). */}
    for i := 31 downto 0 do
    begin
        //* limit volume to avoid clipping */
        if (out2 > MAX_OUTPUT) then
            AYPSG.VolTable2[i] := MAX_OUTPUT
        else
            AYPSG.VolTable2[i] := Round(out2);

        out1 := out1 / 1.188502227; // .188502227 '/* = 10 ^ (1.5/20) = 1.5dB */
        out2 := out2 / 1.188502227  // .188502227
    end;
    AYPSG.VolTable2[63] := MAX_OUTPUT;
end;

procedure AYWriteReg(r: integer; v: integer);
  var
    old: integer;
begin
  AYPSG.Regs[r] := v;

  {'/* A note about the period of tones, noise and envelope: for speed reasons,*/
  '/* we count down from the period to 0, but careful studies of the chip     */
  '/* output prove that it instead counts up from 0 until the counter becomes */
  '/* greater or equal to the period. This is an important difference when the*/
  '/* program is rapidly changing the period to modulate the sound.           */
  '/* To compensate for the difference, when the period is changed we adjust  */
  '/* our internal counter.                                                   */
  '/* Also, note that period = 0 is the same as period = 1. This is mentioned */
  '/* in the YM2203 data sheets. However, this does NOT apply to the Envelope */
  '/* period. In that case, period = 0 is half as period = 1. */}
  case r of
    AY_AFINE, AY_ACOARSE:
      begin
        AYPSG.Regs[AY_ACOARSE] := AYPSG.Regs[AY_ACOARSE] and $F;

        old := AYPSG.PeriodA;

        AYPSG.PeriodA := Round((AYPSG.Regs[AY_AFINE] + (256 * AYPSG.Regs[AY_ACOARSE]))
           *AYPSG.UpdateStep);

        if (AYPSG.PeriodA = 0) then
          AYPSG.PeriodA := Round(AYPSG.UpdateStep);

        AYPSG.CountA := AYPSG.CountA + (AYPSG.PeriodA - old);

        if (AYPSG.CountA <= 0) then
          AYPSG.CountA := 1;
      end;
    AY_BFINE, AY_BCOARSE:
      begin
        AYPSG.Regs[AY_BCOARSE] := AYPSG.Regs[AY_BCOARSE] and $F;

        old := AYPSG.PeriodB;

        AYPSG.PeriodB := Round((AYPSG.Regs[AY_BFINE] + (256 * AYPSG.Regs[AY_BCOARSE]))
          * AYPSG.UpdateStep);

        if (AYPSG.PeriodB = 0) then
          AYPSG.PeriodB := Round(AYPSG.UpdateStep);

        AYPSG.CountB := AYPSG.CountB + AYPSG.PeriodB - old;

        if (AYPSG.CountB <= 0) then
          AYPSG.CountB := 1
      end;

    AY_CFINE, AY_CCOARSE:
      begin
        AYPSG.Regs[AY_CCOARSE] := AYPSG.Regs[AY_CCOARSE] and $F;

        old := AYPSG.PeriodC;

        AYPSG.PeriodC := Round((AYPSG.Regs[AY_CFINE] + (256 * AYPSG.Regs[AY_CCOARSE]))
          * AYPSG.UpdateStep);

        if (AYPSG.PeriodC = 0) then
          AYPSG.PeriodC := Round(AYPSG.UpdateStep);

        AYPSG.CountC := AYPSG.CountC + (AYPSG.PeriodC - old);

        if (AYPSG.CountC <= 0) then
          AYPSG.CountC := 1;
      end;

    AY_NOISEPER:
      begin
        AYPSG.Regs[AY_NOISEPER] := AYPSG.Regs[AY_NOISEPER] and $1F;

        old := AYPSG.PeriodN;

        AYPSG.PeriodN := Round(AYPSG.Regs[AY_NOISEPER] * AYPSG.UpdateStep);

        if (AYPSG.PeriodN = 0) then
          AYPSG.PeriodN := Round(AYPSG.UpdateStep);

        AYPSG.CountN := AYPSG.CountN + (AYPSG.PeriodN - old);

        if (AYPSG.CountN <= 0) then
          AYPSG.CountN := 1;
      end;

    AY_AVOL:
      begin
        AYPSG.Regs[AY_AVOL] := AYPSG.Regs[AY_AVOL] and $1F;

        AYPSG.EnvelopeA := AYPSG.Regs[AY_AVOL] and $10;

        if AYPSG.EnvelopeA <> 0 then
            AYPSG.VolA := AYPSG.VolE
        else
        begin
            if AYPSG.Regs[AY_AVOL] <> 0 then
                AYPSG.VolA := AYPSG.VolTable2[AYPSG.Regs[AY_AVOL] * 2 + 1]
            else
                AYPSG.VolA := AYPSG.VolTable2[0];
        end;
      end;

    AY_BVOL:
      begin
        AYPSG.Regs[AY_BVOL] := AYPSG.Regs[AY_BVOL] and $1F;

        AYPSG.EnvelopeB := AYPSG.Regs[AY_BVOL] and $10;

        if AYPSG.EnvelopeB <> 0 then
            AYPSG.VolB := AYPSG.VolE
        else
        begin
            if AYPSG.Regs[AY_BVOL] <> 0 then
                AYPSG.VolB := AYPSG.VolTable2[AYPSG.Regs[AY_BVOL] * 2 + 1]
            else
                AYPSG.VolB := AYPSG.VolTable2[0];
        end;
      end;

    AY_CVOL:
      begin
        AYPSG.Regs[AY_CVOL] := AYPSG.Regs[AY_CVOL] and $1F;

        AYPSG.EnvelopeC := AYPSG.Regs[AY_CVOL] and $10;

        if AYPSG.EnvelopeC <> 0 then
            AYPSG.VolC := AYPSG.VolE
        else
        begin
            if AYPSG.Regs[AY_CVOL] <> 0 then
                AYPSG.VolC := AYPSG.VolTable2[AYPSG.Regs[AY_CVOL] * 2 + 1]
            else
                AYPSG.VolC := AYPSG.VolTable2[0];
        end;
      end;

    AY_EFINE, AY_ECOARSE:
      begin
        old := AYPSG.PeriodE;

        AYPSG.PeriodE := Round(((AYPSG.Regs[AY_EFINE] + (256 * AYPSG.Regs[AY_ECOARSE])))
          * AYPSG.UpdateStep);

        if (AYPSG.PeriodE = 0) then
          AYPSG.PeriodE := Round(AYPSG.UpdateStep / 2);

        AYPSG.CountE := AYPSG.CountE + (AYPSG.PeriodE - old);

        if (AYPSG.CountE <= 0) then
          AYPSG.CountE := 1
      end;

    AY_ESHAPE:
      begin
        {'/* envelope shapes:
        'C AtAlH
        '0 0 x x  \___
        '
        '0 1 x x  /___
        '
        '1 0 0 0  \\\\
        '
        '1 0 0 1  \___
        '
        '1 0 1 0  \/\/
        '          ___
        '1 0 1 1  \
        '
        '1 1 0 0  ////
        '          ___
        '1 1 0 1  /
        '
        '1 1 1 0  /\/\
        '
        '1 1 1 1  /___
        '
        'The envelope counter on the AY-3-8910 has 16 AY_STEPs. On the YM2149 it
        'has twice the AY_STEPs, happening twice as fast. Since the end result is
        'just a smoother curve, we always use the YM2149 behaviour.
        '*/}
        if (AYPSG.Regs[AY_ESHAPE] <> $FF) then
        begin
          AYPSG.Regs[AY_ESHAPE] := AYPSG.Regs[AY_ESHAPE] and $F;

          if ((AYPSG.Regs[AY_ESHAPE] and $4) = $4) then
            AYPSG.Attack := MAXVOL
          else
            AYPSG.Attack := $0;

          AYPSG.Hold := AYPSG.Regs[AY_ESHAPE] and $1;

          AYPSG.Alternate := AYPSG.Regs[AY_ESHAPE] and $2;

          AYPSG.CountE := AYPSG.PeriodE;

          AYPSG.CountEnv := MAXVOL; // &h1f

          AYPSG.Holding := 0;

          AYPSG.VolE := AYPSG.VolTable2[AYPSG.CountEnv xor AYPSG.Attack];

          if (AYPSG.EnvelopeA <> 0) then
            AYPSG.VolA := AYPSG.VolE;

          if (AYPSG.EnvelopeB <> 0) then
            AYPSG.VolB := AYPSG.VolE;

          if (AYPSG.EnvelopeC <> 0) then
            AYPSG.VolC := AYPSG.VolE;
        end;
      end;
  end; //case
end;

function AYReadReg(r: integer): Byte;
begin
  AYReadReg := AYPSG.Regs[r];
end;

function AY8912_init(clock: Double; sample_rate: integer; sample_bits: integer): integer;
begin
  AYPSG.sampleRate := sample_rate;
  AY8912_set_clock(clock);
  AY8912_set_volume(255, 12);
  AY8912_reset;
  AY8912_init := 0;
end;

procedure AY8912Update_8;
  var Buffer_Length: integer;
begin

  Buffer_Length := 400;

  { /* The 8910 has three outputs, each output is the mix of one of the three */
  ' /* tone generators and of the (single) noise generator. The two are mixed */
  ' /* BEFORE going into the DAC. The formula to mix each channel is: */
  ' /* (ToneOn | ToneDisable) & (NoiseOn | NoiseDisable). */
  ' /* Note that this means that if both tone and noise are disabled, the output */
  ' /* is 1, not 0, and can be modulated changing the volume. */

  ' /* if the channels are disabled, set their output to 1, and increase the */
  ' /* counter, if necessary, so they will not be inverted during this update. */
  ' /* Setting the output to 1 is necessary because a disabled channel is locked */
  ' /* into the ON state (see above); and it has no effect if the volume is 0. */
  ' /* if the volume is 0, increase the counter, but don't touch the output. */}

  if (AYPSG.Regs[AY_ENABLE] and $1) = $1 then
  begin
    if AYPSG.CountA <= (Buffer_Length * AY_STEP) then
        AYPSG.CountA := AYPSG.CountA + (Buffer_Length * AY_STEP);

    AYPSG.OutputA := 1;
  end
  else if (AYPSG.Regs[AY_AVOL] = 0) then
  begin
      {' /* note that I do count += Buffer_Length, NOT count = Buffer_Length + 1. You might think */
      ' /* it's the same since the volume is 0, but doing the latter could cause */
      ' /* interferencies when the program is rapidly modulating the volume. */}
      if AYPSG.CountA <= (Buffer_Length * AY_STEP) then
        AYPSG.CountA := AYPSG.CountA + (Buffer_Length * AY_STEP);
  end;

  if (AYPSG.Regs[AY_ENABLE] and $2) = $2 then
  begin
      if AYPSG.CountB <= (Buffer_Length * AY_STEP) then
        AYPSG.CountB := AYPSG.CountB + (Buffer_Length * AY_STEP);

      AYPSG.OutputB := 1;
  end
  else if AYPSG.Regs[AY_BVOL] = 0 then
  begin
      if AYPSG.CountB <= (Buffer_Length * AY_STEP) then
        AYPSG.CountB := AYPSG.CountB + (Buffer_Length * AY_STEP);
  end;

  if (AYPSG.Regs[AY_ENABLE] and $4) = $4 then
  begin
      if AYPSG.CountC <= (Buffer_Length * AY_STEP) then
        AYPSG.CountC := AYPSG.CountC + (Buffer_Length * AY_STEP);

      AYPSG.OutputC := 1;
  end
  else if (AYPSG.Regs[AY_CVOL] = 0) then
  begin
      if AYPSG.CountC <= (Buffer_Length * AY_STEP) then
        AYPSG.CountC := AYPSG.CountC + (Buffer_Length * AY_STEP);
  end;

  {'/* for the noise channel we must not touch OutputN - it's also not necessary */
  '/* since we use AY_OutNoise. */}
  if ((AYPSG.Regs[AY_ENABLE] and $38) = $38) then //* all off */
  begin
      if AYPSG.CountN <= (Buffer_Length * AY_STEP) then
        AYPSG.CountN := AYPSG.CountN + (Buffer_Length * AY_STEP);
  end;

  AY_OutNoise := (AYPSG.OutputN Or AYPSG.Regs[AY_ENABLE]);
end;


function RenderByte: integer;
begin
    VolA := 0; VolB := 0; VolC := 0;

    //vola, volb and volc keep track of how long each square wave stays
    //in the 1 position during the sample period.

    AY_Left := AY_STEP;

    repeat
        AY_NextEvent := 0;

        If (AYPSG.CountN < AY_Left) Then
            AY_NextEvent := AYPSG.CountN
        Else
            AY_NextEvent := AY_Left;

        If (AY_OutNoise And $8) = $8 Then
        begin
            If (AYPSG.OutputA = 1) Then VolA := VolA + AYPSG.CountA;

            AYPSG.CountA := AYPSG.CountA - AY_NextEvent;

            {PeriodA is the half period of the square wave. Here, in each
             loop I add PeriodA twice, so that at the end of the loop the
             square wave is in the same status (0 or 1) it was at the start.
             vola is also incremented by PeriodA, since the wave has been 1
             exactly half of the time, regardless of the initial position.
             If we exit the loop in the middle, OutputA has to be inverted
             and vola incremented only if the exit status of the square
             wave is 1. }

             While (AYPSG.CountA <= 0) do
             begin
                AYPSG.CountA := AYPSG.CountA + AYPSG.PeriodA;
                If (AYPSG.CountA > 0) Then
                begin
                    If (AYPSG.Regs[AY_ENABLE] And 1) = 0 Then AYPSG.OutputA := AYPSG.OutputA Xor 1;
                    If (AYPSG.OutputA<>0) Then VolA := VolA + AYPSG.PeriodA;
                    break;
                end;

                AYPSG.CountA := AYPSG.CountA + AYPSG.PeriodA;
                VolA := VolA + AYPSG.PeriodA;
            end;
            If (AYPSG.OutputA = 1) Then VolA := VolA - AYPSG.CountA;
        end
        Else
        begin
            AYPSG.CountA := AYPSG.CountA - AY_NextEvent;

            While (AYPSG.CountA <= 0) do
            begin
                AYPSG.CountA := AYPSG.CountA + AYPSG.PeriodA;
                If (AYPSG.CountA > 0) Then
                begin
                    AYPSG.OutputA := AYPSG.OutputA Xor 1;
                    break;
                end;
                AYPSG.CountA := AYPSG.CountA + AYPSG.PeriodA;
            end;
        end;

        If (AY_OutNoise And $10) = $10 Then
        begin
            If (AYPSG.OutputB = 1) Then VolB := VolB + AYPSG.CountB;
            AYPSG.CountB := AYPSG.CountB - AY_NextEvent;

            While (AYPSG.CountB <= 0) do
            begin
                AYPSG.CountB := AYPSG.CountB + AYPSG.PeriodB;
                If (AYPSG.CountB > 0) Then
                begin
                    If (AYPSG.Regs[AY_ENABLE] And 2) = 0 Then AYPSG.OutputB := AYPSG.OutputB Xor 1;
                    If (AYPSG.OutputB<>0) Then VolB := VolB + AYPSG.PeriodB;
                    break;
                end;
                AYPSG.CountB := AYPSG.CountB + AYPSG.PeriodB;
                VolB := VolB + AYPSG.PeriodB;
            end;
            If (AYPSG.OutputB = 1) Then VolB := VolB - AYPSG.CountB;
        end
        Else
        begin
            AYPSG.CountB := AYPSG.CountB - AY_NextEvent;

            While (AYPSG.CountB <= 0) do
            begin
                AYPSG.CountB := AYPSG.CountB + AYPSG.PeriodB;
                If (AYPSG.CountB > 0) Then
                begin
                    AYPSG.OutputB := AYPSG.OutputB Xor 1;
                    break;
                end;
                AYPSG.CountB := AYPSG.CountB + AYPSG.PeriodB;
            end;
        end;

        If (AY_OutNoise And $20) = $20 Then
        begin
            If (AYPSG.OutputC = 1) Then VolC := VolC + AYPSG.CountC;
            AYPSG.CountC := AYPSG.CountC - AY_NextEvent;
            While (AYPSG.CountC <= 0) do
            begin
                AYPSG.CountC := AYPSG.CountC + AYPSG.PeriodC;
                If (AYPSG.CountC > 0) Then
                begin
                    If (AYPSG.Regs[AY_ENABLE] And 4) = 0 Then AYPSG.OutputC := AYPSG.OutputC Xor 1;
                    If (AYPSG.OutputC<>0) Then VolC := VolC + AYPSG.PeriodC;
                    break;
                end;

                AYPSG.CountC := AYPSG.CountC + AYPSG.PeriodC;
                VolC := VolC + AYPSG.PeriodC;
            end;
            If (AYPSG.OutputC = 1) Then VolC := VolC - AYPSG.CountC;
        end
        Else
        begin
            AYPSG.CountC := AYPSG.CountC - AY_NextEvent;
            While (AYPSG.CountC <= 0) do
            begin
                AYPSG.CountC := AYPSG.CountC + AYPSG.PeriodC;
                If (AYPSG.CountC > 0) Then
                begin
                    AYPSG.OutputC := AYPSG.OutputC Xor 1;
                    break;
                end;
                AYPSG.CountC := AYPSG.CountC + AYPSG.PeriodC;
            end;
        end;

        AYPSG.CountN := AYPSG.CountN - AY_NextEvent;
        If (AYPSG.CountN <= 0) Then
        begin
            //Is noise output going to change?
            AYPSG.OutputN := Round(random(510));
            AY_OutNoise := (AYPSG.OutputN Or AYPSG.Regs[AY_ENABLE]);
            AYPSG.CountN := AYPSG.CountN + AYPSG.PeriodN;
        end;

        AY_Left := AY_Left - AY_NextEvent;
    until (AY_Left <= 0);


    if (AYPSG.Holding = 0) then
    begin
        AYPSG.CountE := AYPSG.CountE - AY_STEP;
        If (AYPSG.CountE <= 0) then
        begin
            repeat
                AYPSG.CountEnv := AYPSG.CountEnv - 1;
                AYPSG.CountE := AYPSG.CountE + AYPSG.PeriodE;
            until (AYPSG.CountE > 0);

            //check envelope current position
            if (AYPSG.CountEnv < 0) then
            begin
                if (AYPSG.Hold<>0) then
                begin
                    if (AYPSG.Alternate<>0) then
                    begin
                        AYPSG.Attack := AYPSG.Attack xor MAXVOL; //$1f
                    end;
                    AYPSG.Holding := 1;
                    AYPSG.CountEnv := 0;
                end
                else
                begin
                    //if CountEnv has looped an odd number of times (usually 1),
                    //invert the output.
                    If (AYPSG.Alternate<>0) and ((AYPSG.CountEnv and $20) = $20) then
                    begin
                        AYPSG.Attack := AYPSG.Attack xor MAXVOL; //$1f
                    end;

                    AYPSG.CountEnv := AYPSG.CountEnv and MAXVOL;  //$1f
                end;
            end;

            AYPSG.VolE := AYPSG.VolTable2[AYPSG.CountEnv xor AYPSG.Attack];

            //reload volume
            If (AYPSG.EnvelopeA <> 0) then AYPSG.VolA := AYPSG.VolE;
            If (AYPSG.EnvelopeB <> 0) then AYPSG.VolB := AYPSG.VolE;
            If (AYPSG.EnvelopeC <> 0) then AYPSG.VolC := AYPSG.VolE;
        end;
    end;

    lOut1 := (VolA * AYPSG.VolA) div 65535;
    lOut2 := (VolB * AYPSG.VolB) div 65535;
    lOut3 := (VolC * AYPSG.VolC) div 65535;

    RenderByte := lOut1 + lOut2 + lOut3;
end;


end.
