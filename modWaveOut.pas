unit modWaveOut;
{' /*******************************************************************************
'   modWaveOut.bas within vbSpec.vbp
'
'   API declarations and support routines for proving beeper emulation using
'   the Windows waveOut* API fucntions.
'
'   Author: Chris Cowley <ccowley@grok.co.uk>
'
'   Copyright (C)1999-2000 Grok Developments Ltd.
'   http://www.grok.co.uk/
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
' *******************************************************************************/}

interface

uses windows, mmsystem;

{$I 'OrionZEm.inc'}

{$IFDEF USE_SOUND}
    // We can only have beeper emulation in the USE_SOUND build
    // Function for moving waveform data from a VB byte array into a block of memory
    // allocated by GlobalAlloc()
    //This was not declared in Delphi RTL at all ? Odd..
    procedure RtlMoveMemory(Dest: POINTER; Source:POINTER; Length:integer);external 'kernel32.dll';

const
  NUM_WAV_BUFFERS = 50;
  WAVE_FREQUENCY = 22050;
  WAV_BUFFER_SIZE = 441;              //(WAVE_FREQUENCY \ NUM_WAV_BUFFERS)

  // Variables and constants used by the beeper emulation
var
  glphWaveOut : integer = -1;
  ghMem: array[1..NUM_WAV_BUFFERS+1] of HGLOBAL;
  gpMem: array[1..NUM_WAV_BUFFERS+1] of Pointer;
  gtWavFormat: TWAVEFORMATEX;
  gtWavHdr: array[1..NUM_WAV_BUFFERS+1] of WAVEHDR;
  gcWaveOut: array[0..48000] of Byte;
  glWavePtr: integer;
  glWaveAddTStates: integer;
{$ENDIF}


procedure AddSoundWave(ts : integer);

implementation

uses modAY8912, modOrion, MainWin;

var
  WCount : integer = 0;
  lCounter : integer = 0;

procedure AddSoundWave(ts : integer);
begin
{$IFDEF USE_SOUND}
  
        WCount := WCount + 1;
        If WCount = 800 Then
        begin
            AY8912Update_8;
            WCount := 0;
        End;

        lCounter := lCounter + ts;
        While lCounter >= glWaveAddTStates do
        begin
            If AYEnabled Then
              gcWaveOut[glWavePtr] := glBeeperVal + RenderByte()
            else
              gcWaveOut[glWavePtr] := glBeeperVal;
            glWavePtr := glWavePtr + 1;
            lCounter := lCounter - glWaveAddTStates;
        end;
{$ENDIF}
End;




end.
