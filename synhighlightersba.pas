{-------------------------------------------------------------------------------
SynHighlighterSBA v1.1

This unit provides a SBA VHDL highlighter for SynEdit.

2015 (c) Miguel A. Risco-Castillo

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Some concepts in this unit are based in the Original Code of Zhou Kan and
"La Biblia del SynEdit" of Tito Hinostroza.
All Rights Reserved.

v1.1 Add special list of highlighted words
-------------------------------------------------------------------------------}

unit SynHighlighterSBA;

interface

uses
  SysUtils, Classes, Graphics, SynEditHighlighterFoldBase, SynEditHighlighter, SynEditTypes, SynEditStrConst;

type
  TtkTokenKind = (tkSBA, tkIeee, tkComment, tkIdentifier, tkKey, tkNull,
    tkNumber, tkSpace, tkString, tkSymbol, tkAttribute, tkUnknown);

  TRangeState = (rsUnknown);

  TProcTableProc = procedure of object;

  TBlockID = (PackageBlk, EntityBlk, ArchBlk, CompBlk, EntCompBlk, MapBlk, RecordBlk, ProcessBlk, CaseBlk, ForBlk, IfBlk, WhileBlk, FuncProcBlk);

type

  { TSynSBASyn }

  TSynSBASyn = class(TSynCustomFoldHighlighter)
  private
    FHLWordsList: TStringList;
    fRange: TRangeState;
    fLine: PChar;
    fLineNumber: integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: longint;
    fStringLen: integer;
    fToIdent: PChar;
    fTokenPos: integer;
    FTokenID: TtkTokenKind;
    fCommentAttri: TSynHighLighterAttributes;
    fIdentifierAttri: TSynHighLighterAttributes;
    fKeyAttri: TSynHighLighterAttributes;
    fNumberAttri: TSynHighLighterAttributes;
    fSpaceAttri: TSynHighLighterAttributes;
    fStringAttri: TSynHighLighterAttributes;
    fSymbolAttri: TSynHighLighterAttributes;
    fAttribAttri: TSynHighLighterAttributes;
    FIeeeAttri: TSynHighLighterAttributes;
    fSBAAttri: TSynHighLighterAttributes;
    fDivider:TSynDividerDrawConfigSetting;
    function KeyComp(const aKey: string): boolean;
    procedure SetHLWordsList(AValue: TStringList);
    procedure SymbolsProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure ColonProc;
    procedure EqualProc;
    procedure EndTokenProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StarProc;
    procedure StringProc;
    procedure TildeProc;
    procedure AttribProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
    procedure FoldValidProc;
    procedure MakeMethodTables;
  protected
    function GetIdentChars: TSynIdentChars; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
      override;
    function GetEOL: boolean; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(const NewValue: String; LineNumber:Integer); override;
    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighLighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: integer; override;
    procedure Next; override;
    function GetRange: Pointer; override;
    procedure SetRange(Value: Pointer); override;
    procedure ReSetRange; override;
    function GetDrawDivider(Index: integer): TSynDividerDrawConfigSetting; override;
    property IdentChars;
  published
    property CommentAttri: TSynHighLighterAttributes read fCommentAttri write fCommentAttri;
    property IdentifierAttri: TSynHighLighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighLighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighLighterAttributes read fNumberAttri write fNumberAttri;
    property SpaceAttri: TSynHighLighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighLighterAttributes read fStringAttri write fStringAttri;
    property SymbolAttri: TSynHighLighterAttributes read fSymbolAttri write fSymbolAttri;
    property AttribAttri: TSynHighLighterAttributes read fAttribAttri write fAttribAttri;
    property IeeeAttri: TSynHighLighterAttributes read fIeeeAttri write fIeeeAttri;
    property SBAAttri: TSynHighLighterAttributes read fSBAAttri write fSBAAttri;
    property HLWordsList: TStringList read FHLWordsList write SetHLWordsList;
  end;

implementation

var
  Identifiers: array[#0..#255] of bytebool;
  mHashTable: array[#0..#255] of integer;

procedure MakeIdentTable;
var
  I, J: char;
begin
  for I := #0 to #255 do
  begin
    case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
      else 
        Identifiers[I] := False;
    end;
    J := UpCase(I);
    case I in ['_', '0'..'9', 'A'..'Z', 'a'..'z'] of
      True: mHashTable[I] := Ord(J)
        else
          mHashTable[I] := 0;
    end;
  end;
end;

function TSynSBASyn.KeyComp(const aKey: string): boolean;
var
  I: integer;
  Temp: PChar;
begin
  Temp := fToIdent;   // Test from the second char +1
  if Length(aKey) = fStringLen then
  begin
    Result := True;
    for i := 1 to fStringLen do   // Test from the second char := 2
    begin
      if mHashTable[Temp^] <> mHashTable[aKey[i]] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end 
  else 
    Result := False;
end;

procedure TSynSBASyn.SetHLWordsList(AValue: TStringList);
begin
  if FHLWordsList=AValue then Exit;
  FHLWordsList:=AValue;
end;

procedure TSynSBASyn.IdentProc;
var
  i:integer;
  blk:TBlockID;
begin
  while Identifiers[fLine[Run]] do inc(Run);
  fStringLen:=Run-fTokenPos;
  fToIdent:=fLine + fTokenPos;
  fTokenID := tkUnknown;
  if FHLWordsList.Count>0 then for i:=0 to FHLWordsList.Count-1 do
  begin
    if KeyComp(FHLWordsList[i]) then fTokenID := tkString;
  end;
  blk:=TBlockID(PtrUInt(TopCodeFoldBlockType));
  case Upcase(fToIdent^) of
    'A':begin
      //vhdl
      if KeyComp('abs') or
         KeyComp('access') or
         KeyComp('after') or
         KeyComp('alias') or
         KeyComp('all') or
         KeyComp('and') or
         KeyComp('array') or
         KeyComp('assert') or
         KeyComp('attribute')
      then fTokenID := tkKey;
      if KeyComp('architecture') then begin fTokenID := tkKey; StartCodeFoldBlock(Pointer(PtrUInt(ArchBlk))); end;
      //ieee
      if KeyComp('add_unsigned') or
         KeyComp('and_table') or
         KeyComp('append_mode')
      then fTokenID := tkIeee;
    end;
    'B':begin
      //vhdl
      if KeyComp('begin') or
         KeyComp('block') or
         KeyComp('body') or
         KeyComp('buffer') or
         KeyComp('bus')
      then fTokenID := tkKey;
//      if KeyComp('begin') then begin fTokenID := tkKey; StartCodeFoldBlock(nil); end;
      //ieee
      if KeyComp('bit_vector') or
         KeyComp('bit') or
         KeyComp('bitwise_eql') or
         KeyComp('bitwise_neq') or
         KeyComp('boolean')
      then fTokenID := tkIeee;
    end;
    'C':begin
      //vhdl
      if KeyComp('configuration') or
         KeyComp('constant')
      then fTokenID := tkKey;
      if KeyComp('case') then begin fTokenID := tkKey; StartCodeFoldBlock(Pointer(PtrUInt(CaseBlk))); end;
      if KeyComp('component') then begin fTokenID := tkKey; StartCodeFoldBlock(Pointer(PtrUInt(CompBlk))); end;
      //ieee
      if KeyComp('character') then fTokenID := tkIeee;
    end;
    'D':begin
      //vhdl
      if KeyComp('disconnect') or
         KeyComp('downto') then fTokenID := tkKey;
      //ieee
      if KeyComp('delay_length') then fTokenID := tkieee;
    end;
    'E':begin
      //vhdl
      if KeyComp('else') or
         KeyComp('elsif') or
         KeyComp('exit')
      then fTokenID := tkKey;
      if KeyComp('entity') then
      begin
        fTokenID := tkKey;
        if (Blk<>ArchBlk) then StartCodeFoldBlock(Pointer(PtrUInt(EntityBlk))) else StartCodeFoldBlock(Pointer(PtrUInt(EntCompBlk)));
      end;
      if KeyComp('end') then endTokenProc;
    end;
    'F':begin
      //vhdl
      if KeyComp('file') then fTokenID := tkKey;
      if KeyComp('for') then  fTokenID := tkKey;
      if KeyComp('function')then begin fTokenID := tkKey; FoldValidProc; end;
      //ieee
      if KeyComp('falling_edge') or
         KeyComp('false') or
         KeyComp('fs')
      then fTokenID := tkIeee;
    end;
    'G':begin
      //vhdl
      if KeyComp('generate') or
         KeyComp('generic') or
         KeyComp('group') or
         KeyComp('guarded')
      then fTokenID := tkKey;
    end;
    'H':begin
      //ieee
      if KeyComp('hr') then fTokenID := tkIeee;
    end;
    'I':begin
      //vhdl
      if KeyComp('if') then begin fTokenID := tkKey; StartCodeFoldBlock(Pointer(PtrUInt(IfBlk))); end;
      if KeyComp('impure') or
         KeyComp('in') or
         KeyComp('inertial') or
         KeyComp('inout') or
         KeyComp('is')
      then fTokenID := tkKey;
      //ieee
      if KeyComp('ieee') or
         KeyComp('image') or
         KeyComp('input') or
         KeyComp('integer') or
         KeyComp('is_x')
      then fTokenID := tkIeee;
    end;
    'J':begin
    end;
    'K':begin
    end;
    'L':begin
      //vhdl
      if KeyComp('label') or
         KeyComp('library') or
         KeyComp('linkage') or
         KeyComp('literal')
      then fTokenID := tkKey;
      if KeyComp('loop') then begin fTokenID := tkKey; StartCodeFoldBlock(Pointer(PtrUInt(ForBlk))); end;
    end;
    'M':begin
      //vhdl
      if KeyComp('map') then begin fTokenID := tkKey; end; //StartCodeFoldBlock(Pointer(PtrUInt(MapBlk))); end;
      if KeyComp('mod') then fTokenID := tkKey;
      //ieee
      if KeyComp('math_real') or
         KeyComp('min') or
         KeyComp('ms')
      then fTokenID := tkIeee;
    end;
    'N':begin
      //vhdl
      if KeyComp('nand') or
         KeyComp('new') or
         KeyComp('next') or
         KeyComp('nor') or
         KeyComp('not') or
         KeyComp('null')
      then fTokenID := tkKey;
      //ieee
      if KeyComp('natural') or
         KeyComp('ns') or
         KeyComp('numeric_bit') or
         KeyComp('numeric_std')
      then fTokenID := tkIeee;
    end;
    'O':begin
      //vhdl
      if KeyComp('of') or
         KeyComp('on') or
         KeyComp('open') or
         KeyComp('or') or
         KeyComp('others') or
         KeyComp('out')
      then fTokenID := tkKey;
      //ieee
      if KeyComp('output') then fTokenID := tkIeee;
    end;
    'P':begin
      //vhdl
      if KeyComp('port') or
         KeyComp('postponed') or
         KeyComp('pure')
      then fTokenID := tkKey;
      if KeyComp('process') or
         KeyComp('package') then begin fTokenID := tkKey; StartCodeFoldBlock(Pointer(PtrUInt(PackageBlk))); end;
      if KeyComp('procedure') then begin fTokenID := tkKey; FoldValidProc; end;
      //ieee
      if KeyComp('positive') or
         KeyComp('pos') or
         KeyComp('ps')
      then fTokenID := tkIeee;
    end;
    'Q':begin
    end;
    'R':begin
      //vhdl
      if KeyComp('range') or
         KeyComp('register') or
         KeyComp('reject') or
         KeyComp('rem') or
         KeyComp('report') or
         KeyComp('return') or
         KeyComp('rol') or
         KeyComp('ror')
      then fTokenID := tkKey;
      if KeyComp('record') then begin fTokenID := tkKey; StartCodeFoldBlock(Pointer(PtrUInt(RecordBlk))); end;
      //ieee
      if KeyComp('real') or
         KeyComp('resize') or
         KeyComp('rising_edge') or
         KeyComp('rotate_left') or
         KeyComp('rotate_right')
      then fTokenID := tkIeee;
    end;
    'S':begin
      //vhdl
      if KeyComp('select') or
         KeyComp('severity') or
         KeyComp('signal') or
         KeyComp('shared') or
         KeyComp('sla') or
         KeyComp('sll') or
         KeyComp('sra') or
         KeyComp('srl') or
         KeyComp('subtype')
      then fTokenID := tkKey;
      //ieee
      if KeyComp('severity_level') or
         KeyComp('sec') or
         KeyComp('shift_left') or
         KeyComp('shift_right') or
         KeyComp('shl') or
         KeyComp('shr') or
         KeyComp('signed_equal') or
         KeyComp('signed_less_or_equal') or
         KeyComp('signed_less') or
         KeyComp('signed_num_bits') or
         KeyComp('signed_return_boolean') or
         KeyComp('signed') or
         KeyComp('small_int') or
         KeyComp('sqrt') or
         KeyComp('standard') or
         KeyComp('status_error') or
         KeyComp('std_logic_1164') or
         KeyComp('std_logic_arith') or
         KeyComp('std_logic_signed') or
         KeyComp('std_logic_unsigned') or
         KeyComp('std_logic_vector') or
         KeyComp('std_logic') or
         KeyComp('std_match') or
         KeyComp('std_ulogic_vector') or
         KeyComp('std_ulogic') or
         KeyComp('string')
      then fTokenID := tkIeee;
      //SBA
      if KeyComp('sbacall') or
         KeyComp('sbaconfig') or
         KeyComp('sbainte') or
         KeyComp('sbajump') or
         KeyComp('sbapackage') or
         KeyComp('sbaread') or
         KeyComp('sbaret') or
         KeyComp('sbareti') or
         KeyComp('sbawait') or
         KeyComp('sbawrite')
      then fTokenID := tkSBA;
    end;
    'T':begin
      //vhdl
      if KeyComp('then') or
         KeyComp('to') or
         KeyComp('transport') or
         KeyComp('type')
      then fTokenID := tkKey;
      //ieee
      if KeyComp('text') or
         KeyComp('textio') or
         KeyComp('time') or
         KeyComp('to_01') or
         KeyComp('to_bit') or
         KeyComp('to_bitvector') or
         KeyComp('to_integer') or
         KeyComp('to_signed') or
         KeyComp('to_stdlogicvector') or
         KeyComp('to_stdulogic') or
         KeyComp('to_stdulogicvector') or
         KeyComp('to_unsigned') or
         KeyComp('true')
      then fTokenID := tkIeee;
    end;
    'U':begin
      //vhdl
      if KeyComp('unaffected') or
         KeyComp('units') or
         KeyComp('until') or
         KeyComp('use')
      then fTokenID := tkKey;
      //ieee
      if KeyComp('unsigned') or
         KeyComp('us')
      then fTokenID := tkIeee;
    end;
    'V':begin
      //vhdl
      if KeyComp('variable') then fTokenID := tkKey;
      //ieee
      if KeyComp('val') then fTokenID := tkIeee;
    end;
    'W':begin
      //vhdl
      if KeyComp('wait') or
         KeyComp('when') or
         KeyComp('with')
      then fTokenID := tkKey;
      if KeyComp('while') then begin fTokenID := tkKey; StartCodeFoldBlock(Pointer(PtrUInt(WhileBlk))); end;
      //ieee
      if KeyComp('work') then fTokenID := tkIeee;
    end;
    'X':begin
      //vhdl
      if KeyComp('xnor') or
         KeyComp('xor')
      then fTokenID := tkKey;
      //ieee
      if KeyComp('xrol') or
         KeyComp('xror') or
         KeyComp('xsll') or
         KeyComp('xsra') or
         KeyComp('xsrl')
      then fTokenID := tkIeee;
    end;
    'Y':begin
    end;
    'Z':begin
    end;
  end;
end;

procedure TSynSBASyn.MakeMethodTables;
var
  I: char;
begin
  for I := #0 to #255 do
    case I of
      #0 : fProcTable[I]      := @NullProc;
      '#','$','&','@' : fProcTable[I]  := @SymbolsProc;
      '}': fProcTable[I]      := @BraceCloseProc;
      '{': fProcTable[I]      := @BraceOpenProc;
      #13: fProcTable[I]      := @CRProc;
      ':': fProcTable[I]      := @ColonProc;
      '=': fProcTable[I]      := @EqualProc;
      '>': fProcTable[I]      := @GreaterProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := @IdentProc;
      #10: fProcTable[I]      := @LFProc;
      '<': fProcTable[I]      := @LowerProc;
      '-': fProcTable[I]      := @MinusProc;
      '%': fProcTable[I]      := @ModSymbolProc;
      '!': fProcTable[I]      := @NotSymbolProc;
      '0'..'9': fProcTable[I] := @NumberProc;
      '|': fProcTable[I]      := @OrSymbolProc;
      '+': fProcTable[I]      := @PlusProc;
      '.': fProcTable[I]      := @PointProc;
      '(': fProcTable[I]      := @RoundOpenProc;
      ')': fProcTable[I]      := @RoundCloseProc;
      ';': fProcTable[I]      := @SemiColonProc;
      '/': fProcTable[I]      := @SlashProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := @SpaceProc;
      ']': fProcTable[I]      := @SquareCloseProc;
      '[': fProcTable[I]      := @SquareOpenProc;
      '*': fProcTable[I]      := @StarProc;
      #34: fProcTable[I]      := @StringProc;     //"
      '~': fProcTable[I]      := @TildeProc;
      #39: fProcTable[I]      := @AttribProc;       //'
      '^': fProcTable[I]      := @XOrSymbolProc;
      else 
        fProcTable[I] := @UnknownProc;
    end;
end;

constructor TSynSBASyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Foreground := clGreen;
  AddAttribute(fCommentAttri);
  
  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier);
  fIdentifierAttri.Foreground := clWindowText;
  AddAttribute(fIdentifierAttri);
  
  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Foreground := clBlue;
  AddAttribute(fKeyAttri);
  
  fNumberAttri := TSynHighLighterAttributes.Create(SYNS_AttrNumber);
  fNumberAttri.Foreground := clRed;
  AddAttribute(fNumberAttri);
  
  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrWhitespace);
  AddAttribute(fSpaceAttri);
  
  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString);
  fStringAttri.Foreground := clMaroon;
  AddAttribute(fStringAttri);
  
  fSymbolAttri := TSynHighLighterAttributes.Create(SYNS_AttrSymbol);
  fSymbolAttri.Foreground := clTeal;
  AddAttribute(fSymbolAttri);
  
  fAttribAttri := TSynHighLighterAttributes.Create(SYNS_AttrAttributeName);
  fAttribAttri.Foreground := $000080FF;
  AddAttribute(fAttribAttri);

  fIeeeAttri := TSynHighLighterAttributes.Create(SYNS_AttrSecondReservedWord);
  fIeeeAttri.Foreground := $00804000;
  AddAttribute(fIeeeAttri);

  fSBAAttri := TSynHighLighterAttributes.Create(SYNS_AttrMiscellaneous);
  fSBAAttri.Foreground := $00C08000;
  AddAttribute(fSBAAttri);

  SetAttributesOnChange(@DefHighlightChange);
  MakeMethodTables;
  fRange := rsUnknown;

  fDivider.Color:=clNone;

  FHLWordsList:=TStringList.Create;
end;

destructor TSynSBASyn.Destroy;
begin
  FreeAndNil(FHLWordsList);
  inherited Destroy;
end;

procedure TSynSBASyn.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited;
  fLine       := PChar(NewValue);
  Run         := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynSBASyn.SymbolsProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynSBASyn.BraceCloseProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynSBASyn.BraceOpenProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynSBASyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run + 1] = #10 then Inc(Run);
end;

procedure TSynSBASyn.ColonProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSBASyn.EqualProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSBASyn.GreaterProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSBASyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynSBASyn.LowerProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSBASyn.MinusProc;
begin
  fTokenID := tkSymbol;
  inc(Run);

  if (FLine[Run] = '-') then
  begin
    if (FLine[Run+1] = ' ') and (FLine[Run+2] = '/') and
       (((FLine[Run+3] = 'L') and (FLine[Run+4] = ':')) or
        ((FLine[Run+3] = 'S') and (FLine[Run+4] = 'B') and (FLine[Run+5] = 'A') and (FLine[Run+6] = ':')))
    then
    begin
       fTokenID := tkSBA;
    end else fTokenID := tkComment;
    while not (FLine[Run] in [#0, #10, #13]) do inc(Run);
  end;
end;

procedure TSynSBASyn.ModSymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynSBASyn.NotSymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSBASyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynSBASyn.NumberProc;
begin
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'u', 'U', 'l', 'L', 'x', 'X', 'e', 'E', 'f', 'F'] do
   inc(Run);
end;

procedure TSynSBASyn.OrSymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynSBASyn.PlusProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSBASyn.PointProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSBASyn.RoundCloseProc;
var Blk:TBlockID;
begin
  blk:=TBlockID(PtrUInt(TopCodeFoldBlockType));
  if blk=MapBlk then EndCodeFoldBlock();
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSBASyn.RoundOpenProc;
begin
  inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynSBASyn.SemiColonProc;
var Blk:TBlockID;
begin
  blk:=TBlockID(PtrUInt(TopCodeFoldBlockType));
  if blk=EntCompBlk then EndCodeFoldBlock();
  inc(Run);
  fTokenID := tkSymbol;
end;


procedure TSynSBASyn.SlashProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSBASyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynSBASyn.SquareCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSBASyn.SquareOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSBASyn.StarProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSBASyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
      #92:
        if FLine[Run + 1] = #10 then inc(Run);
    end;
    inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynSBASyn.TildeProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynSBASyn.AttribProc;
var tmp:integer;
begin
  fTokenID := tkSymbol;
  inc(Run);
  tmp:=Run;
  while Identifiers[fLine[tmp]] do inc(tmp);
  fStringLen:=tmp-fTokenPos;
  fToIdent:=fLine + fTokenPos;

  if keycomp('''ascending') or
     keycomp('''base') or
     keycomp('''delayed') or
     keycomp('''driving') or
     keycomp('''driving_value') or
     keycomp('''event') or
     keycomp('''high') or
     keycomp('''image') or
     keycomp('''instance_name') or
     keycomp('''last_active') or
     keycomp('''last_event') or
     keycomp('''last_value') or
     keycomp('''left') or
     keycomp('''leftof') or
     keycomp('''length') or
     keycomp('''low') or
     keycomp('''path_name') or
     keycomp('''pos') or
     keycomp('''pred') or
     keycomp('''quiet') or
     keycomp('''range') or
     keycomp('''reverse_range') or
     keycomp('''right') or
     keycomp('''rightof') or
     keycomp('''simple_name') or
     keycomp('''stable') or
     keycomp('''succ') or
     keycomp('''transaction') or
     keycomp('''val') or
     keycomp('''value') then
  begin
    fTokenID := tkAttribute;
    while Identifiers[fLine[Run]] do inc(Run);
  end;
end;

procedure TSynSBASyn.XOrSymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynSBASyn.EndTokenProc;
var tmp:integer;
begin
  fTokenID := tkKey;
  while fLine[Run]=' ' do inc(Run);
  tmp:=Run;
  while Identifiers[fLine[Run]] do inc(Run);
  fStringLen:=Run-tmp;
  fToIdent:=fLine + tmp;
  if not (
     keycomp('architecture') or
     keycomp('if') or
     keycomp('case') or
     keycomp('component') or
     keycomp('function') or
     keycomp('loop') or
     keycomp('package') or
     keycomp('procedure') or
     keycomp('process') or
     keycomp('record')
     ) then Run:=tmp;
  EndCodeFoldBlock();
end;

procedure TSynSBASyn.UnknownProc;
begin
  inc(Run);
  while (fLine[Run] in [#128..#191]) OR // continued utf8 subcode
   ((fLine[Run]<>#0) and (fProcTable[fLine[Run]] = @UnknownProc)) do inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynSBASyn.FoldValidProc;
var
  tmp:integer;
  found,inRoundbracket:boolean;
begin
  tmp:=run;
  found:=false;
  while (not (fLine[tmp] in [#0, #10, #13])) do
  begin
    if (fLine[tmp]='(') then inRoundbracket:=true;
    if (fLine[tmp]=')') then inRoundbracket:=false;
    if (fLine[tmp]=';') and not inRoundbracket then
    begin
      found:=true;
      break;
    end else inc(tmp);
  end;
  if not found then StartCodeFoldBlock(Pointer(PtrUInt(FuncProcBlk)));
end;

procedure TSynSBASyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TSynSBASyn.GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result    := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result    := fKeyAttri;
    SYN_ATTR_STRING: Result     := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result     := fSymbolAttri;
    else
      Result := nil;
  end;
end;

function TSynSBASyn.GetEOL: boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynSBASyn.GetToken: string;
var
  Len: longint;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

procedure TSynSBASyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  TokenStart := fLine + fTokenPos;

  //if TokenLength>0 then begin
  //  TokenStart:=@fLine[fTokenPos];
  //end else begin
  //  TokenStart:=nil;
  //end;
end;

function TSynSBASyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynSBASyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkComment: Result    := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result        := fKeyAttri;
    tkNumber: Result     := fNumberAttri;
    tkSpace: Result      := fSpaceAttri;
    tkString: Result     := fStringAttri;
    tkSymbol: Result     := fSymbolAttri;
    tkAttribute: Result  := fAttribAttri;
    tkIeee: Result       := fIeeeAttri;
    tkSBA:  Result       := fSBAAttri;
    tkUnknown: Result    := fIdentifierAttri;
    else 
      Result := nil;
  end;
end;

function TSynSBASyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynSBASyn.GetTokenPos: integer;
begin
  Result := fTokenPos;
end;

function TSynSBASyn.GetRange: Pointer;
begin
//  Result := Pointer(fRange);
  CodeFoldRange.RangeType := Pointer(PtrInt(fRange));
  Result := inherited;
end;

procedure TSynSBASyn.ReSetRange;
begin
  inherited;
  fRange := rsUnknown;
end;

function TSynSBASyn.GetDrawDivider(Index: integer
  ): TSynDividerDrawConfigSetting;
begin
  Result:=fDivider;
end;

procedure TSynSBASyn.SetRange(Value: Pointer);
begin
  //  fRange := TRangeState(Value);
  inherited;
  fRange := TRangeState(PtrUInt(CodeFoldRange.RangeType));
end;

function TSynSBASyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
end;

initialization
  MakeIdentTable;
  RegisterPlaceableHighlighter(TSynSBASyn);

end.

