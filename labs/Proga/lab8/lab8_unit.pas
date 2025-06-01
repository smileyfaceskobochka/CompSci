unit lab8_unit;
{$mode objfpc}{$H+}
{$hints off}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Math;

const
  RECORD_COUNT    = 8000000;
  CHUNK_THRESHOLD = 250000;
  PAGE_SIZE       = 100;

type
  TMicrocontroller = record
    Manufacturer: string[50];
    Model: string[50];
    ClockMHz: double;
    FlashKB: cardinal;
    RAMKB: cardinal;
    GPIOCount: word;
    ADCChannels: byte;
    ADCResolutionBits: byte;
    UARTCount: byte;
    OtherInfo: string[100];
  end;
  TMCUFile = file of TMicrocontroller;

  TKWNode = record
    Rec: TMicrocontroller;
    SourceIdx: integer;
  end;

  TManufacturer = record
    Name: string;
    Models: array of string;
  end;

  { TForm1 }
  TForm1 = class(TForm)
    btnGen: TButton;
    btnSort: TButton;
    btnPrev: TButton;
    btnNext: TButton;
    btnShowAll: TButton;
    lvOut: TListView;
    rgField: TRadioGroup;
    statBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure btnGenClick(Sender: TObject);
    procedure btnSortClick(Sender: TObject);
    procedure btnShowAllClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
  private
    CurrentPage: Int64;
    TotalRecords: Int64;
    Manufacturers: array of TManufacturer;

    procedure InitManufacturers;
    procedure SetupListView;
    procedure DisplaySample(const ACaption: string);
    procedure DisplayPage(Page: Int64);

    function  CompareRecords(const A, B: TMicrocontroller; fieldIndex: integer): integer;
    procedure QuickSortChunk(var Arr: array of TMicrocontroller; low, high, fieldIndex: integer);

    procedure ExternalMergeSort(const InFile: string; fieldIndex: integer);
    procedure SafeDeleteFile(const FileName: string);

    procedure SplitToRuns(const InFile: string; fieldIndex: integer; out RunFiles: TStringList);
    procedure KWayMerge(const RunFiles: TStringList; fieldIndex: integer; const OutFile: string);
  public
  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}

procedure TForm1.SafeDeleteFile(const FileName: string);
begin
  if FileExists(FileName) then
    DeleteFile(FileName);
end;

function TForm1.CompareRecords(const A, B: TMicrocontroller; fieldIndex: integer): integer;
begin
  case fieldIndex of
    0: Result := CompareText(A.Manufacturer, B.Manufacturer);
    1: Result := CompareText(A.Model, B.Model);
    2: Result := Sign(A.ClockMHz - B.ClockMHz);
    3: Result := Integer(A.FlashKB) - Integer(B.FlashKB);
    4: Result := Integer(A.RAMKB) - Integer(B.RAMKB);
    5: Result := Integer(A.GPIOCount) - Integer(B.GPIOCount);
    6: Result := Integer(A.ADCChannels) - Integer(B.ADCChannels);
    7: Result := Integer(A.ADCResolutionBits) - Integer(B.ADCResolutionBits);
    8: Result := Integer(A.UARTCount) - Integer(B.UARTCount);
  else
    Result := CompareText(A.OtherInfo, B.OtherInfo);
  end;
end;

procedure TForm1.QuickSortChunk(var Arr: array of TMicrocontroller; low, high, fieldIndex: integer);
var
  i, j: integer;
  pivot, tmp: TMicrocontroller;
begin
  if low >= high then Exit;
  pivot := Arr[(low + high) div 2];
  i := low; j := high;
  repeat
    while CompareRecords(Arr[i], pivot, fieldIndex) < 0 do Inc(i);
    while CompareRecords(Arr[j], pivot, fieldIndex) > 0 do Dec(j);
    if i <= j then
    begin
      tmp := Arr[i]; Arr[i] := Arr[j]; Arr[j] := tmp;
      Inc(i); Dec(j);
    end;
  until i > j;
  QuickSortChunk(Arr, low, j, fieldIndex);
  QuickSortChunk(Arr, i, high, fieldIndex);
end;

procedure TForm1.SetupListView;
begin
  lvOut.ViewStyle := vsReport;
  lvOut.Columns.Clear;
  with lvOut.Columns.Add do Caption := 'Index';
  with lvOut.Columns.Add do Caption := 'Manufacturer';
  with lvOut.Columns.Add do Caption := 'Model';
  with lvOut.Columns.Add do Caption := 'Clock MHz';
  with lvOut.Columns.Add do Caption := 'Flash KB';
  with lvOut.Columns.Add do Caption := 'RAM KB';
  with lvOut.Columns.Add do Caption := 'GPIO';
  with lvOut.Columns.Add do Caption := 'ADC Ch';
  with lvOut.Columns.Add do Caption := 'ADC Res';
  with lvOut.Columns.Add do Caption := 'UART';
  with lvOut.Columns.Add do Caption := 'Info';
end;

{Инициализация производителей и их моделей}
procedure TForm1.InitManufacturers;
begin
  SetLength(Manufacturers, 7);
  Manufacturers[0].Name := 'Espressif';
  Manufacturers[0].Models := ['ESP32', 'ESP32-S3', 'ESP32-C3', 'ESP8266'];
  Manufacturers[1].Name := 'Microchip';
  Manufacturers[1].Models := ['PIC16F877A', 'PIC18F4550', 'ATmega328P'];
  Manufacturers[2].Name := 'ST';
  Manufacturers[2].Models := ['STM32F103', 'STM32F407', 'STM32L476'];
  Manufacturers[3].Name := 'NXP';
  Manufacturers[3].Models := ['LPC1768', 'LPC1114', 'LPC55S69'];
  Manufacturers[4].Name := 'TI';
  Manufacturers[4].Models := ['MSP430G2553', 'TMS320F28379D'];
  Manufacturers[5].Name := 'Nordic';
  Manufacturers[5].Models := ['nRF52832', 'nRF52840', 'nRF5340'];
  Manufacturers[6].Name := 'Silicon Labs';
  Manufacturers[6].Models := ['EFM32GG11', 'EFM32HG322', 'EFM32ZG222'];
end;

{Загрузка формы}
procedure TForm1.FormCreate(Sender: TObject);
begin
  InitManufacturers;
  rgField.Items.Clear;
  rgField.Items.AddStrings([
    'Manufacturer','Model','ClockMHz','FlashKB','RAMKB',
    'GPIOCount','ADCChannels','ADCResolutionBits','UARTCount','OtherInfo'
  ]);
  rgField.ItemIndex := 0;
  SetupListView;
  CurrentPage := 0;
  TotalRecords := 0;
end;

{Генерация данных}
procedure TForm1.btnGenClick(Sender: TObject);
var
  F: TMCUFile;
  rec: TMicrocontroller;
  i, mi, mo: Integer;
begin
  Randomize;
  AssignFile(F,'mcus.dat'); Rewrite(F);
  try
    for i := 1 to RECORD_COUNT do
    begin
      mi := Random(Length(Manufacturers));
      rec.Manufacturer := Manufacturers[mi].Name;
      mo := Random(Length(Manufacturers[mi].Models));
      rec.Model := Manufacturers[mi].Models[mo];
      rec.ClockMHz := 20 + Random * 220;
      rec.FlashKB := 256 + Random(4096);
      rec.RAMKB := 16 + Random(1024);
      rec.GPIOCount := 8 + Random(120);
      rec.ADCChannels := 1 + Random(16);
      rec.ADCResolutionBits := 8 + 4 * Random(3);
      rec.UARTCount := 1 + Random(4);
      rec.OtherInfo := Format('ID:%d',[Random(1000000)]);
      Write(F,rec);
      if i mod CHUNK_THRESHOLD = 0 then
        statBar.SimpleText := Format('Generated %d/%d',[i,RECORD_COUNT]);
    end;
  finally
    CloseFile(F);
  end;
  AssignFile(F,'mcus.dat'); Reset(F);
  try TotalRecords := FileSize(F); finally CloseFile(F); end;
  CurrentPage := 0;
  DisplaySample('After Generation');
end;

{Сортировка}
procedure TForm1.btnSortClick(Sender: TObject);
var F: TMCUFile;
begin
  if not FileExists('mcus.dat') then Exit;
  statBar.SimpleText := 'Sorting...'; Application.ProcessMessages;
  ExternalMergeSort('mcus.dat', rgField.ItemIndex);
  AssignFile(F,'mcus.dat'); Reset(F);
  try TotalRecords := FileSize(F); finally CloseFile(F); end;
  CurrentPage := 0;
  DisplaySample('After Sort by '+rgField.Items[rgField.ItemIndex]);
end;

procedure TForm1.btnShowAllClick(Sender: TObject);
begin
  CurrentPage := 0; DisplayPage(CurrentPage);
end;

procedure TForm1.btnNextClick(Sender: TObject);
var MaxP: Int64;
begin
  MaxP := (TotalRecords + PAGE_SIZE - 1) div PAGE_SIZE - 1;
  if CurrentPage < MaxP then Inc(CurrentPage);
  DisplayPage(CurrentPage);
end;

procedure TForm1.btnPrevClick(Sender: TObject);
begin
  if CurrentPage > 0 then Dec(CurrentPage);
  DisplayPage(CurrentPage);
end;

{Постраничный вывод}
procedure TForm1.DisplayPage(Page: Int64);
var
  F: TMCUFile; rec: TMicrocontroller; i, startIdx: Int64; item: TListItem;
begin
  AssignFile(F,'mcus.dat'); Reset(F);
  try
    TotalRecords := FileSize(F);
    startIdx := Page * PAGE_SIZE;
    if startIdx >= TotalRecords then Exit;
    Seek(F, startIdx);
    lvOut.Items.BeginUpdate;
    try
      lvOut.Items.Clear;
      for i := 0 to PAGE_SIZE-1 do
      begin
        if startIdx + i >= TotalRecords then Break;
        Read(F,rec);
        item := lvOut.Items.Add; item.Caption := IntToStr(startIdx + i + 1);
        with item.SubItems do
        begin
          Add(rec.Manufacturer); Add(rec.Model);
          Add(Format('%.2f',[rec.ClockMHz]));
          Add(IntToStr(rec.FlashKB)); Add(IntToStr(rec.RAMKB));
          Add(IntToStr(rec.GPIOCount)); Add(IntToStr(rec.ADCChannels));
          Add(IntToStr(rec.ADCResolutionBits)); Add(IntToStr(rec.UARTCount));
          Add(rec.OtherInfo);
        end;
      end;
    finally lvOut.Items.EndUpdate; end;
  finally CloseFile(F); end;
  statBar.SimpleText := Format('Page %d/%d',[Page+1,(TotalRecords+PAGE_SIZE-1) div PAGE_SIZE]);
end;

procedure TForm1.DisplaySample(const ACaption: string);
var
  F: TMCUFile; rec: TMicrocontroller; i, total: Int64; item: TListItem;
begin
  AssignFile(F,'mcus.dat'); Reset(F);
  try
    total := FileSize(F);
    lvOut.Items.BeginUpdate;
    try
      lvOut.Items.Clear; statBar.SimpleText := ACaption;
      for i := 0 to Min(49, total-1) do
      begin
        Read(F,rec);
        item := lvOut.Items.Add; item.Caption := IntToStr(i+1);
        with item.SubItems do
        begin
          Add(rec.Manufacturer); Add(rec.Model);
          Add(Format('%.2f',[rec.ClockMHz]));
          Add(IntToStr(rec.FlashKB)); Add(IntToStr(rec.RAMKB));
          Add(IntToStr(rec.GPIOCount)); Add(IntToStr(rec.ADCChannels));
          Add(IntToStr(rec.ADCResolutionBits)); Add(IntToStr(rec.UARTCount));
          Add(rec.OtherInfo);
        end;
      end;
      if total > 50 then
        for i := Max(50, total-50) to total-1 do
        begin
          Seek(F,i); Read(F,rec);
          item := lvOut.Items.Add; item.Caption := IntToStr(i+1);
          with item.SubItems do
          begin
            Add(rec.Manufacturer); Add(rec.Model);
            Add(Format('%.2f',[rec.ClockMHz]));
            Add(IntToStr(rec.FlashKB)); Add(IntToStr(rec.RAMKB));
            Add(IntToStr(rec.GPIOCount)); Add(IntToStr(rec.ADCChannels));
            Add(IntToStr(rec.ADCResolutionBits)); Add(IntToStr(rec.UARTCount));
            Add(rec.OtherInfo);
          end;
        end;
    finally lvOut.Items.EndUpdate; end;
  finally CloseFile(F); end;
end;

{Split -> quick-sort chunks -> k-way merge}
procedure TForm1.SplitToRuns(const InFile: string; fieldIndex: integer; out RunFiles: TStringList);
var
  FIn, FRun: TMCUFile;
  Buffer: array of TMicrocontroller;
  cnt, runIdx, i: integer;
  RunName: string;
begin
  RunFiles := TStringList.Create;
  SetLength(Buffer, CHUNK_THRESHOLD);

  AssignFile(FIn, InFile); Reset(FIn);
  try
    runIdx := 0;
    while not EOF(FIn) do
    begin
      cnt := 0;
      while (cnt < CHUNK_THRESHOLD) and not EOF(FIn) do
      begin
        Read(FIn, Buffer[cnt]);
        Inc(cnt);
      end;
      QuickSortChunk(Buffer, 0, cnt-1, fieldIndex);

      RunName := Format('run_%d.dat',[runIdx]);
      AssignFile(FRun, RunName); Rewrite(FRun);
      for i := 0 to cnt-1 do Write(FRun, Buffer[i]);
      CloseFile(FRun);

      RunFiles.Add(RunName);
      Inc(runIdx);
    end;
  finally
    CloseFile(FIn);
  end;
end;

procedure TForm1.KWayMerge(const RunFiles: TStringList; fieldIndex: integer; const OutFile: string);
var
  Files: array of TMCUFile;
  Curr:  array of TMicrocontroller;
  HasRec: array of Boolean;
  Heap:  array of TKWNode;
  heapSize, i, idx, l, r, smallest: integer;
  FOut: TMCUFile;
  node, tmp: TKWNode;
begin
  SetLength(Files, RunFiles.Count);
  SetLength(Curr,  RunFiles.Count);
  SetLength(HasRec, RunFiles.Count);

  for i := 0 to RunFiles.Count-1 do
  begin
    AssignFile(Files[i], RunFiles[i]); Reset(Files[i]);
    HasRec[i] := not EOF(Files[i]);
    if HasRec[i] then Read(Files[i], Curr[i]);
  end;

  heapSize := 0; SetLength(Heap, RunFiles.Count);
  for i := 0 to RunFiles.Count-1 do
    if HasRec[i] then
    begin
      Heap[heapSize].Rec := Curr[i];
      Heap[heapSize].SourceIdx := i;
      Inc(heapSize);
    end;

  for idx := (heapSize div 2)-1 downto 0 do
  begin
    l := 2*idx+1; r := 2*idx+2; smallest := idx;
    if (l<heapSize) and (CompareRecords(Heap[l].Rec, Heap[smallest].Rec, fieldIndex)<0) then smallest := l;
    if (r<heapSize) and (CompareRecords(Heap[r].Rec, Heap[smallest].Rec, fieldIndex)<0) then smallest := r;
    if smallest<>idx then
    begin
      tmp:=Heap[idx]; Heap[idx]:=Heap[smallest]; Heap[smallest]:=tmp;
    end;
  end;

  AssignFile(FOut, OutFile); Rewrite(FOut);
  try
    while heapSize>0 do
    begin
      node := Heap[0];
      Write(FOut, node.Rec);
      i := node.SourceIdx;
      if not EOF(Files[i]) then
      begin
        Read(Files[i], Curr[i]);
        Heap[0].Rec := Curr[i];
      end
      else
      begin
        Dec(heapSize);
        Heap[0] := Heap[heapSize];
      end;

      idx := 0;
      while True do
      begin
        l := 2*idx+1; r := 2*idx+2; smallest := idx;
        if (l<heapSize) and (CompareRecords(Heap[l].Rec, Heap[smallest].Rec, fieldIndex)<0) then smallest := l;
        if (r<heapSize) and (CompareRecords(Heap[r].Rec, Heap[smallest].Rec, fieldIndex)<0) then smallest := r;
        if smallest=idx then Break;
        tmp:=Heap[idx]; Heap[idx]:=Heap[smallest]; Heap[smallest]:=tmp;
        idx := smallest;
      end;
    end;
  finally
    CloseFile(FOut);
    for i := 0 to High(Files) do
    begin
      CloseFile(Files[i]);
      SafeDeleteFile(RunFiles[i]);
    end;
  end;
end;

procedure TForm1.ExternalMergeSort(const InFile: string; fieldIndex: integer);
var
  Runs: TStringList;
  TempOut: string;
begin
  SplitToRuns(InFile, fieldIndex, Runs);
  TempOut := InFile + '.tmp';
  KWayMerge(Runs, fieldIndex, TempOut);
  SafeDeleteFile(InFile);
  RenameFile(TempOut, InFile);
  Runs.Free;
end;

end.

