program TheLinker;

{$ASMMODE INTEL}

uses
  SysUtils,
  Linker,
  DLLParser;

  procedure FakeWriteTextShortStr(Len: Longint; var f: Text; const s: ShortString);
  begin
    Write(S);
  end;

  procedure FakeWritelnEnd(var f: Text);
  begin
    WriteLn();
  end;

  procedure FakeIoCheck;
  begin

  end;


  procedure FakeInitUnits;
  begin
    WriteLn('Init Units');
  end;

  procedure FakeDoExit;
  begin
    WriteLn('Do Exit');
  end;

var
  I, SecAlign: Integer;
  L: TLinker;
  O: TLinkerObject;
  S: TLinkerObjectSection;
  T: QWord;
begin
  SecAlign := 64;
  T := GetTickCount64;
  L := TLinker.Create;
  with L do
  begin
    OutputFormat := lofPeX86_64;
    EntryPoint := 'P$OFILE_$$_FAKEMAIN';
    //EntryPoint := 'main';
    AddInput('../tests/ofile/OFile.o');
    AddInput('../tests/pascal_common_object_files/system.o');
    AddInput('../tests/pascal_common_object_files/fpintres.o');
    AddInput('../tests/pascal_common_object_files/objpas.o');
    AddInput('../tests/pascal_common_object_files/sysinit.o');
    begin //We must load dll here
      AddDLL('C:\Windows\System32\kernel32.dll', '_$dll$kernel32$');
      AddDLL('C:\Windows\System32\oleaut32.dll', '_$dll$oleaut32$');
      AddDLL('C:\Windows\System32\user32.dll', '_$dll$user32$');
    end;
    begin //Add Fake calls
      AddFakeCallSymbol('fpc_initializeunits', @FakeInitUnits);
      AddFakeCallSymbol('fpc_do_exit', @FakeDoExit);
      AddFakeCallSymbol('fpc_write_text_shortstr', @FakeWriteTextShortStr);
      AddFakeCallSymbol('fpc_iocheck', @FakeIoCheck);
      AddFakeCallSymbol('fpc_writeln_end', @FakeWritelnEnd);
    end;
    begin //Text Section
      //CurrentLocation:= SizeHeaders;
      CurrentLocation := Align(SecAlign);
      NewSection('.text');
      AddSymbol(TLinkerSymbol.Create('__text_start__', CurrentLocation));
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.init');
      end;
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.text');
        Link(O.FileName, '.stub');
        for S in O.Find('.text.*') do
        begin
          Link(O.FileName, S.Name);
        end;
        for S in O.Find('.gnu.linkonce.t.*') do
          Link(O.FileName, S.Name);
      end;
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        //This must sort before write
        for S in O.Find('.text$*') do
          Link(O.FileName, S.Name);
      end;
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.glue_7t');
      end;
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.glue_7');
      end;
      CurrentLocation := Align(8);
      AddSymbol(TLinkerSymbol.Create('___CTOR_LIST__', CurrentLocation));
      AddSymbol(TLinkerSymbol.Create('__CTOR_LIST__', CurrentLocation));
      WriteDWord($FFFFFFFF);
      WriteDWord($FFFFFFFF);
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.ctors');
      end;
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.ctor');
      end;
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        //This must sort before write
        for S in O.Find('.ctors.*') do
          Link(O.FileName, S.Name);
      end;
      WriteDWord(0);
      WriteDWord(0);
      AddSymbol(TLinkerSymbol.Create('___DTOR_LIST__', CurrentLocation));
      AddSymbol(TLinkerSymbol.Create('__DTOR_LIST__', CurrentLocation));
      WriteDWord($FFFFFFFF);
      WriteDWord($FFFFFFFF);
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.dtors');
      end;
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.dtor');
      end;
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        //This must sort before write
        for S in O.Find('.dtors.*') do
          Link(O.FileName, S.Name);
      end;
      WriteDWord(0);
      WriteDWord(0);
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.fini');
      end;
      ProvideSymbol('etext', CurrentLocation);
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.gcc_except_table');
      end;
    end;
    begin //Data section
      CurrentLocation := Align(SecAlign);
      NewSection('.data');
      AddSymbol(TLinkerSymbol.Create('__data_start__', CurrentLocation));
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.data');
        for S in O.Find('.data.*') do
          Link(O.FileName, S.Name);
        for S in O.Find('.gnu.linkonce.d.*') do
          Link(O.FileName, S.Name);
        for S in O.Find('.fpc*') do
          Link(O.FileName, S.Name);
      end;
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.data2');
      end;
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        //This must sort before write
        for S in O.Find('.data$*') do
          Link(O.FileName, S.Name);
      end;
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.jcr');
      end;
      ProvideSymbol('_tls_index', CurrentLocation);
      WriteDWord(0);
      AddSymbol(TLinkerSymbol.Create('__data_end__', CurrentLocation));
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.data_cygwin_nocopy');
      end;
    end;
    begin //RData section
      CurrentLocation := Align(SecAlign);
      NewSection('.rdata');
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.rdata');
      end;
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        for S in O.Find('.rdata.*') do
          Link(O.FileName, S.Name);
      end;
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.rodata');
        for S in O.Find('.rodata.*') do
          Link(O.FileName, S.Name);
        for S in O.Find('.gnu.linkonce.r.*') do
          Link(O.FileName, S.Name);
      end;
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        //This must sort before write
        for S in O.Find('.rdata$*') do
          Link(O.FileName, S.Name);
      end;
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.eh_frame');
      end;
      AddSymbol(TLinkerSymbol.Create('___RUNTIME_PSEUDO_RELOC_LIST__', CurrentLocation));
      AddSymbol(TLinkerSymbol.Create('__RUNTIME_PSEUDO_RELOC_LIST__', CurrentLocation));
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.rdata_runtime_pseudo_reloc');
      end;
      AddSymbol(TLinkerSymbol.Create('___RUNTIME_PSEUDO_RELOC_LIST_END__', CurrentLocation));
      AddSymbol(TLinkerSymbol.Create('__RUNTIME_PSEUDO_RELOC_LIST_END__', CurrentLocation));
    end;
    begin //PData section
      CurrentLocation := Align(SecAlign);
      NewSection('.pdata');
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.pdata');
        for S in O.Find('.pdata.*') do
          Link(O.FileName, S.Name);
      end;
    end;
    begin //XData section
      CurrentLocation := Align(SecAlign);
      NewSection('.xdata');
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.xdata');
        for S in O.Find('.xdata.*') do
          Link(O.FileName, S.Name);
      end;
    end;
    begin //BSS section
      CurrentLocation := Align(SecAlign);
      NewSection('.bss');
      AddSymbol(TLinkerSymbol.Create('__bss_start__', CurrentLocation));
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.bss');
        for S in O.Find('.bss.*') do
          Link(O.FileName, S.Name);
        for S in O.Find('.gnu.linkonce.b.*') do
          Link(O.FileName, S.Name);
      end;
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        //This must sort before write
        for S in O.Find('.bss$*') do
          Link(O.FileName, S.Name);
      end;
      //*(COMMON)
      AddSymbol(TLinkerSymbol.Create('__bss_end__', CurrentLocation));
    end;
    begin //EData section
      CurrentLocation := Align(SecAlign);
      NewSection('.edata');
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.edata');
      end;
    end;
    begin //IData section
      CurrentLocation := Align(SecAlign);
      NewSection('.idata');
      //This must sort before write
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.idata$2');
      end;
      //This must sort before write
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.idata$3');
      end;
      WriteDWord(0);
      WriteDWord(0);
      WriteDWord(0);
      WriteDWord(0);
      WriteDWord(0);
      //This must sort before write
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.idata$4');
      end;
      //This must sort before write
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.idata$5');
      end;
      //This must sort before write
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.idata$6');
      end;
      //This must sort before write
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.idata$7');
      end;
    end;
    begin //CRT section
      CurrentLocation := Align(SecAlign);
      NewSection('.CRT');
      AddSymbol(TLinkerSymbol.Create('___crt_xc_start__', CurrentLocation));
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        //This must sort before write
        for S in O.Find('.CRT$XC$*') do
          Link(O.FileName, S.Name);
      end;
      AddSymbol(TLinkerSymbol.Create('___crt_xc_end__', CurrentLocation));
      AddSymbol(TLinkerSymbol.Create('___crt_xi_start__', CurrentLocation));
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        //This must sort before write
        for S in O.Find('.CRT$XI$*') do
          Link(O.FileName, S.Name);
      end;
      AddSymbol(TLinkerSymbol.Create('___crt_xi_end__', CurrentLocation));
      AddSymbol(TLinkerSymbol.Create('___crt_xl_start__', CurrentLocation));
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        //This must sort before write
        for S in O.Find('.CRT$XL*') do
          Link(O.FileName, S.Name);
      end;
      ProvideSymbol('___crt_xl_end__', CurrentLocation);
      AddSymbol(TLinkerSymbol.Create('___crt_xp_start__', CurrentLocation));
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        //This must sort before write
        for S in O.Find('.CRT$XP$*') do
          Link(O.FileName, S.Name);
      end;
      AddSymbol(TLinkerSymbol.Create('___crt_xp_end__', CurrentLocation));
      AddSymbol(TLinkerSymbol.Create('___crt_xt_start__', CurrentLocation));
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        //This must sort before write
        for S in O.Find('.CRT$XT$*') do
          Link(O.FileName, S.Name);
      end;
      AddSymbol(TLinkerSymbol.Create('___crt_xt_end__', CurrentLocation));
    end;
    begin //Tls section
      CurrentLocation := Align(SecAlign);
      NewSection('.tls');
      AddSymbol(TLinkerSymbol.Create('___tls_start__', CurrentLocation));
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.tls');
        for S in O.Find('.tls.*') do
          Link(O.FileName, S.Name);
      end;
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        //This must sort before write
        for S in O.Find('.tls$*') do
          Link(O.FileName, S.Name);
      end;
      AddSymbol(TLinkerSymbol.Create('___tls_end__', CurrentLocation));
    end;
    begin //RSrc section
      CurrentLocation := Align(SecAlign);
      NewSection('.rsrc');
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        Link(O.FileName, '.rsrc');
      end;
      for I := 0 to ObjectCount - 1 do
      begin
        O := &Object[I];
        //This must sort before write
        for S in O.Find('.rsrc$*') do
          Link(O.FileName, S.Name);
      end;
    end;
    MakeExecutable;
    Execute;
  end;
  WriteLn(GetTickCount64 - T);
  ReadLn;
end.
