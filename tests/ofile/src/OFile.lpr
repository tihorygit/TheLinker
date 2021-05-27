program OFile;

uses
  Windows;

  procedure FakeMain();
  begin
    MessageBox(0, 'Hello World', 'Hello Message', MB_OK);
    WriteLn('Hello World');
    CreateFileA('X.X', GENERIC_ALL, 0, nil, CREATE_ALWAYS, 0, 0);
  end;

begin
  FakeMain();
end.
