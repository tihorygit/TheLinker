program OFile;

uses
  Windows;

  procedure FakeMain();
  begin
    MessageBox(0, 'Hello World', 'Hello Message', MB_OK);
  end;

begin
  FakeMain();
end.
