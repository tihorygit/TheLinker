extern int add(int a, int b);
extern "C" void print(const char* text);

int main()
{
	print("Hello World");
	return add(10, 20);
}
