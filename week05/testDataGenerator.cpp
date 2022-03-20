#include <iostream>
#include <string>
#include <stdio.h>      
#include <time.h> 
using namespace std;

void generate(string name, int year) {
	for (int i = 0;i < 12; i++)
	cout<<"    (\""+ name + "\", " + to_string(rand() % 60 - 30) + ", "
		+ to_string(i) + ", " + to_string(year) + "),"<<endl;
	
}

int main () {
	cout<<"[ "<<endl;
	for (int i = 2010;i < 2022;i++) {
		generate("Burgas", i);
		generate("Varna", i);
		generate("Plovdiv", i);
		generate("Sofia", i);
		generate("Vratsa", i);
	}
	cout<<"]"<<endl;
	return 0;
}