#include <iostream>
using namespace std;
#include <fstream>
//#include <stdio.h>
//#include <cstdio>

extern "C" {
	
	void UpdateHeaderTSD_float(const char* *filename, float *lvar, int *nlvar, int *bytes, int *pos)
	{
				
		ofstream thisfile ( *filename, ios::in | ios::out | ios::binary);
		
		thisfile.seekp (*pos, ios::beg);
		
		thisfile.write ((char*)&*lvar, *nlvar * *bytes);
		
	}
}
