//--------------------------------------------------------
//  tscalonl_main.C
//
//  Example to use the online scaler code that reads 
//  data from VME.
//  R. Michaels, April 2001
//  for hall C, Jan 2017
//--------------------------------------------------------

#include <iostream>
#include <string>
#include "THaScaler.h"

using namespace std;

int main(int argc, char* argv[]) {

  //   Default is SHMS for hall C
   string bank = "SHMS";  

   THaScaler scaler(bank.c_str());
   if (scaler.Init() == -1) {  
      cout << "Error initializing scalers"<<endl;  return 1;
   }

   int event = 0;

   cout << "Test of online scalers "<<endl;

   while ( scaler.LoadDataOnline() != SCAL_ERROR ) {  // each call gets 1 event

      scaler.Print();

      system("sleep 2");
   
      if (event++ > 100) break;

   }   

   return 0;
}








