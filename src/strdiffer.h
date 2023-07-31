// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

#ifndef _STRDIFFER_
#define _STRDIFFER_

inline bool strdiffer(const char* str1, const char* str2) {for(int i=0; str1[i]==str2[i]; ++i) {if(!str1[i]) return false;} return true;}

#endif
