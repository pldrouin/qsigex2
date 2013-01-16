// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#ifndef _STRDIFFER_
#define _STRDIFFER_

inline bool strdiffer(const char* str1, const char* str2) {for(int i=0; str1[i]==str2[i]; ++i) {if(!str1[i]) return false;} return true;}

#endif
