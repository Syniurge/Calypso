/** * std::valarray example.  
* 
* Build with: 
* $ ldc2 valarray.d
*/

modmap (C++) "<valarray>";

import std.stdio, std.conv, std.string;
import (C++) std.valarray;

void main()
{
    auto v = new valarray!(int)(24);
    writeln("valarray");
}