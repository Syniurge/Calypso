#pragma once

#define tims_my_def2(x) (2*x)

template<typename T> auto _tims_my_def2(const T& x){
  return tims_my_def2(x);
}
