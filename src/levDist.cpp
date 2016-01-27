#include <Rcpp.h>
#include <string>
#include <iostream>
using namespace Rcpp;
using namespace std;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
size_t uiLevenshteinDistance(const std::string &s1, const std::string &s2)
{
  const size_t m(s1.size());
  const size_t n(s2.size());
  
  if( m==0 ) return n;
  if( n==0 ) return m;
  
  size_t *costs = new size_t[n + 1];
  
  for( size_t k=0; k<=n; k++ ) costs[k] = k;
  
  size_t i = 0;
  for ( std::string::const_iterator it1 = s1.begin(); it1 != s1.end(); ++it1, ++i )
  {
    costs[0] = i+1;
    size_t corner = i;
    
    size_t j = 0;
    for ( std::string::const_iterator it2 = s2.begin(); it2 != s2.end(); ++it2, ++j )
    {
      size_t upper = costs[j+1];
      if( *it1 == *it2 )
      {
        costs[j+1] = corner;
      }
      else
      {
        size_t t(upper<corner?upper:corner);
        costs[j+1] = (costs[j]<t?costs[j]:t)+1;
      }
      
      corner = upper;
    }
  }
  
  size_t result = costs[n];
  delete [] costs;
  
  return result;
}

// [[Rcpp::export]]
IntegerVector levVectors(
    CharacterVector x,
    CharacterVector y
){
  IntegerVector Result(x.length());

  
  for (int i=0; i<x.length();i++){
    std::string x_ = Rcpp::as<std::string>(x[i]); 
    std::string y_ = Rcpp::as<std::string>(y[i]); 
    Result[i] = uiLevenshteinDistance(x_,y_);
    // Result[i] = foo(1,2);
  }
  return Result;
}
// 
// // Now for lists:
// // [[Rcpp::export]]
// LogicalVector testPair(
//   List x,
//   List y,
//   int maxDist
// ){
//   LogicalVector Result(x.length());
//   
//   for (int i = 0; i < x.length(); i++)
//   {
//       IntegerVector Pair =  levVectors(x[i], y[i]) < maxDist; 
//       logical test = false;
//       for (int )
//   } 
//   
//   
//   return Result;
// }
