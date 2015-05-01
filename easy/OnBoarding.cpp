#include <iostream>
#include <string>
#include <vector>

using namespace std;

int main() {
  while( 1 ) {
    string enemy1;
    cin >> enemy1; cin.ignore();
    int dist1;
    cin >> dist1; cin.ignore();
    string enemy2;
    cin >> enemy2; cin.ignore();
    int dist2;
    cin >> dist2; cin.ignore();

    cout << (dist1 < dist2 ? enemy1 : enemy2) << endl;
  }
}
