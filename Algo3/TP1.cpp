// Solves problem UVA-11804 (Argentina)

#include <iostream>
#include <algorithm>
#include <tuple>
#include <vector>
#include <string>
#include <utility>
#include <numeric>

using namespace std;

int ataque, defensa;

vector<bool> jugadoresUsados(10, false);
vector<string> equipoTitularAtaqueDefinitivo(5);
vector<string> equipoTitularDefensaDefinitivo(5);
vector<string> equipoTitularAtaqueActual(5);
vector<string> equipoTitularDefensaActual(5);

void imprimirResultado(vector<string>& ataque, vector<string>& defensa, int cases){
    cout << "Case " << cases << ":" << endl;
    cout << "(";
    for(int i = 0; i < 5; i++){
        cout << ataque[i];
        if(i != 4){
            cout << ", ";
        }
    }
    cout << ")" << endl;
    cout << "(";
    for(int i = 0; i < 5; i++){
        cout << defensa[i];
        if(i != 4){
            cout << ", ";
        }
    }
    cout << ")" << endl;
    
}

class Jugador{
    public:
        string nombre;
        int ataque;
        int defensa;
};

void findBestTeam(vector<Jugador>& jugadores, int cantDefinidosAtaque, int cantDefinidosDefensa, int ataqueActual, int defensaActual){
    
    if(cantDefinidosAtaque + cantDefinidosDefensa == 10){
        if(ataqueActual > ataque){
            ataque = ataqueActual;
            equipoTitularAtaqueDefinitivo = equipoTitularAtaqueActual;
        }
        if(defensaActual > defensa){
            defensa = defensaActual;
            equipoTitularDefensaDefinitivo = equipoTitularDefensaActual;
        }
        return;
    }else{
        for(int i = 0; i < 10; i++){
            if(!jugadoresUsados[i]){ // si ya lo puse, no lo vuelvo a poner
                if(cantDefinidosAtaque < 5){
                    equipoTitularAtaqueActual[cantDefinidosAtaque] = jugadores[i].nombre;
                    jugadoresUsados[i] = true;
                    findBestTeam(jugadores, cantDefinidosAtaque + 1, cantDefinidosDefensa, ataqueActual + jugadores[i].ataque, defensaActual);
                }
                if(cantDefinidosDefensa < 5){
                    equipoTitularDefensaActual[cantDefinidosDefensa] = jugadores[i].nombre;
                    jugadoresUsados[i] = true;
                    findBestTeam(jugadores, cantDefinidosAtaque, cantDefinidosDefensa + 1, ataqueActual, defensaActual + jugadores[i].defensa);
                }
            }   
        }
    }
}


int main(){

    int cantCasos; cin >> cantCasos;

    int cases = 1;
    while(cantCasos--){
        vector<Jugador> jugadores(10);
        for(int i = 0; i < 10;i++){
            cin >> jugadores[i].nombre >> jugadores[i].ataque >> jugadores[i].defensa;
        }

        sort(jugadores.begin(), jugadores.end(), [](Jugador a, Jugador b){
            if(a.ataque == b.ataque){
                if(a.defensa == b.defensa){
                    //desempato por nombre en orden alfabetico
                    return a.nombre < b.nombre;
                }
                return a.defensa < b.defensa;
            }
            return a.ataque > b.ataque;
        });

        ataque = defensa = 0;
        jugadoresUsados = vector<bool>(10, false);

        findBestTeam(jugadores, 0, 0, 0, 0);

        sort(equipoTitularAtaqueDefinitivo.begin(), equipoTitularAtaqueDefinitivo.end());
        sort(equipoTitularDefensaDefinitivo.begin(), equipoTitularDefensaDefinitivo.end());
        imprimirResultado(equipoTitularAtaqueDefinitivo, equipoTitularDefensaDefinitivo, cases);
        cases++;
    }
}