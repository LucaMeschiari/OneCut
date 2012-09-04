// Programma di risoluzione del problema "Cutting stock problem"


#include <iostream>
#include <fstream>
#include <math.h>
#include <time.h>
#include <stdio.h>
#include "ClpSimplex.hpp"
#include "MTU2.h"
#include "f2c.h"
using namespace std;


typedef struct Dati{
	int n_ogg; //pezzi diversi da tagliare
	double l_lastra; //lunghezza lastra
	double* l_ogg; //lunghezza oggetti
	int* n_pezzi; //n pezzi per oggetto
};	


char GraficaIN ();
Dati Inserimento();
Dati Esempio1();
Dati Esempio2();
void Risolvi(Dati);
ClpSimplex Prep(Dati);
void Knap(double*,Dati,int *);
int Arrotonda1(double);
int Arrotonda2(double);
double Check(int*,double*,int);
void LogSim(double *, double*, int, int );
void LogK(int,int,int *, int);
ClpSimplex AggCol(ClpSimplex,int*m,int);
void StampaR(ClpSimplex,Dati);
void StampaD(Dati);
void CreaLog();


//main
int main (int argc, char * const argv[]) {
	char Sel;
	Dati In;
	CreaLog();
	do {
		Sel=GraficaIN();
		if ((Sel == '1') or (Sel == '2') or (Sel == '3')){
			if (Sel== '1') {
				In=Inserimento();
			}
			if (Sel=='2') {
				In=Esempio1();
			}
			if (Sel=='3') {
				In=Esempio2();
			}
			Risolvi(In);
		}
	} while (1);
}


//Gestione grafica iniziale
char GraficaIN ()
{
	char scelta = '0';
	
	do {
		system ("clear");
		cout << "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ \n";
		cout << "---------------Problema del Taglio monodimensionale-------------- \n";
		cout << "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ \n";
		cout << " Seleziona:\n";
		cout << "\n";
		cout << " 1 - Inserisci Dati\n";
		cout << " 2 - Risolvi un problema preimpostato\n";
		cout << " 3 - Esci\n";
		cout << "\n";
		cout << "Scegli l'opzione desiderata e premi invio: ";
		cin >> scelta;
	} while ((scelta!='1') && (scelta!='2') && (scelta!='3'));
	switch (scelta) {
		case '3':
			exit(0);
			break;
		case '1':
			return scelta;
			break;
		case '2':
			do {
				system ("clear");
				cout << "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ \n";
				cout << "--------------------------Scelta Esempio------------------------- \n";
				cout << "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ \n";
				cout << " Seleziona:                                                       \n";
				cout << "                                                                  \n";
				cout << " 1 - Esempio 1                                                    \n";
				cout << "      (Dati: Lunghezza assi iniziali= 17 metri, si richiedono:    \n"; 
				cout << "             - 15 assi da 9 metri                                 \n";
				cout << "             - 20 assi da 5 metri                                 \n";
				cout << "             - 25 assi da 3 metri)                                \n";
				cout << " 2 - Esempio 2                                                    \n";
				cout << "      (Dati: Lunghezza lastre iniziali= 5400 cm, si richiedono:   \n";
				cout << "             - 1000 lastre da 1267.2 cm                           \n";
				cout << "             - 2300 lastre da 66.3 cm                             \n";
				cout << "             - 1896 lastre da 7.83 cm                             \n";
				cout << "             - 567 lastre da 9.7 cm                               \n";
				cout << "             - 50 lastre da 456 cm)                               \n";
				cout << " 3 - Ritorna alla schermata precedente                            \n";
				cout << "                                                                  \n";
				cout << "Scegli l'opzione desiderata e premi invio: ";
				cin >> scelta;
			} while ((scelta!='1') && (scelta!='2') && (scelta!='3'));
			switch (scelta) {
				case '1':
					return '2';
					break;
				case '2':
					return '3';
					break;
				case '3':
					return '4';
					break;	
				default:
					break;
			}
			break;			
		default:
			break;
	}
	return 10;
}
	

//Inserimento dati problema da utente
Dati Inserimento(){
	int i;
	char ch;
	Dati input;
	do{
		do{
			system ("clear");
			cout << "Inserisci il numero di oggetti diversi da tagliare: ";
			cin>>input.n_ogg;
		}while (input.n_ogg<=0);
		input.n_pezzi=new int[input.n_ogg];	
		input.l_ogg=new double[input.n_ogg];
		do{
			system ("clear");
			cout << "- Numero di oggetti: "<<input.n_ogg<<endl<<endl ;
			cout << "Inserisci la lunghezza della lastra: ";
			cin>>input.l_lastra;
		}while (input.l_lastra<=0);
		for (i=1;i<=input.n_ogg;i++){
			do{
				system ("clear");
				cout << "- Numero di oggetti: "<<input.n_ogg<<endl ;
				cout << "- Lunghezza lastra: "<<input.l_lastra<<endl<<endl ;
				cout << "Inserisci la lunghezza dell'oggetto "<<i<<": ";
				cin>>input.l_ogg[i-1];
			}while ((input.l_ogg[i-1]<=0) or (input.l_ogg[i-1]>input.l_lastra));
		}	
		for (i=1;i<=input.n_ogg;i++){
			do{
				system ("clear");
				cout << "Numero di oggetti: "<<input.n_ogg<<endl ;
				cout << "Lunghezza lastra: "<<input.l_lastra<<endl<<endl ;
				cout << "Inserisci il numero di pezzi desiderati per l'oggetto "<<i<<": ";
				cin>>input.n_pezzi[i-1];
			}while (input.n_pezzi[i-1]<=0);
		}
		StampaD(input);
		cout << endl<<endl<<"Premi c per confermare o qualsiasi altro tasto per riniziare l'inserimento, poi premi invio: ";
		cin >> ch;
	}while (ch != 'c');	
	return input;
}	
	
		
// Risoluzione simplesso+knapsack+controllo
void Risolvi(Dati In){
	ClpSimplex model;
	int end=0;
	int SolK[In.n_ogg];

	model=Prep(In);
	do{
		model.primal();
		double * rowDual = model.dualRowSolution();
		double * columnPrimal = model.primalColumnSolution();
		LogSim(rowDual,columnPrimal,model.numberRows(),model.numberColumns());	
		Knap(rowDual,In,SolK);	
		if (Check(SolK,rowDual,In.n_ogg)<=1) {
			StampaR(model,In);
			break;
			end=1;
		}
		else{
			model=AggCol(model,SolK,In.n_ogg);
		}
	}while (end==0);
}


//Preparazione dati per il simplesso
ClpSimplex Prep(Dati x){
	ClpSimplex  mod;
	int j;
	int numberRows=x.n_ogg;
	int numberColumns=x.n_ogg;
	int numberElements=x.n_ogg;
	CoinBigIndex start[x.n_ogg+1];
	int length[x.n_ogg];
	int rows[x.n_ogg];
	double elements[x.n_ogg];
	
	//popolo i vettori rows e lenght
	for (j=0; j<x.n_ogg; j++) {
		length[j]=1;
		rows[j]=j;
	}
	//popolo il vettore start
	for (j=0; j<x.n_ogg+1; j++) {
		start[j]=j;
	}
	//popolo il vettore elements
	for (j=0; j<x.n_ogg; j++) {
		elements[j]=floor(x.l_lastra/x.l_ogg[j]);
	}
	CoinPackedMatrix matrix(true,numberRows,numberColumns,numberElements,elements,rows,start,length);
	// rim data
	double objective[x.n_ogg];
	double rowLower[x.n_ogg];
	double rowUpper[x.n_ogg];
	double colLower[x.n_ogg];
	double colUpper[x.n_ogg];
	//popolo i vettori
	for (j=0; j<x.n_ogg; j++) {
		objective[j]=1;
		rowLower[j]=x.n_pezzi[j];
		rowUpper[j]=COIN_DBL_MAX;
		colLower[j]=0;
		colUpper[j]=COIN_DBL_MAX;
	}
	// load problem
	mod.loadProblem(matrix,colLower,colUpper,objective,
					  rowLower,rowUpper);
	return(mod);
}	


//Inserimento Dati Esempio 1
Dati Esempio1(){
	Dati E1;
	
	E1.n_ogg=3;
	E1.l_lastra=17;
	E1.n_pezzi=new int[3];	
	E1.l_ogg=new double[3];
	//numero pezzi
	E1.n_pezzi[0]=15;
	E1.n_pezzi[1]=20;
	E1.n_pezzi[2]=25;
	//lunghezza oggetti
	E1.l_ogg[0]=9.0;
	E1.l_ogg[1]=5.0;
	E1.l_ogg[2]=3.0;
	return E1;
}


//Inserimento Dati esempio 2
Dati Esempio2(){
	Dati E2;
	
	E2.n_ogg=5;
	E2.l_lastra=5400;
	E2.n_pezzi=new int[5];	
	E2.l_ogg=new double[5];
	//numero pezzi
	E2.n_pezzi[0]=1000;
	E2.n_pezzi[1]=2300;
	E2.n_pezzi[2]=1896;
	E2.n_pezzi[3]=567;
	E2.n_pezzi[4]=50;
	//lunghezza oggetti
	E2.l_ogg[0]=1267.2;
	E2.l_ogg[1]=66.3;
	E2.l_ogg[2]=7.83;
	E2.l_ogg[3]=9.7;
	E2.l_ogg[4]=456;
	return E2;
}	


//Knapsack
void Knap(double * DuRo, Dati D,int* Risultati)
{
	integer num=D.n_ogg;
	integer P[D.n_ogg];
	integer W[D.n_ogg];
	integer c=Arrotonda2(D.l_lastra);
	integer jdim=D.n_ogg+1;
	integer jfo = 1;
	integer JCK = 1;
	integer z;
	integer x[D.n_ogg+1];
	integer jub;
	integer po[D.n_ogg+1];
	integer wo[D.n_ogg+1];
	real xo[D.n_ogg+1];
	real rr[D.n_ogg+1];
	integer pp[D.n_ogg+1];
	int w;
	int t;
	
	for (w=0;w<D.n_ogg;w++){
		t=Arrotonda1(DuRo[w]);
		if (t==0) {
			t=1;
		}
		P[w]=t;
		W[w]=Arrotonda2(D.l_ogg[w]);
	}	
	mtu2_(&num, P, W, &c, &z, x, &jdim, &jfo, &JCK, &jub, po, wo, xo, rr, pp);
	for (w=0;w<D.n_ogg;w++){
		Risultati[w]=(int)x[w];
	}
	LogK(z,jub,Risultati,D.n_ogg);
	return;
}	
		  
		  		  
//Arrotonda i valori P entranti nel knapsack alla ottava(modificabile) cifra decimale trasformandoli in int		  
int Arrotonda1(double a){
	int u;
	
	a=a*10000000;
	a=a-0.5;
	a=ceil(a);
	u=(int)a;
	return (u);
}


//Arrotonda i valori W e l entranti nel knapsack alla quinta(modificabile) cifra decimale trasformandoli in int	
int Arrotonda2(double a){
	int u;
	
	a=a*10000;
	a=a-0.5;
	a=ceil(a);
	u=(int)a;
	return (u);
}




//Controlla se la soluzione ottenuta è ottima
double Check(int* Kn, double* Si,int no){
	double res=0;
	int c;
	
	for (c=0;c<no;c++){
		res=(Kn[c]*Si[c])+res;
	}
	return res;
}

	
//Aggiunge la colonna con i dati ottenuti dal knapsack alla matrice che verrà calcolate da simplesso
ClpSimplex AggCol(ClpSimplex M, int* Kna, int n)
{
	int count=0,ContIn=0,G=0; 
	int column2Index[n];
	double column2Value[n];
	
	
	for (G=0; G<n; G++) {
		if (Kna[G]>0) {
			count++;
			column2Index[ContIn]=G;
			column2Value[ContIn]=Kna[G];
			ContIn++;		
		}
	}
	M.addColumn(count,column2Index,column2Value,0.0,COIN_DBL_MAX,1);
	return M;
}


//Stampa i risultati finali
void StampaR (ClpSimplex CS,Dati Da){
	CoinPackedMatrix M=* CS.matrix();
	int i,j,x=0,r,tot=0;
	char c;
	double * columnPrimal = CS.primalColumnSolution();
	const double * EL = M.getElements();
	const int * Ind = M.getIndices();
	const CoinBigIndex* ST= M.getVectorStarts();
	
	StampaD(Da);
	cout <<endl<<endl;
	cout << "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ \n";
	cout << "---------------------------Risultati----------------------------- \n";
	cout << "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ \n"<<endl;
	for (i=0; i<CS.numberColumns();i++) {
		tot=(int)ceil(columnPrimal[i])+tot;
		cout << "- pattern "<<i<<":\t|\t";
		for (j=0; j<CS.numberRows(); j++) {
			if((Ind[x]==j)and(x<(int)ST[i+1])){
				cout << "Ogg["<<j+1<<"] = "<<EL[x]<<"\t|\t";
				x++;
			}
			else {
				cout << "Ogg["<<j+1<<"] = "<<0<< "\t|\t";
			}

		}
		cout << "Utilizzato "<<ceil(columnPrimal[i])<<"  volte"<<endl;	
	}
	cout<<endl<<"Totale lastre utilizzate: "<<tot<<endl;
	cout<<endl<<endl<<"Premi il tasto invio per tornare alla schermata iniziale: "<<endl;
	r = read (STDIN_FILENO, &c, sizeof(char));
}	


//Stampa i Dati del problema come riepilogo
void StampaD(Dati D){
	int i;
	
	system ("clear");
	cout << "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ \n";
	cout << "---------------------------Riepilogo----------------------------- \n";
	cout << "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ \n";
	cout <<endl<< "- Numero di oggetti: "<<D.n_ogg<<endl ;
	cout << "- Lunghezza lastra: "<<D.l_lastra<<endl<<endl ;
	for(i=0;i<D.n_ogg;i++){
		cout <<"- Oggetto "<<i+1<<":  Lunghezza: "<<D.l_ogg[i]<<"  Numero pezzi: "<<D.n_pezzi[i]<<endl;
	}
}	


//Crea il file di log o sovrascrive quello già esistente
void CreaLog(){
	int s=0;
	char ch;
	time_t ltime;
	
	time( &ltime );
	ofstream f("log.txt", ios::out);
    if(!f) {
        cout<<"Errore nella creazione del file di Log, premi invio per continuare";
		s = read (STDIN_FILENO, &ch, sizeof(char));
        return;
    }
	f<<"File di Log"<<endl<<ctime( &ltime );
	f.close();
}	


//Scrive i risultati del simplesso sul file di Log
void LogSim(double* R, double* C, int NR, int NC)
{
	int i;
	char ch;	
	
	ofstream f("log.txt", ios::app);
    if(!f) {
        cout<<"Errore nell'apertura del file di Log, premi invio per continuare";
		i = read (STDIN_FILENO, &ch, sizeof(char));
        return;
    }
	f<<endl<<endl<<"SIMPLESSO"<<endl;
	for (i=0;i<NR;i++)        
		f<<"Row "<<i<<" dual: "<<R[i]<<endl;
	for (i=0;i<NC;i++)        
		f<<"Column "<<i<<" primal:" <<C[i]<<endl;
	f.close();
}


//Scrive i risultati del knapsack sul file di Log
void LogK(int Z, int Jub, int * X,int n){
	int s=0;
	char ch;
	
	ofstream f("log.txt", ios::app);
    if(!f) {
        cout<<"Errore nell'apertura del file di Log, premi invio per continuare";
		s = read (STDIN_FILENO, &ch, sizeof(char));
        return;
    }
	f << endl<<endl<<"KNAPSACK"<<endl<<"Z= "<<Z<<" JUB= "<<Jub<<endl;
	for(s=0; s<n;s++){
		f<< "Valore "<<s<<": "<<X[s]<<endl;
	}
	f.close();
}	



	
