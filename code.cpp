#include <algorithm>
#include <bitset>
#include <complex>
#include <deque>
#include <exception>
#include <fstream>
#include <functional>
#include <iomanip>
#include <ios>
#include <iosfwd>
#include <iostream>
#include <istream>
#include <iterator>
#include <limits>
#include <list>
#include <locale>
#include <map>
#include <memory>
#include <new>
#include <numeric>
#include <ostream>
#include <queue>
#include <set>
#include <sstream>
#include <stack>
#include <stdexcept>
#include <streambuf>
#include <string>
#include <typeinfo>
#include <utility>
#include <valarray>
#include <vector>
#include <array>
#include <atomic>
#include <chrono>
#include <condition_variable>
#include <forward_list>
#include <future>
#include <mutex>
#include <random>
#include <ratio>
#include <regex>
#include <scoped_allocator>
#include <system_error>
#include <thread>
#include <tuple>
#include <typeindex>
#include <type_traits>
#include <unordered_map>
#include <unordered_set> 
//#include <bits/stdc++.h>
using namespace std;
#define ld long double
#define ll long long int
#define mp make_pair

bool checks(string wow) {
	bool ok = true;
	set<char> ror;
	for (auto v : wow) {
		if (v < '0' || v>'9') {
			ok = false; break;
		}
	}
	if (wow.size() == 0) { ok = false; }
	return ok;
}
ld secnd(ld zam) {
	return zam / (ld)1000000.0;
}
vector<pair<string, string>> train;
hash<string> qoq;
vector<pair<string, string>> crr, brr;
map<ld, int> all;
ld ans = -1;
vector<ld> sos1;
vector<int> sos2;
vector<ld> force;
void recursive(ld base, ld prv, ld aft, int a, int b, int ir) {
	if (aft < 29.0) { return; }
	brr.clear(); all.clear();
	for (int z = a; z <= b; z++) {
		brr.push_back(crr[z]);
		all[stof(crr[z].second)]++;
	}

	if (brr.size() > 0) {
		for (int i = 0; i < (int)brr.size() + 1; i++) {
			sos1[i] = 0.0;
			sos2[i] = 0.0;
		}
	}
	ld jam = base;
	auto it = sos1.begin();
	for (int i = 0; i < brr.size(); i++) {
		it++;
		jam = jam + stof(brr[i].first);
		ld lam = secnd(jam);
		sos1[i] = lam;
		all[stof(brr[i].second)]--;
		if (all[stof(brr[i].second)] > 3) {
			all[stof(brr[i].second)]++;
			brr[i].second = "!";
			sos2[i] = 0;
		}
		else {
			sos2[i] = 1;
			all[stof(brr[i].second)]++;
		}
		if (i >= 1) {
			sos2[i] = sos2[i] + sos2[i - 1];
		}
	}
	sos1[brr.size()] = jam + 1.0;
	ld slc = (aft - prv) / 200.0;
	ld ov = slc / 2.0;
	//*****
	vector < pair<pair<ld, ld>, pair<ld, ld>>> prr;
	for (ld ab = prv; ab <= aft; ab = ab + slc) {
		if (ab + ov > aft || ab - ov < 0.0 || ab < 30) { continue; }
		int rak = 0, tart = 0;
		int lf = -1, ri = -1;
		int df1 = upper_bound(sos1.begin(), it, (ld)(ab - ov - 0.00001)) - sos1.begin();
		int df2 = upper_bound(sos1.begin(), it, (ld)(ab + ov - 0.00001)) - sos1.begin(); df2--;
		rak = sos2[df2];
		if (df1 > 0) { rak = rak - sos2[df1 - 1]; }
		tart = df2 - df1 + 1;
		if (tart > 0.0 && rak > 0.0) {
			if (1) {
				//			cout << (ld)rak / (ld)tart << " " << ab << endl;
			}
			prr.push_back(mp(mp((ld)rak / (ld)tart, ab), mp(df1, df2)));
		}
	}
	ld kam = 0.0;
	pair<ld, ld> best = mp(10000, 10000);
	ld cucu = (aft - prv) / 60.0;
	for (int i = 0; i < prr.size(); i++) {
		if (i != 0) {
			kam = kam + prr[i].first.second - prr[i - 1].first.second;
		}
		if (kam >= cucu) {
			force.push_back(best.second);
			best = mp(10000, 10000);
			kam = 0.0;
		}
		else {
			if (best.first > prr[i].first.first) {
				best.first = prr[i].first.first;
				best.second = prr[i].first.second;
			}
		}
	}
}
ld vas[6][2][5001];
bool ID1(vector<ld> vov) {
	int sum = 0;
	if (vov[0] >= 250.0) { sum++; }
	if (vov[1] >= 0.75) { sum++; }
	if (vov[2] >= 10000.0 && vov[2] < 41000.0) { sum++; }
	if (vov[3] >= 10.3 && vov[3] <= 15.0) { sum++; }
	if (vov[4] >= 0.21 && vov[4] <= 0.25) { sum++; }
	if (vov[5] >= 0.015 && vov[5] <= 0.018) { sum++; }
	if (vov[6] >= 0.140 && vov[6] <= 0.150) { sum++; }
	if (vov[7] >= 0.111 && vov[7] <= 0.115) { sum++; }
	if (vov[8] >= 0.48 && vov[8] < 0.85) { sum++; }
	if (vov[9] >= (0.000000004) && vov[9] <= (0.000000009)) { sum++; }
	return (sum == 10);
}
int dog;
bool ID2(vector<ld> vov) {
	int sum = 0;
	if (vov[0] >= 140.0 && vov[0] <= 400.0) { sum++; }
	if (vov[1] >= 0.75) { sum++; }
	if (vov[2] >= 41000.0 || (vov[3] >= 12.3)) { sum++; }
	if (vov[4] >= 0.22 && vov[4] <= 0.30) { sum++; }
	if (vov[5] >= 0.017 && vov[5] <= 0.026) { sum++; }
	if (vov[6] >= 0.142 && vov[6] <= 0.149) { sum++; }
	if (vov[7] >= 0.111 && vov[7] <= 0.115) { sum++; }
	if (vov[8] >= 0.45 && vov[8] < 0.85) { sum++; }
	if (vov[9] >= (0.000000004) && vov[9] <= (0.000000012)) { sum++; }
	if (vov[2] >= 41000.0) {
		dog = 1;
	}
	else {
		dog = 2;
	}
	return (sum == 9);
}
bool ID3(vector<ld> vov) {
	int sum = 0;
	if (vov[0] >= 150.0 && vov[0] <= 250.0) { sum++; }
	if (vov[1] >= 0.74) { sum++; }
	if (vov[2] >= 20000.0 && vov[2] < 36000.0) { sum++; }
	if (vov[3] >= 10.0 && vov[3] <= 18.0) { sum++; }
	if (vov[4] >= 0.298 || vov[5] >= 0.022) { sum++; }
	if (vov[6] >= 0.145 && vov[6] <= 0.150) { sum++; }
	if (vov[7] >= 0.111 && vov[7] <= 0.115) { sum++; }
	if (vov[8] >= 0.50 && vov[8] < 0.85) { sum++; }
	if (vov[9] >= (0.0000000065) && vov[9] <= (0.00000001)) { sum++; }
	return (sum == 9);
}
bool ID4(vector<ld> vov) {
	return (vov[6] >= 0.1551 || vov[7] >= 0.1176);
}
int yoy;
bool ID6(vector<ld> vov) {
	int sum = 0;
	if (vov[0] >= 100.0 && vov[0] <= 6000.0) { sum++; }
	if (vov[8] >= 0.85 || vov[9] >= (0.000000012)) { sum++; }
	if (vov[8] >= 0.85) {
		yoy = 1;
	}
	else if (vov[9] >= (0.000000012)) {
		yoy = 2;
	}
	return (sum == 2);
}
int main()
{
	for (int i = 0; i <= 5; i++) {
		for (int j = 0; j <= 1; j++) {
			for (int z = 0; z <= 5000; z++) {
				vas[i][j][z] = -1;
			}
		}
	}
	ifstream sas;
	sas.open("D:\\programming\\radio\\SourceInfov3\\SourceData.csv", ios::in);

	string st, sid, shield, pe, cntr;
	bool fir = true;
	while (!sas.eof()) {
		getline(sas, st, ',');
		getline(sas, sid, ',');
		getline(sas, shield, ',');
		getline(sas, pe, ',');
		sas >> cntr;
		if (fir) {
			fir = false;
			continue;
		}
		if (!sas.eof()) {
			vas[stoi(sid)][stoi(shield)][stoi(pe)] = stof(cntr);
		}
	}
	sas.close();
	sas.clear();
	//102894
	//103555
	//104901
	//105701
	//106501
	//107301
	//108101
	//108901
	int beg = 200001, last = 215840;
	for (int cant = beg; cant <= last; cant++) {
		string res = to_string(cant);
		string ws = res;
		res = "D:\\programming\\radio\\testing\\" + res + ".csv";
		train.push_back(mp(res, ws));
	}
	//****test it on example!
	ofstream kill;
	kill.open("solution56.csv", ios::out);
	kill << "RunID" << "," << "SourceID" << "," << "SourceTime" << endl;
	for (auto v : train) {
		cout << v.second << endl;
		ifstream *exambl;
		exambl = new ifstream(v.first, ios::in);
		string sc, kev;
		crr.clear(); all.clear(); brr.clear();
		while (!exambl->eof()) {
			getline(*exambl, sc, ',');
			*exambl >> kev;
			if (!exambl->eof()) {
				crr.push_back(mp(sc, kev));
				all[stof(kev)]++;
			}
		}
		for (auto v : crr) {
			brr.push_back(v);
		}
		ld tmt = -1, zam = 0.0;
		for (int i = 0; i < brr.size(); i++) {
			all[stof(brr[i].second)]--;
			if (all[stof(brr[i].second)] > 3) {
				all[stof(brr[i].second)]++;
				brr[i].second = "!";
			}
			else {
				all[stof(brr[i].second)]++;
			}
			zam = zam + stof(brr[i].first);
		}
		ld akh = zam;
		tmt = secnd(zam);
		ans = -1;
		sos1.clear(); sos2.clear();
		sos1.resize(crr.size() + 1);
		sos2.resize(crr.size() + 1);
		force.clear();
		recursive(0, 0, secnd(zam), 0, brr.size() - 1, 0);

		vector<string> fap;
		vector<ld> fag;
		vector<pair<pair<int, pair<ld, ld>>, ld>> asas;
		ld max1 = 0, max5 = 0, ones = 0, mini = 1000000000, twos = 0;
		ld max62 = 0, val62 = 0, max61 = 0, val61 = 0;
		ld max31 = 0, max32 = 0, val31 = 0, val32 = 0;
		ld max21 = 0, val21 = 0, max22 = 0, val22 = 0;
		zam = 0.0;
		vector<ld> sps1, val; val.resize((int)crr.size());
		vector<int> super;
		for (int z = 0; z < crr.size(); z++) {
			zam = zam + stof(crr[z].first);
			sps1.push_back(secnd(zam));
			val[z] = stof(crr[z].second);
			if (z > 0) {
				val[z] = val[z] + val[z - 1];
			}
			int ab = stof(crr[z].second);
			if (ab % 2 == 0) { ab++; }
			super.push_back(ab);
		}
		ld fal = secnd(zam) / 50;
		for (auto ss : force) {
			if (ss < 29.8) { continue; }
			ans = ss;
			zam = 0.0;
			for (auto v : crr) {
				zam = zam + stof(v.first);
			}
			ld sm1 = 0, td1 = 0, sm2 = 0, td2 = 0;
			int sp1 = -1, sp2 = -1;
			sp1 = upper_bound(sps1.begin(), sps1.end(), ans - fal - 0.00001) - sps1.begin();
			sp2 = upper_bound(sps1.begin(), sps1.end(), ans + fal - 0.00001) - sps1.begin(); sp2--;
			sm2 = val[sp2];
			if (sp1) { sm2 = sm2 - val[sp1 - 1]; }
			td2 = sp2 - sp1 + 1;
			td1 = (int)crr.size() - td2;
			if (sp1) { sm1 += val[sp1 - 1]; }
			sm1 = sm1 + val[(int)crr.size() - 1];
			sm1 = sm1 - val[sp2];
			if (1) {
				string sourid = "0";
				vector<ld> vov, row;
				for (int sor = 1; sor <= 5; sor++) {
					for (int tp = 0; tp <= 1; tp++) {
						ld sam = 0, ted = 0;
						ld sam2 = 0, ted2 = 0;
						for (int x = sp1; x <= sp2; x++) {
							if (vas[sor][tp][super[x]]>(ld)-1.0) {
								sam = sam + vas[sor][tp][super[x]];
								ted = ted + 1.0;
							}
						}
						if (((sor == 1 || sor == 5) && tp == 0) || sor == 3) {
							for (int x = sp1; x <= sp2; x++) {
								if (0.0<vas[sor][tp][super[x]]) {
									sam2 = sam2 + vas[sor][tp][super[x]];
									ted2 = ted2 + 1.0;
								}
							}
							if (sam2 == 0.0) { ted2 = 1; }
							row.push_back((ld)sam2 / ted2);
						}
						//					cout << sor << " " << (ld)sam / ted << endl;
						if (ted == 0.0) { ted = 1; }
						vov.push_back((ld)sam / ted);
					}
				}
				if (max21 < vov[2]) {
					max21 = vov[2];
					val21 = ans;
				}
				if (max22 < vov[3]) {
					max22 = vov[3];
					val22 = ans;
				}
				if (max31 < row[1]) {
					max31 = row[1];
					val31 = ans;
				}
				if (max32 < row[2]) {
					max32 = row[2];
					val32 = ans;
				}
				if (max62 < vov[9]) {
					max62 = vov[9];
					val62 = ans;
				}
				if (max61 < vov[8]) {
					max61 = vov[8];
					val61 = ans;
				}
				if (max1 < row[0]) {
					max1 = row[0];
					ones = ans;
				}
				if (mini > row[0]) {
					mini = row[0];
					twos = ans;
				}
				max5 = max(max5, row[3]);
				if (ID1(vov)) {
					sourid = "1";
				}
				else if (ID2(vov)) {
					sourid = "2";
				}
				else if (ID3(vov)) {
					sourid = "3";
				}
				else if (ID4(vov)) {
					sourid = "4";
				}
				else if (ID6(vov)) {
					sourid = "6";
				}
				if (sourid == "0") {
					sm2 = (ld)sm2 / td2;
					sm1 = (ld)sm1 / td1;
					ld res = (ld)sm2 / sm1;
					if (res <= 0.85) {
						if (vov[0] >= 240) {
							asas.push_back(mp(mp(1, mp(-vov[0], 1)), ans));
						}
						else if (vov[2] >= 41000) {
							dog = 1;
							asas.push_back(mp(mp(2, mp(-vov[2], 1)), ans));
						}
						else if (vov[4] >= 0.298 || vov[5] >= 0.022) {
							asas.push_back(mp(mp(3, mp(-vov[4], 1)), ans));
						}
						else if (vov[8] >= 0.9) {
							yoy = 1;
							ld ak = -vov[8];
							asas.push_back(mp(mp(6, mp(ak, 1)), ans));
						}

					}
				}
				else if (1) {
					int sd = stoi(sourid);
					int sw = sd;
					if (sd == 6) { sd = 5; }
					ld ass = vov[(sd - 1) * 2];
					ass = ass*(ld)(-1.0);
					if (sw == 6) {
						if (yoy == 2) {
							ass = vov[((sd - 1) * 2) + 1];
							ass = ass*(ld)(-1.0);
							asas.push_back(mp(mp(sw, mp(ass, 2)), ans));
						}
						else {
							asas.push_back(mp(mp(sw, mp(ass, 1)), ans));
						}
					}
					else {
						if (sw == 2) {
							if (dog == 2) {
								ass = vov[((sd - 1) * 2) + 1];
								ass = ass*(ld)(-1.0);
								asas.push_back(mp(mp(sw, mp(ass, 2)), ans));
							}
							else {
								asas.push_back(mp(mp(sw, mp(ass, 1)), ans));
							}
						}
						else {
							asas.push_back(mp(mp(sw, mp(ass, 1)), ans));
						}
					}
				}
			}
		}
		if (asas.size()) {
			sort(asas.begin(), asas.end());
			fap.push_back(v.second); fap.push_back(to_string(asas[0].first.first));
			fag.push_back(asas[0].second);
		}
		//cout << max1 << " " << max5 << endl;
		if (fap.size() == 0 && max1 >= 236) {
			fap.push_back(v.second); fap.push_back(to_string(1)); fag.push_back(ones);
		}
		if (fap.size() == 0) {
			ld ans = twos;
			ld fal = tmt / 50;
			int sp1 = -1, sp2 = -1;
			sp1 = upper_bound(sps1.begin(), sps1.end(), ans - fal - 0.00001) - sps1.begin();
			sp2 = upper_bound(sps1.begin(), sps1.end(), ans + fal - 0.00001) - sps1.begin(); sp2--;
			ld lam = 0, bam = 0;
			for (int x = sp1; x <= sp2; x++) {
				lam = lam + super[x];
				bam = bam + 1;
			}
			if (bam == 0) { bam = 1; }
			lam = lam / bam;
			ld variance = 0;
			for (int x = sp1; x <= sp2; x++) {
				variance = variance + fabs((ld)super[x] - lam)*fabs((ld)super[x] - lam);
			}
			variance = variance / bam;
			if (variance > 205000) {
				fap.push_back(v.second); fap.push_back(to_string(9)); fag.push_back(twos);
			}
		}
		bool ghool = false;
		if (fap.size() && (fap[1] == "9" || fap[1] == "1")) {
			ld ans = fag[0];
			ld fal = tmt / 50;
			int sp1 = -1, sp2 = -1;
			sp1 = upper_bound(sps1.begin(), sps1.end(), ans - fal - 0.00001) - sps1.begin();
			sp2 = upper_bound(sps1.begin(), sps1.end(), ans + fal - 0.00001) - sps1.begin(); sp2--;
			vector<ld> vov;
			for (int tp = 0; tp <= 1; tp++) {
				ld sam = 0, ted = 0;
				for (int x = sp1; x <= sp2; x++) {
					if (0.0<vas[5][tp][super[x]]) {
						sam = sam + vas[5][tp][super[x]];
						ted = ted + 1.0;
					}
				}
				//					cout << id << " " << (ld)sam / ted << endl;
				if (sam == 0.0 || ted == 0.0) { ted = 1; }
				vov.push_back((ld)sam / ted);
			}
			if (vov[0] >= 0.89) {
				ghool = true;
			}
			else if (vov[1] >= 0.00000001235) {
				ghool = true;
			}
		}
		if (fap.size() == 0) {
			if (max62 >= 0.0000000102) {
				asas.push_back(mp(mp(2, mp(2, 2)), 2));
				fap.push_back(v.second); fap.push_back(to_string(6)); fag.push_back(val62);
			}
			else if (max61 >= 0.84) {
				asas.push_back(mp(mp(1, mp(1, 1)), 1));
				fap.push_back(v.second); fap.push_back(to_string(6)); fag.push_back(val61);
			}
		}
		if (fap.size() && fap[1] == "6") {

			ld ans = fag[0];
			ld fal = tmt / 50;
			int sp1 = -1, sp2 = -1;
			sp1 = upper_bound(sps1.begin(), sps1.end(), ans - fal - 0.00001) - sps1.begin();
			sp2 = upper_bound(sps1.begin(), sps1.end(), ans + fal - 0.00001) - sps1.begin(); sp2--;
			vector<ld> vov;
			for (int tp = 0; tp <= 1; tp++) {
				ld sam = 0, ted = 0;
				for (int x = sp1; x <= sp2; x++) {
					if (0.0<vas[1][tp][super[x]]) {
						sam = sam + vas[1][tp][super[x]];
						ted = ted + 1.0;
					}
				}
				//					cout << id << " " << (ld)sam / ted << endl;
				if (sam == 0.0 || ted == 0.0) { ted = 1; }
				vov.push_back((ld)sam / ted);
			}

			ld lam = 0, bam = 0;
			for (int x = sp1; x <= sp2; x++) {
				lam = lam + super[x];
				bam = bam + 1;
			}
			if (bam == 0) { bam = 1; }
			lam = lam / bam;
			ld variance = 0;
			for (int x = sp1; x <= sp2; x++) {
				variance = variance + fabs((ld)super[x] - lam)*fabs((ld)super[x] - lam);
			}
			variance = variance / bam;
			//	cout << v.second << " " << vov[0] << " " << vov[1] << " " << vov[0] * vov[1] <<" "<<variance<< endl;
			ld av1 = vov[0] * vov[1];
			if (av1 < 176.0 && variance < 169000) {
				fap[1] = "5";
			}
		}
		if (fap.size() == 0) {
			if (max31 > 0.34) {
				fap.push_back(v.second); fap.push_back(to_string(3)); fag.push_back(val31);
			}
			else if (max32 > 0.024) {
				fap.push_back(v.second); fap.push_back(to_string(3)); fag.push_back(val32);
			}
		}
		if (fap.size() == 0) {
			if (max21 >= 40500) {
				asas.push_back(mp(mp(1, mp(1, 1)), 1));
				fap.push_back(v.second); fap.push_back(to_string(2)); fag.push_back(val21);
			}
			else if (max22 >= 12.25) {
				asas.push_back(mp(mp(2, mp(2, 2)), 2));
				fap.push_back(v.second); fap.push_back(to_string(2)); fag.push_back(val22);
			}
		}
		if (fap.size() == 0) {
			fap.push_back(v.second); fap.push_back(to_string(0)); fag.push_back(0);
		}
		else if (stoi(fap[1]) == 6) {
			//cout << tap << endl;
			ld ans = fag[0]; fag.clear();
			pair<ld, ld> best;
			vector<pair<ld, ld>> iter;
			iter.push_back(mp(25.0, 4.0));
			iter.push_back(mp(4.0, 2.0));
			iter.push_back(mp(2.0, 1.0));
			iter.push_back(mp(0.5, 0.5));
			akh = secnd(akh);
			for (auto bus : iter) {
				best = mp(-1, -1);
				for (ld ij = max(ans - bus.first, (ld)30.0); ij<akh && ij < ans + bus.first; ij = ij + 0.1) {
					if (ij > akh) { break; }
					int sp1 = -1, sp2 = -1;
					sp1 = upper_bound(sps1.begin(), sps1.end(), ij - (bus.second / 2.0) - 0.00001) - sps1.begin();
					sp2 = upper_bound(sps1.begin(), sps1.end(), ij + (bus.second / 2.0) - 0.00001) - sps1.begin(); sp2--;
					vector<ld> vov;

					for (int tp = 0; tp <= 0; tp++) {
						ld sam = 0, ted = 0;
						for (int x = sp1; x <= sp2; x++) {
							if (0.0<vas[1][tp][super[x]]) {
								sam = sam + vas[1][tp][super[x]];
								ted = ted + 1.0;
							}
						}
						//					cout << id << " " << (ld)sam / ted << endl;
						if (sam == 0.0 || ted == 0.0) { ted = 1; }
						vov.push_back((ld)sam / ted);
					}
					for (int tp = asas[0].first.second.second - 1; tp <= asas[0].first.second.second - 1; tp++) {
						ld sam = 0, ted = 0;
						for (int x = sp1; x <= sp2; x++) {
							if (0.0<vas[5][tp][super[x]]) {
								sam = sam + vas[5][tp][super[x]];
								ted = ted + 1.0;
							}
						}
						//					cout << id << " " << (ld)sam / ted << endl;
						if (sam == 0.0 || ted == 0.0) { ted = 1; }
						vov.push_back((ld)sam / ted);
					}
					if (vov[1] != 0 && vov[0] != 0 && (ld)vov[0] * vov[1]> best.first) {
						best.first = vov[0] * vov[1];
						best.second = ij;
					}
				}
				if (best.second > -0.5) {
					ans = best.second;
				}
			}
			fag.push_back(ans);
		}
		else if (stoi(fap[1]) == 2) {
			ld ans = fag[0]; fag.clear();
			pair<ld, ld> best;
			int id = stoi(fap[1]);
			if (id > 5) { id = 5; }
			vector<pair<ld, ld>> iter;
			iter.push_back(mp(25.0, 4.0));
			iter.push_back(mp(4.0, 2.0));
			iter.push_back(mp(2.0, 1.0));
			iter.push_back(mp(0.5, 0.5));
			akh = secnd(akh);
			for (auto bus : iter) {
				best = mp(-1, -1);
				for (ld ij = max(ans - bus.first, (ld)30.0); ij<akh && ij < ans + bus.first; ij = ij + 0.1) {
					if (ij > akh) { break; }
					int sp1 = -1, sp2 = -1;
					sp1 = upper_bound(sps1.begin(), sps1.end(), ij - (bus.second / 2.0) - 0.00001) - sps1.begin();
					sp2 = upper_bound(sps1.begin(), sps1.end(), ij + (bus.second / 2.0) - 0.00001) - sps1.begin(); sp2--;
					vector<ld> vov;
					for (int tp = asas[0].first.second.second - 1; tp <= asas[0].first.second.second - 1; tp++) {
						ld sam = 0, ted = 0;
						for (int x = sp1; x <= sp2; x++) {
							if (0.0<vas[id][tp][super[x]]) {
								sam = sam + vas[id][tp][super[x]];
								ted = ted + 1.0;
							}
						}
						//					cout << id << " " << (ld)sam / ted << endl;
						if (sam == 0.0 || ted == 0.0) { ted = 1; }
						vov.push_back((ld)sam / ted);
					}
					if (vov[0] != 0 && (ld)vov[0]> best.first) {
						best.first = vov[0];
						best.second = ij;
					}
				}
				if (best.second > -0.5) {
					ans = best.second;
				}
			}
			fag.push_back(ans);
		}
		else if (stoi(fap[1]) == 3 || stoi(fap[1]) == 4) {
			ld ans = fag[0]; fag.clear();
			pair<ld, ld> best;
			int id = stoi(fap[1]);
			if (id > 5) { id = 5; }
			vector<pair<ld, ld>> iter;
			iter.push_back(mp(25.0, 4.0));
			iter.push_back(mp(4.0, 2.0));
			iter.push_back(mp(2.0, 1.0));
			iter.push_back(mp(0.5, 0.5));
			akh = secnd(akh);
			for (auto bus : iter) {
				best = mp(-1, -1);
				for (ld ij = max(ans - bus.first, (ld)30.0); ij<akh && ij < ans + bus.first; ij = ij + 0.1) {
					if (ij > akh) { break; }
					int sp1 = -1, sp2 = -1;
					sp1 = upper_bound(sps1.begin(), sps1.end(), ij - (bus.second / 2.0) - 0.00001) - sps1.begin();
					sp2 = upper_bound(sps1.begin(), sps1.end(), ij + (bus.second / 2.0) - 0.00001) - sps1.begin(); sp2--;
					vector<ld> vov;
					for (int tp = 0; tp <= 1; tp++) {
						ld sam = 0, ted = 0;
						for (int x = sp1; x <= sp2; x++) {
							if (0.0<vas[id][tp][super[x]]) {
								sam = sam + vas[id][tp][super[x]];
								ted = ted + 1.0;
							}
						}
						//					cout << id << " " << (ld)sam / ted << endl;
						if (sam == 0.0 || ted == 0.0) { ted = 1; }
						vov.push_back((ld)sam / ted);
					}
					if (vov[0] != 0 && (ld)vov[0] * vov[1] > best.first) {
						best.first = (ld)vov[0] * vov[1];
						best.second = ij;
					}
				}
				if (best.second > -0.5) {
					ans = best.second;
				}
			}
			fag.push_back(ans);
		}
		else if (stoi(fap[1]) <= 4) {
			ld ans = fag[0]; fag.clear();
			pair<ld, ld> best;
			int id = stoi(fap[1]);
			if (id > 5) { id = 5; }
			vector<pair<ld, ld>> iter;
			iter.push_back(mp(25.0, 4.0));
			iter.push_back(mp(4.0, 2.0));
			iter.push_back(mp(2.0, 1.0));
			iter.push_back(mp(0.5, 0.5));
			akh = secnd(akh);
			for (auto bus : iter) {
				best = mp(-1, -1);
				for (ld ij = max(ans - bus.first, (ld)30.0); ij<akh && ij < ans + bus.first; ij = ij + 0.1) {
					if (ij > akh) { break; }
					int sp1 = -1, sp2 = -1;
					sp1 = upper_bound(sps1.begin(), sps1.end(), ij - (bus.second / 2.0) - 0.00001) - sps1.begin();
					sp2 = upper_bound(sps1.begin(), sps1.end(), ij + (bus.second / 2.0) - 0.00001) - sps1.begin(); sp2--;
					vector<ld> vov;
					for (int tp = 0; tp <= 0; tp++) {
						ld sam = 0, ted = 0;
						for (int x = sp1; x <= sp2; x++) {
							if (0.0<vas[id][tp][super[x]]) {
								sam = sam + vas[id][tp][super[x]];
								ted = ted + 1.0;
							}
						}
						//					cout << id << " " << (ld)sam / ted << endl;
						if (sam == 0.0 || ted == 0.0) { ted = 1; }
						vov.push_back((ld)sam / ted);
					}
					if (vov[0] != 0 && vov[0] > best.first) {
						best.first = vov[0];
						best.second = ij;
					}
				}
				if (best.second > -0.5) {
					ans = best.second;
				}
			}
			fag.push_back(ans);
		}
		else if (stoi(fap[1]) == 8 || stoi(fap[1]) == 9) {
			if (stoi(fap[1]) == 8 || stoi(fap[1]) == 9) {
				fap[1] = "1";
			}
			ld ans = fag[0]; fag.clear();
			pair<ld, ld> best;
			int id = 1;
			if (id > 5) { id = 5; }
			vector<pair<ld, ld>> iter;
			iter.push_back(mp(25.0, 4.0));
			iter.push_back(mp(4.0, 2.0));
			akh = secnd(akh);
			for (auto bus : iter) {
				best = mp(-1, -1);
				for (ld ij = max(ans - bus.first, (ld)30.0); ij<akh && ij < ans + bus.first; ij = ij + 0.05) {
					if (ij > akh) { break; }
					int sp1 = -1, sp2 = -1;
					sp1 = upper_bound(sps1.begin(), sps1.end(), ij - (bus.second / 2.0) - 0.00001) - sps1.begin();
					sp2 = upper_bound(sps1.begin(), sps1.end(), ij + (bus.second / 2.0) - 0.00001) - sps1.begin(); sp2--;
					ld lam = 0, bam = 0;
					for (int x = sp1; x <= sp2; x++) {
						lam = lam + super[x];
						bam = bam + 1;
					}
					if (bam == 0) { bam = 1; }
					lam = lam / bam;
					ld variance = 0;
					for (int x = sp1; x <= sp2; x++) {
						variance = variance + fabs((ld)super[x] - lam)*fabs((ld)super[x] - lam);
					}
					variance = variance / bam;
					if (variance > best.first) {
						best.first = variance;
						best.second = ij;
					}
				}
				if (best.second > -0.5) {
					ans = best.second;
				}
			}
			fag.push_back(ans);
		}
		else if (stoi(fap[1]) == 5) {
			ld ans = fag[0]; fag.clear();
			pair<ld, ld> best;
			int id = stoi(fap[1]);
			if (id > 5) { id = 5; }
			vector<pair<ld, ld>> iter;
			iter.push_back(mp(25.0, 4.0));
			iter.push_back(mp(4.0, 2.0));
			iter.push_back(mp(2.0, 1.0));
			iter.push_back(mp(0.5, 0.5));
			akh = secnd(akh);
			for (auto bus : iter) {
				best = mp(-1, -1);
				for (ld ij = max(ans - bus.first, (ld)30.0); ij<akh && ij < ans + bus.first; ij = ij + 0.1) {
					if (ij > akh) { break; }
					int sp1 = -1, sp2 = -1;
					sp1 = upper_bound(sps1.begin(), sps1.end(), ij - (bus.second / 2.0) - 0.00001) - sps1.begin();
					sp2 = upper_bound(sps1.begin(), sps1.end(), ij + (bus.second / 2.0) - 0.00001) - sps1.begin(); sp2--;
					vector<ld> vov;
					for (int tp = asas[0].first.second.second - 1; tp <= asas[0].first.second.second - 1; tp++) {
						ld sam = 0, ted = 0;
						for (int x = sp1; x <= sp2; x++) {
							if (0.0<vas[id][tp][super[x]]) {
								sam = sam + vas[id][tp][super[x]];
								ted = ted + 1.0;
							}
						}
						//					cout << id << " " << (ld)sam / ted << endl;
						if (sam == 0.0 || ted == 0.0) { ted = 1; }
						vov.push_back((ld)sam / ted);
					}
					if (vov[0] != 0 && (ld)vov[0]> best.first) {
						best.first = vov[0];
						best.second = ij;
					}
				}
				if (best.second > -0.5) {
					ans = best.second;
				}
			}
			fag.push_back(ans);
		}
		if (ghool) { fap[1] = "6"; }
		if (fag[0] > 0.0) {
			kill << fap[0] << "," << fap[1] << "," << to_string(fag[0]) << endl;
		}
		else {
			kill << fap[0] << "," << fap[1] << "," << to_string(0) << endl;
		}
		exambl->close();
		exambl->clear();
	}
	return 0;
}