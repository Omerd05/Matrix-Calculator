#include <iostream>
#include <vector>
#include <cmath>
#include <algorithm>
using namespace std;
typedef long double ld;

struct Arithmetic {
    void scalarMulti(vector<vector<ld>>& m1, int l1, ld scalar) {
        int N = m1[0].size();
        for (int i = 0; i < N; i++) {
            m1[l1][i] *= scalar;
        }
    }
    void switchRows(vector<vector<ld>>& m1, int l1, int l2) {
        int N = m1[0].size();
        for (int i = 0; i < N; i++) {
            swap(m1[l1][i], m1[l2][i]);
        }
    }
    void Addition (vector<vector<ld>>& m1, vector<ld>& row, int l1, ld scalar) { //changing the matrix
        for (int i = 0; i < row.size(); i++) {
            m1[l1][i] += scalar * row[i];
        }
    }
    vector<ld> rowAddition(vector<vector<ld>>& m1, vector<ld>& row, int l1, ld scalar) { // isn't changing the matrix
        vector<ld> ans;
        for (int i = 0; i < row.size(); i++) {
            ans.push_back(m1[l1][i] + scalar*row[i]);
        }
        return ans;
    }
    ld rowMulti(vector<vector<ld>>& m1, vector<ld>& col, int l1) {
        ld ans = 0;
        for (int i = 0; i < col.size(); i++) {
            ans += m1[l1][i] * col[i];
        }
        return ans;
    }
    vector<ld> getCol(vector<vector<ld>>& m1, int c1) {
        vector<ld> ans;
        for (int j = 0; j < m1.size(); j++) {
            ans.push_back(m1[j][c1]);
        }
        return ans;
    }
    vector<vector<ld>> matrixAddition(vector<vector<ld>>& m1, vector<vector<ld>>& m2, ld scalar) { //Updates m1
        int M = m1.size(), N = m1[0].size();
        auto ans = vector<vector<ld>>(M);
        for (int j = 0; j < M; j++) {
            ans[j] = rowAddition(m1, m2[j], j, scalar);
        }
        return ans;
    }
    vector<vector<ld>> matrixMulti(vector<vector<ld>>& m1, vector<vector<ld>>& m2) { //Updates m1
        int M = m1.size(), N = m1[0].size(), K = m2[0].size(); //MxN,NxK
        vector<vector<ld>> ans(M,vector<ld>(K));
        for (int i = 0; i < M; i++) {
            for (int j = 0; j < K; j++) {
                vector<ld> col = getCol(m2, j);
                ans[i][j] = rowMulti(m1, col, i);
            }
        }
        return ans;
    }
};

struct Ranking
{
    vector<vector<ld>> inv;
    int swapsPairty = 0;
    void rank(vector<vector<ld>>& matrix) {
        Arithmetic ari;
        int M = matrix.size(), N = matrix[0].size(), r = 0;
        if (N == M) {
            inv.resize(M, vector<ld>(M, 0));
            for (int i = 0; i < M; i++)inv[i][i] = 1;
        }
        for (int j = 0; j < N; j++) {
            int i = r;
            while (i < M && !matrix[i][j])i++;
            if (i == M)continue;
            ari.switchRows(matrix,i,r);
            swapsPairty = 1 - swapsPairty;
            if (N == M) {
                ari.switchRows(inv, i, r);
            }
            for (int i = r + 1; i < M; i++) {
                if (!matrix[i][j])continue;
                ld tmp = -matrix[i][j] / matrix[r][j];
                ari.Addition(matrix, matrix[r], i, tmp);
                if (N == M) {
                    ari.Addition(inv, inv[r], i, tmp);
                }
            }
            r++;
        }
    }
    void cannonRanking(vector<vector<ld>>& matrix) {
        Arithmetic ari;
        rank(matrix);
        int M = matrix.size(), N = matrix[0].size(), h = M - 1;
        for (int j = N - 1; j >= 0; j--) {
            int i = h;
            while (i >= 0 && !matrix[i][j])i--;
            if (i == -1)continue;
            ld aa = 1 / matrix[i][j];
            ari.scalarMulti(matrix,i,1 / matrix[i][j]);
            if(N==M)ari.scalarMulti(inv, i, aa);
            for (int k = i - 1; k >= 0; k--) {
                if (!matrix[k][j])continue;
                ld tmp = -matrix[k][j] / matrix[i][j];
                ari.Addition(matrix, matrix[i], k, tmp);
                if(N==M)ari.Addition(inv, inv[i], k, tmp);
            }
            h--;
        }
    }
};

struct MatrixOperations
{
    vector<vector<ld>> Transpose(vector<vector<ld>>& matrix) {
        int M = matrix.size(), N = matrix[0].size();
        vector<vector<ld>> t(N, vector<ld>(M));
        for (int i = 0; i < M; i++) {
            for (int j = 0; j < N; j++) {
                t[j][i] = matrix[i][j];
            }
        }
        return t;
    }

    vector<vector<ld>> Inverse(vector<vector<ld>>& matrix) {
        Ranking ranking;
        ranking.cannonRanking(matrix);
        return ranking.inv;
    }

    ld Determinant(vector<vector<ld>>& matrix) {
        int M = matrix.size(), N = matrix[0].size();
        if (N != M)return 0;
        Ranking ranking;
        ranking.rank(matrix);
        ld ans = pow(1, ranking.swapsPairty);
        for (int i = 0; i < M; i++) {
            ans *= matrix[i][i];
        }
        return ans;
    }
};

struct UI {
    void print(vector<vector<ld>>& matrix) {
        for (auto row : matrix) {
            cout << "|" << " ";
            for (ld v : row) {
                if (v == -0) {
                    v = 0;
                }
                cout << v << " | ";
            }
            cout << '\n';
        }
    }
    void Informaton(vector<vector<ld>>& matrix) {
        Ranking ranking;
        MatrixOperations mo;
        auto dupli = matrix;
        cout << "Your matrix is : \n"; print(matrix);
        auto tmp = mo.Transpose(matrix);
        cout << "Its transpose is : \n"; print(tmp);
        cout << "Its cannoical form is : \n"; ranking.cannonRanking(matrix); print(matrix);
        cout << "Its inverse is : \n"; print(ranking.inv);
        cout << "And finally, its determinant is : " << mo.Determinant(dupli);
    }
};

int main()
{
    vector<vector<ld>> m1 = { {1,2,3},{4,5,6},{7,8,8} };
    UI toPrint;
    toPrint.Informaton(m1);
}
