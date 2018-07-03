#include <bits/stdc++.h>

using namespace std;

const int base = 1000000000;
const int base_digits = 9;

struct bigint {
    vector<int> a;
    int sign;

    bigint() :
        sign(1) {
    }

    bigint(long long v) {
        *this = v;
    }

    bigint(const string &s) {
        read(s);
    }

    void operator=(const bigint &v) {
        sign = v.sign;
        a = v.a;
    }

    void operator=(long long v) {
        sign = 1;
        if (v < 0)
            sign = -1, v = -v;
        for (; v > 0; v = v / base)
            a.push_back(v % base);
    }

    bigint operator+(const bigint &v) const {
        if (sign == v.sign) {
            bigint res = v;

            for (int i = 0, carry = 0; i < (int) max(a.size(), v.a.size()) || carry; ++i) {
                if (i == (int) res.a.size())
                    res.a.push_back(0);
                res.a[i] += carry + (i < (int) a.size() ? a[i] : 0);
                carry = res.a[i] >= base;
                if (carry)
                    res.a[i] -= base;
            }
            return res;
        }
        return *this - (-v);
    }

    bigint operator-(const bigint &v) const {
        if (sign == v.sign) {
            if (abs() >= v.abs()) {
                bigint res = *this;
                for (int i = 0, carry = 0; i < (int) v.a.size() || carry; ++i) {
                    res.a[i] -= carry + (i < (int) v.a.size() ? v.a[i] : 0);
                    carry = res.a[i] < 0;
                    if (carry)
                        res.a[i] += base;
                }
                res.trim();
                return res;
            }
            return -(v - *this);
        }
        return *this + (-v);
    }

    void operator*=(int v) {
        if (v < 0)
            sign = -sign, v = -v;
        for (int i = 0, carry = 0; i < (int) a.size() || carry; ++i) {
            if (i == (int) a.size())
                a.push_back(0);
            long long cur = a[i] * (long long) v + carry;
            carry = (int) (cur / base);
            a[i] = (int) (cur % base);
            //asm("divl %%ecx" : "=a"(carry), "=d"(a[i]) : "A"(cur), "c"(base));
        }
        trim();
    }

    bigint operator*(int v) const {
        bigint res = *this;
        res *= v;
        return res;
    }

    bool operator>(const bigint &v) const {
        return v < *this;
    }
    bool operator<=(const bigint &v) const {
        return !(v < *this);
    }
    bool operator>=(const bigint &v) const {
        return !(*this < v);
    }
    bool operator==(const bigint &v) const {
        return !(*this < v) && !(v < *this);
    }
    bool operator!=(const bigint &v) const {
        return *this < v || v < *this;
    }

    friend pair<bigint, bigint> divmod(const bigint &a1, const bigint &b1) {
        int norm = base / (b1.a.back() + 1);
        bigint a = a1.abs() * norm;
        bigint b = b1.abs() * norm;
        bigint q, r;
        q.a.resize(a.a.size());

        for (int i = a.a.size() - 1; i >= 0; i--) {
            r *= base;
            r += a.a[i];
            int s1 = r.a.size() <= b.a.size() ? 0 : r.a[b.a.size()];
            int s2 = r.a.size() <= b.a.size() - 1 ? 0 : r.a[b.a.size() - 1];
            int d = ((long long) base * s1 + s2) / b.a.back();
            r -= b * d;
            while (r < 0)
                r += b, --d;
            q.a[i] = d;
        }

        q.sign = a1.sign * b1.sign;
        r.sign = a1.sign;
        q.trim();
        r.trim();
        return make_pair(q, r / norm);
    }

    bigint operator/(const bigint &v) const {
        return divmod(*this, v).first;
    }

    bigint operator%(const bigint &v) const {
        bigint m = divmod(*this, v).second;
        return (m >= 0) ? (m) : (m + v);
        // return divmod(*this, v).second;
    }

    void operator/=(int v) {
        if (v < 0)
            sign = -sign, v = -v;
        for (int i = (int) a.size() - 1, rem = 0; i >= 0; --i) {
            long long cur = a[i] + rem * (long long) base;
            a[i] = (int) (cur / v);
            rem = (int) (cur % v);
        }
        trim();
    }

    bigint operator/(int v) const {
        bigint res = *this;
        res /= v;
        return res;
    }

    int operator%(int v) const {
        if (v < 0)
            v = -v;
        int m = 0;
        for (int i = a.size() - 1; i >= 0; --i)
            m = (a[i] + m * (long long) base) % v;
        return m * sign;
    }

    void operator+=(const bigint &v) {
        *this = *this + v;
    }
    void operator-=(const bigint &v) {
        *this = *this - v;
    }
    void operator*=(const bigint &v) {
        *this = *this * v;
    }
    void operator/=(const bigint &v) {
        *this = *this / v;
    }

    bool operator<(const bigint &v) const {
        if (sign != v.sign)
            return sign < v.sign;
        if (a.size() != v.a.size())
            return a.size() * sign < v.a.size() * v.sign;
        for (int i = a.size() - 1; i >= 0; i--)
            if (a[i] != v.a[i])
                return a[i] * sign < v.a[i] * sign;
        return false;
    }


    void trim() {
        while (!a.empty() && !a.back())
            a.pop_back();
        if (a.empty())
            sign = 1;
    }

    bool isZero() const {
        return a.empty() || (a.size() == 1 && !a[0]);
    }

    bigint operator-() const {
        bigint res = *this;
        res.sign = -sign;
        return res;
    }

    bigint abs() const {
        bigint res = *this;
        res.sign *= res.sign;
        return res;
    }

    long long longValue() const {
        long long res = 0;
        for (int i = a.size() - 1; i >= 0; i--)
            res = res * base + a[i];
        return res * sign;
    }

    friend bigint gcd(const bigint &a, const bigint &b) {
        return b.isZero() ? a : gcd(b, a % b);
    }
    friend bigint lcm(const bigint &a, const bigint &b) {
        return a / gcd(a, b) * b;
    }

    void read(const string &s) {
        sign = 1;
        a.clear();
        int pos = 0;
        while (pos < (int) s.size() && (s[pos] == '-' || s[pos] == '+')) {
            if (s[pos] == '-')
                sign = -sign;
            ++pos;
        }
        for (int i = s.size() - 1; i >= pos; i -= base_digits) {
            int x = 0;
            for (int j = max(pos, i - base_digits + 1); j <= i; j++)
                x = x * 10 + s[j] - '0';
            a.push_back(x);
        }
        trim();
    }

    friend istream& operator>>(istream &stream, bigint &v) {
        string s;
        stream >> s;
        v.read(s);
        return stream;
    }

    friend ostream& operator<<(ostream &stream, const bigint &v) {
        if (v.sign == -1)
            stream << '-';
        stream << (v.a.empty() ? 0 : v.a.back());
        for (int i = (int) v.a.size() - 2; i >= 0; --i)
            stream << setw(base_digits) << setfill('0') << v.a[i];
        return stream;
    }

    static vector<int> convert_base(const vector<int> &a, int old_digits, int new_digits) {
        vector<long long> p(max(old_digits, new_digits) + 1);
        p[0] = 1;
        for (int i = 1; i < (int) p.size(); i++)
            p[i] = p[i - 1] * 10;
        vector<int> res;
        long long cur = 0;
        int cur_digits = 0;
        for (int i = 0; i < (int) a.size(); i++) {
            cur += a[i] * p[cur_digits];
            cur_digits += old_digits;
            while (cur_digits >= new_digits) {
                res.push_back(int(cur % p[new_digits]));
                cur /= p[new_digits];
                cur_digits -= new_digits;
            }
        }
        res.push_back((int) cur);
        while (!res.empty() && !res.back())
            res.pop_back();
        return res;
    }

    typedef vector<long long> vll;

    static vll karatsubaMultiply(const vll &a, const vll &b) {
        int n = a.size();
        vll res(n + n);
        if (n <= 32) {
            for (int i = 0; i < n; i++)
                for (int j = 0; j < n; j++)
                    res[i + j] += a[i] * b[j];
            return res;
        }

        int k = n >> 1;
        vll a1(a.begin(), a.begin() + k);
        vll a2(a.begin() + k, a.end());
        vll b1(b.begin(), b.begin() + k);
        vll b2(b.begin() + k, b.end());

        vll a1b1 = karatsubaMultiply(a1, b1);
        vll a2b2 = karatsubaMultiply(a2, b2);

        for (int i = 0; i < k; i++)
            a2[i] += a1[i];
        for (int i = 0; i < k; i++)
            b2[i] += b1[i];

        vll r = karatsubaMultiply(a2, b2);
        for (int i = 0; i < (int) a1b1.size(); i++)
            r[i] -= a1b1[i];
        for (int i = 0; i < (int) a2b2.size(); i++)
            r[i] -= a2b2[i];

        for (int i = 0; i < (int) r.size(); i++)
            res[i + k] += r[i];
        for (int i = 0; i < (int) a1b1.size(); i++)
            res[i] += a1b1[i];
        for (int i = 0; i < (int) a2b2.size(); i++)
            res[i + n] += a2b2[i];
        return res;
    }

    bigint operator*(const bigint &v) const {
        vector<int> a6 = convert_base(this->a, base_digits, 6);
        vector<int> b6 = convert_base(v.a, base_digits, 6);
        vll a(a6.begin(), a6.end());
        vll b(b6.begin(), b6.end());
        while (a.size() < b.size())
            a.push_back(0);
        while (b.size() < a.size())
            b.push_back(0);
        while (a.size() & (a.size() - 1))
            a.push_back(0), b.push_back(0);
        vll c = karatsubaMultiply(a, b);
        bigint res;
        res.sign = sign * v.sign;
        for (int i = 0, carry = 0; i < (int) c.size(); i++) {
            long long cur = c[i] + carry;
            res.a.push_back((int) (cur % 1000000));
            carry = (int) (cur / 1000000);
        }
        res.a = convert_base(res.a, 6, base_digits);
        res.trim();
        return res;
    }
};

struct Dxy{
    bigint d, x, y;
    Dxy(bigint d, bigint x, bigint y){
        this->d = d, this->x = x, this->y = y;
    }
};

bigint euclid(bigint a, bigint b){
    if (b == 0){
        return a;
    }
    else{
        return euclid(b, a % b);
    }
}

Dxy ext_euclid(bigint a, bigint b){
    if (b == 0)
        return Dxy(a, 1, 0);
    Dxy dxy1 = ext_euclid(b, a % b);
    Dxy dxy = Dxy(dxy1.d, dxy1.y, dxy1.x - (a / b) * dxy1.y);
    return dxy;
}

bigint exp(bigint base, bigint e, bigint m){
    bigint pot = base;
    bigint res = 1;
    for (; e > 0; e = e / 2) {
        if(e % 2 == 1){
            res = (res * pot) % m;
        }
        pot = (pot * pot) % m;
        // cout << res << endl;
    }
    return res;
}

string numeroAleatorio(int n){
    string aleatorio;
    for(int i = 0; i < n; i++){
        int numero = rand() % 10;
        aleatorio.append(to_string(numero));
    }
    return aleatorio;
}

bool testaPrimo(int base, bigint primo){
    if(exp(base, primo - 1, primo) == 1) return true;
    return false;
}

bool millerRabin(bigint primo, int n){
    for (int i = 0; i < n; i++) {
        int base = rand();
        if(!testaPrimo(base, primo)) return false;
    }
    return true;
}

pair<bigint, bigint> gerarPrimos(int casas, int testes){
    bool primeiro = true;
    pair<bigint, bigint> primos;
    for (int i = 0; i < 2; i++){
        // 32416190071
        bigint possivel_primo(numeroAleatorio(casas));
        while (!millerRabin(possivel_primo, testes)) {
            possivel_primo = bigint(numeroAleatorio(casas));
        }
        primeiro ? primos.first = possivel_primo : primos.second = possivel_primo;
        primeiro = false;
    }
    return primos;
}

bigint gerarPrimoPequeno(){
    bigint pequeno(numeroAleatorio(4));
    while (!millerRabin(pequeno, 200)) {
        pequeno = bigint(numeroAleatorio(4));
    }
    return pequeno;
}

bigint mod_lin_solver(bigint a, bigint n){
    Dxy result_ext_euclid = ext_euclid(a, n);
    bigint um(1);
    return exp(result_ext_euclid.x*(um/result_ext_euclid.d), 1, n);
}

bigint concatenarMensagem(string mensagem, bigint n){
    string bit_msg;
    for (int i = mensagem.size(); i >= 0; --i) {
        bitset<8> char_atual (mensagem[i]);
        bit_msg += char_atual.to_string();
    }
    bigint m(0);
    bigint potencia(1);
    for (size_t i = 0; i < bit_msg.size(); i++) {
        if(bit_msg[bit_msg.size() - i - 1] == '1'){
            m += potencia;
        }
        potencia *= 2;
    }
    return m;
}

string decifraMensagem(bigint msg){
    bigint dois(2);
    string mensagem;
    while(msg > 0){
        int multiplicador = 1;
        char char_atual = 0;
        for (size_t i = 0; i < 8; i++) {
            if(msg % dois == 1){
                char_atual += multiplicador;
            }
            multiplicador *= 2;
            msg /= 2;
        }
        mensagem.push_back(char_atual);
    }
    return mensagem;

}


void clearScreen(){
    system("clear");
}

void wait(){
    getchar();getchar();
}

void menuEuclid(){
    clearScreen();
    string aa, bb;

    cout << "Digite a, b: ";

    cin >> aa >> bb;
    bigint a(aa);
    bigint b(bb);

    bigint r = euclid(a, b);

    cout << r << endl;

    wait();
}

void menuExtEuclid(){
    clearScreen();
    string aa, bb;

    cout << "Digite a, b: ";

    cin >> aa >> bb;

    bigint a(aa);
    bigint b(bb);

    Dxy dxy = ext_euclid(a, b);

    cout << dxy.x << endl;

    wait();
}

void menuModulo(){
    clearScreen();
    string aa, nn;

    cout << "Digite a, n: ";

    cin >> aa >> nn;

    bigint a(aa);
    bigint n(nn);

    bigint r = a % n;

    cout << r << endl;

    wait();
}

void menuExp(){
    clearScreen();
    string aa, ee, nn;

    cout << "Digite a, e, n: ";

    cin >> aa >> ee >> nn;

    bigint a(aa);
    bigint e(ee);
    bigint n(nn);

    bigint r = exp(a, e, n);

    cout << r << endl;

    wait();
}

int main(){
    srand(time(NULL));
    int casas, testes;
    cout << "Digite a quantidade de casas decimais para as chaves e a quantidade de testes de primalidade" << endl;
    cin >> casas >> testes;
    cout << "Gerando chaves, aguarde um momento..." << endl;
    pair<bigint, bigint> primos;
    primos = gerarPrimos(casas, testes);
    bigint n(primos.first * primos.second);
    bigint n_2 = (primos.first - 1) * (primos.second - 1);
    bigint e(gerarPrimoPequeno());
    bigint inverso_modular = mod_lin_solver(e, n_2);
    pair<bigint, bigint> chave_publica(e, n);
    pair<bigint, bigint> chave_privada(inverso_modular, n);
    cout << "CHAVE PÚBLICA: " << endl << "\tE:" << e << endl << "\tN:" << n << endl;
    cout << "CHAVE PRIVADA: " << endl << "\tD:" << inverso_modular << endl << "\tN:" << n << endl;
    int op = -1;
    while(op != 0){
        cout << "1 - Escrever mensagem" << endl << "2 - Decodificar mensagem" << endl << "0 - sair" << endl;
        cin >> op;
        switch(op){
            case 1:{
                cout << "1 - Utilizar chave pública desta instância" << endl << "2 - Utilizar chave pública de outra instância" << endl;
                string mensagem;
                int op2;
                cin >> op2;
                if(op2 == 1) {
                    cout << "digite a mensagem: ";
                    cin >> mensagem;
                    bigint msg_concatenada(concatenarMensagem(mensagem, n));
                    cout << "Sua mensagem concatenada é: " << msg_concatenada << endl;
                    bigint envio = exp(msg_concatenada, e, n);
                    cout << "Sua mensagem cifrada é: " << envio << endl;
                }else{
                    string e_chave_publica, n_chave_publica;
                    cout << "Insira o E da chave pública: ";
                    cin >> e_chave_publica;
                    cout << "Insira o N da chave pública: ";
                    cin >> n_chave_publica;
                    cout << "digite a mensagem: ";
                    cin >> mensagem;
                    bigint e2(e_chave_publica);
                    bigint n2(n_chave_publica);
                    bigint msg_concatenada(concatenarMensagem(mensagem, n2));
                    cout << "Sua mensagem concatenada é: " << msg_concatenada << endl;
                    bigint envio = exp(msg_concatenada, e2, n2);
                    cout << "Sua mensagem cifrada é: " << envio << endl;
                }
                break;
            }
            case 2:{
                cout << "Digite a mensagem cifrada: ";
                string mensagem_cifrada;
                cin >> mensagem_cifrada;
                bigint recebimento = exp(mensagem_cifrada, inverso_modular, n);
                cout << "A mensagem enviada foi: " << decifraMensagem(recebimento) << endl;
                break;
            }

        }
    }



    // int op;
    //
    // do{
    //     clearScreen();
    //     cout << "Criptografia RSA" << "\n\n";
    //     cout << "Escolha uma opcao:" << "\n";
    //     cout << "\t1 - Euclid" << endl;
    //     cout << "\t2 - Ext-Euclid" << endl;
    //     cout << "\t3 - Modulo" << endl;
    //     cout << "\t4 - Exponenciacao" << endl;
    //     cout << "\n\t0 - Sair" << endl;
    //     cout << "\n$: ";
    //
    //     cin >> op;
    //
    //     switch(op){
    //         case 1:
    //             menuEuclid();
    //             break;
    //         case 2:
    //             menuExtEuclid();
    //             break;
    //         case 3:
    //             menuModulo();
    //             break;
    //         case 4:
    //             menuExp();
    //             break;
    //     }
    // }while(op != 0);

}
