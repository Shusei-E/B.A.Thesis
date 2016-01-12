stancode = "
data {
    int<lower=0> n; #データの数
    int<lower=0> time;
    int<lower=1> L; #グループの数 (ここではDem OR Non-Dem)
    int<lower=1,upper=L> ll[n]; #データの1点がどのグループに属しているかのindex
    vector[n] p4_polity2;
    vector[n] polity_lag;
    vector[n] Giniall;
    vector[n] Aid_All;
    vector[n] GDP;
    vector[n] Corruption;
    vector[n] Population;
    vector[n] TotalRents;
    vector[n] WinningCoalition;
    vector[n] Selectorate;

}

parameters {
      real alpha[L]; #2つのalphaは一旦alpha[n]とはしないでおく
      real<lower=0> sigma1;
      real<lower=0> sigma2[L];
      real mu[L];
      real AidAll[L];
      real Corup;
      real WinCoa;
      real Selec;
}
transformed parameters {  #stage-2 #これもmodelに含めてみる
#この設定で大丈夫？ 


}

model {

        for (l in 1:L){ #stage2 categoryごとに効きが違うと考えられるもの
        mu[l] ~  normal(0,100);
        sigma2[l] ~ uniform(0,1000);
        alpha[l] ~ normal(mu[l],sigma2[l]);
        AidAll[l] ~ normal(mu[l],sigma2[l]);
        }
 

    for (i in 1:n){     #stage2
            Giniall[i] ~ normal(alpha[ll[i]] +  
            AidAll[ll[i]]*Aid_All[i] + Corup*Corruption[i] + 
            WinCoa*WinningCoalition[i] + Selec*Selectorate[i], sigma1);
            #国ごとの固定効果をなくしてみた alpha[i]をやめた --> 一時的に入れてみた
        }  

        #Prior;
        sigma1 ~ uniform(0,1000);
}
" #stancodeここまで
