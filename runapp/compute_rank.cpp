#include <Rcpp.h>
using namespace Rcpp;

// ----------------------------
// SAFE comparaison 4 équipes
// ----------------------------
bool better(
        int i, int j,
        int pts[4],
               int h2h_pts[4][4],
                             int h2h_gd[4],
                                       int h2h_gf[4],
                                                 int gd[4],
                                                       int gf[4]
){
    if(pts[i] != pts[j])
        return pts[i] > pts[j];
    
    if(h2h_pts[i][j] != h2h_pts[j][i])
        return h2h_pts[i][j] > h2h_pts[j][i];
    
    if(h2h_gd[i] != h2h_gd[j])
        return h2h_gd[i] > h2h_gd[j];
    
    if(h2h_gf[i] != h2h_gf[j])
        return h2h_gf[i] > h2h_gf[j];
    
    if(gd[i] != gd[j])
        return gd[i] > gd[j];
    
    return gf[i] > gf[j];
}

// ----------------------------
// SAFE SORT 4
// ----------------------------
void sort4(
        int order[4],
                 int pts[4],
                        int h2h_pts[4][4],
                                      int h2h_gd[4],
                                                int h2h_gf[4],
                                                          int gd[4],
                                                                int gf[4]
){
    for(int i=0;i<4;i++){
        for(int j=i+1;j<4;j++){
            
            if(!better(order[i], order[j],
                       pts,
                       h2h_pts,
                       h2h_gd,
                       h2h_gf,
                       gd,
                       gf)){
                std::swap(order[i], order[j]);
            }
        }
    }
}

// [[Rcpp::export]]
List compute_rank_cpp(
        IntegerVector home_id,
        IntegerVector away_id,
        IntegerVector group_start,
        IntegerVector group_size,
        IntegerVector score_home,
        IntegerVector score_away,
        int n_groups,
        CharacterVector team_names,
        CharacterVector group_names
){
    
    int n = home_id.size();
    int nT = team_names.size();
    
    // ----------------------------
    // GLOBAL STATS
    // ----------------------------
    std::vector<int> pts(nT, 0);
    std::vector<int> gf(nT, 0);
    std::vector<int> ga(nT, 0);
    
    for(int i=0;i<n;i++){
        
        int h = home_id[i] - 1;
        int a = away_id[i] - 1;
        
        if(h < 0 || h >= nT || a < 0 || a >= nT)
            stop("Invalid team index");
        
        int sh = score_home[i];
        int sa = score_away[i];
        
        gf[h] += sh;
        ga[h] += sa;
        
        gf[a] += sa;
        ga[a] += sh;
        
        if(sh > sa) pts[h] += 3;
        else if(sa > sh) pts[a] += 3;
        else {
            pts[h] += 1;
            pts[a] += 1;
        }
    }
    
    List groups(n_groups);
    
    CharacterVector third_team(n_groups);
    CharacterVector third_group(n_groups);
    IntegerVector third_pts(n_groups);
    IntegerVector third_gd(n_groups);
    
    // ----------------------------
    // GROUP LOOP
    // ----------------------------
    for(int g=0; g<n_groups; g++){
        
        int start = group_start[g] - 1;   // SAFE: R -> C++
        int size  = group_size[g];
        
        int team_ids[4];
        int cnt = 0;
        
        // init safe
        for(int i=0;i<4;i++) team_ids[i] = -1;
        
        // ----------------------------
        // collect teams (SAFE UNIQUE)
        // ----------------------------
        for(int i=start;i<start+size;i++){
            
            int h = home_id[i] - 1;
            int a = away_id[i] - 1;
            
            bool fh=false, fa=false;
            
            for(int k=0;k<cnt;k++){
                if(team_ids[k]==h) fh=true;
                if(team_ids[k]==a) fa=true;
            }
            
            if(!fh && cnt < 4) team_ids[cnt++] = h;
            if(!fa && cnt < 4) team_ids[cnt++] = a;
        }
        
        // safety check
        if(cnt != 4)
            stop("Group does not contain exactly 4 teams");
        
        int pts_g[4]={0,0,0,0};
        int gd_g[4]={0,0,0,0};
        int gf_g[4]={0,0,0,0};
        
        int h2h_pts[4][4]={};
        int h2h_gd[4]={0,0,0,0};
        int h2h_gf[4]={0,0,0,0};
        
        // ----------------------------
        // MATCHES
        // ----------------------------
        for(int i=start;i<start+size;i++){
            
            int h = home_id[i] - 1;
            int a = away_id[i] - 1;
            
            int sh = score_home[i];
            int sa = score_away[i];
            
            int ih=-1, ia=-1;
            
            for(int k=0;k<4;k++){
                if(team_ids[k]==h) ih=k;
                if(team_ids[k]==a) ia=k;
            }
            
            if(ih == -1 || ia == -1)
                stop("Team mapping error inside group");
            
            gf_g[ih] += sh;
            gf_g[ia] += sa;
            
            gd_g[ih] += (sh - sa);
            gd_g[ia] += (sa - sh);
            
            if(sh > sa){
                pts_g[ih] += 3;
                h2h_pts[ih][ia] += 3;
            }
            else if(sh < sa){
                pts_g[ia] += 3;
                h2h_pts[ia][ih] += 3;
            }
            else{
                pts_g[ih] += 1;
                pts_g[ia] += 1;
                h2h_pts[ih][ia] += 1;
                h2h_pts[ia][ih] += 1;
            }
            
            h2h_gd[ih] += (sh - sa);
            h2h_gd[ia] += (sa - sh);
            
            h2h_gf[ih] += sh;
            h2h_gf[ia] += sa;
        }
        
        int order[4] = {0,1,2,3};
        
        sort4(order,
              pts_g,
              h2h_pts,
              h2h_gd,
              h2h_gf,
              gd_g,
              gf_g);
        
        CharacterVector ranked(4);
        
        for(int i=0;i<4;i++){
            ranked[i] = team_names[ team_ids[ order[i] ] ];
        }
        
        groups[g] = ranked;
        
        third_team[g] = ranked[2];
        third_group[g] = group_names[g];
        third_pts[g] = pts_g[order[2]];
        third_gd[g] = gd_g[order[2]];
    }
    
    // ----------------------------
    // BEST 3RD
    // ----------------------------
    IntegerVector idx(n_groups);
    for(int i=0;i<n_groups;i++) idx[i]=i;
    
    std::sort(idx.begin(), idx.end(),
              [&](int a, int b){
                  
                  if(third_pts[a] != third_pts[b])
                      return third_pts[a] > third_pts[b];
                  
                  if(third_gd[a] != third_gd[b])
                      return third_gd[a] > third_gd[b];
                  
                  return third_team[a] < third_team[b];
              }
    );
    
    CharacterVector best_thirds(8);
    CharacterVector best_groups(8);
    
    for(int i=0;i<8;i++){
        best_thirds[i] = third_team[idx[i]];
        best_groups[i] = third_group[idx[i]];
    }
    
    return List::create(
        _["groups"] = groups,
        _["best_thirds"] = best_thirds,
        _["best_third_groups"] = best_groups
    );
}