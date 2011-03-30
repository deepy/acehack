/*	SCCS Id: @(#)botl.c	3.4	1996/07/15	*/
/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */
/* Modified 30 Mar 2011 by Alex Smith */
/* NetHack may be freely redistributed.  See license for details. */

#include "hack.h"

#ifdef OVL0
extern const char *hu_stat[];	/* defined in eat.c */

const char * const enc_stat[] = {
	"",
	"Burdened",
	"Stressed",
	"Strained",
	"Overtaxed",
	"Overloaded"
};

/* hunger texts used on bottom line (each 8 chars long) */
#define SATIATED	0
#define NOT_HUNGRY	1
#define HUNGRY		2
#define WEAK		3
#define FAINTING	4
#define FAINTED		5
#define STARVED		6

STATIC_DCL void NDECL(bot1);
STATIC_DCL void NDECL(bot2);
#endif /* OVL0 */

/* MAXCO must hold longest uncompressed status line, and must be larger
 * than COLNO
 */
#if COLNO <= 140
#define MAXCO 160
#else
#define MAXCO (COLNO+20)
#endif

#ifndef OVLB
STATIC_DCL int mrank_sz;
#else /* OVLB */
STATIC_OVL NEARDATA int mrank_sz = 0; /* loaded by max_rank_sz (from u_init) */
#endif /* OVLB */

STATIC_DCL const char *NDECL(rank);

#ifdef OVL1

/* convert experience level (1..30) to rank index (0..8) */
int
xlev_to_rank(xlev)
int xlev;
{
	return (xlev <= 2) ? 0 : (xlev <= 30) ? ((xlev + 2) / 4) : 8;
}

#if 0	/* not currently needed */
/* convert rank index (0..8) to experience level (1..30) */
int
rank_to_xlev(rank)
int rank;
{
	return (rank <= 0) ? 1 : (rank <= 8) ? ((rank * 4) - 2) : 30;
}
#endif

const char *
rank_of(lev, monnum, female)
	int lev;
	short monnum;
	boolean female;
{
	register struct Role *role;
	register int i;


	/* Find the role */
	for (role = (struct Role *) roles; role->name.m; role++)
	    if (monnum == role->malenum || monnum == role->femalenum)
	    	break;
	if (!role->name.m)
	    role = &urole;

	/* Find the rank */
	for (i = xlev_to_rank((int)lev); i >= 0; i--) {
	    if (female && role->rank[i].f) return (role->rank[i].f);
	    if (role->rank[i].m) return (role->rank[i].m);
	}

	/* Try the role name, instead */
	if (female && role->name.f) return (role->name.f);
	else if (role->name.m) return (role->name.m);
	return ("Player");
}


STATIC_OVL const char *
rank()
{
	return(rank_of(u.ulevel, Role_switch, flags.female));
}

int
title_to_mon(str, rank_indx, title_length)
const char *str;
int *rank_indx, *title_length;
{
	register int i, j;


	/* Loop through each of the roles */
	for (i = 0; roles[i].name.m; i++)
	    for (j = 0; j < 9; j++) {
	    	if (roles[i].rank[j].m && !strncmpi(str,
	    			roles[i].rank[j].m, strlen(roles[i].rank[j].m))) {
	    	    if (rank_indx) *rank_indx = j;
	    	    if (title_length) *title_length = strlen(roles[i].rank[j].m);
	    	    return roles[i].malenum;
	    	}
	    	if (roles[i].rank[j].f && !strncmpi(str,
	    			roles[i].rank[j].f, strlen(roles[i].rank[j].f))) {
	    	    if (rank_indx) *rank_indx = j;
	    	    if (title_length) *title_length = strlen(roles[i].rank[j].f);
	    	    return ((roles[i].femalenum != NON_PM) ?
	    	    		roles[i].femalenum : roles[i].malenum);
	    	}
	    }
	return NON_PM;
}

#endif /* OVL1 */
#ifdef OVLB

void
max_rank_sz()
{
	register int i, r, maxr = 0;
	for (i = 0; i < 9; i++) {
	    if (urole.rank[i].m && (r = strlen(urole.rank[i].m)) > maxr) maxr = r;
	    if (urole.rank[i].f && (r = strlen(urole.rank[i].f)) > maxr) maxr = r;
	}
	mrank_sz = maxr;
	return;
}

#endif /* OVLB */
#ifdef OVL0

#ifdef SCORE_ON_BOTL
long
botl_score()
{
#ifndef GOLDOBJ
    long umoney = u.ugold + hidden_gold();
#else
    long umoney = money_cnt(invent) + hidden_gold();
#endif
    calculate_score(-1, 0, umoney);
    return u.urexp;
}
#endif

/* Draws an HP bar, covering columns 1-20 of the specified row.
 * Very loosely based on the "hpmon" patch by Ralph Churchill. */
STATIC_OVL void
hp_pw_bar(current, outof, ishp, y)
  int current, outof, ishp, y;
{
  int hpcolor;
  char buf[16];
  char bt;
  int hpsplit;
  /* Rules: HP     Pw
     max    white  white
     >2/3   green  cyan
     >1/3   yellow blue
     >1/7   red    magenta
     <=1/7  br.red br.magenta */
  if (current < 0) current = 0;
  hpsplit = (current*14/outof) + 1;
  if (current == 0) hpsplit = 0;
  if (current == outof) hpcolor = NO_COLOR;
  else if (current*3 > outof*2) hpcolor = ishp ? CLR_GREEN : CLR_CYAN;
  else if (current*3 > outof)   hpcolor = ishp ? CLR_BROWN : CLR_BLUE;
  else hpcolor = ishp ? CLR_RED : CLR_MAGENTA;
  if (current*7 <= outof) hpcolor |= BRIGHT;
  curs(WIN_STATUS, 1, y);
  if (ishp) putstr_colored(WIN_STATUS, 0, hpcolor, "HP:[");
  else putstr_colored(WIN_STATUS, 0, hpcolor, "Pw:[");
  if (current <= 999999 && outof <= 999999)
    Sprintf(buf, "%6d / %-6d", current, outof);
  else if (current <= 999999 && outof <= 99999999)
    Sprintf(buf, "%6d / %-5dK", current, outof/1000);
  else if (current <= 999999)
    Sprintf(buf, "%6d / %-4dM", current, outof/1000000);
  else if (current <= 99999999 && outof <= 99999999)
    Sprintf(buf, "%5dK / %-5dK", current/1000, outof/1000);
  else if (current <= 99999999)
    Sprintf(buf, "%5dK / %-4dM", current/1000, outof/1000000);
  else
    Sprintf(buf, "%4dM / %-4dM", current/1000000, outof/1000000);
  curs(WIN_STATUS, 5, y);
  bt = buf[hpsplit]; buf[hpsplit] = 0;
  putstr_colored(WIN_STATUS, ATR_INVERSE, hpcolor, buf);
  buf[hpsplit] = bt;
  curs(WIN_STATUS, 5+hpsplit, y);
  putstr_colored(WIN_STATUS, 0, hpcolor, buf+hpsplit);
  curs(WIN_STATUS, 20, y);
  putstr_colored(WIN_STATUS, 0, hpcolor, "]");
  curs(WIN_STATUS, 22, y);
}

STATIC_OVL void
bot1()
{
  char buf[MAXCO];
  hp_pw_bar(Upolyd ? u.mh : u.uhp, Upolyd ? u.mhmax : u.uhpmax, 1, 0);
  char* nb;
  Sprintf(buf, "AC:%d Xp:%d ", u.uac, u.ulevel);
  if (Upolyd)
    Sprintf(buf, "AC:%d HD:%d ", u.uac, mons[u.umonnum].mlevel);
  (void) describe_level(nb = eos(buf));
  putstr(WIN_STATUS, 0, buf);
  Strcpy(buf, plname);
  if('a' <= buf[0] && buf[0] <= 'z') buf[0] += 'A'-'a';
  buf[12] = 0;
  Sprintf(nb = eos(buf),", ");
  
  if (Upolyd) {
    char mbot[BUFSZ];
    int k = 0;
    
    Strcpy(mbot, mons[u.umonnum].mname);
    while(mbot[k] != 0) {
      if ((k == 0 || (k > 0 && mbot[k-1] == ' ')) &&
          'a' <= mbot[k] && mbot[k] <= 'z')
        mbot[k] += 'A' - 'a';
      k++;
    }
    Sprintf(nb = eos(nb), "%s", mbot);
  } else Sprintf(nb = eos(nb), "%s", rank());
  curs(WIN_STATUS, 80-strlen(buf), 0);
  putstr(WIN_STATUS, 0, buf);
}

/* provide the name of the current level for display by various ports */
int
describe_level(buf)
char *buf;
{
  int ret = 1;

  if (Is_knox(&u.uz))
    Sprintf(buf, "%s", dungeons[u.uz.dnum].dname);
  else if (In_quest(&u.uz))
    Sprintf(buf, "Home:%d", dunlev(&u.uz));
  else if (In_endgame(&u.uz))
    Sprintf(buf, Is_astralevel(&u.uz) ? "Astral Plane" : "End Game");
  else if (In_mines(&u.uz))
    Sprintf(buf, "Mines:%d", depth(&u.uz));
  else if (In_sokoban(&u.uz))
    Sprintf(buf, "Sokoban:%d", depth(&u.uz));
  else if (Is_valley(&u.uz))
    Sprintf(buf, "Valley:%d", depth(&u.uz));
  else if (In_hell(&u.uz))
    Sprintf(buf, "Gehennom:%d", depth(&u.uz));
  else if (In_V_tower(&u.uz))
    Sprintf(buf, "Tower:%d", depth(&u.uz));
  else
    Sprintf(buf, "Dungeons:%d", depth(&u.uz)), (ret=0);
  return ret;
}

STATIC_OVL void
draw_status(msg, pos, color)
char* msg;
int* pos;
int color;
{
  if (*pos < 21) {*pos += strlen(msg)+1; return;}
  curs(WIN_STATUS, *pos, 1);
  *pos += strlen(msg);
  putstr_colored(WIN_STATUS, 0, color, msg);
  if (*pos < 80) {
    curs(WIN_STATUS, *pos, 1);
    putstr(WIN_STATUS, 0, " ");
  }
  (*pos)++;
}

STATIC_OVL void
bot2()
{
  char  buf[MAXCO];
  int cap = near_capacity();
  int spos = -1081, i;
  
  hp_pw_bar(u.uen, u.uenmax, 0, 1);
  Sprintf(buf, "%c%ld S:%ld T:%ld", oc_syms[COIN_CLASS],
#ifndef GOLDOBJ
          u.ugold,
#else
          money_cnt(invent),
#endif
          botl_score(), moves);
  putstr(WIN_STATUS, 0, buf);

  /* Rules for statuscolors:
   * Things that the player cannot go around with forever are bright:
   * Delayed instadeaths (e.g. Ill) are bright magenta
   * Warnings (e.g. Weak) are orange, or red if less urgent (e.g. Hungry)
   * Things that the player can live with for ages are darker:
   * Good statuses (e.g. St+) are green
   * Bad statuses (e.g. St-) are magenta
   * Neutral statuses (e.g. Lev) are brown
   * Temporary statuses (e.g. Blind, Conf) are bright blue
   * The least bad statuses are drawn first; two passes are made, one to
   * figure out where to start drawing, the other to draw.
   */
  for(i = 1; i <= 2; i++) {
    if(ACURR(A_STR) > AMAX(A_STR)) draw_status("St+", &spos, CLR_GREEN);
    if(ACURR(A_DEX) > AMAX(A_DEX)) draw_status("Dx+", &spos, CLR_GREEN);
    if(ACURR(A_CON) > AMAX(A_CON)) draw_status("Co+", &spos, CLR_GREEN);
    if(ACURR(A_INT) > AMAX(A_INT)) draw_status("In+", &spos, CLR_GREEN);
    if(ACURR(A_WIS) > AMAX(A_WIS)) draw_status("Wi+", &spos, CLR_GREEN);
    if(ACURR(A_CHA) > AMAX(A_CHA)) draw_status("Ch+", &spos, CLR_GREEN);
    if(heptagram_count(u.ux, u.uy)) {
      int hc = heptagram_count(u.ux, u.uy);
      char bf[20];
      Sprintf(bf, "Hept:%d", hc);
      draw_status(bf, &spos, CLR_GREEN);
    }
    if(Levitation)     draw_status("Lev", &spos, CLR_BROWN);
    if(unweapon)       draw_status("Unarmed", &spos, CLR_MAGENTA);
    if(ACURR(A_STR) < AMAX(A_STR)) draw_status("St-", &spos, CLR_MAGENTA);
    if(ACURR(A_DEX) < AMAX(A_DEX)) draw_status("Dx-", &spos, CLR_MAGENTA);
    if(ACURR(A_CON) < AMAX(A_CON)) draw_status("Co-", &spos, CLR_MAGENTA);
    if(ACURR(A_INT) < AMAX(A_INT)) draw_status("In-", &spos, CLR_MAGENTA);
    if(ACURR(A_WIS) < AMAX(A_WIS)) draw_status("Wi-", &spos, CLR_MAGENTA);
    if(ACURR(A_CHA) < AMAX(A_CHA)) draw_status("Ch-", &spos, CLR_MAGENTA);
    if(cap > UNENCUMBERED)
      draw_status(enc_stat[cap], &spos, CLR_MAGENTA);
    if(u.utrap)       draw_status("Trap", &spos, u.utraptype == TT_LAVA ?
                                  CLR_BRIGHT_MAGENTA : CLR_RED);
    if(strcmp(hu_stat[u.uhs], "")) {
      draw_status(hu_stat[u.uhs], &spos,
                  u.uhs==HUNGRY||u.uhs==SATIATED ? CLR_RED :
                  u.uhs>=FAINTING ? CLR_BRIGHT_MAGENTA : CLR_ORANGE);
    }
    if(Blind)          draw_status("Blind", &spos, CLR_BRIGHT_BLUE);
    if(Glib)           draw_status("Greasy", &spos, CLR_BRIGHT_BLUE);
    if(Confusion)      draw_status("Conf", &spos, CLR_BRIGHT_BLUE);
    if(Wounded_legs)   draw_status("Lame", &spos, CLR_BRIGHT_BLUE);
    if(Stunned)        draw_status("Stun", &spos, CLR_BRIGHT_BLUE);
    if(Hallucination)  draw_status("Hallu", &spos, CLR_BRIGHT_BLUE);
    if(Sick) {
      if (u.usick_type & SICK_VOMITABLE)
        draw_status("FoodPois", &spos, CLR_BRIGHT_MAGENTA);
      if (u.usick_type & SICK_NONVOMITABLE)
        draw_status("Ill", &spos, CLR_BRIGHT_MAGENTA);
    }
    if(Strangled)      draw_status("Strangle", &spos, CLR_BRIGHT_MAGENTA);
    if(Slimed)         draw_status("Slime", &spos, CLR_BRIGHT_MAGENTA);
    if(Stoned)         draw_status("Petrify", &spos, CLR_BRIGHT_MAGENTA);
    spos = -1000-spos;
  }
  curs(WIN_STATUS, 80, 1);
}

void
bot()
{
	bot1();
	bot2();
	flags.botl = flags.botlx = 0;
}

#endif /* OVL0 */

/*botl.c*/
