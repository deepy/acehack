/*	SCCS Id: @(#)mapglyph.c	3.4	2003/01/08	*/
/* Copyright (c) David Cohrs, 1991				  */
/* Modified 13 May 2011 by Alex Smith */
/* NetHack may be freely redistributed.  See license for details. */

#include "hack.h"
#if defined(TTY_GRAPHICS)
#include "wintty.h"	/* for prototype of has_color() only */
#endif
#include "color.h"
#define HI_DOMESTIC CLR_WHITE	/* monst.c */

int explcolors[] = {
	CLR_BLACK,	/* dark    */
	CLR_GREEN,	/* noxious */
	CLR_BROWN,	/* muddy   */
	CLR_BLUE,	/* wet     */
	CLR_MAGENTA,	/* magical */
	CLR_ORANGE,	/* fiery   */
	CLR_WHITE,	/* frosty  */
};

#if !defined(TTY_GRAPHICS)
#define has_color(n)  TRUE
#endif

#ifdef TEXTCOLOR
#define zap_color(n)  color = iflags.use_color ? zapcolors[n] : NO_COLOR
#define cmap_color(n) color = iflags.use_color ? defsyms[n].color : NO_COLOR
#define obj_color(n)  color = iflags.use_color ? objects[n].oc_color : NO_COLOR
#define mon_color(n)  color = iflags.use_color ? mons[n].mcolor : NO_COLOR
#define invis_color(n) color = iflags.use_color && x==u.ux && y==u.uy   \
    ? HI_DOMESTIC : NO_COLOR
#define pet_color(n)  color = iflags.use_color ? mons[n].mcolor : NO_COLOR
#define warn_color(n) color = iflags.use_color ? def_warnsyms[n].color : NO_COLOR
#define explode_color(n) color = iflags.use_color ? explcolors[n] : NO_COLOR
# if defined(REINCARNATION) && defined(ASCIIGRAPH)
#  define ROGUE_COLOR
# endif

#else	/* no text color */

#define zap_color(n)
#define cmap_color(n)
#define obj_color(n)
#define mon_color(n)
#define invis_color(n)
#define pet_color(c)
#define warn_color(n)
#define explode_color(n)
#endif

#define check_uline() color & MG_ULINE ? color &= ~MG_ULINE, special |= MG_ULINE : 0

#ifdef ROGUE_COLOR
# if defined(USE_TILES) && defined(MSDOS)
#define HAS_ROGUE_IBM_GRAPHICS (iflags.IBMgraphics && !iflags.grmode && \
	Is_rogue_level(&u.uz))
# else
#define HAS_ROGUE_IBM_GRAPHICS (iflags.IBMgraphics && Is_rogue_level(&u.uz))
# endif
#endif

STATIC_DCL int
dungeon_specific_color(glyph, disturbed, defcolor, x, y)
int glyph, defcolor, x, y;
boolean disturbed;
{
  /* The only glyphs that we color per-dungeon are walls, and
     room/darkroom/corr/litcoor. */
  register int glyphtype = 0;
  switch(glyph_to_cmap(glyph)) {
  case S_room: case S_litcorr: glyphtype = 1; break;
  case S_corr: case S_darkroom: glyphtype = 2; break;
  case S_tlcorn: case S_trcorn: case S_blcorn: case S_brcorn:
  case S_tuwall: case S_tdwall: case S_tlwall: case S_trwall:
  case S_vwall: case S_hwall: case S_crwall: glyphtype = 3; break;
  }
  if (glyphtype <= (iflags.floorcolor ? 0 : 2)) return defcolor;
  /* There are five custom colors per branch:
     lit undisturbed, dark undisturbed, lit disturbed, dark disturbed, wall */
#define CLR_NONE NO_COLOR
#define COLORSET(l1,u1,l2,u2,w)                                         \
  return disturbed ?                                                    \
    (glyphtype == 1 ? CLR_##l2 : glyphtype == 2 ? CLR_##u2 : CLR_##w) : \
    (glyphtype == 1 ? CLR_##l1 : glyphtype == 2 ? CLR_##u1 : CLR_##w)
  if (Is_knox(&u.uz)) /* Ludios */
    COLORSET(YELLOW,BROWN,BRIGHT_GREEN,RED,YELLOW);
/* Just use defaults for the Quest for now, it's hard to find a good scheme  
  else if (In_quest(&u.uz))
    COLORSET(GREEN,BLUE,BROWN,CYAN,NONE); */
  else if (In_endgame(&u.uz)) /* Planes; the only walls are on Astral */
    COLORSET(WHITE,RED,YELLOW,BROWN,WHITE);
  else if (In_mines(&u.uz)) /* Mines */
    COLORSET(NONE,BROWN,MAGENTA,RED,BROWN);
  else if (In_sokoban(&u.uz)) /* Sokoban */
    COLORSET(CYAN,BLUE,GREEN,MAGENTA,BRIGHT_BLUE);
  else if (Is_valley(&u.uz)) /* Valley: a minimalist look */
    COLORSET(WHITE,NONE,WHITE,NONE,NONE);
  else if (In_W_tower(x, y, &u.uz)) /* Rodney's */
    COLORSET(BRIGHT_BLUE,BLUE,BRIGHT_CYAN,CYAN,BRIGHT_MAGENTA);
  else if (In_hell(&u.uz)) /* Gehennom */
    COLORSET(ORANGE,RED,BRIGHT_MAGENTA,MAGENTA,ORANGE);
  else /* Vlad's or Dungeons */
    COLORSET(NONE,BLUE,BROWN,CYAN,NONE);
#undef COLOR_NONE
#undef COLORSET
  /* unreachable */
}

static int getdirx, getdiry, getdirh = GETDIRH_NONE;
void
set_getdir_type(how)
int how;
{
  getdirh = how;
  getdirx = u.ux;
  getdiry = u.uy;
}

/*ARGSUSED*/
void
mapglyph(glyph, ochar, ocolor, ospecial, x, y)
int glyph, *ocolor, x, y;
int *ochar;
unsigned *ospecial;
{
	register int offset;
#if defined(TEXTCOLOR) || defined(ROGUE_COLOR)
	int color = NO_COLOR;
#endif
	uchar ch;
	unsigned special = 0;

    /*
     *  Map the glyph back to a character and color.
     *
     *  Warning:  For speed, this makes an assumption on the order of
     *		  offsets.  The order is set in display.h.
     */
    if ((offset = (glyph - GLYPH_WARNING_OFF)) >= 0) {	/* a warning flash */
    	ch = warnsyms[offset];
# ifdef ROGUE_COLOR
	if (HAS_ROGUE_IBM_GRAPHICS)
	    color = NO_COLOR;
	else
# endif
	    warn_color(offset);
    } else if ((offset = (glyph - GLYPH_SWALLOW_OFF)) >= 0) {	/* swallow */
	/* see swallow_to_glyph() in display.c */
	ch = (uchar) showsyms[S_sw_tl + (offset & 0x7)];
#ifdef ROGUE_COLOR
	if (HAS_ROGUE_IBM_GRAPHICS && iflags.use_color)
	    color = NO_COLOR;
	else
#endif
	    mon_color(offset >> 3);
        check_uline();
    } else if ((offset = (glyph - GLYPH_ZAP_OFF)) >= 0) {	/* zap beam */
	/* see zapdir_to_glyph() in display.c */
	ch = showsyms[S_vbeam + (offset & 0x3)];
#ifdef ROGUE_COLOR
	if (HAS_ROGUE_IBM_GRAPHICS && iflags.use_color)
	    color = NO_COLOR;
	else
#endif
	    zap_color((offset >> 2));
    } else if ((offset = (glyph - GLYPH_EXPLODE_OFF)) >= 0) {	/* explosion */
	ch = showsyms[(offset % MAXEXPCHARS) + S_explode1];
	explode_color(offset / MAXEXPCHARS);
    } else if ((offset = (glyph - GLYPH_CMAP_OFF)) >= 0) {	/* cmap */
	ch = showsyms[offset];
#ifdef ROGUE_COLOR
	if (HAS_ROGUE_IBM_GRAPHICS && iflags.use_color) {
	    if (offset >= S_vwall && offset <= S_hcdoor)
		color = CLR_BROWN;
	    else if (offset >= S_arrow_trap && offset <= S_polymorph_trap)
		color = CLR_MAGENTA;
	    else if (offset == S_corr || offset == S_litcorr)
		color = CLR_GRAY;
	    else if (offset >= S_room && offset <= S_water && offset != S_darkroom)
		color = CLR_GREEN;
	    else
		color = NO_COLOR;
	} else
#endif
        {
            cmap_color(offset);
            /* The vibrating square has color rules of its own... */
#ifdef TEXTCOLOR
            if (iflags.use_color && glyph == cmap_to_glyph(S_room) &&
                x == inv_pos.x && y == inv_pos.y && Invocation_lev(&u.uz))
                color = CLR_BRIGHT_GREEN;
            /* Otherwise, customise colors for the location based on the branch
               and exact location */
            else if (glyph_to_cmap(glyph) == S_vcdoor ||
                     glyph_to_cmap(glyph) == S_hcdoor) {
              /* Closed doors that have known lock status are coloured
                 accordingly; if they also are known to be trapped, that
                 shows in their background color. (Yes, this is looking
                 at character-secret information in the display code,
                 but it's safe because we first check that the character
                 already knew that information. */
              if (levl[x][y].fknown & FKNOWN_LOCKED)
                color = (levl[x][y].flags & D_LOCKED) ? CLR_RED : CLR_GREEN;
              if (levl[x][y].fknown & FKNOWN_TRAPPED &&
                  levl[x][y].flags & D_TRAPPED) {
                color %= CLR_MAX; /* just in case... */
                color += CLR_MAX*CLR_CYAN;
              }
            }
            else color = dungeon_specific_color(
              glyph, levl[x][y].stepped_on, color, x, y);
        }
#endif
    } else if ((offset = (glyph - GLYPH_OBJ_OFF)) >= 0) {	/* object */
	if (offset == BOULDER && iflags.bouldersym) ch = iflags.bouldersym;
	else ch = oc_syms[(int)objects[offset].oc_class];
#ifdef ROGUE_COLOR
	if (HAS_ROGUE_IBM_GRAPHICS && iflags.use_color) {
	    switch(objects[offset].oc_class) {
		case COIN_CLASS: color = CLR_YELLOW; break;
		case FOOD_CLASS: color = CLR_RED; break;
		default: color = CLR_BRIGHT_BLUE; break;
	    }
	} else
#endif
	    obj_color(offset);
    } else if ((offset = (glyph - GLYPH_RIDDEN_OFF)) >= 0) {	/* mon ridden */
	ch = monsyms[(int)mons[offset].mlet];
#ifdef ROGUE_COLOR
	if (HAS_ROGUE_IBM_GRAPHICS)
	    /* This currently implies that the hero is here -- monsters */
	    /* don't ride (yet...).  Should we set it to yellow like in */
	    /* the monster case below?  There is no equivalent in rogue. */
	    color = NO_COLOR;	/* no need to check iflags.use_color */
	else
#endif
            mon_color(offset);
        check_uline();
        if (iflags.wc_color) color += CLR_MAX*CLR_BLUE;
        else special |= MG_RIDDEN;
    } else if ((offset = (glyph - GLYPH_BODY_OFF)) >= 0) {	/* a corpse */
	ch = oc_syms[(int)objects[CORPSE].oc_class];
#ifdef ROGUE_COLOR
	if (HAS_ROGUE_IBM_GRAPHICS && iflags.use_color)
	    color = CLR_RED;
	else
#endif
	    mon_color(offset);
        color %= CLR_MAX;
        special |= MG_CORPSE;
    } else if ((offset = (glyph - GLYPH_DETECT_OFF)) >= 0) {	/* mon detect */
	ch = monsyms[(int)mons[offset].mlet];
#ifdef ROGUE_COLOR
	if (HAS_ROGUE_IBM_GRAPHICS)
	    color = NO_COLOR;	/* no need to check iflags.use_color */
	else
#endif
	    mon_color(offset);
        check_uline();
        if (iflags.wc_color) color += CLR_MAX*CLR_MAGENTA;
        else special |= MG_DETECT;
    } else if ((offset = (glyph - GLYPH_INVIS_OFF)) >= 0) {	/* invisible */
	ch = DEF_INVISIBLE;
#ifdef ROGUE_COLOR
	if (HAS_ROGUE_IBM_GRAPHICS)
	    color = NO_COLOR;	/* no need to check iflags.use_color */
	else
#endif
	    invis_color(offset);
	    special |= MG_INVIS;
    } else if ((offset = (glyph - GLYPH_PET_OFF)) >= 0) {	/* a pet */
	ch = monsyms[(int)mons[offset].mlet];
#ifdef ROGUE_COLOR
	if (HAS_ROGUE_IBM_GRAPHICS)
	    color = NO_COLOR;	/* no need to check iflags.use_color */
	else
#endif
	    pet_color(offset);
        check_uline();
        if (iflags.wc_color) color += CLR_MAX*CLR_BLUE;
        else special |= MG_PET;
    } else {							/* a monster */
	ch = monsyms[(int)mons[glyph].mlet];
#ifdef ROGUE_COLOR
	if (HAS_ROGUE_IBM_GRAPHICS && iflags.use_color) {
	    if (x == u.ux && y == u.uy)
		/* actually player should be yellow-on-gray if in a corridor */
		color = CLR_YELLOW;
	    else
		color = NO_COLOR;
	} else
#endif
	{
            struct monst * mon;
	    mon_color(glyph);
            check_uline();
            /* Peaceful monsters get their own branding. If we're drawing
               the monster at all, the hero must be able to see it, and thus
               farlook it, so this doesn't leak info. */
            mon = m_at(x,y);
            if (mon && mon->mpeaceful && iflags.wc_color)
                color += CLR_MAX*CLR_BROWN;
	    /* special case the hero for `showrace' option */
#ifdef TEXTCOLOR
	    if (iflags.use_color && x == u.ux && y == u.uy &&
		    iflags.showrace && !Upolyd)
		color = HI_DOMESTIC;
#endif
	}
    }

#ifdef TEXTCOLOR
    /* Don't draw trap/portal branding on Water; the portals move, so the
       player shouldn't be allowed to memorize their positions. */
    if (!Is_waterlevel(&u.uz))
    {
      struct trap *ttmp = t_at(x,y);
      /* Brand squares with known stairs/ladders/portals as red, if there's
         no existing branding. (White looks nicer but several terminals
         can't handle it.) */
      if ((levl[x][y].styp == STAIRS || levl[x][y].styp == LADDER ||
           (ttmp && ttmp->tseen && ttmp->ttyp == MAGIC_PORTAL)) &&
          color < CLR_MAX) {
        color += CLR_MAX*CLR_RED;
      }
      /* Brand squares known to be trapped as cyan, if there's no existing
         branding. */
      if (ttmp && ttmp->tseen && color < CLR_MAX) {
        color += CLR_MAX*CLR_CYAN;
      }
    }
#endif

#ifdef TEXTCOLOR
    /* Turn off color if no color defined, or rogue level w/o PC graphics. */
# ifdef REINCARNATION
#  ifdef ASCIIGRAPH
    if (!has_color(color%CLR_MAX) || (Is_rogue_level(&u.uz) && !HAS_ROGUE_IBM_GRAPHICS))
#  else
    if (!has_color(color%CLR_MAX) || Is_rogue_level(&u.uz))
#  endif
# else
    if (!has_color(color%CLR_MAX))
# endif
	color = NO_COLOR;
#endif

    /* Do any background marking that getdir might want. For the time being,
       keep it simple: the lines extend to the edge of the map for RANGE or
       BOUNCE, and one character for NEXT. */
    if (getdirh != GETDIRH_NONE && iflags.targethighlight) {
      /* Grid bugs don't understand the concept of diagonals, even for
         merely highlighting on the map. */
      boolean gridbug = u.umonnum == PM_GRID_BUG;
      if (color < CLR_MAX &&
        (x == getdirx || y == getdiry ||
         (!gridbug && (x+y == getdirx+getdiry || x-y == getdirx-getdiry))) &&
        (getdirh != GETDIRH_NEXT ||
         (abs(x-getdirx) <= 1 && abs(y-getdiry) <= 1)))
        color += CLR_MAX * CLR_RED;
    }

    if (Hallucination) {
      /* randomize backgrounds to avoid leaking information */
      color %= CLR_MAX;
      color += CLR_MAX * rn2(8);
      /* backgrounding every square is silly... */
      if (glyph_is_cmap(glyph)) color %= CLR_MAX;
    }

    *ochar = (int)ch;
    *ospecial = special;
#ifdef TEXTCOLOR
    *ocolor = color;
#endif
    return;
}

/*mapglyph.c*/
