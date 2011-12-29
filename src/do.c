/*	SCCS Id: @(#)do.c	3.4	2003/12/02	*/
/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */
/* Modified 29 Dec 2011 by Alex Smith */
/* NetHack may be freely redistributed.  See license for details. */

/* Contains code for 'd', 'D' (drop), '>', '<' (up, down) */

#include "hack.h"
#include "lev.h"
#include "edog.h"

#ifdef SINKS
# ifdef OVLB
STATIC_DCL void FDECL(trycall, (struct obj *));
# endif /* OVLB */
STATIC_DCL void FDECL(dosinkring, (struct obj *));
#endif /* SINKS */

STATIC_PTR int FDECL(drop, (struct obj *));
STATIC_PTR int NDECL(wipeoff);

#ifdef OVL0
STATIC_DCL int FDECL(menu_drop, (int));
#endif
#ifdef OVL2
STATIC_DCL int NDECL(currentlevel_rewrite);
STATIC_DCL void NDECL(final_level);
/* static boolean FDECL(badspot, (XCHAR_P,XCHAR_P)); */
#endif

#ifdef OVLB

static NEARDATA const char drop_types[] =
	{ ALLOW_COUNT, COIN_CLASS, ALL_CLASSES, 0 };

/* 'd' command: drop one inventory item */
int
dodrop()
{
#ifndef GOLDOBJ
	int result, i = (invent || u.ugold) ? 0 : (SIZE(drop_types) - 1);
#else
	int result, i = (invent) ? 0 : (SIZE(drop_types) - 1);
#endif

	if (*u.ushops) sellobj_state(SELL_DELIBERATE);
	result = drop(getobj(&drop_types[i], "drop"));
	if (*u.ushops) sellobj_state(SELL_NORMAL);
	reset_occupations();

	return result;
}

#endif /* OVLB */
#ifdef OVL0

/* Called when a boulder is dropped, thrown, or pushed.  If it ends up
 * in a pool, it either fills the pool up or sinks away.  In either case,
 * it's gone for good...  If the destination is not a pool, returns FALSE.
 */
boolean
boulder_hits_pool(otmp, rx, ry, pushing)
struct obj *otmp;
register int rx, ry;
boolean pushing;
{
	if (!otmp || otmp->otyp != BOULDER)
	    impossible("Not a boulder?");
	else if (!Is_waterlevel(&u.uz) && (is_pool(rx,ry) || is_lava(rx,ry))) {
	    boolean lava = is_lava(rx,ry), fills_up;
	    const char *what = waterbody_name(rx,ry);
	    schar ltyp = levl[rx][ry].typ;
	    int chance = rn2(10);		/* water: 90%; lava: 10% */
	    fills_up = lava ? chance == 0 : chance != 0;

	    if (fills_up) {
		struct trap *ttmp = t_at(rx, ry);

		if (ltyp == DRAWBRIDGE_UP) {
		    levl[rx][ry].drawbridgemask &= ~DB_UNDER; /* clear lava */
		    levl[rx][ry].drawbridgemask |= DB_FLOOR;
		} else
		    levl[rx][ry].typ = ROOM;

		if (ttmp) (void) delfloortrap(ttmp);
		bury_objs(rx, ry);
		
		newsym(rx,ry);
		if (pushing) {
#ifdef STEED
                    if (u.usteed) {
                      char *bp = y_monnam(u.usteed);
                      *bp = highc(*bp); /* bp points to a static buffer */
                        pline("%s pushes %s into the %s.",
                              bp, the(xname(otmp)), what);
                    }
                    else
#endif
		    You("push %s into the %s.", the(xname(otmp)), what);
		    if (flags.verbose && !Blind)
			pline("Now you can cross it!");
		    /* no splashing in this case */
		}
	    }
	    if (!fills_up || !pushing) {	/* splashing occurs */
		if (!u.uinwater) {
		    if (pushing ? !Blind : cansee(rx,ry)) {
			There("is a large splash as %s %s the %s.",
			      the(xname(otmp)), fills_up? "fills":"falls into",
			      what);
		    } else if (flags.soundok)
			You_hear("a%s splash.", lava ? " sizzling" : "");
		    wake_nearto(rx, ry, 40);
		}

		if (fills_up && u.uinwater && distu(rx,ry) == 0) {
		    u.uinwater = 0;
                    display_nhwindow(WIN_MESSAGE, FALSE);
		    docrt();
		    vision_full_recalc = 1;
		    You("find yourself on dry land again!");
		} else if (lava && distu(rx,ry) <= 2) {
		    You("are hit by molten lava%c",
			Fire_resistance ? '.' : '!');
			burn_away_slime();
		    losehp(d((Fire_resistance ? 1 : 3), 6),
			   "molten lava", KILLED_BY);
		} else if (!fills_up && flags.verbose &&
			   (pushing ? !Blind : cansee(rx,ry)))
		    pline("It sinks without a trace!");
	    }

	    /* boulder is now gone */
	    if (pushing) delobj(otmp);
	    else obfree(otmp, (struct obj *)0);
	    return TRUE;
	}
	return FALSE;
}

/* Used for objects which sometimes do special things when dropped; must be
 * called with the object not in any chain.  Returns TRUE if the object goes
 * away.
 */
boolean
flooreffects(obj,x,y,verb)
struct obj *obj;
int x,y;
const char *verb;
{
	struct trap *t;
	struct monst *mtmp;

	if (obj->where != OBJ_FREE)
	    panic("flooreffects: obj not free");

	/* make sure things like water_damage() have no pointers to follow */
	obj->nobj = obj->nexthere = (struct obj *)0;

	if (obj->otyp == BOULDER && boulder_hits_pool(obj, x, y, FALSE))
		return TRUE;
	else if (obj->otyp == BOULDER && (t = t_at(x,y)) != 0 &&
		 (t->ttyp==PIT || t->ttyp==SPIKED_PIT
			|| t->ttyp==TRAPDOOR || t->ttyp==HOLE)) {
		if (((mtmp = m_at(x, y)) && mtmp->mtrapped) ||
			(u.utrap && u.ux == x && u.uy == y)) {
		    if (*verb)
			pline_The("boulder %s into the pit%s.",
				vtense((const char *)0, verb),
				(mtmp) ? "" : " with you");
		    if (mtmp) {
                        /* We let other players phase through boulders, in
                           order to avoid transmitting to the other game
                           what just happened. (TODO: transmit trapped
                           status so that this works.) */
			if (!passes_walls(mtmp->data) &&
                            !throws_rocks(mtmp->data) &&
                            !is_mp_player(mtmp)) {
			    if (hmon(mtmp, obj, TRUE) && !is_whirly(mtmp->data))
				return FALSE;	/* still alive */
			}
                        message_monster(mtmp,
                                        "You climb out of danger as a boulder "
                                        "falls past you.");
			mtmp->mtrapped = 0;
		    } else {
			if (!Passes_walls && !throws_rocks(youmonst.data)) {
			    losehp(rnd(15), "squished under a boulder",
				   NO_KILLER_PREFIX);
			    return FALSE;	/* player remains trapped */
			} else u.utrap = 0;
		    }
		}
		if (*verb) {
			if (Blind) {
				if ((x == u.ux) && (y == u.uy))
					You_hear("a CRASH! beneath you.");
				else
					You_hear("the boulder %s.", verb);
			} else if (cansee(x, y)) {
				pline_The("boulder %s%s.",
				    t->tseen ? "" : "triggers and ",
				    t->ttyp == TRAPDOOR ? "plugs a trap door" :
				    t->ttyp == HOLE ? "plugs a hole" :
				    "fills a pit");
			}
		}
		deltrap(t);
		obfree(obj, (struct obj *)0);
		bury_objs(x, y);
		newsym(x,y);
		return TRUE;
	} else if (is_lava(x, y)) {
		return fire_damage(obj, FALSE, FALSE, x, y);
	} else if (is_pool(x, y)) {
		/* Reasonably bulky objects (arbitrary) splash when dropped.
		 * If you're floating above the water even small things make noise.
		 * Stuff dropped near fountains always misses */
		if ((Blind || (Levitation || Flying)) && flags.soundok &&
		    ((x == u.ux) && (y == u.uy))) {
		    if (!Underwater) {
			if (weight(obj) > 9) {
				pline("Splash!");
		        } else if (Levitation || Flying) {
				pline("Plop!");
		        }
		    }
		    map_background(x, y, 0);
		    newsym(x, y);
		}
		return water_damage(obj, FALSE, FALSE);
	} else if (u.ux == x && u.uy == y &&
		(!u.utrap || u.utraptype != TT_PIT) &&
		(t = t_at(x,y)) != 0 && t->tseen &&
			(t->ttyp==PIT || t->ttyp==SPIKED_PIT)) {
		/* you escaped a pit and are standing on the precipice */
		if (Blind && flags.soundok)
			You_hear("%s %s downwards.",
				The(xname(obj)), otense(obj, "tumble"));
		else
			pline("%s %s into %s pit.",
				The(xname(obj)), otense(obj, "tumble"),
				the_your[t->madeby_u]);
	}
	return FALSE;
}

#endif /* OVL0 */
#ifdef OVLB

void
doaltarobj(obj)  /* obj is an object dropped on an altar */
	register struct obj *obj;
{
	if (Blind)
		return;

	/* KMH, conduct */
	u.uconduct.gnostic++;

	if ((obj->blessed || obj->cursed) && obj->oclass != COIN_CLASS) {
		There("is %s flash as %s %s the altar.",
			an(hcolor(obj->blessed ? NH_AMBER : NH_BLACK)),
			doname(obj), otense(obj, "hit"));
		if (!Hallucination) obj->bknown = 1;
	} else {
		pline("%s %s on the altar.", Doname2(obj),
			otense(obj, "land"));
		obj->bknown = 1;
	}
        /* Also BCU one level deep inside containers */
        if (Has_contents(obj)) {
            int bcucount = 0;
            struct obj *otmp;
            for (otmp = obj->cobj; otmp; otmp = otmp->nobj) {
                if (otmp->blessed || otmp->cursed)
                    bcucount++;
                if (!Hallucination) otmp->bknown = 1;
            }
            if (bcucount == 1) {
                pline("Looking inside %s, you see a colored flash.",
                      the(xname(obj)));
            } else if (bcucount > 1) {
                pline("Looking inside %s, you see colored flashes.",
                      the(xname(obj)));
            }
        }
}

#ifdef SINKS
STATIC_OVL
void
trycall(obj)
register struct obj *obj;
{
	if(!objects[obj->otyp].oc_name_known &&
	   !objects[obj->otyp].oc_uname)
	   docall(obj);
}

STATIC_OVL
void
dosinkring(obj)  /* obj is a ring being dropped over a kitchen sink */
register struct obj *obj;
{
	register struct obj *otmp,*otmp2;
	register boolean ideed = TRUE;

	You("drop %s down the drain.", doname(obj));
	obj->in_use = TRUE;	/* block free identification via interrupt */
	switch(obj->otyp) {	/* effects that can be noticed without eyes */
	    case RIN_SEARCHING:
		You("thought your %s got lost in the sink, but there it is!",
			xname(obj));
		goto giveback;
	    case RIN_SLOW_DIGESTION:
		pline_The("ring is regurgitated!");
giveback:
		obj->in_use = FALSE;
		dropx(obj);
		trycall(obj);
		return;
	    case RIN_LEVITATION:
		pline_The("sink quivers upward for a moment.");
		break;
	    case RIN_POISON_RESISTANCE:
		You("smell rotten %s.", makeplural(fruitname(FALSE)));
		break;
	    case RIN_AGGRAVATE_MONSTER:
		pline("Several flies buzz angrily around the sink.");
		break;
	    case RIN_SHOCK_RESISTANCE:
		pline("Static electricity surrounds the sink.");
		break;
	    case RIN_CONFLICT:
		You_hear("loud noises coming from the drain.");
		break;
	    case RIN_SUSTAIN_ABILITY:	/* KMH */
		pline_The("water flow seems fixed.");
		break;
	    case RIN_GAIN_STRENGTH:
		pline_The("water flow seems %ser now.",
			(obj->spe<0) ? "weak" : "strong");
		break;
	    case RIN_GAIN_CONSTITUTION:
		pline_The("water flow seems %ser now.",
			(obj->spe<0) ? "less" : "great");
		break;
	    case RIN_INCREASE_ACCURACY:	/* KMH */
		pline_The("water flow %s the drain.",
			(obj->spe<0) ? "misses" : "hits");
		break;
	    case RIN_INCREASE_DAMAGE:
		pline_The("water's force seems %ser now.",
			(obj->spe<0) ? "small" : "great");
		break;
	    case RIN_HUNGER:
		ideed = FALSE;
		for(otmp = level.objects[u.ux][u.uy]; otmp; otmp = otmp2) {
		    otmp2 = otmp->nexthere;
		    if (otmp != uball && otmp != uchain &&
			    !obj_resists(otmp, 1, 99)) {
			if (!Blind) {
			    pline("Suddenly, %s %s from the sink!",
				  doname(otmp), otense(otmp, "vanish"));
			    ideed = TRUE;
			}
			delobj(otmp);
		    }
		}
		break;
	    case MEAT_RING:
		/* Not the same as aggravate monster; besides, it's obvious. */
		pline("Several flies buzz around the sink.");
		break;
	    default:
		ideed = FALSE;
		break;
	}
	if(!Blind && !ideed && obj->otyp != RIN_HUNGER) {
	    ideed = TRUE;
	    switch(obj->otyp) {		/* effects that need eyes */
		case RIN_ADORNMENT:
		    pline_The("faucets flash brightly for a moment.");
		    break;
		case RIN_REGENERATION:
		    pline_The("sink looks as good as new.");
		    break;
		case RIN_INVISIBILITY:
		    You("don't see anything happen to the sink.");
		    break;
		case RIN_FREE_ACTION:
		    You("see the ring slide right down the drain!");
		    break;
		case RIN_SEE_INVISIBLE:
		    You("see some air in the sink.");
		    break;
		case RIN_STEALTH:
		pline_The("sink seems to blend into the floor for a moment.");
		    break;
		case RIN_FIRE_RESISTANCE:
		pline_The("hot water faucet flashes brightly for a moment.");
		    break;
		case RIN_COLD_RESISTANCE:
		pline_The("cold water faucet flashes brightly for a moment.");
		    break;
		case RIN_PROTECTION_FROM_SHAPE_CHAN:
		    pline_The("sink looks nothing like a fountain.");
		    break;
		case RIN_PROTECTION:
		    pline_The("sink glows %s for a moment.",
			    hcolor((obj->spe<0) ? NH_BLACK : NH_SILVER));
		    break;
		case RIN_WARNING:
		    pline_The("sink glows %s for a moment.", hcolor(NH_WHITE));
		    break;
		case RIN_TELEPORTATION:
		    pline_The("sink momentarily vanishes.");
		    break;
		case RIN_TELEPORT_CONTROL:
	    pline_The("sink looks like it is being beamed aboard somewhere.");
		    break;
		case RIN_POLYMORPH:
		    pline_The("sink momentarily looks like a fountain.");
		    break;
		case RIN_POLYMORPH_CONTROL:
	pline_The("sink momentarily looks like a regularly erupting geyser.");
		    break;
	    }
	}
	if(ideed)
	    trycall(obj);
	else
	    You_hear("the ring bouncing down the drainpipe.");
	if (!rn2(20)) {
		pline_The("sink backs up, leaving %s.", doname(obj));
		obj->in_use = FALSE;
		dropx(obj);
	} else
		useup(obj);
}
#endif

#endif /* OVLB */
#ifdef OVL0

/* some common tests when trying to drop or throw items */
boolean
canletgo(obj,word)
register struct obj *obj;
register const char *word;
{
	if(obj->owornmask & (W_ARMOR | W_RING | W_AMUL | W_TOOL)){
		if (*word)
			Norep("You cannot %s %s you are wearing.",word,
				something);
		return(FALSE);
	}
	if (obj->otyp == LOADSTONE && obj->cursed) {
		/* getobj() kludge sets corpsenm to user's specified count
		   when refusing to split a stack of cursed loadstones */
		if (*word) {
			/* getobj() ignores a count for throwing since that is
			   implicitly forced to be 1; replicate its kludge... */
			if (!strcmp(word, "throw") && obj->quan > 1L)
			    obj->corpsenm = 1;
			pline("For some reason, you cannot %s%s the stone%s!",
			      word, obj->corpsenm ? " any of" : "",
			      plur(obj->quan));
		}
		obj->corpsenm = 0;		/* reset */
		obj->bknown = 1;
		return(FALSE);
	}
	if (obj->otyp == LEASH && obj->leashmon != 0) {
		if (*word)
			pline_The("leash is tied around your %s.",
					body_part(HAND));
		return(FALSE);
	}
#ifdef STEED
	if (obj->owornmask & W_SADDLE) {
		if (*word)
			You("cannot %s %s you are sitting on.", word,
				something);
		return (FALSE);
	}
#endif
	return(TRUE);
}

STATIC_PTR
int
drop(obj)
register struct obj *obj;
{
	if(!obj) return(0);
	if(!canletgo(obj,"drop"))
		return(0);
	if(obj == uwep) {
		if(welded(uwep)) {
			weldmsg(obj);
			return(0);
		}
		setuwep((struct obj *)0);
	}
	if(obj == uquiver) {
		setuqwep((struct obj *)0);
	}
	if (obj == uswapwep) {
		setuswapwep((struct obj *)0);
	}

	if (u.uswallow) {
		/* barrier between you and the floor */
		if(flags.verbose)
		{
			char buf[BUFSZ];

			/* doname can call s_suffix, reusing its buffer */
			Strcpy(buf, s_suffix(mon_nam(u.ustuck)));
			You("drop %s into %s %s.", doname(obj), buf,
				mbodypart(u.ustuck, STOMACH));
		}
	} else {
#ifdef SINKS
	    if((obj->oclass == RING_CLASS || obj->otyp == MEAT_RING) &&
			IS_SINK(levl[u.ux][u.uy].typ)) {
		dosinkring(obj);
		return(1);
	    }
#endif
	    if (!can_reach_floor()) {
		if(flags.verbose) You("drop %s.", doname(obj));
#ifndef GOLDOBJ
		if (obj->oclass != COIN_CLASS || obj == invent) freeinv(obj);
#else
		/* Ensure update when we drop gold objects */
		if (obj->oclass == COIN_CLASS) flags.botl = 1;
		freeinv(obj);
#endif
		hitfloor(obj);
		return(1);
	    }
	    if (!IS_ALTAR(levl[u.ux][u.uy].typ) && flags.verbose)
		You("drop %s.", doname(obj));
	}
	dropx(obj);
	return(1);
}

/* Called in several places - may produce output */
/* eg ship_object() and dropy() -> sellobj() both produce output */
void
dropx(obj)
register struct obj *obj;
{
#ifndef GOLDOBJ
	if (obj->oclass != COIN_CLASS || obj == invent) freeinv(obj);
#else
        /* Ensure update when we drop gold objects */
        if (obj->oclass == COIN_CLASS) flags.botl = 1;
        freeinv(obj);
#endif
	if (!u.uswallow) {
	    if (ship_object(obj, u.ux, u.uy, FALSE)) return;
	    if (IS_ALTAR(levl[u.ux][u.uy].typ))
		doaltarobj(obj); /* set bknown */
	}
	dropy(obj);
}

void
dropy(obj)
register struct obj *obj;
{
	if (obj == uwep) setuwep((struct obj *)0);
	if (obj == uquiver) setuqwep((struct obj *)0);
	if (obj == uswapwep) setuswapwep((struct obj *)0);

	if (!u.uswallow && flooreffects(obj,u.ux,u.uy,"drop")) return;
	/* uswallow check done by GAN 01/29/87 */
	if(u.uswallow) {
	    boolean could_petrify = FALSE;
	    boolean could_poly = FALSE;
	    boolean could_slime = FALSE;
	    boolean could_grow = FALSE;
	    boolean could_heal = FALSE;

	    if (obj != uball) {		/* mon doesn't pick up ball */
		if (obj->otyp == CORPSE) {
		    could_petrify = touch_petrifies(&mons[obj->corpsenm]);
		    could_poly = polyfodder(obj);
		    could_slime = (obj->corpsenm == PM_GREEN_SLIME);
		    could_grow = (obj->corpsenm == PM_WRAITH);
		    could_heal = (obj->corpsenm == PM_NURSE);
		}
		(void) mpickobj(u.ustuck,obj);
		if (is_animal(u.ustuck->data)) {
		    if (could_poly || could_slime) {
			(void) newcham(u.ustuck,
				       could_poly ? (struct permonst *)0 :
				       &mons[PM_GREEN_SLIME],
				       FALSE, could_slime);
			delobj(obj);	/* corpse is digested */
		    } else if (could_petrify) {
			minstapetrify(u.ustuck, TRUE);
			/* Don't leave a cockatrice corpse in a statue */
			if (!u.uswallow) delobj(obj);
		    } else if (could_grow) {
			(void) grow_up(u.ustuck, (struct monst *)0);
			delobj(obj);	/* corpse is digested */
		    } else if (could_heal) {
			u.ustuck->mhp = u.ustuck->mhpmax;
			delobj(obj);	/* corpse is digested */
		    }
		}
	    }
	} else  {
	    place_object(obj, u.ux, u.uy);
	    if (obj == uball)
		drop_ball(u.ux,u.uy);
	    else
		sellobj(obj, u.ux, u.uy);
	    stackobj(obj);
	    if(Blind && Levitation)
		map_object(obj, 0);
	    newsym(u.ux,u.uy);	/* remap location under self */
	}
}

/* things that must change when not held; recurse into containers.
   Called for both player and monsters */
void
obj_no_longer_held(obj)
struct obj *obj;
{
	if (!obj) {
	    return;
	} else if ((Is_container(obj) || obj->otyp == STATUE) && obj->cobj) {
	    struct obj *contents;
	    for(contents=obj->cobj; contents; contents=contents->nobj)
		obj_no_longer_held(contents);
	}
	switch(obj->otyp) {
	case CRYSKNIFE:
	    /* KMH -- Fixed crysknives have only 10% chance of reverting */
	    /* only changes when not held by player or monster */
	    if (!obj->oerodeproof || !rn2(10)) {
		obj->otyp = WORM_TOOTH;
		obj->oerodeproof = 0;
	    }
	    break;
	}
}

/* 'D' command: drop several things */
int
doddrop()
{
	int result = 0;

	add_valid_menu_class(0); /* clear any classes already there */
	if (*u.ushops) sellobj_state(SELL_DELIBERATE);
	if (flags.menu_style != MENU_TRADITIONAL ||
		(result = ggetobj("drop", drop, 0, FALSE, (unsigned *)0)) < -1)
	    result = menu_drop(result);
	if (*u.ushops) sellobj_state(SELL_NORMAL);
	reset_occupations();

	return result;
}

/* Drop things from the hero's inventory, using a menu. */
STATIC_OVL int
menu_drop(retry)
int retry;
{
    int n, i, n_dropped = 0;
    long cnt;
    struct obj *otmp, *otmp2;
#ifndef GOLDOBJ
    struct obj *u_gold = 0;
#endif
    menu_item *pick_list;
    boolean all_categories = TRUE;
    boolean drop_everything = FALSE;

#ifndef GOLDOBJ
    if (u.ugold) {
	/* Hack: gold is not in the inventory, so make a gold object
	   and put it at the head of the inventory list. */
	u_gold = mkgoldobj(u.ugold);	/* removes from u.ugold */
	u_gold->in_use = TRUE;
	u.ugold = u_gold->quan;		/* put the gold back */
	assigninvlet(u_gold);		/* might end up as NOINVSYM */
	u_gold->nobj = invent;
	invent = u_gold;
    }
#endif
    if (retry) {
	all_categories = (retry == -2);
    } else if (flags.menu_style == MENU_FULL) {
	all_categories = FALSE;
	n = query_category("Drop what type of items?",
			invent,
			UNPAID_TYPES | ALL_TYPES | CHOOSE_ALL |
			BUC_BLESSED | BUC_CURSED | BUC_UNCURSED | BUC_UNKNOWN,
			&pick_list, PICK_ANY);
	if (!n) goto drop_done;
	for (i = 0; i < n; i++) {
	    if (pick_list[i].item.a_int == ALL_TYPES_SELECTED)
		all_categories = TRUE;
	    else if (pick_list[i].item.a_int == 'A')
		drop_everything = TRUE;
	    else
		add_valid_menu_class(pick_list[i].item.a_int);
	}
	free((genericptr_t) pick_list);
    } else if (flags.menu_style == MENU_COMBINATION) {
	unsigned ggoresults = 0;
	all_categories = FALSE;
	/* Gather valid classes via traditional NetHack method */
	i = ggetobj("drop", drop, 0, TRUE, &ggoresults);
	if (i == -2) all_categories = TRUE;
	if (ggoresults & ALL_FINISHED) {
		n_dropped = i;
		goto drop_done;
	}
    }

    if (drop_everything) {
	for(otmp = invent; otmp; otmp = otmp2) {
	    otmp2 = otmp->nobj;
	    n_dropped += drop(otmp);
	}
    } else {
	/* should coordinate with perm invent, maybe not show worn items */
	n = query_objlist("What would you like to drop?", invent,
			USE_INVLET|INVORDER_SORT, &pick_list,
			PICK_ANY, all_categories ? allow_all : allow_category);
	if (n > 0) {
	    for (i = 0; i < n; i++) {
		otmp = pick_list[i].item.a_obj;
		cnt = pick_list[i].count;
		if (cnt < otmp->quan) {
		    if (welded(otmp)) {
			;	/* don't split */
		    } else if (otmp->otyp == LOADSTONE && otmp->cursed) {
			/* same kludge as getobj(), for canletgo()'s use */
			otmp->corpsenm = (int) cnt;	/* don't split */
		    } else {
#ifndef GOLDOBJ
			if (otmp->oclass == COIN_CLASS)
			    (void) splitobj(otmp, otmp->quan - cnt);
			else
#endif
			    otmp = splitobj(otmp, cnt);
		    }
		}
		n_dropped += drop(otmp);
	    }
	    free((genericptr_t) pick_list);
	}
    }

 drop_done:
#ifndef GOLDOBJ
    if (u_gold && invent && invent->oclass == COIN_CLASS) {
	/* didn't drop [all of] it */
	u_gold = invent;
	invent = u_gold->nobj;
	u_gold->in_use = FALSE;
	dealloc_obj(u_gold);
	update_inventory();
    }
#endif
    return n_dropped;
}

#endif /* OVL0 */
#ifdef OVL2

/* on a ladder, used in goto_level */
static NEARDATA boolean at_ladder = FALSE;

int
dodown()
{
	struct trap *trap = 0;
	boolean stairs_down = ((u.ux == xdnstair && u.uy == ydnstair) ||
		    (u.ux == sstairs.sx && u.uy == sstairs.sy && !sstairs.up)),
		ladder_down = (u.ux == xdnladder && u.uy == ydnladder);

        /* these might not be set if arriving here indirectly */
        u.dx = u.dy = 0;
        u.dz = 1;

#ifdef STEED
	if (u.usteed && !u.usteed->mcanmove) {
		pline("%s won't move!", Monnam(u.usteed));
		return(0);
	} else if (u.usteed && u.usteed->meating) {
		pline("%s is still eating.", Monnam(u.usteed));
		return(0);
	} else
#endif
	if (Levitation) {
	    if ((HLevitation & I_SPECIAL) || (ELevitation & W_ARTI)) {
		/* end controlled levitation */
		if (ELevitation & W_ARTI) {
		    struct obj *obj;

		    for(obj = invent; obj; obj = obj->nobj) {
			if (obj->oartifact &&
					artifact_has_invprop(obj,LEVITATION)) {
			    if (obj->age < monstermoves)
				obj->age = monstermoves + rnz(100);
			    else
				obj->age += rnz(100);
			}
		    }
		}
		if (float_down(I_SPECIAL|TIMEOUT, W_ARTI))
		    return (1);   /* came down, so moved */
	    }
	    floating_above(stairs_down ? "stairs" : ladder_down ?
			   "ladder" : surface(u.ux, u.uy));
	    return (0);   /* didn't move */
	}
	if (!stairs_down && !ladder_down) {
		if (!(trap = t_at(u.ux,u.uy)) ||
			(trap->ttyp != TRAPDOOR && trap->ttyp != HOLE)
			|| !Can_fall_thru(&u.uz) || !trap->tseen) {

                        if (trap && (trap->ttyp == PIT ||
                                     trap->ttyp == SPIKED_PIT) &&
                            !u.utrap) {
                                /* deliberately entering the pit */
                                dotrap(trap, FORCEBUNGLE);
                                return(1);
                        }

			if (flags.autodig && !flags.nopick &&
				uwep && is_pick(uwep)) {
				return use_pick_axe2(uwep);
			} else {
				You_cant("go down here.");
				return(0);
			}
		}
	}
	if(u.ustuck) {
		You("are %s, and cannot go down.",
			!u.uswallow ? "being held" : is_animal(u.ustuck->data) ?
			"swallowed" : "engulfed");
		return(1);
	}
	if (on_level(&valley_level, &u.uz) && !u.uevent.gehennom_entered) {
		You("are standing at the gate to Gehennom.");
		pline("Unspeakable cruelty and harm lurk down there.");
		if (yn("Are you sure you want to enter?") != 'y')
			return(0);
		else pline("So be it.");
		u.uevent.gehennom_entered = 1;	/* don't ask again */
	}

	if(!next_to_u()) {
		You("are held back by your pet!");
		return(0);
	}

	if (trap)
	    You("%s %s.", locomotion(youmonst.data, "jump"),
		trap->ttyp == HOLE ? "down the hole" : "through the trap door");

	if (trap && Is_stronghold(&u.uz)) {
		goto_hell(FALSE, TRUE);
	} else {
		at_ladder = (boolean) (levl[u.ux][u.uy].typ == LADDER);
		next_level(!trap);
		at_ladder = FALSE;
	}
	return(1);
}

int
doup()
{
	if( (u.ux != xupstair || u.uy != yupstair)
	     && (!xupladder || u.ux != xupladder || u.uy != yupladder)
	     && (!sstairs.sx || u.ux != sstairs.sx || u.uy != sstairs.sy
			|| !sstairs.up)
	  ) {
		You_cant("go up here.");
		return(0);
	}
#ifdef STEED
	if (u.usteed && !u.usteed->mcanmove) {
		pline("%s won't move!", Monnam(u.usteed));
		return(0);
	} else if (u.usteed && u.usteed->meating) {
		pline("%s is still eating.", Monnam(u.usteed));
		return(0);
	} else
#endif
	if(u.ustuck) {
		You("are %s, and cannot go up.",
			!u.uswallow ? "being held" : is_animal(u.ustuck->data) ?
			"swallowed" : "engulfed");
		return(1);
	}
	if(near_capacity() > SLT_ENCUMBER) {
		/* No levitation check; inv_weight() already allows for it */
		Your("load is too heavy to climb the %s.",
			levl[u.ux][u.uy].typ == STAIRS ? "stairs" : "ladder");
		return(1);
	}
	if(ledger_no(&u.uz) == 1) {
		if (yn("Beware, there will be no return! Still climb?") != 'y')
			return(0);
	}
	if(!next_to_u()) {
		You("are held back by your pet!");
		return(0);
	}
	at_ladder = (boolean) (levl[u.ux][u.uy].typ == LADDER);
	prev_level(TRUE);
	at_ladder = FALSE;
	return(1);
}

/* Perform a context-sensitive command on the local terrain:
   upstairs           doup()
   downstairs         dodown()
   water/fountain     dodip()
   altar              dosacrifice()
   pit/hole/etc       dodown()
   autodig with pick  dodown(), which autodigs
   throne/floor       dosit()
   We assume even unseen traps are interacted with, because the
   player is putting a lot of pressure on the ground to perform the
   action (dosit) they'd otherwise perform.
*/
int
doterrain()
{
	struct trap *ttmp = t_at(u.ux, u.uy);
	if (Levitation) return dodown(); /* end levitation or error out */
        if (ttmp && (ttmp->ttyp == TRAPDOOR || ttmp->ttyp == HOLE ||
                     ttmp->ttyp == PIT || ttmp->ttyp == SPIKED_PIT) &&
            !u.utrap)
          return dodown();
	switch (levl[u.ux][u.uy].typ) {
          /* 0..15: impassable terrain, use default #sit */
        case POOL:
        case MOAT:
        case WATER:
        case FOUNTAIN:
          setnextdodipinto(&zeroobj);
          return dodip();
          /* DRAWBRIDGE_UP: impassable */
        case LAVAPOOL:
          pline("It seems a little unwise to mess with the lava.");
          return 0;
          /* IRONBARS: impassable */
        case DOOR:
          pline("You can't do much with a door while standing in the doorway.");
          return 0;
        case CORR:
        case ROOM:
        case ICE:
          if (flags.autodig && !flags.nopick &&
              uwep && is_pick(uwep)) return dodown();
          /* otherwise fall through */
        case THRONE:
        case DRAWBRIDGE_DOWN:
          return dosit();
        case STAIRS:
        case LADDER:
          /* If you can't go up, go down. */
          if(    (u.ux != xupstair || u.uy != yupstair)
              && (!xupladder || u.ux != xupladder || u.uy != yupladder)
              && (!sstairs.sx || u.ux != sstairs.sx || u.uy != sstairs.sy
			|| !sstairs.up)) return dodown();
          else return doup();
        case SINK:
          pline("Use %s then , to quaff from the sink.", key_for_cmd("#quaff"));
          return 0;
        case GRAVE:
          pline("Use %s with a digging tool to dig up the grave.",
                key_for_cmd("#apply"));
        case ALTAR:
          return dosacrifice();
        case AIR:
        case CLOUD:
          pline("You can't see an obvious way to move up or down here.");
          return 0;
        default: /* impassable terrain */
          return dosit();
        }
}

d_level save_dlevel = {0, 0};

/* check that we can write out the current level */
STATIC_OVL int
currentlevel_rewrite()
{
	register int fd;
	char whynot[BUFSZ];

	/* since level change might be a bit slow, flush any buffered screen
	 *  output (like "you fall through a trap door") */
	mark_synch();

	fd = create_levelfile(ledger_no(&u.uz), whynot);
	if (fd < 0) {
		/*
		 * This is not quite impossible: e.g., we may have
		 * exceeded our quota. If that is the case then we
		 * cannot leave this level, and cannot save either.
		 * Another possibility is that the directory was not
		 * writable.
		 */
		pline("%s", whynot);
		return -1;
	}

#ifdef MFLOPPY
	if (!savelev(fd, ledger_no(&u.uz), COUNT_SAVE)) {
		(void) close(fd);
		delete_levelfile(ledger_no(&u.uz));
		pline("NetHack is out of disk space for making levels!");
		You("can save, quit, or continue playing.");
		return -1;
	}
#endif
	return fd;
}

/* Checkpointing a level is useful in both single-player and
   multiplayer, for allowing recovery as well as communicating the
   current gamestate. Uncheckpointing a level, though, is an operation
   mostly only meaningful in multiplayer, as there's no other way it
   could have changed behind our back. These both return negative if
   the operation fails due to I/O issues, in which case multiplayer
   can't do much but give the player the opportunity to abandon the
   game or try again. No messages are produced with a normal return;
   an error message will accompany an error return.

   Calling checkpoint_level then uncheckpoint_level is idempotent, if
   nothing changes the level files in between. (There probably will
   be changes to the level files in between, though; that's the
   whole purpose of uncheckpointing, to reflect changes made by
   other processes.)

   In order to mark our current location in the checkpoint files,
   we use player-monsters, flagged as tame, and named by the game's
   multiplayer lockname. (For instance, "tame valkyrie named
   1000ais523".) These are removed upon uncheckpointing by
   getlev().

   Note that these routines both mess around with monster pointers, so
   checkpointing will invalidate any pointers to monsters you might
   happen to have around. (Uncheckpointing also messes up pointers to
   everything else on the level.) Thus, be careful using it.
*/
int
checkpoint_level()
{
  int fd;
  struct monst *mtmp;

  /* TODO: In singleplayer mode, don't create the placeholder creature
     on our square. (The problem is that we can't use iflags.multiplayer
     for the purpose, because we might be in the process of entering
     multiplayer mode.) */
  in_mklev = TRUE; /* turn off normal sanity checks,
                      we're about to do something insane */
  mtmp = makemon(&mons[u.umonster], u.ux, u.uy, /* i.e. original form */
                 NO_MINVENT | MM_NOCOUNTBIRTH | MM_IGNOREWATER | MM_EDOG);
  in_mklev = FALSE;
  if (!mtmp) {
    /* Self-genocide while polymorphed is the most plausible reason to
       get here, in which case, there's not much we can do to
       continue. So just die in spectacular fashion, by having a
       monster genocide the player's current form too. Ideally, this
       will also eventually happen upon one player genociding
       another. (That is, if we don't disable genocides on @ altogether
       in multiplayer to prevent fatal logic issues.) */
    pline("You hear the sound of a genocide approaching in the distance.");
    pline("Wiped out all %s.", youmonst.data->mname);
    killer_format = KILLED_BY_AN;
    killer = "distant genocide";
    done(GENOCIDED);
    pline("Unfortunately, you are still genocided...");
    done(GENOCIDED);
    panic("player did not die even after being genocided three times");
  }

  mtmp->female = poly_gender();
  initedog(mtmp); /* mark as tame */
  mtmp->mtame = 127;
  mtmp->mlstmv = monstermoves; /* indicate what this level's "local time" is */
  /* Setting the movement value is very important, so that the other
     games know whether they should yield to us this turn. */
  mtmp->movement = youmonst.movement;
  christen_monst(mtmp, mplock);

  /* In multiplayer, lock the file that we're checkpointing, so that
     we don't have someone trying to merge with the level due to a
     level change reading a partial level file and crashing */
  if (iflags.multiplayer) {
    Strcpy(lock, iflags.mp_lock_name);
    set_levelfile_name(lock, ledger_no(&u.uz));
    if (!lock_file(lock, LEVELPREFIX, 20))
      panic("Stale lockfile lock when checkpointing level!");
  }

  fd = currentlevel_rewrite();

  if (iflags.multiplayer && fd < 0) {
    /* lock may have changed value by now */
    Strcpy(lock, iflags.mp_lock_name);
    set_levelfile_name(lock, ledger_no(&u.uz));
    unlock_file(lock);
  } /* otherwise keep holding the lock */
  if(fd < 0) return fd;

  bufon(fd);

  vision_reset();

  savelev(fd, ledger_no(&u.uz), WRITE_SAVE);

  bclose(fd);

  if (iflags.multiplayer) {
    /* lock may have changed value by now */
    Strcpy(lock, iflags.mp_lock_name);
    set_levelfile_name(lock, ledger_no(&u.uz));
    unlock_file(lock);
  }

  /* we cannot use the old pointer here, it's no longer valid */
  mongone(m_at(u.ux, u.uy));
  dmonsfree();

  /* We uncheckpoint as part of checkpointing, in order to handle
     things like light sources being renamed. */
  if (iflags.multiplayer) return uncheckpoint_level();
  else return 0; /* successful */
}

int
uncheckpoint_level()
{
  int fd;
  char whynot[BUFSZ];

  /* The lock is required, in case we try to uncheckpoint the file while
     another process is checkpointing the same file (possible due to
     vision updates). */
  if (iflags.multiplayer) {
    Strcpy(lock, iflags.mp_lock_name);
    set_levelfile_name(lock, ledger_no(&u.uz));
    if (!lock_file(lock, LEVELPREFIX, 20))
      panic("Stale lockfile lock when uncheckpointing level!");
  }

  fd = open_levelfile(ledger_no(&u.uz), whynot);
  if (fd < 0) {
    if (iflags.multiplayer) {
      /* lock may have changed value by now */
      Strcpy(lock, iflags.mp_lock_name);
      set_levelfile_name(lock, ledger_no(&u.uz));
      unlock_file(lock);
    }
    pline("%s", whynot);
    return fd;
  }
  minit(); /* needed to reset compression status */

  /* free the old version of the level */
  dmonsfree();
  savelev(0, ledger_no(&u.uz), FREE_SAVE);
  /* The 0 here indicates that there shouldn't be a trickery if the
     lockfile has the wrong PID. Of course, in multiplayer, it'll have
     the other game's PID. */
  getlev(fd, 0, ledger_no(&u.uz), FALSE);
  (void) close(fd);

  if (iflags.multiplayer) {
    /* lock may have changed value by now */
    Strcpy(lock, iflags.mp_lock_name);
    set_levelfile_name(lock, ledger_no(&u.uz));
    unlock_file(lock);
  }

  /* Genocide should extend from one game onto the next. Multiplayer
     reveals its true horror: instead of just conquering the whole
     world instantly, it places a permanent curse on its user that
     wipes out any of the sort of monster in question that they end
     up sharing a level with. */
  kill_genocided_monsters();

  /* Redraw the screen to reflect the new information. */
  vision_reset();
  docrt();
  flush_screen(1);  

  return 0; /* successful */
}

/* Looks at the current level, to deduce what its local time is.  We
   rely on the facts that a) nothing on the level can be dated in the
   future, and b) any other players on the level are flagged with
   their current times, so that if there are other players there, the
   times will line up exactly (to within a turn, anyway). */
static long
calculate_objchain_local_time(ochain)
struct obj *ochain;
{
  struct obj *otmp;
  long localtime = 0L;
  for (otmp = ochain; otmp; otmp = otmp->nobj) {
    if (age_is_relative(otmp)) continue;
    if (otmp->age > localtime) localtime = otmp->age;
    /* Age has a different meaning inside ice boxes. */
    if (Has_contents(otmp) && otmp->otyp != ICE_BOX) {
      long t = calculate_objchain_local_time(otmp->cobj);
      if (t > localtime) localtime = t;
    }
  }
  return localtime;
}
static long
calculate_level_local_time()
{
  struct monst *mtmp;
  long localtime = -1L;
  long t;

  for (mtmp = fmon; mtmp; mtmp = mtmp->nmon) {
    if (DEADMONSTER(mtmp)) continue;
    if (mtmp->mlstmv > localtime) localtime = mtmp->mlstmv;
    if (mtmp->minvent) {
      t = calculate_objchain_local_time(mtmp->minvent);
      if (t > localtime) localtime = t;
    }
    if (mtmp->mw && mtmp->mw->age > localtime) localtime = mtmp->mw->age;
  }
  t = calculate_objchain_local_time(fobj);
  if (t > localtime) localtime = t;
  t = calculate_objchain_local_time(level.buriedobjlist, FALSE);
  if (t > localtime) localtime = t;
  if (localtime == -1) {
    /* no monsters or items on the level; this is rather unlikely, but
       theoretically possible, and in this case we can choose the time
       we like for the level's local time, so set it to our own local
       time */
    localtime = monstermoves;
  }
  return localtime;
}

static void
time_dilate_objchain(ochain, dtime)
struct obj *ochain;
long dtime;
{
  struct obj *otmp;
  for (otmp = ochain; otmp; otmp = otmp->nobj) {
    if (age_is_relative(otmp)) continue;
    otmp->age += dtime;
    /* Age has a different meaning inside ice boxes. */
    if (Has_contents(otmp) && otmp->otyp != ICE_BOX)
      time_dilate_objchain(otmp->cobj, dtime);
  }
}
static void
time_dilate_monchain(mchain, dtime)
struct monst *mchain;
long dtime;
{
  struct monst *mtmp;
  for (mtmp = mchain; mtmp; mtmp = mtmp->nmon) {
    if (DEADMONSTER(mtmp)) continue;
    mtmp->mlstmv += dtime;
    if (mtmp->minvent) time_dilate_objchain(mtmp->minvent, dtime);
    /* Assume monsters don't wield containers. */
    if (mtmp->mw && !age_is_relative(mtmp->mw)) mtmp->mw->age += dtime;
    /* migrating pets need special treatment */
    if (mtmp->mtame && !mtmp->isminion) {
      EDOG(mtmp)->droptime += dtime;
      EDOG(mtmp)->whistletime += dtime;
      EDOG(mtmp)->hungrytime += dtime;
    }
  }
}

/* Adjusts all timestamps saved in lockfile 0 by the given amount. */
static void
time_dilation(dtime)
long dtime;
{
    /* nothing relevant in flags */
    /* timestamps in struct you */
    if (u.ucleansed) u.ucleansed += dtime;
    if (u.usleep) u.usleep += dtime;
    /* timers */
    adjust_nonlocal_timers(dtime);
    /* nothing relevant in light sources */
    /* inventory */
    time_dilate_objchain(invent, dtime);
    /* migration */
    time_dilate_objchain(migrating_objs, dtime);
    time_dilate_monchain(migrating_mons, dtime);
    time_dilate_monchain(mydogs, dtime);
    /* nothing relevant in monster statistics */
    /* nothing relevant in dungeons or level chains */
    /* time handling; moves is /not/ updated, that's its whole purpose */
    monstermoves += dtime;
    /* nothing relevant in quest status */
    /* nothing relevant in spells (perhaps surprisingly, spell
       knowledge is relative not absolute) */
    /* nothing relevant in artifacts */
    /* nothing relevant in oracularities */
    /* stuck/steed are not multiplayer-safe, so needn't be adjusted */
    /* nothing relevant in tutorial flags (TODO: the tutorial is currently
       based on local time, when it should be based on character time,
       but we can't fix that here) */
    /* nothing relevant in character name */
    /* nothing relevant in fruitnames */
    /* nothing relevant in Plane of Water status */
}

#ifdef INSURANCE
void
save_currentstate()
{
	if (flags.ins_chkpt) {
		/* write out just-attained level, with pets and everything */
		if(checkpoint_level() < 0) return;
	}

	/* write out non-level state */
	savestateinlock();
}
#endif

/*
static boolean
badspot(x, y)
register xchar x, y;
{
	return((levl[x][y].typ != ROOM && levl[x][y].typ != AIR &&
			 levl[x][y].typ != CORR) || MON_AT(x, y));
}
*/

void
goto_level(newlevel, at_stairs, falling, portal)
d_level *newlevel;
boolean at_stairs, falling, portal;
{
	int fd, l_idx;
	xchar new_ledger;
	boolean cant_go_back,
		up = (depth(newlevel) < depth(&u.uz)),
		newdungeon = (u.uz.dnum != newlevel->dnum),
		was_in_W_tower = In_W_tower(u.ux, u.uy, &u.uz),
		familiar = FALSE;
	boolean new = FALSE;	/* made a new level? */
        boolean level_file_already_loaded = FALSE;
	struct monst *mtmp;
	char whynot[BUFSZ];
        char yield_after[BUFSZ] = "";

	if (dunlev(newlevel) > dunlevs_in_dungeon(newlevel))
		newlevel->dlevel = dunlevs_in_dungeon(newlevel);
	if (newdungeon && In_endgame(newlevel)) { /* 1st Endgame Level !!! */
		if (u.uhave.amulet)
		    assign_level(newlevel, &earth_level);
		else return;
	}
	new_ledger = ledger_no(newlevel);
	if (new_ledger <= 0)
		done(ESCAPED);	/* in fact < 0 is impossible */

	/* If you have the amulet and are trying to get out of Gehennom, going
	 * up a set of stairs sometimes does some very strange things!
	 * Biased against law and towards chaos, but not nearly as strongly
	 * as it used to be (prior to 3.2.0).
	 * Odds:	    old				    new
	 *	"up"    L      N      C		"up"    L      N      C
	 *	 +1   75.0   75.0   75.0	 +1   75.0   75.0   75.0
	 *	  0    0.0   12.5   25.0	  0    6.25   8.33  12.5
	 *	 -1    8.33   4.17   0.0	 -1    6.25   8.33  12.5
	 *	 -2    8.33   4.17   0.0	 -2    6.25   8.33   0.0
	 *	 -3    8.33   4.17   0.0	 -3    6.25   0.0    0.0
	 */
	if (Inhell && up && u.uhave.amulet && !newdungeon && !portal &&
				(dunlev(&u.uz) < dunlevs_in_dungeon(&u.uz)-3)) {
		if (!rn2(4)) {
		    int odds = 3 + (int)u.ualign.type,		/* 2..4 */
			diff = odds <= 1 ? 0 : rn2(odds);	/* paranoia */

		    if (diff != 0) {
			assign_rnd_level(newlevel, &u.uz, diff);
			/* if inside the tower, stay inside */
			if (was_in_W_tower &&
			    !On_W_tower_level(newlevel)) diff = 0;
		    }
		    if (diff == 0)
			assign_level(newlevel, &u.uz);

		    new_ledger = ledger_no(newlevel);

		    pline("A mysterious force momentarily surrounds you...");
		    if (on_level(newlevel, &u.uz)) {
			(void) safe_teleds(FALSE);
			(void) next_to_u();
			return;
		    } else
			at_stairs = at_ladder = FALSE;
		}
	}

	/* Prevent the player from going past the first quest level unless
	 * (s)he has been given the go-ahead by the leader.
	 */
	if (on_level(&u.uz, &qstart_level) && !newdungeon && !ok_to_quest()) {
		pline("A mysterious force prevents you from descending.");
		return;
	}

	if (on_level(newlevel, &u.uz)) return;		/* this can happen */

        /* Lock the level we're leaving, in order to avoid someone arriving
           on the level while it's being rewritten. */
        if (iflags.multiplayer) {
          Strcpy(lock, iflags.mp_lock_name);
          set_levelfile_name(lock, ledger_no(&u.uz));
          if (!lock_file(lock, LEVELPREFIX, 20)) {
            /* This shouldn't happen. */
            panic("Stale lockfile lock when leaving level!");
          }
        }

	fd = currentlevel_rewrite();
	if (fd < 0) {
          if (iflags.multiplayer) {
            Strcpy(lock, iflags.mp_lock_name);
            set_levelfile_name(lock, ledger_no(&u.uz));
            unlock_file(lock);
          }
          return;
        }

	if (falling) /* assuming this is only trap door or hole */
	    impact_drop((struct obj *)0, u.ux, u.uy, newlevel->dlevel);

	check_special_room(TRUE);		/* probably was a trap door */
	if (Punished) unplacebc();
	u.utrap = 0;				/* needed in level_tele */
	fill_pit(u.ux, u.uy);
	u.ustuck = 0;				/* idem */
	u.uinwater = 0;
	u.uundetected = 0;	/* not hidden, even if means are available */
	keepdogs(FALSE);
	if (u.uswallow)				/* idem */
		u.uswldtim = u.uswallow = 0;
	recalc_mapseen(); /* recalculate map overview before we leave the level */
	/*
	 *  We no longer see anything on the level.  Make sure that this
	 *  follows u.uswallow set to null since uswallow overrides all
	 *  normal vision.
	 */
	vision_recalc(2);

        /*
         * If leaving a level which has another player on it, yield to
         * them, while continuing to execute. This increases the number
         * of drivers by one.
         */
        rYou(" left the level.");
        if (iflags.multiplayer) {
          for (mtmp = fmon; mtmp; mtmp = mtmp->nmon) {
            if (is_mp_player(mtmp)) break;
          }
          if (mtmp) Strcpy(yield_after, is_mp_player(mtmp));
        }

	/*
	 * Save the level we're leaving.  If we're entering the endgame,
	 * we can get rid of all existing levels because they cannot be
	 * reached any more.  We still need to use savelev()'s cleanup
	 * for the level being left, to recover dynamic memory in use and
	 * to avoid dangling timers and light sources.
         *
         * In multiplayer, we don't delete existing levels in case there
         * are still players on them. They'll eventually get deleted upon
         * death or ascension of the last remaining player.
	 */
	cant_go_back = (newdungeon && In_endgame(newlevel) &&
                        !iflags.multiplayer);
	if (!cant_go_back) {
	    update_mlstmv();	/* current monsters are becoming inactive */
	    bufon(fd);		/* use buffered output */
	}
	savelev(fd, ledger_no(&u.uz),
		cant_go_back ? FREE_SAVE : (WRITE_SAVE | FREE_SAVE));
	bclose(fd);
	if (cant_go_back) {
	    /* discard unreachable levels; keep #0 */
	    for (l_idx = maxledgerno(); l_idx > 0; --l_idx)
		delete_levelfile(l_idx);
	}

	assign_level(&u.uz0, &u.uz);
	assign_level(&u.uz, newlevel);
	assign_level(&u.utolev, newlevel);
	u.utotype = 0;

        /* In multiplayer, we need careful synchronization handling
           between levels. When someone leaves a level, if anyone else
           is there, one of them should be yielded to, desynchronizing
           the two parties; that bit is easy. The tricky part is
           resynchronizing on arrival on a level; we should ideally
           just yield without telling anyone to continue, which would
           be fine, except then nobody would know we existed. So we
           have to place ourselves on the level so people know to
           yield to us, and that can only be done if someone yields
           to us so we can place ourselves there.

           Just to make things more fun, if nobody else is on the
           level we arrive on, we should just continue by loading it.
           But we shouldn't load it if someone else is there in case
           they're halfway through checkpointing it, and we can only
           find that out by loading it...

           This dilemma can only be resolved by, um, locking the
           lockfiles. This is arguably silly, but is the only solution
           I can see to the problem.
        */

        if (iflags.multiplayer) {

          char mergebuf[3];
          const char *levelmerger;

          /* First, unlock the old level file. */
          Strcpy(lock, iflags.mp_lock_name);
          set_levelfile_name(lock, ledger_no(&u.uz0));
          unlock_file(lock);

          if (*yield_after) {
            /* Must happen after we've vacated the level; we already
               saved (rather than checkpointing) the level file above,
               so it now doesn't have us on it */
            char yield_message[BUFSZ];
            Sprintf(yield_message, "%s y\n", mplock);
            mp_message(yield_after, yield_message);
            *yield_after = 0;
          }

          /* This might be rather time-consuming, so we show a message on
             a blank screen to make it clear that the program hasn't hung */
          cls();
          pline("Communicating...");
          suppress_more();

          /* Avoid deadlocks involving levels with multiple people on
             each trying to change levels simultaneously */
          for (levelmerger = mp_levelmerger_name(); levelmerger;
               levelmerger = mp_levelmerger_name())
            mp_message(levelmerger, "!\n");

          Strcpy(lock, iflags.mp_lock_name);
          set_levelfile_name(lock, new_ledger);
          if (!lock_file(lock, LEVELPREFIX, 20)) {
            /* This shouldn't happen. TODO: This is probably
               recoverable anyway, but just panic for now. */
            panic("Stale lockfile lock in level changes!");
          }
          fd = open_levelfile(new_ledger, whynot);
          /* We need to check that the levelfile is not an empty file
             in case two processes tried to lock the same nonexistent
             file simultaneously. Only one will get the lock, but the
             other might erroneously think it exists. */
          if (fd >= 0 && read(fd, mergebuf, 1) >= 1) {
            /* It's entirely possible that someone created the level,
               but we didn't previously know it existed. We do now! */
            level_info[new_ledger].flags |= LFILE_EXISTS;
            close(fd);
            /* We need to find someone else on the level, and contact
               them asking for a yield. It's so frustrating, to load
               the level we want to arrive on and have to throw it
               away again, but necessary so that there's a consistent
               version of events. This may potentially need to be done
               multiple times, if the people we contact keep leaving
               the level before they can process the command (which is
               unlikely but possible). If nobody else is on the level,
               we take ownership of it, and this is similar to the
               case where the level file didn't exist; the lock will
               ensure nobody else tries to do the same thing.
            */            
            for (;;) {
              fd = open_levelfile(new_ledger, whynot);
              getlev(fd, 0, new_ledger, FALSE);
              (void) close(fd);

              for (mtmp = fmon; mtmp; mtmp = mtmp->nmon) {
                if (is_mp_player(mtmp)) break;
              }
              
              if (mtmp) {
                /* Yield the lock, this operation is timeconsuming,
                   and we want people to be able to change the locked
                   lockfile anyway while we wait */
                Strcpy(lock, iflags.mp_lock_name);
                set_levelfile_name(lock, new_ledger);
                unlock_file(lock);

                /* Encode the level we want to arrive on */
                mergebuf[2] = 0;
                mergebuf[0] = newlevel->dnum + 11;
                mergebuf[1] = newlevel->dlevel + 11;

                if (mp_message_and_reply(
                      is_mp_player(mtmp), "g", mergebuf, TRUE)) {
                  /* OK acknowledgement; we can safely add ourselves
                     to the map, so long as we yield afterwards */
                  Strcpy(yield_after, is_mp_player(mtmp));
                  savelev(0, new_ledger, FREE_SAVE);
                  Strcpy(lock, iflags.mp_lock_name);
                  set_levelfile_name(lock, new_ledger);
                  /* The lock could fail to lock first time if someone
                     else is trying to join the level in the meantime.
                     This is OK, though; they'll ask one of the players
                     on the level for a merge, then unlock the file
                     again, and that player won't reply until we're
                     done because they're waiting on our yield */
                  if (!lock_file(lock, LEVELPREFIX, 20))
                    panic("Stale lockfile lock in level merge!");
                  break;
                } else {
                  /* error acknowledgement; wait a second (to avoid a
                     tight loop if there are 11 or more people trying
                     to all enter the same level at once), then relock
                     and try again */
                  sleep(1);
                  if (!lock_file(lock, LEVELPREFIX, 20))
                    panic("Stale lockfile lock in level change retry!");
                  continue;
                }
              } else {
                /* maintain the lock and take ownership */
                level_file_already_loaded = TRUE;
                break;
              }
            }
          } /* else maintain the lock and take ownership... */
        }

#ifdef REINCARNATION
	if (Is_rogue_level(newlevel) || Is_rogue_level(&u.uz0))
		assign_rogue_graphics(Is_rogue_level(newlevel));
#endif
#ifdef USE_TILES
	substitute_tiles(newlevel);
#endif
	/* record this level transition as a potential seen branch unless using
	 * some non-standard means of transportation (level teleport).
	 */
	if ((at_stairs || falling || portal) && (u.uz0.dnum != newlevel->dnum))
		recbranch_mapseen(&u.uz0, newlevel);
	if (dunlev_reached(&u.uz) < dunlev(&u.uz))
		dunlev_reached(&u.uz) = dunlev(&u.uz);
	reset_rndmonst(NON_PM);   /* u.uz change affects monster generation */

	/* set default level change destination areas */
	/* the special level code may override these */
	(void) memset((genericptr_t) &updest, 0, sizeof updest);
	(void) memset((genericptr_t) &dndest, 0, sizeof dndest);

	if (!(level_info[new_ledger].flags & LFILE_EXISTS)) {
		/* entering this level for first time; make it now */
        remake_level:
		if (level_info[new_ledger].flags & (FORGOTTEN|VISITED)) {
		    impossible("goto_level: returning to discarded level?");
		    level_info[new_ledger].flags &= ~(FORGOTTEN|VISITED);
		}
		mklev();
		new = TRUE;	/* made the level */
	} else if (!level_file_already_loaded) {
		/* returning to previously visited level; reload it */
		fd = open_levelfile(new_ledger, whynot);
		if (fd < 0) {
			pline("%s", whynot);
			pline("Probably someone removed it.");
                        pline("Attempting to continue, but it might not work...");
                        goto remake_level;
		}
		minit();	/* ZEROCOMP */
		getlev(fd, 0, new_ledger, FALSE);
		(void) close(fd);
	}

        if (iflags.multiplayer) {
          /* We must release the lock before anything that could print
             messages, to avoid panicking other processes; the error
             on level file removal above is OK, though, because it can
             only happen if someone's gone around deleting files while
             we have them locked, which would crash those processes
             anyway. */
          Strcpy(lock, iflags.mp_lock_name);
          set_levelfile_name(lock, new_ledger);
          unlock_file(lock);

          /* We also need to adjust for local time, before doing
             anything that might depend on timers. */
          time_dilation(calculate_level_local_time() - monstermoves);
        }

	/* do this prior to level-change pline messages */
	vision_reset();		/* clear old level's line-of-sight */
	vision_full_recalc = 0;	/* don't let that reenable vision yet */
	flush_screen(-1);	/* ensure all map flushes are postponed */

	if (portal && !In_endgame(&u.uz)) {
	    /* find the portal on the new level */
	    register struct trap *ttrap;

	    for (ttrap = ftrap; ttrap; ttrap = ttrap->ntrap)
		if (ttrap->ttyp == MAGIC_PORTAL) break;

	    if (!ttrap) panic("goto_level: no corresponding portal!");
	    seetrap(ttrap);
	    u_on_newpos(ttrap->tx, ttrap->ty);
	} else if (at_stairs && !In_endgame(&u.uz)) {
	    if (up) {
		if (at_ladder && !newdungeon) {
                  u_on_dnladder();
		} else {
		    if (newdungeon) {
			if (Is_stronghold(&u.uz)) {
			    register xchar x, y;

			    do {
				x = (COLNO - 2 - rnd(5));
				y = rn1(ROWNO - 4, 3);
			    } while(occupied(x, y) ||
				    IS_WALL(levl[x][y].typ));
			    u_on_newpos(x, y);
			} else u_on_sstairs();
		    } else u_on_dnstairs();
		}
		/* Remove bug which crashes with levitation/punishment  KAA */
		if (Punished && !Levitation) {
			pline("With great effort you climb the %s.",
				at_ladder ? "ladder" : "stairs");
		}
	    } else {	/* down */
		if (at_ladder && !newdungeon) {
                  u_on_upladder();
		} else {
		    if (newdungeon) u_on_sstairs();
		    else u_on_upstairs();
		}
		if (u.dz && Flying)
		    You("fly down along the %s.",
			at_ladder ? "ladder" : "stairs");
		else if (u.dz &&
		    (near_capacity() > UNENCUMBERED || Punished || Fumbling)) {
		    You("fall down the %s.", at_ladder ? "ladder" : "stairs");
		    if (Punished) {
			drag_down();
			if (carried(uball)) {
			    if (uwep == uball)
				setuwep((struct obj *)0);
			    if (uswapwep == uball)
				setuswapwep((struct obj *)0);
			    if (uquiver == uball)
				setuqwep((struct obj *)0);
			    freeinv(uball);
			}
		    }
#ifdef STEED
		    /* falling off steed has its own losehp() call */
		    if (u.usteed)
			dismount_steed(DISMOUNT_FELL);
		    else
#endif
			losehp(rnd(3), "falling downstairs", KILLED_BY);
		    selftouch("Falling, you");
		} else if (u.dz && at_ladder)
		    You("climb down the ladder.");
	    }
	} else {	/* trap door or level_tele or In_endgame */
	    if (was_in_W_tower && On_W_tower_level(&u.uz))
		/* Stay inside the Wizard's tower when feasible.	*/
		/* Note: up vs down doesn't really matter in this case. */
		place_lregion(dndest.nlx, dndest.nly,
				dndest.nhx, dndest.nhy,
				0,0, 0,0, LR_DOWNTELE, (d_level *) 0);
	    else if (up)
		place_lregion(updest.lx, updest.ly,
				updest.hx, updest.hy,
				updest.nlx, updest.nly,
				updest.nhx, updest.nhy,
				LR_UPTELE, (d_level *) 0);
	    else
		place_lregion(dndest.lx, dndest.ly,
				dndest.hx, dndest.hy,
				dndest.nlx, dndest.nly,
				dndest.nhx, dndest.nhy,
				LR_DOWNTELE, (d_level *) 0);
	    if (falling) {
		if (Punished) ballfall();
		selftouch("Falling, you");
	    }
	}

	if (Punished) placebc();
	obj_delivery();		/* before killing geno'd monsters' eggs */
	losedogs();
	kill_genocided_monsters();  /* for those wiped out while in limbo */
	/*
	 * Expire all timers that have gone off while away.  Must be
	 * after migrating monsters and objects are delivered
	 * (losedogs and obj_delivery).
	 */
	run_timers();

	initrack();

	if ((mtmp = m_at(u.ux, u.uy)) != 0
#ifdef STEED
		&& mtmp != u.usteed
#endif
		) {
	    /* There's a monster at your target destination; it might be one
	       which accompanied you--see mon_arrive(dogmove.c)--or perhaps
	       it was already here.  Randomly move you to an adjacent spot
	       or else the monster to any nearby location.  Prior to 3.3.0
	       the latter was done unconditionally. */
	    coord cc;

	    if (!rn2(2) &&
		    enexto(&cc, u.ux, u.uy, youmonst.data) &&
		    distu(cc.x, cc.y) <= 2)
		u_on_newpos(cc.x, cc.y);	/*[maybe give message here?]*/
	    else if (is_mp_player(mtmp) && enexto(&cc, u.ux, u.uy, youmonst.data))
                /* This can be a bit ridiculous, but is better than the
                   alternative (which always, rather than very rarely,
                   places two players on the same square, leading to
                   one of them getting genocided). No doubt someone
                   will come up with an ingenious way to abuse this
                   to phase through walls or something; it's easiest
                   to let them have their fun in that case. */
                u_on_newpos(cc.x, cc.y);
            else
		mnexto(mtmp);

	    if ((mtmp = m_at(u.ux, u.uy)) != 0) {
		impossible("mnexto failed (do.c)?");
		(void) rloc(mtmp, FALSE);
	    }
	}

	/* initial movement of bubbles just before vision_recalc. Don't
           do this in multiplayer, it causes too many issues. */
	if (Is_waterlevel(&u.uz) && !iflags.multiplayer)
		movebubbles();

	if (level_info[new_ledger].flags & FORGOTTEN) {
	    forget_map(ALL_MAP);	/* forget the map */
	    forget_traps();		/* forget all traps too */
	    familiar = TRUE;
	    level_info[new_ledger].flags &= ~FORGOTTEN;
	}

	/* Reset the screen. */
        display_nhwindow(WIN_MESSAGE, FALSE); /* messages on the old level */
	vision_reset();		/* reset the blockages */
	docrt();		/* does a full vision recalc */
	flush_screen(-1);

	/*
	 *  Move all plines beyond the screen reset.
	 */

	/* give room entrance message, if any */
	check_special_room(FALSE);

	/* Check whether we just entered Gehennom. */
	if (!In_hell(&u.uz0) && Inhell) {
	    if (Is_valley(&u.uz)) {
		You("arrive at the Valley of the Dead...");
		pline_The("odor of burnt flesh and decay pervades the air.");
#ifdef MICRO
		display_nhwindow(WIN_MESSAGE, FALSE);
#endif
		You_hear("groans and moans everywhere.");
	    } else pline("It is hot here.  You smell smoke...");
	}

	if (familiar) {
	    static const char * const fam_msgs[4] = {
		"You have a sense of deja vu.",
		"You feel like you've been here before.",
		"This place %s familiar...",
		0	/* no message */
	    };
	    static const char * const halu_fam_msgs[4] = {
		"Whoa!  Everything %s different.",
		"You are surrounded by twisty little passages, all alike.",
		"Gee, this %s like uncle Conan's place...",
		0	/* no message */
	    };
	    const char *mesg;
	    char buf[BUFSZ];
	    int which = rn2(4);

	    if (Hallucination)
		mesg = halu_fam_msgs[which];
	    else
		mesg = fam_msgs[which];
	    if (mesg && index(mesg, '%')) {
		Sprintf(buf, mesg, !Blind ? "looks" : "seems");
		mesg = buf;
	    }
	    if (mesg) pline("%s",mesg);
	}

#ifdef REINCARNATION
	if (new && Is_rogue_level(&u.uz))
	    You("enter what seems to be an older, more primitive world.");
#endif
	/* Final confrontation */
	if (In_endgame(&u.uz) && newdungeon && u.uhave.amulet)
		resurrect();
	if (newdungeon && In_V_tower(&u.uz) && In_hell(&u.uz0))
		pline_The("heat and smoke are gone.");

	/* the message from your quest leader */
	if (!In_quest(&u.uz0) && at_dgn_entrance("The Quest") &&
		!(u.uevent.qexpelled || u.uevent.qcompleted || quest_status.leader_is_dead)) {

		if (u.uevent.qcalled) {
			com_pager(Role_if(PM_ROGUE) ? 4 : 3);
		} else {
			com_pager(2);
			u.uevent.qcalled = TRUE;
		}
	}

	/* once Croesus is dead, his alarm doesn't work any more */
	if (Is_knox(&u.uz) && (new || !mvitals[PM_CROESUS].died)) {
		You("penetrated a high security area!");
		pline("An alarm sounds!");
		for(mtmp = fmon; mtmp; mtmp = mtmp->nmon)
		    if (!DEADMONSTER(mtmp) && mtmp->msleeping &&
                        !is_mp_player(mtmp)) mtmp->msleeping = 0;
	}

	if (on_level(&u.uz, &astral_level))
	    final_level();
	else
	    onquest();
	assign_level(&u.uz0, &u.uz); /* reset u.uz0 */

#ifdef INSURANCE
	save_currentstate();
#endif

	/* assume this will always return TRUE when changing level */
	(void) in_out_region(u.ux, u.uy);
	(void) pickup(1);

        if (*yield_after) { /* can only happen in multiplayer */
          char buf[BUFSZ];
          rYou(" arrives on this level.");
          /* TODO: Should we boost our own movement energy a bit upon
             doing this? We're meshing two incompatible time systems,
             and everyone else ends up a turn ahead the current way */
          checkpoint_level();
          Sprintf(buf, "%s y\n", mplock);
          mp_message(yield_after, buf);
          while (mp_await_reply_or_yield() != 2) {}
          uncheckpoint_level();
        }
}

STATIC_OVL void
final_level()
{
	struct monst *mtmp;
	struct obj *otmp;
	coord mm;
	int i;

	/* reset monster hostility relative to player */
	for (mtmp = fmon; mtmp; mtmp = mtmp->nmon)
            if (!DEADMONSTER(mtmp) && !is_mp_player(mtmp))
                reset_hostility(mtmp);

	/* create some player-monsters */
	create_mplayers(rn1(4, 3), TRUE);

	/* create a guardian angel next to player, if worthy */
	if (Conflict) {
	    pline(
	     "A voice booms: \"Thy desire for conflict shall be fulfilled!\"");
	    for (i = rnd(4); i > 0; --i) {
		mm.x = u.ux;
		mm.y = u.uy;
		if (enexto(&mm, mm.x, mm.y, &mons[PM_ANGEL]))
		    (void) mk_roamer(&mons[PM_ANGEL], u.ualign.type,
				     mm.x, mm.y, FALSE);
	    }

	} else if (u.ualign.record > 8) {	/* fervent */
	    pline("A voice whispers: \"Thou hast been worthy of me!\"");
	    mm.x = u.ux;
	    mm.y = u.uy;
	    if (enexto(&mm, mm.x, mm.y, &mons[PM_ANGEL])) {
		if ((mtmp = mk_roamer(&mons[PM_ANGEL], u.ualign.type,
				      mm.x, mm.y, TRUE)) != 0) {
		    if (!Blind)
			pline("An angel appears near you.");
		    else
			You_feel("the presence of a friendly angel near you.");
		    /* guardian angel -- the one case mtame doesn't
		     * imply an edog structure, so we don't want to
		     * call tamedog().
		     */
		    mtmp->mtame = 10;
		    /* make him strong enough vs. endgame foes */
		    mtmp->m_lev = rn1(8,15);
		    mtmp->mhp = mtmp->mhpmax =
					d((int)mtmp->m_lev,10) + 30 + rnd(30);
		    if ((otmp = select_hwep(mtmp)) == 0) {
			otmp = mksobj(SILVER_SABER, FALSE, FALSE);
			if (mpickobj(mtmp, otmp))
			    panic("merged weapon?");
		    }
		    bless(otmp);
		    if (otmp->spe < 4) otmp->spe += rnd(4);
		    if ((otmp = which_armor(mtmp, W_ARMS)) == 0 ||
			    otmp->otyp != SHIELD_OF_REFLECTION) {
			(void) mongets(mtmp, AMULET_OF_REFLECTION);
			m_dowear(mtmp, TRUE);
		    }
		}
	    }
	}
}

static char *dfr_pre_msg = 0,	/* pline() before level change */
	    *dfr_post_msg = 0;	/* pline() after level change */

/* change levels at the end of this turn, after monsters finish moving */
void
schedule_goto(tolev, at_stairs, falling, portal_flag, pre_msg, post_msg)
d_level *tolev;
boolean at_stairs, falling;
int portal_flag;
const char *pre_msg, *post_msg;
{
	int typmask = 0100;		/* non-zero triggers `deferred_goto' */

	/* destination flags (`goto_level' args) */
	if (at_stairs)	 typmask |= 1;
	if (falling)	 typmask |= 2;
	if (portal_flag) typmask |= 4;
	if (portal_flag < 0) typmask |= 0200;	/* flag for portal removal */
	u.utotype = typmask;
	/* destination level */
	assign_level(&u.utolev, tolev);

	if (pre_msg)
	    dfr_pre_msg = strcpy((char *)alloc(strlen(pre_msg) + 1), pre_msg);
	if (post_msg)
	    dfr_post_msg = strcpy((char *)alloc(strlen(post_msg)+1), post_msg);
}

/* handle something like portal ejection */
void
deferred_goto()
{
	if (!on_level(&u.uz, &u.utolev)) {
	    d_level dest;
	    int typmask = u.utotype; /* save it; goto_level zeroes u.utotype */

	    assign_level(&dest, &u.utolev);
	    if (dfr_pre_msg) pline("%s",dfr_pre_msg);
	    goto_level(&dest, !!(typmask&1), !!(typmask&2), !!(typmask&4));
	    if (typmask & 0200) {	/* remove portal */
		struct trap *t = t_at(u.ux, u.uy);

		if (t) {
		    deltrap(t);
		    newsym(u.ux, u.uy);
		}
	    }
	    if (dfr_post_msg) pline("%s",dfr_post_msg);
	}
	u.utotype = 0;		/* our caller keys off of this */
	if (dfr_pre_msg)
	    free((genericptr_t)dfr_pre_msg),  dfr_pre_msg = 0;
	if (dfr_post_msg)
	    free((genericptr_t)dfr_post_msg),  dfr_post_msg = 0;
}

#endif /* OVL2 */
#ifdef OVL3

/*
 * Return TRUE if we created a monster for the corpse.  If successful, the
 * corpse is gone.
 */
boolean
revive_corpse(corpse)
struct obj *corpse;
{
    struct monst *mtmp, *mcarry;
    boolean is_uwep, chewed;
    xchar where;
    char *cname, cname_buf[BUFSZ];
    struct obj *container = (struct obj *)0;
    int container_where = 0;
    
    where = corpse->where;
    is_uwep = corpse == uwep;
    cname = eos(strcpy(cname_buf, "bite-covered "));
    Strcpy(cname, corpse_xname(corpse, TRUE));
    mcarry = (where == OBJ_MINVENT) ? corpse->ocarry : 0;

    if (where == OBJ_CONTAINED) {
    	struct monst *mtmp2 = (struct monst *)0;
	container = corpse->ocontainer;
    	mtmp2 = get_container_location(container, &container_where, (int *)0);
	/* container_where is the outermost container's location even if nested */
	if (container_where == OBJ_MINVENT && mtmp2) mcarry = mtmp2;
        if (mtmp2 && is_mp_player(mtmp2)) { /* sanity */
            impossible("Nondriving player holding a container?");
            return FALSE;
        }
    }
    mtmp = revive(corpse);	/* corpse is gone if successful */

    if (mtmp) {
	chewed = (mtmp->mhp < mtmp->mhpmax);
	if (chewed) cname = cname_buf;	/* include "bite-covered" prefix */
	switch (where) {
	    case OBJ_INVENT:
		if (is_uwep)
		    pline_The("%s writhes out of your grasp!", cname);
		else
		    You_feel("squirming in your backpack!");
		break;

	    case OBJ_FLOOR:
		if (cansee(mtmp->mx, mtmp->my))
		    pline("%s rises from the dead!", chewed ?
			  Adjmonnam(mtmp, "bite-covered") : Monnam(mtmp));
		break;

	    case OBJ_MINVENT:		/* probably a nymph's */
		if (cansee(mtmp->mx, mtmp->my)) {
		    if (canseemon(mcarry))
			pline("Startled, %s drops %s as it revives!",
			      mon_nam(mcarry), an(cname));
		    else
			pline("%s suddenly appears!", chewed ?
			      Adjmonnam(mtmp, "bite-covered") : Monnam(mtmp));
		}
		break;
	   case OBJ_CONTAINED:
	   	if (container_where == OBJ_MINVENT && cansee(mtmp->mx, mtmp->my) &&
		    mcarry && canseemon(mcarry) && container) {
		        char sackname[BUFSZ];
		        Sprintf(sackname, "%s %s", s_suffix(mon_nam(mcarry)),
				xname(container)); 
	   		pline("%s writhes out of %s!", Amonnam(mtmp), sackname);
	   	} else if (container_where == OBJ_INVENT && container) {
		        char sackname[BUFSZ];
		        Strcpy(sackname, an(xname(container)));
	   		pline("%s %s out of %s in your pack!",
	   			Blind ? Something : Amonnam(mtmp),
				locomotion(mtmp->data,"writhes"),
	   			sackname);
	   	} else if (container_where == OBJ_FLOOR && container &&
		            cansee(mtmp->mx, mtmp->my)) {
		        char sackname[BUFSZ];
		        Strcpy(sackname, an(xname(container)));
			pline("%s escapes from %s!", Amonnam(mtmp), sackname);
		}
		break;
	    default:
		/* we should be able to handle the other cases... */
		impossible("revive_corpse: lost corpse @ %d", where);
		break;
	}
	return TRUE;
    }
    return FALSE;
}

/* Revive the corpse via a timeout. */
/*ARGSUSED*/
void
revive_mon(arg, timeout)
genericptr_t arg;
long timeout;
{
    struct obj *body = (struct obj *) arg;

    /* if we succeed, the corpse is gone, otherwise, rot it away */
    if (!revive_corpse(body)) {
	if (is_rider(&mons[body->corpsenm]))
	    You_feel("less hassled.");
	(void) start_timer(250L - (monstermoves-body->age),
					TIMER_OBJECT, ROT_CORPSE, arg);
    }
}

int
donull()
{
	return(1);	/* Do nothing, but let other things happen */
}
int
donull0()
{
	return(0);	/* Do nothing, consuming no time */
}


#endif /* OVL3 */
#ifdef OVLB

STATIC_PTR int
wipeoff()
{
	if(u.ucreamed < 4)	u.ucreamed = 0;
	else			u.ucreamed -= 4;
	if (Blinded < 4)	Blinded = 0;
	else			Blinded -= 4;
	if (!Blinded) {
		pline("You've got the glop off.");
		u.ucreamed = 0;
		Blinded = 1;
		make_blinded(0L,TRUE);
		return(0);
	} else if (!u.ucreamed) {
		Your("%s feels clean now.", body_part(FACE));
		return(0);
	}
	return(1);		/* still busy */
}

int
dowipe()
{
	if(u.ucreamed)  {
		static NEARDATA char buf[39];

		Sprintf(buf, "wiping off your %s", body_part(FACE));
		set_occupation(wipeoff, buf, 0);
		/* Not totally correct; what if they change back after now
		 * but before they're finished wiping?
		 */
		return(1);
	}
	Your("%s is already clean.", body_part(FACE));
	return(1);
}

void
set_wounded_legs(side, timex)
register long side;
register int timex;
{
	/* KMH -- STEED
	 * If you are riding, your steed gets the wounded legs instead.
	 * You still call this function, but don't lose hp.
	 * Caller is also responsible for adjusting messages.
	 */

	if(!Wounded_legs) {
		ATEMP(A_DEX)--;
		flags.botl = 1;
	}

	if(!Wounded_legs || (HWounded_legs & TIMEOUT))
		HWounded_legs = timex;
	EWounded_legs = side;
	(void)encumber_msg();
}

void
heal_legs()
{
	if(Wounded_legs) {
		if (ATEMP(A_DEX) < 0) {
			ATEMP(A_DEX)++;
			flags.botl = 1;
		}

#ifdef STEED
		if (!u.usteed)
#endif
		{
			/* KMH, intrinsics patch */
			if((EWounded_legs & BOTH_SIDES) == BOTH_SIDES) {
			Your("%s feel somewhat better.",
				makeplural(body_part(LEG)));
		} else {
			Your("%s feels somewhat better.",
				body_part(LEG));
		}
		}
		HWounded_legs = EWounded_legs = 0;
	}
	(void)encumber_msg();
}

#endif /* OVLB */

/*do.c*/
