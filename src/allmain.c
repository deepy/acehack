/*	SCCS Id: @(#)allmain.c	3.4	2003/04/02	*/
/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */
/* Modified 28 Dec 2011 by Alex Smith */
/* NetHack may be freely redistributed.  See license for details. */

/* various code that was replicated in *main.c */

#include "hack.h"

#ifndef NO_SIGNAL
#include <signal.h>
#endif

#ifdef POSITIONBAR
STATIC_DCL void NDECL(do_positionbar);
#endif

#ifdef OVL0

void
moveloop()
{
#if defined(MICRO) || defined(WIN32)
    char ch;
    int abort_lev;
#endif
    int moveamt = 0, wtcap = 0, change = 0;
    int turns_behind = 0;
    boolean didmove = FALSE, monscanmove = FALSE;
    const char *merging_player;

    flags.moonphase = phase_of_the_moon();
    if(flags.moonphase == FULL_MOON) {
	You("are lucky!  Full moon tonight.");
	change_luck(1);
    } else if(flags.moonphase == NEW_MOON) {
	pline("Be careful!  New moon tonight.");
    }
    flags.friday13 = friday_13th();
    if (flags.friday13) {
	pline("Watch out!  Bad things can happen on Friday the 13th.");
	change_luck(-1);
    }

    initrack();


    /* Note:  these initializers don't do anything except guarantee that
	    we're linked properly.
    */
    decl_init();
    monst_init();
    monstr_init();	/* monster strengths */
    objects_init();

#ifdef WIZARD
    if (wizard) add_debug_extended_commands();
#endif

    (void) encumber_msg(); /* in case they auto-picked up something */

    u.uz0.dlevel = u.uz.dlevel;
    youmonst.movement = NORMAL_SPEED;	/* give the hero some movement points */

    for(;;) {
	get_nh_event();
#ifdef POSITIONBAR
	do_positionbar();
#endif

	didmove = flags.move;
	if(didmove) {
	    /* actual time passed */
	    youmonst.movement -= NORMAL_SPEED;

	    do { /* hero can't move this turn loop */

                turns_behind += mp_turns_behind();

		wtcap = encumber_msg();

		flags.mon_moving = TRUE;
                do {
		    monscanmove = movemon();

                    /* Note that monsters might choose not to move
                       right now, in order to target a different
                       player. This is fine if they do so by the end
                       of the turn; if they don't, they lose their
                       move. This is also where we yield to players
                       who still have movement left. The options:

                       turns_behind == 0, movement >= NORMAL_SPEED
                       The player's action. We break out of this loop,
                       do the player's-action behaviour but not the
                       end-of-turn codepath, then come back into this
                       loop and run monster and other player turns.

                       turns_behind == 0, movement < NORMAL_SPEED,
                       monscanmove == TRUE

                       Monster extra actions at end of turn, because
                       they're faster than the player, or because this
                       player hasn't acted yet and needs to do a
                       monster AI update. We just call monscanmove()
                       again, updating monsters and then (if necessary
                       yielding).

                       turns_behind == 0, movement < NORMAL_SPEED,
                       monscanmove == FALSE

                       End of turn. We go straight into the global end
                       of turn update, re-energizing every monster and
                       ourself, and marking other players as one turn
                       out of sync. Then we take a turn ourself if we
                       have movement, but not if we don't (that is,
                       we're slower than NORMAL_SPEED and also don't
                       get an action this turn). Then we fall into
                       the top of this loop and do movemon again,
                       which causes every other player in turn to do
                       a movement ration recalculation.

                       turns_behind == 1

                       Calculating movement energy for the turn for
                       this player specifically, and then taking an
                       action if it reaches at least NORMAL_SPEED.
                       This also causes monster AI to run if and only
                       if this is the last player to do the
                       start-of-turn routine.
                    */
                    turns_behind += mp_turns_behind();
		    if (youmonst.movement >= NORMAL_SPEED || turns_behind)
                      break;	/* it's now your action or the next turn */
                } while (monscanmove);
		flags.mon_moving = FALSE;

		if (((!monscanmove && youmonst.movement < NORMAL_SPEED) ||
                     turns_behind) && (!iflags.multiplayer || !u.ustuck)) {

                    /* We get here with turns_behind == 0 only if
                       every other player is out of movement energy
                       and so are we, in which case /somebody/ has to
                       do the updates; it should be us. Otherwise, we
                       get here with turns_behind == 1 on our first
                       action of the turn; we do the end-of-turn
                       updates and go back to calculating monster and
                       other-player movements if we still don't have
                       enough movement energy. */

		    /* both you and the monsters are out of steam this round */
		    /* set up for a new turn */
		    struct monst *mtmp;

		    /***************************************************************/
		    /* once-per-turn things done once across all processes go here */
		    /***************************************************************/

                    if (!turns_behind) {

                        mcalcdistress();	/* adjust monsters' trap, blind, etc */

                        /* reallocate movement rations to monsters and players;
                           note that mcalcmove reallocates movement rations to
                           remote multiplayer players too, but indirectly */
                        for (mtmp = fmon; mtmp; mtmp = mtmp->nmon)
                            mtmp->movement += mcalcmove(mtmp);

                        if(!rn2(u.uevent.udemigod ? 25 :
                                (depth(&u.uz) > depth(&stronghold_level)) ? 50 : 70))
                            (void) makemon((struct permonst *)0, 0, 0, NO_MM_FLAGS);

                        /* TODO: stinking cloud damage on nondriving player,
                           and stinking cloud interaction with anti-PvP. For
                           the time being, use of stinking cloud scrolls is
                           just forced to fail in multiplayer. */
                        run_regions();

                        if (Is_waterlevel(&u.uz))
                            movebubbles();
                    } else turns_behind--;

		    /**********************************************************/
		    /* once-per-turn things done once in each process go here */
		    /**********************************************************/

		    /* calculate how much time passed */
#ifdef STEED
		    if (u.usteed && u.umoved) {
			/* your speed doesn't augment steed's speed */
			moveamt = mcalcmove(u.usteed);
		    } else
#endif
		    {
			moveamt = youmonst.data->mmove;

			if (Very_fast) {	/* speed boots or potion */
			    /* average movement is 1.67 times normal */
			    moveamt += NORMAL_SPEED / 2;
			    if (rn2(3) == 0) moveamt += NORMAL_SPEED / 2;
			} else if (Fast) {
			    /* average movement is 1.33 times normal */
			    if (rn2(3) != 0) moveamt += NORMAL_SPEED / 2;
			}
		    }

		    switch (wtcap) {
			case UNENCUMBERED: break;
			case SLT_ENCUMBER: moveamt -= (moveamt / 4); break;
			case MOD_ENCUMBER: moveamt -= (moveamt / 2); break;
			case HVY_ENCUMBER: moveamt -= ((moveamt * 3) / 4); break;
			case EXT_ENCUMBER: moveamt -= ((moveamt * 7) / 8); break;
			default: break;
		    }

		    youmonst.movement += moveamt;
		    if (youmonst.movement < 0) youmonst.movement = 0;
		    settrack();

		    monstermoves++;
		    moves++;

		    if (flags.bypasses) clear_bypasses();
		    if(Glib) glibr();
		    nh_timeout();

		    if (u.ublesscnt)  u.ublesscnt--;
		    if(flags.time && !flags.run)
			flags.botl = 1;

		    /* One possible result of prayer is healing.  Whether or
		     * not you get healed depends on your current hit points.
		     * If you are allowed to regenerate during the prayer, the
		     * end-of-prayer calculation messes up on this.
		     * Another possible result is rehumanization, which requires
		     * that encumbrance and movement rate be recalculated.
		     */
		    if (u.uinvulnerable) {
			/* for the moment at least, you're in tiptop shape */
			wtcap = UNENCUMBERED;
		    } else if (Upolyd && youmonst.data->mlet == S_EEL && !is_pool(u.ux,u.uy) && !Is_waterlevel(&u.uz)) {
			if (u.mh > 1) {
			    u.mh--;
			    flags.botl = 1;
			} else if (u.mh < 1)
			    rehumanize();
		    } else if (Upolyd && u.mh < u.mhmax) {
			if (u.mh < 1)
			    rehumanize();
			else if (Regeneration ||
				    (wtcap < MOD_ENCUMBER && !(moves%20))) {
			    flags.botl = 1;
			    u.mh++;
			}
		    } else if (u.uhp < u.uhpmax &&
			 (wtcap < MOD_ENCUMBER || !u.umoved || Regeneration)) {
			if (u.ulevel > 9 && !(moves % 3)) {
			    int heal, Con = (int) ACURR(A_CON);

			    if (Con <= 12) {
				heal = 1;
			    } else {
				heal = rnd(Con);
				if (heal > u.ulevel-9) heal = u.ulevel-9;
			    }
			    flags.botl = 1;
			    u.uhp += heal;
			    if(u.uhp > u.uhpmax)
				u.uhp = u.uhpmax;
			} else if (Regeneration ||
			     (u.ulevel <= 9 &&
			      !(moves % ((MAXULEV+12) / (u.ulevel+2) + 1)))) {
			    flags.botl = 1;
			    u.uhp++;
			}
		    }

		    /* moving around while encumbered is hard work */
		    if (wtcap > MOD_ENCUMBER && u.umoved) {
			if(!(wtcap < EXT_ENCUMBER ? moves%30 : moves%10)) {
			    if (Upolyd && u.mh > 1) {
				u.mh--;
			    } else if (!Upolyd && u.uhp > 1) {
				u.uhp--;
			    } else {
				You("pass out from exertion!");
				exercise(A_CON, FALSE);
				fall_asleep(-10, FALSE);
			    }
			}
		    }

		    if ((u.uen < u.uenmax) &&
			((wtcap < MOD_ENCUMBER &&
			  (!(moves%((MAXULEV + 8 - u.ulevel) *
				    (Role_if(PM_WIZARD) ? 3 : 4) / 6))))
			 || Energy_regeneration)) {
			u.uen += rn1((int)(ACURR(A_WIS) + ACURR(A_INT)) / 15 + 1,1);
			if (u.uen > u.uenmax)  u.uen = u.uenmax;
			flags.botl = 1;
		    }

		    if(!u.uinvulnerable) {
			if(Teleportation && !rn2(85)) {
			    xchar old_ux = u.ux, old_uy = u.uy;
			    tele();
			    if (u.ux != old_ux || u.uy != old_uy) {
				if (!next_to_u()) {
				    check_leash(old_ux, old_uy);
				}
#ifdef REDO
				/* clear doagain keystrokes */
				pushch(0);
				savech(0);
#endif
			    }
			}
			/* delayed change may not be valid anymore */
			if ((change == 1 && !Polymorph) ||
			    (change == 2 && u.ulycn == NON_PM))
			    change = 0;
			if(Polymorph && !rn2(100))
			    change = 1;
			else if (u.ulycn >= LOW_PM && !Upolyd &&
				 !rn2(80 - (20 * night())))
			    change = 2;
			if (change && !Unchanging) {
			    if (multi >= 0) {
				if (occupation)
				    stop_occupation();
				else
				    nomul(0);
				if (change == 1) polyself(FALSE);
				else you_were();
				change = 0;
			    }
			}
		    }

		    if(Searching && multi >= 0) (void) dosearch0(1);
		    dosounds();
		    do_storms();
		    gethungry();
		    age_spells();
		    exerchk();
		    invault();
		    if (u.uhave.amulet) amulet();
		    if (!rn2(40+(int)(ACURR(A_DEX)*3)))
			u_wipe_engr(rnd(3));
		    if (u.uevent.udemigod && !u.uinvulnerable) {
			if (u.udg_cnt) u.udg_cnt--;
			if (!u.udg_cnt) {
			    intervene();
			    u.udg_cnt = rn1(200, 50);
			}
		    }
		    restore_attrib();
		    /* underwater and waterlevel vision are done here */
		    if (Underwater && !Is_waterlevel(&u.uz))
			under_water(0);
		    /* vision while buried done here */
		    else if (u.uburied) under_ground(0);

		    /* when immobile, count is in turns */
		    if(multi < 0) {
			if (++multi == 0) {	/* finished yet? */
			    unmul((char *)0);
			    /* if unmul caused a level change, take it now */
			    if (u.utotype) deferred_goto();
			}
		    }

                    /* We may need to give a break from the normal
                       driving sequence to allow another player onto
                       the level. (This would also allow players to
                       join uninvited, if implementing that seems
                       desirable.) This is not a "true" yield,
                       because it doesn't allow the other player
                       actions, but it does allow the other player
                       to place themselves on the map. Also, it's
                       technically an acknowledgement rather than a
                       yield; but we get a true yield back again.
                       This reduces the total number of drivers by
                       one, as we require. */
                    if (!u.ustuck) { /* cannot safely yield when stuck */
                      if ((merging_player = mp_levelmerger_name())) {
                        checkpoint_level();
                        mp_message(merging_player, "\n");
                        while (mp_await_reply_or_yield() != 2) {}
                        uncheckpoint_level();
                      }
                    }
		} else if (iflags.multiplayer && u.ustuck &&
                           !monscanmove && youmonst.movement < NORMAL_SPEED) {
                    int oldmovement = youmonst.movement;
                    /* In order to avoid an infinite loop, give the player
                       and the engulfing/sticking monster extra actions.
                       Adding to every monster on the level might be less
                       abusable, but would make unrelated players on the
                       level potentially have to live through 20 attacks
                       in a row, which would be ridiculous. */
                    u.ustuck->movement += mcalcmove(u.ustuck);

                    {
			moveamt = youmonst.data->mmove;

			if (Very_fast) {	/* speed boots or potion */
			    /* average movement is 1.67 times normal */
			    moveamt += NORMAL_SPEED / 2;
			    if (rn2(3) == 0) moveamt += NORMAL_SPEED / 2;
			} else if (Fast) {
			    /* average movement is 1.33 times normal */
			    if (rn2(3) != 0) moveamt += NORMAL_SPEED / 2;
			}
		    }

		    switch (wtcap) {
			case UNENCUMBERED: break;
			case SLT_ENCUMBER: moveamt -= (moveamt / 4); break;
			case MOD_ENCUMBER: moveamt -= (moveamt / 2); break;
			case HVY_ENCUMBER: moveamt -= ((moveamt * 3) / 4); break;
			case EXT_ENCUMBER: moveamt -= ((moveamt * 7) / 8); break;
			default: break;
		    }

		    youmonst.movement += moveamt;
                    /* Prevent blue jellies engulfed by ice vortices
                       locking the game forever. */
		    if (youmonst.movement <= oldmovement)
                      youmonst.movement = oldmovement + 1;
                }
	    } while (youmonst.movement<NORMAL_SPEED); /* hero can't move loop */

	    /******************************************/
	    /* once-per-hero-took-time things go here */
	    /******************************************/

	} /* actual time passed */

	/****************************************/
	/* once-per-player-input things go here */
	/****************************************/

	find_ac();
	if(!flags.mv || Blind) {
	    /* redo monsters if hallu or wearing a helm of telepathy */
	    if (Hallucination) {	/* update screen randomly */
		see_monsters();
		see_objects();
		see_traps();
		if (u.uswallow) swallowed(0);
	    } else if (Unblind_telepat) {
		see_monsters();
	    } else if (Warning || Warn_of_mon)
	     	see_monsters();

	    if (vision_full_recalc) vision_recalc(0);	/* vision! */
	}
	if(flags.botl || flags.botlx) bot();

	flags.move = 1;

	if(multi >= 0 && occupation) {
#if defined(MICRO) || defined(WIN32)
	    abort_lev = 0;
	    if (kbhit()) {
		if ((ch = Getchar()) == ABORT)
		    abort_lev++;
# ifdef REDO
		else
		    pushch(ch);
# endif /* REDO */
	    }
	    if (!abort_lev && (*occupation)() == 0)
#else
	    if ((*occupation)() == 0)
#endif
		occupation = 0;
	    if(
#if defined(MICRO) || defined(WIN32)
		   abort_lev ||
#endif
		   monster_nearby()) {
		stop_occupation();
		reset_eat();
	    }
#if defined(MICRO) || defined(WIN32)
	    if (!(++occtime % 7))
		display_nhwindow(WIN_MAP, FALSE);
#endif
	    continue;
	}

	if ((u.uhave.amulet || Clairvoyant) &&
	    !In_endgame(&u.uz) && !BClairvoyant &&
	    !(moves % 15) && !rn2(2))
		do_vicinity_map();

	if(u.utrap && u.utraptype == TT_LAVA) {
	    if(!is_lava(u.ux,u.uy))
		u.utrap = 0;
	    else if (!u.uinvulnerable) {
		u.utrap -= 1<<8;
		if(u.utrap < 1<<8) {
		    killer_format = KILLED_BY;
		    killer = "molten lava";
		    You("sink below the surface and die.");
		    done(DISSOLVED);
		} else if(didmove && !u.umoved) {
		    Norep("You sink deeper into the lava.");
		    u.utrap += rnd(4);
		}
	    }
	}

#ifdef WIZARD
	if (iflags.sanity_check)
	    sanity_check();
#endif

#ifdef CLIPPING
	/* just before rhack */
	cliparound(u.ux, u.uy);
#endif

        if (iflags.multiplayer) {
          rdocrt();
        }

	u.umoved = FALSE;

	if (multi > 0) {
	    lookaround();
	    if (!multi) {
		/* lookaround may clear multi */
		flags.move = 0;
		if (flags.time) flags.botl = 1;
		continue;
	    }
	    if (flags.mv) {
		if(multi < COLNO && !--multi)
		    flags.travel = iflags.travel1 = flags.mv = flags.run = 0;
		domove();
	    } else {
		--multi;
		rhack(save_cm);
	    }
	} else if (multi == 0) {
#ifdef MAIL
	    ckmailstatus();
#endif
            maybe_tutorial();
	    rhack((char *)0);
	}
	if (u.utotype)		/* change dungeon level */
	    deferred_goto();	/* after rhack() */
	/* !flags.move here: multiple movement command stopped */
	else if (flags.time && (!flags.move || !flags.mv))
	    flags.botl = 1;

	if (vision_full_recalc) vision_recalc(0);	/* vision! */
	/* when running in non-tport mode, this gets done through domove() */
	if ((!flags.run || iflags.runmode == RUN_TPORT) &&
		(multi && (!flags.travel ? !(multi % 7) : !(moves % 7L)))) {
	    if (flags.time && flags.run) flags.botl = 1;
	    display_nhwindow(WIN_MAP, FALSE);
	}
    }
}

#endif /* OVL0 */
#ifdef OVL1

void
stop_occupation()
{
	if(occupation) {
		if (!maybe_finished_meal(TRUE))
		    You("stop %s.", occtxt);
		occupation = 0;
		flags.botl = 1; /* in case u.uhs changed */
/* fainting stops your occupation, there's no reason to sync.
		sync_hunger();
*/
#ifdef REDO
		nomul(0);
		pushch(0);
#endif
	}
}

#endif /* OVL1 */
#ifdef OVLB

void
display_gamewindows()
{
    WIN_MESSAGE = create_nhwindow(NHW_MESSAGE);
    WIN_STATUS = create_nhwindow(NHW_STATUS);
    WIN_MAP = create_nhwindow(NHW_MAP);
    WIN_INVEN = create_nhwindow(NHW_MENU);

#ifdef MAC
    /*
     * This _is_ the right place for this - maybe we will
     * have to split display_gamewindows into create_gamewindows
     * and show_gamewindows to get rid of this ifdef...
     */
	if ( ! strcmp ( windowprocs . name , "mac" ) ) {
	    SanePositions ( ) ;
	}
#endif

    /*
     * The mac port is not DEPENDENT on the order of these
     * displays, but it looks a lot better this way...
     */
    display_nhwindow(WIN_STATUS, FALSE);
    display_nhwindow(WIN_MESSAGE, FALSE);
    clear_glyph_buffer();
    display_nhwindow(WIN_MAP, FALSE);
}

/* Use a version of rndmonst() that's safe to use in the newgame sequence?
   Also used to determine egg safety (because it makes no sense for starting
   inventory to have eggs capable of hatching, and in fact crashes the game if
   such eggs are deleted in a newgame_part_2 reexecution). */
boolean rndmonst_safe_in_newgame = FALSE;

static int newgame_progress = 0;

void
newgame_part_1()
{
	int i;

        if (newgame_progress != 0) panic("Newgame sequence out of order: 1");
#ifdef MFLOPPY
        gameDiskPrompt();
#endif

        flags.ident = 1;

        for (i = 0; i < NUMMONS; i++)
                mvitals[i].mvflags = mons[i].geno & G_NOCORPSE;

        init_objects();		/* must be before u_init() */
        newgame_progress = 1;
}

/* This part of the sequence is idempotent, so that it can be called repeatedly
   in character selection. (Well, not quite idempotent because random numbers
   are involved; but "safe to run repeatedly".) */
void
newgame_part_2()
{
 	if (newgame_progress < 1) newgame_part_1();
        if (newgame_progress > 2) panic("Newgame sequence out of order: 2");
	do {
	    flags.pantheon = -1; /* role_specific_modifications() will reset this */
            role_init();

            rndmonst_safe_in_newgame = TRUE;
            u_init_idempotent();
            rndmonst_safe_in_newgame = FALSE;
            reset_rndmonst(NON_PM);
            newgame_progress = 2;
        } while (inv_cnt() > 16); /* ensure no more than 16 items in starting inventory */
}

void
newgame()
{
	if (newgame_progress < 2) newgame_part_2();
        if (newgame_progress > 2) panic("Newgame sequence out of order: 3");

        role_specific_modifications();
        u_init_nonidempotent();
	init_dungeons();	
	init_artifacts();	/* TODO: putting this here means wizkits
                                   can't contain artifacts */

#ifndef NO_SIGNAL
	(void) signal(SIGINT, (SIG_RET_TYPE) done1);
#endif
#ifdef NEWS
	if(iflags.news) display_file(NEWS, FALSE);
#endif
	load_qtlist();	/* load up the quest text info */

	mklev();
	u_on_upstairs();
	vision_reset();		/* set up internals for level (after mklev) */
	check_special_room(FALSE);

	flags.botlx = 1;

	/* Move the monster from under you or else
	 * makedog() will fail when it calls makemon().
	 *			- ucsfcgl!kneller
	 */
	if(MON_AT(u.ux, u.uy)) mnexto(m_at(u.ux, u.uy));
	(void) makedog();
        display_nhwindow(WIN_MESSAGE, FALSE);
	docrt();

	if (flags.legacy) {
		flush_screen(1);
		com_pager(1);
	}

#ifdef INSURANCE
	save_currentstate();
#endif
	program_state.something_worth_saving++;	/* useful data now exists */
        newgame_progress = 3;

	/* Success! */
	welcome(TRUE);
	return;
}

void
terminate_during_newgame()
{
  display_nhwindow(WIN_MESSAGE, TRUE);
  wait_synch();
  program_state.gameover = 1;
  clearlocks();
  exit_nhwindows((char *) 0);
  terminate(EXIT_SUCCESS);  
}

/* Signal handler for connection failure in a multiplayer game, during
   the newgame sequence. This just displays an error message and quits
   the game. */
/*ARGSUSED*/
STATIC_DCL void
mp_newgame_connection_failure(sig_unused)
int sig_unused;
{
  pline("Connection failure!");
  terminate_during_newgame();
}

/* "Join game"; intialise a new game for the second or subsequent player
   in a multiplayer game. */
void
newgame_mp()
{
        int mpcfd;
        const char *ln;

	if (newgame_progress < 2) newgame_part_2();
        if (newgame_progress > 2) panic("Newgame sequence out of order: 3");

        /* This has to come first, because the qtext system is used to
           print the multiplayer instructions. */
        load_qtlist();

        /* Before trying to start the game, find a game to attach to. */
        for (;;) {

          /* Advertise that we're willing to join a multiplayer game. */
          mpcfd = setup_multiplayer_pipe('a');
          /* We don't actually want to panic, as this isn't our
             fault. But we can't continue. We're halfway through
             initialisation, so cannot safely use any of the cleanup
             routines but panic(); thus, we use our own here. */
          if (mpcfd < 0) {
            mp_newgame_connection_failure(0);
          }

          int other = FALSE;
          mp_track_yielders(TRUE);
          com_pager(8); /* gives instructions, waits for Space */
          cls();
          teardown_multiplayer_pipe('a'); /* stop listening */
          /* Whenever we tear down a pipe, we delay for a short time to
             avoid a potential race condition with something that just
             grabbed the old pipe from its name, and hasn't written to
             it yet. (The race condition isn't the end of the world 
             from our point of view, but it'll leave the other end
             hanging until cancelled manually.) This additionally
             prevents two multiplayer games being started by the same
             person in the same second, making it easier to generate
             unique filenames. */
          pline("Communicating...");
          suppress_more();
          sleep(1);
          mark_synch();
          mp_flush_log();
          mp_track_yielders(FALSE);
          for (;;) {
            ln = mp_yielder_name();
            const char* n;
            char qbuf[QBUFSZ];
            if (!ln) break;
            other = TRUE;
            /* Remove the numerical prefix from the lockname to get a
               character name. (TODO: .0 suffix) */
            n = ln + strspn(ln, "0123456789");
            /* Singular they for now, as we don't know gender yet. */
            Sprintf(qbuf, "%s invited you to their dungeon. Join their game?", n);
            switch (ynq(qbuf)) {
            case 'y':
              goto found_game_to_attach_to; /* multilevel break */
            case 'q':
              /* Decline all remaining invitations */
              while (ln) {mp_message(ln, "!\n"); ln = mp_yielder_name();}
              /* Quit the entire program */
              terminate_during_newgame();
            default:
              /* Let the other player know we declined their invitation */
              mp_message(ln, "!\n");
              continue;
            }
          }
          if (!other && yn("Nobody has invited you to their dungeon. Exit?") == 'y') {
            terminate_during_newgame();
          }
        }
        /* C doesn't have a multilevel break, so fake one with goto. */
found_game_to_attach_to: ;
        /* Let the other player know we accepted. Now we are driving.
           We need to set up the control pipe first in case it tries
           to reply (and it should, with things like dungeon layout). */
        mpcfd = setup_multiplayer_pipe('c');
        {
          /* We have to reject any other invitations. */
          const char* other_ln = mp_yielder_name();
          while (other_ln) {mp_message(other_ln, "!\n"); other_ln = mp_yielder_name();}
        }
        mp_message(ln, "\n");

        /* If connection takes more than 20 seconds, assume something's
           gone wrong at the other end. (Like, for instance, the other
           player cancelling before we accept.) */
	(void) signal(SIGALRM, (SIG_RET_TYPE) mp_newgame_connection_failure);
        alarm(20);

        /* This mirrors newgame in a way, but much of what we
           initialise has to be copied from the other game rather than
           generated from scratch, so we have to be very careful about
           multiplayer safety here. */

        /* Most of these are MP-safe. This will lead to different god
           names between the two games, but that's OK; there's nothing
           wrong with two characters each praying to their respective
           gods on the same lawful altar, for example. (We assume that
           gods of the same alignment cooperate to the extent that
           they're fine with that.) Quest nemesis fixup could get
           "interesting" if the nemesis is lured out of the quest;
           that will need testing. (Most likely it will work, but with
           garbled AI.) */
        role_specific_modifications();

        /* Affects just the character him/herself. */
        u_init_nonidempotent();

        /* We /cannot/ call init_dungeons, as that would lead to an
           incompatible dungeon layout between the games, which causes
           immense problems (both in terms of implementation, and in
           terms of player expectation). Instead, we grab the data
           from the other game. Given that the game we're connecting
           to is the only one with our control pipe, we don't need
           to worry about interruptions, so we just send binary
           right down the pipe. */
        restore_dungeon(mpcfd);
        restlevchn(mpcfd);

        /* A bit of a weird dependency issue here. We have to
           uncheckpoint before placing the player on the stairs, or we
           don't know where the stairs are. But we have to place the
           player on the stairs before uncheckpointing, or the game
           tries to move the cursor off the map, leading to an
           infinite loop. The solution is to get the coordinates from
           the other game. We also get the current local time from the
           other game, because the timings must be approximately in
           sync; and the level lockfile name, as otherwise we wouldn't
           be able to uncheckpoint. */
        if (read(mpcfd, &u.ux, sizeof(u.ux)) <= 0 ||
            read(mpcfd, &u.uy, sizeof(u.uy)) <= 0 ||
            read(mpcfd, &monstermoves, sizeof(monstermoves)) <= 0 ||
            read(mpcfd, iflags.mp_lock_name, BUFSZ) <= 0) {
          panic("Multiplayer control pipe read failure");
        }

        /* Remove watchdog timer. */
        alarm(0);

        /* Fix up the Quest (which isn't shared between games). */
        rerole_quest_branch();

        /* I /hope/ this is multiplayer-safe... */
        init_artifacts();

#ifndef NO_SIGNAL
	(void) signal(SIGINT, (SIG_RET_TYPE) done1);
#endif

        /* Record our process as being in the dungeon. */
        iflags.multiplayer = TRUE;
        mpcfd = open_levelfile(-1, 0);
        if (mpcfd < 0) {
          panic("Multiplayer main lockfile open failure");
        }
        flock(mpcfd, LOCK_SH);
        iflags.shared_lockfd = mpcfd;

        uncheckpoint_level();
        vision_reset(); /* we have separate vision for the two games,
                           even though we have shared map memory */
        check_special_room(FALSE);
        flags.botlx = 1;

        /* no pets; one pet between the players is enough, one pet each
           would just be annoying (note that there's no ownership
           relationship for pets, a monster is either tame to all or
           nontame to all */
#ifdef INSURANCE
	save_currentstate();
#endif
	program_state.something_worth_saving++;	/* useful data now exists */
        newgame_progress = 3;

        /* /Not/ rpline; we want to tell just the player whose game
           we're joining that everything worked. The ! suppresses a
           --More--. */
        (void) mp_message_and_reply(ln, "p!", "Communicating...", FALSE);

	/* Success! */
#ifdef NEWS
	if(iflags.news) display_file(NEWS, FALSE);
#endif
        display_nhwindow(WIN_MESSAGE, FALSE);
        docrt();
	welcome(TRUE);
}

/* show "welcome [back] to nethack" message at program startup */
void
welcome(new_game)
boolean new_game;	/* false => restoring an old game */
{
    char buf[BUFSZ];
    boolean currentgend = Upolyd ? u.mfemale : flags.female;

    /*
     * The "welcome back" message always describes your innate form
     * even when polymorphed or wearing a helm of opposite alignment.
     * Alignment is shown unconditionally for new games; for restores
     * it's only shown if it has changed from its original value.
     * Sex is shown for new games except when it is redundant; for
     * restores it's only shown if different from its original value.
     */
    *buf = '\0';
    if (new_game || u.ualignbase[A_ORIGINAL] != u.ualignbase[A_CURRENT])
	Sprintf(eos(buf), " %s", align_str(u.ualignbase[A_ORIGINAL]));
    if (!urole.name.f &&
	    (new_game ? (urole.allow & ROLE_GENDMASK) == (ROLE_MALE|ROLE_FEMALE) :
	     currentgend != flags.initgend))
	Sprintf(eos(buf), " %s", genders[currentgend].adj);

    pline(new_game ? "%s %s, welcome to AceHack!  You are a%s %s %s."
		   : "%s %s, the%s %s %s, welcome back to AceHack!",
	  Hello((struct monst *) 0), plname, buf, urace.adj,
	  (currentgend && urole.name.f) ? urole.name.f : urole.name.m);
}

#ifdef POSITIONBAR
STATIC_DCL void
do_positionbar()
{
	static char pbar[COLNO];
	char *p;
	
	p = pbar;
	/* up stairway */
	if (upstair.sx &&
	   (glyph_to_cmap(level.locations[upstair.sx][upstair.sy].glyph) ==
	    S_upstair ||
 	    glyph_to_cmap(level.locations[upstair.sx][upstair.sy].glyph) ==
	    S_upladder)) {
		*p++ = '<';
		*p++ = upstair.sx;
	}
	if (sstairs.sx &&
	   (glyph_to_cmap(level.locations[sstairs.sx][sstairs.sy].glyph) ==
	    S_upstair ||
 	    glyph_to_cmap(level.locations[sstairs.sx][sstairs.sy].glyph) ==
	    S_upladder)) {
		*p++ = '<';
		*p++ = sstairs.sx;
	}

	/* down stairway */
	if (dnstair.sx &&
	   (glyph_to_cmap(level.locations[dnstair.sx][dnstair.sy].glyph) ==
	    S_dnstair ||
 	    glyph_to_cmap(level.locations[dnstair.sx][dnstair.sy].glyph) ==
	    S_dnladder)) {
		*p++ = '>';
		*p++ = dnstair.sx;
	}
	if (sstairs.sx &&
	   (glyph_to_cmap(level.locations[sstairs.sx][sstairs.sy].glyph) ==
	    S_dnstair ||
 	    glyph_to_cmap(level.locations[sstairs.sx][sstairs.sy].glyph) ==
	    S_dnladder)) {
		*p++ = '>';
		*p++ = sstairs.sx;
	}

	/* hero location */
	if (u.ux) {
		*p++ = '@';
		*p++ = u.ux;
	}
	/* fence post */
	*p = 0;

	update_positionbar(pbar);
}
#endif

#endif /* OVLB */

/*allmain.c*/
