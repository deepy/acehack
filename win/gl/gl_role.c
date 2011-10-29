/* Copyright (C) 2002 Andrew Apted <ajapted@users.sourceforge.net> */
/* NetHack may be freely redistributed.  See license for details.  */
/* Modified 29 Oct 2011 by Alex Smith */

/*
 * SDL/GL window port for NetHack & Slash'EM.
 *
 * Character selection code (Role, Race, Gender, Alignment),
 * and game mode selection code (main, tutorial, etc).
 */

#include "hack.h"
#include "dlb.h"
#include "patchlevel.h"

#if defined(GL_GRAPHICS) || defined(SDL_GRAPHICS)

#define WINGL_INTERNAL
#include "winGL.h"

#include <string.h>
#include <ctype.h>


/* value chosen not to conflict with existing ROLE values */
#define ROLE_QUIT  (-7)


#define HEIGHT_PICKER  7
#define DEPTH_PICKER   8


/* callback functions used to create menus for player_selection().
 * Each one returns: 1 for a valid name, 0 for an invalid name, and -1
 * when the list is exhausted.  For 1 and 0, the name is copied into
 * the buffer provided.
 */
static int query_role_name(int what, char *buf)
{
  if (roles[what].name.m == NULL)
    return -1;

  if (flags.initgend >= 0 && flags.female && roles[what].name.f)
    strcpy(buf, roles[what].name.f);
  else
    strcpy(buf, roles[what].name.m);

  return ok_role(what, flags.initrace, flags.initgend,
      flags.initalign) ? 1 : 0;
}

static int query_race_name(int what, char *buf)
{
  if (races[what].noun == NULL)
    return -1;

  strcpy(buf, races[what].noun);

  return ok_race(flags.initrole, what, flags.initgend,
      flags.initalign) ? 1 : 0;
}

static int query_gender_name(int what, char *buf)
{
  if (what >= ROLE_GENDERS)
    return -1;

  strcpy(buf, genders[what].adj);
  
  return ok_gend(flags.initrole, flags.initrace, what,
      flags.initalign) ? 1 : 0;
}

static int query_align_name(int what, char *buf)
{
  if (what >= ROLE_ALIGNS)
    return -1;

  strcpy(buf, aligns[what].adj);

  return ok_align(flags.initrole, flags.initrace, flags.initgend,
      what) ? 1 : 0;
}

/* print a string with line splitting.  Based on sdlgl_putstr(), but
 * has some significant differences now.
 */
static void raw_puts_split(struct TextWindow *win, const char *str)
{
  int len = strlen(str);

  while (len > 0)
  {
    int use_len;
    int width = win->show_w;
    
    /* does it fit yet ? */
    if (len < width)
    {
      sdlgl_puts_nolf(win, str);
      return;
    }

    /* choose a good place to break the line */
    for (use_len = width-1; use_len > width/2; use_len--)
      if (str[use_len] == ' ')
        break;

    /* no space in latter half of string ?  Must be a very long token,
     * so we might as well split it at the screen edge.
     */
    if (str[use_len] != ' ')
      use_len = width-1;

    for (; use_len > 0; use_len--, str++, len--)
      if (*str != '\n' && *str != '\r')
        sdlgl_putc(win, *str);

    sdlgl_putc(win, '\n');

    /* remove leading spaces */
    for (; *str == ' '; str++, len--) { }
  }
}

/* may return ROLE_RANDOM, or ROLE_NONE when the list is empty (which
 * can only mean that the role/race/gender/alignment combination that
 * the user supplied on the command line was not possible), or
 * ROLE_QUIT if the user pressed escape.
 */
static int do_player_selection_menu(const char *type,
    int (* func)(int what, char *buf))
{
  int i, kind;
  int valid = 0, total = 0, last = -1;

  winid window;
  anything any;
  int attr, choice;

  menu_item *selected = NULL;

  int used_accs[52] = { 0, };
  char accel;
  
  char name[BUFSZ];
  char prompt[BUFSZ];
  char randstr[BUFSZ];

  /* count valid entries */
  for (i=0; ; i++)
  {
    kind = (* func)(i, name);

    if (kind < 0)
      break;

    total++;

    if (kind > 0)
    {
      valid++;
      last = i;
    }
  }
  
  if (valid == 0)
    return ROLE_NONE;

  assert(last >= 0);

  /* -AJA- this is disabled -- it's better IMHO to show the user the
   *       single choice, letting them press ESC and pick some other
   *       role/race/gender if they're not satisfied.
   */
#if 0
  if (valid == 1)
    return last;
#endif
 
  /* create menu */

  sprintf(prompt,  "Pick your %s", type);
  sprintf(randstr, "Random %s", type);
 
  window = Sdlgl_create_nhwindow(NHW_MENU);
  assert(window != WIN_ERR);

  Sdlgl_start_menu(window);

  for (i=0; i < total; i++)
  {
    kind = (* func)(i, name);
    any.a_void = 0;
    attr = ATR_NONE;

    /* choose accelerator key */
    accel = lowc(name[0]);

    if (accel < 'a' || accel > 'z')
      accel = 0;
    else
    {
      if (! used_accs[accel - 'a'])
        used_accs[accel - 'a'] = 1;
      else
      {
        accel = highc(accel);

        if (! used_accs[accel - 'A' + 26])
          used_accs[accel - 'A' + 26] = 1;
        else
          accel = 0;
      }
    }

    if (kind > 0)
    {
      /* must add one, zero is not allowed */
      any.a_int = 1 + i;
    }
    else
    {
      /* shift name right (to align), and make dim */
      memmove(name + 5, name, strlen(name) + 1);
      memcpy(name, "     ", 5);
      attr = ATR_DIM;
    }

    Sdlgl_add_menu(window, NO_GLYPH, &any, accel,
        0 /* no groupacc */, attr, name, MENU_UNSELECTED);
  }

  /* add `Random' line (unless there's only one choice) */
  if (valid > 1)
  {
    any.a_int = 1 + total;

    Sdlgl_add_menu(window, NO_GLYPH, &any, '*',
          0 /* no groupacc */, ATR_NONE, randstr, MENU_UNSELECTED);
  }

  Sdlgl_end_menu(window, prompt);

  /* get result back from menu */

  i = Sdlgl_select_menu(window, PICK_ONE, &selected);
  Sdlgl_destroy_nhwindow(window);

  if (i <= 0)
    return ROLE_QUIT;

  assert(i == 1 && selected);

  choice = selected[0].item.a_int - 1;
  free(selected);

  assert(0 <= choice && choice <= total);

  if (choice == total)
    return ROLE_RANDOM;

  return choice;
}

/* These return -1 if cancelled, otherwise 0.
 */
static int select_auto_pick(int *pick4u)
{
  struct TextWindow *win;
  int pixel_h;
  int ch;

  /* handle the fully random (-@) option */
  if (flags.randomall)
  {
    (* pick4u) = 1;
    return 0;
  }
  
  /* create text & tile windows */
  win = sdlgl_new_textwin(NHW_TEXT);  /* type unimportant */
   
  win->show_w = sdlgl_width / sdlgl_font_message->tile_w;
  win->show_h = HEIGHT_PICKER;

  pixel_h = sdlgl_font_message->tile_h * win->show_h;
  
  win->base = sdlgl_new_tilewin(sdlgl_font_message, win->show_w,
      win->show_h, 1,0);
   
  sdlgl_map_tilewin(win->base, 0, sdlgl_height - pixel_h,
      sdlgl_width, pixel_h, DEPTH_PICKER);

  /* do the query */
  {
    char pbuf[QBUFSZ];
    const char *prompt = "Shall I pick a character for you? [ynq] ";
    
    if (flags.initrole != ROLE_NONE || flags.initrace  != ROLE_NONE ||
        flags.initgend != ROLE_NONE || flags.initalign != ROLE_NONE)
    {
      prompt = build_plselection_prompt(pbuf, QBUFSZ, flags.initrole,
        flags.initrace, flags.initgend, flags.initalign);
    }

    sdlgl_home(win);

    /* if the copyright wasn't shown on the splash screen (because it
     * was disabled), then show it here.
     */
    if (! iflags.wc_splash_screen)
    {
      sdlgl_puts(win, COPYRIGHT_BANNER_A "\n");
      sdlgl_puts(win, COPYRIGHT_BANNER_B "\n");
      sdlgl_puts(win, COPYRIGHT_BANNER_C "\n");
      sdlgl_puts(win, "\n");
    }
    
    raw_puts_split(win, prompt);
  }

  do
  {
    sdlgl_flush();
    ch = sdlgl_get_key(0);
  }
  while (strchr("ynq\033", ch) == NULL);

  sdlgl_unmap_tilewin(win->base);
  sdlgl_free_textwin(win);

  if (ch == 'q' || ch == '\033')
    return -1;

  /* should we randomly pick for the player ? */
  (* pick4u) = (ch == 'y') ? 1 : 0;

  return 0;
}

static int select_a_role(int pick4u)
{
  int choice;

  while (flags.initrole < 0)
  {
    if (pick4u || flags.initrole == ROLE_RANDOM || flags.randomall)
    {
      flags.initrole = pick_role(flags.initrace, flags.initgend,
          flags.initalign, PICK_RANDOM);
      break;
    }

    /* select a role */
    for (;;)
    {
      choice = do_player_selection_menu("role", query_role_name);
      
      if (choice != ROLE_NONE)
        break;

      /* reset */
      if (flags.initalign >= 0) flags.initalign = ROLE_NONE;
      else if (flags.initgend >= 0) flags.initgend = ROLE_NONE;
      else if (flags.initrace >= 0) flags.initrace = ROLE_NONE;
      else
        return 0;
    }

    if (choice == ROLE_QUIT)
    {
      flags.initrole = ROLE_NONE;
      return -1;
    }

    flags.initrole = choice;
  }

  return 0;
}

static int select_a_race(int pick4u)
{
  int choice;

  while (!validrace(flags.initrole, flags.initrace))
  {
    if (pick4u || flags.initrace == ROLE_RANDOM || flags.randomall)
    {
      flags.initrace = pick_race(flags.initrole, flags.initgend,
          flags.initalign, PICK_RANDOM);
      break;
    }

    /* select a race */
    for (;;)
    {
      choice = do_player_selection_menu("race", query_race_name);
    
      if (choice != ROLE_NONE)
        break;

      /* reset */
      if (flags.initalign >= 0) flags.initalign = ROLE_NONE;
      else if (flags.initgend >= 0) flags.initgend = ROLE_NONE;
      else
        return 0;
    }

    if (choice == ROLE_QUIT)
    {
      flags.initrole = ROLE_NONE;
      flags.initrace = ROLE_NONE;
      return -1;
    }

    flags.initrace = choice;
  }

  return 0;
}

static int select_a_gender(int pick4u)
{
  int choice;

  while (!validgend(flags.initrole, flags.initrace, flags.initgend))
  {
    if (pick4u || flags.initgend == ROLE_RANDOM || flags.randomall)
    {
      flags.initgend = pick_gend(flags.initrole, flags.initrace,
          flags.initalign, PICK_RANDOM);
      break;
    }
    
    /* select a gender */
    for (;;)
    {
      choice = do_player_selection_menu("gender", query_gender_name);
    
      if (choice != ROLE_NONE)
        break;

      /* reset */
      if (flags.initalign >= 0)
        flags.initalign = ROLE_NONE;
      else
        return 0;
    }

    if (choice == ROLE_QUIT)
    {
      flags.initrace = ROLE_NONE;
      flags.initgend = ROLE_NONE;
      return -1;
    }

    flags.initgend = choice;
  }

  return 0;
}

static int select_an_alignment(int pick4u)
{
  int choice;

  while (!validalign(flags.initrole, flags.initrace, flags.initalign))
  {
    if (pick4u || flags.initalign == ROLE_RANDOM || flags.randomall)
    {
      /* pick_align */
      flags.initalign = pick_align(flags.initrole, flags.initrace,
          flags.initgend, PICK_RANDOM);
      break;
    }

    /* select an alignment */
    for (;;)
    {
      choice = do_player_selection_menu("alignment", query_align_name);
    
      if (choice != ROLE_NONE)
        break;

      /* nothing to reset ! */
      return 0;
    }

    if (choice == ROLE_QUIT)
    {
      flags.initgend  = ROLE_NONE;
      flags.initalign = ROLE_NONE;
      return -1;
    }

    flags.initalign = choice;
  }

  return 0;
}

#define INIT_IS_RANDOM(val)  \
    ((val) == ROLE_RANDOM || \
     (flags.randomall && (val) == ROLE_NONE))
 
static void do_random_role_checks(void)
{
  if (INIT_IS_RANDOM(flags.initrole))
  {
    flags.initrole = pick_role(flags.initrace, flags.initgend,
        flags.initalign, PICK_RANDOM);
  }

  if (INIT_IS_RANDOM(flags.initrace))
  {
    flags.initrace = pick_race(flags.initrole, flags.initgend,
        flags.initalign, PICK_RANDOM);
  }

  if (INIT_IS_RANDOM(flags.initalign))
  {
    flags.initalign = pick_align(flags.initrole, flags.initrace,
        flags.initgend, PICK_RANDOM);
  }

  if (INIT_IS_RANDOM(flags.initgend))
  {
    flags.initgend = pick_gend(flags.initrole, flags.initrace,
        flags.initalign, PICK_RANDOM);
  }
}

void Sdlgl_player_selection(void)
{
  /* -AJA- Note that the initrole, initrace, initgend and initalign
   *       fields of the `flag' global have been set to ROLE_NONE in
   *       the initoptions() routine in src/options.c.  Those values
   *       may then be updated by the system code (sys/unixmain.c)
   *       depending on command line options.
   */
 
  int pick4u = 0;

  /* avoid unnecessary prompts further down */
  do_random_role_checks();
  rigid_role_checks();

  if (select_auto_pick(&pick4u) < 0)
    sdlgl_error("Quit from pick-for-you prompt.\n");

  for (;;)
  {
    if (select_a_role(pick4u) < 0)
      sdlgl_error("Quit from player selection menu.\n");

    if (select_a_race(pick4u) < 0)
      continue;

    if (select_a_gender(pick4u) < 0)
      continue;

    if (select_an_alignment(pick4u) < 0)
      continue;

    break;
  }

  if (flags.initrole < 0 || flags.initrace < 0 ||
      flags.initgend < 0 || flags.initalign < 0)
  {
    raw_printf("WARNING: failed to select a valid character !  "
        "(%d,%d,%d,%d)\n", flags.initrole, flags.initrace,
        flags.initgend, flags.initalign);

    /* do nothing -- let init_role() deal with it */
  }
}

/**
   Runs before player selection, and is used to determine which game
   mode to play in. If the choice is to continue or start a new game, it
   sets the character name to determine which (currently unused name for
   the player = new game, currently used name = continue that game);
   otherwise, it implements the choice itself then either loops or
   exits.

   This is based on the tty code, except that it uses a menu rather
   than rendering the game mode selection by hand.
*/
void
Sdlgl_game_mode_selection()
{
  int c;
  char** saved;
  char** sp;
  int plname_in_saved = 0;
  int plname_was_explicit = !!*plname;
  (void) plname_was_explicit; /* TODO: use this */

  winid menuwin;
  int n;
  menu_item *selected;
  anything any;

  saved = get_saved_games();

  if (*plname && saved && *saved) {
    for (sp = saved; *sp; sp++) {
      /* Note that this means that public servers need to prevent two
         users with the same name but different capitalisation. (If
         they aren't, it's likely to cause problems anyway...) */
      if (!strcmpi(plname, *sp)) {
        plname_in_saved = 1;
        /* Fix capitalisation of the name to match the saved game. */
        strncpy(plname, *sp, sizeof(plname)-1);
      }
    }
  } else if (*plname) {
    set_savefile_name();
    /* Let's just try to open the savefile directly to see if it exists */
    plname_in_saved = verify_savefile();
  }

retry_mode_selection:;
reread_char:
  menuwin = create_nhwindow(NHW_MENU);
  any.a_void = 0;
  start_menu(menuwin);

  any.a_int = 'c';
  add_menu(menuwin, NO_GLYPH, &any, 'c', 0, ATR_NONE,
           "Continue game", MENU_UNSELECTED);
  any.a_int = 'n';
  add_menu(menuwin, NO_GLYPH, &any, 'n', 0, ATR_NONE,
           "New game", MENU_UNSELECTED);
  any.a_int = 't';
  add_menu(menuwin, NO_GLYPH, &any, 't', 0, ATR_NONE,
           "Tutorial", MENU_UNSELECTED);
  any.a_int = 'm';
  add_menu(menuwin, NO_GLYPH, &any, 'm', 0, ATR_NONE,
           "Other modes", MENU_UNSELECTED);

  any.a_void = 0;
  add_menu(menuwin, NO_GLYPH, &any, 0, 0, ATR_NONE, "", FALSE);

  any.a_int = 's';
  add_menu(menuwin, NO_GLYPH, &any, 's', 0, ATR_NONE,
           "Continue game", MENU_UNSELECTED);
  any.a_int = 'o';
  add_menu(menuwin, NO_GLYPH, &any, 'o', 0, ATR_NONE,
           "New game", MENU_UNSELECTED);
  any.a_int = '?';
  add_menu(menuwin, NO_GLYPH, &any, '?', 0, ATR_NONE,
           "Tutorial", MENU_UNSELECTED);
  any.a_int = 'q';
  add_menu(menuwin, NO_GLYPH, &any, 'q', 0, ATR_NONE,
           "Other modes", MENU_UNSELECTED);

  end_menu(menuwin, "Welcome to AceHack!");
  n = select_menu(menuwin, PICK_ONE, &selected);
  destroy_nhwindow(menuwin);

  if (n > 0) {
    c = selected[0].item.a_int;
    free((genericptr_t)selected);
  } else goto retry_mode_selection;

#if 0
  /* TODO: This is tty code for graying out unavailable options;
     we should do that in SDL/GL code too */
  {
    if (c == 1) { /* gray out if there's no game to continue */
      if (!*plname && (!saved || !*saved)) xputs("\033[31m");
      else if (*plname && !plname_in_saved) xputs("\033[31m");
      /* and bold if there is */
      else xputs("\033[1m");
    }
    else if (c == 2) { /* gray out if a game must be continued */
      if (*plname && plname_in_saved) xputs("\033[31m");
    }
    else xputc(c); /* low-level as codes are in the file itself */
  }
#endif

  switch (c) {
  case 'c': /* continue game */
    if (*plname && plname_in_saved) break; /* just load by name */
    if (!saved || !*saved) goto reread_char;
    if (*plname && !plname_in_saved) goto reread_char;
    if (!saved[1]) {
      /* only one game to continue */
      (void) strncpy(plname, *saved, sizeof(plname)-1);
    } else {
      /* we'll have to ask which game to continue */
      winid win;
      win = create_nhwindow(NHW_MENU);
      start_menu(win);
      any.a_void = 0;
      for (sp = saved; *sp; sp++) {
        any.a_void = (void*) *sp;
        add_menu(win, NO_GLYPH, &any, 0, 0, ATR_NONE, *sp, MENU_UNSELECTED);
      }
      end_menu(win, "Continue which saved game?");
      c = select_menu(win, PICK_ONE, &selected);
      destroy_nhwindow(win);
      if (c != 1) {
        free((genericptr_t) selected);
        goto retry_mode_selection;
      }
      (void) strncpy(plname, (char*)selected[0].item.a_void, sizeof(plname)-1);
      free((genericptr_t) selected);
    }
    break;
  case 'n': case 't': case 'm': /* the new game options */
    /* if the name is forced by the -u option, don't give an option to
       start a new game, as we're forced onto the existing game */
    if (*plname && plname_in_saved) goto reread_char;
    if (!*plname) {
      Sdlgl_askname();
      if (saved)
        for (sp = saved; *sp; sp++) {
          if (!strcmpi(plname, *sp)) {
            pline("A game is running with that name already.");
            Sdlgl_dismiss_nhwindow(NHW_MESSAGE); /* force a --More-- */
            *plname = 0;
            goto retry_mode_selection;
          }
        }
    }
    if (c == 'm') {
      pline("TODO: implement game mode selection here");
      goto retry_mode_selection;
      do {
        c = 0; /* read from keyboard */
        switch(c) {
          /* -D, -X on command line are overriden by selecting a normal game
             explicitly */
        case 'n': wizard = FALSE; discover = FALSE; break;
        case 't': break;
        case 'd':
#ifdef WIZARD
          /* The rules here are a little complex:
             - with no -u (local play, private server play), anyone
               can enter debug mode;
             - with -u set (generally public server play), only an
               authorized user can enter debug mode.
             Thus people can enter it freely in their own compiles,
             but on public servers likely only the admin can enter it,
             as only they can tweak the command line. (-D works too
             to enter wizmode, whatever the -u option, as if it's
             given the player has control over the command line anyway.) */
          if (plname_was_explicit && strcmpi(plname,WIZARD)) {
            extern int wiz_error_flag; /* from unixmain.c */
            wiz_error_flag = TRUE;
          } else {
            wizard = TRUE;
            c = 'n';
            break;
          }
          /*FALLTHRU*/
#endif /* otherwise fall through, as debug mode isn't compiled in */
        case 'x': discover = TRUE; c = 'n'; break;
        case 's': solo = TRUE; c = 'n'; break;
        case '\033': goto retry_mode_selection;
        default: continue;
        }
      } while (c != 'n' && c != 't');
    }
    if (c == 't') flags.tutorial = 1;
    break;
  case 's': /* show high score list */
    prscore_interactive();
    goto retry_mode_selection;
  case 'o': /* options */
    doset();
    goto retry_mode_selection;
  case '?': /* help */
    dohelp();
    Sdlgl_dismiss_nhwindow(NHW_MESSAGE); /* force a --More-- */
    goto retry_mode_selection;
  case 'q': case '\033': /* exit */
    /* This is the standard Sdlgl method of exiting. */
    Sdlgl_exit_nhwindows(0);
    terminate(EXIT_SUCCESS);
    /*NOTREACHED*/
  default:
    goto reread_char;
  }

  if (saved) free_saved_games(saved);
}

#endif /* GL_GRAPHICS */
/*gl_role.c*/
