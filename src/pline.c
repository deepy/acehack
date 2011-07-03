/*	SCCS Id: @(#)pline.c	3.4	1999/11/28	*/
/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */
/* Modified 3 Jul 2011 by Alex Smith */
/* NetHack may be freely redistributed.  See license for details. */

#define NEED_VARARGS /* Uses ... */	/* comment line for pre-compiled headers */
#include "hack.h"
#include "epri.h"
#ifdef WIZARD
#include "edog.h"
#endif

#include <errno.h>

#ifdef OVLB

static boolean no_repeat = FALSE;

static char *FDECL(You_buf, (int));

static void FDECL(mp_log_and_handle_later,(const char *));
static void FDECL(mp_log_and_handle_now,(const char *));
static void FDECL((*mp_input_handler),(const char *)) = mp_log_and_handle_later;

/*VARARGS1*/
/* Note that these declarations rely on knowledge of the internals
 * of the variable argument handling stuff in "tradstdc.h"
 */

#if defined(USE_STDARG) || defined(USE_VARARGS)
static void FDECL(vpline, (const char *, va_list));

void
pline VA_DECL(const char *, line)
	VA_START(line);
	VA_INIT(line, char *);
	vpline(line, VA_ARGS);
	VA_END();
}

# ifdef USE_STDARG
static void
vpline(const char *line, va_list the_args) {
# else
static void
vpline(line, the_args) const char *line; va_list the_args; {
# endif

#else	/* USE_STDARG | USE_VARARG */

#define vpline pline

void
pline VA_DECL(const char *, line)
#endif	/* USE_STDARG | USE_VARARG */

	char pbuf[BUFSZ];
        void FDECL((*old_mp_input_handler),(const char *));

/* Do NOT use VA_START and VA_END in here... see above */

	if (!line || !*line) return;
	if (index(line, '%')) {
	    Vsprintf(pbuf,line,VA_ARGS);
	    line = pbuf;
	}
	if (!iflags.window_inited) {
	    raw_print(line);
	    return;
	}
#ifndef MAC
	if (no_repeat && !strcmp(line, toplines))
	    return;
#endif /* MAC */
	if (vision_full_recalc) vision_recalc(0);
	if (u.ux) flush_screen(1);		/* %% */

        /* Here's a fun potential infinite loop: pline() -> putstr()
           -> nhgetch() -> handle_mp_input() -> pline(). Even were it
           not for this, we want the game to appear to block upon
           --More--. So we want to temporarily switch into
           player-driving multiplayer handling mode even in the middle
           of a computer-driving cutscene. */

        old_mp_input_handler = mp_input_handler;
        mp_input_handler = mp_log_and_handle_later;
	putstr(WIN_MESSAGE, 0, line);
        mp_input_handler = old_mp_input_handler;
        if (mp_input_handler == mp_log_and_handle_now) mp_flush_log();
}

/*VARARGS1*/
void
Norep VA_DECL(const char *, line)
	VA_START(line);
	VA_INIT(line, const char *);
	no_repeat = TRUE;
	vpline(line, VA_ARGS);
	no_repeat = FALSE;
	VA_END();
	return;
}

/* work buffer for You(), &c and verbalize() */
static char *you_buf = 0;
static int you_buf_siz = 0;

static char *
You_buf(siz)
int siz;
{
	if (siz > you_buf_siz) {
		if (you_buf) free((genericptr_t) you_buf);
		you_buf_siz = siz + 10;
		you_buf = (char *) alloc((unsigned) you_buf_siz);
	}
	return you_buf;
}

void
free_youbuf()
{
	if (you_buf) free((genericptr_t) you_buf),  you_buf = (char *)0;
	you_buf_siz = 0;
}

/* `prefix' must be a string literal, not a pointer */
#define YouPrefix(pointer,prefix,text) \
 Strcpy((pointer = You_buf((int)(strlen(text) + sizeof prefix))), prefix)

#define YouMessage(pointer,prefix,text) \
 strcat((YouPrefix(pointer, prefix, text), pointer), text)

/*VARARGS1*/
void
You VA_DECL(const char *, line)
	char *tmp;
	VA_START(line);
	VA_INIT(line, const char *);
	vpline(YouMessage(tmp, "You ", line), VA_ARGS);
	VA_END();
}

/*VARARGS1*/
void
Your VA_DECL(const char *,line)
	char *tmp;
	VA_START(line);
	VA_INIT(line, const char *);
	vpline(YouMessage(tmp, "Your ", line), VA_ARGS);
	VA_END();
}

/*VARARGS1*/
void
You_feel VA_DECL(const char *,line)
	char *tmp;
	VA_START(line);
	VA_INIT(line, const char *);
	vpline(YouMessage(tmp, "You feel ", line), VA_ARGS);
	VA_END();
}


/*VARARGS1*/
void
You_cant VA_DECL(const char *,line)
	char *tmp;
	VA_START(line);
	VA_INIT(line, const char *);
	vpline(YouMessage(tmp, "You can't ", line), VA_ARGS);
	VA_END();
}

/*VARARGS1*/
void
pline_The VA_DECL(const char *,line)
	char *tmp;
	VA_START(line);
	VA_INIT(line, const char *);
	vpline(YouMessage(tmp, "The ", line), VA_ARGS);
	VA_END();
}

/*VARARGS1*/
void
There VA_DECL(const char *,line)
	char *tmp;
	VA_START(line);
	VA_INIT(line, const char *);
	vpline(YouMessage(tmp, "There ", line), VA_ARGS);
	VA_END();
}

/*VARARGS1*/
void
You_hear VA_DECL(const char *,line)
	char *tmp;
	VA_START(line);
	VA_INIT(line, const char *);
	if (Underwater)
		YouPrefix(tmp, "You barely hear ", line);
	else if (u.usleep)
		YouPrefix(tmp, "You dream that you hear ", line);
	else
		YouPrefix(tmp, "You hear ", line);
	vpline(strcat(tmp, line), VA_ARGS);
	VA_END();
}

/*VARARGS1*/
void
verbalize VA_DECL(const char *,line)
	char *tmp;
	if (!flags.soundok) return;
	VA_START(line);
	VA_INIT(line, const char *);
	tmp = You_buf((int)strlen(line) + sizeof "\"\"");
	Strcpy(tmp, "\"");
	Strcat(tmp, line);
	Strcat(tmp, "\"");
	vpline(tmp, VA_ARGS);
	VA_END();
}

/*VARARGS1*/
/* Note that these declarations rely on knowledge of the internals
 * of the variable argument handling stuff in "tradstdc.h"
 */

#if defined(USE_STDARG) || defined(USE_VARARGS)
static void FDECL(vraw_printf,(const char *,va_list));

void
raw_printf VA_DECL(const char *, line)
	VA_START(line);
	VA_INIT(line, char *);
	vraw_printf(line, VA_ARGS);
	VA_END();
}

# ifdef USE_STDARG
static void
vraw_printf(const char *line, va_list the_args) {
# else
static void
vraw_printf(line, the_args) const char *line; va_list the_args; {
# endif

#else  /* USE_STDARG | USE_VARARG */

void
raw_printf VA_DECL(const char *, line)
#endif
/* Do NOT use VA_START and VA_END in here... see above */

	if(!index(line, '%'))
	    raw_print(line);
	else {
	    char pbuf[BUFSZ];
	    Vsprintf(pbuf,line,VA_ARGS);
	    raw_print(pbuf);
	}
}


/*VARARGS1*/
void
impossible VA_DECL(const char *, s)
	VA_START(s);
	VA_INIT(s, const char *);
	if (program_state.in_impossible)
		panic("impossible called impossible");
	program_state.in_impossible = 1;
	{
	    char pbuf[BUFSZ];
	    Vsprintf(pbuf,s,VA_ARGS);
	    paniclog("impossible", pbuf);
	}
	vpline(s,VA_ARGS);
	pline("Program in disorder! Try saving and restoring.");
	program_state.in_impossible = 0;
	VA_END();
}

const char *
align_str(alignment)
    aligntyp alignment;
{
    switch ((int)alignment) {
	case A_CHAOTIC: return "chaotic";
	case A_NEUTRAL: return "neutral";
	case A_LAWFUL:	return "lawful";
	case A_NONE:	return "unaligned";
    }
    return "unknown";
}

void
mstatusline(mtmp)
register struct monst *mtmp;
{
	aligntyp alignment;
	char info[BUFSZ], monnambuf[BUFSZ];

	if (mtmp->ispriest || mtmp->data == &mons[PM_ALIGNED_PRIEST]
				|| mtmp->data == &mons[PM_ANGEL])
		alignment = EPRI(mtmp)->shralign;
	else
		alignment = mtmp->data->maligntyp;
	alignment = (alignment > 0) ? A_LAWFUL :
		(alignment < 0) ? A_CHAOTIC :
		A_NEUTRAL;

	info[0] = 0;
	if (mtmp->mtame) {	  Strcat(info, ", tame");
#ifdef WIZARD
	    if (wizard) {
		Sprintf(eos(info), " (%d", mtmp->mtame);
		if (!mtmp->isminion)
		    Sprintf(eos(info), "; hungry %ld; apport %d",
			EDOG(mtmp)->hungrytime, EDOG(mtmp)->apport);
		Strcat(info, ")");
	    }
#endif
	}
	else if (mtmp->mpeaceful) Strcat(info, ", peaceful");
	if (mtmp->meating)	  Strcat(info, ", eating");
	if (mtmp->mcan)		  Strcat(info, ", cancelled");
	if (mtmp->mconf)	  Strcat(info, ", confused");
	if (mtmp->mblinded || !mtmp->mcansee)
				  Strcat(info, ", blind");
	if (mtmp->mstun)	  Strcat(info, ", stunned");
	if (mtmp->msleeping)	  Strcat(info, ", asleep");
#if 0	/* unfortunately mfrozen covers temporary sleep and being busy
	   (donning armor, for instance) as well as paralysis */
	else if (mtmp->mfrozen)	  Strcat(info, ", paralyzed");
#else
	else if (mtmp->mfrozen || !mtmp->mcanmove)
				  Strcat(info, ", can't move");
#endif
				  /* [arbitrary reason why it isn't moving] */
	else if (mtmp->mstrategy & STRAT_WAITMASK)
				  Strcat(info, ", meditating");
	else if (mtmp->mflee)	  Strcat(info, ", scared");
	if (mtmp->mtrapped)	  Strcat(info, ", trapped");
	if (mtmp->mspeed)	  Strcat(info,
					mtmp->mspeed == MFAST ? ", fast" :
					mtmp->mspeed == MSLOW ? ", slow" :
					", ???? speed");
	if (mtmp->mundetected)	  Strcat(info, ", concealed");
	if (mtmp->minvis)	  Strcat(info, ", invisible");
	if (mtmp == u.ustuck)	  Strcat(info,
			(sticks(youmonst.data)) ? ", held by you" :
				u.uswallow ? (is_animal(u.ustuck->data) ?
				", swallowed you" :
				", engulfed you") :
				", holding you");
#ifdef STEED
	if (mtmp == u.usteed)	  Strcat(info, ", carrying you");
#endif

	/* avoid "Status of the invisible newt ..., invisible" */
	/* and unlike a normal mon_nam, use "saddled" even if it has a name */
	Strcpy(monnambuf, x_monnam(mtmp, ARTICLE_THE, (char *)0,
	    (SUPPRESS_IT|SUPPRESS_INVISIBLE), FALSE));

	pline("Status of %s (%s):  Level %d  HP %d(%d)  AC %d%s.",
		monnambuf,
		align_str(alignment),
		mtmp->m_lev,
		mtmp->mhp,
		mtmp->mhpmax,
		find_mac(mtmp),
		info);
}

void
ustatusline()
{
	char info[BUFSZ];

	info[0] = '\0';
	if (Sick) {
		Strcat(info, ", dying from");
		if (u.usick_type & SICK_VOMITABLE)
			Strcat(info, " food poisoning");
		if (u.usick_type & SICK_NONVOMITABLE) {
			if (u.usick_type & SICK_VOMITABLE)
				Strcat(info, " and");
			Strcat(info, " illness");
		}
	}
	if (Stoned)		Strcat(info, ", solidifying");
	if (Slimed)		Strcat(info, ", becoming slimy");
	if (Strangled)		Strcat(info, ", being strangled");
	if (Vomiting)		Strcat(info, ", nauseated"); /* !"nauseous" */
	if (Confusion)		Strcat(info, ", confused");
	if (Blind) {
	    Strcat(info, ", blind");
	    if (u.ucreamed) {
		if ((long)u.ucreamed < Blinded || Blindfolded
						|| !haseyes(youmonst.data))
		    Strcat(info, ", cover");
		Strcat(info, "ed by sticky goop");
	    }	/* note: "goop" == "glop"; variation is intentional */
	}
	if (Stunned)		Strcat(info, ", stunned");
#ifdef STEED
	if (!u.usteed)
#endif
	if (Wounded_legs) {
	    const char *what = body_part(LEG);
	    if ((Wounded_legs & BOTH_SIDES) == BOTH_SIDES)
		what = makeplural(what);
				Sprintf(eos(info), ", injured %s", what);
	}
	if (Glib)		Sprintf(eos(info), ", slippery %s",
					makeplural(body_part(HAND)));
	if (u.utrap)		Strcat(info, ", trapped");
	if (Fast)		Strcat(info, Very_fast ?
						", very fast" : ", fast");
	if (u.uundetected)	Strcat(info, ", concealed");
	if (Invis)		Strcat(info, ", invisible");
	if (u.ustuck) {
	    if (sticks(youmonst.data))
		Strcat(info, ", holding ");
	    else
		Strcat(info, ", held by ");
	    Strcat(info, mon_nam(u.ustuck));
	}

	pline("Status of %s (%s%s):  Level %d  HP %d(%d)  AC %d%s.",
		plname,
		    (u.ualign.record >= 20) ? "piously " :
		    (u.ualign.record > 13) ? "devoutly " :
		    (u.ualign.record > 8) ? "fervently " :
		    (u.ualign.record > 3) ? "stridently " :
		    (u.ualign.record == 3) ? "" :
		    (u.ualign.record >= 1) ? "haltingly " :
		    (u.ualign.record == 0) ? "nominally " :
					    "insufficiently ",
		align_str(u.ualign.type),
		Upolyd ? mons[u.umonnum].mlevel : u.ulevel,
		Upolyd ? u.mh : u.uhp,
		Upolyd ? u.mhmax : u.uhpmax,
		u.uac,
		info);
}

void
self_invis_message()
{
	pline("%s %s.",
	    Hallucination ? "Far out, man!  You" : "Gee!  All of a sudden, you",
	    See_invisible ? "can see right through yourself" :
		"can't see yourself");
}

/* Remote pline is a little more interesting. We put all the multiplayer
   input handling code here, as remote pline is its major use, other than
   yield synchronization, which is arguably even more important, but falls
   into the general case. */
static char partial_mp_input_buf[BUFSZ*2] = "";

static boolean mp_ackflag = FALSE;
static boolean mp_errflag = FALSE;
static boolean mp_yieldflag = FALSE;

static boolean mp_stopgetch = FALSE;
static boolean mp_stopgetch_on_aey = FALSE;

boolean
handle_mp_input()
{
  char buf[BUFSZ*2];
  char *bp;
  int fd = multiplayer_pipe_fd();
  int rs;
  if (fd < 0) panic("Multiplayer routines called with no communication pipe");
  errno = 0;
  /* EINTR is the only error that isn't fatal here. (I'm not sure if it can
     happen, but recovering from it is trivial, so we may as well...) */
  rs = read(fd, buf, BUFSZ*2-1);
  while (rs < 0 && errno == EINTR) {errno = 0; rs = read(fd, buf, BUFSZ-1);}
  if (rs < 0) panic("Read error in mp_log_and_handle_later");
  /* EOF can't happen, as we have one of the write handles to the pipe ourselves. */
  if (rs == 0) panic("Impossible EOF on multiplayer communication pipe");
  buf[rs] = 0;
  /* In case we had only partial input last time, add the new input to
     the partial input buffer. We can't overflow this because the
     permissions are set to allow only other NetHack processes to
     write to the FIFOs. (If we were using network sockets, which are
     writable by anyone, we'd need much tighter checks.) */
  Strcpy(eos(partial_mp_input_buf),buf);
  /* Break the input into lines, and send them all to the handler. */
  while ((bp = strchr(partial_mp_input_buf,'\n'))) {
    *bp = '\0';
    /* We can't use partial_mp_input_buf directly here. The reason is
       that mp_input_handler might call pline() or even nhgetch(). */
    Strcpy(buf,partial_mp_input_buf);
    /* /Not/ Strcpy for this; the input and output might overlap. */
    memmove(partial_mp_input_buf,bp+1,strlen(bp+1)+1);
    /* Now partial_mp_input_buf is sane again, we can handle the input. */
    (*mp_input_handler)(buf);
  }

  /* If the handler involves flushing, it may set ack/err/yield flags.
     That might make us want to break out of the getch altogether. */
  if (mp_stopgetch_on_aey && (mp_ackflag || mp_errflag || mp_yieldflag))
    mp_stopgetch = TRUE;

  return mp_stopgetch;
}

struct mp_pline_log_entry {
  struct mp_pline_log_entry *next;
  char entry_type;
  char* entry_data; /* allocated or NULL */
};

static struct mp_pline_log_entry *first_mpple = NULL;
static struct mp_pline_log_entry *last_mpple = NULL;

#define MAX_INVITATION_COUNT 10
static char yielder_locknames[BUFSZ][MAX_INVITATION_COUNT];
static int yielder_locknames_p = 0;
static boolean yielder_locknames_track = FALSE;

static char levelmerger_locknames[BUFSZ][MAX_INVITATION_COUNT];
static xchar levelmerger_dnums[MAX_INVITATION_COUNT];
static xchar levelmerger_lnums[MAX_INVITATION_COUNT];
static int levelmerger_locknames_p = 0;

static int turns_behind = 0;

/* The default handling routine for input from other programs.

   This is used in player-driving mode; that is, when everything that
   happens appears to be in response to player input.

   There are several sorts of input we could get:

   lockname space p message newline
   remote pline; we log this and print it when we get back to the main
   loop, or other place which wants to respond to a remote pline; we
   respond with the standard acknowledgement, a newline

   lockname space y newline
   yield; we log this, and hang onto control until told to yield back
   or send a remote message

   lockname space q newline
   query presence; we respond to this with the standard newline,
   because we are here, and don't log it

   lockname space t newline
   "time passes"; respond with standard newline, and immediately
   log that we're a turn behind (the turn itself will be played out
   on next yield)

   lockname space g dnum dlevel newline
   goto level request; respond with a newline, and log it
   the dnum/dlevel numbers are represented as value+11, to prevent
   them being newlines or nulls

   newline
   standard "OK" reply; this can only happen if we're in a nested pline
   inside a computer-driving mode, in which case we log it and don't
   re-reply (and besides, we can't reply without a lockname to send to)

   ! newline
   standard error reply, a similar situation to the case above (e.g.
   replying to a yield to an advertised join with "don't yield to me,
   I decline your invite")

   Eventually there will probably also be something to give
   non-textual feedback; something to show monsters moving around on
   the screen, for instance. Maybe even sparkle, if we don't force
   that off in multiplayer.
*/
static void
mp_log_and_handle_later(s)
const char *s;
{
  char lockname[BUFSZ];
  const char *bp;
  boolean log = TRUE, respond = TRUE;
  /* Handle acknowledgements */
  if (!*s || *s == '!') {
    struct mp_pline_log_entry* newmpple =
      (struct mp_pline_log_entry*) alloc(sizeof (struct mp_pline_log_entry));
    if (last_mpple) last_mpple->next = newmpple;
    else last_mpple = first_mpple = newmpple;
    newmpple->entry_type = *s ? *s : '\n';
    newmpple->next = NULL;
    newmpple->entry_data = NULL;
    return;
  }
  /* If this isn't a simple acknowledgement, we have a lockname followed
     by a space, and a message. */
  bp = strchr(s, ' ');
  if (!bp) panic("Multiplayer message was in the incorrect format");
  memcpy(lockname, s, bp-s);
  lockname[bp-s] = 0;
  bp++;

  /* Parse the message type. */
  switch (*bp) {
  case 'y':
    respond = FALSE;
    /* During a join-game sequence, we want to record who it was who
       tried to yield to us. (Especially because they'll be waiting
       for an ack or err in response.) We store up to ten names;
       past that, we just err-respond immediately. */
    if (yielder_locknames_track) {
      if (yielder_locknames_p >= MAX_INVITATION_COUNT)
        mp_message(lockname, "!\n");
      else
        Strcpy(yielder_locknames[yielder_locknames_p++], lockname);
    }
    break;
  case 'g':
    /* A similar case to the one above. This time, we don't log,
       because the levelmerger log is synchronized to player-driven
       rather than remote-driven events. */
    log = FALSE;
    respond = FALSE;
    if (levelmerger_locknames_p >= MAX_INVITATION_COUNT)
      mp_message(lockname, "!\n");
    else {
      levelmerger_dnums[levelmerger_locknames_p] = bp[1] - 11;
      levelmerger_lnums[levelmerger_locknames_p] = bp[2] - 11;      
      Strcpy(levelmerger_locknames[levelmerger_locknames_p++], lockname);
    }
    break;
  case 'q': log = FALSE; break;
  case 'p': break;
  case 't': log = FALSE; turns_behind++; break;
  default: panic("Invalid multiplayer message type");
  }

  if (respond)
    mp_message(lockname, "\n");

  if (log) {
    struct mp_pline_log_entry* newmpple =
      (struct mp_pline_log_entry*) alloc(sizeof (struct mp_pline_log_entry));
    if (last_mpple) last_mpple->next = newmpple;
    else last_mpple = first_mpple = newmpple;
    newmpple->entry_type = *bp;
    newmpple->next = NULL;
    if (bp[1]) {
      char *newdata = strdup(bp+1);
      if (!newdata) panic("Allocation failure handling multiplayer message");
      newmpple->entry_data = newdata;
    } else newmpple->entry_data = NULL;
  }
}

/* The equivalent of the above, for remote-driving.
   Much simpler, because we can just reuse the code from two other
   functions. */
static void
mp_log_and_handle_now(s)
const char *s;
{
  mp_log_and_handle_later(s);
  mp_flush_log();
}

/* Actually handle the logged events. This is called in remote-driving
   mode whenever an event is handled, and also when leaving a nested
   player-driving mode and returning to remote-driving mode (e.g. if a
   lot of remote plines come at once, the first --More-- will shift to
   player-driving mode, and we want to show the next few plines after
   the --More-- is cleared). It may also make sense to call this in
   the main loop, so that chat messages can be implemented without
   costing turns, but there's probably a neater way to do that without
   messing with the controls or deleting existing plines (or popping
   up a --More-- as the player's about to press a key). */
void
mp_flush_log()
{
  struct mp_pline_log_entry* this_mpple;
  while (first_mpple) {
    /* It's important to leave the mpple queue in a consistent state
       before anything that might call pline() happens. The log
       might be appended to while it's being flushed (most likely
       due to a flood of rplines spaced out in time). */
    this_mpple = first_mpple;
    if (first_mpple == last_mpple) last_mpple = NULL;
    first_mpple = first_mpple->next;
    switch (this_mpple->entry_type) {
    case '\n': mp_ackflag = TRUE; break;
    case '!':  mp_errflag = TRUE; break;
    case 'y':  mp_yieldflag = TRUE; break;
    case 'p': 
      pline("%s",*(this_mpple->entry_data) == '!' ?
            this_mpple->entry_data+1 :
            this_mpple->entry_data);
      if (*(this_mpple->entry_data) == '!') suppress_more();
      break;
    default: panic("Impossible mpple logged");
    }
    if (this_mpple->entry_data) free(this_mpple->entry_data);
    free(this_mpple);
  }
  curs(WIN_MESSAGE, 1, 0);
}

/* Logging for yielders, for use in the joingame sequence. */
void
mp_track_yielders(t)
boolean t;
{
  yielder_locknames_track = t;
}
const char *
mp_yielder_name()
{
  if (!yielder_locknames_p) return NULL;
  return yielder_locknames[--yielder_locknames_p];
}

/* A little more complex than the above case, as we only care about
   people whose merge request was for our current level (others
   get an error return, telling them to ask someone else or take
   ownership). */
const char *
mp_levelmerger_name()
{
  const char *rv;
  if (!levelmerger_locknames_p) return NULL;
  rv = levelmerger_locknames[--levelmerger_locknames_p];
  if (u.uz.dnum   == levelmerger_dnums[levelmerger_locknames_p] &&
      u.uz.dlevel == levelmerger_lnums[levelmerger_locknames_p])
    return rv;
  mp_message(rv, "!\n");
  return mp_levelmerger_name();
}

/* Time handling. Calling this resets the counter to 0. */
int
mp_turns_behind()
{
  int rv = turns_behind;
  turns_behind = 0;
  return rv;
}

/* Sends a message to a single process on a multiplayer control pipe.
   This should be newline-and-null-terminated, and must contain the
   lockname if the message requires one. */
void
mp_message(lockname, msgstr)
const char *lockname;
const char *msgstr;
{
  /* TODO: handle unexpected failures to open the file, which could
     happen due to crashed processes; is the situation recoverable, or
     do we have to panic/trickery? */
  int fd = other_multiplayer_pipe_fd(lockname, 'c');
  if (fd < 0) {
    /* Avoid infinite loop. */
    teardown_multiplayer_pipe('a');
    teardown_multiplayer_pipe('c');
    pline("Multiplayer process with lockname %s (errno %d)", lockname, errno);
    done(TRICKED);
  }
  if (write(fd, msgstr, strlen(msgstr)) < 0) {
    panic("Cannot write to remote multiplayer control pipe");
  }
  close(fd);
}

/* Sends a message to a single process on a multiplayer control pipe.
   This should be null-terminated; the lockname will be prefixed and a
   newline will be suffixed. Then waits for an acknowledgement.
   Returns 0 for error acknowledgement, 1 for OK acknowledgement. If
   the third argument is false, ignores error acknowledgements on the
   basis that they're likely spurious.

   TODO: Perhaps place an alarm here? The acknowledgement should be
   near-instant, the only thing that can potentially hold it up is
   timeconsuming animations at the other end. (Should sparkle be
   forced off in multiplayer?)
*/
int
mp_message_and_reply(lockname, msgprefix, msgstr, allow_errors)
const char *lockname;
const char *msgstr, *msgprefix;
boolean allow_errors;
{
  char buf[BUFSZ];
  sprintf(buf, "%s %s%s\n", mplock, msgprefix, msgstr ? msgstr : "");
  mp_message(lockname, buf);
  for (;;)
    switch (mp_await_reply_or_yield()) {
    case 0: if (allow_errors) return 0; break;
    case 1: return 1;
    case 2: panic("Received yield during wait for acknowledgement");
    case 3: break;
    default: impossible("Invalid mp_await_reply_or_yield return"); break;
    }
}

/* Waits for a reply or yield to come in on our control pipe.

   Return value: 0 for error acknowledgement, 1 for OK acknowledgement,
   2 for yield, 3 if the user cancelled by pressing ESC. (A simultaneous
   cancel and acknowledgement will ignore the cancel.)

   This will probably eventually allow some sort of interaction while
   waiting. (Several commands are meaningful while waiting for a yield,
   for instance, such as 'i'; while waiting for an acknowledgement,
   though, a timeout would be more sensible.)

   TODO: This should ideally change screen appearance to mark the
   change of driving mode, which is a player-visible concept ("it's
   not my turn"). Moving the cursor to the corner is OK, but not
   visible enough.
*/
int
mp_await_reply_or_yield()
{
        void FDECL((*old_mp_input_handler),(const char*));
        int rv = 3;

        /* Switch to remote-driving. */
        old_mp_input_handler = mp_input_handler;
        mp_input_handler = mp_log_and_handle_now;
        mp_flush_log();

        /* Repeatedly listen for user commands until one of the
           ack/err/yield commands comes in. */
        mp_stopgetch_on_aey = TRUE;
        mp_ackflag = mp_errflag = mp_yieldflag = FALSE;
        /* TODO: Implement some commands */
        while (!mp_stopgetch) {
          curs(WIN_MESSAGE, 1, 0);
          if (nhgetch() == '\033') break;
          if (!mp_stopgetch) {
            pline("Communicating...");
            suppress_more();
          }
        }
        mp_stopgetch_on_aey = FALSE;
        mp_stopgetch = FALSE;
        if (mp_ackflag) rv = 1;
        if (mp_errflag) rv = 0;
        /* Acks/errs can be spurious in some race conditions involving
           cancelled invitations. Yields can't be. As a result, it's
           possible we're waiting for a yield - and only a yield - in
           a loop, in which case a spurious ack and a genuine yield
           should report the yield value in order to avoid an infninite
           loop. */
        if (mp_yieldflag) rv = 2;
        mp_ackflag = mp_errflag = mp_yieldflag = FALSE;

        /* Switch back to the original driving mode. */
        mp_input_handler = old_mp_input_handler;

        return rv;
}

/* The whole machinery above has three main purposes; join game
   negotiation, yielding, and remote pline. Here's the third case. */

#if defined(USE_STDARG) || defined(USE_VARARGS)
static void FDECL(rvpline, (const char *, va_list));

void
rpline VA_DECL(const char *, line)
	VA_START(line);
	VA_INIT(line, char *);
	rvpline(line, VA_ARGS);
	VA_END();
}

# ifdef USE_STDARG
static void
rvpline(const char *line, va_list the_args) {
# else
static void
rvpline(line, the_args) const char *line; va_list the_args; {
# endif

#else	/* USE_STDARG | USE_VARARG */

#define rvpline rpline

void
rpline VA_DECL(const char *, line)
#endif	/* USE_STDARG | USE_VARARG */

	char pbuf[BUFSZ];

	struct monst *mtmp, *nmtmp;

/* Do NOT use VA_START and VA_END in here... see above */

	if (!line || !*line) return;
	if (index(line, '%')) {
	    Vsprintf(pbuf,line,VA_ARGS);
	    line = pbuf;
	}
        
        /* We loop over mp_players in order to find out who should
           receive a message. */
        for(mtmp = fmon; mtmp; mtmp = nmtmp) {
            nmtmp = mtmp->nmon;

            if (!is_mp_player(mtmp)) continue;

            mp_message_and_reply(is_mp_player(mtmp), "p", line, FALSE);
        }

}

/*VARARGS1*/
/* Note that the grammar here will have to be changed around a bit, as
   the player will be talked about in the third person from the remote
   point of view. */
void
rYou VA_DECL(const char *, line)
	char *tmp;
	VA_START(line);
	VA_INIT(line, const char *);
	rvpline(YouMessage(tmp, plname, line), VA_ARGS);
	VA_END();
}


#endif /* OVLB */
/*pline.c*/
