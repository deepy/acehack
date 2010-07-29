/*	SCCS Id: @(#)func_tab.h 3.4	1992/04/03	*/
/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */
/* NetHack may be freely redistributed.  See license for details. */

#ifndef FUNC_TAB_H
#define FUNC_TAB_H

struct func_tab {
  char f_char;
  boolean can_if_buried;
  int NDECL((*f_funct));
  const char *f_text;
};

struct ext_func_tab {
  const char *ef_txt, *ef_desc; /* text, description */
  int NDECL((*ef_funct)); /* function to call */
  boolean can_if_buried;
  int prio; /* lower makes it more likely to autocomplete */
  char binding1, binding2, binding3; /* default keybindings */
  char replacewithkey; /* to implement movement and redo */
};

extern struct ext_func_tab extcmdlist[];

#endif /* FUNC_TAB_H */
