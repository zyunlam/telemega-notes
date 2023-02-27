/*
 * Copyright © 2023 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */
#include    <stdio.h>
#include    <stdlib.h>
#include    <ctype.h>
#include    <strings.h>
#include    <poll.h>
#include    <X11/Xos.h>
#include    <X11/Xlib.h>
#include    <X11/Xutil.h>
#include    <X11/Xatom.h>

#ifndef DEFAULT_VISUAL
#define DEFAULT_VISUAL 0
#endif

#ifndef DEFAULT_ROP
#define DEFAULT_ROP "copy"
#endif

#ifndef DEFAULT_WIDTH
#define DEFAULT_WIDTH 200
#endif

#ifndef DEFAULT_HEIGHT
#define DEFAULT_HEIGHT 200
#endif

#ifndef DEFAULT_FONT
#define DEFAULT_FONT 0
#endif

#ifndef DEFAULT_LINE_WIDTH
#define DEFAULT_LINE_WIDTH 0
#endif

#ifndef DEFAULT_LINE_STYLE
#define DEFAULT_LINE_STYLE NULL
#endif

#ifndef DEFAULT_FILL_STYLE
#define DEFAULT_FILL_STYLE NULL
#endif

#ifndef DEFAULT_CAP_STYLE
#define DEFAULT_CAP_STYLE NULL
#endif

#ifndef DEFAULT_JOIN_STYLE
#define DEFAULT_JOIN_STYLE NULL
#endif

#ifndef DEFAULT_DASHES
#define DEFAULT_DASHES	0
#define DEFAULT_NUM_DASHES 0
#endif

#ifdef ALL_MOTION
#define MOTION	PointerMotionMask
#else
#define MOTION	ButtonMotionMask
#endif

#ifndef EXTRA_EVENTS
#define EXTRA_EVENTS 0
#endif

unsigned long   fg_pixel;
unsigned long   bg_pixel;
int		default_rop;
XFontStruct	*default_font;

typedef struct _RopNames { char	*name; int  rop; } RopNameRec, *RopNamePtr;

RopNameRec ropNames[] = {
	{ "clear",    	GXclear,	}, /* 0 */
	{ "and",		GXand,		}, /* src AND dst */
	{ "andReverse",	GXandReverse,	}, /* src AND NOT dst */
	{ "copy",		GXcopy,		}, /* src */
	{ "andInverted",	GXandInverted,	}, /* NOT src AND dst */
	{ "noop",		GXnoop,		}, /* dst */
	{ "xor",		GXxor,		}, /* src XOR dst */
	{ "or",		GXor,		}, /* src OR dst */
	{ "nor",		GXnor,		}, /* NOT src AND NOT dst */
	{ "equiv",	GXequiv,	}, /* NOT src XOR dst */
	{ "invert",	GXinvert,	}, /* NOT dst */
	{ "orReverse",	GXorReverse,	}, /* src OR NOT dst */
	{ "copyInverted",	GXcopyInverted,	}, /* NOT src */
	{ "orInverted",	GXorInverted,	}, /* NOT src OR dst */
	{ "nand",		GXnand,		}, /* NOT src OR NOT dst */
	{ "set",		GXset,		}, /* 1 */
};

static int
RopNameToRop (char *name)
{
    int	rop;

    for (rop = 0; rop < 16; rop++) {
	if (!strcmp (name, ropNames[rop].name)) {
	    return ropNames[rop].rop;
	}
    }
    return GXcopy;
}

static void
Error (char *s, char *a)
{
    fprintf (stderr, s, a);
    fprintf (stderr, "\n");
    exit (1);
}

static int
MatchCapStyle (char *s)
{
    if (!strcmp (s, "round"))
	return CapRound;
    if (!strcmp (s, "projecting"))
	return CapProjecting;
    if (!strcmp (s, "butt"))
	return CapButt;
    if (!strcmp (s, "notlast"))
	return CapNotLast;
    Error ("Unknown cap style %s", s);
    return CapNotLast;
}

static int
MatchJoinStyle (char *s)
{
    if (!strcmp (s, "round"))
	return JoinRound;
    if (!strcmp (s, "miter"))
	return JoinMiter;
    if (!strcmp (s, "bevel"))
	return JoinBevel;
    Error ("Unknown join style %s", s);
    return JoinBevel;
}

static int
MatchFillStyle(char *s)
{
    if (!strcmp (s, "solid"))
	return FillSolid;
    if (!strcmp (s, "tiled"))
	return FillTiled;
    if (!strcmp (s, "stippled"))
	return FillStippled;
    if (!strcmp (s, "opaquestippled"))
	return FillOpaqueStippled;
    Error ("Unknown fill style %s", s);
    return FillSolid;
}

static int
MatchLineStyle(char *s)
{
    if (!strcmp (s, "solid"))
	return LineSolid;
    if (!strcmp (s, "onoff"))
	return LineOnOffDash;
    if (!strcmp (s, "double"))
	return LineDoubleDash;
    Error ("Unknown line style %s", s);
    return LineSolid;
}

void
HandleButtonPress(Display *dpy, Window win, GC gc, XEvent *ev);

void
HandleButtonRelease(Display *dpy, Window win, GC gc, XEvent *ev);

void
HandleMotionNotify(Display *dpy, Window win, GC gc, XEvent *ev);

#ifdef TRACK_POINTER

#define PASS_BUTTONS
typedef struct _Position {
    int	seen;
    int	start_x, start_y;
    int	cur_x, cur_y;
    int	end_x, end_y;
} PositionRec, *PositionPtr;

void
Draw(Display *dpy, Window win, GC gc, PositionRec positions[5]);

void
Undraw(Display *dpy, Window win, GC gc, PositionRec positions[5]);

PositionRec current_positions[5];

static void
UpdatePositions (unsigned state, int x, int y)
{
    int	i;

    state >>= 8;
    for (i = 0; i < 5; i++)
    {
	if (state & (1 << i)) {
	    current_positions[i].cur_x = x;
	    current_positions[i].cur_y = y;
	}
    }
}

void
HandleButtonPress (Display *dpy, Window win, GC gc, XEvent *ev)
{
    XButtonEvent    *bev = (XButtonEvent *) ev;

    Undraw (dpy, win, gc, current_positions);
    current_positions[bev->button - 1].seen = 1;
    current_positions[bev->button - 1].start_x = bev->x;
    current_positions[bev->button - 1].start_y = bev->y;
    current_positions[bev->button - 1].cur_x = bev->x;
    current_positions[bev->button - 1].cur_y = bev->y;
    current_positions[bev->button - 1].end_x = bev->x;
    current_positions[bev->button - 1].end_y = bev->y;
    UpdatePositions (bev->state, bev->x, bev->y);
    Draw (dpy, win, gc, current_positions);
}

void
HandleButtonRelease (Display *dpy, Window win, GC gc, XEvent *ev)
{
    XButtonEvent    *bev = (XButtonEvent *) ev;

    Undraw (dpy, win, gc, current_positions);
    UpdatePositions (bev->state, bev->x, bev->y);
    current_positions[bev->button - 1].end_x = bev->x;
    current_positions[bev->button - 1].end_y = bev->y;
    Draw (dpy, win, gc, current_positions);
}

void
HandleMotionNotify (Display *dpy, Window win, GC gc, XEvent *ev)
{
    XMotionEvent    *mev = (XMotionEvent *) ev;

    Undraw (dpy, win, gc, current_positions);
    UpdatePositions (mev->state, mev->x, mev->y);
    Draw (dpy, win, gc, current_positions);
}

static inline void
DisplayPositions (Display *dpy, Window win, GC gc, PositionRec positions[5])
{
    static char	text[1024];
    static int	hastext;
    int		pos;
    int		i;
    int		dir, font_ascent, font_descent;
    XCharStruct	overall;

    XTextExtents (default_font, text, (int) strlen(text),
	    &dir, &font_ascent, &font_descent, &overall);
    if (hastext)
	    XClearArea (dpy, win, 0, 0, (unsigned) overall.width, (unsigned) (font_ascent + font_descent), False);
    pos = 0;
    for (i = 0; i < 5; i++)
    {
	if (positions[i].seen)
	{
	    if (pos)
		text[pos++] = ',';
	    sprintf (text + pos, "%1d: (%4d,%4d),(%4d,%4d)",
		i, positions[i].start_x, positions[i].start_y,
		positions[i].cur_x, positions[i].cur_y);
	    pos = (int) strlen (text);
	}
    }
    XDrawString (dpy, win, gc, 0, font_ascent, text, pos);
    hastext = 1;
}

#endif

static inline void
SetToFg (Display *dpy, GC gc)
{
    XSetForeground (dpy, gc, fg_pixel);
    XSetFunction (dpy, gc, default_rop);
}

static inline void
SetToBg (Display *dpy, GC gc)
{
    XSetForeground (dpy, gc, bg_pixel);
    XSetFunction (dpy, gc, GXcopy);
}
static void
Usage (char *program)
{
    int	i;
    fprintf (stderr, "Usage: %s\n", program);
    fprintf (stderr, "\t-display <display-name>\n");
    fprintf (stderr, "\t-rop {");
    for (i = 0; i < 16; i++) {
	fprintf (stderr, "%s", ropNames[i].name);
	if (i == 15)
	    fprintf (stderr, "}\n");
	else if (i % 4 == 3)
	    fprintf (stderr, ",\n\t\t");
	else
	    fprintf (stderr, ", ");
    }
    fprintf (stderr, "\t-fg <fg color name>\n");
    fprintf (stderr, "\t-bg <bg color name>\n");
    fprintf (stderr, "\t-fg_pixel <fg pixel value>\n");
    fprintf (stderr, "\t-bg_pixel <bg pixel value>\n");
    fprintf (stderr, "\t-fn <font name>\n");
    fprintf (stderr, "\t-geometry <geometry>\n");
    fprintf (stderr, "\t-lw <line width>\n");
    fprintf (stderr, "\t-cap {round, projecting, butt, notlast}\n");
    fprintf (stderr, "\t-join {round, miter, bevel}\n");
    fprintf (stderr, "\t-fill {solid, tiled, stippled, opaquestippled}\n");
    fprintf (stderr, "\t-linestyle {solid, onoff, double}\n");
    fprintf (stderr, "\t-bitmap <bitmap file name>\n");
    fprintf (stderr, "\t-dashes [<dash value> ...]\n");
    exit (1);
}

char    *dpy_name;
char    *rop_name;
char    *fg;
char    *bg;
char    *bitmap_file;
char	*cap_name = DEFAULT_CAP_STYLE;
char	*join_name = DEFAULT_JOIN_STYLE;
char	*fill_name = DEFAULT_FILL_STYLE;
char	*line_name = DEFAULT_LINE_STYLE;
char	*font_name = DEFAULT_FONT;
double	line_width = DEFAULT_LINE_WIDTH;
char	dashes[256] = { DEFAULT_DASHES };
int	ndashes = DEFAULT_NUM_DASHES;
VisualID    vid = DEFAULT_VISUAL;
unsigned long	planemask = ~0UL;
Colormap    colormap;
Visual	    *visual;
int	    depth;
Window  root = 0;
int	    screen;
#ifdef TIMEOUT
int	current_timeout = TIMEOUT;
#endif

void
Setup(Display *dpy, Window win);

void
HandleExpose(Display *dpy, Window win, GC gc);

void
HandleKeyPress(Display *dpy, Window win, GC gc, XEvent *ev);

void
HandleKeyRelease(Display *dpy, Window win, GC gc, XEvent *ev);

void
HandleTimeout(Display *dpy, Window win, GC gc);

#include <X11/extensions/Xrender.h>

XRenderColor	renderBlack = { 0, 0, 0, 0xffff };
XRenderColor	renderWhite = { 0xffff, 0xffff, 0xffff, 0xffff };
XRenderColor	renderRed = { 0xffff, 0, 0, 0xffff };
XRenderColor	renderGreen = { 0, 0xffff, 0, 0xffff };
XRenderColor	renderBlue = { 0, 0, 0xffff, 0xffff };

XRenderColor	renderClearRed = { 0x8000, 0, 0, 0x8000 };
XRenderColor	renderClearGreen = { 0, 0x8000, 0, 0x8000 };
XRenderColor	renderClearBlue = { 0, 0, 0x8000, 0x8000 };

static inline Picture
GetPicture (Display *dpy, Window w)
{
    static Picture p;

    if (!p)
	p = XRenderCreatePicture (dpy, w, 
				  XRenderFindVisualFormat (dpy, visual),
				  0, 0);
    return p;
}

static inline Picture
GetSrc (Display *dpy, XRenderColor *color)
{
    static Picture	p;
    static Pixmap	pix;
    static XRenderColor	lastColor;
    XRenderPictFormat	*f;
    XRenderPictureAttributes	attr;

    if (p && !memcmp (color, &lastColor, sizeof (XRenderColor)))
	return p;
    if (!p)
    {
	f = XRenderFindStandardFormat (dpy, PictStandardARGB32);
	pix = XCreatePixmap (dpy, RootWindow (dpy, DefaultScreen (dpy)),
			     1, 1, (unsigned) f->depth);
	attr.repeat = True;
	p = XRenderCreatePicture (dpy, pix, f, CPRepeat, &attr);
	XFreePixmap (dpy, pix);
    }
    XRenderFillRectangle (dpy, PictOpSrc, p, color, 0, 0, 1, 1);
    lastColor = *color;
    return p;
}

static inline XRenderPictFormat *
GetMask (Display *dpy)
{
    return XRenderFindStandardFormat (dpy, PictStandardA8);
}

int
main (int argc, char **argv)
{
    Display *dpy;
    Window  win;
    GC	    gc;
    char    **init_argv = argv;
    XSetWindowAttributes    attr;
    XColor  hard, exact;
    int	    x = 0, y = 0;
    unsigned int    width = DEFAULT_WIDTH, height = DEFAULT_HEIGHT;
    unsigned int	    border_width = 1u;
    XSizeHints	sizeHints;
    XWMHints	wmHints;
    XEvent	ev;
    XGCValues	gcv;
#ifdef MATCH_ARGUMENT
    int		i;
#endif
#ifdef PASS_BUTTONS
    int		HasMotion = 0;
    XEvent	mev;
#endif
#ifdef COMPRESS_EXPOSE
    XEvent	eev;
    int		HasExpose = 0;
#endif
    int		sync = 0;
    XTextProperty   wm_name, icon_name;
    Atom	wm_delete_window;
    int		has_fg_pixel = 0, has_bg_pixel = 0;
    int		has_colormap = 0;
    unsigned long   gc_mask;
#ifndef PASS_KEYS
    char	quit_string[10];
#endif
    unsigned long   window_mask;

    if (!rop_name)
	rop_name = DEFAULT_ROP;
    wm_name.value = (unsigned char *) argv[0];
    wm_name.encoding = XA_STRING;
    wm_name.format = 8;
    wm_name.nitems = strlen ((const char *) wm_name.value) + 1;
    icon_name = wm_name;
    gc_mask = 0;
    while (*++argv) {
	if (!strcmp (*argv, "-display"))
	    dpy_name = *++argv;
	else if (!strcmp (*argv, "-visual"))
	    vid = strtoul(*++argv, NULL, 0);
	else if (!strcmp (*argv, "-cmap"))
	{
	    colormap = strtoul(*++argv, NULL, 0);
	    has_colormap = 1;
	}
	else if (!strcmp (*argv, "-rop"))
	    rop_name = *++argv;
	else if (!strcmp (*argv, "-fg"))
	    fg = *++argv;
	else if (!strcmp (*argv, "-bg"))
	    bg = *++argv;
	else if (!strcmp (*argv, "-fg_pixel"))
	{
	    fg_pixel = strtoul(*++argv, NULL, 0);
	    has_fg_pixel = 1;
	}
	else if (!strcmp (*argv, "-bg_pixel"))
	{
	    bg_pixel = strtoul(*++argv, NULL, 0);
	    has_bg_pixel = 1;
	}
	else if (!strcmp (*argv, "-fn"))
	    font_name = *++argv;
	else if (!strcmp (*argv, "-pm"))
	    planemask = strtoul(*++argv, NULL, 0);
	else if (!strcmp (*argv, "-geometry"))
	    XParseGeometry (*++argv, &x, &y, &width, &height);
	else if (!strcmp (*argv, "-sync"))
	    sync = 1;
	else if (!strcmp (*argv, "-bw"))
	    border_width = (unsigned int) strtoul(*++argv, NULL, 0);
	else if (!strcmp (*argv, "-lw"))
	    line_width = strtod(*++argv, NULL);
	else if (!strcmp (*argv, "-cap"))
	    cap_name = *++argv;
	else if (!strcmp (*argv, "-join"))
	    join_name = *++argv;
	else if (!strcmp (*argv, "-fill"))
	    fill_name = *++argv;
	else if (!strcmp (*argv, "-linestyle"))
	    line_name = *++argv;
	else if (!strcmp (*argv, "-bitmap"))
	    bitmap_file = *++argv;
	else if (!strcmp (*argv, "-dashes"))
	{
	    argv++;
	    ndashes = 0;
	    while (*argv && isdigit (**argv))
		dashes[ndashes++] = (char) atoi(*argv++);
	    argv--;
	}
#ifdef MATCH_ARGUMENT
	else if (i = MatchArgument (argv))
	    argv += i - 1;
#endif
	else if (!strcmp (*argv, "-root"))
	    root = strtoul (*++argv, NULL, 0);
	else
	    Usage (*init_argv);
    }
    sizeHints.flags = 0;
    wmHints.flags = InputHint;
    wmHints.input = True;
    dpy = XOpenDisplay (dpy_name);
    if (!dpy)
	Error ("Can't open display", "");
    if (sync)
	XSynchronize (dpy, sync);
    screen = DefaultScreen (dpy);
    if (!root)
	root = RootWindow (dpy, screen);
    window_mask = CWBackPixel|CWBorderPixel|CWEventMask;
    if (!has_colormap)
	colormap = DefaultColormap (dpy, screen);
    else
    {
	window_mask |= CWColormap;
	attr.colormap = colormap;
    }
    visual = DefaultVisual (dpy, screen);
    depth = DefaultDepth (dpy, screen);
    if (vid)
    {
	XVisualInfo vi, *vi_ret;
	int	    n;

	vi.visualid = vid;
	vi.screen = screen;
	vi_ret = XGetVisualInfo (dpy, VisualIDMask|VisualScreenMask,
				 &vi, &n);
	if (vi_ret)
	{
	    visual = vi_ret->visual;
	    if (!has_colormap)
	    {
		colormap = XCreateColormap (dpy, root, visual, AllocNone);
		window_mask |= CWColormap;
		attr.colormap = colormap;
	    }
	    depth = vi_ret->depth;
	}
    }
    if (!has_fg_pixel)
	fg_pixel = BlackPixel (dpy, screen);
    if (!has_bg_pixel)
	bg_pixel = WhitePixel (dpy, screen);
    if (fg)
    {
	if (!XAllocNamedColor (dpy, colormap, fg, &hard, &exact))
	    Error ("Can't allocate fg pixel %s", fg);
	fg_pixel = hard.pixel;
    }
    if (bg)
    {
	if (!XAllocNamedColor (dpy, colormap, bg, &hard, &exact))
	    Error ("Can't allocate bg pixel %s", bg);
	bg_pixel = hard.pixel;
    }
    attr.background_pixel = bg_pixel;
    attr.border_pixel = fg_pixel;
    attr.event_mask =
	ExposureMask|KeyPressMask|KeyReleaseMask|ButtonPressMask|ButtonReleaseMask|MOTION|EXTRA_EVENTS;
//    attr.override_redirect = True;
//    window_mask |= CWOverrideRedirect;
    wm_delete_window = XInternAtom(dpy, "WM_DELETE_WINDOW", False);
    win = XCreateWindow (dpy, root, x, y, width, height, border_width,
			 depth, InputOutput,
			 visual,
 			 window_mask,
			 &attr);
#ifdef PASS_SETUP
    Setup (dpy, win);
#endif
    XSetWMProperties (dpy, win,
		      &wm_name, &icon_name,
		      init_argv, argc,
 		      &sizeHints, &wmHints, 0);
    XSetWMProtocols (dpy, win, &wm_delete_window, 1);
    default_rop =  RopNameToRop (rop_name);
    if (line_width) 
    {
	gcv.line_width = (int) line_width;
	gc_mask |= GCLineWidth;
    }
    if (cap_name)
    {
	gcv.cap_style = MatchCapStyle (cap_name);
	gc_mask |= GCCapStyle;
    }
    if (fill_name) {
	gcv.fill_style = MatchFillStyle (fill_name);
	gc_mask |= GCFillStyle;
    }	
    if (line_name) {
	gcv.line_style = MatchLineStyle (line_name);
	gc_mask |= GCLineStyle;
    }
    if (join_name) {
	gcv.join_style = MatchJoinStyle (join_name);
	gc_mask |= GCJoinStyle;
    }
    if (font_name)
	default_font = XLoadQueryFont (dpy, font_name);
    if (default_font) {
	gcv.font = default_font->fid;
	gc_mask |= GCFont;
    }
    gcv.function = default_rop;
    gcv.foreground = fg_pixel;
    gcv.background = bg_pixel;
    gcv.plane_mask = planemask;
    gc_mask |= GCFunction|GCForeground|GCBackground|GCPlaneMask;
    if (bitmap_file)
    {
	unsigned int	b_width, b_height;
	int		ret;
	Pixmap		bitmap, pixmap;

	ret = XReadBitmapFile (dpy, win, bitmap_file, &b_width, &b_height,
			       &bitmap, (int *) 0, (int *) 0);
	switch (ret) {
	case BitmapOpenFailed:
	    Error ("Can't open bitmap file %s", bitmap_file);
	    break;
	case BitmapFileInvalid:
	    Error ("Bitmap file %s invalid", bitmap_file);
	    break;
	case BitmapNoMemory:
	    Error ("Out of memory reading bitmap file %s", bitmap_file);
	    break;
	case BitmapSuccess:
	    break;
	}
	switch (gcv.fill_style) {
	case FillTiled:
	    pixmap = XCreatePixmap (dpy, win, b_width, b_height, (unsigned) DefaultDepth (dpy, screen));
	    gc = XCreateGC (dpy, pixmap, GCForeground|GCBackground, &gcv);
	    XCopyPlane (dpy, bitmap, pixmap, gc, 0, 0, b_width, b_height, 0, 0, 1);
	    XFreeGC (dpy, gc);
	    XFreePixmap (dpy, bitmap);
	    gcv.tile = pixmap;
	    gc_mask |= GCTile;
	    break;
	case FillStippled:
	case FillOpaqueStippled:
	    gcv.stipple = bitmap;
	    gc_mask |= GCStipple;
	    break;
	}
    }
    gc = XCreateGC (dpy, win, gc_mask, &gcv);
    if (!default_font)
	default_font = XQueryFont (dpy, XGContextFromGC(gc));
    if (ndashes)
	XSetDashes (dpy, gc, 0, dashes, ndashes);
    XMapWindow (dpy, win);
    for (;;) {
#ifdef TIMEOUT
	while (current_timeout && !XEventsQueued(dpy, QueuedAfterFlush)) {
	    struct pollfd pollfd = {
		.fd = ConnectionNumber(dpy),
		.events = POLLIN
	    };
	    int r = poll(&pollfd, 1, current_timeout);
	    if (r == 0)
		HandleTimeout(dpy, win, gc);
	}
#endif
	XNextEvent (dpy, &ev);
#ifdef PASS_BUTTONS
	if (HasMotion && ev.type != MotionNotify) {
	    HasMotion = 0;
	    HandleMotionNotify (dpy, win, gc, &mev);
	}
#endif
#ifdef COMPRESS_EXPOSE
	if (HasExpose && ev.type != Expose) {
	    HasExpose = 0;
	    HandleExpose (dpy, eev.xexpose.window, gc);
	}
#endif
	switch (ev.type) {
	case Expose:
#ifdef COMPRESS_EXPOSE
	    if (QLength(dpy)) {
		eev = ev;
		HasExpose = 1;
	    } else if (ev.xexpose.count == 0)
#endif
	    
	    HandleExpose (dpy, ev.xexpose.window, gc);
	    break;
#ifdef PASS_KEYS
	case KeyPress:
	    HandleKeyPress (dpy, ev.xkey.window, gc, &ev);
	    break;
	case KeyRelease:
	    HandleKeyRelease (dpy, ev.xkey.window, gc, &ev);
	    break;
#else
	case KeyPress:
	    if (XLookupString ((XKeyEvent *) &ev, quit_string, sizeof (quit_string), 0, 0) == 1) {
		switch (quit_string[0]) {
		case 'q':
		    exit (0);
		case 'c':
		    XClearArea (dpy, ev.xkey.window, 0, 0, 0, 0, True);
		    break;
		}
	    }
	    break;
#endif
#ifdef PASS_BUTTONS
	case ButtonPress:
	    HandleButtonPress (dpy, ev.xbutton.window, gc, &ev);
	    break;
	case ButtonRelease:
	    HandleButtonRelease (dpy, ev.xbutton.window, gc, &ev);
	    break;
	case MotionNotify:
	    if (QLength(dpy)) {
	    	mev = ev;
	    	HasMotion = 1;
	    } else {
	    	HandleMotionNotify (dpy, ev.xmotion.window, gc, &ev);
	    }
	    break;
#endif
	case ClientMessage:
	    exit (0);
#ifdef PASS_OTHER
	default:
	    HandleOtherEvent (dpy, win, gc, &ev);
#endif
	}
    }
}
