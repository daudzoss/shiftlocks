.include "macrodef.inc"

.if BASIC
*	= BASIC+1
.else
*	= $0002+1
COPIED2	= $0400
	.word	(+), 3
	.text	$81,$41,$b2,$30	; FOR A = 0
	.text	$a4		; TO prefld-start
	.text	format("%4d",pre_end-start)
	.text	$3a,$dc,$30	; : BANK 0
	.text	$3a,$42,$b2,$c2	; : B = PEEK
	.text	$28		; ( start
	.text	format("%2d",COPIED2)
	.text	$aa,$41,$29,$3a	; + A ) :
	.text	$dc,$31,$35,$3a	; BANK 1 5 :
	.text	$97		; POKE start
	.text 	format("%2d",COPIED2)
	.text	$aa,$41,$2c,$42	; + A , B
	.text	$3a,$82,$00	; : NEXT
+
	.word	(+), 2055
	.text	$99,$22,$09,$8e	; PRINT " CHR$(9) CHR$(142)
	.text	$08,$93,$13,$13	; CHR$(8) CHR$(147) CHR$(19) CHR$(19)
topline	.text	"the crime scene     "
	.text	"  shiftlocks office"
	.text	$9d,$9d,$9d,$9d
	.text	$9d,$9d,$9d,$9d
	.text	$94,"'"
	.null	$11,$11,$11,$22
+	.word	(+), 2055
	.text	$99,$22
	.text	"                 "
	.null	$af,$22
+	.word	(+), 2055
	.text	$99,$22
	.text	"                 "
	.null	$ce,$b4,$22
+	.word	(+), 2055
	.text	$99,$22
	.text	"    threats      "
	.null	"dis-",$22
+	.word	(+), 2055
	.text	$99,$22
	.text	"   ",$12,"wounds: ?"
	.null	$92,"     card",$22
+	.word	(+), 2055
	.text	$99,$22
	.text	" investigations  "
	.null	$22
+	.word	(+), 2055
	.text	$99,$22
	.text	"                 "
	.null	"pile",$22,$3a,$99
+	.word	(+), 2055
	.text	$99,$22,$11
	.text	"                 "
	.null	" draw",$22
+	.word	(+), 2055
	.text	$99,$22,$11
	.text	"                 "
	.null	" deck",$22
+	.word	(+), 2055
	.text	$99,$22
	.text	" ",$5e,"f1"
	.text	" ",$5e,"f3"
	.text	" ",$5e,"f5"
	.text	" ",$5e,"f7"
	.text	"    "
	.null	$a7,$ce,$22
+	.word	(+), 2055
	.text	$99,$22
	.text	"                  "
	.null	$12,"   ",$92,$b7,$22
+	.word	(+), 2055
	.text	$99,$22
	.text	"                  "
	.null	$12,"   ",$92,$22
+	.word	(+), 2055
	.text	$99,$22
	.text	"                  "
	.null	$12," ? ",$92,$22
+	.word	(+), 2055
	.text	$99,$22
	.text	"                  "
	.null	$12,"   ",$92,$22
+	.word	(+), 2055
	.text	$99,$22
	.text	"                  "
	.null	$12,"   ",$92,$22
+	.word	(+),2055
	.text	$99,$22
	.text	" f2",$c0," f4",$c0
	.text	" f6",$c0," f8",$c0
	.null	" to solve: ",$22
+	.word	(+),2055
	.text	$99,$3a,$99,$22,"pairing "
	.text	"   +       +    "
	.null	"   +       +",$22
+	.word	(+),2055
	.text	$99,$22
	.null	" rules:",$22,$3b
.endif
+	.word	(+),2055
	.text	$9e
	.null	format("%4d",main)
+	.word	0

.if !BASIC
*	= COPIED2
.endif

.include "gamerule.asm"		; tosolve, solvlen, gamewon()
.include "playeras.asm"		; ability, abillen, double1(), '2(), '3(), '4()

SCREEND	= SCREENC-SCREENM
SCRSIZE	= SCREENW*SCREENH
ONLYTOP	= SCREENM+SCREENW*(SCREENH-8)
WDX	= $0b
WDY	= $07

REVCARD = $08
NONCARD	= $09
DECKSIZ	= pstdeck-stddeck

UNDOABL	= vararea
STACKHT	= UNDOABL + $00		; 0-3 invest, 4-7 threat, 8-11 office
HAND	= UNDOABL + $0c		;
DISCARD	= UNDOABL + $10
DECK	= DISCARD + DECKSIZ
ODRAWER	= DISCARD + 2*DECKSIZ	; +0~15 1's, 16~31 2's, 32~47 3's,...

UNDOABH	= DISCARD + 2*DECKSIZ  + 4*(8+8)
HANDREM = UNDOABH + 0
DISCREM	= UNDOABH + 1
DECKREM	= UNDOABH + 2
NWOUNDS	= UNDOABH + 3
HANDFOM	= UNDOABH + $c
MOVESEL	= UNDOABH + $d
TOSCENE	= UNDOABH + $e
TOFFICE	= UNDOABH + $f
UNDOEND	= UNDOABH + $10		; must be last
	
UNSAVED	= vararea + $100
.if UNSAVED <= UNDOEND
.error "saved variables crashed into ceiling, obliterated UNSAVED"
.endif
HANDHST	= UNSAVED + $0
ACURSOR	= UNSAVED + $08
TEMPVAR	= UNSAVED + $09
UNDOPTR	= UNSAVED + $0a
UNDOLIM = UNSAVED + $0b		; first byte not read-writeable in RAM
REDOMAX	= UNSAVED + $0c
SCRATCH	= UNSAVED + $10		; must be last

F1_KEY	= $85
F3_KEY	= $86
F5_KEY	= $87
F7_KEY	= $88
F2_KEY	= $89
F4_KEY	= $8a
F6_KEY	= $8b
F8_KEY	= $8c
DEL_KEY	= $14
INS_KEY	= $94
HOM_KEY = $13
CLR_KEY = $93

start

initptr	sta	1+ZP		;register uint8_t initptr(register uint8_t a) {
	lda	#>UNDOABL	; zp = (uint8_t*) (a << 8); // save slot to use
	sta	1+ZP+2		; zp2 = (uint8_t*) UNDOABL; // latest game state
	lda	#0		;
	sta	ZP		;
	sta	ZP+2		;
	ldy	#UNDOEND-UNDOABL; return y = UNDOEND-UNDOABL; // #bytes to save
	rts			;} // initptr()
	
memprob	sta	1+ZP		;void memprob(register uint8_t a) {
	clc			;
	adc	#$40		;
	sta	1+ZP+2		;
	lda	#0		;
	sta	ZP		; uint8_t* zp = a << 8;
	sta	ZP+2		; uint8_t* zp2 = zp + 0x4000; // detect 16k wrap
	ldy	#$ff		; register uint8_t y = 0xff; // 0 might be ports
	
-	lda	(ZP),y		; do {
	sta	TEMPVAR		;  TEMPVAR = zp[y];
	lda	#$55		;
	sta	(ZP),y		;  zp[y] = 0x55; // one beyond last state byte
	lda	(ZP),y		;  a = zp[y];
	cmp	#$55		;
	php			;
	lda	TEMPVAR		;
	sta	(ZP),y		;  zp[y] = TEMPVAR;
	plp			;  if (zp[y] != 0x55)
	bne	++		;   return a = zp >>8; // page where read failed
	pha			;  uint8_t stack  = TEMPVAR;
	lda	#$55		;
	sta	(ZP),y		;  zp[y] = 0x55;
	lda	(ZP+2),y	;
	sta	TEMPVAR		;  TEMPVAR = zp2[y];
	lda	#$aa		;
	sta	(ZP+2),y	;  zp2[y] = 0xaa;
	lda	(ZP),y		;  a = zp[y]; // should still be 0x55
	cmp	#$aa		;
	php			;
	pla			;
	tax			;  x = a == 0xaa;// and not have changed to 0xaa
	pla			;
	sta	(ZP),y		;  zp[y] = stack;
	lda	TEMPVAR		;
	sta	(ZP+2),y	;  zp2[y] = TEMPVAR;
	txa			;
	pha			;  if (x) // zp[y] did contain 0x55 so a write
	plp			;         // to 4k above it indeed clobbered ti
	beq	+		;   break;
	lda	(ZP),y		;
	sta	TEMPVAR		;  TEMPVAR = zp[y];
	lda	#$aa		;
	sta	(ZP),y		;  zp[y] = 0xaa; // all bits flipped from 0x55
	lda	(ZP),y		;
	cmp	#$aa		;  if (zp[y] != 0xaa)
	php			;
	lda	TEMPVAR		;
	sta	(ZP),y		;  zp[y] = stack;
	plp			;
	bne	++		;   return a = zp >>8; // page where read failed
	inc	1+ZP		;
	inc	1+ZP+2		;
	bne	-		; } while (zp += 0x0100); // nix infinite wrap
+	lda	#>undobuf	;
	ora	#$0f		;
	clc			;
	adc	#1		;
 	rts			; return a = (undobuf|0x0fff)+1;//end of 4k buf
+	lda	1+ZP		;
.if 0
	pha			;
	lsr			;
	lsr			;
	lsr			;
	lsr			;
	cmp	#9+1		;
	bcc	+		;
	sec			;
	sbc	#$39		;
+	sta	TEMPVAR		;
	digitxy	TEMPVAR,$4,$16,0;
	pla			;
	pha			;
	and	#$0f		;
	cmp	#9+1		;
	bcc	+		;
	sec			;
	sbc	#$39		;
+	sta	TEMPVAR		;
	digitxy	TEMPVAR,$5,$16,0;
	pla			;
.endif
	rts			;} // memprob()

snpshot	ldx	UNDOPTR		;void snpshot(void) {
	inx			; register uint8_t x, y;
	cpx	UNDOLIM		;
	bcc	+		; while (UNDOPTR + 1 >= UNDOLIM) { // slots full
	ldx	#>undobuf	;
	lda	#0		;
	sta	ZP		;
	sta	ZP+2		;
	stx	1+ZP		;  zp = undobuf; // 1st save slot is obliterated
	inx			;  for (x = (zp+0x100)>>8; x < UNDOLIM>>8; x++){
-	stx	1+ZP+2		;   zp2 = x << 8; // subsequent slots scoot down
	ldy	#UNDOEND-UNDOABL;   for (y = UNDOEND-UNDOABL; y; y--) {
-	dey			;
	lda	(ZP+2),y	;
	sta	(ZP),y		;    zp[y-1] = zp2[y-1];
	cpy	#0		;
	bne	-		;   }
	stx	1+ZP		;   zp = zp2;
	inx			;
	cpx	UNDOLIM		;
	bcc	--		;  }
	dec	UNDOPTR		;  if (!--UNDOPTR) exit(0); // nix infinite wrap
	bne	snpshot		; }
	brk			;  
	brk			;
+	txa			; // UNDOPTR is highest used slot's page number
	sta	UNDOPTR		; UNDOPTR++;
	sta	REDOMAX		; REDOMAX = UNDOPTR;
	jsr	initptr		; // REDOMAX is increased in tandem with UNDOPTR
-	dey			; for (y = initptr(UNDOPTR); y; y--) {
	lda	(ZP+2),y	;
	sta	(ZP),y		;  zp[y-1] = zp2[y-1];//saved slot<-state byte
	cpy	#0		;
	bne	-		; }
	rts			;} //snpshot()

undomov	dec	UNDOPTR		;void undomov(void) {
	lda	UNDOPTR		; UNDOPTR--;
	cmp	#>undobuf	;
	bcs	+		; if (UNDOPTR < undobuf>>8) {
	inc	UNDOPTR		;  UNDOPTR++; // can't undo any more; hold fast
	rts			; } else {
+	jsr	initptr		;  register uint8_t y;
-	dey			;
	lda	(ZP),y		;  for (y = initptr(UNDOPTR); y; y--) {
	sta	(ZP+2),y	;   zp2[y-1] = zp[y-1];//saved state<-slot byte
	cpy	#0		;  }
	bne	-		;  redraws();
	jmp	redraws		; }
	;rts			;} // undomov()

redomov	lda	UNDOPTR		;void redomov(void) {
	cmp	REDOMAX		;
	bne	+		; if (UNDOPTR == REDOMAX)
	rts			;  return; // can't redo any more; hold fast
+	inc	UNDOPTR		; UNDOPTR++;
	lda	UNDOPTR		;
	jsr	initptr		; register uint8_t y;
-	dey			;
	lda	(ZP),y		; for (y = initptr(UNDOPTR); y; y--) {
	sta	(ZP+2),y	;  zp2[y-1] = zp[y-1];//saved state<-slot byte
	cpy	#0		;
	bne	-		; }
	jmp	redraws		; redraws();
	;rts			;} // redomov()
redoall	lda	REDOMAX		;void redoall(void) {
	cmp	UNDOPTR		; if (UNDOPTR != REDOMAX) {// slots left to undo
	bne	+		;  UNDOPTR = REDOMAX;
	rts			;  for (y = initptr(UNDOPTR); y; y--)
+	sta	UNDOPTR		;   zp2[y-1] = zp[y-1];//saved state<-slot byte
	jsr	initptr		; }
	jmp	-		;} // redoall()
	
redraws	jsr	discsho		;void redraws(void) {
	jsr	drawsho		; discsho();
	jsr 	handsho		; drawsho();
	digitxy	NWOUNDS,WDX,WDY	; handsho();
	ldx	#8		; digitxy(NWOUNDS, WDX, WDY);
-	txa			; for (register uint8_t x = 8; x; x--) {
	pha			;  register uint8_t a;
	lda	STACKHT-1,x	;
	beq	+		;  if (STACKHT[x-1])
	dex			;
	txa			;
	inx			;   a = x - 1; // just the card number itself
	jmp	++		;  else
+	lda	#NONCARD	;   a = NONCARD; // blank out that stack
+	pha			;
	ldy	stacky-1,x	;  
	lda	stackx-1,x	;
	tax			;
	pla			;
	clc			;
	jsr	cardsho		;  cardsho(0, a, stackx[x-1], stacky[x-1]);
	pla			;
	tax			;
	dex			;
	bne	-		; }
	
	ldy	#$40		;
-	dey			;
	tya			; for (register uint8_t y = 4*16; y; y--) {
	pha;1:index y preserved	;  register uint8_t x, y_;
	lsr			;  register uint1_t c;
	lsr			;
	lsr			;
	lsr			;
	tax			;  x = (y-1) >> 4; // 0~3
	tya			;
	and	#$0f		;
	clc			;
	adc	#1		;  a = ((y-1) & 0x0f) + 1; // 1~16
	sta	TEMPVAR		;  TEMPVAR = a;
	cmp	STACKHT+8,x	;
	bcc	++		;
	beq	+		;  if (a > STACKHT[8+x]) { // above stack top
	clc			;   c = 0;
	lda	#NONCARD	;   a = NONCARD; // blank 3x5
	jmp	+++		;  } else if (a == STACKHT[8+x]) { // visible
+	clc			;   c = 0;
	lda	ODRAWER,y	;   a = ODRAWER[y-1]; // entire 3x5 card on top
	jmp	++		;  } else /* if (a < STACKHT[8+x]) */ { // hid
+	sec			;   c = 1;
	lda	ODRAWER,y	;   a = ODRAWER[y-1]; // top 3x1 edge in between
+	pha			;  }
	lda	stackx+8,x	;
	tax			;  x = stackx[8+x];
	pla			;
	ldy	TEMPVAR		;  y_ = TEMPVAR;
	jsr	cardsho		;  cardsho(c, a, x, y_);
	pla			;
	tay			;
	bne	-		; }
	rts			;} // redraws()

stddeck	.byte	$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$01,$01,$01,$01,$01,$01,$01,$01
	.byte	$02,$02,$02,$02,$02,$02,$02,$02
	.byte	$03,$03,$03,$03,$03,$03,$03,$03
	.byte	$04,$04,$04,$04,$05,$05,$05,$05
	.byte	$06,$06,$06,$06,$07,$07,$07,$07
pstdeck


;;; 1,2,3,4 = 000,001,010,011
;;; A,B,C,D = 100,101,110,111
cardnum
invest0	.byte	$b1,$b2,$b3,$b4	; const
threat0	.byte	$81,$82,$83,$84
cardtyp
investc	.byte	$c0,$d7
threatc	.byte	$db,$c0
pileout	.byte	$4f,$77,$50
	.byte	$65,$20,$67
	.byte	$65,$20,$67
	.byte	$65,$20,$67
	.byte	$4c,$6f,$7a
cardofs	.byte	0*SCREENW+0,0*SCREENW+1,0*SCREENW+2
	.byte	1*SCREENW+0,1*SCREENW+1,1*SCREENW+2
	.byte	2*SCREENW+0,2*SCREENW+1,2*SCREENW+2
	.byte	3*SCREENW+0,3*SCREENW+1,3*SCREENW+2
	.byte	4*SCREENW+0,4*SCREENW+1,4*SCREENW+2
cardclr	.byte	$66,$63,$64,$65
	.byte	$32,$3a,$58,$69,0,0
drawx	.byte	$12
drawy	.byte	$10
discx	.byte	$11
discy	.byte	$00
solvex	.byte	$1b
solvey	.byte	$15
abilix	.byte	$08
abiliy	.byte	$18
inhandx	.byte	$01,$05,$09,$0d
inhandy	.byte	$10;,$10,$10,$10
stackx	.byte	$00,$04,$08,$0c
	.byte	$00,$04,$08,$0c
	.byte	$16,$1b,$20,$25
stacky	.byte	9,9,9,9,1,1,1,1
	.byte	1,1,1,1		;

main
.if !BASIC
	lda	#$0f		;// P500 has to start in bank 15
	sta	$01		;static volatile int execute_bank = 15;
.endif
	lda	#0		;void main (void) {
	sta	ACURSOR		; ACURSOR = 0; // visible from arrow to Esc key
	sta	DISCREM		;
	sta	HANDREM		;
	sta	NWOUNDS		; NWOUNDS = DISCREM = HANDREM = 0;
.if BASIC
	jsr	bckdrop		; bckdrop(); // draw most of the scren
.endif
	jsr	finishr		; finishr(); // rule/ability text and card color
	digitxy	NWOUNDS,WDX,WDY	; digitxy(NWOUNDS, WDX, WDY); // set wounds to 0
	jsr	discsho		; discsho(); // show the empty discard pile
	jsr	initstk		; initstk(); // empty the crime scene and office
	jsr	animshf		; animshf(); // shuffle deck briefly onscreen
	lda	#>undobuf	;
	tax			;
	dex			;// UNDOPTR always points to the slot reflecting
	stx	UNDOPTR		;// the current state, REDOMAX its maximum limit
	stx	REDOMAX		; UNDOPTR = REDOMAX = (undobuf - 0x0100) >> 8;
	jsr	memprob		;
	sta	UNDOLIM		; UNDOLIM = memprob(undobuf); // deduce R/W area
	ldx	#$18		;
	ldy	#$0a		;
	lda	#ownmsg1-ownmsg0;
	txtclip	ownmsg1		;
	lda	#ownmsg1-ownmsg0;
	replace	ownmsg0		;

newhand	lda	#2		; do {// until we have a hand that is acceptable
	sta	TOSCENE		;  TOSCENE = TOFFICE = 2; // mandatory unplayed
	sta	TOFFICE		;  // will play half to scene and half to office
	jsr	drw4hnd		;  do {
	sta	HANDFOM		;   HANDFOM = drw4hand();// nonzero if we drew 4
	bne	+		;   if (HANDFOM == 0)
	brk			;    exit(1); // more than 44 cards in stacks?!?
	.byte	$1		;
+	jsr	snpshot		;   snpshot();// undo state saved on a new hand
	jsr	handsho		;   handsho();// draw empty deck pile after if 0
	jsr	discsho		;   discsho();// ,empty discard pile if shuffled
	lda	HANDFOM		;
	jsr	rejctok		;
      	beq	getmove		;
	jsr	handrej		;
	bne	newhand		;  } while (rejctok(HANDFOM) && handrej());

getmove	jsr	cdwnsho		;  do {
	jsr	$ffe4		;   cdwnsho();
	beq	getmove		;   a = getchar();
	cmp	#DEL_KEY	;   if (a == DEL_KEY) {
	bne	+		;
	jsr	undomov		;    undomov();
	jmp	getmove		;    continue;

+	ldy	NWOUNDS		;   }
	cpy	#3		;   if (NWOUNDS < 3) {// once lost: undo or quit
	bcc	+		;    if (askquit())
	jsr	askquit		;     exit(1);
	beq	getmove		;    else continue;
	jmp	tobasic		;   }
	
+	cmp	#INS_KEY	;
	bne	+		;   if (a == INS_KEY) {
	jsr	redomov		;    redomov();   
	jmp	getmove		;    continue;

+	cmp	#HOM_KEY	;
	bne	+		;   } else if (a == HOM_KEY) {
	jsr	redoall		;    redomov();   
	jmp	getmove		;    continue;

+	cmp	#CLR_KEY	;
	bne	+		;   } else if (a == CLR_KEY) {
	lda	#>undobuf	;
	tax			;
	dex			;
	sta	UNDOPTR		;
	sta	REDOMAX		;    UNDOPTR = REDOMAX =(undobuf - 0x0100) >> 8;
	jmp	getmove		;    continue;

+	cmp	#'1'		;
	bcc	notfkey		;   } else if (a >= '1'
	cmp	#'8'+1		;              &&
	bcs	+		;              a < '9') {
nummove	ldy	#0		;   nummove:
	sty	TEMPVAR		;    TEMPVAR = 0;
	sec			;
	sbc	#'1'		;    a -= '1'; // 0~7 even:crimescene,odd:office
	lsr			;    // a:         0   1   2   3   4   5   6   7
	ldy	#3		;              // CS  OF  CS  OF  CS  OF  CS  OF
-	rol	TEMPVAR		;    // (a&1)<<2:  0   4   0   4   0   4   0   4
	dey			;    // a>>1:      0   0   1   1   2   2   3   3
	bne	-		;    TEMPVAR = ((a & 1) << 2) | (a >> 1);
	ora	TEMPVAR		;    a |= TEMPVAR; // 0~3 crimescene, 4~7 office
	sta	MOVESEL		;    MOVESEL = a;
	jmp	trymove		;

+	cmp	#F1_KEY		;
	bcc	notfkey		;   } else if (a >= F1_KEY
	cmp	#F1_KEY+8	;              &&
	bcs	notfkey		;              a < F1_KEY+8) {
	sec			;
	sbc	#F1_KEY		;    a -= F1_KEY - 1;// 0~3 crimescene, 4~7 as ^
	sta	MOVESEL		;    MOVESEL = a;
	jmp	trymove		;

notfkey	cmp	#$5f		; // <-
	beq	+		;   } else if (a == '<-' // Commodore back-arrow
	cmp	#$1b		; // Esc
	bne	++		;              || a == '\033') // 264 & P500 Esc
+	lda	#0		;
	jsr	arrowky		;    arrowky(0); // dehighlight cursor til arrow
	jmp	getmove		;

+	cmp	#$1d		; // R
	bne	+++		;   } else if (a == 0x1d) { // right arrow
	lda	ACURSOR		;
	bne	+		;    if (!ACURSOR)
	lda	#8		;     arrowky(8); // highlight F8
	bne	++		;    else
+	lda	#$02		;     arrowky(+2); // next card to right
+	jsr	arrowky		;
	jmp	getmove		;

+	cmp	#$11		; // D
	bne	+++		;   } else if (a == 0x11) { // down arrow
	lda	ACURSOR		;
	bne	+		;    if (!ACURSOR)
	lda	#4		;     arrowky(4); // highlight F4
	bne	++		;    else
+	lda	#$01		;     arrowky(+1); // next card down/right
+	jsr	arrowky		;
	jmp	getmove		;

+	cmp	#$9d		; // L
	bne	+++		;   } else if (a == 0x9d) { // left arrow
	lda	ACURSOR		;
	bne	+		;    if (!ACURSOR)
	lda	#1		;     arrowky(1); // highlight F1
	bne	++		;    else
+	lda	#$fe		;     arrowky(-2); // next card to left
+	jsr	arrowky		;
	jmp	getmove		;

+	cmp	#$91		; // U
	bne	+++		;   } else if (a == 0x91) { // up arrow
	lda	ACURSOR		;
	bne	+		;    if (!ACURSOR)
	lda	#5		;     arrowky(5); // highlight F5
	bne	++		;    else
+	lda	#$ff		;     arrowky(-1); // next card up/left
+	jsr	arrowky		;
	jmp	getmove		;

+	cmp	#$0d		; // Return
	bne	++		;   } else if (a == 0x1d) { // accept selection
	lda	ACURSOR		;
	bne	+		;
	jmp	getmove		;    if (ACURSOR) {
+	clc			;     a += '0';
	adc	#'0'		;     goto nummove; // re-use code above for 1-8
	jmp	nummove		;    }

+	cmp	#'q'|$20	;
	beq	+		;
	cmp	#'q'		;
	bne	notakey		;   } else if (a == 'q' || a == 'Q') { //->BASIC
+	jsr	askquit		;
	beq	notakey		;    if (askquit())
	jmp	tobasic		;     return;

.if 0
notakey	jmp	getmove		;   } else /* handle others here or */ continue;
.else
notakey	cmp	#$21		;
	bcc	+		;
	cmp	#$29		;
	bcs	+		;
	ldx	#$16		;
	ldy	#0		;
	lda	#5		;
	txtclip	notabuf		;
	lda	#5		;
	replace	notaneg		;
	ldx	#5		;
-	lda	notabuf-1,x	;
	sta	notaneg-1,x	;
	dex			;
	bne	-		;
+	jmp	getmove		;   } else continue;
notaneg	.byte	$20,$13,$08,$05	;
	.byte	$12		;
notabuf	.fill	5		;
.endif

trymove	pha			;
	lda	SCREENM+$18+SCREENW*$0a	;
	cmp	ownmsg0		;
	bne	+		;
	ldx	#$18		;
	ldy	#$0a		;
	lda	#ownmsg1-ownmsg0;
	txtclip			;
	lda	#ownmsg1-ownmsg0;
	replace	ownmsg1		;
+	pla			;
	and	#$03		;
	tay			;
	lda	HAND,y		;
	cmp	#8		;
	bcc	+		;   if (HAND[MOVESEL & 3] >= 8)
	jmp	getmove		;    continue; // already played that card
+	lda	MOVESEL		;
	jsr	movedok		;   // movedok() returns 1<<MOVESEL if ok else 0
	bne	wndleft		;   if (!movedok(MOVESEL)) {
	jsr	warning		;    if (warning() == 0 /* 0 wounds accepted */)
	bne	acceptw		;
	jmp	getmove		;     continue;
acceptw	lda	MOVESEL		;
	jsr	fromhnd		;
	ldy	DISCREM		;
	sta	DISCARD,y	;    DISCARD[DISCREM] = fromhnd(MOVESEL);
	inc	DISCREM		;    DISCREM++;
	inc	NWOUNDS		;    NWOUNDS++;
wndleft	digitxy	NWOUNDS,WDX,WDY	;   }
	jsr	snpshot		;   digitxy(NWOUNDS, WDX, WDY);
	jsr	handsho		;   snpshot();//after card play, save undo state
	lda	NWOUNDS		;   handsho(); // show the played slot as blank
	cmp	#$03		;
	bcc	+		;   if (NWOUNDS >= 3)
	jmp	getmove		;    continue; // in case player wants to undo
+	lda	HANDREM		;
	beq	+		;
	jmp	getmove		;  } while (HANDREM);
+	jsr	gamewon		;
	bne	mainend		;
	jmp	newhand		; } while (!gamewon());
mainend	handmsg	wonmsg0,wonmsg1-wonmsg0,wonmsg2-wonmsg1,SCRATCH
-	jsr	$ffe4		; do {
	beq 	-		; } while (getchar());
	handmsg	SCRATCH,wonmsg1-wonmsg0,wonmsg2-wonmsg1
tobasic	rts			;} // main()
ownmsg0	.byte	$01,$0c,$0c,$20	; ALL
	.byte	$12,$09,$07,$08	; RIGH
	.byte	$14,$13,$20,$17	; TS W
	.byte	$09,$13,$05,$20	; ISE
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20	;
	.byte	$17,$09,$1a,$01	; WIZA
	.byte	$12,$04,$20,$07	; RD G
	.byte	$01,$0d,$05,$13	; AMES
	.byte	$20,$32,$30,$32	;  202
	.byte	$34		; 4
ownmsg1	.fill	ownmsg1-ownmsg0
wonmsg0	.byte	$19,$0f,$15,$20	; YOU
	.byte	$17,$0f,$0e,$20	; WON
	.byte	$14,$08,$05,$20	; THE
	.byte	$07,$01,$0d,$05	; GAME
	.byte	$21		; !
wonmsg1	.byte	$a0,$81,$8e,$99	;  ANY
	.byte	$a0,$8b,$85,$99	;  KEY
	.byte	$a0,$94,$8f,$a0	;  TO
	.byte	$85,$98,$89,$94	; EXIT
	.byte	$a0		;
wonmsg2

.if 0
cdwnsho	digitxy	TOSCENE,$10,0	;void cdwnsho(void) { digitxy(TOSCENE,*discx-2,
	digitxy	TOFFICE,$14,0	; *discy);   digitxy(TOFFICE,*discx+2,*discy);
.else
cdwnsho	lda	TOSCENE		;void cdwnsho(void) {
	bubble2	SCREENM,$0f,$37	;
	lda	TOFFICE		; bubble2(TOSCENE, SCREENM, 0x10, 0x09);
	bubble2	SCREENM,$15,$3d	; bubble2(TOFFICE, SCREENM, 0x14, 0x15);
.endif
.if 0
	lda	UNDOLIM		;
	lsr			;
	lsr			;
	lsr			;
	lsr			;
	cmp	#9+1		;
	bcc	+		;
	sec			;
	sbc	#$39		;
+	sta	TEMPVAR		;
	digitxy	TEMPVAR,$1,$16,0;

	lda	UNDOLIM
	and	#$0f		;
	cmp	#9+1		;
	bcc	+		;
	sec			;
	sbc	#$39		;
+	sta	TEMPVAR		;
	digitxy	TEMPVAR,$2,$16,0;
.endif
	rts			;} // cdwnsho()

ARROWOD	= SCREENM+SCREENW*$0f+1-2
ARROWEV	= ARROWOD+6*SCREENW
evenodd	.byte	$02		;
arrowky	cmp	#0		;void arrowky(register int8_t& a) { // -2,...,+2
	bne	cursor1		; if (a == 0) { // a request to de-highlight
	lda	ACURSOR		;
	asl			;
	tax			;  register uint8_t x = ACURSOR << 1;// turn off
	lda	#0		;
	sta	ACURSOR		;  ACURSOR = 0;
	cpx	#0		;  if (x) // arrow mode not already cancelled
	bne	cursor0		;   cursor0(x);
	rts			; } else {
cursor1	sta	TEMPVAR		;
	lda	ACURSOR		;
	asl			;
	pha			;  uint8_t stack = ACURSOR << 1; // turn off
	lsr			;
	clc			;
	adc	TEMPVAR		;
	;bmi	+		;
	beq	+		;  if ((ACURSOR + a) <= 0
	cmp	#9		;      ||
	bcc	++		;      (ACURSOR + a) > 8)
+	pla			;   return;
	rts			;  else {
+	sta	ACURSOR		;   ACURSOR += a;
	asl			;
	tax			;   register uint8_t x = ACURSOR << 1;// turn on
	bit	evenodd		;
	beq	+		;   if (x & 2) // ACURSOR odd (cursor above hand)
	lda	ARROWOD,x	;
	ora	#$80		;
	sta	ARROWOD,x	;    ARROWOD[x] |= 0x80;
	pla			;    cursor0(stack);
	tax			;   } else {
	jmp	cursor0		;    ARROWEV[x] |= 0x80;
+	lda	ARROWEV,x	;    cursor0(stack);
	ora	#$80		;   }
	sta	ARROWEV,x	;  }
	pla			; }
	tax			;} // arrowky()
cursor0	txa			;void cursor1(register uint8_t x) {
	bit	evenodd		;
	beq	+		; if (x & 2) // cursor above hand
	lda	ARROWOD,x	;
	and	#$7f		;
	sta	ARROWOD,x	;  ARROWOD[x] &= 0x7f;
	rts			; else
+	lda	ARROWEV,x	;
	and	#$7f		;
	sta	ARROWEV,x	;  ARROWEV[x] &= 0x7f;
	rts			;} // arrowky()


fromhnd	and	#$03		;uint8_t fromhnd(register uint2_t a) {
	tay			; uint8_t retval; // took HAND index 0 ~ 3
fasthnd	lda	HAND,y		; retval = HAND[a & 0x03]; //in here if y passed
	cmp	#NONCARD	;
	bcs	+		; if (retval < NONCARD) {
	pha			;
	lda	#NONCARD	;  HAND[a & 0x03] = NONCARD;
	sta	HAND,y		;  HANDREM--;
	pla			;
	dec	HANDREM		; }
+	and	#$ff		; return retval; // returned 0 ~ 7 card, 8 empty
	rts			;} // fromhnd()

intohnd	and	#$07		;uint8_t intohnd(register uint3_t a)  {
	ldy	#4		; register int8_t x, y; // took card 0 ~ 7
-	ldx	HAND-1,y	; for (y = 3; y >= 0; y--) {
	cpx	#NONCARD	;  if (HAND[y] >= NONCARD) // empty slot found
	bcs	+		;   break;
	dey			;
	bne	-		;
	dey			; if (y < 0)
	bmi	++		;  return -1; // returned negative if hand full
+	sta	HAND-1,y	; HAND[y] = a;
	inc	HANDREM		; HANDREM++;
	tya			; return y; // returned HAND index 1 ~ 4
+	rts			;} // intohnd()

unambig	lda	#0		;uint3_t unambig(void) {
	ldx	#1		; register uint8_t a = 0;
-	clc			; for (register int8_t x = 1; x < 5; x++) {
	ror	STACKHT+7,x	;
	php			;
	rol	STACKHT+7,x	;
	plp			;
	ror			;  a = ((STACKHT[8+x-1] & 1) << 7) | (a >> 1);
	inx			;
	cpx	#5		;
	bne	-		; }
	lsr			; // if a!=0 we have identified the one possible
	lsr			; // office stack for a threat because of unique
	lsr			; // investigation card showing, so avoid prompt
	lsr			; switch (a >> 4) {
	cmp	#1		; case 1:
	beq	+		;  return a /* = 1 */;
	cmp	#2		; case 2:
	beq	+		;  return a /* = 2 */;
	cmp	#4		; case 4:
	beq	++		;  return a = 3;
	cmp	#8		; case 8:
	beq	+++		;  return a = 4;
	lda	#0		; default:
+	and	#$ff		;
	rts			;  return 0;
+	lda	#3		;
	rts			;
+	lda	#4		; }
	rts			;} // unambig()

officem
threatm
	.byte	$04		;// card 0~3 crimescene, 4~7 office
tisnext	.byte	$01		;// height LSB 0 invest next, 1 threat next

move_of	pha;1:4~7 since move_of	;uint8_t move_of(register uint3_t a) {
	and	#$03		;
	tay			; register uint2_t y = a & 0x03; // hand slot
	pha;2:index in hand 0~3	; register uint8_t a_;
	lda	HAND,y		; TEMPVAR = a_ = HAND[y]; // card 0 ~ 7
	sta	TEMPVAR		;
	and	#$03		;
	tax			; register uint2_t x = a_& 0x03; // office slot
	lda	TEMPVAR		;
	bit	threatm		;
	beq	playinv		; if (TEMPVAR & 0x04) { // threat card attempt
	jsr	unambig		;  x = unambig(); //check: is pile unambiguous?
	bne	+		;  if (!x) // >1 pile is generally possible, so
	jsr	oprompt		;  //unlike investigation card, prompt for pile
+	tax			;   x = oprompt(); // threat to 1 ~ 4, 0 cancel
	bne	+		;  if (x == 0) {
	pla;2->1		;
	pla;1->0		;
	inc	TOFFICE		;   TOFFICE++; //non-movedok() caller, decrement
	lda	#0		;   return a_= 0;
	rts			;  } else {
+	dex			;   x--; // convert from pile 1~4 to offset 0~3
	lda	STACKHT+8,x	;  } // x no longer the card, but user's choice
	bit	tisnext		;
	bne	+		;  if (STACKHT[8+x]&1 == 0) {// but thr showing
	pla;2->1		;
	pla;1->0		;
	inc	TOFFICE		;   TOFFICE++; //non-movedok() caller, decrement
	lda	#0		;   return a = 0;
	rts			;  }
+	pla;2->1		;  // guaranteed STACKHT[8+x] is at least 1!
	tay			;
	pha;2:index in hand 0~3	;
	lda	HAND,y		;
	sta	TEMPVAR		;  TEMPVAR = HAND[y];
	txa			;
	pha;3:off. stack # 0 ~ 3;
	asl			;
	asl			;
	asl			;
	asl			;   //stack#, position above top card in stack
	ora	STACKHT+8,x	;  a_= x<<4 + STACKHT[8+x];
	tay			;  register uint8_t y_ = a;
	lda	STACKHT+8,x	;
	tax			;  for (register uint8_t x_ = STACKHT[8+x]; x_;
-	dey			;       x_--)
	lda	ODRAWER,y	;
	cmp	TEMPVAR		;   if (ODRAWER[y_--] == TEMPVAR)// oops! can't
	beq	+		;    break;//since threat already in this stack
	dex			;
	bne	-		;
	beq	++		;  if (x_ > 0) { // found a conflict
+	pla;3->2		;
	pla;2->1		;
	pla;1->0		;
	inc	TOFFICE		;   TOFFICE++; //non-movedok() caller, decrement
	lda	#0		;   return a = 0;
	rts			;  }
+	pla;3->2		;
	tax			;
	jmp	+		;
playinv	lda	STACKHT+8,x	; } else { // trying to play investigation card
	bit	tisnext		;  // which alternate so need stack height even
	beq	+		;  if (STACKHT[8+x] & 1) {// but inv is showing
	pla;2->1		;
	pla;1->0		;
	inc	TOFFICE		;   TOFFICE++; //non-movedok() caller, decrement
	lda	#0		;
	rts			;   return a = 0;
+	pla;2->1		;  }
	tay			; }
	txa			;
	asl			;
	asl			;
	asl			;
	asl			;   //stack#, position in stack
	ora	STACKHT+8,x	; a_= x<<4 + STACKHT[8+x];
	pha;2:index into ODRAWER;
	inc	STACKHT+8,x	; STACKHT[8+x]++;
	txa			;
	pha;3:off. stack # 0 ~ 3;
	lda	STACKHT+8,x	;
	pha;4:stack level 1 ~ 16;
	jsr	fasthnd		; 
	sta	TEMPVAR		; TEMPVAR = fasthnd(y);// card taken from hand,
	pla;4->3		;
	;adc	stacky+8	;
	;sec			;
	;sbc	#1		;
	tay			; y = stacky[8+x] /* == 1 */ + a_/* - 1 */;
	pla;3->2		;
	tax			;
	lda	stackx+8,x	;
	tax			; x = stackx[8+x];
	lda	TEMPVAR		;
	pha;3:TEMPVAR		;
	clc			;
	jsr	cardsho		; cardsho(0, TEMPVAR, x, y); // shown in office
	pla;3->2		;
	sta	TEMPVAR		;
	pla;2->1		;
	tax			;
	lda	TEMPVAR		;
	sta	ODRAWER,x	; ODRAWER[a] = TEMPVAR;// and placed in drawer
	pla;1->0		; return a; // original value passed in
	ldx	#1		; // clear z flag to indicate succesful exit
	rts			;} // move_of()

move_cs	pha;1:0~3 since move_cs	;uint8_t move_cs(register uint3_t a) {
	;and	#$03		;
	jsr	fromhnd		;
	sta	TEMPVAR		; TEMPVAR = fromhnd(a);// card taken from hand,
	tax			; register uint3_t x = TEMPVAR;//shown in scene
	inc	STACKHT,x	; STACKHT[x]++;//(equivalent to placing a card)
	lda	stacky,x	;
	pha;2:y location of slot;
	lda	stackx,x	;
	pha;3:x location of slot;
	lda	STACKHT,x	;
	cmp	#2		;
	bcs	+		; if (STACKHT[x] < 2) // 1st card harmless, but
	pla;3->2		;
	tax			;
	pla;2->1		;
	tay			;
	clc			;
	lda	TEMPVAR		;
	jsr	cardsho		;  cardsho(0, TEMPVAR, stackx[x], stacky[x]);
	jmp	+++		; } else { // wounded from adding 2nd if threat
+	lda	#0		;
	sta	STACKHT,x	;  STACKHT[x] = 0;// reset the height here to 0
	pla;3->2		;
	tax			;
	pla;2->1		;
	tay			;
	clc			;
	lda	TEMPVAR		;
	pha;2: card from hand	;
	lda	#NONCARD	;
	jsr	cardsho		;  cardsho(0, NONCARD, stackx[x], stacky[x]);
	pla;2->1		;
	pha;2: card from hand	;
	ldy	DISCREM		;  // discard both cards that have accumulated
	sta	DISCARD,y	;
	iny			;  DISCARD[DISCREM++] = a;
	;sty	DISCREM		;
	sta	DISCARD,y	;
	iny			;
	sty	DISCREM		;  DISCARD[DISCREM++] = a;
	jsr	discsho		;  discsho();   
	pla;2->1		;
	bit	threatm		;
	beq	+		;  if (a & 0x04) {// threat card, won't cascade
	inc	NWOUNDS		;   NWOUNDS++;
	digitxy	NWOUNDS,WDX,WDY	;   digitxy(NWOUNDS, WDX, WDY);
	jmp	++		;  } else // may re-call movedok() re-entrant
+	jsr	invest2		;   invest2(a); } }
+	pla;1->0		; return a = y;
	rts			;} // move_cs()

movedok	bit	officem		;uint8_t movedok(register uint8_t& a) {
	beq	++		; if (a & 0x04) { // trying dec TOFFICE first,
	dec	TOFFICE		;                // will inc it back if invalid
	bpl	+		;  if (--TOFFICE < 0) {// already played 2 cards
	inc	TOFFICE		;   TOFFICE = 0;
	ldy	TOSCENE		;
	cpy	HANDREM		;
	bne	+		;   if (TOSCENE == HANDREM) // and rest of hand:
	lda	#0		;// required to meet minimum of 2 played TOSCENE
	rts			;    return a = 0; // z set
+	jsr	move_of		;  } else if (!move_of(a)) {
	bne	movepwr		;   return a = 0;//tried play-to-office, invalid
	rts			;  }
+	dec	TOSCENE		; } else { // trying dec TOSCENE, always valid
	bpl	+		;  if (--TOSCENE < 0) {// already played 2 cards
	inc	TOSCENE		;   TOSCENE = 0;
	ldy	TOFFICE		;
	cpy	HANDREM		;
	bne	+		;   if (TOFFICE == HANDREM) // and rest of hand:
	lda	#0		;// required to meet minimum of 2 played TOFFICE
	rts			;    return a = 0; // z set
+	jsr	move_cs		;  } else move_cs(a);//no check, is always valid
movepwr	tay			; }
	iny			;
	lda	#0		;
	sec			;
-	rol			;
	dey			;
	bne	-		; return 1<<a; // z clear
	and	#$ff		;
	rts			;} // movedok()

invest2	pha			;void invest2 (uint2_t a) {
	jsr	handsho		; handsho(); // show gap for the played card
	pla			;
	ror			;
	ror			; // a will contain an investigation card (0~3)
	ror			; // corresponding to a face value of 1~4
	sta	TEMPVAR		;
	bit	TEMPVAR		; // that just completed a pair at the scene
	bmi	++		; switch (a & 0x03) {
	bvs	+		; case 0: double1();
	jmp	double1		; case 1: double2();
+	jmp	double2		; case 2: double3();
+	bvs	+		; case 3: double4();
	jmp	double3		; }
+	jmp	double4		;} // invest2()

finishr	lda	#0		;void finishr(void) {
-	pha			; register uint8_t a, x, y;
	pha			; for (a = 0; a < 8; a++) {
	asl			;
	asl			;
	clc			;
	adc	#8		;
	tax			;  x = 4 * a + 8; // cols 8, 12, ..., 36
	pla			;
	lsr			;
	ldy	#$17		;  y = 23;
 	sec			;
	jsr	cardsho		;  cardsho(1, a >> 2, x, y); //a=1,1,2,2,3,3,4,4
	pla			;
	clc			;
	adc	#$01		;
	and	#$07		;
	bne	-		; }
	lda	abillen		;
	ldx	abilix		;
	ldy	abiliy		;
	printxy	ability		; printxy(ability, abilix, abiliy, abillen);
	lda	solvlen		;
	ldx	solvex		;
	ldy	solvey		;
	printxy	tosolve		; printxy(tosolve, solvex, solvey, solvlen);
	rts			;} // finishr()

cardsho	php			;void cardsho(uint1_t& c, register uint8_t& a,
	pha			;             register uint8_t& x,
	cpx	#SCREENW	;             register uint8_t& y) {
	bcs	+++		; if (x < SCREENW) { // x >= SCREENW is special
	lda	#<SCREENM	;
	sta	selfsha		; 
	lda	#>SCREENM	;
	sta	1+selfsha	;  selfsha = SCREENM;
	cpy	#SCREENH-1	;
	bcc	+		;  if (y >= SCREENH - 1)
	pla			;
	pla			;
	jmp	cardrts		;   return; // not allowed to draw below screen
+	tya			;
	beq	+		;
-	clc			;
	lda	#SCREENW	;
	adc	selfsha		;
	sta	selfsha		;
	lda	#0		;
	adc	1+selfsha	;
	sta	1+selfsha	;
	dey			;
	bne	-		;
+	txa			;
	clc			;
	adc	selfsha		;
	sta	selfsha		;
	sta	ZP		;
	lda	#0		;
	adc	1+selfsha	;
	sta	1+selfsha	;  selfsha = y * SCREENW + x; // card's top left
	sta	1+ZP		;  zp = selfsha; // for drawsho()
	clc			;
;	.if <+SCREEND
;	lda	selfsha		;
;	adc	#<+SCREEND	;
;	sta	selfcla		;
;	lda	1+selfsha	;
;	.endif
	adc	#>+SCREEND	;
	sta	1+selfcla	;
;	.if !<+SCREEND		;
	lda	selfsha		;
	sta	selfcla		;  selfcla = 0xffff & (selfsha + SCREEND);
;	.endif

+	pla			; } // else skip selfmod, using previous address
	tax			; x = a; // card code, no mask (>= 8 "not card")
	cmp	#$08		;
	bcs	blankit		; if (a < 0x08) { // cards 0 ~ 7 index cardnum[]

	and	#$07		;
	tay			;  y = a & 0x07; // card code, masked onto 0 ~ 7
	clc			;
	adc	#$fc		;  uint1_t c = (a & 0x04) ? 1 : 0; // bit 2 to c

	lda	cardnum,y	;  a = cardnum[y];
	ldy	#0		;
	jsr	selfsho		;  selfsho(cardnum[y], y = 0); // 1 ~ 4, A ~ D

	bcc	+		;
	lda	threatc,y	;
	bcs	++		;
+	lda	investc,y	;
+	iny			;
	jsr	selfsho		;  selfsho(c ? threatc[0] : investc[0], y = 1);

	bcc	+		;
	lda	threatc,y	;
	bcs	++		;
+	lda	investc,y	;
+	iny			;
	jsr	selfsho		;  selfsho(c ? threatc[1] : investc[1], y = 2);
	ldy	#0		;  y = 0; // done drawing top of bona fide card
	beq	+		; } else {

blankit	lda	#$a0		;
	ldy	#2		;
	jsr	selfsho		;
	dey			;  selfsho(0xa0, y = 2);
	jsr	selfsho		;  selfsho(0xa0, y = 1);
	dey			;  selfsho(0xa0, y = 0);
	jsr	selfsho		; }

+	plp			; y = 0;
	bcs	+		; if (c || // caller explicitly requested a top
	lda	1+selfsha	;
	cmp	#1+>ONLYTOP	;
	bcs	+		;
	cmp	#>ONLYTOP	;
	bne	++		;
	lda	selfsha		;
	cmp	#<ONLYTOP	;
	bcc	++		;     (selfsha >= ONLYTOP)) { //in bottom 8 rows
+	cpx	#8		;  if (x >= 8)
	bcs	cardtop		;   goto cardtop; // draw the blank card
	ldy	#2		;  else
	bcc	colrtop		;   goto colrtop; // only finish the color info

+	ldy	#SCREENW*5	; }
-	tya			; for (y = SCREENW*5; y; ) {
	sec			;
	sbc	#SCREENW	;
	tay			;  y -= SCREENW; // SCREENW*4 down to 0 (or *1?)
	bne	+		;  if (y == 0) {
	cpx	#NONCARD	; 
	bcs	+		;   if (x < 8 || x == REVCARD) {
	iny			;    y = 2;
	iny			;    a = 0;
	lda	#0		;    if (x == REVCARD) goto colrto2;    
	cpx	#REVCARD	;    else goto colrtop;
	beq	colrto2		;   }
	bne	colrtop		;  }
+	lda	#$a0		;
	cpx	#9;8		;  // drawing card body or a blank top
	bcc	+		;  cardtop:
cardtop	lda	#$20		;   a = (x<8) ? 0xa0 /*solid*/ : 0x20 /*blank*/;
+	jsr	selfsho		;   selfsho(a, y);
	iny			;
	jsr	selfsho		;   selfsho(a, ++y);
	iny			;
	jsr	selfsho		;   selfsho(a, ++y);
;	cpx	#8		;  }
;	bcc	colrtop		;
;	cpy	#0		;  if (x<8) { // only apply color to non-erasure
;	bne	-		;
;	beq	cardrts		;  colrtop:
	lda	 #0		;
	cpx	 #8		;
	beq	colrto2		;
colrtop	lda	cardclr,x	;   a = cardclr[x];
colrto2	jsr	selfclr		;   selfclr(a, y);
	dey			;
	jsr	selfclr		;   selfclr(a, --y);
	dey			;
	jsr	selfclr		;   selfclr(a, --y);
	;cpy	#0
	beq	cardrts		;   if (y == 0 || y <= SCREENW) break;
	cpy	#SCREENW;+1	;  }   // express exit, don't overwrite top    
	bcs	-		; }
cardrts	rts			;} // cardsho()
selfsho	.byte	$99	; sta,y	;static uint8_t* selfsha = SCREENM;
selfsha	.byte	<SCREENM	;void selfsho(uint8_t a, uint8_t y) {
	.byte	>SCREENM	; selfsha[y] = a; // sta $XXXX,y
	rts			;} // selfsho()
selfclr	.byte	$99	; sta,y	;static uint8_t* selfclr = SCREENC;
selfcla	.byte	<SCREENC	;void selfclr(uint8_t a, uint8_t y) {
	.byte	>SCREENC	; selfcla[y] = a; // sta $XXXX,y
	rts			;} // selfclr()

initstk	lda	#0		;void initstk(void) {
	ldx	#$0c		; for (register uint8_t x = 11; x >= 0; x--)
-	sta	STACKHT-1,x	;
	dex			;  STACKHT[x] = 0;
	bne	-		; }
	rts			;} // initstk()

shuffle	ldx	#$ff		;void shuffle(register uint8_t& a) {
	tay			;
	beq	shuffl1		; if (a > 0) { // # cards to copy from stddeck
	sta	DECKREM		;  DECKREM = a;
-	lda	stddeck-1,y	;  for (register int8_t y = a-1; y >= 0; y--) {
	sta	DECK-1,y	;   DECK[y] = stddeck[y];
	dey			;  }
	bne	-		; }
shuffl1	ldy	#$ff		; for (register uint8_t x = 255; x; x--) {
shuffl2	txa			;  for (register uint8_t y = 255; y; y--) {
	pha			;
	tya			;   register uint8_t x, y; // local
	pha			;
	lda	RNDLOC1		;   for (x = (*RNDLOC1 ^ *RNDLOC2) >> 1;
	eor	RNDLOC2		;        x >= *DECKREM;
-	lsr			;        x >>= 1)
	cmp 	DECKREM		;
	bcs	-		;    ; // x now a valid index into the deck
	tax			;
	lda	RNDLOC1		;   for (y = (*RNDLOC1 ^ *RNDLOC2) >> 1;
	eor	RNDLOC2		;        y >= *DECKREM;
-	lsr			;        y >>= 1)
	cmp 	DECKREM		;
	bcs	-		;    ; // y now a valid index into the deck
	tay			;
	lda	DECK,x		;
	pha			;   uint8_t temp = DECK[x];
	lda	DECK,y		;
	sta	DECK,x		;   DECK[x] = DECK[y];
	pla			;
	sta	DECK,y		;   DECK[y] = temp;
	pla			;
	tax			;
	pla			;
	tay			;
	dey			;
	bne	shuffl2		;  }
	dex			;
	bne	shuffl1		; }
	rts			;} // shuffle()

animshf	lda 	#DECKSIZ	;void animshf(void) {
	jsr	shuffle		; shuffle(/* DECKREM =*/ DECKSIZ);
	ldx	#$14		;
-	txa			;
	pha			; for (register uint8_t x = 20; x; x--) {
	lda	#0		;
	jsr	shuffle		;  shuffle(0);
	lda	DECK		;
	ldx	drawx		;
	ldy	drawy		;
	clc			;
	jsr	cardsho		;  cardsho(0, DECK[0], drawx, drawy);
	pla			;
	tax			;
	dex			;
	bne	-		; }
	rts			;} // animshf()

drw1new	ldx	DECKREM		;int8_t drw1new(void) {
	dex			; // x gets clobbered hence drw4hand() loop on y
	txa			; if (DECKREM <= 0)
	bmi	+		;  return /*DECKREM*/ -1;
	stx	DECKREM		; else
	lda	DECK,x		;  return DECK[--DECKREM];
+	rts			;} // drw1new()

drwflag	.byte	$01,$02,$04,$08	;static const uint8_t drwflag[] = {1, 2, 4, 8,
	.byte	$10,$20,$40,$80	;                                 16,32,64,128};
drw4hnd	lda	HANDREM		;void drw4hand(void) {
	bne	++		; if (HANDREM == 0) { // should've emptied first
	ldy	#8		;  register uint8_t a, x, y;
-	sta	HANDHST-1,y	;  for (y = 7; y >= 0; y--)
	dey			;   HANDHST[y] = 0; // clear the card histogram
	bne	-		;  uint8_t retval = 0; // return figure of merit
	ldy	#4		;
-	pha			;  for (y = 3; y >= 0; y--) {
	jsr	drw1new		;   a = drw1new();
	bpl	+		;   if (a < 0) { // deck ran out
	tya			;
	pha			;
	ldx	DISCREM		;    x = DISCREM;
	stx	DECKREM		;
-	lda	DISCARD-1,x	;    for (DECKREM = x--; x >= 0 ; x--) {
	sta	DECK-1,x	;     DECK[x] = DISCARD[x];
	dex			;
	bne	-		;    }
	stx	DISCREM		;    DISCREM = 0;
	txa			;
	jsr	shuffle		;    shuffle(0); // exists so don't copy stddeck
	pla			;
	tay			;
	jsr	drw1new		;    a = drw1new();
	bpl	+		;    if (a < 0) // all cards evaporated somehow?
	pla			;
	rts			;     return retval; // exit with what we've got
+	tax			;   } // a is a valid card in the range 0 ~ 7
	sta	HAND-1,y	;   HAND[y] = a /* & 0x07 */;
	inc	HANDHST,x	;   HANDHST[a]++; // reflected in hand histogram
	inc	HANDREM		;   HANDREM++; // hand size grows toward 4 by 1
	pla			;
	ora	drwflag,x	;   retval |= drwflag[a]];
	dey			;  }
	bne	--		;
	tax			;  return retval; // Z should be clear (nonzero)
	rts			; } else
+	lda	#0		;  return 0; // Z will be set
	rts			;} // drw4hand()

rejctok	sta	TEMPVAR		;uint8_t rejctok(register uint8_t& a) {
	and	#$0f		; if (a & 0x0f == 0) // got no investig. cards
	beq	rejrtn4		;  return 4;
	lda	TEMPVAR		;
	and	#$f0		; else if (a & 0xf0 == 0) // got no threat cards
	beq	rejrtn4		;  return 4;
	lda	#3		;
	ldy	#8		;  for (register int8_t y = 7; y >= 0; y--)
-	cmp	HANDHST-1,y	;   if (HANDHST[y] >= 3)
	beq	rejrtn3		;    return 3; // indeed have 3 copies of card y
	dey			;
	bne	-		; }
rejrtn0	lda	#$fc		; return 0;
rejrtn4	clc			;
	adc	#4		;
rejrtn3	rts			;} // rejctok()

handsho	ldx	#4		;void handsho(void) { // just paint them for now
-	txa			;
	pha			; for (register int8_t x = 4; x>=0; x--) {
	lda	HAND-1,x	;  register int8_t a = HAND[x];
	pha			;
	lda	inhandx-1,x	;
	tax			;
	clc			;
	pla			;
	ldy	inhandy ;-1,x	;
	jsr	cardsho		;   cardsho(0, a, inhandx[a], inhandy[a]);
	pla			;  }
	tax			; }
	dex			; drawsho();
	bne	-		;} // handsho()
drawsho	ldx	drawx		;void drawsho(void) {
	ldy	drawy		; register uint8_t x, y;
	lda	DECKREM		;
	beq	+		; if  (DECKREM) {
	lda	#REVCARD	;  // draw just the blank card back
	clc			;
	jsr	cardsho		;  cardsho(0,a = REVCARD, x = drawx, y = drawy);
	lda	#$bf		;
	ldy	cardofs+7	;  printxy("?"       , drawx + 1, drawy + 2, 1);
	sta	(ZP),y		;
	rts			; } else {
+	jsr	cardout		;  cardout(x = drawx, y = drawy);
	lda	#$0f		;
	ldy	cardofs+3+1	;  printxy("O"  - "@", drawx + 1, drawy + 1, 1);
	sta	(ZP),y		;
	lda	#$15		;
	ldy	cardofs+3+3+1	;  printxy("U"  - "@", drawx + 1, drawy + 2, 1);
	sta	(ZP),y		;
	lda	#$14		;
	ldy	cardofs+3+3+3+1	;  printxy("T"  - "@", drawx + 1, drawy + 3, 1);
	sta	(ZP),y		; }
	rts			;} // drawsho()

discsho	ldx	discx		;void discsho(void) {
	ldy	DISCREM		;
	beq	+		; if (DISCREM)
	lda	DISCARD-1,y	;
	ldy	discy		;
	clc			;  cardsho(0, a = DISCARD[DISCREM-1],
	jsr	cardsho		;          x = discx, y = discy);
	rts			; else
+	ldy	discy		;
	jsr	cardout		;  cardout(x = discx,  y = discy);
	rts			;} // discsho()

cardout	clc			;void cardout(register uint8_t& x,
	lda	#NONCARD	;             register uint8_t& y) { // clear it:
	jsr	cardsho		; cardsho(0,a = NONCARD, x = drawx, y = drawy);
	ldx	#cardofs-pileout; // cardsho() left ZP pointing to the top left
-	lda	cardofs-1,x	; for (int8_t x = 3*5 - 1; x > 0; x--) {
	tay			;
	lda	pileout-1,x	;
	sta	(ZP),y		;  zp[cardofs[x]] = pileout[x];
	dex			;
	bne	-		; }
	rts			;} // cardout()

				;uint1_t askquit(void) {
askquit	handmsg	askmsg0,askmsg1-askmsg0,askmsg2-askmsg1,SCRATCH
-	jsr	$ffe4		; handmsg("DISCARD & REDRAW?"/*RVS ON*/
	beq	-		;         " PRESS Y FOR YES "/*RVS OFF*/, 17, 17,
	and	#$df		;         SCRATCH);
	cmp	#$59		; register uint8_t a = getchar();
	php			;handmsg(SCRATCH, 17, 17); // pop backing store
	handmsg	SCRATCH,askmsg1-askmsg0,askmsg2-askmsg1
	plp			;
	bne	quitn		;
quity	lda	#1		;
	rts			;
quitn	lda	#0		; return (a & 0xdf == 'y');
	rts			;} // askquit()
askmsg0	.byte	$12,$05,$01,$0c	; REAL
	.byte	$0c,$19,$20,$11	; LY Q
	.byte	$15,$09,$14,$20	; UIT
	.byte	$07,$01,$0d,$05	; GAME
	.byte	$3f		; ?
askmsg1	.byte	$a0,$90,$92,$85	;  PRE
	.byte	$93,$93,$a0,$99	; SS Y 
	.byte	$a0,$86,$8f,$92	;  FOR
	.byte	$a0,$99,$85,$93	;  YES
	.byte	$a0		;
askmsg2
				;uint1_t handrej(void) {
handrej	handmsg	rejmsg0,rejmsg1-rejmsg0,rejmsg2-rejmsg1,SCRATCH
-	jsr	$ffe4		; handmsg("DISCARD & REDRAW?"/*RVS ON*/
	beq	-		;         " PRESS Y FOR YES "/*RVS OFF*/, 17, 17,
	and	#$df		;         SCRATCH);
	cmp	#$59		; register uint8_t a = getchar();
	php;1:z==getchar()=='y'	; handmsg(SCRATCH, 17, 17); // pop backing store
	handmsg	SCRATCH,rejmsg1-rejmsg0,rejmsg2-rejmsg1
	plp;1->0		;
	bne	rejectn		; if (a == 'Y' || a == 'y') {
	ldx	DISCREM		;  register uint8_t x = DISCREM;
	ldy	HANDREM		;
-	dey			;  for (register int8_t y=HANDREM-1; y>=0; y--){
	bmi	+		;
	jsr	fasthnd		;   a = fasthnd(y);
	sta	DISCARD,x	;
	inx			;   DISCARD[x++] = a;
	bne	-		;  }
+
	stx	DISCREM		;  DISCREM = x;
	jsr	discsho		;  discsho();

rejecty	lda	#1		;  return 1;
	rts			; } else
rejectn	lda	#0		;  return 0;
	rts			;} // handrej()
rejmsg0	.byte	$04,$09,$13,$03	; DISC
	.byte	$01,$12,$04,$20	; ARD 
	.byte	$26,$20,$12,$05	; & RE
	.byte	$04,$12,$01,$17	; DRAW
	.byte	$3f		; ?
rejmsg1	.byte	$a0,$90,$92,$85	;  PRE
	.byte	$93,$93,$a0,$99	; SS Y 
	.byte	$a0,$86,$8f,$92	;  FOR
	.byte	$a0,$99,$85,$93	;  YES
	.byte	$a0		;
rejmsg2


drawone	jsr	drw1new		;void drawone(void) { int8_t a = drw1new();
	bpl	+		; if (a < 0) { draw deck empty?!? impossible?!?
	inc	NWOUNDS		;  NWOUNDS++;
	digitxy	NWOUNDS,WDX,WDY	;  digitxy(NWOUNDS, WDX, WDY);
	rts			;  return;
+	jsr	intohnd		; } else intohnd(a);
	jmp	handsho		;} // drawone()


arwstat	.byte	$ff
arwchar	.byte	$e9,$df,$5f,$69
nxtchar	.byte	+2*SCREENW-1,+4,+SCREENW-4,+4
arwdisc	sta	TEMPVAR		;void arwdisc(uint8_t& a) {
	ldx	discx		; TEMPVAR = a;
	ldy	discy		; register uint8_t x = *discx + *discy *SCREENW;
-	txa			; for (uint8_t y = 0; y < 4; y++) {
	clc			;
	adc	nxtchar,y	;
	tax			;  x += nxtchar[y];
	lda	TEMPVAR		;
	and	#$20		;
	bne	+		;
	lda	arwchar,y	;
+	sta	SCREENM,x	;  SCREENM[x] = (TEMPVAR&0x20) ? ' ':arwchar[y];
	iny			;
	cpy	#4		;
	bne	-		; }
	rts			;} // arwdisc()

findone ldx	DISCREM		;void findone(void) {
	bne	canfind		; if (DISCREM == 0) { // must draw, but can't
	inc	NWOUNDS		;  NWOUNDS++;
	digitxy	NWOUNDS,WDX,WDY	;  digitxy(NWOUNDS, WDX, WDY);
	rts			;  return;
canfind	cpx	#1		; }
	bne	+		;
	jmp	discar1		; if (DISCREM > 1) { // able to choose so prompt
+	lda	#0		;
	jsr	arwdisc		;  arwdisc(0); // draw L/R arrows beside discard
	handmsg	fndmsg0,0,fndmsg1-fndmsg0,SCRATCH
getfind	jsr	$ffe4		;  do {
	beq	getfind		;   uint8_t a = getchar();
	cmp	#$9d		;
	bne	+		;   if (a == 0x9d) { // L arrow cycle pile down

	ldy	#1		;
	lda	discard		;
	sta	TEMPVAR		;    TEMPVAR = discard[0]; // save for [DISCREM]
-	lda	discard,y	;    for (register int8_t y = 1; y<DISCREM; y++)
	sta	discard-1,y	;     discard[y] = discard[y+1]; // next-highest
	iny			;
	cpy	DISCREM		;
	bcc	-		;
	lda	TEMPVAR		;
	sta	discard-1,y	;    discard[(/* y =*/ DISCREM) - 1] = TEMPVAR;   
	jsr	discsho		;
	jmp	getfind		;

+	cmp	#$1d		;
	bne	+		;   } else if (a == 0x1d) { // R arrow cycle up

	lda	DISCREM		;
	tay			;
	dey			;
	beq	getfind		;
	lda	discard,y	;
	sta	TEMPVAR		;    TEMPVAR = discard[DISCREM-1]; // for [0]
-	lda	discard-1,y	;    for (register int8_t y = DISCREM-1;y>0;y--)
	sta	discard,y	;     discard[y-1] = discard[y]; // next-lowest
	dey			;
	bne	-		;
	lda	TEMPVAR		;
	sta	discard;,y	;    discard[/* y =*/ 0] = TEMPVAR;
	jsr	discsho		;
	jmp	getfind		;

+	cmp	#$0d		;
	bne	getfind		;   } else if (a == '\r') { // Return to select
	handmsg	SCRATCH,0,fndmsg1-fndmsg0
	lda	#$20		;    arwdisc(' '); // blank the pile L/R arrows
	jsr	arwdisc		;    break;
	ldx	DISCREM		;   }
discar1	dex			;  }
	stx	DISCREM		; }
	lda	discard,x	;
	jsr	intohnd		; intohnd(discard[--DISCREM]);
	jsr	discsho		; discsho();
	jsr	handsho		; handsho();
	rts			;} // findone()
fndmsg0	.byte	$8c,$af,$92,$a0	; L/R
	.byte	$81,$92,$92,$8f	; ARRO
	.byte	$97,$93,$ac,$92	; WS,R
	.byte	$85,$94,$95,$92	; ETUR
	.byte	$8e		; N
fndmsg1				;//FIXME - static storage - not reentrant!

inv_l2r	lda	#0		;void inv_l2r(void) {
	jsr	pickl2r		; register uint4_t a = pickl2r(0);
 	jmp	sendl2r		; return sendl2r(a);
	;rts			;} // inv_l2r()

thr_l2r lda	#4		;void thr_l2r(void) {
	jsr	pickl2r		; register uint4_t a = pickl2r(4);
 	jmp	sendl2r		; return sendl2r(a);
	;rts			;} // thr_l2r()

TOP_ROW	= SCREENM+SCREENW*1
BOT_ROW	= SCREENM+SCREENW*9

pickl2r	tax			;uint4_t pickl2r(register uint3_t a) {// a={0,4}
-	lda	STACKHT,x	; for (register uint4_t x = a; x < a + 4; x++) {
	bne	+		;  if (STACKHT[x] > 0) break;
	inx			;
	txa			;
	and	#$03		;
	bne	-		; }
	inc	NWOUNDS		; if (x == a + 4) {
	digitxy	NWOUNDS,WDX,WDY	;  digitxy(++NWOUNDS, WDX, WDY); // FIXME: digitxy() redundant?
	lda	#NONCARD	;
	rts			;  return NONCARD;
+	txa			; }
	pha			;
	handmsg	plrmsg0,plrmsg1-plrmsg0,plrmsg2-plrmsg1,SCRATCH
	pla			;
	tax			;
	cpx	#$04		;
	bcc	pickbot		; if (x >= 4) // threat cards in top row
	pick_cs	TOP_ROW		;  a = x = pick_cs(TOP_ROW, x);
	jmp	picktor		; else // investigation cards in bottom row
pickbot	pick_cs	BOT_ROW		;  a = x = pick_cs(BOT_ROW, x);
picktor	;php			;
	pha			;
	;tax			;
	lda	#0		;
	sta	STACKHT,x	; STACKHT[x] = NONCARD; // reset the height to 0
	lda	stacky,x	;
	tay			;
	lda	stackx,x	;
	tax			;
	lda	#NONCARD	;
	clc			;
	jsr	cardsho		; cardsho(0, NONCARD, stackx[x], stacky[x]);
	handmsg	SCRATCH,plrmsg1-plrmsg0,plrmsg2-plrmsg1
	pla			; return a;
	;plp			;
	rts			;} // pickl2r()
plrmsg0	.byte	$17,$08,$09,$03	; WHIC
	.byte	$08,$20,$0f,$0e	; H ON
	.byte	$05,$20,$14,$0f	; E TO
	.byte	$20,$15,$13,$05	;  USE
	.byte	$3f		; ?
plrmsg1	.byte	$8c,$af,$92,$a0	; L/R
	.byte	$81,$92,$92,$8f	; ARRO
	.byte	$97,$93,$ac,$92	; WS,R
	.byte	$85,$94,$95,$92	; ETUR
	.byte	$8e		; N
plrmsg2

sendl2r	cmp	#NONCARD	;void sendl2r(register uint8_t a) {
	beq	++		; if (a == NONCARD) return;// wound in pickl2r()
	jsr	intohnd		; a = intohnd(a);// momentary hand slot 1 ~ 4
	pha			; register uint8_t x, y;
	clc			; // 1,2,3,4
	adc	#3		; a += 3; // 4,5,6,7 i.e. moving to office
	jsr	move_of		; // spoof playing that card from hand to office
	bne	+		; if ((x = move_of(a)) == 0) {//FIXME?: -1 not checked
	pla			;
	pha			;
	tax			;
	lda	HAND-1,x	;
	ldy	DISCREM		;
	sta	DISCARD,y	;
	iny			;
	sty	DISCREM		;  DISCARD[DISCREM++] = HAND[x - 1];//discard it
	lda	#NONCARD	;
	sta	HAND-1,x	;  HAND[x - 1] = NONCARD;// clear momentary slot
	dec	HANDREM		;  HANDREM--; // intohnd() bumped it up, so undo
	dec	TOFFICE		;  TOFFICE--; // undo spurious inc by move_of()
	inc	NWOUNDS		;  digitxy(++NWOUNDS, WDX, WDY); // FIXME: digitxy() redundant?
	digitxy	NWOUNDS,WDX,WDY	; }
+	pla			;
+	rts			;} // sendl2r()

thr_r2l	lda	#1		;
	jsr	pickr2l		;
	jmp	sendr2l		;
	;rts

inv_r2l lda	#0		;
	jsr	pickr2l		;
	jmp	sendr2l		;
	;rts

pickr2l	pha			;
	handmsg	prlmsg0,prlmsg1-prlmsg0,prlmsg2-prlmsg1,SCRATCH
	pla			;
	pick_of			; uint8_t* pick_sm;
pick_sm
	pha			;
	handmsg	SCRATCH,prlmsg1-prlmsg0,prlmsg2-prlmsg1
	pla			;
	tax			; x = pick_of(a, &pick_sm);
	lsr			;
	lsr			;
	lsr			;
	lsr			;
	tay			; y = x >> 4;
	txa			;
	and	#$0f		;
	cmp	STACKHT+8,y	;
	bcc	+		; if ((x & 0x0f) >= STACKHT[y+8]) {// off stack
 	inc	NWOUNDS		;  NWOUNDS++; // FIXME: jump up and try again?!?
	digitxy	NWOUNDS,WDX,WDY	;  digitxy(NWOUNDS, WDX, WDY);
	lda	#NONCARD	;  return NONCARD;
	rts			; }
+	lda	ODRAWER,x	;
	pha			; uint8_t stack = ODRAWER[x];
	lda	#NONCARD	;
	sta	ODRAWER,x	; ODRAWER[x] = NONCARD;
	lda	pick_sm-2	;
	sta	picksm8+1	;
	sta	picksm9+1	;
	lda	1+pick_sm-2	;
	sta	1+picksm8+1	;
	sta	1+picksm9+1	;
	lda	#$66		;
	ldy	#2		;
picksm8	sta	$ffff,y		; pick_sm[2] = 0x66; // crosshatch
	dey			; pick_sm[1] = 0x66;
	bpl	picksm8		; pick_sm[0] = 0x66;
	txa			;
	lsr			;
	lsr			;
	lsr			;
	lsr			;
	tay			; y = x >> 4;
	txa			;
	and	#$0f		;
	clc			;
	adc	#1		;
	cmp	STACKHT+8,y	;
	bne	+		; if ((x & 0x0f) == STACKHT[y+8]-1) { //took top
	tya			;
	tax			;
	dec	STACKHT+8,x	;  STACKHT[y+8]--; // reduce height only if last
	ldx	#5		;  for (x = 5; x > 0; x--) {
-	ldy	#2		;
	lda	#$66		;
picksm9	sta	$ffff,y		;   pick_sm[2] = 0x66; // crosshatch
	dey			;   pick_sm[2] = 0x66;
	bpl	picksm9		;   pick_sm[0] = 0x66;
	lda	picksm9+1	;
	clc			;
	adc	#SCREENW	;
	sta	picksm9+1	;
	lda	1+picksm9+1	;
	adc	#0		;
	sta	1+picksm9+1	;   pick_sm+ += SCREENW;
	dex			;  }
	bne	-		; }
+	pla			; return stack;
	rts			;} // pickr2l()
prlmsg0	.byte	$17,$08,$09,$03	; WHIC
	.byte	$08,$20,$0f,$0e	; H ON
	.byte	$05,$20,$14,$0f	; E TO
	.byte	$20,$15,$13,$05	;  USE
	.byte	$3f		; ?
prlmsg1	.byte	$95,$af,$84,$af	; U/D/
	.byte	$8c,$af,$92,$a0	; L/R 
	.byte	$8f,$92,$a0,$92	; OR R
	.byte	$85,$94,$95,$92	; ETUR
	.byte	$8e		; N
prlmsg2

sendr2l	cmp	#NONCARD	;void sendr2l(register uint8_t a) {
	beq	+		; if (a == NONCARD) return;// wound in pickr2l()
	jsr	intohnd		; a = intohnd(a);// momentary hand slot 1 ~ 4
	sec			; // 1,2,3,4
	sbc	#1		; a--; // 0,1,2,3
	jsr	move_cs		; // spoof playing that card from hand to cscene
	bne	+		; if (move_cs(a) == 0) {//FIXME?: -1 not checked
	dec	TOSCENE		;  TOSCENE--; // undo spurious inc by move_of()
	inc	NWOUNDS		;  digitxy(++NWOUNDS, WDX, WDY); // FIXME: digitxy() redundant?
	digitxy	NWOUNDS,WDX,WDY	; }
+	rts			;} // sendl2r()

				;uint8_t warning(void) {
warning	handmsg	wrnmsg0,wrnmsg1-wrnmsg0,wrnmsg2-wrnmsg1,SCRATCH
-	jsr	$ffe4		; handmsg("CANNOT PLAY THERE"/*RVS ON*/
	beq	-		;         "PRESS Y FOR WOUND"/*RVS OFF*/,17,17,
	and	#$df		;         SCRATCH);
	cmp	#$59		; register uint8_t a = getchar();
	php			; handmsg(SCRATCH, 17, 17); // pop backing store
	handmsg	SCRATCH,wrnmsg1-wrnmsg0,wrnmsg2-wrnmsg1
	lda	#0		;
	plp			;
	bne	+		;
	lda	#1		; return (a=='Y' || a=='y');// # wounds accepted
+	and	#$ff		;
	rts			;} // warning()
wrnmsg0	.byte	$03,$01,$0e,$0e	; CANN
	.byte	$0f,$14,$20,$10	; OT P
	.byte	$0c,$01,$19,$20	; LAY
	.byte	$14,$08,$05,$12	; THER
	.byte	$05		; E
wrnmsg1	.byte   $90,$92,$85,$93 ; PRES
	.byte   $93,$a0,$99,$a0 ; S Y 
	.byte   $86,$8f,$92,$a0 ; FOR
	.byte	$97,$8f,$95,$8e	; WOUN
	.byte	$84		; D
wrnmsg2


offmsg0	.byte	$20		;
	.byte	$0f,$0e,$14,$0f	; ONTO
	.byte	$20,$17,$08,$09	;  WHI
	.byte	$03,$08,$20,$10	; CH P
	.byte	$09,$0c,$05,$3f	; ILE?
	.byte	$20		;
offmsg1
offmsg2
oprompt	handmsg	offmsg1,offmsg2-offmsg1,0,SCRATCH+offmsg1-offmsg0
	ldx	#$16		;uint8_t oprompt(void) {
	ldy	#0		; do {
	lda	#offmsg1-offmsg0;  register uint8_t a;
	txtclip	SCRATCH		;
	lda	#offmsg1-offmsg0;
	replace	offmsg0		;
	lda	#0		;
	pick_of	0;only L/R keys	;
	txa			;
	pha			;
	lda	#offmsg1-offmsg0;
	replace	SCRATCH		;
	handmsg	SCRATCH+offmsg1-offmsg0,offmsg2-offmsg1,0
	pla			; handmsg(SCRATCH, 17, 15); // pop backing store
	lsr			;
	lsr			; // FIXME: also dehighlight last-picked card?
	lsr			;
	lsr			;
	adc	#'1'		;
	cmp	#'5'		;
	bcc	+		;
	jmp	oprompt		;
+	cmp	#'0'		;
	bcs	+		;
	jmp	oprompt		; } while (a < '0' || a > '4');
+	sec			;
	sbc	#'0'		; return a - '0';
	rts			;} // oprompt()

.if BASIC
	.byte	$00,$00,(+)-*-3	; (0,0)
b_label	.byte	$14,$08,$05,$20	; THE 
	.byte	$03,$12,$09,$0d	; CRIM 
	.byte	$05,$20,$13,$03	; E SC
	.byte	$05,$0e,$05,$20	; ENE
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$13,$08	;   SH
	.byte	$09,$06,$14,$0c	; IFTL
	.byte	$0f,$03,$0b,$27	; OCK'
	.byte	$13,$20,$0f,$06	; S OF
	.byte	$06,$09,$03,$05	; FICE

+
	.byte	$10,$05,(+)-*-3	; (16,5)
b_arwup	.byte	$4e		; /
+

+	.byte	$04,$06,(+)-*-3	; (4,6)
b_threa	.byte	$14,$08,$12,$05	; THRE
	.byte	$01,$14,$13,$20	; ATS
	.byte	$20,$20,$20,$20	;
	.byte	$04,$09,$13,$2d	; DIS-

+	.byte	$03,$07,(+)-*-3	; (3,7)
b_wound	.byte	$97,$8f,$95,$8e	; WOUN
	.byte	$84,$93,$ba,$a0	; DS:
	.byte	$bf,$20,$20,$20	; ?
	.byte	$20,$03,$01,$12	;  CAR
	.byte	$04		; D

+	.byte	$01,$08,(+)-*-3	; (1,8)
b_inves	.byte	$09,$0e,$16,$05	; INVE
	.byte	$13,$14,$09,$07	; STIG
	.byte	$01,$14,$09,$0f	; ATIO
	.byte	$0e,$13		; NS

+	.byte	$10,$09,(+)-*-3	; (16,9)
b_dpile	.byte	$10,$09,$0c,$05	; PILE

+	.byte	$12,$0c,(+)-*-3	; (18,12)
b_draw	.byte	$04,$12,$01,$17	; DRAW

+	.byte	$12,$0e,(+)-*-3	; (18,14)
b_deck	.byte	$04,$05,$03,$0b	; DECK

+	.byte	$01,$0f,(+)-*-3	; (1,15)
b_fodds	.byte	$1e,$06,$31,$20	; ^F1 
	.byte	$1e,$06,$33,$20	; ^F3
	.byte	$1e,$06,$35,$20	; ^F5
	.byte	$1e,$06,$37,$20	; ^F7

+
	.byte	$15,$0f,(+)-*-3	; (21,15)
b_arwdn	.byte	$4e		; /
+

+	.byte	$01,$15,(+)-*-3	; (1,21)
b_feven	.byte	$06,$32,$40,$20	; F2-
	.byte	$06,$34,$40,$20	; F4-
	.byte	$06,$36,$40,$20	; F6-
	.byte	$06,$38,$40,$20	; F8-
	.byte	$14,$0f,$20,$13	; TO S
	.byte	$0f,$0c,$16,$05	; OLVE
	.byte	$3a,$20		; :

+	.byte	$00,$17,(+)-*-3	;
b_pairg	.byte	$10,$01,$09,$12	; PAIR
	.byte	$09,$0e,$07,$20	; ING 
	.byte	$20,$20,$20,$2b	;    +
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20,$2b	;    +
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20,$2b	;    +
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20,$2b	;    +
	.byte	$20,$20,$20,$20	;
	.byte	$20,$12,$15,$0c	;  RUL
	.byte	$05,$13,$3a,$20	; ES:
+

petscii	.text	$09,$83,$08	; enable upper/lower case, uppercase, lock upper
	.text	$13,$13		; clear any BASIC 3.5/4 subwindows on the screen

bckdrop	ldx	#bckdrop-petscii;void bckdrop(void) {
-	lda	petscii,x	;
	jsr	$ffd2		;
	dex			;
	bne	-		; printf("%c%c%c%c%c%c", 9, 147, 8, 19, 19);
	ldy	#SCREENH	; for (register uint8_t y = SCREENH; y > 0; y--)
-	ldx	#SCREENW	;  for (register uint8_t x = SCREENH; x > 0; x--)
-	lda	#$20		;
	jsr	$ffd2		;   printf(" ");
	dex			;
	bne	-		;
	dey			;
	bne	--		;
	lda	#$13		;
	jsr	$ffd2		; printf("%c", 19); // back to home corner

	.for s in b_label,b_arwup,b_threa,b_wound,b_inves,b_dpile,b_draw,b_deck,b_fodds,b_arwdn,b_feven,b_pairg
	 ldx	s-3		; 
	 ldy	s-2		;
	 lda	s-1		; for (i = 0; i < SLEN; i++)
	printxy	s		;  printxy(s[i], x=s[i]-1, y=s[i]-2, a=s[i]-3);
	.next
	rts			;} // bckdrop()
.endif

pre_end
.align	$100
vararea
* = * + $200
undobuf
.end
