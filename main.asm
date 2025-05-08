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
.endif

	.word	(+), 2055
	.text	$99,$22,$09,$8e	; PRINT " CHR$(9) CHR$(142)
	.text	$08,$93,$13,$13	; CHR$(8) CHR$(147) CHR$(19) CHR$(19)
topline	.text	"the crime scene     "
	.text	"  shiftlocks office"
	.text	$9d,$9d,$9d,$9d
	.text	$9d,$9d,$9d,$9d
	.text	$94,"'"
	.null	$11,$11,$11,$11,$22
+	.word	(+), 2055
	.text	$99,$22
	.text	"                 "
	.null	$ce,$22
+	.word	(+), 2055
	.text	$99,$22
	.text	"    threats      "
	.null	"dis-",$22
+	.word	(+), 2055
	.text	$99,$22
	.text	"   ",$12,"wounds: 0"
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
	.text	"     "
	.null	$ce,$22
+	.word	(+), 2055
	.text	$99,$22
	.text	"                  "
	.null	$12,"   ",$92,$22
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
	.text	" rules:",$22,$3b
	.text	$3a,$9e
	.null	format("%4d",main)
+	.word	0

.if !BASIC
*	= COPIED2
.endif

solvlen	.byte	solvtxt-tosolve	; (both addresses defined in the included file)
.include "gamerule.asm"
abillen	.byte	abiltxt-ability	; (both addresses defined in the included file)
.include "playeras.asm"

SCREEND	= SCREENC-SCREENM
SCRSIZE	= SCREENW*SCREENH
REVCARD = $08
NONCARD	= $ff
ONLYTOP	= SCREENM+SCREENW*(SCREENH-8)
DECKSIZ	= pstdeck-stddeck

UNDOABL	= vararea
STACKHT	= UNDOABL + $00		; 0-3 invest, 4-7 threat, 8-11 office
HAND	= UNDOABL + $0c		;
DISCARD	= UNDOABL + $10
DECK	= DISCARD + DECKSIZ
UNDOABH	= DISCARD + 2*DECKSIZ	; should always be 0 or 4?
HANDREM = UNDOABH + 0
DISCREM	= UNDOABH + 1
DECKREM	= UNDOABH + 2
NWOUNDS	= UNDOABH + 3
HANDHST	= UNDOABH + 4
HANDGET	= UNDOABH + $c;?
HANDFOM	= UNDOABH + $d;?
;????	= UNDOABH + $e?
;????	= UNDOABH + $f

UNSAVED	= vararea + $100
TEMPVAR	= UNSAVED + $0
SCRATCH	= UNSAVED + $1

start

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
	.byte	$32,$3a,$58,$69
woundsx	.byte	$0b
woundsy	.byte	$07
drawx	.byte	$12
drawy	.byte	$10
discx	.byte	$12
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
	.byte	1,1,1,1		;bottom card of a stack allowed to grow up to...
stacklm	.byte	1,1,1,1,1,1,1,1
	.byte	$10,$10,$10,$10	;16 (alternating invest with threat or removals)

main	lda	#0		;void main (void) {
	sta	DISCREM		;
	sta	HANDREM		;
	sta	NWOUNDS		; NWOUNDS = DISCREM = HANDREM = 0;
	jsr	finishr		; finishr(); // rule/ability text and card color
	jsr	initstk		; initstk();
	lda 	#DECKSIZ	;
	jsr	shuffle		; shuffle(/* DECKREM =*/ DECKSIZ);
	ldx	#$ff		;
-	txa			;
	pha			; for (register uint8_t a = 255; a; a--) {
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
newhand	jsr	drw4hnd		; do {
	sta	HANDFOM		;  HANDFOM = drw4hand(); // nonzero if we drew 4
	bne	+		;  if (HANDFOM == 0)
	brk			;   exit(1); // more than 44 cards in office?!?
	.byte	$1		;
+	jsr	animhnd		;  animhnd(); // draw empty deck pile after if 0
	lda	HANDFOM		;
	jsr	redrwok		;
   ;   	bne	+		;
	jsr	animrej		;
	beq	newhand		; } while (/*redrwok(HANDFOM) &&*/ animrej());
+
 	rts			;} // main()

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
	ldy	#0		; } // top of any bona fide card has been drawn

blankit	plp			; y = 0;
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
	iny			;    if (x == REVCARD) goto colrto2;
	lda	#0		;    
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

initstk	lda	#$00		;void initstk(void) {
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
	rts			;} //shuffle()

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
	bne	-		;
	ldy	#4		;  uint8_t retval = 0; // return figure of merit
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
	brk			;     exit(2);
	.byte	2		;   } // a is a valid card in the range 0 ~ 7
+	;and	#$07		;
	sta	HAND-1,y	;   HAND[y] = a /* & 0x07 */;
	tax			;
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

redrwok	sta	TEMPVAR		;uint8_t redrwok(register uint8_t& a) {
	and	#$0f		; if (a & 0x0f == 0) // got no investig. cards
	beq	+++		;  return 1;
	lda	TEMPVAR		;
	and	#$f0		; else if (a & 0xf0 == 0) // got no threat cards
	beq	+++		;  return 1;
	lda	#0		; TEMPVAR = a;
	clc			;
	rol	TEMPVAR		;
-	ror	TEMPVAR		; for (a = 0; TEMPVAR; TEMPVAR >>= 1) // 1-count
	beq	+		;
	adc	#0		;
	bcc	-		;  a++;  // (this adc will never set carry)
+	cmp	#3		;
	bcc	+		; if (a < 3) { // might have 3 copies of a card
	lda	#1		;
	ldy	#8		;
-	ldx	HANDHST-1,y	;  for (register int8_t y = 7; y >= 0; y--)
	cpx	#3		;   if (HANDHST[y] >= 3)
	bcs	+++		;    return 1; // indeed have 3 copies of card y
	dey			;
	bne	-		; }
+	lda	#$ff		; return 0;
+	clc			;
	adc	#1		;
+	rts			;} // redrwok()

animhnd	ldx	HANDREM		;void animhnd(void) { // just paint them for now
	beq	drawsho		; if (HANDREM) {
-	txa			;  for (register int8_t x = HANDREM; x>=0; x--){
	pha			;
	lda	HAND-1,x	;   register int8_t a = HAND[x];
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
	bne	-		;} // animhnd()
drawsho	ldx	drawx		;void drawsho(void) {
	ldy	drawy		;
	lda	DECKREM		;
	beq	+		; if  (DECKREM) {
	clc			;
	lda	#REVCARD	;  // draw just the blank card back
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
	beq	+		; if (DISCREM) {
	tay			; 
	lda	DISCARD-1,y	;
	ldy	discy		;
	clc			;  cardsho(0, a = DISCARD[DISCREM-1],
	jsr	cardsho		;          x = discx, y = discy);
	rts			; } else
+	jsr	cardout		;  cardout(x = discx,  y = discy);
	rts			;} // discsho()

cardout	clc			;void cardout(register uint8_t& x,
	lda	#NONCARD	;             register uint8_t& y) { // clear it:
	jsr	cardsho		; cardsho(0,a = NONCARD, x = drawx, y = drawy);
	ldx	#cardofs-pileout; // cardsho() left zp pointing to the top left
-	lda	cardofs-1,x	; for (int8_t x = 3*5 - 1; x > 0; x--) {
	tay			;
	lda	pileout-1,x	;
	sta	(zp),y		;  zp[cardofs[x]] = pileout[x];
	dex			;
	bne	-		; }
	rts			;} // cardout()

animrej				;uint1_t animrej(void) {
	handmsg	rejmsg0,rejmsg1-rejmsg0,rejmsg2-rejmsg1,SCRATCH
-	jsr	$ffe4		; handmsg("DISCARD & REDRAW?"/*RVS ON*/
	beq	-		;          "PRESS Y FOR YES"/*RVS OFF*/, 17, 15
	and	#$df		;         SCRATCH);
	cmp	#$59		; register uint8_t a = getchar();
	php			; handmsg(SCRATCH, 17, 15); // pop backing store
	handmsg	SCRATCH,rejmsg1-rejmsg0,rejmsg2-rejmsg1
	plp			;
	php
	bne	+		; if (a == 'Y' || a == 'y') {
	ldx	#4		;
	ldy	DISCREM		;  register uint8_t y = DISCREM;
-	lda	HAND-1,x	;  for (register int8_t& x = 3; x >= 0; x--) {
	sta	DISCARD,y	;   DISCARD[y] = HAND[x];
	lda	#NONCARD	;
	sta	HAND-1,x	;   HAND[x] = 0xff; // blank a 3x5 rectangle...
	txa			;
	pha			;
	tya			;
	pha			;
	lda	#NONCARD	;
	ldx	discx		;
	ldy	discy		;
	clc			;
	jsr	cardsho		;   cardsho(0, 0xff, *discx, *discy); // at card
	pla			;
	tay			;
	pla			;
	tax			;
	iny			;
	sty	DISCREM		;   DISCREM++;
	dex			;   HANDREM--;
	bne	-		;  }
;	jsr	animhnd		;  
	jsr	discsho		;  discsho();
	stx	HANDREM		; }
+	plp			; return a == 'Y' || a == 'y';
	rts			;} // animrej()
rejmsg0	.byte	$04,$09,$13,$03	; DISC
	.byte	$01,$12,$04,$20	; ARD 
	.byte	$26,$20,$12,$05	; & RE
	.byte	$04,$12,$01,$17	; DRAW
	.byte	$3f		; ?
rejmsg1	.byte	$90,$92,$85,$93	; PRES
	.byte	$93,$a0,$99,$a0	; S Y 
	.byte	$86,$8f,$92,$a0	; FOR
	.byte	$99,$85,$93	; YES
rejmsg2	

pre_end
.align	$80
vararea
.align	$100	
undobuf
.end
