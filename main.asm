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

.include "gamerule.asm"		; tosolve, solvlen
.include "playeras.asm"		; abillen, ability

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
UNDOABH	= DISCARD + 2*DECKSIZ	; should always be 0 or 4?
HANDREM = UNDOABH + 0
DISCREM	= UNDOABH + 1
DECKREM	= UNDOABH + 2
NWOUNDS	= UNDOABH + 3
HANDFOM	= UNDOABH + $c
MOVEPWR	= UNDOABH + $d
TOSCENE	= UNDOABH + $e
TOFFICE	= UNDOABH + $f

UNSAVED	= vararea + $80
.if UNSAVED <= TOFFICE
.error "saved variables crashed into ceiling, obliterated UNSAVED"
.endif
HANDHST	= UNSAVED + $0
TEMPVAR	= UNSAVED + $8
SCRATCH	= UNSAVED + $9		; must be last

snapsht	rts

undomov	rts

redomov	rts

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
	.byte	$32,$3a,$58,$69,0,0
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
isoffic	.byte	$04		;for nondestructive bit tests

main	lda	#0		;void main (void) {
	sta	DISCREM		;
	sta	HANDREM		;
	sta	NWOUNDS		; NWOUNDS = DISCREM = HANDREM = 0;
	jsr	bckdrop		; bckdrop(); // draw most of the scren
	jsr	finishr		; finishr(); // rule/ability text and card color
	digitxy	NWOUNDS,WDX,WDY	; digitxy(NWOUNDS, WDX, WDY); // set wounds to 0
	jsr	discsho		; discsho(); // show the empty discard pile
	jsr	initstk		; initstk(); // empty the crime scene and office
	jsr	animshf		; animshf(); // shuffle deck briefly onscreen

	nop			; do {
newhand	jsr	drw4hnd		;  do {
	sta	HANDFOM		;   HANDFOM = drw4hand(); // nonzero if we drew 4
	bne	+		;   if (HANDFOM == 0)
	brk			;    exit(1); // more than 44 cards in office?!?
	.byte	$1		;
+	jsr	snapsht		;   snapshot();
	jsr	animhnd		;   animhnd(); // draw empty deck pile after if 0
	lda	HANDFOM		;
	jsr	rejctok		;
      	beq	+		;
	jsr	animrej		;
	beq	newhand		;  } while (rejctok(HANDFOM) && animrej());

+	lda	#2		;
	sta	TOSCENE		;
	sta	TOFFICE		;  TOSCENE = TOFFICE = 2; // must play 2 to each

getmove	jsr	$ffe4		;  do {
	beq	getmove		;   a = getchar();
	cmp	#DEL_KEY	;   if (a == DEL_KEY) {
	bne	+		;
	jsr	undomov		;    undomov();
	jmp	getmove		;    continue;

+	cmp	#INS_KEY	;
	bne	+		;   } else if (a == INS_KEY) {
	jsr	redomov		;    redomov();   
	jmp	getmove		;    continue;

+	cmp	#'1'		;
	bcc	notfkey		;   } else if (a >= '1'
	cmp	#'8'+1		;              &&
	bcs	+		;              a < '9') {
	pha			;
	lda	#0		;
	sta	TEMPVAR		;   TEMPVAR = 0;
	pla			;
	sec			;
	sbc	#'1'		;    a -= '1'; // 0~7 even:crimescene,odd:office
	lsr			;    // a:         0   1   2   3   4   5   6   7
	ldy	#3		;              // CS  OF  CS  OF  CS  OF  CS  OF
-	rol	TEMPVAR		;    // (a&1)<<2:  0   4   0   4   0   4   0   4
	dey			;    // a>>1:      0   0   1   1   2   2   3   3
	bne	-		;    TEMPVAR = ((a & 1) << 2) | (a >> 1);
	ora	TEMPVAR		;    a = TEMPVAR; // 0~3 crimescene, 4~7 office!
	jmp	trymove		;

+	cmp	#F1_KEY		;
	bcc	notfkey		;   } else if (a >= F1_KEY
	cmp	#F1_KEY+8	;              &&
	bcs	notfkey		;              a < F1_KEY+8) {
	sec			;
	sbc	#F1_KEY		;    a -= F1_KEY - 1;// 0~3 crimescene, 4~7 as ^
	jmp trymove;bcs	trymove		;

notfkey	jmp	getmove		;   } else continue;

trymove	pha			;
	and	#$03		;
	tay			;
	lda	HAND,y		;
	and	#$f8		;
	beq	+		;   if (HAND[a & 3] >= 8)
	pla			;
	jmp	getmove		;    continue; // already played that card
+
	pla			;
	pha			;   uint8_t stack = a; // movedok() returns 1<<a
	jsr	movedok		;   
	bne	numleft		;   if (!movedok(a)) { //returns 1<<a if ok or 0
	jsr	warning		;    if (warning() == 0 /* 0 wounds accepted */)
	bne	acceptw		;
	pla			;
	beq	notfkey		;     continue;
acceptw	pla			;
	and	#$03		;
	tay			;
	lda	#NONCARD	;
	sta	HAND,y		;    HAND[stack & 3] = NONCARD;
	dec	HANDREM		;    HANDREM--;
	inc	NWOUNDS		;    NWOUNDS++;
	digitxy	NWOUNDS,WDX,WDY	;    digitxy(NWOUNDS, WDX, WDY);
numleft	lda	NWOUNDS		;   }
	cmp	#$03		;
	bcc	+		;   if (NWOUNDS >= 3)
	bcs	mainend		;    exit(0);
+
.if 0
	and	#$03		;
	tay			;
	ldx	inhandx,y	;
	ldy	inhandy		;
	clc			;
	lda	#NONCARD	;
	jsr	cardsho		;
.else
	jsr	animhnd		;   animhnd(); // show the card as missing
.endif

	jsr	snapsht		;   snapsht();
	lda	HANDREM		;
	beq	+		;
	jmp	getmove		;  } while (HANDREM);
+	jsr	gamewon		;
	bne	mainend		;
	jmp	newhand		; } while (!gamewon());
mainend	rts			;} // main()

officem	.byte	$f0		;
movedok	bit	officem		;uint8_t moveok(register uint8_t& a) {
	beq	scenem		; if (a & 0xf0) { // trying dec TOFFICE, compat?
	dec	TOFFICE		;  if (TOFFICE-- == 0) // didn't already play 2
	bpl	handgap		;//FIXME:need to check compatibility of pile too
	lda	#0		;
	sta	TOFFICE		;   return a = TOFFICE = 0; // z set
	beq	notmove		; } else { // trying dec TOSCENE, less stringent
scenem	dec	TOSCENE		;  if (TOSCENE-- == 0) // didn't already play 2
	bpl	handgap		;
	lda	#0		;
	sta	TOSCENE		;   return a = TOSCENE = 0; // z set
	beq	notmove		; }

handgap	pha			;
	and	#$03		;
	tay			;
	lda	#NONCARD	;//FIXME:need to update the new location of card
	sta	HAND,y		; HAND[a & 0x03] = NONCARD;
	dec	HANDREM		; HANDREM--;
	pla			;
	tay			;
	iny			;
	lda	#0		;
	sec			;
-	rol			;
	dey			;
	bne	-		; return 1<<a; // z clear
	and	#$ff		;
notmove	rts			;} // movedok()


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
.if 1
	lda	#0		; TEMPVAR = a;
	clc			; // optimization (check flag bits before array)
	rol	TEMPVAR		;
-	ror	TEMPVAR		; for (a = 0; TEMPVAR; TEMPVAR >>= 1) // 1-count
	beq	+		;
	adc	#0		;
	bcc	-		;  a++;  // (this adc will never set carry)
+	cmp	#3		;
	bcs	rejrtn3		; if (a < 3) { // might have 3 copies of a card
.endif
	lda	#1		;
	ldy	#8		;
-	ldx	HANDHST-1,y	;  for (register int8_t y = 7; y >= 0; y--)
	cpx	#3		;   if (HANDHST[y] >= 3)
	bcs	rejrtn3		;    return 3; // indeed have 3 copies of card y
	dey			;
	bne	-		; }
rerrtn0	lda	#$fc		; return 0;
rejrtn4	clc			;
	adc	#4		;
rejrtn3	rts			;} // rejctok()

animhnd	ldx	#4		;void animhnd(void) { // just paint them for now
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
	bne	-		;} // animhnd()
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
	beq	+		; if (DISCREM) {
	lda	DISCARD-1,y	;
	ldy	discy		;
	clc			;  cardsho(0, a = DISCARD[DISCREM-1],
	jsr	cardsho		;          x = discx, y = discy);
	rts			; } else
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

	nop			;uint1_t animrej(void) {
animrej	handmsg	rejmsg0,rejmsg1-rejmsg0,rejmsg2-rejmsg1,SCRATCH
-	jsr	$ffe4		; handmsg("DISCARD & REDRAW?"/*RVS ON*/
	beq	-		;         " PRESS Y FOR YES "/*RVS OFF*/, 17, 17,
	and	#$df		;         SCRATCH);
	cmp	#$59		; register uint8_t a = getchar();
	php			; handmsg(SCRATCH, 17, 17); // pop backing store
	handmsg	SCRATCH,rejmsg1-rejmsg0,rejmsg2-rejmsg1
	plp			;
	php			;
	bne	+		; if (a == 'Y' || a == 'y') {
	ldx	#4		;
	ldy	DISCREM		;
-	lda	HAND-1,x	;  for (register int8_t x = 3; x >= 0; x--) {
	sta	DISCARD,y	;
	iny			;
	sty	DISCREM		;   DISCARD[DISCREM++] = HAND[x];
	lda	#NONCARD	;
	sta	HAND-1,x	;   HAND[x] = 0x09; // blank a 3x5 rectangle...
	txa			;
	pha			;
	tya			;
	pha			;
.if 0
	lda	inhandx-1,x	;
	tax			;
	lda	#NONCARD	;
	ldy	inhandy		;
	clc			;   // at that card's location:
	jsr	cardsho		;   cardsho(0, 0x09, inhandx[x], inhandy[x]);
.endif
	jsr	discsho		;   discsho(); // and show the card in the pile
	pla			;
	tay			;
	pla			;
	tax			;
	dex			;   HANDREM--;
	bne	-		;  }
	stx	HANDREM		; }
+	plp			; return a == 'Y' || a == 'y', a = 1;
	rts			;} // animrej()
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

	nop			;uint8_t warning(void) {
warning	handmsg	wrnmsg0,wrnmsg1-wrnmsg0,wrnmsg2-wrnmsg1,SCRATCH
-	jsr	$ffe4		; handmsg("CANNOT PLAY THERE"/*RVS ON*/
	beq	-		;         "PRESS Y FOR WOUND"/*RVS OFF*/, 17, 17,
	and	#$df		;         SCRATCH);
	cmp	#$59		; register uint8_t a = getchar();
	php			; handmsg(SCRATCH, 17, 15); // pop backing store
	handmsg	SCRATCH,rejmsg1-rejmsg0,rejmsg2-rejmsg1
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
.if 1
	.byte	$11,$05,(+)-*-3	; (17,5)
b_arwup	.byte	$4e		; /
+
.else
	.byte	$11,$04,(+)-*-3	; (17,4)
b_arwup	.byte	$64,$20,$20,$20	; _
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20,$20	;
	.byte	$4e,$65		; /[
+	
.endif

+	.byte	$04,$06,(+)-*-3	; (4,6)
b_threa	.byte	$14,$08,$12,$05	; THRE
	.byte	$01,$14,$13,$20	; ATS
	.byte	$20,$20,$20,$20	;
	.byte	$20,$04,$09,$13	;  DIS
	.byte	$2d		; -

+	.byte	$03,$07,(+)-*-3	; (3,7)
b_wound	.byte	$97,$8f,$95,$8e	; WOUN
	.byte	$84,$93,$ba,$a0	; DS:
	.byte	$bf,$20,$20,$20	; ?
	.byte	$20,$20,$03,$01	;   CA
	.byte	$12,$04		; RD

+	.byte	$01,$08,(+)-*-3	; (1,8)
b_inves	.byte	$09,$0e,$16,$05	; INVE
	.byte	$13,$14,$09,$07	; STIG
	.byte	$01,$14,$09,$0f	; ATIO
	.byte	$0e,$13		; NS

+	.byte	$11,$09,(+)-*-3	; (17,9)
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
.if 1
	.byte	$15,$0f,(+)-*-3	; (21,15)
b_arwdn	.byte	$4e		; /
+
.else
	.byte	$14,$0f,(+)-*-3	; (20,15)
b_arwdn	.byte	$67,$43,$20,$20	; ]/
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20,$20	;
	.byte	$20,$20,$20,$20	;
	.byte	$20,$77		; ^
+
.endif

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
.if BASIC
	ldy	#SCREENH	; for (register uint8_t y = SCREENH; y > 0; y--)
-	ldx	#SCREENW	;  for (register uint8_t x = SCREENH; x > 0; x--)
-	lda	#$20		;
	jsr	$ffd2		;   printf(" ");
	dex			;
	bne	-		;
	dey			;
	bne	--		;
.endif
	.for s in b_label,b_arwup,b_threa,b_wound,b_inves,b_dpile,b_draw,b_deck,b_fodds,b_arwdn,b_feven,b_pairg
	 ldx	s-3		; 
	 ldy	s-2		;
	 lda	s-1		; for (i = 0; i < SLEN; i++)
	printxy	s		;  printxy(s[i], x=s[i]-1, y=s[i]-2, a=s[i]-3);
	.next
	rts			;}
pre_end
.align	$80
vararea
.align	$100	
undobuf
.end
